{
  Physical Disk Access for Delphi
  supports Windows(R) 95,98,ME,NT and 2000

  Copyright (C) 2001 by Alexander Grau, All rights reserved
  contact: alexander_grau@web.de

  THIS IS FREEWARE
}

{
	History
  2016/08/22 Change physical file sharing to FILE_SHARE_READ

}


unit int13ext;

interface

const
  IFLAG_HANDLES_DMA_BOUNDARY = 1;
  IFLAG_GEOMETRY_VALID       = 2;
  IFLAG_REMOVABLE            = 4;
  IFLAG_VERIFY_SUPPORT       = 8;
  IFLAG_CHANGE_LINE_SUPPORT  = 16;
  IFLAG_IS_LOCKABLE          = 32;
  IFLAG_NO_MEDIA_PRESENT     = 64;


type
  PDriveParams = ^TDriveParams;
  TDriveParams = packed record { used by GetDriveParams }
     bufsize    : word;
     infoflags  : word;
     physcyl    : longint;
     physheads  : longint;
     physsecptrk: longint;
     physsecLO  : longint;
     physsecHI  : longint;
     bytesPerSec: word;
     EDDparams  : pointer;
  end;



{-----------------------------------------------------------------------------
  Procedure: CheckExtensions
  Author:    Alexander Grau (alexander_grau@web.de)
  Date:      17-mai-2004
  Arguments: drv: byte
  Result:    Boolean
-----------------------------------------------------------------------------}
function CheckExtensions(drv: byte):Boolean;

{-----------------------------------------------------------------------------
  Procedure: ExtendedRead : Lit des secteurs physiques du disque
  Author:	 Alexander Grau (alexander_grau@web.de)
  Date:		 2001
  Arguments: drv: byte;			Numero du disque
  			 LBA: longint;		Numero du secteur
             blocks: byte;		Nombre de secteurs à lire
             buf: pointer		Buffer pour les secteurs
  Result:    True si pas d'erreurs
-----------------------------------------------------------------------------}
function ExtendedRead(drv: byte; LBA: longint; blocks: byte; buf: pointer): boolean;

{-----------------------------------------------------------------------------
  Procedure: ExtendedWrite
  Author:    Alexander Grau (alexander_grau@web.de)
  Date:      2001
  Arguments: drv: byte; LBA: longint; blocks: byte; buf: pointer; verify: boolean
  Result:    boolean
-----------------------------------------------------------------------------}
function ExtendedWrite(drv: byte; LBA: longint; blocks: byte; buf: pointer;
  verify: boolean): boolean;

{-----------------------------------------------------------------------------
  Procedure: GetDriveParams
  Author:    Alexander Grau (alexander_grau@web.de)
  Date:      20-mai-2004
  Arguments: drv: byte; resultbuf: PDriveParams
  Result:    boolean
-----------------------------------------------------------------------------}
function GetDriveParams(drv: byte; resultbuf: PDriveParams): boolean;


// https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa365467(v=vs.85).aspx

implementation

uses sysutils, windows;

const
  DIOC_CHECKEXTENSIONS = 1;
  DIOC_EXTENDEDREAD    = 2;
  DIOC_EXTENDEDWRITE   = 3;
  DIOC_GETDRIVEPARAMS  = 4;


  // --------- NT stuff... -------------------------------------------------------------
  FILE_DEVICE_DISK               =  $00000007;
  FILE_ANY_ACCESS                =  0;

  METHOD_BUFFERED                =  0;

  IOCTL_DISK_BASE                = FILE_DEVICE_DISK;
  IOCTL_DISK_GET_DRIVE_GEOMETRY  = ( ((IOCTL_DISK_BASE) SHL 16) OR ((FILE_ANY_ACCESS) SHL 14) OR (($0000) SHL 2) OR (METHOD_BUFFERED) );
  // -------------------------------------------------------------------------------------


type
  extstruc = packed record  { wichtig! Delphi darf nicht auf 32-Bit ausrichten!
                               (sonst geht's schief!) }
    drv   : byte;
    LBA   : longint;
    blocks: byte;
    buf   : pointer;
    verify: byte;
  end;



  // --------- NT stuff... -------------------------------------------------------------
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/base/media_type_str.asp
  (*typedef enum _MEDIA_TYPE {
     Unknown,                // Format is unknown
     F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
     F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
     F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
     F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
     F3_720_512,             // 3.5",  720KB,  512 bytes/sector
     F5_360_512,             // 5.25", 360KB,  512 bytes/sector
     F5_320_512,             // 5.25", 320KB,  512 bytes/sector
     F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
     F5_180_512,             // 5.25", 180KB,  512 bytes/sector
     F5_160_512,             // 5.25", 160KB,  512 bytes/sector
     RemovableMedia,         // Removable media other than floppy
     FixedMedia,             // Fixed hard disk media
     F3_120M_512,            // 3.5", 120M Floppy
     F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
     F5_640_512,             // 5.25",  640KB,  512 bytes/sector
     F5_720_512,             // 5.25",  720KB,  512 bytes/sector
     F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
     F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
     F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
     F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
     F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
     F8_256_128              // 8",     256KB,  128 bytes/sector
  } MEDIA_TYPE, *PMEDIA_TYPE;*)

  PLARGE_INTEGER = ^LARGE_INTEGER;
  LARGE_INTEGER = packed record
	LowPart: dword;
	HighPart: dword;
  end;

  PDISK_GEOMETRY = ^TDISK_GEOMETRY;
  TDISK_GEOMETRY = packed record
    Cylinders: LARGE_INTEGER;
    MediaType: dword;
    TracksPerCylinder: dword;
    SectorsPerTrack: dword;
    BytesPerSector: dword;
  end;
  // -------------------------------------------------------------------------


var
  ghDevice: thandle;
  //ghNTdevice: array[0..255] of thandle;
  ExitSave: Pointer;
  winNTflag: boolean;
  i: integer;
  disk_geometry: tdisk_geometry;



function IsWinNT: boolean;
var
  info: TOSVersionInfo;
begin
  IsWinNT:=false;
  info.dwOSVersionInfoSize:=sizeof(TOSVersionInfo);
  if GetVersionEx(info) then
  begin
    if info.dwPlatformId = VER_PLATFORM_WIN32_NT then IsWinNT:=true;
  end;
end;


// ------------------------ NT stuff ..... ---------------------------------------------------
function NT_GetDriveGeometry(drv: byte; dg: PDISK_GEOMETRY): boolean;
var
  hDevice: thandle;
  fResult: boolean;
  cb: DWORD;
begin
    fResult:=false;
    hDevice := CreateFile(pchar('\\.\PhysicalDrive'+inttostr(drv)),
        0, FILE_SHARE_READ {OR FILE_SHARE_WRITE},
        nil, OPEN_EXISTING, 0, 0);

    if (hDevice <> INVALID_HANDLE_VALUE) then
// Correction Chris 23/01/2005
//    if (hDevice <> 0) then
    begin
      fResult := DeviceIoControl(hDevice,
        IOCTL_DISK_GET_DRIVE_GEOMETRY, nil, 0,
        dg, sizeof(TDISK_GEOMETRY), cb, nil);
    end;

    CloseHandle(hDevice);
    NT_GetDriveGeometry:=fResult;
end;


function NT_Read(drv: byte; LBA: longint; blocks: byte; buf: pointer): boolean;
var
  res: boolean;
  bytestoread, numread: dword;
  err: dword;
  i: integer;
  bufp: ^byte;
  dwpointer: dword;
  hdevice: thandle;
  ldistancelow, ldistancehigh: dword;
  tempbuf: array[0..511] of byte;
begin
  res:=false;
  //hDevice:=ghNTdevice[drv];
  hDevice:=CreateFile(pchar('\\.\PhysicalDrive'+inttostr(drv)), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  if hDevice <> INVALID_HANDLE_VALUE then
  begin
    ldistanceLow:=dword(LBA SHL 9);
    ldistanceHigh:=dword(LBA SHR (32-9));
    dwpointer:=SetFilePointer(hdevice, ldistancelow, @ldistancehigh, FILE_BEGIN);
    if dwPointer <> $FFFFFFFF then
    begin
      bytestoread:=blocks*512;
      res:=ReadFile(hDevice, {buf^} tempbuf, bytestoread, numread, nil);
      if res then move(tempbuf, buf^, 512);

      if not res then
      begin
        err:=GetLastError;
        //#MODIF Pas des messages d'erreurs
        //messagebox(0, pchar('error no.'+inttostr(err)+#13#10+'drv:'+inttostr(drv)+' LBA:'+inttostr(LBA)
        //  +' blocks:'+inttostr(blocks) ), 'NT_read error', mb_ok);
      end;
    end;
    CloseHandle(hDevice);
  end;
  NT_Read:=(res) AND (numread=blocks*512);
end;

// -----------------------------------------------------------------------------------




function CheckExtensions(drv: byte):Boolean;
var
  res: boolean;
  outbuf: byte;
  cb: Cardinal;
begin
  outbuf:=0;
  res:=DeviceIoControl(ghDevice, DIOC_CHECKEXTENSIONS,
      @drv, 1,
      @outbuf, 1, cb, nil);

  CheckExtensions:=res AND (outbuf=1);
end;


function ExtendedRead(drv: byte; LBA: longint; blocks: byte; buf: pointer): boolean;
var
  res: boolean;
  struc: extstruc;
  cb: Cardinal;
  tempbuf: array[0..511] of byte;

begin
  if winNTflag then
    res:=NT_Read(drv,  LBA, blocks, buf)
  else begin
    struc.Drv    := drv;
    struc.LBA    := LBA;
    struc.blocks := blocks;
    struc.buf    := {buf;} @tempbuf;

    res:=DeviceIoControl(ghDevice, DIOC_EXTENDEDREAD,
        @struc, sizeof(extstruc),
        nil, 0, cb, nil);
    if res then move(tempbuf, buf^, 512);
  end;

  ExtendedRead:=res;
end;


function ExtendedWrite(drv: byte; LBA: longint; blocks: byte; buf: pointer;
  verify: boolean): boolean;
var
  res: boolean;
  struc: extstruc;
  cb: Cardinal;

begin
  struc.Drv    := drv;
  struc.LBA    := LBA;
  struc.blocks := blocks;
  struc.buf    := buf;
  struc.verify := byte(verify);

  res:=DeviceIoControl(ghDevice, DIOC_EXTENDEDWRITE,
      @struc, sizeof(extstruc),
      nil, 0, cb, nil);

  ExtendedWrite:=res;
end;


function GetDriveParams(drv: byte; resultbuf: PDriveParams): boolean;
var
  res: boolean;
  struc: extstruc;
  cb: Cardinal;
  dg: TDisk_Geometry;

begin
  res:=false;
  if WinNTflag then
  begin
    res:=NT_GetDriveGeometry(drv, @dg);
    if res then
    begin
      // simulate INT13 extensions...
      resultbuf^.bufsize:=30;
      resultbuf^.infoflags:=0;
      case dg.MediaType of
        1..11, 13..22:  resultbuf^.infoflags:=resultbuf^.infoflags OR IFLAG_REMOVABLE;
      end;
      resultbuf^.physcyl:=dg.cylinders.lowpart;
      resultbuf^.physheads:=dg.trackspercylinder;
      resultbuf^.physsecptrk:=dg.sectorspertrack;
      resultbuf^.physsecLO:=dg.cylinders.lowpart * dg.trackspercylinder * dg.sectorspertrack;
      resultbuf^.physsecHI:=0;
      resultbuf^.bytesPerSec:=dg.bytespersector;
      resultbuf^.EDDparams:=nil;
    end;
  end else
  begin
    struc.Drv    := drv;
    struc.buf    := resultbuf;
    resultbuf^.bufsize:=30;

    if drv = 1 then begin GetDriveParams:=false; exit; end;
    res:=DeviceIoControl(ghDevice, DIOC_GETDRIVEPARAMS,
        @struc, sizeof(extstruc),
        nil, 0, cb, nil);
    if drv IN [0..$7f] then resultbuf^.infoflags:=resultbuf^.infoflags OR IFLAG_REMOVABLE;
  end;

  GetDriveParams:=res;
end;




procedure MyExit;
begin
  ExitProc := ExitSave;            { Zuerst alten Vektor wiederherstellen }

  if (winNTflag) then
  begin
    (*for i:=0 to 255 do
    begin
      if ghNTdevice[i] <> INVALID_HANDLE_VALUE then
       CloseHandle(ghNTdevice[i]);
    end;*)
  end
  else begin
    if (ghDevice <> INVALID_HANDLE_VALUE) then
    begin
      // CloseHandle(hDevice);
      DeleteFile('\\.\INT13EXT');
    end;
  end;
end;


begin
  ExitSave := ExitProc;
  ExitProc := @MyExit;
  ghdevice:=invalid_handle_value;

  winNTflag:=IsWinNT;

  if winNTflag then
  begin
    (*for i:=0 to 255 do
    begin
      ghNTdevice[i]:=INVALID_HANDLE_VALUE;
      if  GetDriveParams(i, @disk_geometry) then
      begin
        ghNTdevice[i]:=CreateFile(pchar('\\.\PhysicalDrive'+inttostr(i)), GENERIC_READ, {FILE_SHARE_READ OR} FILE_SHARE_WRITE,
          nil, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
      end;
    end;*)

  end
  else begin
    ghDevice:=CreateFile('\\.\INT13EXT.VXD', 0, 0, nil, 0,
      FILE_FLAG_DELETE_ON_CLOSE, 0);

    if ghDevice = INVALID_HANDLE_VALUE then
    begin
      MessageBox(0, 'Error loading "INT13EXT.VXD"', 'Error', mb_IconExclamation + mb_ok);
      halt;
    end;
  end;
end.

