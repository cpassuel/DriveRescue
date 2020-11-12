{-----------------------------------------------------------------------------
 Unit Name: PhysicalDrive
 Author:    Chris
 Purpose:	Gère des disques physiques, et permet la lecture de secteurs
 Date:		07/02/2005
 History:	Ajout lecture table des partitions

 31/12/2004 Correction bug ds lecture de la table des partitions
            Suppression de pdDriveParam, Normalisation géométrie
-----------------------------------------------------------------------------}


unit PhysicalDrive;

interface

uses
	SysUtils, Classes, Windows, Types,
    //
    GenericDrive, int13ext;


const
	AppDefinedErrorMask	= $20000000;

    CANT_GET_DRIVE_PARAM	= $0001 or AppDefinedErrorMask;

type


//
// Classe pour manupuler un disque physique
//
TPhysicalDrive = class(TGenericDrive)
	private
        wNumCyls                   : Word;	// Number of logical cylinders
        wNumHeads                  : Word;	// Number of logical heads
        wSectorsPerTrack           : Word;	// Number of logical sectors per logical track
        wNumCurrentCyls            : Word;      // Number of current cylinders
        wNumCurrentHeads           : Word;      // Number of current heads
        wNumCurrentSectorsPerTrack : Word;      // Number of current sectors per track
        ulCurrentSectorCapacity    : ULONG;     // Current capacity in sectors (Word 57 specifies the low word)
        wMultSectorStuff           : Word;
        ulTotalAddressableSectors  : ULONG;     // Total Number of User Addressable Sectors
    protected
    	pdDriveNum		: Byte;			// Numéro du disque 0 pour Primary Master
      hdDevice: THandle; // Windows Handle for physical drive (WinNT+ only)
        // ----------------------- Gestion des propriétés ------------------
        function isDriveRemovable: boolean;
        // -----------------------------------------------------
		procedure GetSmartInfo(); override;
    procedure OpenPhysicalDrive();	// Create a handle to physical drive
    public
        constructor Create(const drive: Byte); overload;
        destructor Destroy; override;
        function ReadSectors(const LBA: Cardinal; nbsect: Byte; buf: Pointer): boolean; override;
        function ReadNTSectors(const lba: Cardinal; nbsect: Cardinal; buf: Pointer): Cardinal; override;
        class procedure EnumDrives(drivelist: TList);
        procedure GetDriveProperties(info: TStrings); override;
        // ---------------------- Propriétés ------------------------------
        property Drive			: Byte read pdDriveNum;
        property isRemovable	: boolean read isDriveRemovable;
        //property Flags			: Word read pdDriveParam.infoflags;
end;

//
// Mettre un callback en param dans enumdrive ???
// TEnumDrivesCallBack = procedure(disque: TPhysicalDrive);
// Gerer les erreurs dans l'enum


// Fonctions exportées
//function isDriveExist(const drv: byte): boolean;
function isAFixedDrive(const drv: byte): boolean;


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation

uses
  Dialogs;

// URL for managing Physical drive under WinNT+
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa365467(v=vs.85).aspx
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa365468(v=vs.85).aspx
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa365541(v=vs.85).aspx
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa365542(v=vs.85).aspx
// https://msdn.microsoft.com/fr-fr/library/windows/desktop/bb540534(v=vs.85).aspx

{const
    NON_LBA_MAX_SECT = 1024*255*63; 	// Nombre de secteurs maximum pour une part non LBA
}

const
	INVALID_SET_FILE_POINTER = DWORD(-1);
  

// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Creation de l'objet, errorcode à 0 si pas de problème
//
constructor TPhysicalDrive.Create(const drive : Byte);
var
	dptemp: TDriveParams;
begin
	inherited Create();

    // Utile ?
    ZeroMemory(@dptemp, sizeof(dptemp));

	// Verifie si le disque existe
    if GetDriveParams(drive, @dptemp) then
    begin
    	Self.pdDriveNum := drive;

        Self.FCylinderCount := dptemp.physcyl;
        Self.FHeadCount		:= dptemp.physheads;
        Self.FSectorPerTrack:= dptemp.physsecptrk;
        Self.FBytesPerSec	:= dptemp.bytesPerSec;
        Self.pdTotalSectors	:= dptemp.physheads * dptemp.physsecptrk * dptemp.physcyl;
        Self.FDriveFlags	:= dptemp.infoflags;

	    // Normaliser la géometrie pour Win9x => head = 255, S = 63
        // Seulement pour disque > 8Go (NON_LBA_MAX_SECT) ????
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        begin
            Self.FHeadCount		:= 255;
            Self.FSectorPerTrack:= 63;
            Self.FCylinderCount := Self.pdTotalSectors div (Self.FHeadCount * Self.FSectorPerTrack);
            Self.pdTotalSectors := Self.FCylinderCount * 255 * 63;
        end;
        
        // Deux manières d'avoir le nombre de secteur du disque !!!!
        Self.FSectorCount := int64(dptemp.physsecLO) + int64(dptemp.physsecHI) * 4294967296;
    end
    else
    	Self.pdErrorCode := CANT_GET_DRIVE_PARAM;
end;


//
// Detruit l'objet
//
destructor TPhysicalDrive.Destroy;
begin
    inherited;

    //
    CloseHandle(Self.hdDevice);
end;


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


//
//
//
function TPhysicalDrive.isDriveRemovable(): boolean;
begin
	//Result := (Self.pdDriveParam.infoflags AND IFLAG_REMOVABLE <> 0);
  	Result := (Self.FDriveFlags AND IFLAG_REMOVABLE <> 0);
end;



//
// Renvoie les propriétés du disque dans une TString: couple (propriété, value)
//
procedure TPhysicalDrive.GetDriveProperties(info: TStrings);
begin
	inherited;

{	// Ajouter les infos specifique disque dur

    // Fixe / Removable
    info.Add('Status');
    if Self.isDriveRemovable then
	    info.Add('Removable')
    else
   	    info.Add('Fixed');

//    // Attachement Primary/Secondary Master/Slave
//    info.Add('Attachement');
//    info.Add('N/A');
}
    // Infos sur les partition ????

{    info.Add('wNumCyls');
    info.Add(IntToStr(wNumCyls));
    info.Add('wNumHeads');
    info.Add(IntToStr(wNumHeads));
    info.Add('wSectorsPerTrack');
    info.Add(IntToStr(wSectorsPerTrack));

    info.Add('wNumCurrentCyls');
    info.Add(IntToStr(wNumCurrentCyls));
    info.Add('wNumCurrentHeads');
    info.Add(IntToStr(wNumCurrentHeads));
    info.Add('wNumCurrentSectorsPerTrack');
    info.Add(IntToStr(wNumCurrentSectorsPerTrack));
    info.Add('ulTotalAddressableSectors');
    info.Add(IntToStr(ulTotalAddressableSectors)); }
end;



// ---------------------------------------------------------------------
//						Routines d'Entrées/Sorties
// ---------------------------------------------------------------------


//
// Create a handle to physical drive
//
procedure TPhysicalDrive.OpenPhysicalDrive();
begin
	if Self.hdDevice = 0 then
  begin
    Self.hdDevice := CreateFile(pchar('\\.\PhysicalDrive'+ IntToStr(Self.pdDriveNum)), GENERIC_READ, FILE_SHARE_READ {OR FILE_SHARE_WRITE},
    nil, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
    
    if Self.hdDevice = INVALID_HANDLE_VALUE then
    	MessageDlg('OpenPhysicalDrive', mtError, [mbOK], 0);
  end;
end;


//
// Lit un bloc de secteurs sur le disque physique
//
function TPhysicalDrive.ReadSectors(const lba: Cardinal; nbsect: Byte; buf: Pointer): boolean;
begin
	Result := ExtendedRead(Self.pdDriveNum, lba, nbsect, buf);
    if not Result then
    begin
    	Self.pdErrorCode := GetLastError;
        Self.pdSectorError := lba;
        //# Indique dans quelle suite de secteur il ya une erreur.
    end;
end;


//
// Read sectors to buffer (WinNT only)
//
function TPhysicalDrive.ReadNTSectors(const lba: Cardinal; nbsect: Cardinal; buf: Pointer): Cardinal;
var
  ldistancelow, ldistancehigh: dword;
  bytestoread, numread: dword;

begin
	// only for WinNT+
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
  	// open physical drive if not open
    Self.OpenPhysicalDrive;

    // go to the first sector to be read
    // #Todo1 Parameter calculation with real sector size
    ldistanceLow:=dword(LBA SHL 9);
    ldistanceHigh:=dword(LBA SHR (32-9));    
    if SetFilePointer(Self.hdDevice, ldistancelow, @ldistancehigh, FILE_BEGIN) <> INVALID_SET_FILE_POINTER then
    begin
	    // read sectors to buffer
      bytestoread := nbsect * Self.BytesPerSec;
      if ReadFile(Self.hdDevice, buf^, bytestoread, numread, nil) then
        Result := numread div Self.BytesPerSec
      else
      	Self.pdErrorCode := GetLastError();
    end;
  end;
end;


// ---------------------------------------------------------------------
//							Routines de detection
// ---------------------------------------------------------------------


//
//
//
class procedure TPhysicalDrive.EnumDrives(drivelist: TList);
const
	MAX_DRIVE = 7;
var
	i, startdrv: integer;
    hDevice: THandle;
    drive: TPhysicalDrive;
begin
	// Selon l'OS definir le numéro du premier disque
	// Actuellement sous Win9x, il faut commencer à le num à 128
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
		startdrv := $80
    else
		startdrv := 0;

    for i := 0 to MAX_DRIVE do
	begin
    	if Win32Platform = VER_PLATFORM_WIN32_NT then
        begin
        	hDevice := CreateFile(pchar('\\.\PhysicalDrive'+inttostr(i)), 0,
            							FILE_SHARE_READ OR FILE_SHARE_WRITE,
								        nil, OPEN_EXISTING, 0, 0);

        	if hDevice <> INVALID_HANDLE_VALUE then
			    drive := TPhysicalDrive.Create(i);

			CloseHandle(hDevice);
        end; 
    end;
// Comment determiner le nombre de disques et gestion W9x / 2K
// Sous NT/2K/XP, boucle sur CreateFile('\\.\PHYSICALDRIVE' & jusqu'à erreur ERROR_FILE_NOT_FOUND ????
// Sous W9x boucle sur 0 -> 7 ???
end;


//
// Verifie si le disque existe
//
{function isDriveExist(const drv: byte): boolean;
var
	drvparam: TDriveParams;
begin
    Result := GetDriveParams(drv, @drvparam);
end;}


//
// Verifie si le disque existe et si c'est un disque fixe
//
function isAFixedDrive(const drv: byte): boolean;
var
	drvparam: TDriveParams;
begin
    Result := GetDriveParams(drv, @drvparam) and
    		 ((drvparam.infoflags AND IFLAG_REMOVABLE) = 0);
end;


// ---------------------------------------------------------------------
//							Gestion du SMART
// ---------------------------------------------------------------------
//IMPORTANT NOTE for Windows 9x:
//
//  SMARTVSD.VXD MUST be installed: just copy it from \windows\system to
//  \windows\system\iosubsys and reboot.
const

// Miscellaneous
READ_ATTRIBUTE_BUFFER_SIZE	= 512;
IDENTIFY_BUFFER_SIZE		= 512;
READ_THRESHOLD_BUFFER_SIZE	= 512;

//
// IOCTL commands
//
DFP_GET_VERSION			= $00074080;
DFP_SEND_DRIVE_COMMAND	= $0007c084;
DFP_RECEIVE_DRIVE_DATA	= $0007c088;

type

//---------------------------------------------------------------------
// GETVERSIONOUTPARAMS contains the data returned from the
// Get Driver Version function.
//---------------------------------------------------------------------
TGetVetsionOutParams = packed record
    bVersion:		Byte;		// Binary driver version.
    bRevision:		Byte;		// Binary driver revision.
    bReserved:		Byte;		// Not used.
    bIDEDeviceMap:	Byte;		// Bit map of IDE devices.
    fCapabilities:	DWORD;		// Bit mask of driver capabilities.
    dwReserved: array[0..3] of DWORD;	// For future use.
end;
GETVERSIONOUTPARAMS = TGetVetsionOutParams;
PGETVERSIONOUTPARAMS = ^TGetVetsionOutParams;

//
// Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
//
const
CAP_IDE_ID_FUNCTION				= 1;	// ATA ID command supported
CAP_IDE_ATAPI_ID				= 2;	// ATAPI ID command supported
CAP_IDE_EXECUTE_SMART_FUNCTION	= 4;	// SMART commannds supported


//---------------------------------------------------------------------
// IDE registers
//---------------------------------------------------------------------
type

TIDERegs = packed record
    bFeaturesReg     : Byte; // Used for specifying SMART "commands".
    bSectorCountReg  : Byte; // IDE sector count register
    bSectorNumberReg : Byte; // IDE sector number register
    bCylLowReg       : Byte; // IDE low order cylinder value
    bCylHighReg      : Byte; // IDE high order cylinder value
    bDriveHeadReg    : Byte; // IDE drive/head register
    bCommandReg      : Byte; // Actual IDE command.
    bReserved        : Byte; // reserved for future use.  Must be zero.
end;
IDEREGS   = TIDERegs;
PIDERegs  = ^TIDERegs;


//---------------------------------------------------------------------
// SENDCMDINPARAMS contains the input parameters for the
// Send Command to Drive function.
//---------------------------------------------------------------------
TSendCmdInParams = packed record
    cBufferSize  : DWORD;                // Buffer size in bytes
    irDriveRegs  : TIDERegs;             // Structure with drive register values.
    bDriveNumber : Byte;                 // Physical drive number to send command to (0,1,2,3).
    bReserved    : Array[0..2] of Byte;  // Reserved for future expansion.
    dwReserved   : Array[0..3] of DWORD; // For future use.
    bBuffer      : Array[0..0] of Byte;  // Input buffer.
end;
SENDCMDINPARAMS   = TSendCmdInParams;
PSendCmdInParams  = ^TSendCmdInParams;

const
//
// Valid values for the bCommandReg member of IDEREGS.
//
IDE_ATAPI_ID				= $A1;	// Returns ID sector for ATAPI.
IDE_ID_FUNCTION				= $EC;	// Returns ID sector for ATA.
IDE_EXECUTE_SMART_FUNCTION	= $B0;	// Performs SMART cmd.
									// Requires valid bFeaturesReg,
									// bCylLowReg, and bCylHighReg
//
// Cylinder register values required when issuing SMART command
//
SMART_CYL_LOW =	$4F;
SMART_CYL_HI  =	$C2;


//---------------------------------------------------------------------
// Feature register defines for SMART "sub commands"
//---------------------------------------------------------------------
// Vendor specific commands:
SMART_ENABLE_SMART_OPERATIONS			= $D8;
SMART_DISABLE_SMART_OPERATIONS			= $D9;
SMART_RETURN_SMART_STATUS				= $DA;


//---------------------------------------------------------------------
// Status returned from driver
//---------------------------------------------------------------------
type
TDriverStatus = packed record
    bDriverError: Byte;		// Error code from driver,
                	        // or 0 if no error.
    bIDEStatus	: Byte;		// Contents of IDE Error register.
                      		// Only valid when bDriverError
                      		// is SMART_IDE_ERROR.
    bReserved	: array[0..1] of Byte;	// Reserved for future expansion.
    dwReserved	: array[0..1] of DWORD;	// Reserved for future expansion.
end;
DRIVERSTATUS	= TDriverStatus;
PDRIVERSTATUS	= ^TDriverStatus;

//
// bDriverError values
//
const
SMART_NO_ERROR			= 0;	// No error
SMART_IDE_ERROR			= 1;	// Error from IDE controller
SMART_INVALID_FLAG		= 2;	// Invalid command flag
SMART_INVALID_COMMAND	= 3;	// Invalid command byte
SMART_INVALID_BUFFER	= 4;	// Bad buffer (null, invalid addr..)
SMART_INVALID_DRIVE		= 5;	// Drive number not valid
SMART_INVALID_IOCTL		= 6;	// Invalid IOCTL
SMART_ERROR_NO_MEM		= 7;	// Could not lock user's buffer
SMART_INVALID_REGISTER	= 8;	// Some IDE Register not valid
SMART_NOT_SUPPORTED		= 9;	// Invalid cmd flag set
SMART_NO_IDE_DEVICE		= 10;	// Cmd issued to device not present

//---------------------------------------------------------------------
// Structure returned by SMART IOCTL for several commands
//---------------------------------------------------------------------
type

TSendCmdOutParams = packed record
    cBufferSize	: DWORD;				// Size of bBuffer in bytes
    DriverStatus: DRIVERSTATUS;			// Driver status structure.
    bBuffer		: array [0..0] of Byte;	// Buffer of arbitrary length in which to store
    									// the data read from the drive.
end;
SENDCMDOUTPARAMS   = TSendCmdOutParams;
PSENDCMDOUTPARAMS  = ^TSendCmdOutParams;


//---------------------------------------------------------------------
// The following struct defines the interesting part of the IDENTIFY
// buffer:
// http://www.ce-ata.org/docs/SprIDF05_CE-ATA.pdf
// http://www.hitachigst.com/tech/techlib.nsf/techdocs/85256AB8006A31E587256A7A006F9551/$file/dtta_sp.pdf (page 102+
// http://www.oryxmp3.com/pdf/ide_d1153r17.pdf
//---------------------------------------------------------------------
TIdSector = packed record
    wGenConfig                : Word;
    wNumCyls                  : Word;	// Number of logical cylinders (offset 1)
    wReserved                 : Word;
    wNumHeads                 : Word;	// Number of logical heads (offset 3)
    wBytesPerTrack            : Word;
    wBytesPerSector           : Word;
    wSectorsPerTrack          : Word;	// Number of logical sectors per logical track (offset 6)
    wVendorUnique             : Array[0..2] of Word;
    sSerialNumber             : Array[0..19] of Char;
    wBufferType               : Word;
    wBufferSize               : Word;
    wECCSize                  : Word;
    sFirmwareRev              : Array[0..7] of Char;
    sModelNumber              : Array[0..39] of Char;	// Offset Word 27 - 46
    wMoreVendorUnique         : Word;
    wDoubleWordIO             : Word;
    wCapabilities             : Word;
    wReserved1                : Word;
    wPIOTiming                : Word;
    wDMATiming                : Word;
    wBS                       : Word;
    wNumCurrentCyls           : Word;      // Number of current cylinders
    wNumCurrentHeads          : Word;      // Number of current heads
    wNumCurrentSectorsPerTrack: Word;      // Number of current sectors per track
    ulCurrentSectorCapacity   : ULONG;     // Current capacity in sectors (Word 57 specifies the low word)
    wMultSectorStuff          : Word;
    ulTotalAddressableSectors : ULONG;     // Total Number of User Addressable Sectors
    wSingleWordDMA            : Word;	// Retired (offset 62)
    wMultiWordDMA             : Word;	// (offset 63)
    // ---
    bReserved                 : Array[0..127] of Byte;
end;
PIdSector = ^TIdSector;


//---------------------------------------------------------------------
// Open SMART to allow DeviceIoControl communications.
//---------------------------------------------------------------------
function OpenSMART(const drvnum: Byte): Cardinal;
var
	hSMARTIOCTL: Cardinal;
    drivename: string;
begin
	hSMARTIOCTL := 0;

    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    begin
		// Version Windows 95 OSR2, Windows 98
        hSMARTIOCTL := CreateFile('\\.\SMARTVSD', 0, 0, 0, CREATE_NEW, 0, 0);
        if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        	// Unable to open SMARTVSD, error code: 0x%lX\n", GetLastError()
            ;
    end
    else
    begin
		// Windows NT, Windows 2000
    	drivename := '\\.\PhysicalDrive' + IntToStr(drvnum);

        hSMARTIOCTL := CreateFile(PChar(drivename), GENERIC_READ or GENERIC_WRITE,
        						  FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                                  OPEN_EXISTING, 0, 0);
		if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        	// Unable to open physical drive, error code: 0x%lX\n", GetLastError())
        	;
    end;
	Result := hSMARTIOCTL;
end;


{****************************************************************************
*
* DoEnableSMART
*
* FUNCTION: Send a SMART_ENABLE_SMART_OPERATIONS command to the drive
* bDriveNum = 0-3
*
****************************************************************************}
function DoEnableSMART(hSMARTIOCTL: Cardinal; pSCIP: PSENDCMDINPARAMS;
					   pSCOP: PSENDCMDOUTPARAMS; bDriveNum: BYTE;
                       var lpcbBytesReturned: Cardinal): LongBool;
begin
	//
	// Set up data structures for Enable SMART Command.
	//
	pSCIP.cBufferSize := 0;

	pSCIP.irDriveRegs.bFeaturesReg 		:= SMART_ENABLE_SMART_OPERATIONS;
	pSCIP.irDriveRegs.bSectorCountReg	:= 1;
	pSCIP.irDriveRegs.bSectorNumberReg	:= 1;
	pSCIP.irDriveRegs.bCylLowReg		:= SMART_CYL_LOW;
	pSCIP.irDriveRegs.bCylHighReg		:= SMART_CYL_HI;

	//
	// Compute the drive number.
	//
	pSCIP.irDriveRegs.bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
	pSCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
	pSCIP.bDriveNumber := bDriveNum;

    Result := DeviceIoControl(hSMARTIOCTL, DFP_SEND_DRIVE_COMMAND,
			                  Pointer(pSCIP), sizeof(SENDCMDINPARAMS) - 1,
            			      Pointer(pSCOP), sizeof(SENDCMDOUTPARAMS) - 1,
			                  lpcbBytesReturned, nil);
end;


{****************************************************************************
*
* DoIDENTIFY
*
* FUNCTION: Send an IDENTIFY command to the drive
* bDriveNum = 0-3
* bIDCmd = IDE_ID_FUNCTION or IDE_ATAPI_ID
*
****************************************************************************}
function DoIDENTIFY(hSMARTIOCTL: Cardinal; pSCIP: PSENDCMDINPARAMS;
					pSCOP: PSENDCMDOUTPARAMS; bIDCmd: BYTE; bDriveNum: BYTE;
                    var lpcbBytesReturned: Cardinal): LongBool;
begin
	//
	// Set up data structures for IDENTIFY command.
	//
	pSCIP.cBufferSize := IDENTIFY_BUFFER_SIZE;

	pSCIP.irDriveRegs.bFeaturesReg		:= 0;
	pSCIP.irDriveRegs.bSectorCountReg	:= 1;
	pSCIP.irDriveRegs.bSectorNumberReg	:= 1;
	pSCIP.irDriveRegs.bCylLowReg		:= 0;
	pSCIP.irDriveRegs.bCylHighReg		:= 0;

   	//
	// Compute the drive number.
	//
	pSCIP.irDriveRegs.bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);

	//
	// The command can either be IDE identify or ATAPI identify.
	//
	pSCIP.irDriveRegs.bCommandReg	:= bIDCmd;
	pSCIP.bDriveNumber		  		:= bDriveNum;
	pSCIP.cBufferSize				:= IDENTIFY_BUFFER_SIZE;

    Result := DeviceIoControl(hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
             				  Pointer(pSCIP), sizeof(SENDCMDINPARAMS) - 1,
                              Pointer(pSCOP), sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1,
                              lpcbBytesReturned, nil);     //#ATTN Var
end;


//
// Convertit les champs
//
procedure ChangeByteOrder( var Data; Size : Integer );
var
    ptr : PChar;
    i : Integer;
    c : Char;
begin
    ptr := @Data;
    for i := 0 to (Size shr 1)-1 do
    begin
        c := ptr^;
        ptr^ := (ptr+1)^;
        (ptr+1)^ := c;
        Inc(ptr,2);
    end;
end;


{****************************************************************************
*
* DisplayIdInfo
*
* Display the contents of the ID buffer
*
****************************************************************************}
procedure DisplayIdInfo(pids: PIDSECTOR; pSCIP: PSENDCMDINPARAMS; bIDCmd: Byte; bDfpDriveMap: Byte;
 bDriveNum: Byte; var modelname, serialname, firmname: string);
begin
	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids.sModelNumber, sizeof(pids.sModelNumber));
    SetString(modelname, pids.sModelNumber, Sizeof(pids.sModelNumber));
	modelname := trim(modelname);

	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids.sFirmwareRev, sizeof(pids.sFirmwareRev));
    SetString(firmname, pids.sFirmwareRev, Sizeof(pids.sFirmwareRev));
	firmname := trim(firmname);

	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids.sSerialNumber, sizeof(pids.sSerialNumber));
    SetString(serialname, pids.sSerialNumber, Sizeof(pids.sSerialNumber));
	serialname := trim(serialname);
end;


//
// Récupère les infos smart et mets à jour les champ Model, Serial, Firmware
//
procedure TPhysicalDrive.GetSmartInfo();
var
    AttrOutCmd:		array[0..sizeof(SENDCMDOUTPARAMS) + READ_ATTRIBUTE_BUFFER_SIZE - 1] of Byte;
    ThreshOutCmd:	array[0..sizeof(SENDCMDOUTPARAMS) + READ_THRESHOLD_BUFFER_SIZE - 1] of Byte;
    IdOutCmd:		array[0..sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1] of Byte;
	hSMARTIOCTL:	Cardinal;
    cbBytesReturned:DWORD;
    VersionParams:	GETVERSIONOUTPARAMS;
	scip:			SENDCMDINPARAMS;
	OutCmd:			SENDCMDOUTPARAMS;
    bDfpDriveMap:	Byte;
    bIDCmd:			Byte;
    infook:			boolean;
    drivenum:		Byte;
begin
	// Init
    hSMARTIOCTL := 0;
    bDfpDriveMap := 0;
    infook := false;
	//bSuccess := 1;
	drivenum := Self.pdDriveNum;

    // indique que l'on a récupérer les infos smart
    Self.pdFlags := Self.pdFlags or SMART_INFO_LOADED_FLAG;

	//
	// Try to get a handle to SMART IOCTL, report failure and exit if
	// can't.
	//
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    	drivenum := drivenum and $7f;

	hSMARTIOCTL := OpenSMART(drivenum);
    if (hSMARTIOCTL <> INVALID_HANDLE_VALUE) then
    begin
        //
        // Get the version, etc of SMART IOCTL
        //
        ZeroMemory(@VersionParams, sizeof(VersionParams));

        // Sortir si error ???????
        if ( DeviceIoControl(hSMARTIOCTL, DFP_GET_VERSION,
			nil,
			0,
			@VersionParams,
			sizeof(VersionParams),
			cbBytesReturned, nil) ) then
        begin
        end
        else
        begin
        end;

        //
        // If there is a IDE device at number "i" issue commands
        // to the device.
        //
        if ((VersionParams.bIDEDeviceMap shr drivenum) and 1) <> 0 then
        begin
            //
            // Try to enable SMART so we can tell if a drive supports it.
            // Ignore ATAPI devices.
            //

            if ((VersionParams.bIDEDeviceMap shr drivenum) and $10) = 0 then
            begin
                ZeroMemory(@scip, SizeOf(scip));
                ZeroMemory(@OutCmd, SizeOf(OutCmd));


                if (DoEnableSMART(hSMARTIOCTL,
                    @scip,
                    @OutCmd,
                    drivenum,
                    cbBytesReturned)) then
                begin
                    //
                    // Mark the drive as SMART enabled
                    //
                    bDfpDriveMap := bDfpDriveMap or (1 shl drivenum);
                end; // if (DoEnableSMART
            end; // if 

            //
            // Now, get the ID sector for all IDE devices in the system.
            // If the device is ATAPI use the IDE_ATAPI_ID command,
            // otherwise use the IDE_ID_FUNCTION command.
            //
            if ((VersionParams.bIDEDeviceMap shr drivenum) and $10) <> 0 then
                bIDCmd := IDE_ATAPI_ID
            else
                bIDCmd := IDE_ID_FUNCTION;

            //bIDCmd = (VersionParams.bIDEDeviceMap >> i & 0x10) ? \
            //	IDE_ATAPI_ID : IDE_ID_FUNCTION;

            ZeroMemory(@scip, SizeOf(scip));
            ZeroMemory(@IdOutCmd, SizeOf(IdOutCmd));

            if (DoIDENTIFY(hSMARTIOCTL,
                    @scip,
                    PSENDCMDOUTPARAMS(@IdOutCmd),
                    bIDCmd,
                    drivenum,
                    cbBytesReturned)) then
            begin
            	infook := true;
                // Récupère les infos du disque
                DisplayIdInfo(PIDSECTOR(@PSENDCMDOUTPARAMS(@IdOutCmd[0]).bBuffer[0]),
                        @scip,
                        bIDCmd,
                        bDfpDriveMap,
                        drivenum,
                        Self.pdModelNumber, Self.pdSerialNumber, Self.pdFirmwareRev);

            	// Info géometrie
                with PIDSECTOR(@PSENDCMDOUTPARAMS(@IdOutCmd[0]).bBuffer[0])^ do
                begin
                    Self.wNumCurrentCyls := wNumCurrentCyls;
                    Self.wNumCurrentHeads := wNumCurrentHeads;
                    Self.wNumCurrentSectorsPerTrack := wNumCurrentSectorsPerTrack;
                    Self.ulCurrentSectorCapacity := ulCurrentSectorCapacity;
                    Self.ulTotalAddressableSectors := ulTotalAddressableSectors;
                    Self.wNumCyls := wNumCyls;
                    Self.wNumHeads := wNumHeads;
                    Self.wSectorsPerTrack := wSectorsPerTrack;
                end;

            end; // if (DoIDENTIFY
        end; // if ((VersionParams

        // Ferme le handle
        CloseHandle(hSMARTIOCTL);
	end; // if (hSMARTIOCTL 
end;

// --------------------------- Fin de l'unité -----------------------------
end.
Géometrie disque 0 sous Win98
----------------------------
Disk size: 10141286400
Cylinders: 19650
Heads: 16
Sect per track: 63
Sector Count: 19807200
Sector Size: 512
----------------------------
Géometrie disque 0 sous XP
----------------------------
Model Number: IBM-DTTA-351010
Serial Number: WF0KFTF6658
Firmware Version: T56OA73A
Status: Fixed
Disk size: 10133544960
Cylinders: 1232
Heads: 255
Sect per track: 63
Sector Count: 19792080

