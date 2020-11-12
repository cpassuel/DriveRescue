// Extracting IDE(ATA) disk serial number.

// (c) 2000-2003 Alex Konshin mailto:akonshin@earthlink.net

// 30 Jul 2000 created
// 22 Oct 2003 refactoring

unit IdeSN;

interface

//-------------------------------------------------------------
// Tries to extract the serial number from the first IDE disk that is found in the system.
// Returns an empty string if IDE disk is not found.
  function GetIdeSN : String;

//-------------------------------------------------------------
// Tries to extract the serial number from specified IDE disk.
//
// Parameters:
//   ControllerNumber - SCSI port number of the controller.
//   DriveNumber - SCSI port number of the controller.
//
// Raises OSError exception in case of any error during this operation.
//
// Notes:
//  1. The parameter ControllerNumber is ignored on Windows 9x/ME platforms and should be 0.
//  2. This function CAN NOT extract SCSI disk serial number.
//
  function GetIdeDiskSerialNumber( ControllerNumber, DriveNumber : Integer ) : String;

//=============================================================
implementation

uses
  Windows,
  SysUtils; // only for Win32Platform, SysErrorMessage and class Exception

{$IFDEF VER150}
{$DEFINE VER140}
{$ENDIF}

{$IFNDEF VER140}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}

//-------------------------------------------------------------
// Tries to extract the serial number from specified IDE disk.
//
// Parameters:
//   ControllerNumber - SCSI port number of the controller.
//   DriveNumber - SCSI port number of the controller.
// Notes:
//  1. The parameter ControllerNumber is ignored on Windows 9x/ME platforms and should be 0.
//  2. This function CAN NOT extract SCSI disk serial number.
//
function GetIdeDiskSerialNumber( ControllerNumber, DriveNumber : Integer ) : String;
type
  TSrbIoControl = packed record
    HeaderLength : ULONG;
    Signature    : Array[0..7] of Char;
    Timeout      : ULONG;
    ControlCode  : ULONG;
    ReturnCode   : ULONG;
    Length       : ULONG;
  end;
  SRB_IO_CONTROL = TSrbIoControl;
  PSrbIoControl = ^TSrbIoControl;

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

  TIdSector = packed record
    wGenConfig                 : Word;
    wNumCyls                   : Word;
    wReserved                  : Word;
    wNumHeads                  : Word;
    wBytesPerTrack             : Word;
    wBytesPerSector            : Word;
    wSectorsPerTrack           : Word;
    wVendorUnique              : Array[0..2] of Word;
    sSerialNumber              : Array[0..19] of Char;
    wBufferType                : Word;
    wBufferSize                : Word;
    wECCSize                   : Word;
    sFirmwareRev               : Array[0..7] of Char;
    sModelNumber               : Array[0..39] of Char;
    wMoreVendorUnique          : Word;
    wDoubleWordIO              : Word;
    wCapabilities              : Word;
    wReserved1                 : Word;
    wPIOTiming                 : Word;
    wDMATiming                 : Word;
    wBS                        : Word;
    wNumCurrentCyls            : Word;
    wNumCurrentHeads           : Word;
    wNumCurrentSectorsPerTrack : Word;
    ulCurrentSectorCapacity    : ULONG;
    wMultSectorStuff           : Word;
    ulTotalAddressableSectors  : ULONG;
    wSingleWordDMA             : Word;
    wMultiWordDMA              : Word;
    bReserved                  : Array[0..127] of Byte;
  end;
  PIdSector = ^TIdSector;

const
  IDE_ID_FUNCTION = $EC;
  IDENTIFY_BUFFER_SIZE       = 512;
  DFP_RECEIVE_DRIVE_DATA        = $0007c088;
  IOCTL_SCSI_MINIPORT           = $0004d008;
  IOCTL_SCSI_MINIPORT_IDENTIFY  = $001b0501;
  DataSize = sizeof(TSendCmdInParams)-1+IDENTIFY_BUFFER_SIZE;
  BufferSize = SizeOf(SRB_IO_CONTROL)+DataSize;
  W9xBufferSize = IDENTIFY_BUFFER_SIZE+16;
var
  hDevice : THandle;
  cbBytesReturned : DWORD;
  s : String;
  pInData : PSendCmdInParams;
  pOutData : Pointer; // PSendCmdInParams;
  Buffer : Array[0..BufferSize-1] of Byte;
  srbControl : TSrbIoControl absolute Buffer;

  procedure ChangeByteOrder( var Data; Size : Integer );
  var ptr : PChar;
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

begin
  Result := '';
  FillChar(Buffer,BufferSize,#0);
  if Win32Platform=VER_PLATFORM_WIN32_NT then
    begin // Windows NT, Windows 2000
      Str(ControllerNumber,s);
      // Get SCSI port handle
      hDevice := CreateFile(
        PChar('\\.\Scsi'+s+':'),
        GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
      if hDevice=INVALID_HANDLE_VALUE then RaiseLastOSError;
      try
        srbControl.HeaderLength := SizeOf(SRB_IO_CONTROL);
        System.Move('SCSIDISK',srbControl.Signature,8);
        srbControl.Timeout      := 2;
        srbControl.Length       := DataSize;
        srbControl.ControlCode  := IOCTL_SCSI_MINIPORT_IDENTIFY;
        pInData := PSendCmdInParams(PChar(@Buffer)+SizeOf(SRB_IO_CONTROL));
        pOutData := pInData;
        with pInData^ do
        begin
          cBufferSize  := IDENTIFY_BUFFER_SIZE;
          bDriveNumber := DriveNumber;
          with irDriveRegs do
          begin
            bFeaturesReg     := 0;
            bSectorCountReg  := 1;
            bSectorNumberReg := 1;
            bCylLowReg       := 0;
            bCylHighReg      := 0;
            bDriveHeadReg    := $A0 or ((DriveNumber and 1) shl 4);
            bCommandReg      := IDE_ID_FUNCTION;
          end;
        end;
        if not DeviceIoControl( hDevice, IOCTL_SCSI_MINIPORT, @Buffer, BufferSize, @Buffer, BufferSize, cbBytesReturned, nil ) then RaiseLastOSError;
      finally
        CloseHandle(hDevice);
      end;
    end
  else
    begin // Windows 95 OSR2, Windows 98
      hDevice := CreateFile( '\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0 );
      if hDevice=INVALID_HANDLE_VALUE then RaiseLastOSError;
      try
        pInData := PSendCmdInParams(@Buffer);
        pOutData := PChar(@pInData^.bBuffer);
        with pInData^ do
        begin
          cBufferSize  := IDENTIFY_BUFFER_SIZE;
          bDriveNumber := DriveNumber;
          with irDriveRegs do
          begin
            bFeaturesReg     := 0;
            bSectorCountReg  := 1;
            bSectorNumberReg := 1;
            bCylLowReg       := 0;
            bCylHighReg      := 0;
            bDriveHeadReg    := $A0 or ((DriveNumber and 1) shl 4);
            bCommandReg      := IDE_ID_FUNCTION;
          end;
        end;
        if not DeviceIoControl( hDevice, DFP_RECEIVE_DRIVE_DATA, pInData, SizeOf(TSendCmdInParams)-1, pOutData, W9xBufferSize, cbBytesReturned, nil ) then RaiseLastOSError;
      finally
        CloseHandle(hDevice);
      end;
    end;

  with PIdSector(PChar(pOutData)+16)^ do
  begin
    ChangeByteOrder(sSerialNumber,SizeOf(sSerialNumber));
    SetString(Result,sSerialNumber,SizeOf(sSerialNumber));
  end;

  Result := Trim(Result);
end;


//-------------------------------------------------------------
function GetIdeSN : String;
var
  iController, iDrive, maxController : Integer;
begin
  Result := '';
  maxController := 15;
  if Win32Platform<>VER_PLATFORM_WIN32_NT then maxController := 0;
  for iController := 0 to maxController do
  begin
    for iDrive := 0 to 4 do
    begin
      try
        Result := GetIdeDiskSerialNumber(iController,iDrive);
        if Result<>'' then Exit;
      except
        // ignore exceptions
      end;
    end;
  end;
end;

end.


///*+++
//HDID.CPP
//Written by Lu Lin
//http://lu0.126.com
//2000.11.3
//---*/
//#include <windows.h>
//#include <iostream.h>
//#include <stdio.h>
//
//#define DFP_GET_VERSION 0x00074080
//#define DFP_SEND_DRIVE_COMMAND 0x0007c084
//#define DFP_RECEIVE_DRIVE_DATA 0x0007c088
//
//#pragma pack(1)
//typedef struct _GETVERSIONOUTPARAMS {
//BYTE bVersion;  // Binary driver version.
//BYTE bRevision;  // Binary driver revision.
//BYTE bReserved;  // Not used.
//BYTE bIDEDeviceMap; // Bit map of IDE devices. 
//DWORD fCapabilities; // Bit mask of driver capabilities. 
//DWORD dwReserved[4]; // For future use. 
//} GETVERSIONOUTPARAMS, *PGETVERSIONOUTPARAMS, *LPGETVERSIONOUTPARAMS; 
//
//typedef struct _IDEREGS { 
//BYTE bFeaturesReg;  // Used for specifying SMART "commands". 
//BYTE bSectorCountReg; // IDE sector count register 
//BYTE bSectorNumberReg; // IDE sector number register 
//BYTE bCylLowReg;   // IDE low order cylinder value 
//BYTE bCylHighReg;  // IDE high order cylinder value 
//BYTE bDriveHeadReg;  // IDE drive/head register 
//BYTE bCommandReg;  // Actual IDE command. 
//BYTE bReserved;   // reserved for future use.  Must be zero. 
//} IDEREGS, *PIDEREGS, *LPIDEREGS; 
//
//typedef struct _SENDCMDINPARAMS { 
//DWORD cBufferSize;  // Buffer size in bytes 
//IDEREGS irDriveRegs;  // Structure with drive register values. 
//BYTE bDriveNumber;  // Physical drive number to send 
//        // command to (0,1,2,3).
//BYTE bReserved[3];  // Reserved for future expansion. 
//DWORD dwReserved[4];  // For future use. 
////BYTE  bBuffer[1];   // Input buffer. 
//} SENDCMDINPARAMS, *PSENDCMDINPARAMS, *LPSENDCMDINPARAMS; 
//
//typedef struct _DRIVERSTATUS { 
//BYTE bDriverError;  // Error code from driver, 
//        // or 0 if no error. 
//BYTE bIDEStatus;   // Contents of IDE Error register. 
//        // Only valid when bDriverError 
//        // is SMART_IDE_ERROR. 
//BYTE bReserved[2];  // Reserved for future expansion. 
//DWORD dwReserved[2];  // Reserved for future expansion. 
//} DRIVERSTATUS, *PDRIVERSTATUS, *LPDRIVERSTATUS; 
//
//typedef struct _SENDCMDOUTPARAMS { 
//DWORD    cBufferSize;  // Size of bBuffer in bytes 
//DRIVERSTATUS DriverStatus;  // Driver status structure. 
//BYTE   bBuffer[512];   // Buffer of arbitrary length 
//          // in which to store the data read from the drive. 
//} SENDCMDOUTPARAMS, *PSENDCMDOUTPARAMS, *LPSENDCMDOUTPARAMS;
//
//typedef struct _IDSECTOR { 
//USHORT wGenConfig; 
//USHORT wNumCyls; 
//USHORT wReserved; 
//USHORT wNumHeads; 
//USHORT wBytesPerTrack; 
//USHORT wBytesPerSector; 
//USHORT wSectorsPerTrack; 
//USHORT wVendorUnique[3]; 
//CHAR sSerialNumber[20]; 
//USHORT wBufferType; 
//USHORT wBufferSize; 
//USHORT wECCSize; 
//CHAR sFirmwareRev; 
//CHAR sModelNumber[40]; 
//USHORT wMoreVendorUnique; 
//USHORT wDoubleWordIO; 
//USHORT wCapabilities; 
//USHORT wReserved1; 
//USHORT wPIOTiming;
//USHORT wDMATiming; 
//USHORT wBS; 
//USHORT wNumCurrentCyls; 
//USHORT wNumCurrentHeads; 
//USHORT wNumCurrentSectorsPerTrack; 
//ULONG ulCurrentSectorCapacity; 
//USHORT wMultSectorStuff; 
//ULONG ulTotalAddressableSectors; 
//USHORT wSingleWordDMA; 
//USHORT wMultiWordDMA; 
//BYTE bReserved[128]; 
//} IDSECTOR, *PIDSECTOR; 
//
///*+++ 
//Global vars 
//---*/ 
//GETVERSIONOUTPARAMS vers; 
//SENDCMDINPARAMS in; 
//SENDCMDOUTPARAMS out; 
//HANDLE h; 
//DWORD i;
//BYTE j; 
//
//
//VOID ChangeByteOrder(PCHAR szString, USHORT uscStrSize)
//{ 
//
//USHORT i; 
//CHAR temp; 
//
//for (i = 0; i < uscStrSize; i+=2) 
//{ 
//  temp = szString[i]; 
//  szString[i] = szString[i+1]; 
//  szString[i+1] = temp; 
//} 
//} 
//
//void DetectIDE(BYTE bIDEDeviceMap){ 
//if (bIDEDeviceMap&1){ 
//  if (bIDEDeviceMap&16){ 
//   cout<<"ATAPI device is attached to primary controller, drive 0."<<endl; 
//  }else{ 
//   cout<<"IDE device is attached to primary controller, drive 0."<<endl; 
//  } 
//} 
//if (bIDEDeviceMap&2){ 
//  if (bIDEDeviceMap&32){ 
//   cout<<"ATAPI device is attached to primary controller, drive 1."<<endl; 
//  }else{ 
//   cout<<"IDE device is attached to primary controller, drive 1."<<endl; 
//  } 
//} 
//if (bIDEDeviceMap&4){ 
//  if (bIDEDeviceMap&64){ 
//   cout<<"ATAPI device is attached to secondary controller, drive 0."<<endl;
//  }else{
//   cout<<"IDE device is attached to secondary controller, drive 0."<<endl;
//  }
//}
//if (bIDEDeviceMap&8){
//  if (bIDEDeviceMap&128){
//   cout<<"ATAPI device is attached to secondary controller, drive 1."<<endl;
//  }else{
//   cout<<"IDE device is attached to secondary controller, drive 1."<<endl;
//  }
//}
//}
//
//void hdid9x()
//{
//	ZeroMemory(&vers,sizeof(vers));
//	//We start in 95/98/Me
//	h=CreateFile("\\\\.\\Smartvsd",0,0,0,CREATE_NEW,0,0);
//	if (!h)
//    {
//	  cout<<"open smartvsd.vxd failed"<<endl;
//	  exit(0);
//	}
//
//	if (!DeviceIoControl(h,DFP_GET_VERSION,0,0,&vers,sizeof(vers),&i,0))
//    {
//	  cout<<"DeviceIoControl failed:DFP_GET_VERSION"<<endl;
//	  CloseHandle(h);
//	  return;
//	}
//
//	//If IDE identify command not supported, fails
//	if (!(vers.fCapabilities&1))
//    {
//	  cout<<"Error: IDE identify command not supported.";
//	  CloseHandle(h);
//	  return;
//	}
//     
//	//Display IDE drive number detected
//	DetectIDE(vers.bIDEDeviceMap);
//
//	//Identify the IDE drives 
//	for (j=0;j<4;j++)
//    { 
//		PIDSECTOR phdinfo;
//		char s[41];
//
//		ZeroMemory(&in,sizeof(in));
//		ZeroMemory(&out,sizeof(out));
//		if (j&1)
//	    {
//			in.irDriveRegs.bDriveHeadReg=0xb0;
//		}
//		else
//	    {
//			in.irDriveRegs.bDriveHeadReg=0xa0;
//		}
//
//		if (vers.fCapabilities&(16>>j))
//        { 
//			//We don't detect a ATAPI device.
//		   	cout<<"Drive "<<(int)(j+1)<<" is a ATAPI device, we don't detect it"<<endl;
//			continue; 
//		}
//        else
//        { 
//			in.irDriveRegs.bCommandReg=0xec; 
//		}
//         
//		in.bDriveNumber=j; 
//		in.irDriveRegs.bSectorCountReg=1; 
//		in.irDriveRegs.bSectorNumberReg=1; 
//		in.cBufferSize=512; 
//		if (!DeviceIoControl(h,DFP_RECEIVE_DRIVE_DATA,&in,sizeof(in),&out,sizeof(out),&i,0))
//        { 
//			cout<<"DeviceIoControl failed:DFP_RECEIVE_DRIVE_DATA"<<endl; 
//			CloseHandle(h); 
//			return; 
//		}
//		phdinfo=(PIDSECTOR)out.bBuffer; 
//		memcpy(s,phdinfo->sModelNumber,40); 
//		s[40]=0;
//		ChangeByteOrder(s,40); 
//		cout<<endl<<"Module Number:"<<s<<endl; 
//		memcpy(s,phdinfo->sFirmwareRev,8); 
//		s=0; 
//		ChangeByteOrder(s,8); 
//		cout<<"\tFirmware rev:"<<s<<endl; 
//		memcpy(s,phdinfo->sSerialNumber,20); 
//		s[20]=0; 
//		ChangeByteOrder(s,20); 
//		cout<<"\tSerial Number:"<<s<<endl; 
//		cout<<"\tCapacity:"<<phdinfo->ulTotalAddressableSectors/2/1024<<"M"<<endl<<endl; 
//	} //for
//
//	//Close handle before quit
//	CloseHandle(h);
//}
//
//

uses
	Classes;
    
procedure hdidnt();
var
	hd: array[0..79] of char;
    s: array[0..40] of char;
    phdinfo: PIdSector; 
begin
	ZeroMemory()

//void hdidnt()
//{
//	char hd[80];
//	PIDSECTOR phdinfo;
//	char s[41];
//
//	ZeroMemory(&vers,sizeof(vers));
//	//We start in NT/Win2000
//	for (j=0;j<4;j++)
//    {
//		sprintf(hd,"\\\\.\\PhysicalDrive%d",j);
//		h=CreateFile(hd,GENERIC_READ|GENERIC_WRITE,
//		FILE_SHARE_READ|FILE_SHARE_WRITE,0,OPEN_EXISTING,0,0);
//		if (!h)
//        {
//		   continue;
//		}
//		if (!DeviceIoControl(h,DFP_GET_VERSION,0,0,&vers,sizeof(vers),&i,0))
//        {
//			CloseHandle(h);
//			continue;
//		}
//         
//		//If IDE identify command not supported, fails 
//		if (!(vers.fCapabilities&1))
//        { 
//			cout<<"Error: IDE identify command not supported."; 
//			CloseHandle(h); 
//			return;
//		} 
//		//Identify the IDE drives 
//		ZeroMemory(&in,sizeof(in)); 
//		ZeroMemory(&out,sizeof(out)); 
//		if (j&1)
//        {
//			in.irDriveRegs.bDriveHeadReg=0xb0; 
//		}
//        else
//        { 
//			in.irDriveRegs.bDriveHeadReg=0xa0; 
//		} 
//
//		if (vers.fCapabilities&(16>>j))
//        { 
//			//We don't detect a ATAPI device. 
//			cout<<"Drive "<<(int)(j+1)<<" is a ATAPI device, we don't detect it"<<endl; 
//			continue; 
//		}
//        else
//        {
//			in.irDriveRegs.bCommandReg=0xec; 
//		}
//         
//		in.bDriveNumber=j; 
//		in.irDriveRegs.bSectorCountReg=1; 
//		in.irDriveRegs.bSectorNumberReg=1; 
//		in.cBufferSize=512; 
//		if (!DeviceIoControl(h,DFP_RECEIVE_DRIVE_DATA,&in,sizeof(in),&out,sizeof(out),&i,0))
//        {
//			cout<<"DeviceIoControl failed:DFP_RECEIVE_DRIVE_DATA"<<endl; 
//			CloseHandle(h); 
//			return; 
//		}
//		phdinfo=(PIDSECTOR)out.bBuffer; 
//		memcpy(s,phdinfo->sModelNumber,40); 
//		s[40]=0; 
//		ChangeByteOrder(s,40); 
//		cout<<endl<<"Module Number:"<<s<<endl; 
//		memcpy(s,phdinfo->sFirmwareRev,8); 
//		s=0; 
//		ChangeByteOrder(s,8); 
//		cout<<"\tFirmware rev:"<<s<<endl; 
//		memcpy(s,phdinfo->sSerialNumber,20); 
//		s[20]=0; 
//		ChangeByteOrder(s,20); 
//		cout<<"\tSerial Number:"<<s<<endl; 
//		cout<<"\tCapacity:"<<phdinfo->ulTotalAddressableSectors/2/1024<<"M"<<endl<<endl; 
//		CloseHandle(h); 
//	} // for 
//} 
//
//void main(){ 
//OSVERSIONINFO VersionInfo; 
//
//ZeroMemory(&VersionInfo,sizeof(VersionInfo)); 
//VersionInfo.dwOSVersionInfoSize=sizeof(VersionInfo); 
//GetVersionEx(&VersionInfo); 
//
//switch (VersionInfo.dwPlatformId){ 
//case VER_PLATFORM_WIN32s:
//  cout<<"Win32s is not supported by this programm."<<endl;
//  return;
//case VER_PLATFORM_WIN32_WINDOWS:
//  hdid9x();
//  return;
//case VER_PLATFORM_WIN32_NT:
//  hdidnt();
//  return;
//}


// Slt pour W9x
// Verif IDE/ATAPI
// Verif Primary/Secondary
// Verif Master/Slave
// 4 bits poid fort pour flag IDE/ATAPI (1 = ATAPI, 0 = IDE)
//
//TControllerType = (ctIDE, ctATAPI);
//TControllerAttachment = (caPrimary, caSecondary);
//TDrivePosition = (dpMaster, dpSlave);
// Variable
// Controller : primary, secondary, tertiary, quadra
// Type : IDE, ATAPI
// Position: Master/Slave
//
procedure DetectIDE(bIDEDeviceMap: BYTE);
begin
	if (bIDEDeviceMap and 1) then
    begin
    	FController := primary;
        FPosition	:= dpMaster;
        ContrType	:= (bIDEDeviceMap shr 4) and 1;
    end;

   	if (bIDEDeviceMap and 2) then
    begin
    	FController := primary;
        FPosition	:= dpSlave;
        ContrType	:= (bIDEDeviceMap shr 5) and 1;
    end;

   	if (bIDEDeviceMap and 4) then
    begin
    	FController := secondary;
        FPosition	:= dpMaster;
        ContrType	:= (bIDEDeviceMap shr 6) and 1;
    end;

   	if (bIDEDeviceMap and 8) then
    begin
    	FController := secondary;
        FPosition	:= dpSlave;
        ContrType	:= (bIDEDeviceMap shr 7) and 1;
    end;
end;


//void DetectIDE(BYTE bIDEDeviceMap)
//{
//	if (bIDEDeviceMap&1) // bit 0
//    {
//  		if (bIDEDeviceMap&16) // bit 4
//        {
//		   cout<<"ATAPI device is attached to primary controller, drive 0."<<endl;
//		}
//        else
//        {
//		   cout<<"IDE device is attached to primary controller, drive 0."<<endl;
//		}
//	}
//
//	if (bIDEDeviceMap&2) // bit 1
//    {
//		if (bIDEDeviceMap&32) // bit 5
//        {
//			cout<<"ATAPI device is attached to primary controller, drive 1."<<endl;
//		}
//        else
//        {
//			cout<<"IDE device is attached to primary controller, drive 1."<<endl;
//		}
//	}
//
//	if (bIDEDeviceMap&4)
//    {
//		if (bIDEDeviceMap&64)
//        {
//		   cout<<"ATAPI device is attached to secondary controller, drive 0."<<endl;
//		}
//        else
//        {
//			cout<<"IDE device is attached to secondary controller, drive 0."<<endl;
//		}
//	}
//
//	if (bIDEDeviceMap&8)
//    {
//		if (bIDEDeviceMap&128)
//        {
//		   cout<<"ATAPI device is attached to secondary controller, drive 1."<<endl;
//		}
//        else
//        {
//		   cout<<"IDE device is attached to secondary controller, drive 1."<<endl;
//		}
//	}
//}

// Marche avec W98 ????

// Get first IDE harddisk serial number
function GetIdeSerialNumber : string;

const IDENTIFY_BUFFER_SIZE = 512;

type

TIDERegs = packed record
    bFeaturesReg     : BYTE; // Used for specifying SMART "commands".
    bSectorCountReg  : BYTE; // IDE sector count register
    bSectorNumberReg : BYTE; // IDE sector number register
    bCylLowReg       : BYTE; // IDE low order cylinder value
    bCylHighReg      : BYTE; // IDE high order cylinder value
    bDriveHeadReg    : BYTE; // IDE drive/head register
    bCommandReg      : BYTE; // Actual IDE command.
    bReserved        : BYTE; // reserved for future use.  Must be zero.
end;


TSendCmdInParams = packed record
    // Buffer size in bytes
    cBufferSize  : DWORD;
    // Structure with drive register values.
    irDriveRegs  : TIDERegs;
    // Physical drive number to send command to (0,1,2,3).
    bDriveNumber : BYTE;
    bReserved    : Array[0..2] of Byte;
    dwReserved   : Array[0..3] of DWORD;
    bBuffer      : Array[0..0] of Byte;  // Input buffer.
end;

TIdSector = packed record
   wGenConfig                 : Word;
   wNumCyls                   : Word;
   wReserved                  : Word;
   wNumHeads                  : Word;
   wBytesPerTrack             : Word;
   wBytesPerSector            : Word;
   wSectorsPerTrack           : Word;
   wVendorUnique              : Array[0..2] of Word;
   sSerialNumber              : Array[0..19] of CHAR;
   wBufferType                : Word;
   wBufferSize                : Word;
   wECCSize                   : Word;
   sFirmwareRev               : Array[0..7] of Char;
   sModelNumber               : Array[0..39] of Char;
   wMoreVendorUnique          : Word;
   wDoubleWordIO              : Word;
   wCapabilities              : Word;
   wReserved1                 : Word;
   wPIOTiming                 : Word;
   wDMATiming                 : Word;
   wBS                        : Word;
   wNumCurrentCyls            : Word;
   wNumCurrentHeads           : Word;
   wNumCurrentSectorsPerTrack : Word;
   ulCurrentSectorCapacity    : DWORD;
   wMultSectorStuff           : Word;
   ulTotalAddressableSectors  : DWORD;
   wSingleWordDMA             : Word;
   wMultiWordDMA              : Word;
   bReserved                  : Array[0..127] of BYTE;
end;
PIdSector = ^TIdSector;

TDriverStatus = packed record
   // Error code from driver, or 0 if no error.
   bDriverError : Byte;
   // Contents of IDE Error register. Only valid when bDriverError is SMART_IDE_ERROR.
   bIDEStatus   : Byte;
   bReserved    : Array[0..1] of Byte;
   dwReserved   : Array[0..1] of DWORD;
end;

TSendCmdOutParams = packed record
   // Size of bBuffer in bytes
   cBufferSize  : DWORD;
   // Driver status structure.
   DriverStatus : TDriverStatus;
   // Buffer of arbitrary length in which to store the data read from the drive.
   bBuffer      : Array[0..0] of BYTE;
end;

var
	hDevice : THandle;
   cbBytesReturned : DWORD;
   ptr : PChar;
   SCIP : TSendCmdInParams;
   aIdOutCmd : Array [0..(SizeOf(TSendCmdOutParams)+IDENTIFY_BUFFER_SIZE-1)-1] of Byte;
   IdOutCmd  : TSendCmdOutParams absolute aIdOutCmd;

 procedure ChangeByteOrder( var Data; Size : Integer );
 var ptr : PChar;
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

begin
 Result := ''; // return empty string on error
 if SysUtils.Win32Platform=VER_PLATFORM_WIN32_NT then // Windows NT, Windows 2000
   begin
     // warning! change name for other drives: ex.: second drive '\\.\PhysicalDrive1\'
     hDevice := CreateFile( '\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
       FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
   end
 else // Version Windows 95 OSR2, Windows 98
   hDevice := CreateFile( '\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0 );
 if hDevice=INVALID_HANDLE_VALUE then Exit;
 try
   FillChar(SCIP,SizeOf(TSendCmdInParams)-1,#0);
   FillChar(aIdOutCmd,SizeOf(aIdOutCmd),#0);
   cbBytesReturned := 0;
   // Set up data structures for IDENTIFY command.
   with SCIP do
   begin
     cBufferSize  := IDENTIFY_BUFFER_SIZE;
//      bDriveNumber := 0;
     with irDriveRegs do
     begin
       bSectorCountReg  := 1;
       bSectorNumberReg := 1;
//      if Win32Platform=VER_PLATFORM_WIN32_NT then bDriveHeadReg := $A0
//      else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
       bDriveHeadReg    := $A0;
       bCommandReg      := $EC;
     end;
   end;
   if not DeviceIoControl( hDevice, $0007c088, @SCIP, SizeOf(TSendCmdInParams)-1,
     @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil ) then Exit;
 finally
   CloseHandle(hDevice);
 end;
 with PIdSector(@IdOutCmd.bBuffer)^ do
 begin
   ChangeByteOrder( sSerialNumber, SizeOf(sSerialNumber) );
   (PChar(@sSerialNumber)+SizeOf(sSerialNumber))^ := #0;
   Result := PChar(@sSerialNumber);
 end;
end;

// for mor information about S.M.A.R.T. ioctl see
//  http://www.microsoft.com/hwdev/download/respec/iocltapi.rtf

// see also sample SmartApp from MSDN Knowledge Base
//  Windows Development -> Win32 Device Driver Kit ->
//  SAMPLE: SmartApp.exe Accesses SMART stats in IDE drives

// see also http://www.mtgroup.ru/~alexk
//  IdeInfo.zip - sample delphi application using S.M.A.R.T. Ioctl API

// Notice:

//  WinNT/Win2000 - you must have read/WRITE access right to harddisk

//  Win98
//    SMARTVSD.VXD must be installed in \windows\system\iosubsys
//    (Do not forget to reboot after copying)
