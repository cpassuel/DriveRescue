unit SmartUnit;

interface

Uses
	Windows, SysUtils, Types, Classes;


const

// Miscellaneous
MAX_IDE_DRIVES				= 4;	// Max number of drives assuming primary/secondary, master/slave topology
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
LPGETVERSIONOUTPARAMS = ^TGetVetsionOutParams;

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
LPIDEREGS = ^TIDERegs;


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
LPSENDCMDINPARAMS = ^TSendCmdInParams;

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
LPDRIVERSTATUS	= ^TDriverStatus;

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
						  		// although drive number is valid
// 11-255 reserved

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
LPSENDCMDOUTPARAMS = ^TSendCmdOutParams;

//---------------------------------------------------------------------
// Feature register defines for SMART "sub commands"
//---------------------------------------------------------------------
const
SMART_READ_ATTRIBUTE_VALUES				= $D0;	// ATA4: Renamed
												// SMART READ DATA
SMART_READ_ATTRIBUTE_THRESHOLDS			= $D1;	// Obsoleted in ATA4!
SMART_ENABLE_DISABLE_ATTRIBUTE_AUTOSAVE	= $D2;
SMART_SAVE_ATTRIBUTE_VALUES				= $D3;
SMART_EXECUTE_OFFLINE_IMMEDIATE			= $D4;	// ATA4
// Vendor specific commands:
SMART_ENABLE_SMART_OPERATIONS			= $D8;
SMART_DISABLE_SMART_OPERATIONS			= $D9;
SMART_RETURN_SMART_STATUS				= $DA;


//---------------------------------------------------------------------
// The following structure defines the structure of a Drive Attribute
//---------------------------------------------------------------------
type
TDriveAttribute = packed record
    bAttrID		: Byte;		// Identifies which attribute
    wStatusFlags: Word;		// see bit definitions below
    bAttrValue	: Byte;		// Current normalized value
    bWorstValue	: Byte;		// How bad has it ever been?
    bRawValue	: array[0..5] of Byte;	// Un-normalized value
    bReserved	: Byte;		// ...
end;
DRIVEATTRIBUTE	 = TDriveAttribute;
PDRIVEATTRIBUTE  = ^TDriveAttribute;
LPDRIVEATTRIBUTE = ^TDriveAttribute;

//---------------------------------------------------------------------
// The following structure defines the structure of a Warranty Threshold
// Obsoleted in ATA4!
//---------------------------------------------------------------------
TAttrThreshold = packed record
    bAttrID				: Byte;		// Identifies which attribute
    bWarrantyThreshold	: Byte;		// Triggering value
    bReserved			: array[0..9] of Byte;		// ...
end;
ATTRTHRESHOLD	= TAttrThreshold;
PATTRTHRESHOLD	= ^TAttrThreshold;
LPATTRTHRESHOLD	= ^TAttrThreshold;


//---------------------------------------------------------------------
// The following struct defines the interesting part of the IDENTIFY
// buffer:
//---------------------------------------------------------------------
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

//---------------------------------------------------------------------
// Valid Attribute IDs
//---------------------------------------------------------------------
const
ATTR_INVALID				= 0;
ATTR_READ_ERROR_RATE		= 1;
ATTR_THROUGHPUT_PERF		= 2;
ATTR_SPIN_UP_TIME			= 3;
ATTR_START_STOP_COUNT		= 4;
ATTR_REALLOC_SECTOR_COUNT	= 5;
ATTR_READ_CHANNEL_MARGIN	= 6;
ATTR_SEEK_ERROR_RATE		= 7;
ATTR_SEEK_TIME_PERF			= 8;
ATTR_POWER_ON_HRS_COUNT		= 9;
ATTR_SPIN_RETRY_COUNT		= 10;
ATTR_CALIBRATION_RETRY_COUNT= 11;
ATTR_POWER_CYCLE_COUNT		= 12;

//---------------------------------------------------------------------
// Status Flags Values
//---------------------------------------------------------------------
PRE_FAILURE_WARRANTY		= $1;
ON_LINE_COLLECTION			= $2;
PERFORMANCE_ATTRIBUTE		= $4;
ERROR_RATE_ATTRIBUTE		= $8;
EVENT_COUNT_ATTRIBUTE		= $10;
SELF_PRESERVING_ATTRIBUTE	= $20;

NUM_ATTRIBUTE_STRUCTS		= 30;


procedure main();

// =========================================================================
//								IMPLEMENTATION
// =========================================================================
implementation

uses
	Unit1;
//
// Define global buffers.
//
var
AttrOutCmd:		array[0..sizeof(SENDCMDOUTPARAMS) + READ_ATTRIBUTE_BUFFER_SIZE - 1] of Byte;
ThreshOutCmd:	array[0..sizeof(SENDCMDOUTPARAMS) + READ_THRESHOLD_BUFFER_SIZE - 1] of Byte;
IdOutCmd:		array[0..sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1] of Byte;

//BYTE	AttrOutCmd[sizeof(SENDCMDOUTPARAMS) + READ_ATTRIBUTE_BUFFER_SIZE - 1];
//BYTE	ThreshOutCmd[sizeof(SENDCMDOUTPARAMS) + READ_THRESHOLD_BUFFER_SIZE - 1];
//BYTE	IdOutCmd[sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1];


function DoIDENTIFY(hSMARTIOCTL: Cardinal; pSCIP: PSENDCMDINPARAMS;
					pSCOP: PSENDCMDOUTPARAMS; bIDCmd: BYTE; bDriveNum: BYTE;
                    var lpcbBytesReturned: Cardinal): LongBool; forward;

function DoEnableSMART(hSMARTIOCTL: Cardinal; pSCIP: PSENDCMDINPARAMS;
					   pSCOP: PSENDCMDOUTPARAMS; bDriveNum: BYTE;
                       var lpcbBytesReturned: Cardinal): LongBool; forward;

procedure DisplayIdInfo(pids: PIDSECTOR; pSCIP: PSENDCMDINPARAMS; bIDCmd: Byte; bDfpDriveMap: Byte; bDriveNum: Byte); forward;

function OpenSMART(): Cardinal; forward;
procedure PrintIDERegs(pscip: PSENDCMDINPARAMS); forward;


procedure main();
var
	hSMARTIOCTL:	Cardinal;
    cbBytesReturned:DWORD;
    VersionParams:	GETVERSIONOUTPARAMS;
	scip:			SENDCMDINPARAMS;
	OutCmd:			SENDCMDOUTPARAMS;
    bDfpDriveMap:	Byte;
    i:				Byte;
    bSuccess:		Byte;
    bIDCmd:			Byte;
begin
	// Init
    hSMARTIOCTL := 0;
    bDfpDriveMap := 0;
	bSuccess := 1;

	//
	// Try to get a handle to SMART IOCTL, report failure and exit if
	// can't.
	//
	hSMARTIOCTL := OpenSMART();
    if (hSMARTIOCTL <> INVALID_HANDLE_VALUE) then
    begin
        //
        // Get the version, etc of SMART IOCTL
        //
        ZeroMemory(@VersionParams, sizeof(VersionParams));

        if ( DeviceIoControl(hSMARTIOCTL, DFP_GET_VERSION,
			nil,
			0,
			@VersionParams,
			sizeof(VersionParams),
			cbBytesReturned, nil) ) then
        begin

            print('DFP_GET_VERSION returned:');
            print(Format(' bVersion        = %d', [VersionParams.bVersion]));
            print(Format(' bRevision       = %d', [VersionParams.bRevision]));
            print(Format(' fCapabilities   = $%x', [VersionParams.fCapabilities]));
            print(Format(' bReserved       = $%x', [VersionParams.bReserved]));
            print(Format(' bIDEDeviceMap   = $%x', [VersionParams.bIDEDeviceMap]));
            print(Format(' cbBytesReturned = %d', [cbBytesReturned]));
            print('');
        end
        else
	        print('DFP_GET_VERSION failed.');

        for i := 1 to 1{MAX_IDE_DRIVES - 1} do
        begin
            //
            // If there is a IDE device at number "i" issue commands
            // to the device.
            //
            if ((VersionParams.bIDEDeviceMap shr i ) and 1) <> 0 then
            begin
                //
                // Try to enable SMART so we can tell if a drive supports it.
                // Ignore ATAPI devices.
                //

                if ((VersionParams.bIDEDeviceMap shr i) and $10) = 0 then
                begin
                    ZeroMemory(@scip, SizeOf(scip));
                    ZeroMemory(@OutCmd, SizeOf(OutCmd));

                    if (DoEnableSMART(hSMARTIOCTL,
                        @scip,
                        @OutCmd,
                        i,
                        cbBytesReturned)) then
                    begin
                        print(Format('SMART Enabled on Drive: %d', [i]));
                        //
                        // Mark the drive as SMART enabled
                        //
                        bDfpDriveMap := bDfpDriveMap or (1 shl i);
                    end
                    else
                    begin
    					print(Format('SMART Enable Command Failed, Drive: %d.', [i]));
    					print(Format(' DriverStatus: bDriverError=$%X, bIDEStatus=$%X',
    	    					[OutCmd.DriverStatus.bDriverError,
    	    					OutCmd.DriverStatus.bIDEStatus]));
                        print('');
                    end;

    				print(Format(' cbBytesReturned: %d', [cbBytesReturned]));
                    print('');
                end; // if

                //
                // Now, get the ID sector for all IDE devices in the system.
                // If the device is ATAPI use the IDE_ATAPI_ID command,
                // otherwise use the IDE_ID_FUNCTION command.
                //
                if ((VersionParams.bIDEDeviceMap shr i) and $10) <> 0 then
                    bIDCmd := IDE_ATAPI_ID
                else
                    bIDCmd := IDE_ID_FUNCTION;

                //#Todo verifer si traduction correct
                //bIDCmd = (VersionParams.bIDEDeviceMap >> i & 0x10) ? \
                //	IDE_ATAPI_ID : IDE_ID_FUNCTION;

                ZeroMemory(@scip, SizeOf(scip));
                ZeroMemory(@IdOutCmd, SizeOf(IdOutCmd));

                if (DoIDENTIFY(hSMARTIOCTL,
                        @scip,
                        PSENDCMDOUTPARAMS(@IdOutCmd),
                        bIDCmd,
                        i,
                        cbBytesReturned)) then
                begin
                    DisplayIdInfo(PIDSECTOR(@PSENDCMDOUTPARAMS(@IdOutCmd[0]).bBuffer[0]),
                            @scip,
                            bIDCmd,
                            bDfpDriveMap,
                            i);
                end
                else
                begin
                    print(Format('Identify Command Failed on Drive: %d', [i]));
                    print(Format(' DriverStatus: bDriverError=$%X, bIDEStatus=$%X',
                    [PSENDCMDOUTPARAMS(@IdOutCmd[0]).DriverStatus.bDriverError,
                    PSENDCMDOUTPARAMS(@IdOutCmd[0]).DriverStatus.bIDEStatus]));
                end; // if (DoIDENTIFY

                print(Format(' cbBytesReturned: %d', [cbBytesReturned]));
                print('');

			end; // if ((VersionParams
		end; // for

        CloseHandle(hSMARTIOCTL);
	end; // if (hSMARTIOCTL
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
* DoPrintData
*
* FUNCTION: Display the SMART Attributes and Thresholds
*
****************************************************************************}
procedure DoPrintData(pAttrBuffer, pThrsBuffer: PChar; bDriveNum: Byte);
var
    i: integer;
    pDA: PDRIVEATTRIBUTE;
    pAT: PATTRTHRESHOLD;
    Attr: BYTE;
    AttrRev, ThresRev: Word;
begin
	//
	// Print the revisions of the data structures
	//
    AttrRev := PWord(pAttrBuffer)^;
    ThresRev:= PWord(pThrsBuffer)^;
end;


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

{****************************************************************************
*
* DisplayIdInfo
*
* Display the contents of the ID buffer
*
****************************************************************************}
procedure DisplayIdInfo(pids: PIDSECTOR; pSCIP: PSENDCMDINPARAMS; bIDCmd: Byte; bDfpDriveMap: Byte; bDriveNum: Byte);
var
	szOutBuffer: array[0..40] of Byte;
    firmname, serialname, modelname: string;
begin

	if (bIDCmd = IDE_ID_FUNCTION) then
    begin

		print(Format('Drive %d is an IDE Hard drive', [bDriveNum]));
//		print(Format('%s', ( bDfpDriveMap >> bDriveNum & 1) ? \
//				" that supports SMART" : "");
		print(Format('#Cylinders: %d, #Heads: %d, #Sectors per Track: %d',
				[pids.wNumCyls,
				pids.wNumHeads,
				pids.wSectorsPerTrack]));

		print(Format('Cylinders: %d, Heads: %d, Sectors per Track: %d',
                [pids.wNumCurrentCyls,
                pids.wNumCurrentHeads,
                pids.wNumCurrentSectorsPerTrack]));

        print(Format('Total sectors: %d', [pids.ulTotalAddressableSectors]));
		PrintIDERegs(pSCIP);

	end
	else
		print(Format('Drive %d is an ATAPI device.', [bDriveNum]));

	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids.sModelNumber,
		sizeof(pids.sModelNumber));

    SetString(modelname, pids.sModelNumber, Sizeof(pids.sModelNumber));
	modelname := trim(modelname);
	print('\tModel number: ' + modelname);


	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids.sFirmwareRev,
		sizeof(pids.sFirmwareRev));

    SetString(firmname, pids.sFirmwareRev, Sizeof(pids.sFirmwareRev));
	firmname := trim(firmname);

	print('\tFirmware rev: ' + firmname);


	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids.sSerialNumber,
		sizeof(pids.sSerialNumber));

    SetString(serialname, pids.sSerialNumber, Sizeof(pids.sSerialNumber));
	serialname := trim(serialname);

	print('\tSerial number: ' + serialname);
end;


//---------------------------------------------------------------------
// Display contents of IDE hardware registers reported by SMART
//---------------------------------------------------------------------
procedure PrintIDERegs(pscip: PSENDCMDINPARAMS);
begin
    print(Format('bFeaturesReg: $%.2x',[pscip.irDriveRegs.bFeaturesReg]));
    print(Format('bSectorCountReg: $%.2x',[pscip.irDriveRegs.bSectorCountReg]));
    print(Format('bSectorNumberReg: $%.2x',[pscip.irDriveRegs.bSectorNumberReg]));
    print(Format('bCylLowReg: $%.2x',[pscip.irDriveRegs.bCylLowReg]));
    print(Format('bCylHighReg: $%.2x',[pscip.irDriveRegs.bCylHighReg]));
    print(Format('bDriveHeadReg: $%.2x',[pscip.irDriveRegs.bDriveHeadReg]));
    print(Format('Status: $%.2x',[pscip.irDriveRegs.bCommandReg]));
end;


//---------------------------------------------------------------------
// Open SMART to allow DeviceIoControl communications.
//---------------------------------------------------------------------
//#ToDo rajouter le  numéro de disque 
function OpenSMART(): Cardinal;
var
	hSMARTIOCTL: Cardinal;
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
        hSMARTIOCTL := CreateFile('\\.\PhysicalDrive1',GENERIC_READ or GENERIC_WRITE,
        						  FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                                  OPEN_EXISTING, 0, 0);
		if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        	// Unable to open physical drive, error code: 0x%lX\n", GetLastError())
        	;
    end;
	Result := hSMARTIOCTL;
end;


//-------------------------------
end.

// SmartApp
//http://support.microsoft.com/default.aspx?scid=kb;en-us;208048
//
// ================================= Smart.h ================================

/****************************************************************************
*                                                                           *
* THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY     *
* KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE       *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR     *
* PURPOSE.                                                                  *
*                                                                           *
* Copyright 1993-98  Microsoft Corporation.  All Rights Reserved.           *
*                                                                           *
****************************************************************************/

/****************************************************************************
*
* PROGRAM: SMART.H
*
* PURPOSE: Structure definitions for an application that calls SMART Ioctls
*
****************************************************************************/

#ifndef SMARTIOCTL_INCLUDED
#define SMARTIOCTL_INCLUDED

// Miscellaneous

#define	MAX_IDE_DRIVES	4	// Max number of drives assuming primary/secondary, master/slave topology
#define READ_ATTRIBUTE_BUFFER_SIZE	512
#define IDENTIFY_BUFFER_SIZE		512
#define READ_THRESHOLD_BUFFER_SIZE	512

//
// IOCTL commands
//
#define	DFP_GET_VERSION			0x00074080
#define	DFP_SEND_DRIVE_COMMAND	0x0007c084
#define DFP_RECEIVE_DRIVE_DATA	0x0007c088

//---------------------------------------------------------------------
// GETVERSIONOUTPARAMS contains the data returned from the 
// Get Driver Version function.
//---------------------------------------------------------------------
typedef struct _GETVERSIONOUTPARAMS {
	BYTE	bVersion;		// Binary driver version.
	BYTE	bRevision;		// Binary driver revision.
	BYTE	bReserved;		// Not used.
	BYTE	bIDEDeviceMap;	// Bit map of IDE devices.
	DWORD	fCapabilities;	// Bit mask of driver capabilities.
	DWORD	dwReserved[4];	// For future use.
} GETVERSIONOUTPARAMS, *PGETVERSIONOUTPARAMS, *LPGETVERSIONOUTPARAMS;

//
// Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
//
#define	CAP_IDE_ID_FUNCTION				1	// ATA ID command supported
#define	CAP_IDE_ATAPI_ID				2	// ATAPI ID command supported
#define	CAP_IDE_EXECUTE_SMART_FUNCTION	4	// SMART commannds supported

//---------------------------------------------------------------------
// IDE registers
//---------------------------------------------------------------------
typedef struct _IDEREGS {
	BYTE	bFeaturesReg;		// Used for specifying SMART "commands".
	BYTE	bSectorCountReg;	// IDE sector count register
	BYTE	bSectorNumberReg;	// IDE sector number register
	BYTE	bCylLowReg;			// IDE low order cylinder value
	BYTE	bCylHighReg;		// IDE high order cylinder value
	BYTE	bDriveHeadReg;		// IDE drive/head register
	BYTE	bCommandReg;		// Actual IDE command.
	BYTE	bReserved;			// reserved for future use.  Must be zero.
} IDEREGS, *PIDEREGS, *LPIDEREGS;

//---------------------------------------------------------------------
// SENDCMDINPARAMS contains the input parameters for the 
// Send Command to Drive function.
//---------------------------------------------------------------------
typedef struct _SENDCMDINPARAMS {
	DWORD	cBufferSize;		// Buffer size in bytes
	IDEREGS	irDriveRegs;		// Structure with drive register values.
	BYTE	bDriveNumber;		// Physical drive number to send 
								// command to (0,1,2,3).
	BYTE	bReserved[3];		// Reserved for future expansion.
	DWORD	dwReserved[4];		// For future use.
	BYTE 	bBuffer[1];			// Input buffer.
} SENDCMDINPARAMS, *PSENDCMDINPARAMS, *LPSENDCMDINPARAMS;

//
// Valid values for the bCommandReg member of IDEREGS.
//
#define	IDE_ATAPI_ID				0xA1	// Returns ID sector for ATAPI.
#define	IDE_ID_FUNCTION				0xEC	// Returns ID sector for ATA.
#define	IDE_EXECUTE_SMART_FUNCTION	0xB0	// Performs SMART cmd.
											// Requires valid bFeaturesReg,
											// bCylLowReg, and bCylHighReg
//
// Cylinder register values required when issuing SMART command
//
#define	SMART_CYL_LOW	0x4F
#define	SMART_CYL_HI	0xC2

//---------------------------------------------------------------------
// Status returned from driver
//---------------------------------------------------------------------
typedef struct _DRIVERSTATUS {
	BYTE	bDriverError;		// Error code from driver, 
								// or 0 if no error.
	BYTE	bIDEStatus;			// Contents of IDE Error register.
								// Only valid when bDriverError
								// is SMART_IDE_ERROR.
	BYTE	bReserved[2];		// Reserved for future expansion.
	DWORD	dwReserved[2];		// Reserved for future expansion.
} DRIVERSTATUS, *PDRIVERSTATUS, *LPDRIVERSTATUS;

//
// bDriverError values
//
#define	SMART_NO_ERROR			0	// No error
#define	SMART_IDE_ERROR			1	// Error from IDE controller
#define	SMART_INVALID_FLAG		2	// Invalid command flag
#define	SMART_INVALID_COMMAND	3	// Invalid command byte
#define	SMART_INVALID_BUFFER	4	// Bad buffer (null, invalid addr..)
#define	SMART_INVALID_DRIVE		5	// Drive number not valid
#define	SMART_INVALID_IOCTL		6	// Invalid IOCTL
#define	SMART_ERROR_NO_MEM		7	// Could not lock user's buffer
#define	SMART_INVALID_REGISTER	8	// Some IDE Register not valid
#define	SMART_NOT_SUPPORTED		9	// Invalid cmd flag set
#define	SMART_NO_IDE_DEVICE		10	// Cmd issued to device not present
									// although drive number is valid
// 11-255 reserved

//---------------------------------------------------------------------
// Structure returned by SMART IOCTL for several commands
//---------------------------------------------------------------------
typedef struct _SENDCMDOUTPARAMS {
	DWORD 			cBufferSize;		// Size of bBuffer in bytes
	DRIVERSTATUS	DriverStatus;		// Driver status structure.
	BYTE			bBuffer[1];			// Buffer of arbitrary length in which to store the data read from the 											// drive.
} SENDCMDOUTPARAMS, *PSENDCMDOUTPARAMS, *LPSENDCMDOUTPARAMS;


//---------------------------------------------------------------------
// Feature register defines for SMART "sub commands"
//---------------------------------------------------------------------
#define SMART_READ_ATTRIBUTE_VALUES				0xD0	// ATA4: Renamed 
														// SMART READ DATA
#define	SMART_READ_ATTRIBUTE_THRESHOLDS			0xD1	// Obsoleted in ATA4!
#define SMART_ENABLE_DISABLE_ATTRIBUTE_AUTOSAVE	0xD2
#define SMART_SAVE_ATTRIBUTE_VALUES				0xD3
#define	SMART_EXECUTE_OFFLINE_IMMEDIATE			0xD4	// ATA4
// Vendor specific commands:
#define	SMART_ENABLE_SMART_OPERATIONS			0xD8
#define	SMART_DISABLE_SMART_OPERATIONS			0xD9
#define	SMART_RETURN_SMART_STATUS				0xDA

#endif

//---------------------------------------------------------------------
// The following structure defines the structure of a Drive Attribute
//---------------------------------------------------------------------
typedef	struct	_DRIVEATTRIBUTE {
	BYTE	bAttrID;		// Identifies which attribute
	WORD	wStatusFlags;	// see bit definitions below
	BYTE	bAttrValue;		// Current normalized value
	BYTE	bWorstValue;	// How bad has it ever been?
	BYTE	bRawValue[6];	// Un-normalized value
	BYTE	bReserved;		// ...
} DRIVEATTRIBUTE, *PDRIVEATTRIBUTE, *LPDRIVEATTRIBUTE;

//---------------------------------------------------------------------
// The following structure defines the structure of a Warranty Threshold
// Obsoleted in ATA4!
//---------------------------------------------------------------------
typedef	struct	_ATTRTHRESHOLD {
	BYTE	bAttrID;			// Identifies which attribute
	BYTE	bWarrantyThreshold;	// Triggering value
	BYTE	bReserved[10];		// ...
} ATTRTHRESHOLD, *PATTRTHRESHOLD, *LPATTRTHRESHOLD;

//---------------------------------------------------------------------
// The following struct defines the interesting part of the IDENTIFY
// buffer:
//---------------------------------------------------------------------
typedef struct _IDSECTOR {
	USHORT	wGenConfig;
	USHORT	wNumCyls;
	USHORT	wReserved;
	USHORT	wNumHeads;
	USHORT	wBytesPerTrack;
	USHORT	wBytesPerSector;
	USHORT	wSectorsPerTrack;
	USHORT	wVendorUnique[3];
	CHAR	sSerialNumber[20];
	USHORT	wBufferType;
	USHORT	wBufferSize;
	USHORT	wECCSize;
	CHAR	sFirmwareRev[8];
	CHAR	sModelNumber[40];
	USHORT	wMoreVendorUnique;
	USHORT	wDoubleWordIO;
	USHORT	wCapabilities;
	USHORT	wReserved1;
	USHORT	wPIOTiming;
	USHORT	wDMATiming;
	USHORT	wBS;
	USHORT	wNumCurrentCyls;
	USHORT	wNumCurrentHeads;
	USHORT	wNumCurrentSectorsPerTrack;
	ULONG	ulCurrentSectorCapacity;
	USHORT	wMultSectorStuff;
	ULONG	ulTotalAddressableSectors;
	USHORT	wSingleWordDMA;
	USHORT	wMultiWordDMA;
	BYTE	bReserved[128];
} IDSECTOR, *PIDSECTOR;

//---------------------------------------------------------------------
// Valid Attribute IDs
//---------------------------------------------------------------------
#define	ATTR_INVALID				0
#define ATTR_READ_ERROR_RATE		1
#define ATTR_THROUGHPUT_PERF		2
#define ATTR_SPIN_UP_TIME			3
#define ATTR_START_STOP_COUNT		4
#define ATTR_REALLOC_SECTOR_COUNT	5
#define ATTR_READ_CHANNEL_MARGIN	6
#define ATTR_SEEK_ERROR_RATE		7
#define ATTR_SEEK_TIME_PERF			8
#define ATTR_POWER_ON_HRS_COUNT		9
#define ATTR_SPIN_RETRY_COUNT		10
#define ATTR_CALIBRATION_RETRY_COUNT 11
#define ATTR_POWER_CYCLE_COUNT		12

//---------------------------------------------------------------------
// Status Flags Values
//---------------------------------------------------------------------
#define	PRE_FAILURE_WARRANTY		0x1
#define	ON_LINE_COLLECTION			0x2
#define	PERFORMANCE_ATTRIBUTE		0x4
#define	ERROR_RATE_ATTRIBUTE		0x8
#define	EVENT_COUNT_ATTRIBUTE		0x10
#define	SELF_PRESERVING_ATTRIBUTE	0x20

#define	NUM_ATTRIBUTE_STRUCTS		 30

// ================================= Smart.h ================================

/****************************************************************************
*                                                                           *
* THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY     *
* KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE       *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR     *
* PURPOSE.                                                                  *
*                                                                           *
* Copyright 1993-98  Microsoft Corporation.  All Rights Reserved.           *
*                                                                           *
****************************************************************************/

/****************************************************************************
*
* PROGRAM: SMARTAPP.C
*
* PURPOSE: Simple console application that calls SMART IOCTL
*
* FUNCTIONS:
*  main() - Console application opens SMART IOCTL which supports DeviceIoControl.
*	    SMART IOCTL will return values to this application through this 
*	    same DeviceIoControl interface.
*
****************************************************************************/
#define WINDOWS9X	// Define this to compile for Windows 9x

#include <stdio.h>
#include <conio.h>
#include <windows.h>

#pragma pack(1)		// Required to ensure correct SMART IOCTL structure setup
#include "smart.h"
#include "smartapp.h"

//
// Define global buffers.
//
BYTE	AttrOutCmd[sizeof(SENDCMDOUTPARAMS) + READ_ATTRIBUTE_BUFFER_SIZE - 1];
BYTE	ThreshOutCmd[sizeof(SENDCMDOUTPARAMS) + READ_THRESHOLD_BUFFER_SIZE - 1];
BYTE	IdOutCmd[sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1];

int main()
{
	HANDLE      		hSMARTIOCTL = 0;
	DWORD       		cbBytesReturned;
	GETVERSIONOUTPARAMS VersionParams;
	SENDCMDINPARAMS 	scip;
	SENDCMDOUTPARAMS	OutCmd;
	BYTE				bDfpDriveMap = 0;
	BYTE				i;
	BYTE				bSuccess = 1;
	BYTE				bIDCmd;		// IDE or ATAPI IDENTIFY cmd

	//
	// Try to get a handle to SMART IOCTL, report failure and exit if
	// can't.
	//
    if ((hSMARTIOCTL = OpenSMART()) != INVALID_HANDLE_VALUE)
    {
	//
	// Get the version, etc of SMART IOCTL
	//
	memset((void*)&VersionParams, 0, sizeof(VersionParams));

	if ( DeviceIoControl(hSMARTIOCTL, DFP_GET_VERSION,
			NULL, 
			0,
			&VersionParams,
			sizeof(VersionParams),
			&cbBytesReturned, NULL) )
    {
		printf("DFP_GET_VERSION returned:\n");
		printf("\tbVersion        = %d\n", VersionParams.bVersion);
		printf("\tbRevision       = %d\n", VersionParams.bRevision);
		printf("\tfCapabilities   = 0x%lx\n", VersionParams.fCapabilities);
		printf("\tbReserved       = 0x%x\n", VersionParams.bReserved);
		printf("\tbIDEDeviceMap   = 0x%x\n", VersionParams.bIDEDeviceMap);
		printf("\tcbBytesReturned = %d\n\n", cbBytesReturned);
    }
    else
    {
        printf("DFP_GET_VERSION failed.\n");
    }

	for (i = 0; i < MAX_IDE_DRIVES; i++)
	{
		//
		// If there is a IDE device at number "i" issue commands
		// to the device.
		//
		if (VersionParams.bIDEDeviceMap >> i & 1)
		{

			//
			// Try to enable SMART so we can tell if a drive supports it.
			// Ignore ATAPI devices.
			//

			if (!(VersionParams.bIDEDeviceMap >> i & 0x10))
			{

				memset(&scip, 0, sizeof(scip));
				memset(&OutCmd, 0, sizeof(OutCmd));

	    		if (DoEnableSMART(hSMARTIOCTL, 
					&scip, 
					&OutCmd, 
					i,
					&cbBytesReturned))
				{
					printf("SMART Enabled on Drive: %d\n", i);
					//
					// Mark the drive as SMART enabled
					//
					bDfpDriveMap |= (1 << i);
				}
				else
	    		{
					printf("SMART Enable Command Failed, Drive: %d.\n",i);
					printf(" DriverStatus: bDriverError=0x%X, bIDEStatus=0x%X\n\n", 
	    					OutCmd.DriverStatus.bDriverError, 
	    					OutCmd.DriverStatus.bIDEStatus);
	    		}
				printf("\tcbBytesReturned: %d\n\n", cbBytesReturned);
			}


			//
			// Now, get the ID sector for all IDE devices in the system.
			// If the device is ATAPI use the IDE_ATAPI_ID command,
			// otherwise use the IDE_ID_FUNCTION command.
			//
			bIDCmd = (VersionParams.bIDEDeviceMap >> i & 0x10) ? \
				IDE_ATAPI_ID : IDE_ID_FUNCTION;

			memset(&scip, 0, sizeof(scip));
			memset(IdOutCmd, 0, sizeof(IdOutCmd));

			if ( DoIDENTIFY(hSMARTIOCTL, 
					&scip, 
					(PSENDCMDOUTPARAMS)&IdOutCmd, 
					bIDCmd,
					i,
					&cbBytesReturned))
	        	{
				DisplayIdInfo((PIDSECTOR) ((PSENDCMDOUTPARAMS)IdOutCmd)->bBuffer,
						&scip,
						bIDCmd,
						bDfpDriveMap,
						i);
				}

	       	else
	       		{
	       			printf("Identify Command Failed on Drive: %d\n", i);
					printf(" DriverStatus: bDriverError=0x%X, bIDEStatus=0x%X\n\n", 
					((PSENDCMDOUTPARAMS)IdOutCmd)->DriverStatus.bDriverError, 
					((PSENDCMDOUTPARAMS)IdOutCmd)->DriverStatus.bIDEStatus);
	        	}
	    		printf("\tcbBytesReturned: %d\n\n", cbBytesReturned);
	    }

	}

	//
	// Loop through all possible IDE drives and send commands to the ones that support SMART.
	//
	for (i = 0; i < MAX_IDE_DRIVES; i++)
	{
		if (bDfpDriveMap >> i & 1)
		{

			memset(AttrOutCmd, 0, sizeof(AttrOutCmd));
			memset(ThreshOutCmd, 0, sizeof(ThreshOutCmd));

			if ( !(bSuccess = DoReadAttributesCmd(hSMARTIOCTL, 
					&scip, 
					(PSENDCMDOUTPARAMS)&AttrOutCmd, 
					i)))
			{
				printf("\nSMART Read Attr Command Failed on Drive: %d.\n", i);
				printf(" DriverStatus: bDriverError=0x%X, bIDEStatus=0x%X\n\n", 
					((PSENDCMDOUTPARAMS)AttrOutCmd)->DriverStatus.bDriverError, 
					((PSENDCMDOUTPARAMS)AttrOutCmd)->DriverStatus.bIDEStatus);
			}
	
			// ReadAttributes worked. Try ReadThresholds.
			else if ( !(DoReadThresholdsCmd(hSMARTIOCTL, 
					&scip, 
					(PSENDCMDOUTPARAMS)&ThreshOutCmd,
					i)))
			{
				printf("\nSMART Read Thrsh Command Failed on Drive: %d.\n", i);
				printf(" DriverStatus: bDriverError=0x%X, bIDEStatus=0x%X\n\n", 
					((PSENDCMDOUTPARAMS)ThreshOutCmd)->DriverStatus.bDriverError, 
					((PSENDCMDOUTPARAMS)ThreshOutCmd)->DriverStatus.bIDEStatus);
			}

			//
			// The following report will print if ReadAttributes works.
			// If ReadThresholds works, the report will also show values for
			// Threshold values.
			//
			if (bSuccess)
			{
					DoPrintData(((PSENDCMDOUTPARAMS)AttrOutCmd)->bBuffer,
							((PSENDCMDOUTPARAMS)ThreshOutCmd)->bBuffer, 
							i);
			}
	
		}
	}

	//
    // Close SMART.
	//
        CloseHandle(hSMARTIOCTL);
    }

    return(0);
}


/****************************************************************************
*
* DoIDENTIFY
*
* FUNCTION: Send an IDENTIFY command to the drive
* bDriveNum = 0-3
* bIDCmd = IDE_ID_FUNCTION or IDE_ATAPI_ID
*
****************************************************************************/
BOOL DoIDENTIFY(HANDLE hSMARTIOCTL, PSENDCMDINPARAMS pSCIP,
	PSENDCMDOUTPARAMS pSCOP, BYTE bIDCmd, BYTE bDriveNum, PDWORD lpcbBytesReturned)
{
	//
	// Set up data structures for IDENTIFY command.
	//

	pSCIP->cBufferSize = IDENTIFY_BUFFER_SIZE;

	pSCIP->irDriveRegs.bFeaturesReg = 0;
	pSCIP->irDriveRegs.bSectorCountReg = 1;
	pSCIP->irDriveRegs.bSectorNumberReg = 1;
	pSCIP->irDriveRegs.bCylLowReg = 0;
	pSCIP->irDriveRegs.bCylHighReg = 0;

	//
	// Compute the drive number.
	//
	pSCIP->irDriveRegs.bDriveHeadReg = 0xA0 | ((bDriveNum & 1) << 4); 

	//
	// The command can either be IDE identify or ATAPI identify.
	//
	pSCIP->irDriveRegs.bCommandReg = bIDCmd;
	pSCIP->bDriveNumber = bDriveNum;
	pSCIP->cBufferSize = IDENTIFY_BUFFER_SIZE;

    return ( DeviceIoControl(hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
             	(LPVOID)pSCIP, sizeof(SENDCMDINPARAMS) - 1,
               	(LPVOID)pSCOP, sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1,
               	lpcbBytesReturned, NULL) );
}

/****************************************************************************
*
* DisplayIdInfo
*
* Display the contents of the ID buffer
*
****************************************************************************/
VOID DisplayIdInfo(PIDSECTOR pids, PSENDCMDINPARAMS pSCIP, BYTE bIDCmd, BYTE bDfpDriveMap, BYTE bDriveNum)
{
	BYTE	szOutBuffer[41];

	if (bIDCmd == IDE_ID_FUNCTION)
	{
		printf("Drive %d is an IDE Hard drive", bDriveNum); 
		printf("%s\n", ( bDfpDriveMap >> bDriveNum & 1) ? \
				" that supports SMART" : "");
		printf("\t#Cylinders: %d, #Heads: %d, #Sectors per Track: %d\n",
				pids->wNumCyls, 
				pids->wNumHeads, 
				pids->wSectorsPerTrack);

		PrintIDERegs(pSCIP);

	}
	else
	{
		printf("Drive %d is an ATAPI device.\n", bDriveNum);
	}

	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids->sModelNumber, 
		sizeof pids->sModelNumber);

	memset(szOutBuffer,0, sizeof(szOutBuffer));
	strncpy(szOutBuffer, pids->sModelNumber, sizeof(pids->sModelNumber));

	printf("\tModel number: %s\n", szOutBuffer);


	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids->sFirmwareRev, 
		sizeof pids->sFirmwareRev);

	memset(szOutBuffer,0, sizeof(szOutBuffer));
	strncpy(szOutBuffer, pids->sFirmwareRev, sizeof(pids->sFirmwareRev));

	printf("\tFirmware rev: %s\n", szOutBuffer);


	//
	// Change the WORD array to a BYTE
	// array
	//
	ChangeByteOrder(pids->sSerialNumber, 
		sizeof pids->sSerialNumber);

	memset(szOutBuffer,0, sizeof(szOutBuffer));
	strncpy(szOutBuffer, pids->sSerialNumber, sizeof(pids->sSerialNumber));

	printf("\tSerial number: %s\n",szOutBuffer );

}

/****************************************************************************
*
* DoEnableSMART
*
* FUNCTION: Send a SMART_ENABLE_SMART_OPERATIONS command to the drive
* bDriveNum = 0-3
*
****************************************************************************/
BOOL DoEnableSMART(HANDLE hSMARTIOCTL, PSENDCMDINPARAMS pSCIP, PSENDCMDOUTPARAMS pSCOP, BYTE bDriveNum, PDWORD lpcbBytesReturned)
{
	//
	// Set up data structures for Enable SMART Command.
	//
	pSCIP->cBufferSize = 0;

	pSCIP->irDriveRegs.bFeaturesReg = SMART_ENABLE_SMART_OPERATIONS;
	pSCIP->irDriveRegs.bSectorCountReg = 1;
	pSCIP->irDriveRegs.bSectorNumberReg = 1;
	pSCIP->irDriveRegs.bCylLowReg = SMART_CYL_LOW;
	pSCIP->irDriveRegs.bCylHighReg = SMART_CYL_HI;

	//
	// Compute the drive number.
	//
	pSCIP->irDriveRegs.bDriveHeadReg = 0xA0 | ((bDriveNum & 1) << 4); 
	pSCIP->irDriveRegs.bCommandReg = IDE_EXECUTE_SMART_FUNCTION;
	pSCIP->bDriveNumber = bDriveNum;

        return ( DeviceIoControl(hSMARTIOCTL, DFP_SEND_DRIVE_COMMAND,
                (LPVOID)pSCIP, sizeof(SENDCMDINPARAMS) - 1,
                (LPVOID)pSCOP, sizeof(SENDCMDOUTPARAMS) - 1,
                lpcbBytesReturned, NULL) );
}

/****************************************************************************
*
* DoReadAttributesCmd
*
* FUNCTION: Send a SMART_READ_ATTRIBUTE_VALUES command to the drive
* bDriveNum = 0-3
*
****************************************************************************/
BOOL DoReadAttributesCmd(HANDLE hSMARTIOCTL, PSENDCMDINPARAMS pSCIP, PSENDCMDOUTPARAMS pSCOP, BYTE bDriveNum)
{
DWORD	cbBytesReturned;

	//
	// Set up data structures for Read Attributes SMART Command.
	//

	pSCIP->cBufferSize = READ_ATTRIBUTE_BUFFER_SIZE;

	pSCIP->irDriveRegs.bFeaturesReg = SMART_READ_ATTRIBUTE_VALUES;
	pSCIP->irDriveRegs.bSectorCountReg = 1;
	pSCIP->irDriveRegs.bSectorNumberReg = 1;
	pSCIP->irDriveRegs.bCylLowReg = SMART_CYL_LOW;
	pSCIP->irDriveRegs.bCylHighReg = SMART_CYL_HI;

	//
	// Compute the drive number.
	//
	pSCIP->irDriveRegs.bDriveHeadReg = 0xA0 | ((bDriveNum & 1) << 4); 
	pSCIP->irDriveRegs.bCommandReg = IDE_EXECUTE_SMART_FUNCTION;
	pSCIP->bDriveNumber = bDriveNum;
        return ( DeviceIoControl(hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
                (LPVOID)pSCIP, sizeof(SENDCMDINPARAMS) - 1,
                (LPVOID)pSCOP, sizeof(SENDCMDOUTPARAMS) + READ_ATTRIBUTE_BUFFER_SIZE - 1,
                &cbBytesReturned, NULL) );
}

/****************************************************************************
*
* DoReadThresholdsCmd
*
* FUNCTION: Send a SMART_READ_ATTRIBUTE_THRESHOLDS command to the drive
* bDriveNum = 0-3
*
****************************************************************************/
BOOL DoReadThresholdsCmd(HANDLE hSMARTIOCTL, PSENDCMDINPARAMS pSCIP, PSENDCMDOUTPARAMS pSCOP, BYTE bDriveNum)
{
DWORD	cbBytesReturned;

	//
	// Set up data structures for Read Thresholds SMART Command.
	//

	pSCIP->cBufferSize = READ_THRESHOLD_BUFFER_SIZE;

	pSCIP->irDriveRegs.bFeaturesReg = SMART_READ_ATTRIBUTE_THRESHOLDS;
	pSCIP->irDriveRegs.bSectorCountReg = 1;
	pSCIP->irDriveRegs.bSectorNumberReg = 1;
	pSCIP->irDriveRegs.bCylLowReg = SMART_CYL_LOW;
	pSCIP->irDriveRegs.bCylHighReg = SMART_CYL_HI;

	//
	// Compute the drive number.
	//
	pSCIP->irDriveRegs.bDriveHeadReg = 0xA0 | ((bDriveNum & 1) << 4); 
	pSCIP->irDriveRegs.bCommandReg = IDE_EXECUTE_SMART_FUNCTION;
	pSCIP->bDriveNumber = bDriveNum;


        return ( DeviceIoControl(hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
                (LPVOID)pSCIP, sizeof(SENDCMDINPARAMS) - 1,
                (LPVOID)pSCOP, sizeof(SENDCMDOUTPARAMS) + READ_THRESHOLD_BUFFER_SIZE - 1,
                &cbBytesReturned, NULL) );
}
//
// Declare a global structure to help print the data.
// NOTE: Per ATA3 and ATA4 specs, these attribute definitions are defined by the drive vendor
// and hence their attributes may vary between vendors.
//
PCHAR	pAttrNames[] = {
	"No Attribute Here       ", //0
	"Raw Read Error Rate     ", //1
	"Throughput Performance  ", //2
	"Spin Up Time            ", //3
	"Start/Stop Count        ", //4
	"Reallocated Sector Count", //5
	"Read Channel Margin     ", //6
	"Seek Error Rate         ", //7
	"Seek Time Performance   ", //8
	"Power On Hours Count    ", //9
	"Spin Retry Count        ", //10
	"Calibration Retry Count ", //11
	"Power Cycle Count       ", //12
	"(Unknown attribute)     "
};
#define MAX_KNOWN_ATTRIBUTES	12

/****************************************************************************
*
* DoPrintData
*
* FUNCTION: Display the SMART Attributes and Thresholds
*
****************************************************************************/
VOID DoPrintData(PCHAR pAttrBuffer, PCHAR pThrsBuffer, BYTE bDriveNum)
{
int	i;
PDRIVEATTRIBUTE	pDA;
PATTRTHRESHOLD	pAT;
BYTE Attr;

	//
	// Print the drive number
	//
	printf("\nData for Drive Number %d\n", bDriveNum);
	//
	// Print the revisions of the data structures
	//
	printf("Attribute Structure Revision          Threshold Structure Revision\n");
	printf("             %d                                      %d\n\n",
				(WORD)pAttrBuffer[0],
				(WORD)pThrsBuffer[0]);

	//
	// Print the header and loop through the structures, printing
	// the structures when the attribute ID is known.
	//
	printf("   -Attribute Name-      -Attribute Value-     -Threshold Value-\n");

	pDA = (PDRIVEATTRIBUTE)&pAttrBuffer[2];
	pAT = (PATTRTHRESHOLD)&pThrsBuffer[2];

	for (i = 0; i < NUM_ATTRIBUTE_STRUCTS; i++)
	{
		Attr = pDA->bAttrID;
		if (Attr)
		{
			if (Attr > MAX_KNOWN_ATTRIBUTES)
				Attr = MAX_KNOWN_ATTRIBUTES+1;
			printf("%2X %-29s%d%20c%d\n",
					pDA->bAttrID,
					pAttrNames[Attr],
					pDA->bAttrValue,
					' ',
					pAT->bWarrantyThreshold );
		}
		pDA++;
		pAT++;
	}
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------
VOID ChangeByteOrder(PCHAR szString, USHORT uscStrSize)
{

USHORT	i;
CHAR	temp;

	for (i = 0; i < uscStrSize; i+=2)
	{
		temp = szString[i];
		szString[i] = szString[i+1];
		szString[i+1] = temp;
	}
}

//---------------------------------------------------------------------
// Display contents of IDE hardware registers reported by SMART
//---------------------------------------------------------------------
VOID PrintIDERegs(PSENDCMDINPARAMS pscip)
{
	printf("\tIDE TASK FILE REGISTERS:\n");

	printf("\t\tbFeaturesReg     = 0x%X\n", pscip->irDriveRegs.bFeaturesReg);
	printf("\t\tbSectorCountReg  = 0x%X\n", pscip->irDriveRegs.bSectorCountReg);
	printf("\t\tbSectorNumberReg = 0x%X\n", pscip->irDriveRegs.bSectorNumberReg);
	printf("\t\tbCylLowReg       = 0x%X\n", pscip->irDriveRegs.bCylLowReg);
	printf("\t\tbCylHighReg      = 0x%X\n", pscip->irDriveRegs.bCylHighReg);
	printf("\t\tbDriveHeadReg    = 0x%X\n", pscip->irDriveRegs.bDriveHeadReg);
	printf("\t\tStatus           = 0x%X\n", pscip->irDriveRegs.bCommandReg);
}

//---------------------------------------------------------------------
// Open SMART to allow DeviceIoControl communications.
//---------------------------------------------------------------------
HANDLE OpenSMART(VOID)
{
HANDLE	hSMARTIOCTL = 0;

#ifdef WINDOWS9X
	// Version Windows 95 OSR2, Windows 98
	if ((hSMARTIOCTL = CreateFile("\\\\.\\SMARTVSD", 0,0,0,
                        CREATE_NEW, 0, 0)) == INVALID_HANDLE_VALUE)
	{
		printf("Unable to open SMARTVSD, error code: 0x%lX\n", GetLastError());
    }
   	else
	{
		printf("SMARTVSD opened successfully\n");
	}
#else
	// Windows NT, Windows 2000
	if ((hSMARTIOCTL = CreateFile("\\\\.\\PhysicalDrive0",GENERIC_READ | GENERIC_WRITE,FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,
							OPEN_EXISTING,0,NULL)) == INVALID_HANDLE_VALUE)
	{
		printf("Unable to open physical drive, error code: 0x%lX\n", GetLastError());
    }
   	else
	{
		printf("Physical drive opened successfully\n");
	}
#endif

	return hSMARTIOCTL;

}
