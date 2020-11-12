{-----------------------------------------------------------------------------
 Unit Name: FATFileInfo
 Author:    Chris
 Purpose:   Routines et structures pour le parcours des répertoires FAT
 History:   06/05/2005
-----------------------------------------------------------------------------}


unit FATFileInfo;

interface

uses
	Classes, SysUtils, Windows,
    CommonFileInfo;


type


//
// Types correspondant aux entrées dans les répertoires
//
// FAT directory entry - all of this is in 32 bytes
TFATDirEntry = packed record
    FileName: array [0..7] of Char;
    FileExt: array [0..2] of Char;
    Attributes: Byte;       // Attribut de l'entrée (Dir/Volume/R/H/S
    NT: Byte;
    TenthOfCrtSec: Byte;	// Nombre de centième de sec à ajouté CreateTime, valid value range is 0-199.
    TimeCreated: Word;		// Heure de création (granularite 2sec cf. ci dessus)
    DateCreated: Word;		// Date de création
    DateAccessed: Word;     // Date de dernier acces
    StartClustHigh: Word;	// 16 bits de poids fort du num de cluster
    Time: Word;				// heure de la dernière écriture
    Date: Word;             // date de la dernière écriture
    StartClust: Word;		// 16 bits de poids faible du num de cluster
    Size: Cardinal;			// Taille du fichier en octet
end;
PFATDirEntry = ^TFATDirEntry;


// FAT long filename entry - another 32-byte entry
TFATDirEntryLFN = packed record
    Part: 			Byte;			//
    Name1: 			array [0..4] of WideChar;
    Attributes: 	Byte;
    Reserved:		Byte;
    Checksum:		Byte;
    Name2:			array [0..5] of WideChar;
    LFN_FstClusLO:	Word;			// Must be 0
    Name3:			array [0..1] of WideChar;
end;
PFATDirEntryLFN = ^TFATDirEntryLFN;


// Mettre dans GenericPArt ? et sous classe pour fichier FAT ?
TFATFileInfo = class(TCommonFileInfo)
    private
        FFileName:		string;
        FShortName:		string;
        FAttributs:		Byte;
        FFirstCluster:	Cardinal;	// 1er cluster du fichier ou du rep
        //
    protected
        // ============================= Methodes =============================
        function GetFileName(): string; override;
        function GetShortName(): string; override;
        function GetDosAttributes(): Byte; override;
    public
        // ============================= Methodes =============================
        constructor Create(const dirid: int64); overload;
        destructor Destroy; override;
        //
        function LoadFromDirEntry(const longname: string; direntry: PFATDirEntry): boolean;
    	procedure GetFileProperties(info: TStrings); override;
        function isDirectory: boolean; override;
        // ============================= Propriétés =============================
        property FirstCluster: Cardinal read FFirstCluster;
end;




// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation


uses
	ConversionUnit;

    
// --------------------------------------------------------------------------
//							Constructeurs et destructeurs
// --------------------------------------------------------------------------


constructor TFATFileInfo.Create(const dirid: int64);
begin
	inherited Create;

    Self.FParentDirID := dirid;
end;


destructor TFATFileInfo.Destroy;
begin
	inherited;
end;


// --------------------------------------------------------------------------
//							Gestions des propriétés
// --------------------------------------------------------------------------


function TFATFileInfo.GetDosAttributes(): Byte;
begin
	Result := Self.FAttributs;
end;


function TFATFileInfo.GetFileName(): string;
begin
	Result := Self.FFileName;
end;


function TFATFileInfo.GetShortName(): string;
begin
	Result := Self.FShortName;
end;


function TFATFileInfo.isDirectory(): boolean;
begin
	Result := (Self.FAttributs and ATTR_DIRECTORY) <> 0;
end;


//
//
//
procedure TFATFileInfo.GetFileProperties(info: TStrings);
var
	datestr, timestr: string;
begin
	inherited;

    // Afficher les date
    info.Add(LASTWRITE_STR);
    info.Add(FileTimeToString(Self.WriteDate));
	info.Add(CREATEDATE_STR);
    if (Self.CreationDate.dwLowDateTime = 0) and (Self.CreationDate.dwHighDateTime = 0) then
	    info.Add(UNDEFINED_STR)
	else
	    info.Add(FileTimeToString(Self.CreationDate));

   	info.Add(LASTREAD_STR);
    FileTimeToDateAndTimeString(Self.AccessDate, datestr, timestr);
    info.Add(datestr);
end;


// --------------------------------------------------------------------------
//								Init
// --------------------------------------------------------------------------


//
//
//
function TFATFileInfo.LoadFromDirEntry(const longname: string; direntry: PFATDirEntry): boolean;
begin
    Self.FShortName := Trim(direntry.FileName);
    if (Trim(direntry.FileExt) <> '') and (direntry.FileName[0] <> '.') then
	    Self.FShortName := Self.FShortName + '.' + Trim(direntry.FileExt);

    if Self.FShortName[1] = #05 then
    	Self.FShortName[1] := Char($E5);

	if longname = '' then
    	Self.FFileName := Self.FShortName
    else
    	Self.FFileName := longname;

	Self.FFileSize := int64(direntry.Size);
    Self.FFirstCluster := (direntry.StartClustHigh shl 16) + direntry.StartClust;
    Self.FAttributs := direntry.Attributes;

    Self.FFileID := int64(Self.FFirstCluster);

	// Converti les date/times
    DosDateTimeToFileTime(direntry.Date, direntry.Time, Self.FWriteDate);
    DosDateTimeToFileTime(direntry.DateCreated, direntry.TimeCreated, Self.FCreateDate);
    // #TODO3 Ajouter le TenthOfCrtSec
    DosDateTimeToFileTime(direntry.DateAccessed, 0, Self.FAccessDate);
end;


// --------------------------- Fin de l'unité -----------------------------
end.
