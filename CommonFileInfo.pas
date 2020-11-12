{-----------------------------------------------------------------------------
 Unit Name: CommonFileInfo
 Author:    Chris
 Purpose:   Generic class to manage files and directories
 History:
-----------------------------------------------------------------------------}

//#Todo Descendre DOS Attributes dans une sous classe

unit CommonFileInfo;

interface


uses
	Windows, Classes, SysUtils;


Const

// DOS Attribut des fichiers/rep ADVSHR
ATTR_READ_ONLY		= $01;
ATTR_HIDDEN			= $02;
ATTR_SYSTEM			= $04;
ATTR_VOLUME_ID		= $08;	// Entrée contenant le nom du volume (RootDir)
ATTR_DIRECTORY		= $10;
ATTR_ARCHIVE		= $20;


Type


//
// Classe abstraite pour gérer le fichiers/ répertoires
//
TCommonFileInfo = class
    private
    protected
        FFileSize:		int64;		// Taille du fichier, 0 pour les rep
        FFileID:		int64;		// ID unique pour le fichier/directory
        FParentDirID:	int64;		// ID du répertoire contenant le fichier/directory
        FWriteDate:		TFileTime;	// Date/heure dernière ecriture (Date mot poid fort, heure mot poids faible)
        FCreateDate:	TFileTime;	// Date/heure de création ou 0 si pas utilisé
        FAccessDate:	TFileTime;	// Date dernier accès (l'heure n'est pas utilisée = 0)
        //
        // ============================ Methodes ============================
        function GetFileName(): string; virtual;
        function GetShortName(): string; virtual; abstract;
        function GetDosAttributes(): Byte; virtual; abstract;
    public
        // ============================ Methodes ============================
        //constructor Create(const fileseq, parentid: int64); virtual;
        constructor Create(); virtual;
        destructor Destroy; virtual;
        //
        function GetDosAttrString(): string; virtual;
        procedure GetFileProperties(info: TStrings); virtual;
        function isDirectory(): boolean; virtual; abstract;
        function isDeleted():boolean; virtual; abstract;
        //function isVolume: boolean; virtual; abstract;
        // =========================== Propriétés ===========================
        property FileName: string read GetFileName;
        property ShortName: string read GetShortName;
        property Size: int64 read FFileSize;
        property FileID: int64 read FFileID;
        property ParentDirID: int64 read FParentDirID;
        property DOSAttr: Byte read GetDosAttributes; //Flags
        property WriteDate: TFileTime read FWriteDate;
        property AccessDate: TFileTime read FAccessDate;
        property CreationDate: TFileTime read FCreateDate;
end;


// Fonction utilitaires
function RestoreFileTimeStamp(const filename: string; fileinfo: TCommonFileInfo): boolean;
function RestoreDirTimestamp(const dirname: string; dirinfo: TCommonFileInfo): boolean;


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation


uses
	ConversionUnit;


// --------------------------------------------------------------------------
//							Constructeurs et destructeurs
// --------------------------------------------------------------------------


constructor TCommonFileInfo.Create();
begin
	// 
end;


destructor TCommonFileInfo.Destroy;
begin
end;


// --------------------------------------------------------------------------
//							Gestions des propriétés
// --------------------------------------------------------------------------


function TCommonFileInfo.GetFileName(): string;
begin
	Result := '';
end;


//
// Convertit le DOS Attrib en chaine
//
function TCommonFileInfo.GetDosAttrString(): string;

    //
    //
    //
    function GetAttrString(const attr: byte): string;
    begin

        Result := '-----';	// ADSHR

        if (attr and ATTR_ARCHIVE) <> 0 then
            Result[1] := 'A';

        if (attr and ATTR_DIRECTORY) <> 0 then
            Result[2] := 'D';

        if (attr and ATTR_SYSTEM) <> 0 then
            Result[3] := 'S';

        if (attr and ATTR_HIDDEN) <> 0 then
            Result[4] := 'H';

        if (attr and ATTR_READ_ONLY) <> 0 then
            Result[5] := 'R';
    end;

begin
	Result := GetAttrString(Self.DOSAttr); 
end;


//
//
//
procedure TCommonFileInfo.GetFileProperties(info: TStrings);
begin
	info.Add(NAME_STR);
    info.Add(self.FileName);
	info.Add(TYPE_STR);
	if not Self.isDirectory then
    begin
		info.Add(FILE_STR);

		info.Add(SIZE_STR);
		info.Add(Format('%s (%s)', [SizeTo2DigitByteString(Self.Size),
        						    Int64ToThousandSepString(Self.Size)]));
    end
    else
  		info.Add(DIRECTORY_STR);

    if (Self.FileName <> Self.ShortName) and (Self.ShortName <> '')then
    begin
        info.Add(SHORTNAME_STR);
        info.Add(self.ShortName);
    end;

    info.Add(FILEID_STR);
    info.Add(IntToStr(self.FileID));

    info.Add(PARENTID_DIR_STR);
    info.Add(IntToStr(self.ParentDirID));

	info.Add(DOSATTRIBUTES_STR);
	info.Add(Self.GetDosAttrString);
end;


// ----------------------------------------------------------------------
//					Routine de gestion des TimeStamp
// ----------------------------------------------------------------------


//
// Restore le TimeStamp du fichier avec les infos de l'entrée FAT.
//
function RestoreFileTimeStamp(const filename: string; fileinfo: TCommonFileInfo): boolean;
var
	hFile : Cardinal;
    wriTime, AccTime, CreTime : TFileTime;
begin
    Result := false;
	hFile := CreateFile(Pchar(filename),
    				   GENERIC_READ or GENERIC_WRITE,
                       FILE_SHARE_READ,
                       nil,
                       OPEN_EXISTING,
                       0,
                       0);
    if hFile <> INVALID_HANDLE_VALUE then
    begin
    	// Obligé de recopier dans des variables temp pour avoir le pointeur
        WriTime := fileinfo.WriteDate;
        AccTime := fileinfo.AccessDate;
        CreTime := fileinfo.CreationDate;

        // Mettre à jour la date et l'heure
        if SetFileTime(hFile, @CreTime, @AccTime, @WriTime) then
        	Result := true;
        CloseHandle(hFile);
    end;
end;


//
// Restaure le timestamp du répertoire à partir de l'entrée FAT
// (Marche seulement sous NT/XP/2K)
//
function RestoreDirTimestamp(const dirname: string; dirinfo: TCommonFileInfo): boolean;
var
	hDir : Cardinal;
    CreTime, AccTime, WriTime: TFileTime;
begin
    Result := false;

	// Marche seulement sous NT/XP/2K
	if Win32Platform <> VER_PLATFORM_WIN32_NT then
    	exit;

	//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/base/createfile.asp
    hDir := CreateFile(Pchar(dirname),
    				   GENERIC_READ or GENERIC_WRITE,
                       FILE_SHARE_READ or FILE_SHARE_DELETE,
                       nil,
                       OPEN_EXISTING,
                       FILE_FLAG_BACKUP_SEMANTICS,
                       0);
    if hDir <> INVALID_HANDLE_VALUE then
    begin
    	// Obligé de recopier dans des variables temp pour avoir le pointeur
        WriTime := dirinfo.WriteDate;
        AccTime := dirinfo.AccessDate;
        CreTime := dirinfo.CreationDate;

        // Mettre à jour la date et l'heure
        // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/setfiletime.asp
        if SetFileTime(hDir, @CreTime, @AccTime, @WriTime) then
        	Result := true;
        CloseHandle(hDir);
    end;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
