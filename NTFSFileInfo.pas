{-----------------------------------------------------------------------------
 Unit Name: NTFSFileInfo
 Author:    Chris
 Purpose:   Gestion des info sur les fichiers pour parcours répertoire
 History:   06/05/2005
-----------------------------------------------------------------------------}
unit NTFSFileInfo;

interface

uses
	Classes, Windows, SysUtils,
    NTFSAttributes, CommonFileInfo;

    
type


//
// Classe pour gérer le fichiers/ répertoires
//
TNTFSFileInfo = class(TCommonFileInfo)
    private
    	FFileName:		WideString;
        FShortName:		WideString;
        FPermissions:	TNTFSPermissions;  	// Attr de FileName Attr
        FMFTEntryFlags:	TNTFSMFTRecFlags;	// Deleted/In Use, Dir Flags (from FileRecord)
        FMFTDate:		TFileTime;			// ??
    protected
        // ============================= Methodes =============================
        function GetFileName(): string; override;
        function GetShortName(): string; override;
        function GetDosAttributes(): Byte; override;
    public
        // ============================= Methodes =============================
        constructor Create(const fileseq: int64); overload;
        destructor Destroy; override;
        // -----------------
        procedure GetFileProperties(info: TStrings); override;
        function isDirectory: boolean; override;
        function isDeleted(): boolean; override;
        //function isVolume: boolean; virtual; abstract;
        procedure LoadFileAttribute(attrstream: TMemoryStream);
        // ============================= Propriétés =============================
        property MFTDate: TFileTime read FMFTDate write FMFTDate;
end;




// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


uses
	ConversionUnit;

    
// --------------------------------------------------------------------------
//							Constructeurs et destructeurs
// --------------------------------------------------------------------------


constructor TNTFSFileInfo.Create(const fileseq: int64);
begin
	Self.FFileID := fileseq and MASK_48BITS;
end;


destructor TNTFSFileInfo.Destroy;
begin
end;


// --------------------------------------------------------------------------
//							Init
// --------------------------------------------------------------------------


//
// Mets à jour le FileInfo avec l'attribut FileName dans le stream
//
procedure TNTFSFileInfo.LoadFileAttribute(attrstream: TMemoryStream);
var
    fileentry: TNTFSFileNameAttribute;
    filename: WideString;
begin
    // Traiter le TNTFSFileNameAttribute
    attrstream.ReadBuffer(fileentry, sizeof(TNTFSFileNameAttribute));

    // Récupere le filename
	filename := '';
    SetLength(FileName, fileentry.FileNameLength);
    attrstream.ReadBuffer(FileName[1], fileentry.FileNameLength * 2);

	// Copier le nom selon le NameSpace
    case fileentry.FileNamespace of
        //npPosix:;
        npWin32:	Self.FFileName := FileName;
        npDos:      Self.FShortName := FileName;
        npDosAndWin32: begin
        	Self.FFileName := FileName;
            Self.FShortName := FileName;
        end;
    end;

    //#ATTN Flag sur la taille pour les attr en npDos bit 49
    Self.FFileSize := fileentry.RealSize and MASK_48BITS;
    Self.FParentDirID := fileentry.RefToParentDir and MASK_48BITS;

	SElf.FPermissions := fileentry.Flags;

    // Récuperer les dates
    Self.FCreateDate := TFileTime(fileentry.TimeCreated);
    Self.FWriteDate	 := TFileTime(fileentry.TimeAltered);
    Self.FAccessDate := TFileTime(fileentry.TimeLastRead);
    Self.FMFTDate	 := TFileTime(fileentry.TimeMTFAltr)
end;


// --------------------------------------------------------------------------
//							Gestions des propriétés
// --------------------------------------------------------------------------


function TNTFSFileInfo.GetDosAttributes(): Byte;
begin
	Result := Lo(Integer(Self.FPermissions * [aReadOnly, aHidden, aSystem, aArchive]));
end;


function TNTFSFileInfo.isDirectory(): boolean;
begin
	Result := aDirectory in Self.FPermissions;
end;


function TNTFSFileInfo.isDeleted():boolean;
begin
	Result := not (fInUse in Self.FMFTEntryFlags);
end;


function TNTFSFileInfo.GetFileName(): string;
begin
	Result := string(Self.FFileName);
end;


function TNTFSFileInfo.GetShortName(): string;
begin
	Result := string(Self.FShortName);
end;


procedure TNTFSFileInfo.GetFileProperties(info: TStrings);
begin
	inherited;

    // Info spécifiques NTFS
    // Afficher les date (verif time = 0)
    info.Add(LASTWRITE_STR);
    info.Add(FileTimeToString(Self.WriteDate));
	info.Add(CREATEDATE_STR);
    info.Add(FileTimeToString(Self.CreationDate));
   	info.Add(LASTREAD_STR);
    info.Add(FileTimeToString(Self.AccessDate));
   	info.Add(MFTDATE_STR);
    info.Add(FileTimeToString(Self.FMFTDate));

   	info.Add(NTFSPERMATTR_STR);
   	info.Add(NTFSPermissionsToString(Self.FPermissions));
end;


// --------------------------- Fin de l'unité -----------------------------
end.
