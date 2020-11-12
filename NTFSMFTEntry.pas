{-----------------------------------------------------------------------------
 Unit Name: NTFSMFTEntry
 Author:    Chris
 Purpose:	Gestion des entrées de la MFT
 History:   01/07/2005
-----------------------------------------------------------------------------}


unit NTFSMFTEntry;

interface

uses
	Windows, Classes, SysUtils, Math,
	NTFSAttributes, NTFSFileInfo, GenericPartition;

    
type


EMFTEntryException = class(Exception);
EDriveReadError = class(EMFTEntryException);
EUpdateSequenceError = class(EMFTEntryException);
ENotAFileRecord = class(EMFTEntryException);
ENotAnIndexRecord = class(EMFTEntryException);


//
// Infos pour les données complémentaires aux attributs
//
TDataDescriptor = record
	Size:		int64;			// Taille de l'attribut
    AllocSize:	int64;			// Taille alloué
    DataRun:	TDataRuns;		// Indique clusters contenant les donnés non res
    ResData:	TMemoryStream;	// Contient les données résidentes
end;
 

TMFT = class;		// Déclaration avancée

//
// Classe abstraite pour les entrées de la MFT
//
TMFTEntry = class(TObject)
	private
	protected
    	// --------------- Info du File Record -----------------
		FLogFileSeqNum:		Int64;		// $LogFile Sequence Number (LSN)
    	FSeqNum: 			Word;		// Number of times this mft record has been reused.
	    FReferenceCount:  	Word;       // Number of hard links, i.e. the number of directory entries referencing this record
    	FRecordFlags:		TNTFSMFTRecFlags;	// Directory, Deleted Flag
    	FRealFileRecSize:	Cardinal;	// Real size of the FILE record
    	FAllocatedFileRecSize:Cardinal;
    	FRefToBaseFileRec:	Int64;		// File reference to the base FILE record (48/16)
    	FNextAttrID:		Word;       // The Attribute Id that will be assigned to
    	FNumberOfRec:		Cardinal;	// Number of this MFT Record (XP) NTFS 3.1 ?
    	// --------------- Info du File Record -----------------
        //Mettre Std Attr et FileName Attr ?
        FParentDir:			int64;
        FCTime:				TFileTime;	// C Time - File Creation
        FRTime:				TFileTime;	// R Time - File Read
        FATime:				TFileTime;	// A Time - File Altered
        FMTime:				TFileTime;	// M Time - MFT Changed
        FFileFlags:			TNTFSPermissions;
        FFileSize:			int64;
        FAllocFileSize:		int64;
        //
        FFileName:			WideString;
        FShortName:			WideString;
        // ---------------- Info de gestion -----------------------
        FFileID:			int64;			// FileSeq de cette entrée dans la MFT
        FEntryStream:		TMemoryStream;	// var temporaire?
        // ============================= Methodes =============================
		procedure ParseAttributes();
        procedure ParseFileRecord();
        // -------------------- Event -----------------
        procedure OnStandardInformation(const Attr: TNTFSStandardInfoAttribute); virtual;
        procedure OnAttributeList(const Attr: TNTFSStandardInfoAttribute); virtual; abstract;
        procedure OnFileName(const AttrName: WideString; const Attr: TNTFSFileNameAttribute; FileName: WideString); virtual;
        //procedure OnObjectID(); virtual; abstract;
        //procedure OnSecurityDescriptor(): virtual; abstract;
        procedure OnVolumeName(const VolName: WideString); virtual; abstract;
        procedure OnVolumeInformation(const Attr: TVolumeInformation); virtual; abstract;
        procedure OnData(const AttrName: WideString; const Data: TDataDescriptor); virtual; abstract;
        procedure OnIndexRoot(const AttrName: WideString; const Data: TDataDescriptor); virtual; abstract;
        procedure OnIndexAllocation(const AttrName: WideString; DataRuns: TDataRuns); virtual; abstract;
        procedure OnBitmap(const AttrName: WideString; const Data: TDataDescriptor); virtual; abstract;
        //procedure OnReparsePoint(); virtual; abstract;
        //procedure OnExtendedAttributeInformation(); virtual; abstract;
        //procedure OnExtendedAttribute(); virtual; abstract;
        //procedure OnLoggedUtilyStream(); virtual; abstract;
	public
        // ============================= Methodes =============================
        constructor Create(const id:int64); virtual;
        destructor Destroy; override;
        // ---
        class function Partition(): TGenericPartition;
        class function GetMFT(): TMFT;
        // ============================= Propriétés =============================
        property FileID: int64 read FFileID;
        property ParentID: int64 read FParentDir;
        property Flags: TNTFSPermissions read FFileFlags;
    	//property RecordFlags: TNTFSMFTRecFlags read FRecordFlags;
end;


//
// Classe pour un entrée de type fichier (Pas dir)
//
TMFTFileEntry = class(TMFTEntry)
    private
    protected
		FFileSize:		int64;
        FFileAllocSize:	int64;
        FFileDataRuns:	TDataRuns;	// <> nil si fichier dans des clusters
        FResData:		TMemoryStream;	// <> nil Si le fichier est dans le file record
        // ============================= Methodes =============================
        procedure OnData(const AttrName: WideString; const data: TDataDescriptor); override;
    public
        // ============================= Methodes =============================
        constructor Create(const id: int64); override;
        destructor Destroy; override;
        function ExtractToStream(const pos, len: int64; stream: TStream): boolean;
        //ExtractToBuffer(pos, len: int64; var Buffer);
        // ============================= Propriétés =============================
        property FileSize: int64 read FFileSize;
end;


//
// Classe pour un entrée de type Répertoire
//
TMFTDirEntry = class(TMFTEntry)
    private
		FileSeqArray: TStringList;		// Temporaire pour le parse (mettre en class var)
    	// BitField pour les INDEX Record
        FResIndex:		TMemoryStream;	// Data Index root toujours resident
        FIndexDataRuns:	TDataRuns;		// Datarun from IndexAllocation (Alloué ds ParseAttrib)
        FFileInfoList:	TList;			// Liste des FileInfo
        FDirInfoList:	TList;			// Liste des repertoires
    protected
        // ============================= Methodes =============================
    	procedure ParseIndexEntries(indexstream: TMemoryStream);
        procedure LoadIndexRecord(const vcn: int64);
        //function LoadParseIndexRecord(const vcn: int64): boolean;
		procedure OnIndexRoot(const AttrName: WideString; const Data: TDataDescriptor); override;
        procedure OnIndexAllocation(const AttrName: WideString; DataRuns: TDataRuns); override;
        procedure OnBitmap(const AttrName: WideString; const Data: TDataDescriptor); override;
        function GetFileInfo(ind: Cardinal): TNTFSFileInfo;
        function GetFileInfoCount(): Cardinal;
        function GetDirInfo(ind: Cardinal): TNTFSFileInfo;
        function GetDirInfoCount(): Cardinal;
    public
        // ============================= Methodes =============================
        constructor Create(const id: int64); override;
        destructor Destroy; override;
        procedure DetachFileNodes();
        // ============================= Propriétés =============================
        property FileInfo[ind: Cardinal]: TNTFSFileInfo read GetFileInfo;
        property FileInfoCount: Cardinal read GetFileInfoCount;
        property DirInfo[ind: Cardinal]: TNTFSFileInfo read GetDirInfo;
        property DirInfoCount: Cardinal read GetDirInfoCount;
end;


//
// Classe pour l'entrée $Volume
//
TMFTVolumeEntry = class(TMFTEntry)
    private
    	FVolumeName:	WideString;
        FMinor:			Byte;
        FMajor:			Byte;
        FVolumeFlags:	TVolumeInfoFlags;
    protected
        procedure OnVolumeName(const VolName: WideString); override;
        procedure OnVolumeInformation(const Attr: TVolumeInformation); override;
    public
        // ============================= Methodes =============================
        constructor Create(const id:int64); override;
        destructor Destroy; override;
        // ============================ Propriétés ============================
        property Minor: Byte read FMinor;
        property Major: Byte read FMajor;
        property VolumeName: WideString read FVolumeName;
        property VolumeFlags: TVolumeInfoFlags read FVolumeFlags;
end;


//
// Classe pour l'entrée $MFT
//
TMFT = class(TMFTFileEntry)
    private
    protected
    	BitMapInfo: TDataDescriptor;	// Permet de connaitre les entrées in Use
        // ============================= Methodes =============================
        procedure OnBitmap(const AttrName: WideString; const Data: TDataDescriptor); override;
        class procedure SetPartition(part: TGenericPartition);
        class procedure SetMFT(mft: TMFT);
    public
        // ============================= Methodes =============================
        constructor Create(part: TGenericPartition); overload;
        destructor Destroy; override;
        procedure LoadFileRecord(const fileref: int64; stream: TMemoryStream);
        //function IsEntryInUse(const fileref: int64): boolean;
        // ============================= Propriétés =============================
end;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

uses
	NTFSPartition, Dialogs;

resourcestring
// ---------------------- Exceptions Strings -------------------------
EDriveReadError_STR	= 'Can''t Read FileRecord Entry %d';


var
	// Varaibles de classe
	ClassPartition: TGenericPartition;	// Partition commune aux MFTEntries (class var)
    ClassMFT:		TMFT;		// Entrée $MFT

	Buffer:	array of Byte;		// Buffer pour le cluster
    usa: array[0..127] of Word;	// Tableau pour l'Update Sequence Array

    
// ----------------------------------------------------------------------
//
// ----------------------------------------------------------------------


//
//
//
procedure ClearDataDescriptor(data: TDataDescriptor);
begin
	data.Size 		:= 0;
    data.AllocSize	:= 0;
    FreeAndNil(data.ResData);
    FreeAndNil(data.DataRun);
end;


// =====================================================================
//	   						Gestion des TMFTEntry
// =====================================================================


constructor TMFTEntry.Create(const id: int64);
begin
	Self.FFileID := id;

    Self.FEntryStream := TMemoryStream.Create;
    try
        //DEBUGStringList.Add(Format('--> Calling GetMFT.LoadFileRecord: ID = %d', [id]));	//DEBUG
        // Charger le File Record
        TMFTEntry.GetMFT.LoadFileRecord(id, Self.FEntryStream);
        //DEBUGStringList.Add('<-- GetMFT.LoadFileRecord Returned');	//DEBUG

        //DEBUGStringList.Add(Format('--> Calling ParseFileRecord: ID = %d', [id]));	//DEBUG
        // Parser le File Record Header
        Self.ParseFileRecord;
        //DEBUGStringList.Add('<-- ParseFileRecord Returned');	//DEBUG

        //DEBUGStringList.Add(Format('--> Calling ParseAttributes: ID = %d', [id]));	//DEBUG
        // Parser les attributs
        Self.ParseAttributes();
        //DEBUGStringList.Add('<-- ParseAttributes Returned');	//DEBUG
    finally
		FreeAndNil(Self.FEntryStream);
    end;
end;


destructor TMFTEntry.Destroy;
begin
	FreeAndNil(Self.FEntryStream);
end;


class function TMFTEntry.Partition(): TGenericPartition;
begin
	Result := ClassPartition;
end;


class function TMFTEntry.GetMFT(): TMFT;
begin
	Result := ClassMFT;
end;


// ----------------------------------------------------------------------
// 					Gestion FileRecord et Parsing Attr
// ----------------------------------------------------------------------


//
//
//
procedure TMFTEntry.ParseFileRecord();
var
	rec: TNTFSFileRecordHeader;
    i: integer;
    usn, chkusn: Word;
begin
    Self.FEntryStream.ReadBuffer(rec, sizeof(TNTFSFileRecordHeader));
    if rec.FileID <> 'FILE' then
        Raise ENotAFileRecord.CreateFmt('Not Valid FILE @ ID = %d', [Self.FFileID]);

    // Traiter l'Update Sequence
    // Récuperer l'USN
    Self.FEntryStream.Position := rec.UpdateSeqOffset;
    Self.FEntryStream.ReadBuffer(usn, sizeof(Word));

    // Récuperer l'Update Sequence Array
    Self.FEntryStream.ReadBuffer(usa[0], (rec.UpdateSeqSize - 1) * sizeof(Word));
    for i := 0 to rec.UpdateSeqSize - 2 do
    begin
        //#Todo mettre Part.SectorSize ?
        Self.FEntryStream.Position := 510 + (i * 512);
        Self.FEntryStream.ReadBuffer(chkusn, sizeof(Word));
        if usn = chkusn then
        begin
            Self.FEntryStream.Position := 510 + (i * 512);
            Self.FEntryStream.WriteBuffer(usa[i], sizeof(Word));
        end
        else
            raise EUpdateSequenceError.CreateFmt('FileRecord %d corrupted', [Self.FFileID]);
    end; // for

    // Recuperer les infos du FileRecord Header
    Self.FLogFileSeqNum		:= rec.LogFileSeqNum;
    Self.FSeqNum			:= rec.SeqNum;
    Self.FReferenceCount	:= rec.ReferenceCount;
    Self.FRecordFlags		:= rec.Flags;
    Self.FRealFileRecSize	:= rec.RealFileRecSize;
    Self.FAllocatedFileRecSize:= rec.AllocatedFileRecSize;
    Self.FRefToBaseFileRec	:= rec.RefToBaseFileRec;	// Mask 48 ???
    Self.FNextAttrID		:= rec.NextAttrID;
    Self.FNumberOfRec		:= rec.NumberOfRec;

    // Se positionner sur le 1er attribut
    Self.FEntryStream.Position := rec.OffsetToFirstAttr;
end;


//
// Parsing des attributs du FileRecord
//
procedure TMFTEntry.ParseAttributes();
var
    AttrName, VolName, filename: WideString;
	AttrType: Cardinal;
    CurPos: int64;
    commonHeader:	TCommonAttributeHeader;
    ExtraRes:		TExtraResidentAttrHeader;
    ExtraNonRes:	TExtraNonResidentAttrHeader;
    StdInfo:		TNTFSStandardInfoAttribute;
    fileatt: 		TNTFSFileNameAttribute;
    volinfo: 		TVolumeInformation;
    indexroot:		TNTFSIndexRoot;		// $INDEX_ROOT (0x90)
	indexheader:	TNTFSIndexHeader;	// $INDEX_ROOT (0x90)
	data:			TDataDescriptor;

	//
    // Récupère le Data Run pour les attributs non résidents
    //
    procedure GetDataRun();
    begin
        // Analyse
        data.DataRun	:= TDataRuns.Create(Self.FEntryStream);
        data.ResData	:= nil;
        data.Size		:= ExtraNonRes.RealSize;
        data.AllocSize	:= ExtraNonRes.AllocatedSize;
    end;

    procedure GetResData();
    begin
        data.Size		:= int64(ExtraRes.AttributeLength);
        data.AllocSize	:= 0;

        // Charger dans le TMemory Stream
        data.ResData := TMemorystream.Create;
        data.ResData.CopyFrom(Self.FEntryStream, data.Size);

        // Revenir au début
        data.ResData.Position := 0;
    end;

begin
	// Lecture avant de l'AttributeType
    Self.FEntryStream.ReadBuffer(AttrType, Sizeof(Cardinal));

    While (AttrType <> NTFS_AttribID_End_Maker) do
    begin
        data.Size := 0;
        data.AllocSize := 0;
        data.DataRun := nil;
        data.ResData := nil;

        // Se repositinner sur le début de l'attribut
        Self.FEntryStream.Seek(-Sizeof(Cardinal), soFromCurrent);
        CurPos := Self.FEntryStream.Position;

		// Lecture de l'en tête de l'attribut
        Self.FEntryStream.ReadBuffer(commonHeader, sizeof(TCommonAttributeHeader));

        //#DEBUG
        {if (AttrType div 16) in [$1..$10] then
        	DEBUGStringList.Add(Format('  Attribute: %s ($%x)', [AttribNames[AttrType div 16], AttrType]))
        else
        	DEBUGStringList.Add(Format('  Attribute custom : $%x', [AttrType]));

		DEBUGStringList.Add(Format('    Attrib size: %d ($%x)',[commonHeader.Size, commonHeader.Size]));	//#DEBUG}

        // Lit le reste de l'attribut Header
        if (commonHeader.NonResident) then
        begin
			Self.FEntryStream.ReadBuffer(ExtraNonRes, sizeof(TExtraNonResidentAttrHeader));

        	{DEBUGStringList.Add('    Non Resident');		//#DEBUG
			DEBUGStringList.Add(Format('    Starting VCN: %d',[ExtraNonRes.StartingVCN and MASK_48BITS]));	//#DEBUG
			DEBUGStringList.Add(Format('    Last VCN: %d',[ExtraNonRes.LastVCN]));	//#DEBUG
			DEBUGStringList.Add(Format('    Data Runs Offset: %d',[ExtraNonRes.DataRunsOffset]));	//#DEBUG
			DEBUGStringList.Add(Format('    Allocated Size: %d',[ExtraNonRes.AllocatedSize]));	//#DEBUG
			DEBUGStringList.Add(Format('    Attr Data Real Size: %d',[ExtraNonRes.RealSize]));	//#DEBUG}
        end
        else
        begin
			Self.FEntryStream.ReadBuffer(ExtraRes, sizeof(TExtraResidentAttrHeader));
        	//DEBUGStringList.Add('    Resident');		//#DEBUG
        end;

		// Lire le nom
        if (commonHeader.NameLength > 0) then
        begin
        	SetLength(AttrName, commonHeader.NameLength);
            Self.FEntryStream.ReadBuffer(AttrName[1], commonHeader.NameLength * 2);

            //DEBUGStringList.Add(Format('    Attr Name: %s', [String(AttrName)]));	//DEBUG
        end;

        // Si Résident se positionner sur les données de l'attributs
        if not (commonHeader.NonResident) then
	        Self.FEntryStream.Seek(CurPos + ExtraRes.AttributeOffset, soFromBeginning);

        // Gestion des différents attributs
        // Le flux pointe sur les données de l'attribut
        case commonHeader.AttributeType of
            // $STANDARD_INFORMATION (0x10)
            NTFS_AttribID_Standard_Information: begin
				Self.FEntryStream.ReadBuffer(StdInfo , sizeof(TNTFSStandardInfoAttribute));

                //Self.OnStandardInformation(StdInfo);
            end;

        	// $ATTRIBUTE_LIST (0x20)
            NTFS_AttribID_Attribute_List: ;	// Gerer le chargement de info complémentaires

            // $FILE_NAME (0x30)
            NTFS_AttribID_File_Name: begin
				Self.FEntryStream.ReadBuffer(fileatt, sizeof(TNTFSFileNameAttribute));

                SetLength(FileName, fileatt.FileNameLength);
                Self.FEntryStream.ReadBuffer(FileName[1], fileatt.FileNameLength * 2);

                {DEBUGStringList.Add(Format('INF File Name: %s', [String(FileName)]));
                DEBUGStringList.Add(Format('INF NameSpace: %d', [Ord(fileatt.FileNamespace)]));
                DEBUGStringList.Add(Format('INF Parent Reference: %d', [fileatt.RefToParentDir and MASK_48BITS]));
                DEBUGStringList.Add(Format('INF Real Size: %d', [fileatt.RealSize]));
                DEBUGStringList.Add(Format('INF Allocated Size: %d', [fileatt.AllocatedSize]));
                DEBUGStringList.Add(Format('INF Flags: %s', [NTFSPermissionsToString(fileatt.Flags)]));}

			    //DEBUGStringList.Add('--> Calling OnFileName');	//DEBUG
                Self.OnFileName(AttrName, fileatt, FileName);
			    //DEBUGStringList.Add('<-- OnFileName Returned');	//DEBUG
            end;

            // $VOLUME_NAME (0x60)
            NTFS_AttribID_Volume_Name: begin
            	// Taille en octet ds ExtraRes.AttributeLength,
                SetLength(VolName, ExtraRes.AttributeLength div 2);	// Unicode
                Self.FEntryStream.ReadBuffer(VolName[1], ExtraRes.AttributeLength);

			    //DEBUGStringList.Add('--> Calling OnVolumeName');	//DEBUG
                Self.OnVolumeName(VolName);
			    //DEBUGStringList.Add('<-- Returned');	//DEBUG
            end;

            // $VOLUME_INFORMATION (0x70)
            NTFS_AttribID_Volume_Information: begin
            	Self.FEntryStream.ReadBuffer(volinfo, sizeof(TVolumeInformation));

			    //DEBUGStringList.Add('--> Calling OnVolumeInformation');	//DEBUG
                Self.OnVolumeInformation(volinfo);
			    //DEBUGStringList.Add('<-- Returned');	//DEBUG
            end;

			// $DATA (0x80)
            // Si non res taille dans TExtraNonResidentAttrHeader.RealSize
            // Si Res taille dans TExtraResidentAttrHeader.Cardinal
            NTFS_AttribID_Data: begin
		        if (commonHeader.NonResident) then
                begin
                    // Se positionner sur le data run
                    Self.FEntryStream.Seek(CurPos + ExtraNonRes.DataRunsOffset, soFromBeginning);

                    // Analyse
                    data.DataRun	:= TDataRuns.Create(Self.FEntryStream);
                    data.Size		:= ExtraNonRes.RealSize;
                    data.AllocSize	:= ExtraNonRes.AllocatedSize;
                end
                else
                begin
                	// Se positionner sur les données
                    Self.FEntryStream.Seek(CurPos + ExtraRes.AttributeOffset, soFromBeginning);

                	data.Size := int64(ExtraRes.AttributeLength);

                    // Charger dans le TMemory Stream
                    data.ResData := TMemorystream.Create;
					data.ResData.CopyFrom(Self.FEntryStream, data.Size);
                    data.ResData.Position := 0;
                end;

			    //DEBUGStringList.Add('--> Calling OnData');	//DEBUG
                Self.OnData(AttrName, data);
			    //DEBUGStringList.Add('<-- Returned');	//DEBUG
            end;

            // $INDEX_ROOT (0x90)
            NTFS_AttribID_Index_Root: begin
				Self.FEntryStream.ReadBuffer(indexroot, sizeof(TNTFSIndexRoot));
				Self.FEntryStream.ReadBuffer(indexheader, sizeof(TNTFSIndexHeader));

                //#DEBUG
                {DEBUGStringList.Add(Format('    Attribute Type: $%x', [indexroot.AttributeType]));
                info.Add(Format('    Size of Index Alloc Entry: %d', [indexroot.SizeOfIndexAllocEntry]));
                info.Add(Format('    Cluster Per Index Record: %d', [indexroot.ClustPerIndexRec]));
                info.Add(Format('    Flags: $%x', [indexheader.Flags]));}

                // Small or large index ? Comment le passer en param ?
                // deduire du datarun IndexAlloc !
                //if (indexheader.Flags and $01) = 0 then

                Self.FEntryStream.Seek(indexheader.OffsetOfFirstEntry - sizeof(TNTFSIndexHeader), soFromCurrent);

                // Data always resident
                data.ResData := TMemoryStream.Create;
                data.ResData.CopyFrom(Self.FEntryStream, indexheader.SizeOfIndexEntries - $10);
                data.ResData.Position := 0;

			    //DEBUGStringList.Add('--> Calling OnIndexRoot');	//DEBUG
                Self.OnIndexRoot(AttrName, data);
			    //DEBUGStringList.Add('<-- Returned');	//DEBUG
            end;

            // $INDEX_ALLOCATION (0xA0)
            NTFS_AttribID_Index_Allocation: begin
                // Se positionner sur le data run
                Self.FEntryStream.Seek(CurPos + ExtraNonRes.DataRunsOffset, soFromBeginning);

                // Analyse (Always Non Res)
                GetDataRun();
			    //DEBUGStringList.Add('--> Calling OnIndexAllocation');	//DEBUG
                Self.OnIndexAllocation(AttrName, data.DataRun);
			    //DEBUGStringList.Add('<-- Returned');	//DEBUG
            end;

            // $BITMAP (0xB0)
            NTFS_AttribID_Bitmap: begin
		        if (commonHeader.NonResident) then
                begin
                    // Se positionner sur le data run
                    Self.FEntryStream.Seek(CurPos + ExtraNonRes.DataRunsOffset, soFromBeginning);

                    GetDataRun();
                end
                else
                begin
                	// Se positionner sur les données
                    Self.FEntryStream.Seek(CurPos + ExtraRes.AttributeOffset, soFromBeginning);

                    GetResData();
                end;

			    //DEBUGStringList.Add('--> Calling OnBitmap');	//DEBUG
                Self.OnBitmap(AttrName, data);
			    //DEBUGStringList.Add('<-- Returned');	//DEBUG
            end;
        else ;
        end; // case

		// Se positionner sur l'attribut suivant
        Self.FEntryStream.Seek(CurPos + commonHeader.Size, soFromBeginning);

        // Lecture avant de l'AttributeType
        Self.FEntryStream.ReadBuffer(AttrType, Sizeof(Cardinal));
    end;
end;


// ----------------------------------------------------------------------
//							Gestion des evenements
// ----------------------------------------------------------------------


procedure TMFTEntry.OnStandardInformation(const Attr: TNTFSStandardInfoAttribute);
begin
	// Recopier des infos sur les ObjectId, secure... ?
end;


procedure TMFTEntry.OnFileName(const AttrName: WideString; const Attr: TNTFSFileNameAttribute; FileName: WideString);
begin
	if AttrNAme = '' then
    begin
        Self.FATime := TFileTime(Attr.TimeAltered);
        Self.FRTime := TFileTime(Attr.TimeLastRead);
        Self.FCTime := TFileTime(Attr.TimeCreated);
        Self.FMTime := TFileTime(Attr.TimeMTFAltr);

        Self.FFileFlags := Attr.Flags; 
    
        // Copier le nom selon le NameSpace
        case Attr.FileNamespace of
            //npPosix:;
            npWin32:
                Self.FFileName := FileName;
            npDos:
                Self.FShortName := FileName;
            npDosAndWin32: begin
                Self.FFileName := FileName;
                Self.FShortName := FileName;
            end;
        end; //case
    end; // if
end;


// =====================================================================
// 						Gestion des TMFTFileEntry
// =====================================================================


constructor TMFTFileEntry.Create(const id: int64);
begin
	inherited;
    //DEBUGStringList.Add(Format('<-- inherited TMFTFileEntry.Create Returned: ID = %d', [id]));	//DEBUG

    // Verifier si c'est bien un fichier
    if (aDirectory in Self.FFileFlags) then
    	raise EMFTEntryException.CreateFmt('Entry ID #%d is not a file', [id]);
end;


destructor TMFTFileEntry.Destroy;
begin
	inherited;

    // Libère les ressources allouées lor sde l'analyse
    Self.FFileDataRuns.Free;
    Self.FResData.Free;
end;


// ----------------------------------------------------------------------
//							Gestion des evenements
// ----------------------------------------------------------------------


//
//
//
procedure TMFTFileEntry.OnData(const AttrName: WideString; const data: TDataDescriptor);
begin
	if AttrName = '' then
    begin
    	Self.FFileDataRuns	:= data.DataRun;
        Self.FResData		:= data.ResData;
        Self.FFileSize		:= data.Size;
        Self.FFileAllocSize := data.AllocSize;
    end
    else
    begin
		data.DataRun.Free;
        data.ResData.Free;
    end;
end;


// ----------------------------------------------------------------------
//			 		Gestion de la lecture du fichier
// ----------------------------------------------------------------------


//
// Extrait les données du fichier et les ecrit dans le stream
//
function TMFTFileEntry.ExtractToStream(const pos, len: int64; stream: TStream): boolean;
var
	i : integer;
    cluster: Cardinal;
    offset: int64;
    vcn: int64;
    copysize: int64;
    nbclust: int64;
    nbsectlu: word;
    TempStream: TMemoryStream;	// pour les parties de clusters non completes
begin
    Result := true;
    
	//
    if len = 0 then
    	exit;

    if (pos > Self.FileSize) or (pos + len > Self.FileSize) then
    	raise Exception.Create('Seek Overflow')
    else
    	if Assigned(Self.FResData) then
        begin
        	Self.FResData.Seek(pos, soFromBeginning);
            stream.CopyFrom(Self.FResData, len);
        end
        else
        begin
        	TempStream := TMemoryStream.Create;
            TempStream.SetSize(TMFTFileEntry.Partition.ClusterSize);

			offset := pos mod TMFTFileEntry.Partition.ClusterSize;
            vcn := pos div TMFTFileEntry.Partition.ClusterSize;

            copysize := min(len, TMFTFileEntry.Partition.ClusterSize - offset);
            cluster := Self.FFileDataRuns.GetLCN(vcn);
            nbsectlu := Self.Partition.ReadClusterToStream(cluster, TempStream);
            if nbsectlu <> Self.Partition.SectorPerCluster then
            	Result := false;

            TempStream.Position := offset;
			stream.CopyFrom(TempStream, copysize);

            // Données sur plusieurs clusters ?
            if offset + len > TMFTFileEntry.Partition.ClusterSize then
            begin
				// Les données sont sur plusieurs cluster

                nbclust := (len - copysize) div TMFTFileEntry.Partition.ClusterSize;
                for i := 0 to integer(nbclust) - 1 do
                begin
                    //boucle sur les nbclust
                	inc(vcn);
		            cluster := Self.FFileDataRuns.GetLCN(Cardinal(vcn));
        		    nbsectlu := Self.Partition.ReadClusterToStream(cluster, stream);
                    if nbsectlu <> Self.Partition.SectorPerCluster then
                    	Result := false;
                end; // for

                // morceau de cluster restant à traiter
                //TempStream.Clear;
                TempStream.Position := 0;	// pas la peine de désallouer la mem

                inc(vcn);
                cluster := Self.FFileDataRuns.GetLCN(Cardinal(vcn));
	            nbsectlu := Self.Partition.ReadClusterToStream(cluster, TempStream);
                if nbsectlu <> Self.Partition.SectorPerCluster then
                	Result := false;

                // calcul de la taille restante
                copysize := len - copysize - nbclust * TMFTFileEntry.Partition.ClusterSize;
				stream.CopyFrom(TempStream, copysize);
            end;

            TempStream.Free;
        end; // if
end;


// =====================================================================
// 						Gestion de l'entrée $MFT
// =====================================================================


//
//
//
constructor TMFT.Create(part: TGenericPartition);
begin
	TMFT.SetPartition(part);
    TMFT.SetMFT(Self);

    SetLength(Buffer, TMFT.Partition.ClusterSize);
    // Mettre l'objet dans une variable de classe ???

    // Bootstrap fileref($MFT) = 0
    inherited Create(0);
end;


destructor TMFT.Destroy;
begin
	inherited;

    SetLength(Buffer, 0);

	ClearDataDescriptor(Self.BitMapInfo);

    TMFT.SetMFT(nil);
    TMFT.SetPartition(nil); // Normalement derniere entrée à être détruite
end;


class procedure TMFT.SetPartition(part: TGenericPartition);
begin
	ClassPartition := part;
end;


class procedure TMFT.SetMFT(mft: TMFT);
begin
	ClassMFT := mft;
end;


//
// Copie le FileRecord dans le stream
//
procedure TMFT.LoadFileRecord(const fileref: int64; stream: TMemoryStream);
var
	i: integer;
    nbclust: Cardinal;
    nbsectlu: word;
begin
	//#Todo2 quand FileRecordSize < ClusterSize => Chager les secteurs du FileRecordSize
    // pour ne pas déclarer comme defectueux un filerecord dont une une partie du
    // cluster a des secteurs defectueux
    //Self.Partition.GetFirstSectorOfCluster()
     
	// Boot strap ?
    if fileref = FILE_MFT then
    begin
        nbclust := TNTFSPartition(TMFT.Partition).FileRecordSize div TMFT.Partition.ClusterSize;
        if nbclust > 0 then
	    	for i := 0 to nbclust - 1 do
            begin
                // Lire le ou les clusters
                nbsectlu := TMFT.Partition.ReadClusterToBuffer(Cardinal(TNTFSPartition(TMFT.Partition).FirstMTFCluster + i),
                                                   @Buffer[0], caFillInvalid);
                if nbsectlu <> TMFT.Partition.SectorPerCluster then
                	raise EDriveReadError.CreateFmt(EDRIVEREADERROR_STR, [fileref]);

                stream.WriteBuffer(Buffer[0], TMFT.Partition.ClusterSize);
            end
        else
        begin
        	// Lire le cluster
            //#TOdo1 Remplacer par la lecture des secteurs composant le file record
            nbsectlu := TMFT.Partition.ReadClusterToBuffer(Cardinal(TNTFSPartition(TMFT.Partition).FirstMTFCluster),
                                               @Buffer[0], caFillInvalid);
            if nbsectlu <> Self.Partition.SectorPerCluster then
            	raise EDriveReadError.CreateFmt(EDRIVEREADERROR_STR, [fileref]);

            stream.WriteBuffer(Buffer[0], TNTFSPartition(TMFT.Partition).FileRecordSize);
        end;
    end
    else
    	if not Self.ExtractToStream(fileref * TNTFSPartition(TMFT.Partition).FileRecordSize,
					         TNTFSPartition(TMFT.Partition).FileRecordSize,
                             stream) then
        	Raise EDriveReadError.CreateFmt(EDRIVEREADERROR_STR, [fileref]);

    // Se placer au début
    stream.Position := 0;
end;


// ----------------------------------------------------------------------
//							Gestion des evenements
// ----------------------------------------------------------------------


//
//
//
procedure TMFT.OnBitmap(const AttrName: WideString; const Data: TDataDescriptor);
begin
	if AttrNAme = '' then
    begin
    	Self.BitMapInfo := data;
        //#Todo2 Charger le bitmap dans un Tmemory stream, puis bitfield
    end
    else
		ClearDataDescriptor(data);
end;


// =====================================================================
// 						Gestion des TMFTDirEntry
// =====================================================================


constructor TMFTDirEntry.Create(const id: int64);
begin
	inherited;
    //DEBUGStringList.Add(Format('<-- inherited TMFTDirEntry.Create Returned: ID = %d', [id]));	//DEBUG

    // Verifier si c'est bien un répertoire
    if not (aDirectory in Self.FFileFlags) then
	    raise EMFTEntryException.CreateFmt('Entry ID %d is not a directory', [id]);

    // Créer la liste des FileInfo
    Self.FFileInfoList := TList.Create;
    SElf.FDirInfoList := TList.Create;

    {if (Self.FResIndex = nil) and (Self.FIndexDataRuns = nil) then
	    DEBUGStringList.Add(Format('!!! Error: No Index Info Entry (ID = %d)', [id]));	//DEBUG

    if Self.FIndexDataRuns <> nil then
	    DEBUGStringList.Add(Format('INF DataRun Cluster count= %d', [Self.FIndexDataRuns.TotalClusterCount]));	//DEBUG}

    // Pour le cas ou plusieurs filename attr pour le même fichier
	FileSeqArray := TStringList.Create;
    try
        FileSeqArray.Sorted := true;
        FileSeqArray.Duplicates := dupError;
        FileSeqArray.CaseSensitive := true;

        //DEBUGStringList.Add(Format('--> Calling ParseIndexEntries: ID = %d', [id]));	//DEBUG
        Self.ParseIndexEntries(Self.FResIndex);
        //DEBUGStringList.Add(Format('<-- ParseIndexEntries Returned: ID = %d', [id]));	//DEBUG
    finally
		FileSeqArray.Free;
    end;
end;


destructor TMFTDirEntry.Destroy;
var
	i: integer;
begin
	inherited;

    // Supprimer le datarun récupéré lors de l'analyse
    Self.FIndexDataRuns.Free;
    Self.FResIndex.Free;

    // Supprimmer les listes ???

    if Assigned(Self.FFileInfoList) then
        for i := 0 to Self.FileInfoCount - 1 do
            Self.FileInfo[i].Free;

    if Assigned(Self.FDirInfoList) then
        for i := 0 to Self.DirInfoCount - 1 do
            Self.DirInfo[i].Free;

    Self.FFileInfoList.Free;
    Self.FDirInfoList.Free;
end;


//
// Supprime les réferences aux FileInfo => libéreration TMFTDirEntry sans supprimer les FileInfo
//#todo3 mettre un flag à la place ?
procedure TMFTDirEntry.DetachFileNodes();
begin
	if Assigned(Self.FFileInfoList) then
		Self.FFileInfoList.Clear;
	if Assigned(Self.FDirInfoList) then
    	Self.FDirInfoList.Clear;
end;


// ----------------------------------------------------------------------
//							Gestion des evenements
// ----------------------------------------------------------------------


procedure TMFTDirEntry.OnIndexRoot(const AttrName: WideString; const Data: TDataDescriptor);
begin
	// Info index répertoire ?
    if AttrName = '$I30' then
		Self.FResIndex := data.ResData
    else
    	data.ResData.Free;	// Data non traité => le libérer
end;


//
//
//
procedure TMFTDirEntry.OnIndexAllocation(const AttrName: WideString; DataRuns: TDataRuns);
begin
	// Info index répertoire ?
    if AttrName = '$I30' then
		Self.FIndexDataRuns := DataRuns
    else
    	DataRuns.Free;	// Data run non traité => le libérer
end;


//
//
//
procedure TMFTDirEntry.OnBitmap(const AttrName: WideString; const Data: TDataDescriptor);
begin
	// Info index répertoire ?
    if AttrName = '$I30' then
    begin
		//if Assigned(DataRuns) then     //#Todo2 verifier bitmap Index Root
        //begin
        	// Charger le bitmap ????
        //end;
        // Le bitmap est inutile pour le scan des répertoires
       	ClearDataDescriptor(data);
    end
    else
    	ClearDataDescriptor(data);
end;


// ----------------------------------------------------------------------
//							Gestion des Index 
// ----------------------------------------------------------------------

//
// Analyse la liste des couples <IndexEntry><FileNameAttr>
//
procedure TMFTDirEntry.ParseIndexEntries(indexstream: TMemoryStream);
var
	CurrentEntry: TNTFSIndexEntry;
    fileentry: TNTFSFileNameAttribute;
    CurrrentPos, SubNodeVCN: int64;
    FileName: WideString;
    fileinf: TNTFSFileInfo;
    ind: integer;
    isexist: boolean;
begin

	// IndexEntry dans un stream
	repeat
    	CurrrentPos := indexstream.Position;

		// Lire l'indexentry
        indexstream.ReadBuffer(CurrentEntry, sizeof(TNTFSIndexEntry));

        // Subnode ? si oui => Charger et analyser le VCN subnode
        if (CurrentEntry.IndexEntryFlags and $1) <> 0 then
        begin
	        indexstream.Position := CurrrentPos + CurrentEntry.Length - 8;
			indexstream.ReadBuffer(SubNodeVCN, sizeof(SubNodeVCN));
            SubNodeVCN := SubNodeVCN and MASK_48BITS;

            // Verifier avec le bitmap si IndexRecord
            // Charger l'INDX record correspondant
            //#Todo2 Metttre un try except ? => OnError + Message
            try
                //DEBUGStringList.Add(Format('--> Calling LoadIndexRecord: ID = %d, vcn = %d', [Self.FFileID, SubNodeVCN]));	//DEBUG
                Self.LoadIndexRecord(SubNodeVCN);
                //DEBUGStringList.Add(Format('<-- Returned LoadIndexRecord: ID = %d, vcn = %d', [Self.FFileID, SubNodeVCN]));	//DEBUG
            except
				//on E: Exception do DebugExceptionRaised(E.Message);
				// OnError()
                //#Todo1 Remonter l'erreur (mettre un warning), on peut continuer
                // le parsing des autres IndexRecord
            end;
        end;

        // Se positionner sur FileNAme Attr
        indexstream.Position := CurrrentPos + sizeof(TNTFSIndexEntry);

        // Dernière entrée ? (Pas de stream dans la dernière entrée)
        if (CurrentEntry.IndexEntryFlags and $2) = 0 then
        begin
            isexist := FileSeqArray.Find(IntToHex(CurrentEntry.FileReference and MASK_48BITS, 12), ind);
        	if not isexist then
            begin
                // Créer et ajouter si pas deja dans la liste
                fileinf := TNTFSFileInfo.Create(CurrentEntry.FileReference);
                FileSeqArray.AddObject(IntToHex(CurrentEntry.FileReference and MASK_48BITS, 12), fileinf);
            end
            else
            	fileinf := TNTFSFileInfo(FileSeqArray.Objects[ind]);

            // Renseigne ou mets à jour
            fileinf.LoadFileAttribute(indexstream);

            // Si nouvelle entrée, mettre dans la liste correspondante (après LoadFileAttribute)
            if not isexist then
            	if fileinf.isDirectory then
					Self.FDirInfoList.Add(fileinf)
                else
					Self.FFileInfoList.Add(fileinf);
        end;

        // Se positinner sur l'entrée suivante
        indexstream.Position := CurrrentPos + CurrentEntry.Length;
    until (CurrentEntry.IndexEntryFlags and $2) <> 0;
end;


//
//function TMFTDirEntry.LoadParseIndexRecord(const vcn: int64): boolean;
//
procedure TMFTDirEntry.LoadIndexRecord(const vcn: int64);
var
	cluster: Cardinal;
    indexrecord: TMemoryStream;
    rec: TNTFSIndexRecordHeader;
    i: integer;
    usn, chkusn, nbsectlus: Word;
begin
    indexrecord := TMemoryStream.Create;
	try
        indexrecord.SetSize(TNTFSPartition(Self.Partition).IndexRecordSize);

        {*
         Cluster size of each index block (in the index allocation attribute),
         when an index block is >= than a cluster, otherwise this will be the
         log of the size (like how the encoding of the mft record size and the
         index record size found in the boot sector work). Has to be a power of 2.

         IndexRecord sur plusieurs clusters => utlise vcn n, vcn n + 1, ???

        *}
        if Self.Partition.ClusterSize > TNTFSPartition(Self.Partition).IndexRecordSize then
        begin
            // Cas qui ne devrait pas arriver
            raise Exception.Create('Self.Partition.ClusterSize > TNTFSPartition(Self.Partition).IndexRecordSize');
        end
        else
            for i := 0 to TNTFSPartition(Self.Partition).ClustersPerIndexBlock - 1 do
            begin
                cluster := Cardinal(Self.FIndexDataRuns.GetLCN(vcn + i));
                nbsectlus := Self.Partition.ReadClusterToStream(cluster, indexrecord);
                if Self.Partition.SectorPerCluster <> nbsectlus then
                    raise EDriveReadError.CreateFmt('Error Reading Cluster %d', [cluster]); 
            end;

        // Parsing
        indexrecord.Position := 0;
        indexrecord.ReadBuffer(rec, sizeof(TNTFSIndexRecordHeader));
        if rec.IndxID = 'INDX' then
        begin
            // Récuperer l'USN
            indexrecord.Position := rec.UpdateSeqOffset;
            indexrecord.ReadBuffer(usn, sizeof(Word));

            // Récuperer l'Update Sequence Array
            indexrecord.ReadBuffer(usa[0], (rec.UpdateSeqSize - 1) * sizeof(Word));

            // Vérif Update Sequence
            for i := 0 to rec.UpdateSeqSize - 2 do
            begin
                //#Todo mettre Part.SectorSize ?
                indexrecord.Position := 510 + (i * 512);
                indexrecord.ReadBuffer(chkusn, sizeof(Word));
                if usn = chkusn then
                begin
                    indexrecord.Position := 510 + (i * 512);
                    indexrecord.WriteBuffer(usa[i], sizeof(Word));
                end
                else
                    raise EUpdateSequenceError.CreateFmt('IndexRecord %d, vcn %d corrupted', [Self.FFileID, vcn]);
            end; // for

            // Feuille ou  Container ? pas besoin d'utiliser cette info pour le scan
            //if rec.NotLeafNode then
            //	MessageDlg(Format('Sub Node for ID %d', [Self.FileID]), mtInformation, [mbOK], 0);	//DEBUG

            //
            indexrecord.Position := rec.BytesToIndexEntries + $18;
            Self.ParseIndexEntries(indexrecord);
        end;
    finally
	    indexrecord.Free;
    end;
end;


// ----------------------------------------------------------------------
//						  Gestion des propriétés
// ----------------------------------------------------------------------


function TMFTDirEntry.GetDirInfo(ind: Cardinal): TNTFSFileInfo;
begin
	Result := TNTFSFileInfo(Self.FDirInfoList[ind]);
end;


function TMFTDirEntry.GetDirInfoCount(): Cardinal;
begin
	Result := Self.FDirInfoList.Count;
end;


function TMFTDirEntry.GetFileInfo(ind: Cardinal): TNTFSFileInfo;
begin
	Result := TNTFSFileInfo(Self.FFileInfoList[ind]);
end;


function TMFTDirEntry.GetFileInfoCount(): Cardinal;
begin
	Result := Self.FFileInfoList.Count;
end;


// =====================================================================
// 						Gestion des TMFTVolumeEntry
// =====================================================================


constructor TMFTVolumeEntry.Create(const id:int64);
begin
	inherited;
end;


destructor TMFTVolumeEntry.Destroy;
begin
	inherited;
end;


// ----------------------------------------------------------------------
//							Gestion des evenements
// ----------------------------------------------------------------------


procedure TMFTVolumeEntry.OnVolumeName(const VolName: WideString);
begin
	Self.FVolumeName := VolName; 
end;


procedure TMFTVolumeEntry.OnVolumeInformation(const Attr: TVolumeInformation);
begin
	Self.FMinor := Attr.MinorVersion;
	Self.FMajor := Attr.MajorVersion;
	Self.FVolumeFlags := Attr.VolumeInfoFlags;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
