{-----------------------------------------------------------------------------
 Unit Name: NTFSAttributes
 Author:    Chris
 Purpose:	Definitions des attributs NTFS, FileRecord
 History:   27/04/2005
-----------------------------------------------------------------------------}

unit NTFSAttributes;


interface


uses
	Windows, Classes, SysUtils, StrUtils;


const

MASK_48BITS		= $ffffffffffff;		// Mask pour File Sequence reference


// Entrées de la MFT standard
FILE_MFT		= 0;	// Master file table (mft). Data attribute contains the
						// entries and bitmap attribute records which ones are in use (bit==1).
FILE_MFTMirr	= 1;	// Mft mirror: copy of first four mft records in data attribute.
						// If cluster size > 4kiB, copy of first N mft records, with
			   			// N = cluster_size / mft_record_size.
FILE_LogFile	= 2;	// Journalling log in data attribute.
FILE_Volume		= 3;	// Volume name attribute and volume information attribute.
FILE_AttrDef	= 4;	// Array of attribute definitions in data attribute.
FILE_root		= 5;	// Root directory.
FILE_Bitmap		= 6;	// Allocation bitmap of all clusters (lcns) in data attribute.
FILE_Boot		= 7;	// Boot sector (always at cluster 0) in data attribute.
FILE_BadClus	= 8;	// Contains all bad clusters in the non-resident data attribute.
FILE_Secure		= 9;	// Shared security descriptors in data attribute and two indexes
						// into the descriptors. Appeared in Windows 2000.
FILE_UpCase		= 10;	// Uppercase equivalents of all 65536 Unicode characters in data attr
FILE_Extend		= 11;	// Directory containing other system files (eg. $ObjId,
						// $Quota, $Reparse and $UsnJrnl). This is new to NTFS3.0.
FILE_reserved12 = 12;	// Reserved for future use (records 12-15).
FILE_reserved13 = 13;
FILE_reserved14 = 14;
FILE_reserved15 = 15;
FILE_first_user = 16;	// First user file, used as test limit


// Constantes pour les AttributeType stadards (A mettre dans implementation ?)
NTFS_AttribID_Standard_Information            = $10;
NTFS_AttribID_Attribute_List                  = $20;
NTFS_AttribID_File_Name                       = $30;
NTFS_AttribID_Object_ID                       = $40;	// Différent pour NT
NTFS_AttribID_Security_Descriptor             = $50;
NTFS_AttribID_Volume_Name                     = $60;
NTFS_AttribID_Volume_Information              = $70;
NTFS_AttribID_Data                            = $80;
NTFS_AttribID_Index_Root                      = $90;
NTFS_AttribID_Index_Allocation                = $A0;
NTFS_AttribID_Bitmap                          = $B0;
NTFS_AttribID_Reparse_Point                   = $C0;	// Différent pour NT
NTFS_AttribID_Extended_Attribute_Information  = $D0;
NTFS_AttribID_Extended_Attribute              = $E0;
NTFS_AttribID_Property_Set                    = $F0;	// NT seulement
NTFS_AttribID_Logged_Utily_Stream             = $100;
NTFS_AttribID_End_Maker				          = $ffffffff;	// Fin de la liste attributs


// Chaines 
//atrribute id must be divided with 10 so the name array will match!
AttribNames: array [$01..$10] of string =
(
    'Standard Information',
    'Attribute List',
    'File Name',
    'Object ID',
    'Security Descriptor',
    'Volume Name',
    'Volume Information',
    'Data',
    'Index Root',
    'Index Allocation',
    'Bitmap',
    'Reparse Point',
    'Extended Attribute Information',
    'Extended Attribute',
    'Property Set',
    'Logged Utily Stream'
);


type


//
// <File Record> ::= <File Record Header>{<Attribut>}<AttributeEndMarker>
// <Attribut> ::= <Attribut Res>|<Attribut NonRes>
// <Attribut Res> ::= <Attribut Header><Attribut Data>
// <Attribut NonRes> ::= <Attribut Header><DataRun>
//

//
TNTFSMFTRecFlags = set of (fInUse, fDir, fAlign=15);

// File Record Header
// http://linux-ntfs.sourceforge.net/ntfs/concepts/file_record.html
//
TNTFSFileRecordHeader = packed record
    FileID:				array[0..3] of Char;  	// 'FILE' pour les fichiers
    UpdateSeqOffset:	Word;		// Offset to the update sequence
    UpdateSeqSize:		Word;		// Size in words of Update Sequence Number & Array (S)
    LogFileSeqNum:		Int64;		// $LogFile Sequence Number (LSN)
    SeqNum:				Word;		// Number of times this mft record has been reused.
    ReferenceCount:		Word;       // Number of hard links, i.e. the number of directory entries referencing this record
    OffsetToFirstAttr:	Word;		// Offset to the first Attribute
    Flags:				TNTFSMFTRecFlags;
    RealFileRecSize:	Cardinal;	// Real size of the FILE record
    AllocatedFileRecSize:Cardinal;
    RefToBaseFileRec:	Int64;		// File reference to the base FILE record (48/16)
    NextAttrID:			Word;       // The Attribute Id that will be assigned to
    								// the next Attribute added to this MFT Record
    // ------------------ Champs XP seulement ? ------------------------
    Align:				Word;		// Align to 4 byte boundary (XP)
    NumberOfRec:		Cardinal;	// Number of this MFT Record (XP)
    // The offset of the two fields below depends on your operating system.
    // UpdateSeqNum:		Word;
    // Début de	Update Sequence Array Taille 2S - 2
    // ------------
    // Debut des attributs (offset OffsetToFirstAttr)
end;


// --------------------------------------------------------------------------
//                Structures pour les Attribute Header
// --------------------------------------------------------------------------

// Un attribut est composé d'un Attribute Header et d'un Attribute Data

//
// Un attribut header est composé d'une partie commune et d'une partie specifique
// selon que l'attribut est résident ou non
// http://linux-ntfs.sourceforge.net/ntfs/concepts/attribute_header.html
// voir layout.h

//
// Flags pour l'Attribute Header
//
TNTFSAttrFlags = set of (fCompressed,fEncrypted = 14,fSparse = 15);


// Structure commune pour les en-tête d'attribut
TCommonAttributeHeader = packed record
    AttributeType:	Cardinal;	// $ffffffff si  fin de les attributs
    Size:			Cardinal;	// headers + attributes #ATTN
    // http://www.codeproject.com/file/NTFSUndelete.asp?select=1019067&df=100&forumid=143768#xx1019067xx
    // Remplacer Size: Cardinal par wFullLength: Word; wReserved: Word; ????
    NonResident:	boolean;	// vrai si l'attribut n'est pas résident
    NameLength:		Byte;		// Longueur nom de l'attribut, 0 => pas de nom
    NameOffset:		Word;		// Offset sur le nom de l'attribut (Unicode)
    Flags:			TNTFSAttrFlags;
    AttributeID:	Word;		// Each attribute has a unique identifier
    //the data stream continues with resident or non-resident record...
end;


//
// Suite de l'Atttribute Header pour les attributs résidents
//
TExtraResidentAttrHeader = packed record
    AttributeLength:Cardinal;	// Length of the Attribute
    AttributeOffset:Word;		// Offset sur le début des données attributs
    IndexedFlag: 	Byte;		// ???
    Padding: 		Byte;
    // Si Attribut nommé, le nom commence ici avant l'attribut lui-même
    // L'attribut commence ici (AttributeOffset), taille=AttributeLength
end;


//
// Suite de l'Atttribute Header pour les attributs non résidents
//
TExtraNonResidentAttrHeader = packed record 
 	StartingVCN:	int64;
    LastVCN:		int64;
    DataRunsOffset:	Word;		// Rounded up to a multiple of 4 bytes
    CompressionSize:Word;		// Compression Unit Size
    Padding:		Cardinal;	// 0
    AllocatedSize: 	Int64;		// This is the attribute size rounded up to the cluster size
    RealSize:		Int64;		// Real size of the attribute
    StreamInitializedSize: Int64;
    //If attribute is named, unicode name comes here before run lists
end;

// NOTE: Si l'attribut n'est pas résident, il n'est pas stocké après le header,
// mais sur disque, dans les cluster indiqué par le datarun stocké après le header 
//

// -------------- Structures pour les Attribute Data -----------------


{-------------------------------------------------------------------------
 *
 * $STANDARD_INFORMATION (0x10)
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/standard_information.html
 *
 * NOTE: Always resident.
 *
}


// Utilisé aussi dans $FILE_NAME (0x30)
TNTFSPermissions = set of
(
    aReadOnly, aHidden, aSystem, aNull1, aNull2, aArchive, aDevice, aNormal, aTemporary,
    aSparseFile, aReparsePoint, aCompressed, aOffline, aNotIndexed, aEncrypted,
    // specifiques $FILE_NAME (0x30)
    aDirectory=28, aIndexView=29,
    aNullEnd=31		// pour force le set sur 32 bits
);


TNTFSStandardInfoAttribute = packed record
    TimeCreated:		Int64;		// C Time - File Creation
    TimeModified:		Int64;		// A Time - File Altered
    TimeModifiedFileRec:Int64;		// M Time - MFT Changed
    TimeAccessedFileRec:Int64;		// R Time - File Read
    DOSPermissions: TNTFSPermissions;
    // -------- Champs spécifiques 2000/XP+ (NTFS 3.x) ---------- 
    MaximumVersions:	Cardinal;	// Max allowed versions for file. 0 means no versioning
    VersionNumber:		Cardinal;	// This file's version (if any)
    ClassId:			Cardinal;
    OwnerId:			Cardinal;
    SecurityId:			Cardinal;
    QuotaCharged:		int64;
    UpdateSequenceNum:	int64;
end;


{-------------------------------------------------------------------------
 * Attribute: $ATTRIBUTE_LIST (0x20)
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/attribute_list.html
 *
 * - Can be either resident or non-resident.
 * - Value consists of a sequence of variable length, 8-byte aligned,
 * ATTR_LIST_ENTRY records.
 * - The list is not terminated by anything at all! The only way to know when
 * the end is reached is to keep track of the current offset and compare it to
 * the attribute value size.
 * - The attribute list attribute contains one entry for each attribute of
 * the file in which the list is located, except for the list attribute
 * itself. The list is sorted: first by attribute type, second by attribute
 * name (if present), third by instance number. The extents of one
 * non-resident attribute (if present) immediately follow after the initial
 * extent. They are ordered by lowest_vcn and have their instace set to zero.
 * It is not allowed to have two attributes with all sorting keys equal.
 * - Further restrictions:
 *	- If not resident, the vcn to lcn mapping array has to fit inside the
 *	  base mft record.
 *	- The attribute list attribute value has a maximum size of 256kb. This
 *	  is imposed by the Windows cache manager.
 * - Attribute lists are only used when the attributes of mft record do not
 * fit inside the mft record despite all attributes (that can be made
 * non-resident) having been made non-resident. This can happen e.g. when:
 *	- File has a large number of hard links (lots of file name
 *	  attributes present).
 *	- The mapping pairs array of some non-resident attribute becomes so
 *	  large due to fragmentation that it overflows the mft record.
 *	- The security descriptor is very complex (not applicable to
 *	  NTFS 3.0 volumes).
 *	- There are many named streams.
 *}


TNTFSAttributeListRecord = packed record
    AttributeType:	Cardinal;	// Type of referenced attribute.
    Size: 			Word;		// Record length
    NameLength: 	Byte;
    NameOffset: 	Byte;
    StartVCN: 		Int64;		// Lowest virtual cluster number of this portion of the attribute value
    FileRef: 		Int64;		// Base File Reference of the attribute
    AttributeID:	Word;		//* If StartVCN=0, the instance of the attribute being referenced; otherwise 0.
    //Name (Size: NameLength * 2)
end;


{-------------------------------------------------------------------------
 * Attribute: $FILE_NAME (0x30)
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/file_name.html
 *
 * NOTE: Always resident.
 *
}


TNTFSNameSpace = (npPosix,npWin32,npDos,npDosAndWin32);


TNTFSFileNameAttribute = packed record
    RefToParentDir:	Int64;		// File Reference to the base record of the parent dir (48/16)
    TimeCreated:	Int64;		// FILETIME format
    TimeAltered:	Int64;		// FILETIME format
    TimeMTFAltr:	Int64;		// FILETIME format
    TimeLastRead:	Int64;		// FILETIME format
    AllocatedSize:	Int64;      // Byte size of allocated space for the data attribute (Cluster rounded)
    RealSize:		Int64;		// Byte size of actual data in unnamed data attribute.
    Flags:			TNTFSPermissions;
    ForEAsReparse:	Cardinal;
    FileNameLength: Byte;		// Filename length in characters
    FileNamespace:	TNTFSNameSpace;
    //filename is here (size=FileNameLength*2) Unicode, Non null terminated
end;


{-------------------------------------------------------------------------
 *
 * $OBJECT_ID (0x40)
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/object_id.html
 *
 * NOTE: Always resident.
 * NOTE: NTFS 3.0+
 *
}


TNTFSObjectIDAttr = packed record
    GUIDObjectId:		TGUID; 	// Unique Id assigned to file
    // --- Optionnels ----
    GUIDBirthVolumeId:	TGUID; 	// Volume where file was created
    GUIDBirthObjectId:	TGUID; 	// Original Object Id of file
    GUIDDomainId: 		TGUID;	// Domain in which object was created
end;


{*
 * Attribute: Security descriptor (0x50). A standard self-relative security
 * descriptor.
 *
 * NOTE: Can be resident or non-resident.
 * NOTE: Not used in NTFS 3.0+, as security descriptors are stored centrally
 * in FILE_Secure and the correct descriptor is found using the security_id
 * from the standard information attribute.
 *}

// ------------------------------------------------------------------------- //
// $VOLUME_NAME (0x60)
// http://linux-ntfs.sourceforge.net/ntfs/attributes/volume_name.html
//
// NOTE: Always resident.
// NOTE: Present only in FILE_Volume ($Volume)

//
// Taille du nom dans TExtraResidentAttrHeader.AttributeLength (* 2 ?)
// Unicode
//


{-------------------------------------------------------------------------
 *
 * $VOLUME_INFORMATION (0x70)
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/volume_information.html
 *
 * NOTE: Always resident.
 * NOTE: Present only in FILE_Volume.
 * NOTE: Windows 2000 uses NTFS 3.0 while Windows NT4 service pack 6a uses
 *		 NTFS 1.2. I haven't personally seen other values yet.
}

TVolumeInfoFlags = set of
(
	viDirty, viResizeLog, viUpgrade, viNT4Mounted, viDeleteUSN, viRepairObj,
    viModifiedByChkdsk=15
);
// VolumeInfoFlags
//0x0001 Dirty,  0x0002 Resize LogFile, 0x0004 Upgrade on Mount, 0x0008 Mounted on NT4,
//0x0010 Delete USN underway, 0x0020 Repair Object Ids, 0x8000 Modified by chkdsk


TVolumeInformation = packed record
	Reserved0:		int64;		// Always 0 ?
    MajorVersion:	Byte;
    MinorVersion:	Byte;
    VolumeInfoFlags:TVolumeInfoFlags;		//#Todo2 Mettre un Set
	Reserved1:		Cardinal;	// Always 0 ?
end;


{-------------------------------------------------------------------------
 *
 * $DATA (0x80)
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/data.html
 *
 * NOTE: Si unnamed et non resident, contient le Data run du fichier
 * NOTE: Si unnamed et resident, contient les données du fichier
 *
}



// ------------------------------------------------------------------------- //
// $INDEX_ROOT (0x90)
// http://linux-ntfs.sourceforge.net/ntfs/attributes/index_root.html
{
 *
 * NOTE: Always resident.
 * ATTN: Impossible de savoir si un fichier/rep est supprimé à partir des infos
 * de l'index record => obligation de lire le FileRecord associé ou Bitmap
 * associé à $MFT
 }

//
{
$INDEX_ROOT
    Standard Attribute Header
    Index Root
    Index Header
    Index Entry
    Index Entry
}

// Seulement présent si directory résident ?
TNTFSIndexRoot = packed record
    AttributeType: 		Cardinal;  	// Type of the indexed attribute: $FILENAME ou 0 for view indexes
    CollationRule:		Cardinal;  	// Si AttrType = $FILENAME => COLLATION_FILENAME
    SizeOfIndexAllocEntry: Cardinal;	// Byte size of each index block (in the index allocation attribute)
    ClustPerIndexRec: 	Byte;
    Align:				array [1..3] of Byte;
    // Index Header A Séparer ??
end;


// Seulement présent si directory résident ?
TNTFSIndexHeader = packed record
    OffsetOfFirstEntry:	Cardinal;		// Offset to first Index Entry
    SizeOfIndexEntries:	Cardinal;		// Total size of the Index Entries
    AllocatedSizeForEntries: Cardinal;
    Flags:			Byte;
    Align1:			array [1..3] of Byte;	// (Align to 8 bytes)
    // L'index Entry commence ici (TNTFSIndexEntry)
end;

// <IndexRootAttr> ::= <IndexRoot><IndexHeader>{<IndexEntry><FileNameAttr>}<IndexEntry>

// ------------------------------------------------------------------------- //
{*
 * Attribute: Index allocation (0xa0).
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/index_allocation.html
 *
 * NOTE: Always non-resident (doesn't make sense to be resident anyway!).
 *
 * This is an array of index blocks. Each index block starts with an
 * INDEX_BLOCK structure containing an index header, followed by a sequence of
 * index entries (INDEX_ENTRY structures), as described by the INDEX_HEADER.
 *
 *}

{
0x01  	Index entry points to a sub-node ieHasSubNode
0x02 	Last index entry in the node ieLastEntry

TIndexEntryFlags = set of ( ieHasSubNode, ieLastEntry, iePadding=15);
}


TNTFSIndexEntry = packed record
    // The next field is only valid when the last entry flag is not set
    FileReference:	int64;	// The mft reference of the file described by this index
                           	// entry. Used for directory indexes.
                           	// 2 interpretation selon flag
	Length:			Word;	// Byte size of this index entry, multiple of 8-bytes.
	KeyLength:		Word;	// Byte size of the key value, which is in the index entry.
    						// It follows field reserved. Not multiple of 8-bytes.
                            // Stream contient TNTFSFileNameAttribute
	IndexEntryFlags:Word;	// #TODO2 Mettre un set
    Reserved:		Word;
    // ---- Le stream commence ici si pas dernier noeud
    // Le sub node s'il existe est stocké dans les 8 derniers octets (offset = Length - 8)
end;



// --------------------------------------------------------------------------
//                Structures pour les Index Record
// --------------------------------------------------------------------------


//
// En tête de l'Index Record
// http://linux-ntfs.sourceforge.net/ntfs/concepts/index_header.html
//
TNTFSIndexRecordHeader = packed record
    IndxID: 			array [0..3] of Char;	// Contient 'INDX'
    UpdateSeqOffset: 	Word;
    UpdateSeqSize:		Word;			// Size in words of the Update Sequence Number & Array
    LogFileSeqNum: 		Int64;
    VCN: 				Int64;			// VCN of this INDX buffer in the Index Allocation
    BytesToIndexEntries:Cardinal;		// Offset to the Index Entries from here
    SizeOfIndexEntries: Cardinal;
    AllocatedSizeForEntries: Cardinal;
    NotLeafNode: 		boolean;		// 1 if not leaf node
    Align: 				array [1..3] of Byte;
    UpdateSeq: 			Word;
    // Update sequence array commence ici
end;

// <IndexRecord> ::= <IndexRecordHeader>{<IndexEntry><FileNameAttr>}<IndexEntry>

 
{*
 * Attribute: Bitmap (0xb0).
 * http://linux-ntfs.sourceforge.net/ntfs/attributes/bitmap.html
 *
 * Contains an array of bits (aka a bitfield).
 * NOTE: Taille dans l'attribut header 
 }

 
// --------------------------------------------------------------------------
//                			Classe pour les DataRuns
// --------------------------------------------------------------------------

{
Data Runs
http://linux-ntfs.sourceforge.net/ntfs/concepts/data_runs.html

Data Runs = Data Run* #00
Data Run = <header><nombre de clusters><offset cluster>
1er octet <header> :
	4bit poids faibles = taille en octets pour stocker de le nombre de clusters
	4bit poids forts   = taille en octets pour stocker l'offset cluster (Si premier
    					 data run => num cluster) (dernier clust connu)
                         si 0 => sparse file (pas de cluster associés)

Attention à la gestion des nombres négatifs
Pour fichier compressés => voir en détail
}


//
// Classe pour les dataruns
//
TDataRuns = class
    private
    protected
		FRunList:	TList;		// 2 entrées de la TList pour un run (len et VCN)
		procedure readDataRuns(RunStream: TMemoryStream);
        function GetRunCount(): Cardinal;
        function GetRunLCN(ind: Cardinal): Cardinal;
        function GetRunLength(ind: Cardinal): Cardinal;
        function GetTotalClusterCount(): Cardinal;
    public
        // ============================= Methodes =============================
        constructor Create(RunStream: TMemoryStream);
        destructor Destroy; override;
        function GetLCN(vcn: Cardinal): Cardinal;
        // ============================= Propriétés =============================
        property RunCount: Cardinal read GetRunCount;
        property RunLCN[ind: Cardinal]: Cardinal read GetRunLCN;
        property RunClusterCount[ind: Cardinal]: Cardinal read GetRunLength;
        property TotalClusterCount: Cardinal read GetTotalClusterCount;
end;


// Declarations de fonctions utilitaires
function NTFSPermissionsToString(const flag: TNTFSPermissions): string;
function GetVolumeFlagsString(const flags: TVolumeInfoFlags): string;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


// =====================================================================
//	   						Gestion des DataRuns
// =====================================================================


//
// Construit le DataRun à partir du stream
//
constructor TDataRuns.Create(RunStream: TMemoryStream);
begin
	Self.FRunList := TList.Create;

    Self.readDataRuns(RunStream);
end;


//
// Destruction
//
destructor TDataRuns.Destroy;
begin
	Self.FRunList.Free;
end;

 
//
// Analyse le data run
//
procedure TDataRuns.readDataRuns(RunStream: TMemoryStream);
var
    Header, LengthSize, OffsetSize: Byte;
    LengthValue, OffsetValue: Integer;
    CurrentLCN: integer;

    //
    // Renvoie un entier NON SIGNE stocké sur size octets
    //
    function GetDRClusterCount(const size: integer): Cardinal;
    var
        tmpval: Byte;
        i : integer;
    begin
        Result := 0;

        for i := 0 to size - 1 do
        begin
            RunStream.ReadBuffer(tmpval, 1);
            Result := Result + (Cardinal(tmpval) shl (8 * i));
        end;
    end;

    
    //
    // Récupère l'offset depus le flux (dernier byte >= $80) indique un
    // offset negatif
    // Renvoie 0 si size = 0 (sparse)
    function GetLCNOffset(const size: integer): integer;
    var
        negoffset: boolean;
        tmpval: Byte;
        i : integer;
    begin
        Result := 0;
        negoffset := false;

        for i := 0 to size - 1 do
        begin
            RunStream.ReadBuffer(tmpval, 1);

            // Dernier byte de l'offset et tmpval >= $80 ? changer le signe pour avoir la bonne valeur
            if (i = size - 1) and (tmpval >= $80) then
                negoffset := true;

            Result := Result + (Cardinal(tmpval) shl (8 * i));
        end;

        // Si dernier byte de l'offset >= $80, c'est un offset negatif
        if negoffset then
            case size of
              1: Result := ($ffffff00 or Result);
              2: Result := ($ffff0000 or Result);
              3: Result := ($ff000000 or Result);
            end;
    end;


begin
	CurrentLCN := 0;

	RunStream.ReadBuffer(Header, 1);
    While Header <> $00 do
    begin
		LengthSize := Header and $0F;	// Nb oct sur lesquels est code le nom de cluster
		OffsetSize := Header shr 4;		// Nb oct sur lesquels est code l'offset / run precedent

        // Lire le nombre de cluster dans le run
        LengthValue := GetDRClusterCount(LengthSize);

        // Lire l'offset (peut être negatif)
        OffsetValue := GetLCNOffset(OffsetSize);

        // Ajouter le Run (entrée paire = Length, entrée impaire = LCN)
        Self.FRunList.Add(Pointer(LengthValue));

        // Sparse File ?
        if OffsetValue = 0 then
	        Self.FRunList.Add(Pointer(0))
        else
        begin
	        Inc(CurrentLCN, OffsetValue);
	        Self.FRunList.Add(Pointer(CurrentLCN));
        end;

		// Lit l'en tête sur run suivant
        RunStream.ReadBuffer(Header, 1);
    end;
end;


//
// Renvoie le cluster physique correspondant au nieme cluster du fichier (VCN)
// 0 veut dire erreur 
function TDataRuns.GetLCN(vcn: Cardinal): Cardinal;
var
	i: integer;
begin
	Result := 0;	// 0 veut dire erreur: pointe ds sparse, ....)

	for i := 0 to Self.RunCount - 1 do
    begin
		// VCN dans ce run ?
        if vcn < Self.RunClusterCount[i] then
        begin
        	if Self.RunLCN[i] <> 0 then
	        	Result := Self.RunLCN[i] + vcn;

            exit;
        end;

        // Retirer le nombre de cluster du ren courant
        Dec(vcn, Self.RunClusterCount[i]); 
    end;
end;


// --------------------------------------------------------------------------
//							Gestions des propriétés
// --------------------------------------------------------------------------


function TDataRuns.GetRunLength(ind: Cardinal): Cardinal;
begin
	// Un run est stocké sur 2 entrées TList (length + LCN)
    Result := Cardinal(Self.FRunList[ind * 2]);
end;


function TDataRuns.GetRunLCN(ind: Cardinal): Cardinal;
begin
	// Un numéro du cluster en stocké dans l'entrée impaire
    Result := Cardinal(Self.FRunList[ind * 2 + 1]);
end;


function TDataRuns.GetRunCount(): Cardinal;
begin
	// Un run est stocké sur 2 entrées TList (length + LCN)
    Result := Self.FRunList.Count div 2;
end;


//
// Renvoie le nombre total de cluster dans le datarun (sparse clusters inclus)
//
function TDataRuns.GetTotalClusterCount(): Cardinal;
var
	i: integer;
begin
	Result := 0;
    for i := 0 to Self.RunCount - 1 do
    	Inc(Result, Self.RunClusterCount[i]);
end;


// =====================================================================
//	   						Fonctions utilitaires
// =====================================================================


//
// Renvoie une chaine correspondant aux flags du VolumeInfo Attr
//
function GetVolumeFlagsString(const flags: TVolumeInfoFlags): string;
begin
	Result := '';
	if viDirty in flags then  
		Result :=  Result + 'Dirty,';

	if viResizeLog in flags then
		Result :=  Result + 'Resize LogFile,';

	if viUpgrade in flags then
		Result :=  Result + 'Upgrade on Mount,';

	if viNT4Mounted in flags then
		Result :=  Result + 'Mounted on NT4,';

	if viDeleteUSN in flags then  
		Result :=  Result + 'Delete USN underway,';

	if viRepairObj in flags then
		Result :=  Result + 'Repair Object Ids,';

	if viModifiedByChkdsk in flags then
		Result :=  Result + 'Modified by ChkDsk,';

    if Result = '' then
    	Result := 'None'
    else
    	Result := LeftStr(Result, Length(Result) - 1);	// supprimer le ,
end;


//
// Convertit les permissions (Attr StdInfo et FileName) en chaine
//
function NTFSPermissionsToString(const flag: TNTFSPermissions): string;
begin
    Result := '';
	// Ne pas traduire les attributs DOS classiques
    {if aReadOnly in flag then Result := Result + 'Read only,';
    if aHidden in flag then Result := Result + 'Hidden,';
    if aSystem in flag then Result := Result + 'System,';
    if aArchive in flag then Result := Result + 'Archive,';}

    if aDevice in flag then Result := Result + 'Device,';
    if aNormal in flag then Result := Result + 'Normal,';	// ???
    if aTemporary in flag then Result := Result + 'Temporary,';
    if aSparseFile in flag then Result := Result + 'Sparse file,';
    if aReparsePoint in flag then Result := Result + 'Reparse point,';
    if aCompressed in flag then Result := Result + 'Compressed,';
    if aOffline in flag then Result := Result + 'Offline,';
    if aNotIndexed in flag then Result := Result + 'Not Indexed,';
    if aEncrypted in flag then Result := Result + 'Encrypted,';
    // Flags présents seulement dans $FILENAME Attr 
    if aDirectory in flag then Result := Result + 'Directory,';
    if aIndexView in flag then Result := Result + 'Index View,';

    if Result <> '' then
    	Result := LeftStr(Result, Length(Result) - 1)	// supprimer le ,
    else
    	Result := '(none)';
end;


// --------------------------- Fin de l'unité -----------------------------
end.

