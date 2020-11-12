{-----------------------------------------------------------------------------
 Unit Name: FAT16Partition
 Author:    Chris
 Purpose:   Classe pour gérer les partitions Fat16
 History:   21/04/2005
-----------------------------------------------------------------------------}


unit FAT16Partition;


{

 A FAT file system volume is composed of four basic regions, which are laid out in this order on the volume:
	0 - Reserved Region
	1 - FAT Region
	2 - Root Directory Region (doesn't exist on FAT32 volumes)
	3 - File and Directory Data Region


FAT16
     _______________________________
    |								| Start at sector 0 of partition
    |        Reserved Region		|
    |                           	|
    |-------------------------------|
    |                           	| Start at sector BPB_RsvdSecCnt
    |           Fat Region			|
    |                           	|
    |-------------------------------|
    |                           	| Start at sector BPB_ResvdSecCnt + (BPB_NumFATs * FATSz)
    |      Directory Root			|
    |                           	|
    |-------------------------------|
    |                           	| Start at BPB_ResvdSecCnt + (BPB_NumFATs * FATSz)
    | File & Directory Data Region	| 		   + RootDir
    |                           	| (equivalent au cluster 2)
    |                           	|

}


interface

uses
	Classes,
    //
    GenericDrive, GenericPartition, GenericFAT;


type

//
// Classe spécialisée dans la gestion des partitions FAT 32
//
TFAT16Partition = class(TFATPartition)
    private
        fpActiveFAT:		Word;	   	// Numero de la FAT active si pas de mirroring
        //FFreeCount		: Cardinal;	// Nombre de clusters libres sur la partition, $FFFFFFFF si inconnu
		fpMaxiRootDirEntries: Cardinal;	// Nombre d'entrées dans le rep racine
        FRootDirSectors:	Cardinal;	// Nomnre de secteur pour le rep racince
    protected
        function GetFileSystemName : String; override;
        // ------------------
        function GetFirstSectorOfCluster(const n : Cardinal) : Cardinal; override;	// Mettre en public ???
        // ----------------- Gestion de entrées dans la FAT ------------------
        function GetFATEntry(const indice : Cardinal) : Cardinal; override;
        function isFreeCluster(const Cluster : Cardinal) : boolean; override;
        function isLastCluster(const Cluster : Cardinal) : boolean; override;
        function isBadCluster(const Cluster : Cardinal) : boolean; override;
        procedure LoadDirectoryStream(const dircluster: Cardinal); override;
    public
        // ============================= Methodes =============================
        constructor Create(disque: TGenericDrive; const firstsect: Cardinal);
        destructor Destroy; override;
        // --------------------------
        function ScanRootDirectory(): boolean; override; // Scan depuis la racine
		class function isValidBootSector(const buffer: Pointer): boolean; override;
        class function GetDefaultClusterSize(const dsize: int64): Cardinal; override;
		function LoadFromPartInfo(info: PPartitionInfo): boolean; override;
		function LoadFromLBA(const lba:Cardinal): boolean; override;

        function ComputePartInfo(const Size, ResvSect, NbFAT: Cardinal): boolean;
        function GetInfoFromBootSector(const buf: Pointer): boolean; override;	// protected
        //
        procedure GetPartitionProperties(info: TStrings); override;
        // ============================= Propriétés =============================
end;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


uses
	SysUtils,
    ConversionUnit;

type

TFAT16BootSector = packed record
	// ------------- Partie commune FAT16 et FAT32 ----------------------
    BS_jmpBoot      : array [0..2] of Byte;		// commence par 0xEB ou 0xE9
    BS_OEMName      : array [0..7] of Char;		// Generalement "MSWIN4.1"
    BPB_BytsPerSec  : Word;			// Count of bytes per sector
    BPB_SecPerClus	: byte;         // Nombre de secteurs par Cluster
    BPB_RsvdSecCnt	: Word;			// 1 for FAT16: Count of reserved sectors in the Reserved region
    BPB_NumFATs		: byte;			// Utilisé pour FAT32 ? cf BPB_ExtFlags
    BPB_RootEntCnt	: Word;         // 0 pour FAT32
    BPB_TotSec16	: Word;         // 0 pour FAT32
    BPB_Media		: byte;			// 0xF8 is the standard value for "fixed" (non-removable)
    BPB_FATSz16		: Word;			// 0 Pour FAT32
    BPB_SecPerTrk	: Word;			// Pour CHS addressing
    BPB_NumHeads	: Word;			// Pour CHS addressing
    BPB_HiddSec		: Cardinal;     // System specific
    BPB_TotSec32	: Cardinal;		// Total count of sectors on the FAT32 volume
	// -------------------------- FAT32 Part -----------------------------
	BS_DrvNum		: Byte;
	BS_Reserved1	: Byte;			// Always 0
	BS_BootSig	   	: Byte;			// contient $29
	BS_VolID		: Cardinal;		// Volume serial number
	BS_VolLab		: array[0..10] of Char;	// Nom du Volume
	BS_FilSysType	: array[0..7]  of Char;	// Contient souvent 'FAT16   '

{	BPB_FATSz32		: Cardinal;		// nb secteurs occupés par 1 FAT
	BPB_ExtFlags	: Word;         // Flags pour le nombre de FAT
	BPB_FSVer		: Word;			// Hi et Lo version
	BPB_RootClus	: Cardinal;		// cluster # of the 1st cluster of the root directory, usually 2
	BPB_FSInfo		: Word;			// Secteur contenant la struct FSInfo
	BPB_BkBootSec	: Word;         // Secteur contenant la copie de ce Boot sector = 6
	BPB_Reserved	: array[0..11] of Byte;
    BS_Code			: array [0..419] of Byte;
    BS_BootRecordSig: Word;					// Contient $AA55}
end;
PFAT16BootSector = ^TFAT16BootSector;

//
// Crée la partition16 avec le drive et le numéro de secteur où commence la partition
//
constructor TFAT16Partition.Create(disque: TGenericDrive; const firstsect: Cardinal);
begin
	inherited Create(disque, firstsect);

    Self.fpFATEntrySize := 2;
end;


//
//
//
destructor TFAT16Partition.Destroy;
begin
	inherited Destroy;
end;


// ----------------------------------------------------------------------
//						Gestion du BootBlock
// ----------------------------------------------------------------------


class function TFAT16Partition.isValidBootSector(const buffer: Pointer): boolean;
var
	bs: PFAT16BootSector;
begin
	Result := false;

    if not (inherited isValidBootSector(buffer)) then
    	exit;

    bs := PFAT16BootSector(buffer);
     
    //#Todo Rajouter test FAT16
    if bs.BPB_RsvdSecCnt <> 1 then
    	exit;

	if bs.BS_Reserved1 <> 0 then
    	exit;

	if (bs.BPB_TotSec16 = 0) and (bs.BPB_TotSec32 = 0) then
    	exit;

	if (bs.BPB_TotSec16 <> 0) and (bs.BPB_TotSec32 <> 0) then
    	exit;

    if bs.BS_BootSig <> $29 then
    	exit; 

  	Result := true;
end;


//
// Récupère les infos en se basant sur TPartitionInfo 
//
function TFAT16Partition.LoadFromPartInfo(info: PPartitionInfo): boolean;
begin
	inherited LoadFromPartInfo(info);

    // Comment lire si pas de taille secteur / cluster !!!!!
    
    // Lire le Boot Sector, et le backup sector en cas de problème
    Result := Self.ReadSector(0);
    if Result then
    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);
end;



//
// On ne connait que l'adresse de départ pour récupérer la partition
//
function TFAT16Partition.LoadFromLBA(const lba:Cardinal): boolean;
var
	boot: TFileStream;	//#DEBUG
begin
	Self.fpFirstSector := lba;

    Result := false;
    try
		boot := TFileStream.Create('fat16bin.TXT', fmOpenRead);
        boot.ReadBuffer(Self.fpSectorBuffer[0], Self.SectorSize);
    except
    	exit;
    end;

	boot.Free;
    Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);

{    //#Todo2 Mettre cette partie de code dans une fonction
    // Lire le Boot Sector
    Result := Self.ReadSector(0);
    if Result then
    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);}
end;


function TFAT16Partition.GetInfoFromBootSector(const buf : Pointer): boolean;
var
	bootsector: PFAT16BootSector;
begin
    // Verifier si c'est un BootSecot valide
	Result := false;
    if not TFAT16Partition.isValidBootSector(buf) then
    	exit;

	// BB Valide
    Result := true;

    bootsector := PFAT16BootSector(buf);

    // Champs communs FAT16/32
    Self.FOEMName		:= bootsector.BS_OEMName;
    Self.fpSectorSize	:= bootsector.BPB_BytsPerSec;
    Self.fpClusterSize	:= bootsector.BPB_SecPerClus * bootsector.BPB_BytsPerSec;
    Self.fpRsvdSecCnt	:= bootsector.BPB_RsvdSecCnt;
    Self.fpFATCount		:= bootsector.BPB_NumFATs;
    Self.fpMedia		:= bootsector.BPB_Media; 
	Self.FHiddenSectors := bootsector.BPB_HiddSec;

    // Champs spécifiques FAT16
    if bootsector.BPB_TotSec16 <> 0 then
    	Self.fpTotalSectors := Cardinal(bootsector.BPB_TotSec16)
    else
    	Self.fpTotalSectors := bootsector.BPB_TotSec32;

    Self.fpVolumeID		:= bootsector.BS_VolID;
    Self.fpVolumeLabel	:= bootsector.BS_VolLab;
    Self.FFilSysType	:= bootsector.BS_FilSysType;

    Self.fpFATSize := Cardinal(bootsector.BPB_FATSz16);
	Self.fpMaxiRootDirEntries := bootsector.BPB_RootEntCnt;

    //#TODO vérifier le calcul
    Self.FRootDirSectors := ((Self.fpMaxiRootDirEntries * 32) + (Self.SectorSize - 1)) div Self.SectorSize;
	Self.fpMaxValidClustNum := (Self.TotalSectors - (Self.ReservedSecCnt + (Self.FATCount * Self.FATSize) + Self.FRootDirSectors)) div Self.SectorPerCluster;
end;


//
// Procedure: TFAT32Partition.ComputePartInfo
// Purpose:	Calcule les informations sur la partition (Taille Fat,...)
//
// Arguments:
//	Size		Nombre de secteurs de la partition
//	ResvSect	Nombre de secteurs réservés (32 en général)
//	NbFAT		Nombre de FAT (2 en général)
// Result:    boolean
//
function TFAT16Partition.ComputePartInfo(const Size, ResvSect, NbFAT: Cardinal): boolean;
begin
end;


//    DSKSZTOSECPERCLUS DskTableFAT16 [] = {
//        {        8400,   0}, /* disks up to  4.1 MB, the 0 value for SecPerClusVal trips an error */
//        {      32680,   2},  /* disks up to   16 MB,  1k cluster */
//        {    262144,   4},   /* disks up to 128 MB,  2k cluster */
//        {   524288,    8},   /* disks up to 256 MB,  4k cluster */
//        { 1048576,  16},     /* disks up to 512 MB,  8k cluster */
//        /* The entries after this point are not used unless FAT16 is forced */
//        { 2097152,  32},     /* disks up to     1 GB, 16k cluster */
//        { 4194304,  64},     /* disks up to     2 GB, 32k cluster */
//        { 0xFFFFFFFF, 0} /* any disk greater than 2GB, 0 value for SecPerClusVal trips an error */
//    };

//
// Taille en nb secteurs ou octets ??
//
class function TFAT16Partition.GetDefaultClusterSize(const dsize: int64): Cardinal;
const
	PartSizeLimit: array [0..7] of Cardinal = (
		8400, 32680, 262144, 524288, 1048576, 2097152, 4194304,  $FFFFFFFF
	);
	DefClusterSize: array [0..7] of Cardinal = (0, 2, 4, 8, 16, 32, 64, 0);
var
	i: integer;
begin
{	Result := 0;

	// Rechercher la taille du cluster BPB_SecPerClus
    for i:= Low(PartSizeLimit) to High(PartSizeLimit) do
    	if PartSize <= PartSizeLimit[i] then
        begin
			Result := DefClusterSize[i];
            break;
        end;}
end;


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


//
// Renvoie le nom du File System
//
function TFAT16Partition.GetFileSystemName : String;
begin
	Result := 'FAT 16';
end;


//
// Procedure: TFAT16Partition.GetFirstSectorOfCluster
// Purpose: Renvoie le 1er secteur du cluster n
//
// Arguments: const n : Numéro du cluster
// Result:    Numéro du 1er secteur du cluster (relatif à la partition
//
function TFAT16Partition.GetFirstSectorOfCluster(const n : Cardinal) : Cardinal;
begin
	//Start + # of Reserved + (# of Sectors Per FAT * 2) + ((Maximum Root Directory Entries * 32) / Bytes per Sector)
	Result := (n - 2) * (Self.ClusterSize div Self.SectorSize) + Self.fpRsvdSecCnt
    		  + (Self.fpFATSize * 2) + ((Self.fpMaxiRootDirEntries * 32) div Self.SectorSize);
end;



//
// Renvoie les propriétés dans des couples (propriété, valeur)
//
procedure TFAT16Partition.GetPartitionProperties(info: TStrings);
begin
	inherited;
    
    // ------------------ FAT16 Specifique -------------------------
    info.Add('Max Root Directory Entries');
    info.Add(IntToStr(Self.fpMaxiRootDirEntries ));
end;


// --------------------------------------------------------------------------
//					Gestion du DirectoryStream
// --------------------------------------------------------------------------


//
// Scan depuis la racine
//
function TFAT16Partition.ScanRootDirectory(): boolean;
begin
	Result := Self.ScanFATDirectoryNew(0);
    //Result := Self.ScanRelativeDirectory(0);

    // Libère la mémoire
    Self.FDirectoryStream.Clear;
end;


//
// dircluster = 0 => Directory Root pour FAT16
//
procedure TFAT16Partition.LoadDirectoryStream(const dircluster: Cardinal);
var
	i: integer;
begin
    // Si root directory, lire la zone Directory Root 
	if dircluster = 0 then
    begin
    	Self.FDirectoryStream.Clear;

    	// Lire les secteurs correspondant au directory root
        Self.FDirectoryStream.SetSize(Self.FRootDirSectors * Self.SectorSize);

        // Le directory root commence apres la FAT
        for i := 0 to Self.FRootDirSectors - 1 do
        begin
            Self.ReadSector(i + Self.FATSize * Self.fpFATCount + Self.ReservedSecCnt);
            Self.FDirectoryStream.Write(Self.fpSectorBuffer[0], Self.SectorSize);
        end;
         
        // Revient au début du stream
        Self.FDirectoryStream.Seek(0, soFromBeginning);
    end
    else
    	// Sinon lire la suite de clusters associés
    	inherited LoadDirectoryStream(dircluster);
end;


// --------------------------------------------------------------------------
//					Gestion des entrées dans la FAT
// --------------------------------------------------------------------------

//
// Les numéros de cluster sont stockés sur 16bits, la fat est un tableau
// de word
//


//
// Renvoie le n° de cluster stocké dans l'entrée indice; lit un secteur du
// disque si necessaire
//
{-----------------------------------------------------------------------------
  Procedure: TFAT32Partition.GetFATEntry
  Author:    Chris
  Date:      16-mai-2004
  Arguments: const indice : Cardinal
  Result:    Cardinal ou $10000000 en cas d'erreur de lecture
-----------------------------------------------------------------------------}
function TFAT16Partition.GetFATEntry(const indice: Cardinal): Cardinal;
type
	PWord = array of Word;
var
    sectorfat, sectormirfat, sectorswap, offset: Cardinal;
begin
	// Calculer le secteur contentant l'entrée indice et le secteur mirroir
    sectorfat := Self.fpRsvdSecCnt + (indice * Self.fpFATEntrySize) div Self.fpSectorSize;
    sectormirfat := sectorfat + Self.fpFATSize;

    // Calculer l'offset dans le secteur
    offset := (indice) mod (Self.fpSectorSize div Self.fpFATEntrySize);
end;


//
// Retourne vrai si le num de cluster correspond à un cluster libre
//
function TFAT16Partition.isFreeCluster(const Cluster : Cardinal) : boolean;
begin
	result := (Cluster = 0);
end;


//
//  Retourne vrai si le num de cluster correspond au dernier cluster d'un fichier
//
function TFAT16Partition.isLastCluster(const Cluster : Cardinal) : boolean;
begin
	result := (Cluster >= $FFF8);
end;


//
//  Retourne vrai si le num de cluster correspond à un cluster défectueux
//
function TFAT16Partition.isBadCluster(const Cluster : Cardinal) : boolean;
begin
	result := (Cluster = $FFF7);
end;


// --------------------------- Fin de l'unité -----------------------------
end.
