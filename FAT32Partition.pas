{-----------------------------------------------------------------------------
 Unit Name: Partition à Renommer en FAT32 ??
 Author:    Chris
 Purpose:	Gestion des partitions FAT 32 (spécialisation de TFATPartition)
 Date:		05/05/2005
 History:
-----------------------------------------------------------------------------}

//#Todo1 Reflechir à la gestion des erreurs
// FAT32 File System Specification (Microsoft)
// http://download.microsoft.com/download/whistler/hwdev1/1.0/wxp/en-us/fatgen103.exe
//


unit FAT32Partition;
    
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

    
FAT32
     _______________________________
    |								| Start at sector 0 of partition
    |        Reserved Region		|
    |                           	|
    |-------------------------------|
    |                           	| Start at sector BPB_RsvdSecCnt
    |           Fat Region			|
    |                           	|
    |-------------------------------|
    |                           	| Start at BPB_ResvdSecCnt + (BPB_NumFATs * FATSz)
    | File & Directory Data Region	| (equivalent au cluster 2)
    |                           	|
    |                           	|


}


interface

uses
	Classes,
    // Unités propriétaires
    GenericDrive, GenericPartition, GenericFAT;


type


//
// Classe spécialisée dans la gestion des partitions FAT 32
//
TFAT32Partition = class(TFATPartition)
    private
    	fpRootDirCluster: Cardinal;	// Cluster contenant le début du Root Dir
        fpActiveFAT 	: Word;	   	// Numero de la FAT active si pas de mirroring
        fpFSInfo		: Word;	   	// Secteur contenant la struct FSInfo (generalement 1)
        fpBackupBootSec	: Word;     // Secteur contenant le backup du BootSector (O sinon 6 s'il existe)
        FFreeCount		: Cardinal;	// Nombre de clusters libres sur la partition, $FFFFFFFF si inconnu
    protected
        function GetFileSystemName : String; override;
        function GetFileSystemFamily: String; override;
        // ------------------
        function GetFirstSectorOfCluster(const n : Cardinal) : Cardinal; override;	// Mettre en public ???
        // ----------------- Gestion de entrées dans la FAT ------------------
        function GetFATEntry(const indice : Cardinal) : Cardinal; override;
        function isFreeCluster(const Cluster : Cardinal) : boolean; override;
        function isLastCluster(const Cluster : Cardinal) : boolean; override;
        function isBadCluster(const Cluster : Cardinal) : boolean; override;
    public
        // ============================= Methodes =============================
        constructor Create(disque: TGenericDrive; const firstsect: Cardinal); override;
        destructor Destroy; override;
        // ----------------------- Recupère les infos part -----------------
		class function isValidBootSector(const buffer: Pointer): boolean; override;
		function LoadFromPartInfo(info: PPartitionInfo): boolean; override;
		function LoadFromLBA(const lba:Cardinal): boolean; override;
        // --------------------------
        //#Todo2 fonction prenant la taille de la FAT en parametre
        // par ex fatsize = 0 par défaut pour indique qu'il faut la calculer
        function ComputePartInfo(const Size, ResvSect, NbFAT: Cardinal; const fatsize: cardinal = 0): boolean;
        function GetInfoFromBootSector(const buf: Pointer): boolean; override;	// protected
        //
        procedure GetPartitionProperties(info: TStrings); override;
        function ScanRootDirectory(): boolean; override; // Scan depuis la racine
        // ============================= Propriétés =============================
end;



// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


uses
	SysUtils,
    ConversionUnit;

{
FAT: General Overview of On-Disk Format at http://www.microsoft.com/whdc/system/platform/firmware/fatgen.mspx

//
// Placé au début de la partition : 1er secteur de la partition
// (partition FAT)
//

Le Boot Sector est le premier secteur de la partition une partie (36 oct) est
commune FAT16 et FAT32.
Partie commune FAT16 et FAT32
}


Type

TFAT32BootSector = packed record
	// ------------- Partie commune FAT16 et FAT32 ----------------------
    BS_jmpBoot      : array [0..2] of Byte;		// commence par 0xEB ou 0xE9
    BS_OEMName      : array [0..7] of Char;		// Generalement "MSWIN4.1"
    BPB_BytsPerSec  : Word;			// Count of bytes per sector
    BPB_SecPerClus	: byte;         // Nombre de secteurs par Cluster
    BPB_RsvdSecCnt	: Word;			// Count of reserved sectors in the Reserved region of the volume
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
	BPB_FATSz32		: Cardinal;		// nb secteurs occupés par 1 FAT
	BPB_ExtFlags	: Word;         // Flags pour le nombre de FAT
	BPB_FSVer		: Word;			// Hi et Lo version
	BPB_RootClus	: Cardinal;		// cluster # of the 1st cluster of the root directory, usually 2
	BPB_FSInfo		: Word;			// Secteur contenant la struct FSInfo
	BPB_BkBootSec	: Word;         // Secteur contenant la copie de ce Boot sector = 6
	BPB_Reserved	: array[0..11] of Byte;
	BS_DrvNum		: Byte;
	BS_Reserved1	: Byte;
	BS_BootSig	   	: Byte;			// contient $29
	BS_VolID		: Cardinal;		// Volume serial number
	BS_VolLab		: array[0..10] of Char;	// Nom du Volume
	BS_FilSysType	: array[0..7]  of Char;	// Contient souvent 'FAT32   '
    BS_Code			: array [0..419] of Byte;
    BS_BootRecordSig: Word;					// Contient $AA55
end;
PFAT32BootSector = ^TFAT32BootSector;


{

BPB_BytsPerSec	Count of bytes per sector. This value may take on only the following
				values: 512, 1024, 2048 or 4096. If maximum compatibility with old
                implementations is desired, only the value 512 should be used.
                There is a lot of FAT code in the world that is basically "hard wired"
                to 512 bytes per sector and doesn't bother to check this field to
                make sure it is 512. Microsoft operating systems will properly
                support 1024, 2048, and 4096.Note: Do not misinterpret these statements
                about maximum compatibility. If the media being recorded has a physical
                sector size N, you must use N and this must still be less than or
                equal to 4096. Maximum compatibility is achieved by only using media
                with specific sector sizes.

BPB_SecPerClus	Number of sectors per allocation unit. This value must be a power of 2
				that is greater than 0. The legal values are 1, 2, 4, 8, 16, 32, 64, and 128.
                Note however, that a value should never be used that results in
                a "bytes per cluster" value (BPB_BytsPerSec * BPB_SecPerClus) greater than 32K (32 * 1024).
                There is a misconception that values greater than this are OK.
                Values that cause a cluster size greater than 32K bytes do not work properly;
                do not try to define one. Some versions of some systems allow 64K bytes
                per cluster value. Many application setup programs will not work correctly
                on such a FAT volume.

BPB_RsvdSecCnt	Number of reserved sectors in the Reserved region of the volume
				starting at the first sector of the volume. This field must not be 0.
                For FAT12 and FAT16 volumes, this value should never be anything other than 1.
                For FAT32 volumes, this value is typically 32. There is a lot of FAT code
                in the world "hard wired" to 1 reserved sector for FAT12 and FAT16 volumes
                and that doesn't bother to check this field to make sure it is 1. Microsoft
                operating systems will properly support any non-zero value in this field.

BPB_NumFATs		The count of FAT data structures on the volume. This field should always
				contain the value 2 for any FAT volume of any type. Although any value
                greater than or equal to 1 is perfectly valid. The reason the standard
                value for this field is 2 is to provide redundancy for the FAT data
                structure. On non-disk-based media, such as FLASH memory cards,
                where such redundancy is a useless feature, a value of 1 may be used.

BPB_RootEntCnt	For FAT12 and FAT16 volumes, this field contains the count of 32-byte
				directory entries in the root directory. For FAT32 volumes, this
                field must be set to 0. For FAT12 and FAT16 volumes, this value should always specify
                a count that when multiplied by 32 results in an even multiple
                of BPB_BytsPerSec.
                For maximum compatibility, FAT16 volumes should use the value 512.

BPB_TotSec16	This field is the old 16-bit total count of sectors on the volume.
				This count includes the count of all sectors in all four regions
                of the volume. This field can be 0; if it is 0, then BPB_TotSec32
                must be non-zero.
                For FAT32 volumes, this field must be 0. For FAT12 and FAT16 volumes,
                this field contains the sector count, and BPB_TotSec32 is 0 if the total
                sector count "fits" (is less than 0x10000).
BPB_Media
BPB_FATSz16		This field is the FAT12/FAT16 16-bit count of sectors occupied by ONE FAT.
				On FAT32 volumes this field must be 0, and BPB_FATSz32 contains the FAT size count.

BPB_SecPerTrk
    BPB_NumHeads
    BPB_HiddSec
BPB_TotSec32	This field is the new 32-bit total count of sectors on the volume.
				This count includes the count of all sectors in all four regions of
                the volume. This field can be 0; if it is 0, then BPB_TotSec16 must
                be non-zero. For FAT32 volumes, this field must be non-zero.
                For FAT12/FAT16 volumes, this field contains the sector count if BPB_TotSec16
                is 0 (count is greater than or equal to 0x10000).

BPB_FATSz32 ???		For FAT32, the root directory can be of variable size and is a cluster chain, just like any other directory is. The first cluster of the root directory on a FAT32 volume is stored in BPB_RootClus. Unlike other directories, the root directory itself on any FAT type does not have any date or time stamps, does not have a file name (other than the implied file name "\"), and does not contain "." and ".." files as the first two directory entries in the directory. The only other special aspect of the root directory is that it is the only directory on the FAT volume for which it is valid to have a file that has only the ATTR_VOLUME_ID attribute bit set (see below).


BPB_ExtFlags	Bits 0-3	-- Zero-based number of active FAT. Only valid if mirroring is disabled.
				Bits 4-6	-- Reserved.
                Bit  7		-- 0 means the FAT is mirrored at runtime into all FATs.
                			-- 1 means only one FAT is active; it is the one referenced in bits 0-3.
                Bits 8-15 	-- Reserved.
$000f	// masque pour le nombre de FATs
$0080	// masque pour le mirroring
}


// Structure du secteur FSInfo
TFSInfoSector = packed record
    FSI_LeadSig		: Cardinal;		// Value 0x41615252. This lead signature is used to validate that this is in fact an FSInfo sector.
    FSI_Reserved1	: array[1..480] of byte;	// This field is currently reserved for future expansion. FAT32 format code should always initialize all bytes of this field to 0.
    FSI_StrucSig	: Cardinal;		// Value 0x61417272. Another signature that is more localized in the sector to the location of the fields that are used.
    FSI_Free_Count	: Cardinal;		// Contains the last known free cluster count on the volume. If the value is 0xFFFFFFFF, then the free count is unknown and must be computed. Any other value can be used, but is not necessarily correct. It should be range checked at least to make sure it is <= volume cluster count.
    FSI_Nxt_Free	: Cardinal;		// This is a hint for the FAT driver. It indicates the cluster number at which the driver should start looking for free clusters. Because a FAT32 FAT is large, it can be rather time consuming if there are a lot of allocated clusters at the start of the FAT and the driver starts looking for a free cluster starting at cluster 2. Typically this value is set to the last cluster number that the driver allocated. If the value is 0xFFFFFFFF, then there is no hint and the driver should start looking at cluster 2. Any other value can be used, but should be checked first to make sure it is a valid cluster number for the volume.
    FSI_Reserved2	: array[1..12] of byte;	// This field is currently reserved for future expansion. FAT32 format code should always initialize all bytes of this field to 0.
    FSI_TrailSig	: Cardinal;		// Value 0xAA550000. This trail signature is used to validate that this is in fact an FSInfo sector. Note that the high 2 bytes of this value-which go into the bytes at offsets 510 and 511-match the signature bytes used at the same offsets in sector 0.
end;


Const
	// Signature pour la structure FSInfo
	FSI_LeadSigValue	= $41615252;
    FSI_StrucSigValue	= $61417272;
    FSI_TrailSigValue	= $AA550000;

    FATCountMask		= $000f;
    MirrorFATMask		= $0080;

    FAT32EntryReadError	= $10000000;

    // Flags contenus dans FAT[0]
	ClnShutBitMask  = $08000000;	// 1 = volume clean, sinon 0
    HrdErrBitMask   = $04000000;	// 0 = Erreurs E/S rencontrées lors de la dernière utilisation 

    BACKUP_BOOT_SECTOR_OFFSET	=	6;	// Numéro de secteur du Backup BootSector
    FAT32_FATENTRY_SIZE			=	4;	// Taille en octet d'une entrée FAT32

// ========================================================================
//								FAT 32
// ========================================================================


// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Crée la partition32 avec le drive et le numéro de secteur où commence la partition
//
constructor TFAT32Partition.Create(disque: TGenericDrive; const firstsect: Cardinal);
begin
	inherited Create(disque, firstsect);
    Self.fpFATEntrySize := FAT32_FATENTRY_SIZE;

    Self.FFreeCount := MaxCardinal;		// Pas connu au démarrage
end;


//
//
//
destructor TFAT32Partition.Destroy;
begin
	inherited Destroy;
end;


// ----------------------------------------------------------------------
//						Gestion du BootBlock
// ----------------------------------------------------------------------


//
// Verifie que le boot sector correspond bien à FAT32
//
class function TFAT32Partition.isValidBootSector(const buffer: Pointer): boolean;
var
	bs: PFAT32BootSector;
begin
	Result := inherited isValidBootSector(buffer);

    // Verification des champs spécifiques FAT32
    if Result then begin
    	bs := PFAT32BootSector(buffer);

	    if bs.BPB_FATSz16 <> 0 then
    		Result := false;

        if bs.BPB_RootEntCnt <> 0 then
    		Result := false;

        if bs.BPB_TotSec16 <> 0 then
    		Result := false;

        if bs.BPB_FATSz16 <> 0 then
    		Result := false;

        if bs.BPB_TotSec32 = 0 then
    		Result := false;

	    if bs.BS_BootSig <> BootSigValue then
    		Result := false;
    end;
end;


//
// Récupère les infos en se basant sur TPartitionInfo 
//
function TFAT32Partition.LoadFromPartInfo(info: PPartitionInfo): boolean;
begin
	inherited LoadFromPartInfo(info);

    // Comment lire si pas de taille secteur / cluster !!!!!
    
    // Lire le Boot Sector, et le backup sector en cas de problème
    Result := Self.ReadSector(0);
    if Result then
    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);

    // Si Err E/S ou dans BootSector, on essaie le backup Bootsectore
    if not Result then
		if Self.ReadSector(BACKUP_BOOT_SECTOR_OFFSET) then
	    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);
end;


//
// On ne connait que l'adresse de départ pour récupérer la partition
//
function TFAT32Partition.LoadFromLBA(const lba:Cardinal): boolean;
begin
	Self.fpFirstSector := lba;

    //#Todo2 Mettre cette partie de code dans une fonction
    // Lire le Boot Sector, et le backup sector en cas de problème
    Result := Self.ReadSector(0);
    if Result then
    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);

    // Si Err E/S ou dans BootSector, on essaie le backup Bootsectore
    if not Result then
		if Self.ReadSector(BACKUP_BOOT_SECTOR_OFFSET) then
	    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);
end;


// declaration avancée
function ComputeFATSize(const PartSize, BPB_ResvdSecCnt, BPB_NumFATs: Cardinal) : Cardinal; forward;
function GetDefaultClusterSize(const Partsize: Cardinal): Cardinal; forward;


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
function TFAT32Partition.ComputePartInfo(const Size, ResvSect, NbFAT, fatsize: Cardinal): boolean;
var
	temp: Cardinal;
begin
	Self.fpTotalSectors := Size;
    Self.fpRsvdSecCnt := ResvSect;
    Self.fpFATCount := NbFAT;
    Self.fpMirrorFAT := (NbFAT > 1);

	// Calcule la taille de la FAT si elle n'est pas spécifiée
    if fatsize = 0 then
	    Self.fpFATSize := ComputeFATSize(Size, ResvSect, NbFAT);

    // Récupérer la taille du cluster en octet
    Self.fpClusterSize	:= GetDefaultClusterSize(Size) * Self.SectorSize;

    // Mettre les valeurs par défaut ??
    Self.fpRootDirCluster	:= 2;

    //The maximum valid cluster number for the volume is CountofClusters + 1
    temp := Self.TotalSectors - (self.ReservedSecCnt + (Self.FATCount * self.FATSize));
    Self.fpMaxValidClustNum := (temp div (Self.ClusterSize div Self.SectorSize)) + 1;

    // Similarly, a FAT file system driver must not allow a directory (a file that is
    // actually a container for other files) to be larger than 65,536 * 32 (2,097,152) bytes.
    // 65536 entrées max dans un rep
    Self.fpMaxClusterPerDir := 65536 * 32 div Self.ClusterSize;

    // Alloue la mem pour les buffer Cluster et Secteur
    SetLength(Self.fpSectorBuffer, Self.SectorSize);
    SetLength(Self.fpClusterBuffer, Self.ClusterSize);

    result := true;
end;


//
//
// Paramtres nécéssaire:
// LBA
// Taille part
// NbFat		(2 par défaut)
// Resv sector	(? par defaut)
//function LoadFromExpertMode():boolean;
//begin
	// Calcule la taille de la FAT

    // Renseigne les infos
//end;


//
//
//
//Lire BootBlock
//Si Erreur Lire Backup Bootblock
//Si Erreur Sortir
//Lire FSInfo
//Si Erreur indique par un flag
//Lire FAT[0] et FAT[1]
//Si Erreur indique par un flag


//
// Récupère les infos sur la partition à partir d'un BootSecter en mémoire
//
function TFAT32Partition.GetInfoFromBootSector(const buf : Pointer): boolean;
var
	bootsector : PFAT32BootSector;
    temp : cardinal;
begin
	bootsector := PFAT32BootSector(buf);

    // Verifier si c'est un BootSecot valide
	Result := false;
    if not TFAT32Partition.isValidBootSector(buf) then
    	exit;

	// BB Valide
    Result := true;

    {//#Todo2 partie Verif validé à mettre dans isValidBootSector
    // Verif des signatures
    if bootsector.BS_BootSig <> BootSigValue then
    	Result := false;

    if bootsector.BS_BootRecordSig <> BootRecordSigValue then
    	Result := false;

    if bootsector.BPB_FATSz16 <> 0 then
    	Result := false;

	//bootsector.BPB_RootEntCnt = 0 pour FAT32

    //if not (bootsector.BPB_BytsPerSec in [512, 1024, 2048, 4096]) then
    //	Result := false;
    //BPB_RsvdSecCnt <> 0 sinon pas valide

    // On sort en cas d'erreur
    if not result then
    	Exit;}

    // Champs communs FAT16/32
    Self.FOEMName		:= bootsector.BS_OEMName;
    Self.fpSectorSize	:= bootsector.BPB_BytsPerSec;
    Self.fpRsvdSecCnt	:= bootsector.BPB_RsvdSecCnt;
    Self.fpFATCount		:= bootsector.BPB_NumFATs;
    Self.fpMedia		:= bootsector.BPB_Media; 
	Self.FHiddenSectors := bootsector.BPB_HiddSec;
	//#TODO2 mettre les champs communs dans GenericFAT

    // Recupère les infos
    Self.fpClusterSize	:= bootsector.BPB_BytsPerSec * bootsector.BPB_SecPerClus;	// EN OCTETS
    Self.fpVolumeID		:= bootsector.BS_VolID;
    Self.fpVolumeLabel	:= bootsector.BS_VolLab;
    Self.fpMirrorFAT	:= (bootsector.BPB_ExtFlags and MirrorFATMask) = 0;
    Self.fpActiveFAT	:= bootsector.BPB_ExtFlags and FATCountMask; // indique le n° de FAT active si pas de mirroir
    Self.fpFATSize		:= bootsector.BPB_FATSz32;
    Self.fpTotalSectors	:= bootsector.BPB_TotSec32;

    Self.fpRootDirCluster	:= bootsector.BPB_RootClus;
    Self.fpFSInfo			:= bootsector.BPB_FSInfo;
    Self.fpBackupBootSec	:= bootsector.BPB_BkBootSec;

    //FirstDataSector = BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors;
    //FirstSectorofCluster(N) = ((N - 2) * BPB_SecPerClus) + FirstDataSector;

    Self.fpFSVerMajor := Hi(bootsector.BPB_FSVer);
    Self.fpFSVerMinor := Lo(bootsector.BPB_FSVer);

    // Calcule des parametres
    //The maximum valid cluster number for the volume is CountofClusters + 1
    //DataSec = TotSec - (BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors);
    //
    //Now we determine the count of clusters:
    //
    //CountofClusters = DataSec / BPB_SecPerClus;
    temp := Self.fpTotalSectors - (self.fpRsvdSecCnt + (Self.fpFATCount * self.fpFATSize));
    Self.fpMaxValidClustNum := (temp div bootsector.BPB_SecPerClus) + 1;

    // Similarly, a FAT file system driver must not allow a directory (a file that is
    // actually a container for other files) to be larger than 65,536 * 32 (2,097,152) bytes.
    // 65536 entrées max dans un rep
    Self.fpMaxClusterPerDir := 65536 * 32 div Self.fpClusterSize;

    // Alloue la mem pour les buffer Cluster et Secteur
    SetLength(Self.fpSectorBuffer, Self.fpSectorSize);	//#ATTN Déja alloué
    SetLength(Self.fpClusterBuffer, Self.fpClusterSize);
end;



Const
// http://support.microsoft.com/default.aspx?scid=kb;en-us;314878
// http://support.microsoft.com/default.aspx?scid=kb;EN-US;140365

// Taille par défaut des cluster en fonction de la taille de la partition
//    DSKSZTOSECPERCLUS DskTableFAT32 [] = {
//        {       66600,   0},  /* disks up to 32.5 MB, the 0 value for SecPerClusVal trips an error */
//        {     532480,   1},   /* disks up to 260 MB,  .5k cluster */
//        { 16777216,   8},     /* disks up to     8 GB,    4k cluster */
//        { 33554432, 16},      /* disks up to   16 GB,    8k cluster */
//        { 67108864, 32},      /* disks up to   32 GB,  16k cluster */
//        { 0xFFFFFFFF, 64}/* disks greater than 32GB, 32k cluster */
//    };
PartSizeLimit: array [0..5] of Cardinal = (66600, 532480, 16777216, 33554432, 67108864, $FFFFFFFF);
DefClusterSize: array [0..5] of Cardinal = (0, 1, 8, 16, 32, 64);


//
// Calcule la taille de cluster grace à la taille de la partition
//
//class function TNTFSPartition.GetDefaultClusterSize(const dsize: int64): Cardinal;
function GetDefaultClusterSize(const Partsize: Cardinal): Cardinal;
var
    i: integer;
begin
	Result := 0;

	// Rechercher la taille du cluster BPB_SecPerClus
    for i:= Low(PartSizeLimit) to High(PartSizeLimit) do
    	if PartSize <= PartSizeLimit[i] then
        begin
			Result := DefClusterSize[i];
            break;
        end;
end;


//Calcul de la taille de la FAT32
// DskSize = Taille de la partition en secteur (nombre de secteur dans la partition)
// BPB_ResvdSecCnt = 32 en général
// BPB_SecPerClus = déduit de la taille part  => cf tableau ci dessus
// BPB_NumFATs = nombre de FAT, typiquement 2
//
//TmpVal1 = DskSize - BPB_ResvdSecCnt;
//TmpVal2 = ((256 * BPB_SecPerClus) + BPB_NumFATs) / 2;
//FATSz = (TMPVal1 + (TmpVal2 - 1)) / TmpVal2;
//
//
// Procedure: ComputeFATSize
// Purpose: Calcule la taille standard de la FAT
//
// Arguments: PartSize: taille de la partition en secteur
//			  BPB_ResvdSecCnt: nombre de secteur reservé (32 par défaut)
//			  BPB_NumFATs: nombre de FAT (2 par défaut)
// Result:    Taille de la FAT en secteur ou 0 si erreur
//
function ComputeFATSize(const PartSize, BPB_ResvdSecCnt, BPB_NumFATs: Cardinal) : Cardinal;
var
	BPB_SecPerClus, tmpVal1, tmpVal2 : Cardinal;
begin
	// Rechercher la taille du cluster BPB_SecPerClus
    BPB_SecPerClus := GetDefaultClusterSize(PartSize);

    if BPB_SecPerClus <> 0 then
    begin
        TmpVal1 := PartSize - BPB_ResvdSecCnt;
        tmpval2 := ((256 * BPB_SecPerClus) + BPB_NumFATs) div 2;
        Result := (tmpVal1 + (TmpVal2 - 1)) div TmpVal2;
    end
    else
    	Result := 0;
end;

// TmpVal1 = 20482843
// TmpVal2 = ((256 * 16) + 2) / 2 = 2049
// FatSz = (20482843 + (2049 - 1)) / 2049 = 9997
// => Fat Mirror commence à 63 + 32 + 9997 = 10092
// => Cluster 2 commence à 63 + 32 + 2 * 9997 = 20089


// ----------------------------------------------------------------------
//						Gestion de l'analyse
// ----------------------------------------------------------------------


//
// Scan la partition depuis le répertoire racine
//
function TFAT32Partition.ScanRootDirectory(): boolean;
begin
    Result := Self.ScanRelativeDirectory(int64(Self.fpRootDirCluster));
end;


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


//
// Renvoie le nom du File System
//
function TFAT32Partition.GetFileSystemName : String;
begin
    if (Self.fpFSVerMajor <> 0) or (Self.fpFSVerMinor <> 0) then
	    Result := Format('FAT 32 v%d.%d', [Self.fpFSVerMajor, Self.fpFSVerMinor])
    else
		Result := Self.GetFileSystemFamily;
end;


function TFAT32Partition.GetFileSystemFamily(): string;
begin
	Result := 'FAT 32';
end;


//
// Renvoie les propriétés dans des couples (propriété, valeur)
//
procedure TFAT32Partition.GetPartitionProperties(info: TStrings);
begin
    inherited;

    // ------------ Fat32 specifique ------------
	info.Add('FS Info sector');
    info.Add(IntToStr(Self.fpFSInfo));
    info.Add('Root Directory cluster #');
    info.Add(IntToStr(Self.fpRootDirCluster));

    if Self.FFreeCount <> MaxCardinal then
    begin
    	info.Add('Free Clusters');
        info.Add(IntToStr(Self.FFreeCount));
    end;
end;


{-----------------------------------------------------------------------------
  Procedure: TFAT32Partition.GetFirstSectorOfCluster
  Author:    Chris
  Date:      17-mai-2004
  Arguments: const n : Cardinal
  Result:    Cardinal
-----------------------------------------------------------------------------}
function TFAT32Partition.GetFirstSectorOfCluster(const n : Cardinal) : Cardinal;
begin
    Result := ((n - 2) * (Self.ClusterSize div Self.SectorSize)) + Self.fpRsvdSecCnt + (self.fpFATCount * self.FATSize);
end;


// --------------------------------------------------------------------------
//					Gestion des entrées dans la FAT
// --------------------------------------------------------------------------

//
//
// The first reserved cluster, FAT[0], contains the BPB_Media byte value in its
// low 8 bits, and all other bits are set to 1
//
// FAT[1]
//    ClnShutBitMask  = 0x08000000;
//    HrdErrBitMask   = 0x04000000;
//
//Bit ClnShutBitMask -	If bit is 1, volume is "clean".
//If bit is 0, volume is "dirty". This indicates that the file system driver did not Dismount the volume properly the last time it had the volume mounted. It would be a good idea to run a Chkdsk/Scandisk disk repair utility on it, because it may be damaged.
//Bit HrdErrBitMask -	If this bit is 1, no disk read/write errors were encountered.
//If this bit is 0, the file system driver encountered a disk I/O error on the Volume the last time it was mounted, which is an indicator that some sectors may have gone bad on the volume. It would be a good idea to run a Chkdsk/Scandisk disk repair utility that does surface analysis on it to look for new bad sectors.


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
function TFAT32Partition.GetFATEntry(const indice: Cardinal): Cardinal;
type
	PCardinal = array of Cardinal;
var
    sectorfat, sectormirfat, sectorswap, offset: Cardinal;
begin
	// Calculer le secteur contentant l'entrée indice et le secteur mirroir
    sectorfat := Self.fpRsvdSecCnt + (indice * Self.fpFATEntrySize) div Self.fpSectorSize;
    sectormirfat := sectorfat + Self.fpFATSize;

    // Calculer l'offset dans le secteur
    offset := (indice) mod (Self.fpSectorSize div Self.fpFATEntrySize);

    if Self.FATReadFlag = rmMirrorFirst then
    begin
    	// On permute les secteurs
    	sectorswap := sectorfat;
        sectorfat := sectormirfat;
        sectormirfat := sectorswap; 
    end;

	if Self.FATReadFlag = rmOnlyActive then
    	sectormirfat := sectorfat;
    
	if Self.FATReadFlag = rmOnlyMirror then
    	sectorfat := sectormirfat; 
	        
    // Lecture du secteur concerné
    if not Self.ReadSector(sectorfat) then
    	if Self.fpMirrorFAT then
        begin
        	// On essaie le mirroir
            if not Self.ReadSector(sectormirfat) then
            begin
	            Result := FATEntryReadError;
                Exit;
            end;
        end
        else
        begin
            Result := FATEntryReadError;
            Exit;
        end;

    Result := PCardinal(Self.fpSectorBuffer)[offset];
    Result := Result and $0fffffff;
end;


//
// Retourne vrai si le num de cluster correspond à un cluster libre
//
function TFAT32Partition.isFreeCluster(const Cluster : Cardinal) : boolean;
begin
	result := (Cluster = 0);
end;


//
//  Retourne vrai si le num de cluster correspond au dernier cluster d'un fichier
//
function TFAT32Partition.isLastCluster(const Cluster : Cardinal) : boolean;
begin
	result := (Cluster >= $0FFFFFF8);
end;


//
//  Retourne vrai si le num de cluster correspond à un cluster défectueux
//
function TFAT32Partition.isBadCluster(const Cluster : Cardinal) : boolean;
begin
	result := (Cluster = $0FFFFFF7);
end;


// --------------------------- Fin de l'unité -----------------------------
end.
{A primary partition may start at the beginning of a
cylinder (head=0, sector=1), while a logical partition starts a
little further along (head=1, sector=1).
And remember that all partitions begin at the first sector of a cylinder.
MBR  0-0-1
EMBR x-0-1
Part x-1-1
}
//
// Part 0: CHS    0-1-1 TRAVAIL 1stSect :       63 Taille Part 0 : 20482812 sect
// Part 1: CHS 1275-1-1 JEUX	1stSect : 20482938 MaxValidClust 1267008
// Part 2: CHS 3800-1-1 STORAGE 1stSect : 61047063
//
// Part 0
// Repertoires						First Cluster
//	Sites							521
//	Base Documentaire				44304
//	Mes Documents					3
//	PERSO							228277
//	Mes Documents/Dev				3533
//	Mes Documents/Mes Images		1769
//	Mes Documents/Outlook Express	2392

// Root Dir Part 1
//MKY2 (rep) First cluster : $0003AA24 (240164) 1 seul  secteur 28187956
//MONKEY2 (rep) 1st cluster : $00051C33 (334899)
// Program Files (rep) 1st cluster $0001FC10 (130064)
// Save (rep) $0000791A (31002)

// PArt 0: Repertoire Mes Documents (Cluster 3)
