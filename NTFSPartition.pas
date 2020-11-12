{-----------------------------------------------------------------------------
 Unit Name: NTFSPartition
 Author:    Chris
 Purpose:   Gestion des partitions NTFS
 History:   01/07/2005
-----------------------------------------------------------------------------}

// Documentation sur NTFS
// http://linux-ntfs.sourceforge.net/ntfs/index.html
// http://www.geocities.com/thestarman3/asm/mbr/NTFSBR.htm
// http://www.microsoft.com/resources/documentation/windows/2000/professional/reskit/en-us/part7/proch32.mspx
//
// Sources
// http://koti.mbnet.fi/smiika/downloads.htm (Disk Buddy)
// http://www.codeproject.com/file/NTFSUndelete.asp?msg=1048799
//


unit NTFSPartition;

interface

uses
	Classes, SysUtils, Windows, StrUtils,
    GenericDrive, GenericPartition, NTFSMFTEntry, NTFSAttributes, CommonFileInfo;


type              


//
// Partition NTFS
//
TNTFSPartition = class(TGenericPartition)
    private
        // ---------------- Infos du BootSector -----------------
        FOemName:				string;		// Doit contenir NTFS
        FMediaID:				Byte;		// Media Descriptor ID ($F8).
        FHiddenSectors:			Cardinal;	// Offset secteur par rapport à la début du disque ou part etendue
        FVolumeTotalSector:		int64;		// Nombre total de secteur du VOLUME (taille part - 1)
        FMTFClust:				Int64;		// Starting Cluster Number for the $MFT File in this partition
                                            // (Logical Sector 32; if the Sectors per Cluster value is an 8).
        FMTFMirrorClust:		Int64;		// Starting Cluster Number for the $MFTMirror File in this partition.
        FClustersPerFileRec: 	Integer;	// Clusters (or bytes) per File Record Segment (FRS).
        FClustersPerIndexBlock:	Integer;	// Clusters per Index Block (or Record).
        FSerialNumber:			Int64;		// Volume Serial Number Mettre array of byte ???
        FChecksum:				Cardinal;	// Checksum (0 généralement)
        // ---------- Attribute - $VOLUME_INFORMATION ($70) ------------
        FNTFSVersion:			Word;		// http://linux-ntfs.sourceforge.net/ntfs/attributes/volume_information.html
        FVolumeInfoFlags:		TVolumeInfoFlags;	//DirtyFlag = $0001
        // --------------- Attribute - $VOLUME_NAME ($60) --------------
        FVolumeName:			WideString;	// Nom du volume
        // ------------------ Informations calculées ------------------------
        FFileRecordSize:		Cardinal;	// Taille du File Record en OCTETS
        FIndexRecordSize:		Cardinal;	// Taille du Index Record en OCTETS
        // ------------------- Gestion -----------------
        FMFT: 			TMFT;		//
        // Data runs de la MFT
        // Data runs de la MFTMirror ?
        //FBitMap: TMFTFileEntry;	// Data run $Bitmap
        FFileRecordStream: TMemoryStream;
    protected
        function GetInfoFromBootSector(const buf: Pointer): boolean; override;
		function GetFirstSectorOfCluster(const n : Cardinal) : Cardinal; override;
        function GetFileSystemName : String; override;
        function GetFileSystemFamily: String; override;
        // -------------- Fonctions pour les fichiers NTFS ------------------
        procedure LoadMFT();		// Charge la MFT en mémoire
        procedure LoadInfoFromVolumeFile();		// Name et version NTFS
        // -----
        //procedure GetMFTEntryOffset(const ind: Cardinal; var numcluster, offset: Cardinal);
		//function LoadFileRecord(const fileref: int64): boolean;
    public
        // ============================= Methodes =============================
        constructor Create(disque: TGenericDrive; const firstsect : Cardinal); override;
        destructor Destroy; override;
        // ----------------------- Recupère les infos part -----------------
        procedure GetPartitionProperties(info: TStrings); override;
        class function isValidBootSector(const buffer: Pointer): boolean; override;
        class function GetDefaultClusterSize(const dsize: int64): Cardinal; override;
        function LoadFromPartInfo(info: PPartitionInfo): boolean; override;
		function LoadFromLBA(const lba:Cardinal): boolean; override;
		function LoadFromBackupLBA(const lba:Cardinal): boolean;
        // --------------------- Analyse Répertoires -------------------
        function ScanRootDirectory(): boolean; override;
        function ScanRelativeDirectory(const dirid: int64): boolean; override;
		function RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean; override;
        // ============================= Propriétés =============================
        property HiddenSectors: Cardinal read FHiddenSectors;
        property FileRecordSize: Cardinal read FFileRecordSize;
        property IndexRecordSize: Cardinal read FIndexRecordSize;
        property FirstMTFCluster: Int64 read FMTFClust;
		property ClustersPerIndexBlock: integer read FClustersPerIndexBlock;
        property NTFSVersion: Word read FNTFSVersion;
end;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


uses
	ConversionUnit;


type


// NTFS boot sector contents
TNTFSBootRecord = packed record
    BootCode:		array[0..2] of Byte;
    OemName:		array[0..7] of Char;// Contient 'NTFS    '
    // ---- Debut du BPB -----
    BytesPerSec:	Word;				// Bytes Per Sector
    SecPerClust:	Byte;				// Sectors Per Cluster
    ReservedSectors:Word;				// Reserved Sectors
    AlwaysZero0: 	array[0..2] of Byte;// always 0
	Unused0:		Word;				// Not Used
    MediaDescriptor:Byte;				// Media Descriptor
    AlwaysZero1:	Word;				// always 0
    SectorsPerTrack:Word;				// Sectors Per Track
    NumOfHeads:		Word;				// Number Of Heads
    HiddenSectors:	Cardinal;			// Hidden Sectors, inutilisable en NTFS
    Unused2:		array[0..7] of Byte;// not used by NTFS
    TotalSector:	Int64;				// Nb Total Sectors dans le volume != partition
    MTFClust:		Int64;				// Starting Cluster Number for the $MFT File in this partition
    									// (Logical Sector 32; if the Sectors per Cluster value is an 8).
    MTFMirrorClust: Int64;				// Starting Cluster Number for the $MFTMirror File in this partition.
    ClustersPerFileRec:		Integer;	// Clusters (or bytes) per File Record Segment (FRS).
    ClustersPerIndexBlock:	Integer;	// Clusters per Index Block (or Record).
    SerialNumber:	int64;				// Volume Serial Number, mettre tableau de LONG ???
    Checksum:	  	Cardinal;			// Generalement 0
    // ----- Bootstrap Code -----
    Code:					array [0..425] of Byte;	// Bootstrap Code
    BootSignature:			Word;		// $AA55
end;
PNTFSBootRecord = ^TNTFSBootRecord;


// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Creation de la partition NTFS
//
constructor TNTFSPartition.Create(disque: TGenericDrive; const firstsect: Cardinal);
begin
    inherited;

    FFileRecordStream := TMemoryStream.Create;
end;


//
//
//
destructor TNTFSPartition.Destroy;
begin
	inherited;

    FFileRecordStream.Free;
    Self.FMFT.Free;
end;


// ----------------------------------------------------------------------
//  			Récuperation des infos Boot Sector et PartInfo						
// ----------------------------------------------------------------------


//
// Verifie si le buffer contient un Boot Sector NTFS valide
//
class function TNTFSPartition.isValidBootSector(const buffer: Pointer): boolean;
var
	bs: PNTFSBootRecord;
begin
	Result := true;

    bs := PNTFSBootRecord(buffer);

    // Verif sur le bootcode ?
    if (bs.BootCode[0] <> $EB) and (bs.BootCode[0] <> $EB) then
        Result := false;

    if bs.BootSignature <> BootRecordSigValue then
        Result := false;

    // Verif sur l'OEMName ? au moins 4 er car
    if Strlcomp('NTFS', bs.OemName, 4) <> 0 then
        Result := false;

    // Sector size multiple de 512 ()
    if (bs.BytesPerSec <> 512) and (bs.BytesPerSec <> 1024) then
        Result := false;

    if (bs.AlwaysZero0[0] <> 0) or  (bs.AlwaysZero0[1] <> 0) or (bs.AlwaysZero0[2] <> 0) then
        Result := false;

    if bs.AlwaysZero1 <> 0 then
        Result := false;

    if not (bs.SecPerClust in [1, 2, 4, 8, 16, 32, 64, 128]) then
        Result := false;
end;


//
// Récupère les infos sur la partition à partir du BootSecter dans le buffer
// Cela peut être un backup boot sector
//
function TNTFSPartition.GetInfoFromBootSector(const buf : Pointer): boolean;
var
	bs: PNTFSBootRecord;

    //
    // Fonction de normalisation pour les champs ClustersPerFileRec et
    // ClustersPerIndexBlock
    //
    function ConvertClusterPerField(const value: integer): integer;
    begin
    	// Nombre négatif si >= 128
        if value >= $80 then
        	Result := (value or $ffffff00)	// Conversion en vrai negatif
        else
        	Result := value;
    end;

    
begin
	// Verifie si le boot sector est valide
	Result := TNTFSPartition.isValidBootSector(buf);

    if Result then
    begin
   		bs := PNTFSBootRecord(buf);

        //#Todo2 Prendre la taille du secteur depuis le disque ou bs ????
        Self.fpClusterSize := bs.SecPerClust * Self.fpSectorSize;

        Self.FOemName		:= bs.OemName;
        Self.FMediaID		:= bs.MediaDescriptor;
        Self.FHiddenSectors	:= bs.HiddenSectors;
        Self.FVolumeTotalSector	:= bs.TotalSector;
        //#Todo verifier si fpTotalSectors déja initialisé ????
        //#ATTN ne pas confondre taille partition et taille volume !!!
        Self.fpTotalSectors := Cardinal(bs.TotalSector) + 1; 
        Self.FMTFClust		:= bs.MTFClust;
        Self.FMTFMirrorClust:= bs.MTFMirrorClust;
        Self.FClustersPerFileRec := ConvertClusterPerField(bs.ClustersPerFileRec); 
        Self.FClustersPerIndexBlock := ConvertClusterPerField(bs.ClustersPerIndexBlock);
        Self.FSerialNumber	:= bs.SerialNumber;
        Self.FChecksum		:= bs.Checksum;

        // Calculer la taille en octets des File Record
        if Self.FClustersPerFileRec < 0 then
	        Self.FFileRecordSize := 1 shl (-Self.FClustersPerFileRec)
        else
	        Self.FFileRecordSize := Cardinal(Self.FClustersPerFileRec) * Self.fpClusterSize;

        // Calculer la taille en octets des Index Record
        if Self.FClustersPerIndexBlock < 0 then
	        Self.FIndexRecordSize := 1 shl (-Self.FClustersPerIndexBlock)
        else
	        Self.FIndexRecordSize := Cardinal(Self.FClustersPerIndexBlock) * Self.fpClusterSize;

		Self.fpMaxValidClustNum := Cardinal(Self.FVolumeTotalSector div Self.SectorPerCluster) - 1;        

        // Alloue la mem pour les buffer Cluster
        SetLength(Self.fpClusterBuffer, Self.fpClusterSize);
    end;
end;


//
// Renseigne la partition à partir de l'entrée partition : charge le boot sector
// et le backup boot sector si nécessaire
//
function TNTFSPartition.LoadFromPartInfo(info: PPartitionInfo): boolean;
var
	bbs_Offset: Cardinal;
begin
    // Lire le Boot Sector, et le backup sector en cas de problème
    Result := Self.ReadSector(0);
    if Result then
    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);

    // Si Err E/S ou dans BootSector, on essaie le backup Bootsectore
    // Selon OS, le backup n'est pas au même endroit : soit dernier secteur
    // de la partition ou milieu de la partition (NT3.51)

    // Backup Boot sector NT4+
    bbs_Offset := info.FSectorCount - 1;
    if not Result then
		if Self.ReadSector(bbs_Offset) then
	    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);

    //#Todo3 Verifier la validité du calcul
    // NT 3.51 Backup Boot sector
    bbs_Offset := info.FSectorCount div 2;
    if not Result then
		if Self.ReadSector(bbs_Offset) then
	    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);
end;


//
// Charge les information sur la partition à partir du boot sector donné
// en parametre (offset relatif disque)
//
function TNTFSPartition.LoadFromLBA(const lba:Cardinal): boolean;
begin
	//
    Self.fpFirstSector := lba;

    // Lire le Boot Sector
    Result := Self.ReadSector(0);
    if Result then
    	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);

    // Impossible de lire le backup sans connaitre la taille de la partition !!!!
end;


//
// Crée la partition à partir du lba du backup boot sector
//
function TNTFSPartition.LoadFromBackupLBA(const lba:Cardinal): boolean;
begin
	// Considère que lba est le BB, récupère les infos du BB
    Result := Self.LoadFromLBA(lba);

    if Result then
    begin
        // Recalcule le VRAI début de la partition (BBS dernier secteur de la partition)
        // ATTN ne marche que pour NT4, 2000, XP
        Dec(Self.fpFirstSector, Cardinal(Self.FVolumeTotalSector));

        // Vide le cache  puique l'on a modifié fpFirstSector
        Self.FlushSectorCache;
    end;
end;


// ----------------------------------------------------------------------
//
// ----------------------------------------------------------------------


procedure TNTFSPartition.LoadInfoFromVolumeFile();
var
	volinfo: TMFTVolumeEntry;
begin
	// Lire le file record de $Volume
	volinfo := TMFTVolumeEntry.Create(FILE_Volume);
    try
        Self.FVolumeName := volinfo.VolumeName;
        Self.FVolumeInfoFlags := volinfo.VolumeFlags;
        Self.FNTFSVersion := (volinfo.Major shl 8) or volinfo.Minor;
    finally
	    volinfo.Free;
    end;
end;


// --------------------------------------------------------------------------
//							Gestion de la MFT
// --------------------------------------------------------------------------


procedure TNTFSPartition.LoadMFT();
begin
	if not Assigned(Self.FMFT) then
		Self.FMFT := TMFT.Create(Self);

    //#TODO2 Gestion des erreurs chargement MFT
end;


//
//
// Rename to LoadMFTEntry ?
{function TNTFSPartition.LoadFileRecord(const fileref: int64): boolean;
var
	writesize, offset, cluster: Cardinal;
    i, nbclust: integer;
begin
    Self.FFileRecordStream.Clear;
    Self.FFileRecordStream.Size := Self.FFileRecordSize;

    Result := true;

	// Gestion du BootStrap
    if fileref = 0 then
    begin
    	cluster := Cardinal(Self.FMTFClust);

        // FileRecord plus grand que Cluster ?
        if Self.FFileRecordSize > Self.ClusterSize then
        begin
	        // Hypothese cluster consécutifs
            nbclust := Self.FFileRecordSize div Self.ClusterSize;
            writesize := Self.ClusterSize;
        end
        else
        begin
            nbclust := 1;
            writesize := Self.FFileRecordSize;
        end;

        // Lire le(s) cluster(s) contenant le FileRecord
        for i := 0 to nbclust - 1 do
        begin
            //#TODO2 Gestion des erreurs
            Self.ReadCluster(cluster + i);

            Self.FFileRecordStream.WriteBuffer(Self.fpClusterBuffer[offset], writesize);
        end;
    end
    else
    begin
    	// Entrée normale

        //#Todo2 Modifier le parametre
        Self.GetMFTEntryOffset(Cardinal(fileref), cluster, offset);
    end;

    // Se positionner au début du file record
    Self.FFileRecordStream.Position := 0;
end;}


//
// Renvoie le cluster sur le disque qui contient  le filerecord
// FileSeq sur int64 ???
// Calculer aussi nbclust et writesize ?
{procedure TNTFSPartition.GetMFTEntryOffset(const ind: Cardinal; var numcluster, offset: Cardinal);
var
	mftclusterindex: Cardinal;
begin
	// Tester si MFTLoaded et ind <> 0, dans ce cas lever une exception

	// Calculer l'indice du cluster dans la MFT
    mftclusterindex := (Self.FFileRecordSize * ind) div Self.ClusterSize;
    // récuperer le numéro de cluster correspondant à l'indice dans le MFT
	//numcluster := GetMFTCluster(mftclusterindex);

    offset := (Self.FFileRecordSize * ind) mod Self.ClusterSize;
end;}


// --------------------------------------------------------------------------
//							Analyse des répertoires
// --------------------------------------------------------------------------


//
// Procedure: TNTFSPartition.ScanRootDirectory
// Purpose: Commence l'analyse à partir du répertoire racine   
//
// Arguments: aucun 
// Result:    boolean, false si erreur dans le scan du répertoire, true sinon
//
function TNTFSPartition.ScanRootDirectory(): boolean;
begin
	Self.LoadMFT;
	Result := Self.ScanRelativeDirectory(FILE_root);
end;


//
// Result:    boolean, false si erreur dans le scan du répertoire, true sinon
//
function TNTFSPartition.ScanRelativeDirectory(const dirid: int64): boolean;
var
	DirEntry: TMFTDirEntry;
    i: integer;
    abortscan: boolean;
begin
   	Result := false;
    try
	    DirEntry := TMFTDirEntry.Create(dirid);
        try
            if Assigned(Self.OnFile) then
                for i := 0 to DirEntry.FileInfoCount - 1 do
                    if DirEntry.FileInfo[i].FileID >= FILE_first_user then
                        Self.OnFile(Self, DirEntry.FileInfo[i], abortscan);

            if Assigned(Self.OnDirectory) then
                for i := 0 to DirEntry.DirInfoCount - 1 do
                    if DirEntry.DirInfo[i].FileID >= FILE_first_user then
                        Self.OnDirectory(Self, DirEntry.DirInfo[i], abortscan);

            //
            Result := true;
        finally
		    // Liberation
            DirEntry.DetachFileNodes;
            DirEntry.Free;
        end; // try
    except
		//on E: Exception do DebugExceptionRaised(E.Message);
    end;
end;


// --------------------------------------------------------------------------
//
// --------------------------------------------------------------------------


//
// Restaure un fichier dans un stream, retourne false en cas de problème
//
function TNTFSPartition.RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean;
var
	i: integer;
    nbclust: int64;
    fent: TMFTFileEntry;
begin
    Result := false;
    try
        fent := TMFTFileEntry.Create(finfo.FileID);
        try
            // Les fichiers compressed, sparse, ... ne sont pas traités
            if (fent.Flags * [aCompressed, aReparsePoint, aSparseFile]) = [] then
            begin
                // Traitement particulier pour le dernier cluster (probablement pas complet)
                nbclust := fent.FileSize div int64(Self.ClusterSize);
                for i := 0 to integer(nbclust) - 1 do
                begin
                    if not fent.ExtractToStream(i * Self.ClusterSize, Self.ClusterSize, stream) then
                        Result := false;

                    //OnProgress ?
                    if Assigned(Self.OnRestore) then
                        Self.OnRestore(Self, finfo.Size, Self.ClusterSize * i);
                end;

                // Traitement de la partie qui dépasse
                if not fent.ExtractToStream(nbclust * Self.ClusterSize, fent.FileSize mod Self.ClusterSize, stream) then
                    Result := false;

                if Assigned(Self.OnRestore) then
                    Self.OnRestore(Self, finfo.Size, finfo.Size);

			    Result := true;
            end; // if
		finally
	        fent.Free;
        end; // try
    except
    end; // try
end;


// --------------------------------------------------------------------------
//							Gestion des Propriétés
// --------------------------------------------------------------------------


//
// Renvoie le nom du File System Famille + version
//
function TNTFSPartition.GetFileSystemName : String;
begin
	// Selon Self.FNTFSVersion;
    if Self.NTFSVersion <> 0 then
    begin
    	// Major octet poid faible, Minor octet poid fort
        Result := Format('NTFS %d.%d', [Lo(Self.NTFSVersion), Hi(Self.NTFSVersion)]);

        case Self.NTFSVersion of
          $0201: Result := Result + ' (Windows NT)';
          $0003: Result := Result + ' (Windows 2000)';
          $0103: Result := Result + ' (Windows XP)';
        end;
    end
    else
		Result := Self.FileSystemFamilyName;
end;


function TNTFSPartition.GetFileSystemFamily(): string;
begin
	Result := 'NTFS';
end;


//
// Ecrit les infos sur la partition dans un TString (couple propriété, valeur)
//
procedure TNTFSPartition.GetPartitionProperties(info: TStrings);
var
    chs : TCHS;
begin
	//#Todo2 Partie commune à toute les partitions à remonter dans GenericPart
	//inherited;
    Self.fpPhysicalDrive.LBAToCHS(chs, Self.FirstSecOfPart);

    info.Add('Partition starts at');
    info.Add(Format('%d-%d-%d (CHS)',[chs.Cylinder, chs.Head, chs.Sector]));
    info.Add('');
    info.Add(Format('%d (LBA)',[Self.FirstSecOfPart]));

	info.Add('File System');
	info.Add(Self.GetFileSystemName);

   	info.Add(TOTALSECTORS_STR);
    info.Add(IntToStr(Self.TotalSectors));

   	info.Add('Volume Sector Count');
    info.Add(IntToStr(Self.FVolumeTotalSector));

    info.Add(SECTORSIZE_STR);
    info.Add(SizeToRoundedByteString(Self.SectorSize));

    info.Add(CLUSTERSIZE_STR);
    info.Add(SizeToRoundedByteString(Self.ClusterSize));

    info.Add(SECTORPERCLUSTER_STR);
    info.Add(IntToStr(Self.SectorPerCluster));

	info.Add(HIDDENSECTORS_STR);
    info.Add(IntToStr(Self.HiddenSectors));

    info.Add(OEMNAME_STR);
    info.Add(Self.FOEMName);

    // ----------------- Info spécifiques NTFS ------------------
    if Self.FVolumeName <> '' then
    begin
        info.Add(VOLUMENAME_STR);
        info.Add(string(Self.FVolumeName));

        info.Add(NTFSVOLUMEFLAGS_STR);
	    info.Add(GetVolumeFlagsString(Self.FVolumeInfoFlags));
    end;

	// $MFT Info
    info.Add(MFTFIRSTCLUST_STR);
    info.Add(IntToStr(Self.FMTFClust));
    info.Add(MFTMIRFIRSTCLUST_STR);
    info.Add(IntToStr(Self.FMTFMirrorClust));

	info.Add(CLUSPERFILERECORD_STR);
    info.Add(IntToStr(Self.FClustersPerFileRec));

	info.Add(FILERECORDSIZE_STR);
    info.Add(IntToStr(Self.FFileRecordSize));

   	info.Add(CLUSTPERINDXBLCK_STR);
    info.Add(IntToStr(Self.FClustersPerIndexBlock));

   	info.Add(INDEXBLOCKSIZE_STR);
    info.Add(IntToStr(Self.FIndexRecordSize));

    info.Add(NTFSSERIAL_STR);
    info.Add(IntToHex(Self.FSerialNumber, 16));
    //info.Add(IntToHex(Self.FSerialNumber and $ffffffff, 8)); // VolumeID 

    info.Add(CHECKSUM_STR);
    info.Add(IntToHex(Self.FChecksum, 8));

    if Assigned(Self.FMFT) then
    begin
	    info.Add(MFTSIZE_STR);
	    info.Add(SizeTo2DigitByteString(Self.FMFT.FileSize));
    end;
end;


// --------------------------------------------------------------------------
//					Gestion des clusters
// --------------------------------------------------------------------------


//
// n = numéro du cluster 
//
function TNTFSPartition.GetFirstSectorOfCluster(const n : Cardinal) : Cardinal;
begin
    // cluster 0 = secteur 0, pas de zone réservé sur une partition NTFS
    Result := Self.SectorPerCluster * n;
end;


// --------------------------------------------------------------------------
//
// --------------------------------------------------------------------------


{
http://support.microsoft.com/default.aspx?scid=kb;EN-US;140365
The following table shows the default values that Windows NT/2000/XP uses for NTFS formatting:

      Drive size
   (logical volume)             Cluster size          Sectors
   ----------------------------------------------------------
     512 MB or less               512 bytes           1
     513 MB - 1,024 MB (1 GB)   1,024 bytes (1 KB)    2
   1,025 MB - 2,048 MB (2 GB)   2,048 bytes (2 KB)    4
   2,049 MB and larger          4,096 bytes (4 KB)    8
}


//
// Renvoie taille en octet ou nb cluster ????
// 0 = non definie
class function TNTFSPartition.GetDefaultClusterSize(const dsize: int64): Cardinal;
const
DefClusterSizeArray: array[0..2] of int64 = (
	512 * (1024*1024), 1024 * (1024*1024), 2147483648 {2 GB}
);
var
	i: integer;
begin
	// Cas  dsize > 2Go           
    Result := 8;

    for i := 0 to 2 do
    	if dsize <= DefClusterSizeArray[i] then // < ou <= ???
        begin
        	Result := (1 shl i);
            exit;
        end;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
