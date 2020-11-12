{-----------------------------------------------------------------------------
 Unit Name: GenericFat
 Author:    Chris
 Purpose:   Classe génerique pour le gestion des FS FATxx
 History:   12/05/2005

 Added Gestion du root directory pour FAT16/32
 
-----------------------------------------------------------------------------}

// --------------------------------------------------------------------------
//
//#Todo1 Verifier que les entrées dir sont valables
//
// Routine pour lire le bootsector et le backup boot sector + FSINFO + FAT[0], 1 ,2 ?
// Routine pour donner les infos sur la partition en cas de pb avec BootSector
//


// Rechercher des répertoires sur le disque
// Pour tous les clusters
// 4 type d'entrées possible:
// * commence par $E5 => entrée effacée (pas de test)
// * commence par 0 => fin des entrées => dernier cluster du rep
// * Entrée nom court: Vérifier validité( nom, date, cluster, attr,)
//		Si contient '.' et '..' au début => 1er cluster du rep
//		Si contient Volume attr => Root Directory (cluster = )
// * Entrée nom long => verif validité
//

//
// Evenements
//
// Lors d'un Scan d'un repertoire ScanFATDirectory
//	OnFile
//	OnDirectory
//	OnVolume
//
// Lors de la Sauvegarde d'une chaine vers un Fichier
//
//	OnSaveProgress(Sender TObject; const position, size : Cardinal)
//	OnError(Sender TObject; errcode : Cardinal; var action : Cardinal);
//
unit GenericFat;


interface

uses
	Classes, SysUtils, Windows,
    GenericPartition, GenericDrive, CommonFileInfo, FATFileInfo;


const


BootSigValue		= $29;   	// Signature offset 38 ou 66 selon FS du BootSector


// Parametre pour le ReadCluster
READ_ONE_BY_ONE			= $01;	// Toujour actif
CONCAT_VALID_SECTORS	= $02;	// Mets les secteurs valides bout à bouts
FILL_ERR_WITH_PATTERN	= $04;	// Remplit les secteurs defectueux avec une pattern
STOP_FILE_SAVE			= $08;	// 1 si on arrete la sauvegarde d'un fichier en
								// cas d'erreur, sinon rempli avec le pattern
// ErrorCode : Ok, AllSectorBad, SomeSecBad


// Attribut des fichiers/rep ADVSHR
ATTR_READ_ONLY		= $01;
ATTR_HIDDEN			= $02;
ATTR_SYSTEM			= $04;
ATTR_VOLUME_ID		= $08;	// Entrée contenant le nom du volume (RootDir)
ATTR_DIRECTORY		= $10;
ATTR_ARCHIVE		= $20;

// Code d'erreurs pour lors de la lecture d'une cluster chaine
//CHAIN_OK				= $;
//CHAIN_LOOP			= $;
//CHAIN_READ_ERROR		= $;
//INVALID_CLUST_CHAIN	= $;


type


TOnProgressEvent = procedure(Sender: TObject; const filesize, writtenbyte, writtenblock : Cardinal) of Object;

// Mode de lecture de la FAT (priorité);
TFATReadMode = (rmActiveFirst, rmOnlyActive, rmOnlyMirror, rmMirrorFirst);


//
// Classe generique pour la gestion des partitions FAT
//
TFATPartition = class (TGenericPartition)
    private
    protected
        fpRsvdSecCnt	: Word;			// Taille en secteur de la region reservée
        fpVolumeID		: Cardinal;		// ID du volume
        fpVolumeLabel	: string;  		// Label du Volume
        FOEMName		: string;		// Nom OEM de la partition
        FFilSysType		: string;		// Champ FilSysType
        fpFATSize		: Cardinal;		// Taille de la FAT EN SECTEURS !!!
        fpFATCount		: Word;			// Nombre de FATs
        fpMirrorFAT		: boolean;		// True si la FAT est mirroré
        fpMedia			: Byte;			// Type de média, répete dans FAT[0]
        fpFSVerMajor	: Byte;			// Majeur du numéro de version
        fpFSVerMinor	: Byte;         // Mineur du numéro de version
        fpFATEntrySize	: Word;			// Taille d'une entrée dans la FAT (ne marche pas pour FAT12)
        FHiddenSectors	: Cardinal;		// Count of hidden sectors preceding the partition that contains this FAT volume
        // ------------------------ Valeurs calculées ---------------------------
        fpMaxClusterPerDir	: Cardinal;	// Taille max d'un repertoire en cluster (FAT)
        // -------------------------- Flag Lecture FAT ------------------------
        FFATReadFlag	: TFATReadMode;	// Priorité de lecture FAT
        // ----------------------------- Evenements ----------------------------
        FOnVolume		: TDirectoryScanEvent;	// Appellé lors d'une entrée Volume dans ScanDir
        //fpOnSaveError	: TScanErrorEvent;
        FDirectoryStream: TMemoryStream;
        // ============================= Methodes =============================
        procedure SetFATReadFlag(mode: TFATReadMode);
        // ------------------ Gestion des entrées FAT ------------------------
        function GetFATEntry(const indice: Cardinal): Cardinal; virtual; abstract;
        function isFreeCluster(const Cluster: Cardinal): boolean; virtual; abstract;
        function isLastCluster(const Cluster: Cardinal): boolean; virtual; abstract;
        function isBadCluster(const Cluster: Cardinal): boolean; virtual; abstract;
        procedure LoadDirectoryStream(const dircluster: Cardinal); virtual;
        function ScanFATDirectoryNew(const cluster: Cardinal): boolean;
        //
        function GetClusterChain(const firstclust, maxclust: Cardinal; clustlist : TList): boolean; virtual;
        function GetFileClusterChain(const firstclust: Cardinal; const FileSize: int64; clustlist : TList): boolean; virtual;
        function SaveChainToFile(fic: TStream; cluslist: TList; const FileSize : int64):boolean;
    public
        // ============================= Methodes =============================
		class function isValidBootSector(const buffer: Pointer): boolean; override;
		procedure GetPartitionProperties(info: TStrings); override;
    	// ----------------- Propriétés de la partition -------------------
        property OEMName 		: string read FOEMName;
//        property TotalSectors	: Cardinal read fpTotalSectors;
        property VolumeLabel	: string read fpVolumeLabel;
        property VolumeID		: Cardinal read fpVolumeID;
        property ReservedSecCnt	: Word read fpRsvdSecCnt;
        property FATCount		: Word read fpFATCount;
        property FATSize		: Cardinal read fpFATSize;
        property MirrorFAT		: boolean read fpMirrorFAT;
        property HiddenSectors	: Cardinal read FHiddenSectors;
        // -----------------------
        property FATReadFlag: TFATReadMode read FFATReadFlag write SetFATReadFlag;
        // ------------------------ Call Back ------------------------------
        property OnVolume : TDirectoryScanEvent read FOnVolume 	write FOnVolume;
        //property OnRestoreError : read write;
        // ============================= Methodes =============================
        constructor Create(disque : TGenericDrive; const firstsect : Cardinal); override;
        destructor Destroy; override;
        //
        function ScanRelativeDirectory(const dirid: int64): boolean; override;
		function RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean; override;
end;


const
    FATEntryReadError	= $10000000;	// Erreur lors de la lecture de la FAT contenant l'entrée recherchée


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation


uses
	ConversionUnit;

Const

// Flag pour la recherche des entreés d'un repertoire
ATTR_LONG_NAME		= ATTR_READ_ONLY or ATTR_HIDDEN or ATTR_SYSTEM or ATTR_VOLUME_ID;
ATTR_LONG_NAME_MASK	= ATTR_READ_ONLY or ATTR_HIDDEN or ATTR_SYSTEM or ATTR_VOLUME_ID or ATTR_DIRECTORY or ATTR_ARCHIVE;
LAST_LONG_ENTRY		= $40;		// Marqueur de fin (début) d'une LFN


type


TGenFATBootSector = packed record
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
    // ------ Partie spécifique et Code ------
    BS_Dummy		: array[1..474] of Byte;	// Padding
    BS_BootRecordSig: Word;			// Contient $AA55
end;
PGenFATBootSector = ^TGenFATBootSector;


function isValidDirectoryStream(stream: TMemoryStream): boolean; forward;


// ========================================================================
//								FAT Generic
// ========================================================================


// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Crée une partition générique
//
// Parametres :
//	disque		Objet disque physisque qui contient la partition
//	firstsect	Numéro de secteur sur le disque physisque ou commence la partition (BootSector)
//
constructor TFATPartition.Create(disque : TGenericDrive; const firstsect : Cardinal);
begin
	inherited;
    
	Self.FFATReadFlag := rmActiveFirst;
    Self.fpMinValidClustNum := 2;
    Self.fpMaxValidClustNum := 2;

    Self.FDirectoryStream := TMemoryStream.Create;	// Stream temp pour le scan dir
end;


//
// Destructeur
//
destructor TFATPartition.Destroy;
begin
	inherited;

    Self.FDirectoryStream.Free;
end;


procedure TFATPartition.SetFATReadFlag(mode: TFATReadMode);
begin
    //#Todo2 Gerer le cas 1 seule FAT
	Self.FFATReadFlag := mode;
end;


// ----------------------------------------------------------------------
//						Gestion du bootsector
// ----------------------------------------------------------------------


//
// Verifie si cela peut être une partition FAT
//
class function TFATPartition.isValidBootSector(const buffer: Pointer): boolean;
var
	bs: PGenFATBootSector;
begin
	Result := true;
    
    // Verification des champs communs à FAT16/32
    bs := PGenFATBootSector(buffer);

    // $AA55 signature
    if bs.BS_BootRecordSig <> BootRecordSigValue then
        Result := false;

    if (bs.BS_jmpBoot[0] <> $EB) and (bs.BS_jmpBoot[0] <> $EB) then
        Result := false;

    if not (bs.BPB_SecPerClus in [1, 2, 4, 8, 16, 32, 64, 128]) then
        Result := false;

    if bs.BPB_RsvdSecCnt = 0 then
        Result := false;

    if bs.BPB_NumFATs < 1 then
        Result := false;
end;


// ----------------------------------------------------------------------
//						Gestion des Cluster Chain
// ----------------------------------------------------------------------


//
// Récupère la Cluster Chain d'un fichier de taille connue
//
function TFATPartition.GetFileClusterChain(const firstclust: Cardinal;const FileSize: int64; clustlist: TList): boolean;
begin
	// On calcule le nombre max de cluster grace à la taille du fichier
	Result := Self.GetClusterChain(firstclust, Self.SizeToCluster(FileSize), clustlist);
end;


//
// Du fait de la limite sur le nombre de clusters, én evite un eventuel problème
// d'entrées de FAT bouclant
//
// CHAIN_LOOP, CHAIN_OK, CHAIN_READ_ERROR, INVALID_CLUST_CHAIN
//
function TFATPartition.GetClusterChain(const firstclust, maxclust : Cardinal; clustlist : TList): boolean;
var
	curclust : Cardinal;
begin
    Result := true;

    // Si pas de cluster associé, inutile d'aller plus loin
    if maxclust = 0 then
    	exit;

    curclust := firstclust;
	repeat
        // Verifier si on a atteint maxcluster
        if clustlist.Count >= maxclust then
        begin
            // Erreur des clusters en trop ou on boucle
            // Pour le savoir il faut vérifier si doublon dans la liste
            // CHAIN_LOOP
            result := false;
            break;
        end;

        clustlist.Add(Pointer(curclust));
        curclust := Self.GetFATEntry(curclust);

        // Problème lors de la lecture de l'entrée ?
        if curclust = FATEntryReadError then
        	//CHAIN_READ_ERROR
        	break;

        if Self.isLastCluster(curclust) then
        	//CHAIN_OK
        	break;

        // S'il y a une erreur on arrete et on sort
        if Self.isBadCluster(curclust) or Self.isFreeCluster(curclust)
        or (curclust > Self.fpMaxValidClustNum) or (curclust < 2) then
        begin
        	//INVALID_CLUST_CHAIN
            // Erreur num de cluster non valide
            result := false;
            break;
        end;
    until false;
end;


function TFATPartition.RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean;
var
	ClusterList: TList;
begin
	ClusterList := TList.Create;

    //#Todo2 Gerer l'erreur
    Self.GetFileClusterChain(TFATFileInfo(finfo).FirstCluster, finfo.Size, ClusterList);
	//On Progress dans le SaveChainToFile ? 
	Result := Self.SaveChainToFile(stream, ClusterList, finfo.Size);
    ClusterList.Free;
end;


// ----------------------------------------------------------------------
//						Gestion des DirectoryStream
// ----------------------------------------------------------------------


//
// Charge le mémorystream avec le contenu du répertoire associé à dircluster
//
procedure TFATPartition.LoadDirectoryStream(const dircluster: Cardinal);
var
    cluslist: TList;
    i : smallint;
begin
	// initialisation
    cluslist:= Tlist.Create;
    cluslist.Capacity := 32;	// Pour éviter d'avoir trop de reallocation

    Self.FDirectoryStream.Clear;

	// Récuperer le liste des cluster contenant le répertoire
    Self.GetClusterChain(dircluster, Self.fpMaxClusterPerDir, cluslist);
    Self.FDirectoryStream.SetSize(cluslist.Count * Self.ClusterSize);

    // Remplir les secteurs invalides avec $E5 + Attr pour shortentry
    Self.BadSecPattern := $E52020E5;
    Self.ReadErrorFlag := caFillInvalid;

    for i := 0 to cluslist.Count - 1 do
    begin
    	Self.ReadCluster(Cardinal(cluslist[i]));
        Self.FDirectoryStream.Write(Self.fpClusterBuffer[0], Self.ClusterSize);
    end;

    cluslist.Free;
    
    // Revient au début du stream
    Self.FDirectoryStream.Seek(0, soFromBeginning);
end;


// ----------------------------------------------------------------------
//						Gestion des DirectoryEntry
// ----------------------------------------------------------------------


//
//	ChkSum()
//	Returns an unsigned byte checksum computed on an unsigned byte
//	array.  The array must be 11 bytes long and is assumed to contain
//	a name stored in the format of a MS-DOS directory entry.
//	Passed:	 pFcbName    Pointer to an unsigned byte array assumed to be
//                          11 bytes long.
//	Returns: Le checksum du nom de fichier/repertoire court (nom+extension)
//			 pointe par name
//
function CheckSum(name : PChar) : Byte;
var
	i : smallint;
begin
    Result := 0;
    for i := 0 to 10 do
    begin
    	if (Result and 1) = 1 then
        	Result := $80 + (Result shr 1) + Byte(name[i])
        else
        	Result := (Result shr 1) + Byte(name[i]);
    end;
end;


//
// Récupère la chaine dans l'entrée LFN
//
function GetLongName(entry : TFATDirEntryLFN):String;
var
	strtmp : string;
begin
    WideCharLenToStrVar(entry.Name1, 5, strtmp);
    Result := strtmp;
    WideCharLenToStrVar(entry.Name2, 6, strtmp);
    Result := Result + strtmp;
    WideCharLenToStrVar(entry.Name3, 2, strtmp);
    Result := Result + strtmp;
end;


//
// Version avec les stream
//
// Scan un fichier contetant une FAT Directory Structure
// Déclenche les evenements OnFile, OnDirectory, OnVolume
//
// Parametres:
//  cluster = 1er cluster de la FAT Directory Structure
//
// Retourne:
//	Vrai si ok, faux si erreur (Err E/S, chaine invalide,...)
//#Todo Renomer
function TFATPartition.ScanFATDirectoryNew(const cluster: Cardinal): boolean;
var
    CurEntry: TFATFileInfo;
	direntry: TFATDirEntry;
    lfnentry: PFATDirEntryLFN;
    curname, longname : string;
//    i : smallint;
    endofdir : boolean;
    numpart, curchecksum : Byte;
    abortscan, modelfn, longentry : boolean;
begin
	abortscan:= false;
    endofdir := false;
    modelfn  := false;
    longentry:= false;
    longname := '';

    //
    lfnentry := @direntry;

	Result := false;
	// Récuperer le DirectoryStream
    Self.LoadDirectoryStream(cluster);
	if not isValidDirectoryStream(Self.FDirectoryStream) then
    	exit;
        
	result := true;
    // Analyse les entrées jusqu'à la fin du stream (Exception)
    while true do
        try
            Self.FDirectoryStream.ReadBuffer(direntry, Sizeof(TFATDirEntry));

            // Fin des entrées ?
            if lfnentry.Part = 0 then
            begin
                endofdir := true;
                break;
            end;

            if ((lfnentry.Attributes and ATTR_LONG_NAME_MASK) = ATTR_LONG_NAME)
            and (lfnentry.Part <> $E5) then
            begin
	            // On a trouvé une entrée nom long
                if (lfnentry.Part and LAST_LONG_ENTRY) = LAST_LONG_ENTRY then
                begin
                	// Marqueur de Fin (début) détecté
                	modelfn := true;
                    curchecksum := lfnentry.Checksum;
                    longname := GetLongName(TFATDirEntryLFN(direntry));
                    numpart := lfnentry.Part and $1F;
                    // calculer le nombre de parts restantes
                    dec(numpart);
                end
                else
                begin
                	// vérifier si suite de la précédente LFN entry
                	if modelfn and (lfnentry.Checksum = curchecksum)
                    and ((lfnentry.Part and $1F) = numpart) then
                    begin
                    	longname := GetLongName(TFATDirEntryLFN(direntry)) + longname;
                        dec(numpart);
                    end
                    else
                    begin
                    	// Erreur, passer les précédentes LFN
						longname := '';
                        modelfn := false;
                    end;
                end;

                // Vérifier si fin des LFN Part
                if numpart = 0 then
                	longentry := true;	// une LFN est prete
            end;

            // Une entrée standard ?
            if ((lfnentry.Attributes and ATTR_LONG_NAME_MASK) <> ATTR_LONG_NAME)
            and (lfnentry.Part <> $E5) then
            begin
				// Traitement pour le cas ou un nom long est prêt
                if longentry and (curchecksum = Checksum(direntry.FileName)) then
                begin
                	//filedir.Name := longname;	// Met à jour avec le nom long
                    SetString(CurName, PChar(longname), StrLen(PChar(longname)));
                    modelfn:= false;
                    longentry := false;
                    longname := '';
                end
                else
                	CurName := '';

           	    //ATTN Deporter la créatioon dans le OnFile/Dir/Volume ???
            	// Extrait les infos de l'entrée FAT
                CurEntry := TFATFileInfo.Create(int64(cluster));
                CurEntry.LoadFromDirEntry(CurName, @direntry);

                // Selon le type d'entrée
                case lfnentry.Attributes and (ATTR_DIRECTORY or ATTR_VOLUME_ID) of
                    0 : begin
                        // Found a file.
                        if assigned(Self.OnFile) then
                            Self.OnFile(Self, CurEntry, abortscan)
                        else
                        	CurEntry.Free;
                    end;

                    ATTR_DIRECTORY: begin
                        // Found a directory
                        // Les '.' et '..' sont envoyés aussi
                        if Assigned(Self.OnDirectory) then
                            Self.OnDirectory(Self, CurEntry, abortscan)
                        else
                        	CurEntry.Free;
                    end;

                    ATTR_VOLUME_ID: begin
                        // Found a volume label.
                        if Assigned(Self.OnVolume) then
                            Self.OnVolume(Self, CurEntry, abortscan)
                        else
                        	CurEntry.Free;
                    end;
                else
                    // Found an invalid directory entry.
                    CurEntry.Free;
                end; // case of
            end; //if

            // On a demandé un arrêt du scan ???
            if abortscan then
            	exit;
        except
            on EReadError do exit;	// Fin du DirectoryStream
        end; // try
end;


//
//
//
function TFATPartition.ScanRelativeDirectory(const dirid: int64): boolean;
begin
	Result := Self.ScanFATDirectoryNew(Cardinal(dirid));

    Self.FDirectoryStream.Clear;
end;


//
// Parametres :
// Stream
// Filesize
// 1er cluster ou liste des cluster
//
// Le FileSize et le nombre de clusters dans la liste DOIVENT être cohérents !!!
// Le fichier de taille nulle (0 cluster) sont supportés
//
function TFATPartition.SaveChainToFile(fic: TStream; cluslist: TList; const FileSize: int64):boolean;
var
	i : smallint;
    blocksize, writesize : Cardinal;
    //action : word;
begin
    blocksize := Self.ClusterSize;		// Taille du block à écrire dans le stream
    writesize := 0;
    Result := true;

	// Si pas de cluster dans le list, on ne fait rien
    for i := 0 to cluslist.Count - 1 do
    begin
    	if Self.ReadCluster(Cardinal(cluslist[i])) <> (Self.ClusterSize div Self.SectorSize) then
        begin
            // Gerer les erreurs : Continuer, stopper DISK_READ_ERROR
            //if Assigned(OnScanError) then
            //	Self.OnScanError(Self, error, action);
            // Action = STOP, CONCAT (modif du ReadCluster), USE_PATTERN

            //if (action = ??) then
            Result := false;
        	break;
        end;

        // Dernier cluster ?
        if (i = cluslist.Count - 1) then
        	blocksize := FileSize mod Self.ClusterSize;

        // en essaie d'ecrire dans le stream
        try
            fic.WriteBuffer(Self.fpClusterBuffer[0], blocksize);
        except
			on EWriteError do begin
                // Pas d'action, on sort de la procedure
                // mettre un retour d'erreur Erreur fichier FILE_WRITE_ERROR
                Result := false;
                break;
            end;
        end; // try

        // Mettre un appel OnProgress ? (sauvegarder le nb octets écris)
        writesize := writesize + blocksize;
        if Assigned(OnRestore) then
        	Self.OnRestore(self, int64(FileSize), int64(writesize));
    end; // for
end;


// ----------------------------------------------------------------------
//					Routine de gestion des TimeStamp
// ----------------------------------------------------------------------


//
// Renseigne le Tstrings avec les infos communes FAT16/32
//
procedure TFATPartition.GetPartitionProperties(info: TStrings);
var
    chs : TCHS;
begin
    Self.fpPhysicalDrive.LBAToCHS(chs, Self.FirstSecOfPart);

    // Verifier si ces info sont disponibles (PartInfo)
    if (Self.FInfoFlag and PART_INFO_AVAIL_FLAG) <> 0 then
    begin
        info.Add(PARTITIONTYPE_STR);
        info.Add(IntToHex(Self.PartitionType, 2));

        info.Add(BOOTABLE_STR);
        if Self.ActivePart then
            info.Add('Yes')
        else
            info.Add('No');

        info.Add(PARTITIONSTATUS_STR);
        if Self.PrimaryPartition then
            info.Add(PRIMARY_STR)
        else
            info.Add(LOGICAL_STR);
    end;
        
    info.Add(PARTSTARTAT_STR);
    info.Add(Format('%d-%d-%d (CHS)',[chs.Cylinder, chs.Head, chs.Sector]));
    info.Add('');
    info.Add(Format('%d (LBA)',[Self.FirstSecOfPart]));
    info.Add(PARTITIONSIZE_STR);
    info.Add(SizeTo2DigitByteString(int64(Self.TotalSectors) * int64(Self.SectorSize)));
    info.Add(TOTALSECTORS_STR);
    info.Add(IntToStr(Self.TotalSectors));

    info.Add(OEMNAME_STR);
    info.Add(Self.OEMName);
    info.Add(VOLUMENAME_STR);
    info.Add(Self.VolumeLabel);
    info.Add(VOLUMEID_STR);
    info.Add(IntToStr(Self.VolumeID));
	info.Add('File System');
	info.Add(Self.FileSysName);
    info.Add(SECTORSIZE_STR);
    info.Add(SizeToRoundedByteString(Self.SectorSize));
    info.Add(CLUSTERSIZE_STR);
    info.Add(SizeToRoundedByteString(Self.ClusterSize));
    info.Add(FATCOUNT_STR);
    info.Add(IntToStr(Self.FATCount));
	info.Add(FATSIZE_STR);
    info.Add(Format('%d secteurs', [self.FATSize]));
	info.Add(RSVDSECTORCOUNT_STR);
    info.Add(IntToStr(Self.ReservedSecCnt));
	info.Add(HIDDENSECTORS_STR);
    info.Add(IntToStr(Self.HiddenSectors));
end;


// ----------------------------------------------------------------------
//						Routine de verification
// ----------------------------------------------------------------------


//#Todo2 Mettre dans unité FATFileInfo ?
// VErif
// Selon attributs: LFN ou normal, appliquer les tests spécifique à chaque entrée
// verif $E5 pour deleted

const
//Date :
//Bits 0-4: Day of month, valid value range 1-31 inclusive.
//Bits 5-8: Month of year, 1 = January, valid value range 1-12 inclusive.
//Bits 9-15: Count of years from 1980, valid value range 0-127 inclusive (1980-2107).
DATE_MONTH_MASK	= $001F;
DATE_DAY_MASK	= $01E0;
DATE_YEAR_MASK	= $FE00;
DATE_DAY_SHR	= 5;
DATE_YEAR_SHR	= 9;

//Time :
//Bits 0-4: 2-second count, valid value range 0-29 inclusive (0 - 58 seconds).
//Bits 5-10: Minutes, valid value range 0-59 inclusive.
//Bits 11-15: Hours, valid value range 0-23 inclusive.
TIME_SEC_MASK	= $001F;
TIME_MIN_MASK	= $07E0;
TIME_HOUR_MASK	= $F800;
TIME_MIN_SHR	= 5;
TIME_HOUR_SHR	= 11;


//
// Verifie si la date/here est valide
//
function isDateTimeValid(const date, time : Word): boolean;
begin
     Result := false;

     if not (date and DATE_MONTH_MASK) in [1..12] then
     	Exit;
     if not ((date and DATE_DAY_MASK) shr DATE_DAY_SHR) in [1..31] then
     	Exit;
     // Toutes les valeurs sont valides pour l'année

     if not (time and TIME_SEC_MASK) in [0..29] then
     	Exit;
     if not ((time and TIME_MIN_MASK) shr TIME_MIN_SHR) in [0..59] then
     	Exit;
     if not ((time and TIME_HOUR_MASK) shr TIME_HOUR_SHR) in [0..23] then
     	Exit;

     // Tous les tests sont ok
     Result := true;
end;

//cf. fatgen103.doc
//The following characters are not legal in any bytes of DIR_Name:
//·	Values less than 0x20 except for the special case of 0x05 in DIR_Name[0] described above.
//·	0x22, 0x2A, 0x2B, 0x2C, 0x2E, 0x2F, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x5B, 0x5C, 0x5D, and 0x7C.

//Short names are limited to 8 characters followed by an optional period (.)
// and extension of up to 3 characters.  The characters may be any combination
//of letters, digits, or characters with code point values greater than 127.
//  The following special characters are also allowed:
// $   %   '   -   _   @   ~    `   !   (    )   {   }  ^  #  &
//
// Autorisé dans les noms long
// +   ,   ;   =   [   ] .
//
function isValidShortName (nameendext : Pchar) : boolean;
var
	i : smallint;
begin
    for i := 0 to 10 do
    begin
        Result := false;
    	if nameendext[i] in ['0'..'9', 'A'..'Z', 'a'..'z'] then
        	Result := true;

        if nameendext[i] in ['$','%','''','-','_','@','~','`','!','(',')','{','}','^', '#', '&'] then
        	Result := true;

        //#Todo2 verifier la validité
        if (nameendext[i] > Char(127)) or ((nameendext[i] = #05) and (i = 0)) then
        	Result := true;

        if (i > 0) and (nameendext[i] = ' ') then
        	Result := true;

        // Char valide ? sinon on sort et renvoie false
        if not result then
        	exit;
    end;
end;


//
// Est une entrée Long File Name valide ?
//
function isValidLFN(const plfn: PFATDirEntryLFN): boolean;
begin
	Result := false;

    // Bits 7 et 5 reservés ? (http://home.teleport.com/~brainy/lfn.htm)
    // Retirer flag first lfn et vérifier nb lfn <= 20
    if (plfn.Part and $1f) > 20 then exit;

    if plfn.LFN_FstClusLO <> 0 then exit;

	// Tests concluants
    Result := true;
end;


//
// Verifie si le Main Dir Entry est valide
//
function isValidDirEntry(const pdir: PFATDirEntry): boolean;
begin
	Result := false;

    // Vérif du nom
    //if not isValidShortName(pdir.FileName) then exit;
    
    // Verif date et heures
    if not isDateTimeValid(pdir.DateCreated, pdir.TimeCreated) then exit; 

    if not isDateTimeValid(pdir.Date, pdir.Time) then exit;
    
    if not isDateTimeValid(pdir.DateAccessed, 0) then exit;

	// Tests concluants
    Result := true;
end;


//
// VErifie que le stream contient des DirNetries valides
//
function isValidDirectoryStream(stream: TMemoryStream): boolean;
var
	dirent: TFATDirEntry;
begin
	Result := true;

    while Result do
    	try
            stream.ReadBuffer(dirent, sizeof(TFATDirEntry));

            if (dirent.Attributes and ATTR_LONG_NAME_MASK) = ATTR_LONG_NAME then
                Result := isValidLFN(PFATDirEntryLFN(@dirent))
            else
            	if (dirent.FileName[0] <> #$E5) then
                	Result := isValidDirEntry(@dirent);
        except
            on EReadError do break;	// Fin du DirectoryStream
        end; // try

    stream.Position := 0;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
