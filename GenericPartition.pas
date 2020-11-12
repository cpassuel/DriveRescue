{-----------------------------------------------------------------------------
 Unit Name: GenericPartition
 Author:    Chris
 Purpose:   Classe abstraite pour gerer les partitions
 History:
 16/07/2016 : Added generic GetFirstSectorOfCluster function
 25/04/2005
-----------------------------------------------------------------------------}


unit GenericPartition;


interface


uses
	Classes, SysUtils, Windows,
    //
    GenericDrive, CommonFileInfo;


const

BootRecordSigValue	= $AA55; 	// Signature offset 510,511 du BootSector

MaxCardinal			= 4294967295;	// Valeur max d'un Cardinal

PRIVATE_ERROR_CODE_MASK	= $10000000;	// Masque pour les erreurs définies dans l'appli

BufferClustersCount = 256; // Nombre de clusters pour le buffer RestoreFile

type

// Type buffer dynamique
PBuffer = ^TBuffer ;
TBuffer = array of Byte;

// ------------ Action lors du ReadCluster et ReadClusterToBuffer -------------
// caConcatValid	Contatène tous les secteurs valides à la suite
// caFillInvalid	Les secteurs défectueux sont remplacés par une pattern
// caStopOnError	Exit dès qu'un secteur est défectueux
TReadClusterAction = (caConcatValid, caFillInvalid, caStopOnError);


// Procedure pour les evenements générés par ScanDirectory
//TScanErrorEvent = procedure(Sender : TObject; error : word; var action : word) of object;
TDirectoryScanEvent = procedure(Sender: TObject; entry: TCommonFileInfo; var abort: boolean) of object;
TOnRestoreProgressEvent = procedure(Sender: TObject; const filesize, writtenbytes: int64) of Object;


//
// Classe générique pour tous les types de partitions FATx, NTFS, Linux,...
//
TGenericPartition = class
    private
    	// ----------- Information récuperées de TPartitionInfo --------------
    	FPartitionType	: Byte;			// Type de la partition http://members.aol.com/bellamyjc/fr/multiboot.html
        FPrimaryPart	: Boolean;		// Partition primaire ou étendue ?
        FActivePart		: Boolean;		// Partition active ?
        procedure SetBadSecPattern(const pattern: Cardinal);
    protected
        FInfoFlag		: Byte;			// Indique les infos disponibles
    	// --------------------- Gestion du cache et E/S ----------------------
        fpSectorBuffer	: array of Byte;// Cache pour un secteur (utilisé pour la FAT)
        fpCurrentSector	: Cardinal;		// Numéro du secteur dans le buffer ou MaxCardinal
        fpCurrentSecErr : boolean;		// Indique une erreur sur le secteur en cache
        fpSecLastError	: Cardinal;		// Stocke le LastError du cache
        fpClusterBuffer : array of Byte;// Cache pour un cluster
        fpCurrentCluster: Cardinal;		// Numéro du cluster dans le buffer ou MaxCardinal
        fpCurValidSector: Smallint;		// Nombre de secteurs valid dans le cache
        fpCurLastClusErr: Cardinal;		// LastError du cache
        fpBadSecPattern	: Cardinal;		// Pattern qui sera ecrite dans les secteurs en erreur du cluster
        // ------------------------
    	fpPhysicalDrive: TGenericDrive;// Disque physique qui contient la partition
        fpFirstSector:		Cardinal;	// Numéro du premier secteur de la partition sur le disque
        fpTotalSectors:		Cardinal;	// Nombre de secteur pour la partition != volume en NTFS
        fpSectorSize:		Cardinal;	// taille du secteur en octets (à partir du disque)
		fpClusterSize:		Cardinal;	// taille du Cluster en OCTETS !!!!
        // ------------------------ Valeurs calculées ---------------------------
        fpMaxValidClustNum:	Cardinal;	// Num max de cluster valide
        fpMinValidClustNum:	Cardinal;	// Num min de cluster valide (2 pour FAT)
        // ---------------------- Gestion des erreurs ------------------------
        FLastError:			Cardinal;		// Contient la dernière erreur rencontrée
        fpReadErrorFlag:	TReadClusterAction;	// Indique le comportement de ReadCluster en cas d'erreur
        // ----------------------------- Evenements ----------------------------
        FOnFile:		TDirectoryScanEvent;	// Appellé lors d'une entrée fichier dans ScanDir
        FOnDirectory:	TDirectoryScanEvent;	// Appellé lors d'une entrée repert dans ScanDir
        FOnRestore:		TOnRestoreProgressEvent;// Appellé lors de la restauration de fichiers
        // ------------------------ Gestion du cache -----------------------
        procedure FlushSectorCache;
        procedure FlushClusterCache;
		procedure FillBufferWithPattern(buf: PByteArray; const size: Cardinal);
        // ------------------ E/S ----------------
        function ReadSector(const sectornum: Cardinal): boolean; virtual;
        function ReadCluster(const clusternum: Cardinal): word; virtual; // mettre plus bas ?
        function GetFirstSectorOfCluster(const n: Cardinal): Cardinal; virtual; // override for specific cluster management
        // -----------------
        function GetSectorPerCluster: Cardinal; virtual;
        function GetInfoFromBootSector(const buf: Pointer): boolean; virtual; abstract;
        function GetFileSystemName: String; virtual; abstract;
        function GetFileSystemFamily: String; virtual; abstract;
    public
        // ============================= Methodes =============================
        constructor Create(disque: TGenericDrive; const firstsect: Cardinal); virtual;
        destructor Destroy; override;
        procedure GetPartitionProperties(info: TStrings); virtual; abstract;
        // ----------------------- Recupère les infos part -----------------
        class function isValidBootSector(const buffer: Pointer): boolean; virtual; abstract;
        //#TODO2 methode de classe pour récupérer la taille par défaut des cluster selon taille disque
        class function GetDefaultClusterSize(const dsize: int64): Cardinal; virtual; abstract;
        function LoadFromPartInfo(info: PPartitionInfo): boolean; virtual;
		function LoadFromLBA(const lba:Cardinal): boolean; virtual; abstract; // Descendre dans Generic FAT
		// ----------------------- Methodes E/S --------------------------------
        function ReadSectorToBuffer(const sectornum: Cardinal; buf: PByteArray): boolean;
        function SizeToCluster(const filesize: int64): Cardinal;
        function ReadClusterToBuffer(const clusternum: Cardinal; buf: PByteArray; const act : TReadClusterAction): Word;
        function ReadClusterToStream(const clusternum: Cardinal; stream: TStream): Word;
        function ReadClustersToBuffer(const clusternum: Cardinal; const clustercount: Cardinal; buf: PBuffer): Word;
        // Directory scan methods
        function ScanRootDirectory(): boolean; virtual; abstract;
        function ScanRelativeDirectory(const dirid: int64): boolean; virtual; abstract;
		function RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean; virtual; abstract;
        // ============================= Propriétés =============================
    	// ----------------- Propriétés de la partition -------------------
        property FirstSecOfPart: Cardinal	read fpFirstSector;
        property SectorSize: Cardinal		read fpSectorSize;
        property SectorPerCluster: Cardinal read GetSectorPerCluster;
        property ClusterSize: Cardinal		read fpClusterSize;	// Taille du cluster en OCTET
        property PartitionType: Byte 		read FPartitionType;// O si la partition est crée sans entrée de partition
		property PrimaryPartition: Boolean	read FPrimaryPart;	// Partition primaire ou étendue ?
        property ActivePart: boolean  		read FActivePart;	// Partition active ?
        property TotalSectors: Cardinal		read fpTotalSectors;// Nombre de secteur dans la partition
        property FileSysName: string		read GetFileSystemName;
        property FileSystemFamilyName: string read GetFileSystemFamily;
        // ---------------- Propriétés gestion des erreurs -------------------
        property LastError: Cardinal read FLastError;
		property BadSecPattern: Cardinal read fpBadSecPattern write SetBadSecPattern;
        property ReadErrorFlag: TReadClusterAction read fpReadErrorFlag write fpReadErrorFlag;
        // ------------------ Info generique sur la partition --------------
        property MaxValidClust: Cardinal read fpMaxValidClustNum;
        property MinValidClust: Cardinal read fpMinValidClustNum;
        // ------------------------ Call Back ------------------------------
        property OnFile: TDirectoryScanEvent read FOnFile write FOnFile;
        property OnDirectory: TDirectoryScanEvent read FOnDirectory write FOnDirectory;
        property OnRestore: TOnRestoreProgressEvent	read FOnRestore	write FOnRestore;
end;


// Flags pour indique les infos disponibles
const
PART_INFO_AVAIL_FLAG	= $01;	// Mis si les info PartInfo sont dispo (SysInd, bootable, Primary)


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Arguments:
// disque : TGenericDrive;
// firstsect : Cardinal
// Result:    None
//
constructor TGenericPartition.Create(disque: TGenericDrive; const firstsect: Cardinal);
begin
	Self.fpPhysicalDrive := disque;
    Self.fpSectorSize := disque.BytesPerSec;
    Self.fpFirstSector := firstsect;

    Self.fpBadSecPattern := $DEADBEEF;
    Self.fpReadErrorFlag := caFillInvalid;

    // Allouer le cache secteur
    SetLength(Self.fpSectorBuffer, Self.fpSectorSize);

    // Mets un nombre hors limite pour specifier cache vide
    Self.FlushSectorCache;
    Self.FlushClusterCache;

    // Initialisation à 0 pour limiter les problèmes
    Self.fpMinValidClustNum := 0;
    Self.fpMaxValidClustNum := 0;
end;


//
// Libère la partition
//
destructor TGenericPartition.Destroy;
begin
end;



// --------------------------------------------------------------------------
//					Gestion des clusters
// --------------------------------------------------------------------------

//
// by default cluster 0 begins at sector 0, so cluster n = sector * SectorPerCluster
//
function TGenericPartition.GetFirstSectorOfCluster(const n: Cardinal): Cardinal;
begin
    Result := Self.SectorPerCluster * n;
end;


//
// Retourne le nombre de secteurs par cluster.
//
function TGenericPartition.GetSectorPerCluster: Cardinal;
begin
	Result := Self.fpClusterSize div Self.fpSectorSize;
end;


// ----------------------------------------------------------------------
//							Gestion des propriétés
// ----------------------------------------------------------------------



//
//
//
procedure TGenericPartition.SetBadSecPattern(const Pattern: Cardinal);
begin
	if pattern <> Self.fpBadSecPattern then
    begin
        // Vide les caches
        Self.FlushSectorCache;
        Self.FlushClusterCache;

        Self.fpBadSecPattern := pattern;
    end;
end;



//
// Renvoie le nombre de clusters occupés sur la partition par un fichier de
// taille filesize
//
function TGenericPartition.SizeToCluster(Const filesize : int64): Cardinal;
begin
    Result := (filesize + int64(Self.fpClusterSize) - 1) div int64(Self.fpClusterSize);
end;


// ----------------------------------------------------------------------
//							Gestion du cache
// ----------------------------------------------------------------------


//
// Vide le cache secteur
//
procedure TGenericPartition.FlushSectorCache;
begin
	Self.fpCurrentSector := MaxCardinal;
end;


//
// Vide le cache cluster
//
procedure TGenericPartition.FlushClusterCache;
begin
	Self.fpCurrentCluster := MaxCardinal;
end;


//
// Remplit un buffer avec une pattern;
//
//procedure FillBufferWithPattern(buf: PByteArray; const size, pattern: Cardinal);
procedure TGenericPartition.FillBufferWithPattern(buf : PByteArray; const size : Cardinal);
var
	i : smallint;
    bufcard : array of Cardinal;
begin
	bufcard := @buf[0];
    for i := 0 to (size div 4) - 1 do
    	bufcard[i] := Self.fpBadSecPattern;
end;


// ----------------------------------------------------------------------
//						Methodes pour les E/S
// ----------------------------------------------------------------------


//
// Procedure: TFATPartition.ReadSectorToBuffer
// Purpose:   Lit un secteur de la partition dans un buffer
//
// Arguments: sectornum	Numéro de secteur sur la partition (0 pour le 1er sect)
//			  buf		buffer pour recevoir le secteur
// Result:    boolean	True si pas d'erreur, sinon le code est dans GetLastError
//
function TGenericPartition.ReadSectorToBuffer(const sectornum: Cardinal; buf: PByteArray): boolean;
begin
	// On lit 1 secteur
    Result := Self.fpPhysicalDrive.ReadSectors(Self.fpFirstSector + sectornum, 1, buf);
end;


//
// Procedure: TGenericPartition.ReadSector
// Purpose:	  Lit un secteur et le mets dans le cache
//
// Arguments: sectornum	Numéro de secteur sur la partition (0 pour le 1er sect)
//
// Result:    boolean	False si erreur, le code d'erreur est dans LastError
//
function TGenericPartition.ReadSector(const sectornum : Cardinal): boolean;
begin
	// Verification si secteur dans le cache
    if sectornum <> Self.fpCurrentSector then
    begin
    	// Pas dans le cache, dont on lit sur le dsique
        Self.fpCurrentSecErr := Self.ReadSectorToBuffer(sectornum, @Self.fpSectorBuffer[0]);

        // Mets à jour les infos du cache
        Self.fpSecLastError := GetLastError;
      	Self.fpCurrentSector := sectornum;
    end;

    // Récupère les infos liés au cache
    Self.FLastError := Self.fpSecLastError;
    Result := Self.fpCurrentSecErr;
end;



{-----------------------------------------------------------------------------
  Procedure: TFATPartition.ReadCluster
  Author:    Chris
  Date:      16-mai-2004
  Arguments: const clusternum : Cardinal
  Result:    boolean
-----------------------------------------------------------------------------}
// Pour mettre dans le cache, il faut memoriser
//	Nb secteur valide
//	LastError
//	Num Cluster
// Une modif des parametres de gestion des erreur devrait obliger à vide le cache
// Retourne: Le nombre de secteurs réellement lus.
function TGenericPartition.ReadCluster(const clusternum : Cardinal) : word;
{var
	sector : Cardinal;
    nbvalid : word;}
begin
    // Vérif si cluster dans le cache
    if clusternum <> Self.fpCurrentCluster then
    begin
    	// Pas dans le cache => il faut le lire et le mettre dans le cache
        Self.fpCurValidSector := Self.ReadClusterToBuffer(clusternum, @Self.fpClusterBuffer[0], Self.fpReadErrorFlag);
		Self.fpCurLastClusErr := Self.FLastError;
        Self.fpCurrentCluster := clusternum;
    end;

    // Récupérer les infos du cache
    Result := Self.fpCurValidSector;
    Self.FLastError := Self.fpCurLastClusErr;
end;


//
// Retourne: Le nombre de secteurs réellement lus.
//
function TGenericPartition.ReadClusterToStream(const clusternum: Cardinal; stream: TStream): Word;
begin
	Result := Self.ReadCluster(clusternum);
    //#Todo2 gestion des erreurs
    stream.WriteBuffer(Self.fpClusterBuffer[0], Self.ClusterSize);
end;


//
// Transfert un cluster dans un buffer
// Parametres :
//
// Retourne: Le nombre de secteurs réellement lus.
//
// Gestion du code d'erreur
//
//#Todo2 mettre en action par défaut
function TGenericPartition.ReadClusterToBuffer(const clusternum : Cardinal; buf : PByteArray; const act : TReadClusterAction) : Word;
var
	sector, offset : Cardinal;
    i : integer;
begin
    Self.FLastError := 0;
    offset := 0;
    Result := 0;

    // retrouver le 1er secteur du cluster
    sector := Self.GetFirstSectorOfCluster(clusternum);

    // Remplir avec la pattern
    //TODO Verifier le contenu du buffer après une erreur E/S
    Self.FillBufferWithPattern(buf, Self.fpClusterSize);

    for i := 0 to (Self.fpClusterSize div Self.fpSectorSize) - 1 do
    begin
        // Lit le cluster (calcule le nb secteur par cluster)
        if not fpPhysicalDrive.ReadSectors(fpFirstSector + sector + i, 1, @buf[offset]) then
        begin
            // Sauver le code d'erreur
            Self.FLastError := GetLastError;

        	if act = caStopOnError then
            	exit;

            // Si pas de Concat
            if act = caFillInvalid then
	            offset := offset + Self.fpSectorSize;
        end
        else
        begin
        	// Pas d'erreur sur le secteur
            inc(Result);
            offset := offset + Self.fpSectorSize;
        end;
    end; // for
end;


//
// Transfert un cluster dans un buffer
// Parametres :
//
// Retourne: Le nombre de secteurs réellement lus.
//
// Gestion du code d'erreur
//
function TGenericPartition.ReadClustersToBuffer(const clusternum: Cardinal; const clustercount: Cardinal; buf: PBuffer): Word;
var
	sector, offset : Cardinal;
begin
    Self.FLastError := 0;
    Result := 0;

    // retrouver le 1er secteur du cluster
    sector := Self.GetFirstSectorOfCluster(clusternum);

    // Remplir avec la pattern
    //TODO Verifier le contenu du buffer après une erreur E/S
    //MODIF 2019 quick Self.FillBufferWithPattern(buf, Self.fpClusterSize * clustercount);
    Result := Self.fpPhysicalDrive.ReadNTSectors(fpFirstSector + sector, clustercount * Self.SectorPerCluster, buf);
    if Result <> clustercount * Self.SectorPerCluster then
    	Self.FLastError := GetLastError;
end;


// ----------------------------------------------------------------------
//						
// ----------------------------------------------------------------------


//
// Récupère les infos à partir d'une entrée de la table des partitions
//
function TGenericPartition.LoadFromPartInfo(info: PPartitionInfo): boolean;
begin
	// Les données Partinfo sont dispo. effacer le flag en cas d'erreur ????
    Self.FInfoFlag := Self.FInfoFlag or PART_INFO_AVAIL_FLAG;

	Self.FPartitionType	:= info.FPartitionType;
    Self.FPrimaryPart	:= info.FPrimaryPartition;
    Self.FActivePart	:= info.FActivePartition;
    Self.fpFirstSector	:= info.FStartingSector;
	Self.fpTotalSectors	:= info.FSectorCount;

    result := true;
    // Le reste est mis à jour par les methodes filles
end;


// --------------------------- Fin de l'unité -----------------------------
end.
