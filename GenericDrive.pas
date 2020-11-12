{-----------------------------------------------------------------------------
 Unit Name: GenericDrive
 Author:    Chris
 Purpose:	Unité abstraite pour les disques durs
 History:   07/02/2005

 07/02/2005 Suppression de pdDriveParam
-----------------------------------------------------------------------------}


unit GenericDrive;


interface

uses
	SysUtils, Classes, Windows, Types, ConversionUnit;
    // Unités propriétaires


const
// Flags sur les opérations réalisées
//#Todo3	Mettre un set plutôt
	SMART_INFO_LOADED_FLAG	= $01;
	PART_INFO_LOADED_FLAG	= $02;

	MAX_PRIMARY_PART =	4;	// Nombre d'entrée dans la table des partition


type


//
// Struture pour l'adressage CHS
//
TCHS = record
	Cylinder	: Word;
    Head		: Word;
    Sector		: Word;
end;


// Structure info d'une partition
//#ATTN Mettre le num de l'EMBR correspondant ????
TPartitionInfo = record
	FPartitionType:		Byte;		// BootID du type de partition
    FActivePartition:	boolean;
    FPrimaryPartition:	boolean;	// True si partition primaire, sinon etendue
	FSectorCount:		Cardinal;	// Nombre de secteurs de la partition
    FStartingSector:	Cardinal;	// Numéro du premier secteur de la partition
    FTypeName:			string;	 	// Nom du Filesystem d'après le BootID
end;
PPartitionInfo = ^TPartitionInfo;


//
// Descripteur de partition contenu dans le MBR et EMBR
//
TPartEntry = packed record
    BootInd:		Byte;	// 0x80 - active
    Head:			Byte;	// starting head
    Sector:			Byte;	// starting sector + ( quelques bits de cylinder)
    Cylinder:		Byte; 	// starting cylinder
    SysInd:			Byte; 	// Type de partition cf. plus loin (BootID)
    LastHead:		Byte;	// end head
    LastSector:		Byte;	// end sector
    LastCylinder:	Byte;	// end cylinder
    RelativeSector: DWORD;	// For primary: Nb Sects between the MBR and the First Sector of the Partition
    						// For Extended: Nb sects between Ext Part and the First Sector of the Partition ???
    NumberSectors:	DWORD;	// nb of sectors in partition
end;


//
// Secteur 0 du disque physique, ou extended part, dans le cas d'un EMBR,
// PartTable[2] et PartTable[3] sont nuls
TMasterBootRecord = packed record
   BootCode  : array[1..446] of Byte;
   PartTable : array[0..MAX_PRIMARY_PART - 1] of TPartEntry;
   Signature : WORD;	// $AA55
end;
PMasterBootRecord = ^TMasterBootRecord;

TFileSystemFamily = (fsUnknown, fsFAT12, fsFAT16, fsFAT32, fsNTFS, fsLinux);


//
// Classe abstraite pour les disques
//
TGenericDrive = class
    protected
    	pdFlags:		Byte;		// Pour savoir si les infos smart sont ok
        //
        pdErrorCode:	Cardinal;	// Contient l'erreur system lors d'une lecture/ecriture
        pdSectorError:	Cardinal;	// numéro du secteur en erreur
        // ------------------------ Infos Geometrie --------------------
        FCylinderCount:	Cardinal;	// Nombre de cylindres
        FHeadCount:		Cardinal;	// Nombre de têtes normalisé (255)
        FSectorPerTrack:Cardinal;	// Nombre de secteurs par piste normalisé (63)
        FBytesPerSec:	Word;		// Taille du secteur en octet
        pdTotalSectors:	Cardinal;	// Nombre total de secteurs sur le disque
        FSectorCount:	int64;		// #Todo utiliser pdTotalSectors ??? 
        FDriveFlags:	Word;		// Info si Fixed,...
        // -------------------------- Infos SMART ----------------------
        pdModelNumber	: String;  	// Numéro de modèle du disque (40 car max) SMART Info
        pdSerialNumber	: String;  	// Numéro de série unique du disque (20 char max) SMART Info
        pdFirmwareRev	: String;  	// Révision du firmaware SMARTINFO
        //
        FPartitionList	: TList;   	// Liste des partitions (ReadMBR)
        // ----------------------- Gestion des propriétés ------------------
        function GetDiskSize: int64;
        function GetModelNumber: string;
        function GetSerialNumber: string;
		function GetFirmwareRev: string;
        function GetPartitionCount: Cardinal;
        function GetPartInfo(ind: Cardinal): PPartitionInfo;
        // -----------------------------------------------------
		procedure GetSmartInfo(); virtual; abstract;
		function ReadExtendedPart(const offset: Cardinal): boolean;
    public
        constructor Create(); virtual;
        destructor Destroy; override;
        function isValidMBR(const buffer: Pointer): boolean;
        function isValidEMBR(const buffer: Pointer): boolean;
        // ------------ E/S --------------
        function ReadSectors(const LBA: Cardinal; nbsect: Byte; buf: Pointer): boolean; virtual; abstract;
        function ReadNTSectors(const lba: Cardinal; nbsect: Cardinal; buf: Pointer): Cardinal; virtual; abstract;
        //
        function ReadMBR: boolean;		// Lit la table des partitions
        // ----------------- Infos ------------------
		class function GetPartTypeName(const sysid: byte): string;
		class function GetFileSystemFamily(const sysid: byte): TFileSystemFamily;
		class function GetFileSystemFamilyName(const sysid: byte): string;
        procedure GetDriveProperties(info: TStrings); virtual;
        // --------------------- Fonctions de conversion -------------------
        function CHSToLBA(const chsadd: TCHS; var lba: Cardinal): boolean;
        function LBAToCHS(var chsadd: TCHS; const lba: Cardinal): boolean;
        // ---------------------- Propriétés ------------------------------
        property LastErrorCode	: Cardinal read pdErrorCode;
        property LastErrorSector: Cardinal read pdSectorError;
        // ------------------- Geometrie -----------------------
        property CylinderCount	: Cardinal read FCylinderCount;
        property HeadCount		: Cardinal read FHeadCount;
        property SectorPerTrack	: Cardinal read FSectorPerTrack;
        property BytesPerSec	: Word read FBytesPerSec;	//SectorSize
        property TotalSectors	: Cardinal read pdTotalSectors;
        property SectorCount	: int64 read FSectorCount;	// Mettre en Cardinal ???
        property Size			: int64 read GetDiskSize;
        // ------------------- Info SMART -----------------------
        property ModelNumber	: string read GetModelNumber;
        property SerialNumber	: string read GetSerialNumber;
        property FirmwareRev	: string read GetFirmwareRev;
        // ------------------ infos sur les partitions -------------------
        property PartitionCount : Cardinal read GetPartitionCount;
        property PartitionInfo[ind: Cardinal]: PPartitionInfo read GetPartInfo;
end;



// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation


const
    MBR_SIGNATURE = $AA55;
    NON_LBA_MAX_SECT = 1024*255*63; 	// Nombre de secteurs maximum pour une part non LBA


// Validité MBR:
// 1 seule part prim active
// Table part cohérente avec géométrie disque 

// Validité EMBR
// Entrée 3 et 4 à 0
// Table part cohérente avec géométrie disque 
// Offset relatif à la 1er entrée en part etendue

// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Creation de l'objet, errorcode à 0 si pas de problème
//
constructor TGenericDrive.Create();
begin
    Self.pdErrorCode := 0;

    Self.pdModelNumber	:= '';
    Self.pdSerialNumber	:= 'N/A';
    Self.pdFirmwareRev	:= 'N/A';
    Self.pdFlags := 0;

    Self.FPartitionList := TList.Create;
end;


//
// Detruit l'objet
//
destructor TGenericDrive.Destroy;
var
	i: integer;
begin
    for i := Self.FPartitionList.Count - 1 downto 0 do
    	Dispose(Self.PartitionInfo[i]);
         
	Self.FPartitionList.Free;
end;


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


//
// Retourne la taille du disque
//
function TGenericDrive.GetDiskSize(): int64;
begin
	Result := Self.SectorCount * int64(Self.BytesPerSec);
end;


//
// Demande le modele du disque 
//
function TGenericDrive.GetModelNumber(): String;
begin
	// Verifier si on a déjà les infos smart
    if (Self.pdFlags and SMART_INFO_LOADED_FLAG) = 0 then
    	Self.GetSmartInfo();

    Result := Self.pdModelNumber;
end;


//
//
//
function TGenericDrive.GetSerialNumber(): String;
begin
	// Verifier so on a déjà les infos smart
    if (Self.pdFlags and SMART_INFO_LOADED_FLAG) = 0 then
    	Self.GetSmartInfo();

    Result := Self.pdSerialNumber;
end;


function TGenericDrive.GetFirmwareRev(): String;
begin
	// Verifier so on a déjà les infos smart
    if (Self.pdFlags and SMART_INFO_LOADED_FLAG) = 0 then
    	Self.GetSmartInfo();

    Result := Self.pdFirmwareRev;
end;


function TGenericDrive.GetPartitionCount(): Cardinal;
begin
	Result := Self.FPartitionList.Count;
end;


function TGenericDrive.GetPartInfo(ind: Cardinal): PPartitionInfo;
begin
	result := Self.FPartitionList[ind];
end;


//
// Renvoie les propriétés du disque dans une TString: couple (propriété, value)
//
procedure TGenericDrive.GetDriveProperties(info: TStrings);
begin
    info.Add('Model Number');
    info.Add(Self.ModelNumber);
    info.Add('Serial Number');
    info.Add(Self.SerialNumber);
    info.Add('Firmware Version');
    info.Add(Self.FirmwareRev);

//    info.Add('Drive num');
//    info.Add(IntToHex(Self.pdDriveNum,2));

    // Fixe / Removable
//    info.Add('Status');
//    if Self.isDriveRemovable then
//	    info.Add('Removable')
//    else
//   	    info.Add('Fixed');

//    // Attachement Primary/Secondary Master/Slave
//    info.Add('Attachement');
//    info.Add('N/A');

	info.Add('Disk size');
    info.Add(IntToStr(Self.Size));	// Convertir ??

    // Geometrie
    info.Add('Cylinders');
    info.Add(IntToStr(Self.CylinderCount));
    info.Add('Heads');
    info.Add(IntToStr(Self.HeadCount));
    info.Add('Sect per track');
    info.Add(IntToStr(Self.SectorPerTrack));
    info.Add('Sector Count');
    info.Add(IntToStr(Self.SectorCount));
    info.Add('Total Sectors');
    info.Add(IntToStr(Self.pdTotalSectors));
    info.Add('Sector Size');
    info.Add(IntToStr(Self.BytesPerSec));

    // Infos sur les partition ????
end;


// ---------------------------------------------------------------------
//		 			Analyse de la table des partitions
// ---------------------------------------------------------------------


// besoin du numéro du secteur pour verifier la géomotrie
function TGenericDrive.isValidMBR(const buffer: Pointer): boolean;
var
	mbrbuf: PMasterBootRecord;
    i: integer;
begin
	mbrbuf := buffer;
	Result := true;

	// Vérifier la signature
    if mbrbuf.Signature <> MBR_SIGNATURE then
		Result := false;

    // Vérifier le début du code ???

    // Verifier la table des partitions
	for i := 0 to MAX_PRIMARY_PART - 1 do
    begin
    	if (mbrbuf.PartTable[i].BootInd <> 0) or (mbrbuf.PartTable[i].BootInd <> $80) then
			Result := false;

        if mbrbuf.PartTable[i].NumberSectors > Self.TotalSectors then
			Result := false;
    end;
end;


function TGenericDrive.isValidEMBR(const buffer: Pointer): boolean;
var
	mbrbuf: PMasterBootRecord;
    i: integer;
begin
	mbrbuf := buffer;
	Result := false;

	// Vérifier la signature
    if mbrbuf.Signature <> MBR_SIGNATURE then
    	exit;

    // Verifier la table des partitions
	for i := 0 to 1 do
    begin
    	if (mbrbuf.PartTable[i].BootInd <> 0) and (mbrbuf.PartTable[i].BootInd <> $80) then
			exit;

        // Taille part cohérente avec part size ?
        if mbrbuf.PartTable[i].NumberSectors > Self.TotalSectors then
			exit;
    end;

    // Les 2 dernières entrées doivent être nulles
	for i := 2 to 3 do
    begin
    	if mbrbuf.PartTable[i].BootInd <> 0 then exit;
    	if mbrbuf.PartTable[i].Head <> 0 then exit;
    	if mbrbuf.PartTable[i].Sector <> 0 then exit;
    	if mbrbuf.PartTable[i].Cylinder <> 0 then exit;
    	if mbrbuf.PartTable[i].SysInd <> 0 then exit;
    	if mbrbuf.PartTable[i].LastHead <> 0 then exit;
    	if mbrbuf.PartTable[i].LastSector <> 0 then exit;
    	if mbrbuf.PartTable[i].LastCylinder <> 0 then exit;
    	if mbrbuf.PartTable[i].RelativeSector <> 0 then exit;
    	if mbrbuf.PartTable[i].NumberSectors <> 0 then exit;
    end;

    // Les tests ont réussi, c'est donc propablement un EMBR
    Result := true;
end;


{function GetPartSector(const partent: TPartEntry): Cardinal;
begin
	Result := partent.Sector and $3f;
end;


function GetPartCylinder(const partent: TPartEntry): Cardinal;
begin
	Result := partent.Cylinder or ((partent.Sector shl 2) and $300);
end;
}


//
// Remplie la structure TPartitionInfo à partir de l'entrée part
//
function NewPartInfo(const partent: TPartEntry; const offset: Cardinal): PPartitionInfo;
begin
	new(Result);
    Result.FPartitionType	:= partent.SysInd;
    Result.FActivePartition := partent.BootInd = $80;
    Result.FPrimaryPartition:= offset = 0;
    Result.FStartingSector	:= partent.RelativeSector + offset;
    Result.FSectorCount		:= partent.NumberSectors;
    Result.FTypeName		:= TGenericDrive.GetPartTypeName(partent.SysInd);
end;


//
// Lit la table de partition
//
{$O-}	// pour eviter un bug dans la boucle
function TGenericDrive.ReadMBR(): boolean;
var
	mbrbuffer: TMasterBootRecord;
    i: integer;
begin
    // Verifie si déja récupéré
	if (Self.pdFlags and PART_INFO_LOADED_FLAG) <> 0 then
    	exit;

	// Flag partition chargé mis.
    Self.pdFlags := Self.pdFlags or PART_INFO_LOADED_FLAG;

	// Lit la MBR
	Result := Self.ReadSectors(0, 1, @mbrbuffer);
    if not Result then
    	exit;

    // MBR Valide ???
    Result := (mbrbuffer.Signature = MBR_SIGNATURE);
    if Result then
    begin
        // Regarde les 4 entrées
    	for i := 0 to (MAX_PRIMARY_PART - 1) do
        begin
        	if mbrbuffer.PartTable[i].SysInd <> 0 then
            begin
                // Besoin d'utiliser le LBA ?
            	//if buffer.PartTable[i].NumberSectors > NON_LBA_MAX_SECT then
                //;
                // LBA =>

                // Partition étendue ?
                //http://www.ata-atapi.com/hiwtab.htm
                if mbrbuffer.PartTable[i].SysInd in [$05, $0f] then
                begin
                	// Lire la MBR ext (Partition chainées)
                    // Analyse les entrées de la MBR Ext (entrée 1 et 2)
                    Self.ReadExtendedPart(mbrbuffer.PartTable[i].RelativeSector);
                end
                else
                begin
                	// Recupérer les infos de la partition primaire
                    Self.FPartitionList.Add(NewPartInfo(mbrbuffer.PartTable[i], 0));
                end;
            end; // if
        end; // for
    end;
end;
{$O+}


//
// Fonction pour lire la chaine des partitions logiques
//
// Params:
//	Offset: n° secteur contenant l'extended MBR par rapport au début du disque
//
function TGenericDrive.ReadExtendedPart(const offset: Cardinal): boolean;
var
	mbrbuffer: TMasterBootRecord;
    currentembr: Cardinal;
begin
    // Le 1er EMBR correspond à l'offset
    currentembr := offset;

    // Boucle pour la chaine des partitions logiques
    repeat
        // Lit la partition etendues
        Result := Self.ReadSectors(currentembr, 1, @mbrbuffer);
        if not Result then
            exit;

        // MBR Valide ???
        Result := (mbrbuffer.Signature = MBR_SIGNATURE);
        if Result then
        begin
            // Lire La partition etendue
            if (mbrbuffer.PartTable[0].SysInd <> 0) and (mbrbuffer.PartTable[0].NumberSectors <> 0) then
            begin
                // Recuperer les infos de la part
                 Self.FPartitionList.Add(NewPartInfo(mbrbuffer.PartTable[0], currentembr));
            end;
        end
        else
            exit;

        // Mets à jour le pointeur sur le EMBR : L'adresse des EMBR suivant est
        // indexé par rapport au 1er EMBR de la chaine 
        currentembr := offset + mbrbuffer.PartTable[1].RelativeSector;
    until (mbrbuffer.PartTable[1].SysInd = 0);
end;


//
// Renvoie le nom du file system de la partition correspondant au sysid 
//
// http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
//
class function TGenericDrive.GetPartTypeName(const sysid: byte): string;
begin
    case sysid of
        $00: Result := 'Free';
        $01: Result := 'FAT 12';
        $04: Result := 'FAT 16';
        $06: Result := 'FAT 16';
        $07: Result := 'NTFS';
        $0B: Result := 'FAT 32';
        $0C: Result := 'FAT 32';
        $0E: Result := 'FAT 16';
        $0F: Result := 'Extended';
        $12: Result := 'EISA partition';
		$1b: Result := 'Hidden WIN95 OSR2 FAT32';
		$1c: Result := 'Hidden WIN95 OSR2 FAT32, LBA-mapped';
		$1e: Result := 'Hidden WIN95 16-bit FAT, LBA-mapped';
		$3c: Result := 'Partition Magic recovery partition';
        $42: Result := 'Dynamic disk volume';
        $44: Result := 'GoBack partition';
        $53: Result := 'Disk Manager 6.0 Aux3';
        $54: Result := 'Disk Manager 6.0 Dynamic Drive Overlay (DDO)';
        $55: Result := 'EZ-Drive';
		$78: Result := 'XOSL FS';
        $82: Result := 'Linux Swap';	// 'Solaris x86'
        $83: Result := 'Linux native';
		$84: Result := 'Hibernation partition';
        $85: Result := 'Linux Entended';
        $97: Result := 'Free FDISK hidden Primary DOS FAT32 partition';
		$98: Result := 'Free FDISK hidden Primary DOS FAT32 partition (LBA)';
		$9a: Result := 'Free FDISK hidden Primary DOS FAT16 partition (LBA)';
		$9b: Result := 'Free FDISK hidden DOS extended partition (LBA)';
		$9f: Result := 'BSD/OS';
		$a0, $a1: Result := 'Laptop hibernation partition';
        $a5: Result := 'FreeBSD';
        $a8: Result := 'Mac OS-X';
        $a9: Result := 'NetBSD';
        $ab: Result := 'Mac OS-X Boot partition';
        $bb: Result := 'Boot Wizard hidden';
        $de: Result := 'Dell PowerEdge Server utilities (FAT)';
        $EB: Result := 'BeOS';
    else
    	Result := 'Unknown (' + IntToHex(sysid, 2) + ')';
    end;
end;



const
FSFamilyNameArray: array[fsUnknown..fsNTFS] of string =
(
	'Unknown', 'FAT 12', 'FAT 16', 'FAT 32', 'NTFS'
);


//
// Renvoie la famille du filesystem lié au SysInd
//
class function TGenericDrive.GetFileSystemFamily(const sysid: byte): TFileSystemFamily;
begin
    case sysid of
      $01: Result := fsFAT12;
      $0B, $0C, $1b, $1c, $97, $98: Result := fsFAT32;
      $04, $06, $0E, $1e, $9a: Result := fsFAT16;
      $07: Result := fsNTFS;
      $83: Result := fsLinux;
    else
    	Result := fsUnknown;
	end;
end;


class function TGenericDrive.GetFileSystemFamilyName(const sysid: byte): string;
begin
	Result := FSFamilyNameArray[Self.GetFileSystemFamily(sysid)];
end;


//
// Renvoie la famille du filesystem lié au SysInd
//
{class function TGenericDrive.GetFileSystemFamily(const sysid: byte): string;
begin
    case sysid of
      $01: Result := 'FAT 12';
      $0B, $0C, $1b, $1c, $97, $98: Result := 'FAT 32';
      $04, $06, $0E, $1e, $9a: Result := 'FAT 16';
      $07: Result := 'NTFS';
    else
    	Result := 'Unknown';
	end;
end;}


// ---------------------------------------------------------------------
//						Routines de Conversion
// ---------------------------------------------------------------------


//
//From CHS to LBA
//The equation to convert from CHS to LBA follows:
//
//LBA = ( ( CYL * HPC + HEAD ) * SPT ) + SECT - 1
//    = ( ( 1275 * 255 + 1 ) * 63 ) + 1 - 1
//Where:
// LBA: linear base address of the block
// CYL: value of the cylinder CHS coordinate
// HPC: number of heads per cylinder for the disk
//HEAD: value of the head CHS coordinate
// SPT: number of sectors per track for the disk
//SECT: value of the sector CHS coordinate
//
function TGenericDrive.CHSToLBA(const chsadd : TCHS; var lba : Cardinal):boolean;
begin
	// Verifier la validité du LBA
    //if (chsadd.Cylinder <= Self.HeadCount) and (chsadd.Head <= Self.CylinderCount) and
    //   (chsadd.Sector <= Self.SectorPerTrack) then
    
    Result := true;

   lba := ((chsadd.Cylinder * Self.HeadCount + chsadd.Head) * Self.SectorPerTrack) + chsadd.Sector - 1;
end;


//
//From LBA to CHS
//The equations to convert from LBA to CHS follow:
//
// CYL = LBA / (HPC * SPT)
//TEMP = LBA % (HPC * SPT)
//HEAD = TEMP / SPT
//SECT = (TEMP % SPT) + 1
//
//Where:
// LBA: linear base address of the block
// CYL: value of the cylinder CHS coordinate
// HPC: number of heads per cylinder for the disk
//HEAD: value of the head CHS coordinate
// SPT: number of sectors per track for the disk
//SECT: value of the sector CHS coordinate
//TEMP: buffer to hold a temporary value
//
function TGenericDrive.LBAToCHS(var chsadd : TCHS; const lba : Cardinal):boolean;
var
	temp : Cardinal;
begin
    Result := false;

    // Verifier la validité
    if lba < Self.pdTotalSectors then
        if (Self.SectorPerTrack <> 0) and (Self.HeadCount <> 0) then
        begin
            chsadd.Cylinder := Word(lba div (Self.SectorPerTrack * Self.HeadCount));
            temp := lba mod (Self.SectorPerTrack * Self.HeadCount);
            chsadd.Head := Word(temp div Self.SectorPerTrack);
            chsadd.Sector := Word (temp mod Self.SectorPerTrack) + 1;

            Result := true;
        end;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
