{-----------------------------------------------------------------------------
 Unit Name: Recovery
 Author:    Chris
 Purpose:	A supprimer ou modifier en profondeur (pour l'instant doublon avec MainUnit)
 History:   02/03/2005
-----------------------------------------------------------------------------}


unit Recovery;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  FAT32Partition, GenericFAT, NTFSPartition, GenericDrive;


type

TProgressEvent = procedure(Sender: TObject; pos: Cardinal; var abort: boolean) of object;
TPartitionEvent = procedure(Sender: TObject; pinfo: PPartitionInfo; var abort: boolean) of object;

//
//
//
TPartitionScanner = class
    private
    	FAbort:			boolean;		// indique s'il faut arreter le scan
        FCompleted:		boolean;		// Indique que le scan est terminé
        //
		FOnProgress:	TProgressEvent;
        FOnPartition:	TPartitionEvent;
        // ============================= Methodes =============================
		function SearchPart(const lba: Cardinal): PPartitionInfo;
		function SearchNextPart(const lba: Cardinal): Cardinal;
		procedure AddPartToList(pinfo: PPartitionInfo);
    protected
    	FBuffer:	Array of Byte;
		FDrive:		TGenericDrive;
        FListPart:	TList;
        // ============================= Methodes =============================
        function GetMaxPos(): Cardinal;
        procedure ScanPart();
    public
        // ============================= Methodes =============================
        constructor Create(drive: TGenericDrive);
        destructor Destroy;
        procedure Execute;
        // ============================= Propriétés =============================
        property Max: Cardinal read GetMaxPos;
        //property Steps: Cardinal read GetMaxPos;
        //property CurrentStep: Cardinal read GetCurrentStep;
        property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
        property OnPartition: TPartitionEvent read FOnPartition write FOnPartition;
        property Drive: TGenericDrive read FDrive;
        // Se souvenir si scan déja effectué ??
end;

// En cas d'abort, refaire un scan sur le reste du disque ?
// => memoriser la dernière pos, modifier les bornes de la boucles
// => Flag pour indiquer que le scan n'est pas terminé


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation


uses
	ConversionUnit, TreeListManager, GenericPartition;
               

// ========================================================================
//							 Gestion de TPartitionSCcanner
// ========================================================================


// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Contruit le scanner pour le disque drive
//
constructor TPartitionScanner.Create(drive: TGenericDrive);
begin
	Self.FDrive := drive;
    Self.FCompleted := false;

    SetLength(Self.FBuffer, Self.Drive.BytesPerSec);
end;


destructor TPartitionScanner.Destroy;
var
	i: integer;
begin
	// Supprime les PartInfo et la liste
    if Assigned(Self.FListPart) then
    begin
        for i:= 0 to Self.FListPart.Count - 1 do
    	Dispose(Self.FListPart[i]);

		Self.FListPart.Free;
    end;
end;


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


function TPartitionScanner.GetMaxPos(): Cardinal;
begin
	Result := Self.Drive.CylinderCount * 5; // (3 + 2)
end;


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


procedure TPartitionScanner.Execute();
var
	i: integer;
begin
    if Self.FCompleted then
    begin
    	for i := 0 to Self.FListPart.Count - 1 do
        	if Assigned(Self.FOnPartition) then
            	Self.FOnPartition(Self, Self.FListPart[i], Self.FAbort);
    end
    else
		Self.ScanPart; 	//Faire un LoadMBR avant ?
end;


//
// Renvoie le Pinfo commencant à lba ou nil si inexistante
//
function TPartitionScanner.SearchPart(const lba: Cardinal): PPartitionInfo;
var
    i: integer;
begin
    Result := nil;
    for i := 0 to Self.FListPart.Count - 1 do
    begin
    	if PPartitionInfo(Self.FListPart[i]).FStartingSector = lba then
        	Result := Self.FListPart[i];

        // Liste trié => pas besoin d'aller plus loin
        if PPartitionInfo(Self.FListPart[i]).FStartingSector >= lba then
        	exit;
    end;
end;


//
// Cherche s'il existe une partition dans la liste qui commence après lba
// Renvoie le lba de cette partition si elle existe sinon renvoie MaxCard
//
function TPartitionScanner.SearchNextPart(const lba: Cardinal): Cardinal;
var
    i: integer;
begin
    Result := MaxCardinal;
    for i := 0 to Self.FListPart.Count - 1 do
        if PPartitionInfo(Self.FListPart[i]).FStartingSector > lba then
        begin
            Result := PPartitionInfo(Self.FListPart[i]).FStartingSector;
            exit;
        end;
end;


//
// Ajoute une nouvelle partition à la liste
//
procedure TPartitionScanner.AddPartToList(pinfo: PPartitionInfo);

    //
    // Fonction CallBack pour trier la liste des partitions
    //
    function SortPartList(p1, p2: Pointer): integer;
    begin
        Result := PPartitionInfo(p1).FStartingSector - PPartitionInfo(p2).FStartingSector;
    end;

begin
	Self.FListPart.Add(pinfo);
    Self.FListPart.Sort(@SortPartList);
end;


//
// Procedure pour chercher un boot sector ou MBR, EMBR sur le disque
//
// Utiliser les partitions existantes du disque
//
// Calculer la position d'un backup sector NTFS (S = 255 ?)
// Besoin de mémoriser: LBA, CHS, FS, taille?, nom ?,
// Backup NTFS => comment retrouver le lba ?? besoin de taille
// Call back OnProgess et OnPart Possibilité d'abort ???
// Si part from EMBR et BS ok, l'indiquer quelque part  
// Error: BS Invalid, BSB Invalid, ...
// Les infos partition sont stockées dans des TPartitionInfo et les pointeurs
// sont dans la Tlist
// Utiliser HiddenSectors pour vérif ???
procedure TPartitionScanner.ScanPart();
var
	nextlba, lba, lbaback: Cardinal;
	chs: TCHS;
    i, c, h: integer;
    found: boolean;
    temppartinfo: PPartitionInfo;
    temppart: TGenericPartition;

    
    function GetPartInfoFromEMBR(const lba: Cardinal; buffer: Pointer): PPartitionInfo;
    begin
        New(Result);
        with PMasterBootRecord(buffer).PartTable[0] do
        begin
            Result.FPartitionType	:= SysInd;
            Result.FActivePartition := (BootInd = $80);
            Result.FPrimaryPartition:= false;	// EMBR => extended part
            Result.FSectorCount		:= NumberSectors;
            Result.FStartingSector	:= RelativeSector + lba;
            Result.FTypeName		:= TGenericDrive.GetFileSystemFamilyName(SysInd);
        end;
    end;

    
    function GetPartInfoFromPart(part: TGenericPartition): PPartitionInfo;
    begin
        // Faire un new ?
        New(Result);
        // Comment trouver le BootID ? (Notion de Famille de part FAT32,FAT16,NTFS ?)
        Result.FSectorCount := part.TotalSectors;
        Result.FStartingSector := part.FirstSecOfPart;
        Result.FTypeName := part.FileSystemFamilyName;
    end;

    //#Todo3 A t-on besion de cette function ????
    procedure UpdatePartInfo(pinfo: PPartitionInfo; part: TGenericPartition);
    begin
    end;
    
    function isSamePart(pinfo: PPartitionInfo; part: TGenericPartition):boolean;
    begin
    	Result := (pinfo.FSectorCount = part.TotalSectors) and
        		  (pinfo.FStartingSector = part.FirstSecOfPart);
                  // rajouter test sur FS Familly
    end;

begin
    Self.FListPart := TList.Create;
    Self.FAbort := false;
	temppart := nil;

    // RECOPIER les partitions du disque dans listpart

    for i := 0 to Self.FDrive.PartitionCount - 1 do
    begin
    	New(temppartinfo);
		CopyMemory(temppartinfo, Self.FDrive.PartitionInfo[i], sizeof(TPartitionInfo));
    	Self.FListPart.Add(temppartinfo);
    end;

    // Recherche le lba de la premiere partition existante
    nextlba := SearchNextPart(0);

	// Boucle sur les cylindres
    //for c := 11970 to Self.FDrive.CylinderCount - 1 do	//#DEBUG
    for c := 0 to Self.FDrive.CylinderCount - 1 do
    begin
    	// Débit de partition sur tête 0 ou 1
        for h := 0 to 1 do
        begin
			// On passe le secteur 0 du disque = MBR
        	if (c = 0) and (h = 0) then
            	continue;

            found := false;

        	// Se positionner sur le 1er secteur en début de cylindre
            chs.Cylinder:= c;
            chs.Head	:= h;
			chs.Sector	:= 1;
            Self.FDrive.CHSToLBA(chs, lba);

            // Boot Sector ?
            if Self.FDrive.ReadSectors(lba, 1, Self.FBuffer) then
            begin
                if TFAT32Partition.isValidBootSector(Self.FBuffer) then
                begin
                    temppart := TFAT32Partition.Create(Self.FDrive, lba);
                    found := temppart.LoadFromLBA(lba);
                end;

                if (not found) and TNTFSPartition.isValidBootSector(Self.FBuffer) then
                begin
                    temppart := TNTFSPartition.Create(Self.FDrive, lba);
                    found := temppart.LoadFromLBA(lba);
                end;

                // EMBR => Offset + taille part + BootID + Offset part suivante
                // Possibilité de faire des regroupement avec BPB
                // Boot Sector => verifier avec EMBR pour le valider
                if (not found) and Self.FDrive.IsValidEMBR(Self.FBuffer) then
                begin
                    // Recuperer les infos de l'EMBR A Supprimer à un endroit
                    //temppartinfo := GetPartInfoFromEMBR(lba, Self.FBuffer);
                    // Ajouter à la liste
                    //Self.AddPartToList(temppartinfo);
                    found := true;
                end
            end;

            // Pas de BS trouvé, Pas de EMBR => peut être un backup BS
            // Lecture backup FAT32
            //#ATTN GetInfoFrom
            if (not found) and Self.FDrive.ReadSectors(lba + 6, 1, Self.FBuffer) then
                if TFAT32Partition.isValidBootSector(Self.FBuffer) then
                begin
                    //temppart := TFAT32Partition.Create(Self.FDrive, lba);
                    //found := temppart.GeLoadFromLBA(lba);
                end;

            // Lecture backup boot sector NTFS sur 2000/XP (H=254, S=63)
            if (not found) and (h = 0) then
            begin
                // Se positionner en fin de cylindre
                chs.Head	:= 254;
                chs.Sector	:= 63;
                Self.FDrive.CHSToLBA(chs, lbaback);

                if Self.FDrive.ReadSectors(lbaback, 1, Self.FBuffer) then
	                if TNTFSPartition.isValidBootSector(Self.FBuffer) then
                    begin
                        temppart := TNTFSPartition.Create(Self.FDrive, lbaback);
                        found := (temppart as TNTFSPartition).LoadFromBackupLBA(lbaback);
                    end;
            end;

            if Assigned(temppart) then
            begin
	            // On a trouvé un BS ou BBS
            	// Pour un BBS NTFS prendre HiddenSectors comme LBA
                temppartinfo := SearchPart(lba); 
            	if temppartinfo = nil then
                begin
                	// Pas de partition dans la liste
                    temppartinfo := GetPartInfoFromPart(temppart);

                    // Ajouter à la liste
                    Self.AddPartToList(temppartinfo);
                end
                else
                begin
                	// Déja une partition dans la liste
                    if isSamePart(temppartinfo, temppart) then
                    	UpdatePartInfo(temppartinfo, temppart)  // Mettre à jour
                    else
                    begin
                        // Déja une partition dans la liste mais diff
                        //EVENT(old part)
                        if Assigned(Self.FOnPartition) then
                            Self.FOnPartition(Self, temppartinfo, Self.FAbort);
                        
                        // Creer le nouveau PartInfo et ajouter à la liste
                        temppartinfo := GetPartInfoFromEMBR(lba, Self.FBuffer);
                        Self.AddPartToList(temppartinfo);
                    end;
                end;

                //Libère la partition temporaire
                FreeAndNil(temppart);

                //EVENT
                if Assigned(Self.FOnPartition) then
                    Self.FOnPartition(Self, temppartinfo, Self.FAbort);
            end
            else
            	if found then
                begin
                	// found = true, mais temppart = nil => EMBR trouvé
                    temppartinfo := GetPartInfoFromEMBR(lba, Self.FBuffer);
                    Self.AddPartToList(temppartinfo);
                end
                else
                begin
                	// Verifier si on a déja passé la partition de la liste
                    if lba > nextlba then
                	begin
                    	// quel partinfo récuperer ?
                    	// EVENT
                        if Assigned(Self.FOnPartition) then
                            Self.FOnPartition(Self, temppartinfo, Self.FAbort);

                    	nextlba := SearchNextPart(nextlba); //#ATTN lba ou nextlba en param ?
                    end;
                end;

            // ProgessEvent
            if Assigned(Self.FOnProgress) then
				Self.FOnProgress(Self, 5 * c + 3 + h * 2, Self.FAbort);

            if Self.FAbort then
            	exit;
        end; // for h
    end; // for c

    Self.FCompleted := true;
end;


// ========================================================================
//								Recovery du fichier
// ========================================================================


//
// Si le rep du noeud n'exite pas on le crée et mettre à jour attr et dates
// Pour tous les fichiers du rep
//	S'il exite demander confirmation pour reecrire
//	Sauver le fichier
//	Si erreur demander si effacer le fichier
// finpour
// Faire la même chose pour les fils
//
// Options:
// Save Fichier taille 0 (oui, non, demander)
// Create empty dir (oui, non, demander)
// Fichier incomplet : Ecrire partie valide, Remplir avec une pattern, concatener les parties valide, effacer
// Erreur E/S fichier : Garder le fichier ou effacer
//
// Path contient le chemin complet par ex: C:\Temp\Backup
// Rajouter la gestion du abort et flag pour fichier selectionné
{procedure TFormRecovery.SaveTree(node: TTreeNode; const path: string; const checkedonly: boolean);
var
	i : smallint;
    filedata: TFileItemData;
	dirdata : TDirectoryNodeData;
    dirname : string;
begin
    dirdata := TDirectoryNodeData(node.Data);

    // Vérifier si on doit gérer ce répertoire
    if checkedonly and (dirdata.Status = ncUnchecked) then
    	exit;

    // Calcule de nom du répertoire
    dirname := IncludeTrailingPathDelimiter(path) + dirdata.DirInfo.FileName + '\';

    //#ToDo2 Traiter les répertoires nul
    //if (dirdata.FileCount = 0) and not AllowEmptyDir then
    //	exit;

	// Crée eventuellement le rep
    if (not checkedonly) or (dirdata.SelectedCount > 0) then
        if not DirectoryExists(dirname) then
            if not ForceDirectories(dirname) then
            begin
                MessageDlg(Format(CANT_CREATE_DIR_ERROR_MSG, [dirname]), mtError, [mbOK], 0);
                exit;
            end
            else
            begin
                // Mettre à jour l'heure pour le répertoire
                RestoreDirTimestamp(dirname, dirdata.DirInfo);

                // Mettre à jour les attributs du répertoire
                FileSetAttr(dirname, dirdata.DirInfo.Attributs);
            end;

    // Restaurer tous les fichiers du répertoire
    for i := 0 to dirdata.FileCount - 1 do
    begin
    	filedata := TFileItemData(dirdata.FileData[i]);

        // Doit on sauver ce fichier ??
        if checkedonly and (filedata.Selected = ncUnchecked) then
        	continue;

        // Que faire des fichiers de taille 0 ? les ouvrir ou les ignorer ?
        //#Todo2 ajouter un flag pour les fichier vide AllowEmptyFile
        //if (filedata.FileInfo.Size = 0) and not AllowEmptyFile then
        //	continue;

        if FileExists(dirname + filedata.FileInfo.FileName) then
        begin
        	// Effacer l'ancien ou renommer le nouveau ?
            // Soit on sauve ou on passe au suivant
        end;

        // Mettre à jour le label
        Self.LabelFileName.Caption := filedata.FileInfo.FileName;

        if not Self.RestoreFile(dirname + filedata.FileInfo.FileName, filedata.FileInfo) then
        begin
        	Inc(Self.FErrorCount);
            //Self.MemoStdErr.Lines.Add('Error on file : ' + filedata.FileInfo.FileName);
        end;

        Inc(Self.FByteSaved, filedata.FileInfo.Size);
        //Self.UpdateProgressBar();
    end; // for

    // On restaure les sous répertoires
    for i := 0 to node.Count - 1 do
		Self.SaveTree(node.Item[i], dirname, checkedonly);
end;}


//
// filename contient le path
//#Todo1 Gerer le fichier de taille nulle => Pas de cluster associé
//
{function TFormRecovery.RestoreFile(const filename: string; fileinfo: TFileInfo): boolean;
var
    fichier : TFileStream;
    filedeleted: boolean;
begin
	Result := true;
    filedeleted := false;
    Self.CurrentClustList.Count := 0;

    // Récuperer la liste des clusters du fichier
    if not Self.FPart.GetFileClusterChain(fileinfo.FirstCluster, fileinfo.Size, Self.CurrentClustList) then
    begin
    	//Self.AddLine(Format(CLUSTER_CHAIN_ERROR_STR, [filename]));
        // Le fichier n'est pas entier :
        // Passer le fichier ? Ecrire seulement la partie valide ?
        // Ecrire une pattern dans les parties manquantes ?
        // Demander à chaque fois ?
    	Result := false;
        exit;
    end;

    try
    	//#Todo2 Verifier l'existence du fichier et agir en conséquence

        // Creer le fichier
        fichier := TFileStream.Create(filename, fmShareExclusive or fmCreate);
        if not Self.FPart.SaveChainToFile(fichier, Self.CurrentClustList, fileinfo.Size) then
        begin
	    	//Self.AddLine(Format(RECOVER_FILE_ERROR_STR, [filename]));
            Result := False;
        // Comment traiter les erreurs lors de la lecture des cluster ? Evenement ?
        //OnScanError()
        end;

        // Fermer le fichier
        fichier.Free;
    except
        on EFCreateError do
        	Result := false;
        //on EWriteError do; // verifier si traité dans saveChain	
    end; // try

    // Efface le fichier au besoin en cas d'erreur
    if filedeleted then
    	DeleteFile(filename);

    // Si on efface le fichier => ne pas restaurer timestamp et attribut
    if not filedeleted then
    begin
        // Mettre à jour la date et l'heure
        RestoreFileTimeStamp(filename, fileinfo);

        // Mettre à jour les attributs Mettre un mask sur les attributs ?
        FileSetAttr(filename, fileinfo.Attributs);
    end;
end;}


// --------------------------- Fin de l'unité -----------------------------
end.
