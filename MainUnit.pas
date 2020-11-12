{-----------------------------------------------------------------------------
 Unit Name: MainUnit
 Author:    Chris
 Purpose:
 Date:		25/04/2005
 History:

//#TODO1 mettre un abort dans le OnFile et OnDir
//#Todo2 Rajouter le listing des rep dans les menus
                                           clipboard
//#Todo2 Verification des droits administrateur  D:tnt "How to get if logged in as administrator -  NT and" 
-----------------------------------------------------------------------------}

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, CommCtrl, Menus,
  ImgList,
  //
  GenericDrive, GenericFAT, GenericPartition, FAT32Partition, NTFSPartition,
  TreeListManager, ProgressFormUnit, CommonFileInfo;


type

// D�crit l'�tat dans lequel se trouve l'application selon les op�rations r�alis�es
// L'ordre des �tats est important pour la gestion des menus
// asNotDefined = 0 pour le menus qui ne sont toujours desactiv�s (info dans le Tag)
TApplicationState = (asNotDefined, asInitial, asDiskSelected, asPartSelected, asPartScanned);


TMainForm = class(TForm)
    ImageListFile: TImageList;
    ListView: TListView;
    TreeView: TTreeView;
    StatusBar: TStatusBar;
    MemoStdErr: TMemo;
    ImageListState: TImageList;
    MainMenu: TMainMenu;
    MenuDrives: TMenuItem;
    ItemSelectDrive: TMenuItem;
    ItemSelectVirtDrive: TMenuItem;
    N1: TMenuItem;
    ItemDriveInfo: TMenuItem;
    N2: TMenuItem;
    ItemExitApp: TMenuItem;
    MenuPartition: TMenuItem;
    MenuRecover: TMenuItem;
    MenuTools: TMenuItem;
    ItemSelectPartition: TMenuItem;
    ItemRecoverAll: TMenuItem;
    ItemRecoverSelectedFiles: TMenuItem;
    ItemListFiles: TMenuItem;
    ItemListSelFiles: TMenuItem;
    MenuAbout: TMenuItem;
    ItemOption: TMenuItem;
    N3: TMenuItem;
    ItemPartRootScan: TMenuItem;
    SaveDialog: TSaveDialog;
    ItemPartInfo: TMenuItem;
    SplitterHoriz: TSplitter;
    Panel: TPanel;
    SplitterVert: TSplitter;
    ItemSelectFiles: TMenuItem;
    N4: TMenuItem;
    ItemUnselectFiles: TMenuItem;
    ItemExpandTree: TMenuItem;
    N5: TMenuItem;
    CollapseTree1: TMenuItem;
    ItemAbout: TMenuItem;
    N6: TMenuItem;
    ItemClearLog: TMenuItem;
    ItemSaveLog: TMenuItem;
    PopupMenuMemo: TPopupMenu;
    PopItemSaveLog: TMenuItem;
    PopItemClearLog: TMenuItem;
    PopupMenuLV: TPopupMenu;
    PopLVItemProp: TMenuItem;
    PopItemSelect: TMenuItem;
    PopItemUnselect: TMenuItem;
    N7: TMenuItem;
    ItemProp: TMenuItem;
    ItemHelp: TMenuItem;
    ItemDumpCluster: TMenuItem;
    ItemPartList: TMenuItem;
    ItemPartialScan: TMenuItem;
    N8: TMenuItem;
    PopItemSelectAll: TMenuItem;
    PopItemCopy: TMenuItem;
    PopupMenuTV: TPopupMenu;
    Expand1: TMenuItem;
    Collapse1: TMenuItem;
    N9: TMenuItem;
    PopTVItemProperties: TMenuItem;
    N10: TMenuItem;
    ItemDumpDrive: TMenuItem;
    N11: TMenuItem;
    ItemSearchPart: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ItemSelectDriveClick(Sender: TObject);
    procedure ItemRecoverSelectedFilesClick(Sender: TObject);
    procedure ItemListSelFilesClick(Sender: TObject);
    procedure ItemPartScanClick(Sender: TObject);
    procedure ItemPartInfoClick(Sender: TObject);
    procedure ItemSelectPartitionClick(Sender: TObject);
    procedure ItemExpandTreeClick(Sender: TObject);
    procedure CollapseTree1Click(Sender: TObject);
    procedure ItemDriveInfoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ItemRecoverAllClick(Sender: TObject);
    procedure ItemExitAppClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ItemOptionClick(Sender: TObject);
    procedure ItemClearLogClick(Sender: TObject);
    procedure ItemSaveLogClick(Sender: TObject);
    procedure ItemListFilesClick(Sender: TObject);
    procedure ItemAboutClick(Sender: TObject);
    procedure PopItemPropClick(Sender: TObject);
    procedure ItemPropClick(Sender: TObject);
    procedure ItemDumpClick(Sender: TObject);
    procedure ItemPartListClick(Sender: TObject);
    procedure PopItemSelectAllClick(Sender: TObject);
    procedure PopItemCopyClick(Sender: TObject);
    procedure ItemHelpClick(Sender: TObject);
    procedure TreeViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ItemSearchPartClick(Sender: TObject);
    //procedure ListViewData(Sender: TObject; Item: TListItem);
  private
    FApplicationState: TApplicationState;	// Etat de l'application
    // ------
    FRootNode	: TTreeNode;	// Noeud racine du TreeView
    CurrentNode : TTreeNode;	// Noeud courant (utilis� comme variable temporaire Populate)
    CurDirData	: TDirectoryNodeData;	// Donn�es li�s au r�pertoire courant
    CurListItem : TListItem;	// Dernier ListItem lors d'un Bouton droit sur le LV
    CurTreeNode : TTreeNode;	// Dernier TreeNode lors d'un Bouton droit sur le TV
    //
    SysImage	: TImageList;	// Liste des icones pour les type de fichiers
    // --------------- Infos sur les tailles/nb fichier et dir -------------------
    FTotalSize		: int64;		// Taille totale des fichiers dans l'arbre
    FTotalBlockSize	: int64;		// Taille totale occup� sur le disque
    TotalFileCount	: Cardinal;		// Nombre de fichiers dans l'arbre
    TotalDirCount   : Cardinal;		// Nombre de r�pertoires dans l'arbre
	FTotalSelectedSize: int64;		// Taille totale des fichiers selectionn�s
    FSelectedFileCount: Cardinal;	// Nombre de fichiers selectionn�s
    // ----------------------- Infos pour le progess bar -----------------------
    FByteToSave		: int64;		// Nombre d'octets � sauver dans un SaveTree ()
    FByteSaved		: int64;		// Nombre d'octets d�j� sauv�
    FFilesErrorCount: Cardinal;		// Nombre de fichier en erreur lors du SaveTree
    // -----------------------
    RestoreDirectory: string;		//	R�pertoire destination pour la sauvegarde
    //
    FProgressDlg:	TWaitingForm;
    // ============================= Methodes =============================
    procedure DumpPartitionList();
	// ------------- Fonctions pour les listing fichiers / dir ---------------
    procedure DoSaveFileListing(node: TTreeNode; const selected: boolean; const filter: string);
    //procedure DoSaveDirListing(node: TTreeNode; const selected :boolean);
	// -----------------
    procedure AddLine(const str: string);
	procedure WriteLn(const str: string);
    // --------------------- Fonctions pour l'affichage LV/TV --------------------
    function GetExtIconIndex(fext: String; Attributs: DWORD; var TypeFichier: string): Integer;
    procedure AddFileToListView(filedata: TFileItemData);
    procedure DisplayFilesFromNode(dirdata : TDirectoryNodeData);
    procedure UpdateListView;
    procedure UpdateSelectedInfo;
    //
    procedure DisplayFileProp(fileinfo: TCommonFileInfo);
    // ------------------ Fonctions pour (De)Activer les menus ----------------
    procedure MenuInit();
	procedure UpdateMenuState();
	procedure SetApplicationState(const state: TApplicationState);    

    procedure Unpopulate;					// Efface les infos du populate (TV,LV, data)
    // ---------------- Gestion des events -------------------------
    procedure OnFileEntry(Sender: TObject; entry: TCommonFileInfo; var abort: boolean);
    procedure OnDirEntry(Sender: TObject; entry: TCommonFileInfo; var abort: boolean);
	procedure OnRestoreEvent(Sender: TObject; const filesize, writtenbytes: int64);
    // -------
	procedure PopulateTreeview(const clusterid: int64);
    procedure FreeNodes(node : TTreeNode);
    function RestoreFile(const filename: string; fileinfo: TCommonFileInfo): boolean;
    procedure RecalcSelected;
	procedure OnPartition(Sender: TObject; pinfo: PPartitionInfo; var abort: boolean);
	procedure OnSearchPartProgress(Sender: TObject; pos: Cardinal; var abort: boolean);
  protected
  public
    // ============================= Methodes =============================
    procedure Recovery(const checkedonly: boolean);
    procedure SaveTree(node: TTreeNode; const path: string; const checkedonly: boolean);
end;


var
    MainForm: TMainForm;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


uses
    ShellAPI,
    // Unit�s priv�es
    DriveDump, ConversionUnit, FormProperties, SelectDrive,
    RecoverOptions, SelectPartition, ZAboutUnit, Recovery, SHBrowseDialog;


Const
// Index des images dans le ImageList
IMAGE_ROOT			= 0;
IMAGE_DIR_OPEN		= 1;
IMAGE_DIR_CLOSE		= 2;
IMAGE_DIR_OPEN_ERR	= 3;
IMAGE_DIR_CLOSE_ERR	= 4;


var
    disque : TGenericDrive;
	part : TGenericPartition;


{$R *.dfm}


//
// Creation de la forme
//
procedure TMainForm.FormCreate(Sender: TObject);
var
	SHFileInfo: TSHFileINfo;
begin
    Application.Title := APPLI_TITLE_STR;

    Self.Caption := APPLI_TITLE_STR;

    // Initialise les menus
    Self.MenuInit;

    // Mets l'application dans l'�tat Initial
    Self.FApplicationState := asInitial;
    Self.UpdateMenuState;

    // Cr�e une image list avec les icones correspondant aux extensions de fichiers
    Self.SysImage := TImageList.Create(nil);	// Quel AOwner ???
    Self.SysImage.ShareImages := true;
    Self.SysImage.Handle := SHGetFileInfo('', 0, SHFileInfo, SizeOF(SHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

    // Ajoute les images syst�mes
    Self.ListView.SmallImages := Self.SysImage;

    // Virtual Listview DEBUG
    //Self.ListView.OwnerData := true;
    //Self.ListView.OnData := Self.ListViewData; // Mis dans l'inspecteur

	// Initialise le dialog
    Self.SaveDialog.InitialDir := GetCurrentDir;
    Self.SaveDialog.DefaultExt := SAVEDIALOG_DEFAULT_EXT_STR;
    Self.SaveDialog.Title := SAVEDIALOG_TITLE_STR;
    Self.SaveDialog.Filter := SAVEDIALOG_FILTER_STR;
end;


//
// Destruction de la forme
//
procedure TMainForm.FormDestroy(Sender: TObject);
begin
    Self.SetApplicationState(asInitial);
	Self.SysImage.Free;
end;


//
// Effacer tous les noeuds (fichiers et rep) � partir du noeud node
//
procedure TMainForm.FreeNodes(node : TTreeNode);
var
	i : integer;
begin
	// Detruire les fils en premier
    for i := node.Count - 1 downto 0 do
    	Self.FreeNodes(node.Item[i]);

	// Lib�re les donn�es li� � ce noeud
    TDirectoryNodeData(node.Data).Free;

    // Efface le noeud
    node.Free;
end;


//
// Efface le TV, LV et toutes les infos li�es aux fichiers/rep de la partition
//
procedure TMainForm.Unpopulate();
begin
    // Lib�ration des noeuds dans le treeview
    if Assigned(Self.FRootNode) then
	    Self.FreeNodes(Self.FRootNode);

	Self.FRootNode := nil;
    Self.TreeView.Items.Clear;

    Self.ListView.Clear;
    Self.ListView.Tag := 0;		// Pas de colonne tri�e par d�faut

    Self.StatusBar.Panels[1].Text := '';
end;


// =====================================================================
//	   					GESTION DE L'AFFICHAGE
// =====================================================================


//
// Ajoute une nouvelle ligne au m�mo
//
procedure TMainForm.AddLine(const str: string);
begin
	Self.MemoStdErr.Lines.Add(str);
end;


//
// Ajoute une nouvelle ligne au m�mo avec un saut de ligne
//
procedure TMainForm.WriteLn(const str: string);
begin
	Self.AddLine(str);
	Self.AddLine('');
end;


//
// Affiche les fichiers d'un r�pertoire dans la listview
//
procedure TMainForm.DisplayFilesFromNode(dirdata : TDirectoryNodeData);
var
    i : integer;
begin
    // D�but de la mise � jour
    Self.ListView.Items.BeginUpdate;

    // Remplir le liste view
    Self.ListView.Clear;

    for i := 0 to dirdata.FileCount - 1 do
        Self.AddFileToListView(dirdata.FileData[i]);

    // Fin de la mise � jour
    Self.ListView.Items.EndUpdate;
end;


//
// Ajouter une entr�e dans le liste view
//
procedure TMainForm.AddFileToListView(filedata: TFileItemData);
var
    litem : TListItem;
    typfic: string;
begin
    litem := Self.ListView.Items.Add;
    litem.Data := filedata;		// sauver la r�f�rence sur la structure

    litem.StateIndex := Ord(filedata.Selected);
    // Index = 4 pour l'icone d'un r�pertoire
    litem.ImageIndex := Self.GetExtIconIndex(ExtractFileExt(filedata.FileInfo.FileName), 0, typfic);

    litem.Caption := filedata.FileInfo.FileName;
    litem.SubItems.Add(SizeToRoundedKiloString(filedata.FileInfo.Size));
    //litem.SubItems.Add(GetAttrString(filedata.FileInfo.Attributs));

    litem.SubItems.Add(IntToStr(filedata.FileInfo.FileID));
    //litem.SubItems.Add(IntToStr(filedata.FileInfo.FirstCluster));

    litem.SubItems.Add(FileTimeToString(filedata.FileInfo.WriteDate));
    //Mettre le type de fichier ?
end;


//
// OnData pour la listview virtuelle
//
//procedure TMainForm.ListViewData(Sender: TObject; Item: TListItem);
//var
//	filedata: TFileItemData;
//    typfic: string;
//begin
//	// R�cuperer le fichier du r�pertoire courant
//    filedata := TDirectoryNodeData(Self.TreeView.Selected.Data).FileData[Item.Index];
//    item.Data := filedata; // DEBUG
//    item.StateIndex := Ord(filedata.Selected);
//    item.ImageIndex := Self.GetExtIconIndex(ExtractFileExt(filedata.FileInfo.FileName), 0, typfic);
//    item.Caption := filedata.FileInfo.FileName;
//    item.SubItems.Add(SizeToFloatByteStr(filedata.FileInfo.Size));
//    item.SubItems.Add(IntToStr(filedata.FileInfo.FirstCluster));
//    item.SubItems.Add(FileTimeToString(filedata.FileInfo.WriteDate));
//end;


//
// Mets � jour les changement d'�tat dans la ListView
//
procedure TMainForm.UpdateListView();
var
	i : integer;
begin
    // Empeche la r�actualisation
	Self.ListView.Items.BeginUpdate;

	for i := 0 to Self.ListView.Items.Count - 1 do
    	with Self.ListView do
    		if Ord(TFileItemData(Items[i].Data).Selected) <> Items[i].StateIndex then
				Items[i].StateIndex := Ord(TFileItemData(Items[i].Data).Selected);

    // Permet la r�actualisation
    Self.ListView.Items.EndUpdate;
end;


//
// R�cup�re l'index de l'icone associ�e � l'extension ainsi que son type
//
// Parametres:
//
//
function TMainForm.GetExtIconIndex(fext: String; Attributs: DWORD; var TypeFichier :string): Integer;
var
	SHFileInfo: TSHFileInfo;
begin
	//Il faut le "." avant
    if fext = '' then
    	fext := '.'
    else
        if fext[1] <> '.' then
            fext := '.' + fext;

	// En cas d'erreur
    Result := -1;
    TypeFichier := '';

    // On r�colte les info pour l'extension
	//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/functions/shgetfileinfo.asp
    if SHGetFileInfo(PChar(fext), Attributs, SHFileInfo, SizeOf(TSHFileInfo),
             SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_TYPENAME) <> 0 then
    begin
        TypeFichier := SHFileInfo.szTypeName; //Quel est le type de fichier
        Result := SHFileInfo.iIcon; //index de l'icone dans l'image list du systeme
	end;
end;


procedure TMainForm.UpdateSelectedInfo();
begin
	Self.StatusBar.Panels[1].Text := Format(SELECTED_FILES_SUMMARY_STR,
										    [Self.FSelectedFileCount, SizeTo2DigitByteString(Self.FTotalSelectedSize)]);
end;


procedure TMainForm.OnRestoreEvent(Sender: TObject; const filesize, writtenbytes: int64);
begin
   	TProgressForm(Self.FProgressDlg).Pos64 := Self.FByteSaved + writtenbytes;
    Application.ProcessMessages; 
end;


//
// Remets � jours les infos taille fichiers selectionn�s et nb fichier selectionn�s
//
procedure TMainForm.RecalcSelected();

	//
    // Calcul recursif
    //
    procedure GetSelectSize(node: TTreeNode);
    var
    	i : integer;
        dirdata : TDirectoryNodeData;
    begin
        dirdata := TDirectoryNodeData(node.data);

        // Ajoute les infos du dir courant
        Inc(Self.FSelectedFileCount, dirdata.SelectedCount);
        Inc(Self.FTotalSelectedSize, dirdata.SelectedSize);

        // Les noeud ncUnchecked ne contiennent pas de fichier selectionn�s
        for i := 0 to node.Count - 1 do
        	if node.Item[i].StateIndex <> Ord(ncUnchecked) then
            	GetSelectSize(node.Item[i]);
    end;

begin
	Self.FTotalSelectedSize := 0;
    Self.FSelectedFileCount := 0;
    GetSelectSize(Self.FRootNode);
end;


// -------------------------------------------------------------------------
//							Scanning de la partition
// -------------------------------------------------------------------------


//procedure TMainForm.PopulateTreeview(const cluster: Cardinal);
procedure TMainForm.PopulateTreeview(const clusterid: int64);

    procedure Populate(node: TTreeNode; const dirid: int64);
    var
    	i: smallint;
        res: boolean;
    begin
        Self.CurrentNode := node;
        Self.CurDirData := TDirectoryNodeData(node.data);

        // Selon le resultat du scan on met � jour l'image de la racine
        if dirid = -1 then
	        res := part.ScanRootDirectory()
        else
        	res := part.ScanRelativeDirectory(dirid);

        if not res then
        begin
            // Si faux: erreur lors de la lecture repertoire => Changer l'icone des rep
            //node.ImageIndex := IMAGE_DIR_CLOSE_ERR;
            //node.SelectedIndex := IMAGE_DIR_OPEN_ERR;
        end;

        // Les fils sont ajout�s lors des evenements OnDirectory
        // On fait un traitement recursif sur les fils (sous-rep)
        for i := 0 to node.Count - 1 do
        begin
            // r�cup�rer le cluster du sous rep (propri�t� Data)
            Populate(node.Item[i], TDirectoryNodeData(node.Item[i].Data).DirInfo.FileID);
        end;
    end; // proc
    

var
    rootdata : TDirectoryNodeData;
	rootdir: TCommonFileInfo;
begin
    // Cr�e et renseigne le DirInfoNode racine
	rootdir := TCommonFileInfo.Create;

    rootdata := TDirectoryNodeData.Create(nil, rootdir);

    Self.FRootNode := Self.TreeView.Items.Add(nil, '[Racine]');
    Self.FRootNode.SelectedIndex := IMAGE_ROOT;
    Self.FRootNode.ImageIndex := IMAGE_ROOT;
    Self.FRootNode.StateIndex := ord(ncUnchecked);

    rootdata.AssociateNode(Self.FRootNode);

    // Initialise les evenement du scan
    part.OnFile := Self.OnFileEntry;
    part.OnDirectory := Self.OnDirEntry;

    // Initialiser les compteurs
    Self.TotalFileCount := 0;
    Self.TotalDirCount	:= 0;
    Self.FTotalSize		:= 0;
    Self.FTotalBlockSize:= 0;

	// Lancer la fonction r�cursive
    Populate(Self.FRootNode, clusterid);

    //Form1.MemoStdErr.Lines.Add(Format('%d Fichiers et %d r�pertoires trouv�s', [Self.TotalFileCount, Self.TotalDirCount]));

    // Expand du premier niveau
    Self.FRootNode.Expand(false);
end;


//
// Une entr�e fichier a �t� trouv�e dans le r�pertoire
//
procedure TMainForm.OnFileEntry(Sender: TObject; entry: TCommonFileInfo; var abort: boolean);
begin
	Self.CurDirData.AddFile(entry);
    Inc(Self.TotalFileCount);

    // Calcule la taille
    Inc(Self.FTotalSize, entry.Size);
    Inc(Self.FTotalBlockSize, part.SizeToCluster(entry.Size) * part.ClusterSize);
end;


//
// Nouveau r�pertoire trouv�
//
procedure TMainForm.OnDirEntry(Sender: TObject; entry: TCommonFileInfo; var abort: boolean);
var
	dirdata : TDirectoryNodeData;
    newnode : TTreeNode;
begin
    // V�rifier que c'est bien un rep
    if (entry.FileName = '.') or (entry.FileName = '..') then
    	exit;

	dirdata := TDirectoryNodeData.Create(Self.CurDirData, entry);
    newnode := Self.TreeView.Items.AddChildObject(Self.CurrentNode, entry.FileName, dirdata);
	dirdata.AssociateNode(newnode);

    newnode.ImageIndex := IMAGE_DIR_CLOSE;
    newnode.SelectedIndex := IMAGE_DIR_OPEN;
    newnode.StateIndex := Ord(ncUnchecked);   // ncUnchecked

	// Mettre � jour l'affichage
    Self.FProgressDlg.Text := entry.FileName;
    Application.ProcessMessages;

    Inc(Self.TotalDirCount);
end;


// -------------------------------------------------------------------------
//							Recovery du disque
// -------------------------------------------------------------------------



//
// filename contient le path
//#Todo1 Gerer le fichier de taille nulle => Pas de cluster associ�
//
function TMainForm.RestoreFile(const filename: string; fileinfo: TCommonFileInfo): boolean;
var
    fichier : TFileStream;
    filedeleted: boolean;
begin
	Result := true;
    filedeleted := false;

    try
    	//#Todo2 Verifier l'existence du fichier et agir en cons�quence

        // Creer le fichier
        fichier := TFileStream.Create(filename, fmShareExclusive or fmCreate);
        try
            if not part.RestoreFile(fileinfo, fichier) then
            begin
                Self.AddLine(Format(RECOVER_FILE_ERROR_STR, [filename]));
                Result := False;
            // Comment traiter les erreurs lors de la lecture des cluster ? Evenement ?
            //OnScanError()
            end;
		finally
            // Fermer le fichier
            fichier.Free;
        end;
    except
        on EFCreateError do
        	Result := false;
        //on EWriteError do; // verifier si trait� dans saveChain	
    end; // try

    // Efface le fichier au besoin en cas d'erreur
    if filedeleted then
    	DeleteFile(filename);

    // Si on efface le fichier => ne pas retaurer timestamp et attribut
    if not filedeleted then
    begin
        // Mettre � jour la date et l'heure
        RestoreFileTimeStamp(filename, fileinfo);

        // Mettre � jour les attributs Mettre un mask sur les attributs ?
        FileSetAttr(filename, fileinfo.DOSAttr);
    end;
end;


//
// R�cuperer la taille du cluster
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/base/getdiskfreespace.asp
//
//Windows Me/98/95:  For volumes that are larger than 2 GB, the GetDiskFreeSpace
//function may return misleading values. The function caps the values stored into
//*lpNumberOfFreeClusters and *lpTotalNumberOfClusters so as to never report
//volume sizes that are greater than 2 GB. On volumes that are smaller than 2 GB
//with an actual sectors per cluster greater than 64, *lpSectorsPerCluster will be
//capped at 64 and the values stored into *lpSectorsPerCluster, *lpNumberOfFreeClusters,
//and *lpTotalNumberOfClusters will be incorrect. That is because the operating system
//adjusts the values so that computations with them yield the correct volume size
//with the capped *lpSectorsPerCluster.

// Parametres
// Chemin de sauvegarde
// Noeud
//#Todo2 Ajouter un parametre noeud pour ne pas �tre oblig� de sauver � partir de la racinee
// Calculer le nombre d'octet r�ellement sauv�, l'�tat d'avancement somme des tailles
// m�me si erreur ou fichier non trait� (pour le progressbar)
// => Modifier le calcul du Self.FByteToSave
procedure TMainForm.Recovery(const checkedonly: boolean);
var
    freespace, f1: int64;
begin
    // Calcule la taille de ce qu'il faut sauver
    if checkedonly then
		Self.FByteToSave := Self.FTotalSelectedSize
    else
    	Self.FByteToSave := Self.FTotalSize;

	// V�rifier qu'il y a quelque chose � sauvegarder
    //#Todo3 Attention Calculer le nombre de fichier � sauver plut�t
    if Self.FByteToSave = 0 then
    begin
        MessageDlg(NO_FILE_TO_RECOVER_MSG, mtWarning, [mbOK], 0);
    	exit;
    end;

    //
    MessageDlg(RECOVERY_WARNING_MSG, mtWarning, [mbOK], 0);
    
	// Selectionne un r�pertoire pour le backup
    Self.RestoreDirectory := BrowseDialog(Self.Handle, SELECT_DEST_DIR_STR);
	if Self.RestoreDirectory = '' then
    	exit;	// Pas de r�p selectionn�: => on sort

    Self.RestoreDirectory := IncludeTrailingPathDelimiter(Self.RestoreDirectory);

    // R�cup�rer la taille disponible
    //http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/base/getdiskfreespaceex.asp
    GetDiskFreeSpaceEx(PChar(Self.RestoreDirectory), freespace, f1, nil);

    Self.AddLine(Format(DEST_DRIVE_SPACE_STR,
    					 [SizeTo2DigitByteString(freespace),
                         SizeTo2DigitByteString(Self.FByteToSave)]));	//DEBUG

    // V�rifier qu'il y a assez de place dispo (rajouter 10% par s�curit�)
    if (Self.FByteToSave + (Self.FByteToSave div 10)) > freespace then
    begin
    	MessageDlg(NOT_ENOUGH_SPACE_ON_DEST_MSG, mtError, [mbOK], 0);
        exit;
    end;

    Self.FByteSaved := 0;
    part.OnRestore := Self.OnRestoreEvent;
	Self.MemoStdErr.Lines.Add(START_RECOVERY_MSG);

    //Self.FProgressDlg := TWaitingForm.Create(Self);
    Self.FProgressDlg := TProgressForm.Create(Self);
	TProgressForm(Self.FProgressDlg).Max64 := Self.FByteToSave;
   	Self.FProgressDlg.Caption := RECOVER_DLG_TITLE;
    Self.FProgressDlg.Text := '';
    try
    	Self.FProgressDlg.ShowModalAsync;
	    Self.SaveTree(Self.FRootNode, Self.RestoreDirectory, checkedonly);
    	Self.FProgressDlg.CloseModalAsync;
    finally
        Self.FProgressDlg.Release;
        Self.FProgressDlg := nil;
    end;

    Self.AddLine(Format(RECOVERY_SUMMARY_STR, [Self.FByteSaved, Self.FByteToSave]));
end;


//
// Si le rep du noeud n'exite pas on le cr�e et mettre � jour attr et dates
// Pour tous les fichiers du rep
//	S'il exite demander confirmation pour reecrire
//	Sauver le fichier
//	Si erreur demander si effacer le fichier
// finpour
// Faire la m�me chose pour les fils
//
// Options:
// Save Fichier taille 0 (oui, non, demander)
// Create empty dir (oui, non, demander)
// Fichier incomplet : Ecrire partie valide, Remplir avec une pattern, concatener les parties valide, effacer
// Erreur E/S fichier : Garder le fichier ou effacer
//
// Path contient le chemin complet par ex: C:\Temp\Backup
// Rajouter la gestion du abort et flag pour fichier selectionn�
procedure TMainForm.SaveTree(node: TTreeNode; const path: string; const checkedonly: boolean);
var
	i : smallint;
    filedata: TFileItemData;
	dirdata : TDirectoryNodeData;
    dirname : string;
begin
    dirdata := TDirectoryNodeData(node.Data);

    // V�rifier si on doit g�rer ce r�pertoire
    if checkedonly and (dirdata.Status = ncUnchecked) then
    	exit;

    // Calcule de nom du r�pertoire
    dirname := IncludeTrailingPathDelimiter(path) + dirdata.DirInfo.FileName + '\';

    //#ToDo2 Traiter les r�pertoires nul
    //if (dirdata.FileCount = 0) and not AllowEmptyDir then
    //	exit;

	// Cr�e eventuellement le rep
    if (not checkedonly) or (dirdata.SelectedCount > 0) then
        if not DirectoryExists(dirname) then
            if not ForceDirectories(dirname) then
            begin
                MessageDlg(Format(CANT_CREATE_DIR_ERROR_MSG, [dirname]), mtError, [mbOK], 0);
                exit;
            end
            else
            begin
                // Mettre � jour l'heure pour le r�pertoire
                RestoreDirTimestamp(dirname, dirdata.DirInfo);

                // Mettre � jour les attributs du r�pertoire
                FileSetAttr(dirname, dirdata.DirInfo.DOSAttr);
            end;

    // Restaurer tous les fichiers du r�pertoire
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

        if not FileExists(dirname + filedata.FileInfo.FileName) then
        begin
        	// Effacer l'ancien ou renommer le nouveau ?
            // Soit on sauve ou on passe au suivant

            // Mettre � jour le label
            Self.FProgressDlg.Text := filedata.FileInfo.FileName; 

            if not Self.RestoreFile(dirname + filedata.FileInfo.FileName, filedata.FileInfo) then
            begin
                Inc(Self.FFilesErrorCount);
                Self.AddLine(Format(RECOVER_FILE_ERROR_STR, [filedata.FileInfo.FileName]));
            end;
        end
		else
        	Self.AddLine(Format(SKIP_EXISTING_FILE_STR, [filedata.FileInfo.FileName]));

        Inc(Self.FByteSaved, filedata.FileInfo.Size);
    end; // for

    // On restaure les sous r�pertoires
    for i := 0 to node.Count - 1 do
		Self.SaveTree(node.Item[i], dirname, checkedonly);
end;


// ------------------------------------------------------------------------
//						Gestion des listing fichiers/Rep
// ------------------------------------------------------------------------


//
// selected = true si seuelement les fichiers selectionn�s
// filter = '' si on ne veut pas de filtre sinon une pattern (*.doc par ex)
//
procedure TMainForm.DoSaveFileListing(node: TTreeNode; const selected :boolean; const filter: string);
var
	filelist: TStringList;
	flags: TTreeTraversalFlags;
begin
	if Self.SaveDialog.Execute then
    begin
		filelist := TStringList.Create;

        // Mets les flags selon qu'il faut lire tous les fichiers
        flags := [ttFile, ttRecurse, ttSelected];
        //flags := [ttDir, ttRecurse, ttSelected, ttUnselected]; // Pour les dir
        if not selected then
        	flags := flags + [ttUnselected];

    	Screen.Cursor := crHourGlass;
        Self.Enabled := false;

        // Cr�e la liste
		FileListing(Self.FRootNode, flags, filelist, filter);
		//DirListing(Self.FRootNode, flags, filelist);	// Pour les rep

        try
			filelist.SaveToFile(Self.SaveDialog.FileName);
        except
			MessageDlg(SAVE_LOG_ERROR_MSG, mtError, [mbOK], 0);
        end;
        Self.Enabled := true;
	    Screen.Cursor := crDefault;

		filelist.Free;
    end;
end;

// Meme chose pour les dir

// ------------------------------------------------------------------------
//						Gestion des evenements
// ------------------------------------------------------------------------


//
// Modifie le checkbox du TTreeNode et propage la modif dans l'arbre
//
procedure TMainForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	node : TTreeNode;
begin
	node := Self.TreeView.Selected;
	if Assigned(node) then
    	if  (Button = mbLeft) and (htOnStateIcon in Self.TreeView.GetHitTestInfoAt(X, Y)) then
        begin
        	// Quand on clique sur la case � cocher, le treenode est obligatoirement s�lectionn�
        	//node := Self.TreeView.GetNodeAt(x,y); // Ne sert � rien car clic sur
            //if not Assigned(node) then  // l'icone change le selection
            //	exit;

            //Self.MemoStdErr.Lines.Add('DEBUG: TreeViewMouseDown');
        	case TNodeCheckBoxState(node.StateIndex) of
              ncUnchecked: 	node.StateIndex := Ord(ncChecked);
              ncChecked:	node.StateIndex := Ord(ncUnchecked);
              ncGrayed:		node.StateIndex := Ord(ncChecked);
            end;

            // Change l'�tat du noeud et de ses fils
			SetTreeStatus(node, TNodeCheckBoxState(node.StateIndex));
            PropagateNodeStateChange(node);

            // Recalculer la taille des fichiers selectionn�s
            Self.RecalcSelected;
            Self.UpdateSelectedInfo;

            // Mets � jour le ListView
            Self.UpdateListView;
        end;
end;


//http://msdn.microsoft.com/library/en-us/shellcc/platform/commctls/listview/messages/lvm_setitemstate.asp?frame=true
//http://smeschini.altervista.org/download/netstat32.htm
//
// Evenement MouseDown sur le listview
//
// Permet de v�rifier si l'on � click� dans la checkbox d'un item
//
procedure TMainForm.ListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	litem : TListItem;
    filedata: TFileItemData;
    p: TPoint;
begin
	if Button = mbLeft then
    	if htOnStateIcon in Self.ListView.GetHitTestInfoAt(X, Y) then
        begin
        	litem := Self.ListView.GetItemAt(X, Y);
            if Assigned(litem) then
            begin
            	// On change l'�tat d'un fichier
            	filedata := TFileItemData(litem.Data);
                if filedata.Selected = ncChecked then
                begin
                	filedata.ChangeStatus(ncUnchecked);
                    dec(Self.FSelectedFileCount);
                    dec(Self.FTotalSelectedSize, filedata.FileInfo.Size);
                end
                else
                begin
                	filedata.ChangeStatus(ncChecked);
                    inc(Self.FSelectedFileCount);
                    inc(Self.FTotalSelectedSize, filedata.FileInfo.Size);
                end;

            	// Changer l'�tat de l'item
                if litem.StateIndex = Ord(ncChecked) then
                	litem.StateIndex := Ord(ncUnchecked)
                else
                	litem.StateIndex := Ord(ncChecked);

                litem.Update;
                //Self.Invalidate;

                // V�rifier si le noeud (rep) � chang� d'�tat (aucun/tous les fichier selectionnes)
                if filedata.ParentDir.Node.StateIndex <> Ord(filedata.ParentDir.Status) then
                begin
	                // Mettre � jour les infos FileNode
					filedata.ParentDir.Node.StateIndex := Ord(filedata.ParentDir.Status);

                    // Propager la modif
                    PropagateNodeStateChange(filedata.ParentDir.Node);
                end;

                // Mets � jour l'affichage des infos
                Self.UpdateSelectedInfo;
            end; // if Assigned
        end;

    // Bouton droit = Menu Popup
    if Button = mbRight then
//    	if htOnLabel in Self.ListView.GetHitTestInfoAt(X, Y) then
        begin
            litem := Self.ListView.GetItemAt(X, Y);
            PopLVItemProp.Enabled := Assigned(litem);
            Self.CurListItem := litem; 

            // Calcule la position di Popup en coordonn�e Screen
        	P.X := x;	P.Y := y;
            P := Self.ListView.ClientToScreen(P);
			Self.PopupMenuLV.Popup(P.x, P.y);
	    end;
end;


// ------------------------------------------------------------------------
// 						Gestion des menus selon l'etat
// ------------------------------------------------------------------------

{
-------------------------- Etats de l'appli -------------------------

0) Etat initial
	disque = nil
	part = nil
    pas de noeuds

1) Etat disque selectionn�
	disque <> nil
	part = nil
    pas de noeuds

2) Etat partition selectionn�e
	disque <> nil
	part <> nil
    pas de noeuds

3) Etat partition scann�e
	disque <> nil
	part <> nil
    noeuds dans treeview

Quand on selectionne une nouvelle partition => si partition ok alors
passer � etat disque selectionn� puis etat partition selectionn�e

}

//
// Gestion des transitions entre �tats de l'application
// Met � jour l'�tat des menus
//
procedure TMainForm.SetApplicationState(const state: TApplicationState);
begin
	// Determiner si changement vers un �tat sup�rieur ou inf�rieur
    if state < Self.FApplicationState then
    begin
    	// On passe � un �tat inf�rieur => lib�ration de ressources

        // Passage Etat asPartScanned vers asPartSelected
        if (Self.FApplicationState = asPartScanned) and (state < asPartScanned) then
        begin
        	//Self.AddLine('Passage Etat asPartScanned -> asPartSelected');
            //
            Self.Unpopulate;

            // Infos (partition) de la fenetre dump ne sont plus valable => on ferme
            //#TODO mettre dans StatePartitionSelected ?
            if FormDump.Visible then
                FormDump.Close;

        	Self.FApplicationState := asPartSelected;
        end;

        // Passage Etat asPartSelected vers asDiskSelected
        if (Self.FApplicationState = asPartSelected) and (state < asPartSelected) then
        begin
        	//Self.AddLine('Passage Etat asPartSelected -> asDiskSelected');

       	    // Lib�ration de la partition
            FreeAndNil(part);

        	Self.FApplicationState := asDiskSelected;
        end;

        // Passage Etat asDiskSelected -> asInitial
        if (Self.FApplicationState = asDiskSelected) and (state < asDiskSelected) then
        begin
        	//Self.AddLine('Passage Etat asDiskSelected -> asInitial');

		    disque := nil;
        	Self.FApplicationState := asInitial;
        end;
    end;

   	// Mets � jour l'�tat
    Self.FApplicationState := state;

    // Mets � jour les menus en fonction de l'�tat
    Self.UpdateMenuState;
end;


//
// Initialise les menus pour la gestion des �tats
//
procedure TMainForm.MenuInit();

	//
    // Calcule l'�tat minimum d'un MenuItem en fonction de ses sous-menus
    //
    procedure CalcMainMenuState(item: TMenuItem);
    var
    	i: integer;
        minstate: integer;
    begin
    	// On ne modifie les items ayant des sous-menus
    	if item.Count = 0 then
        	exit;

    	minstate := Ord(High(TApplicationState));

        // Calcule l'�tat minimal pour l'item
        For i := 0 to item.Count - 1 do
        begin
	    	if item.Items[i].Count <> 0 then
            begin
            	// Calcule l'�tat du sous menu
                CalcMainMenuState(item.Items[i]);
            end;

            if (item.Items[i].Tag <= minstate) and (item.Items[i].Tag > Ord(asNotDefined)) then
                minstate := item.Items[i].Tag;
        end;

        // Mettre � jour l'item
        item.Tag := minstate;
    end;

var
	i: integer;
begin
	// ----- MenuItems disponibles � l'�tat asInitial ------
    Self.ItemSelectDrive.Tag		:= Ord(asInitial);
    Self.ItemExitApp.Tag			:= Ord(asInitial);

    // Menu About
    Self.ItemHelp.Tag				:= Ord(asInitial);
    Self.ItemAbout.Tag				:= Ord(asInitial);
    Self.ItemOption.Tag				:= Ord(asInitial);

   	// ----- MenuItems disponibles � l'�tat asDiskSelected ------
    Self.ItemDriveInfo.Tag			:= Ord(asDiskSelected);
    Self.ItemDumpDrive.Tag			:= Ord(asDiskSelected);
    Self.ItemSelectPartition.Tag	:= Ord(asDiskSelected);
    Self.ItemSearchPart.Tag  		:= Ord(asDiskSelected);
    Self.ItemPartList.Tag 			:= Ord(asDiskSelected);

  	// ----- MenuItems disponibles � l'�tat asPartSelected ------
    Self.ItemPartInfo.Tag			:= Ord(asPartSelected);
    Self.ItemDumpCluster.Tag		:= Ord(asPartSelected);
    Self.ItemPartRootScan.Tag 		:= Ord(asPartSelected);
    Self.ItemPartialScan.Tag  		:= Ord(asPartSelected);

  	// ----- MenuItems disponibles � l'�tat asPartScanned ------
	// Le Menu Recover est accessible � l'�tat asPartScanned
    for i := 0 to Self.MainMenu.Items[2].Count - 1 do
    	Self.MainMenu.Items[2].Items[i].Tag := Ord(asPartScanned);

	// Le Menu Tools est accessible � l'�tat asPartScanned
    for i := 0 to Self.MainMenu.Items[3].Count - 1 do
    	Self.MainMenu.Items[3].Items[i].Tag := Ord(asPartScanned);

    // Mettre � jour la barre des menus
    for i := 0 to Self.MainMenu.Items.Count - 1 do
		CalcMainMenuState(Self.MainMenu.Items[i]);
end;


//
// Met les menus dans l'etat FApplicationState
// Activer l'�tat N revient � mettre Enable � true pour tous les menuitems
// dont le Tag est <= N
//
procedure TMainForm.UpdateMenuState();

	procedure MenuItemState(const item: TMenuItem);
    var
        i: integer;
    begin
    	item.Enabled := (ord(Self.FApplicationState) >= item.Tag) and (item.Tag > 0);

        // Fait la m�me chose pour les sous-menus
        for i := 0 to item.Count - 1 do
            MenuItemState(item.Items[i]);
    end;

var
	i: integer;
begin
	for i:= 0 to Self.MainMenu.Items.Count - 1 do
    	MenuItemState(Self.MainMenu.Items[i]);
end;


// ------------------------------------------------------------------------
//						  Affiche les propri�t�s
// ------------------------------------------------------------------------


//
// Affiche les propri�t�s des fichiers ou r�pertoires
//
procedure TMainForm.DisplayFileProp(fileinfo: TCommonFileInfo);
var
    infostr: TStrings;
begin
    if Assigned(fileinfo) then
    begin
        infostr := TStringList.Create;
        fileinfo.GetFileProperties(infostr);
        PropertyForm.Execute(Format(FILE_PROPERTIES_TITLE_STR, [fileinfo.FileName]), infostr);
        infostr.Free;
    end;
end;


// ------------------------------------------------------------------------
//							Gestion des Events menus
// ------------------------------------------------------------------------

//
// Selection du disque � utiliser
//
procedure TMainForm.ItemSelectDriveClick(Sender: TObject);
var
	tmpdrv: TGenericDrive;
begin
	tmpdrv := FormSelectDrive.Execute(DRIVE_SELECT_TITLE_STR);

    // verifier si un disque a �t� selectionn�
    if Assigned(tmpdrv) then
    begin
    	//#Todo2 Effacer eventuellement DriveDump
        //if FormDump.Visible then
        //    FormDump.Close;

        // ATTN La destruction des disque se faut dans DriveSelect !!!!!
		disque := tmpdrv;

    	// Mettre � jour les infos disques
        Self.Caption := APPLI_TITLE_STR + ' (' + disque.ModelNumber +
				        ' - ' + SizeTo2DigitByteString(disque.Size) + ')';

        // Ajoute des infos au log
        Self.AddLine(Format(SELECT_DRIVE_REPORT_STR, [disque.ModelNumber,
        				    SizeTo2DigitByteString(disque.Size)]));

        // Reste ou passe � l'�tat DiskSelected si un disque a �t� s�lectionn�
        Self.SetApplicationState(asDiskSelected);
    end;
end;


//
// Menu Selection de la partition
//
procedure TMainForm.ItemSelectPartitionClick(Sender: TObject);
var
	tmppart : TGenericPartition;
    chsinfo: TCHS;
begin
	if Assigned(disque) then
    begin
    	// Ouvre la fenetre de selection des partitions 
        tmppart := FormSelectPart.Execute(disque);

        // Verifier si la selection s'est d�roul�e sans erreur
		if Assigned(tmppart) then
        begin
            // On d�truit l'�ventuelle ancienne partition
		    FreeAndNil(part);
            part := tmppart;

            // Affiche des infos dans le meno
            disque.LBAToCHS(chsinfo, part.FirstSecOfPart);
            Self.AddLine(Format(SELECT_PART_REPORT_STR, [part.FileSysName,
                                SizeTo2DigitByteString(int64(part.TotalSectors) * int64(part.SectorSize)),
                                part.FirstSecOfPart, chsinfo.Cylinder, chsinfo.Head,
                                chsinfo.Sector]));

        	// On passe ou rest � l'etat Partition Selectionn�e
            Self.SetApplicationState(asPartSelected);
        end;
    end;
end;


//
// Affiche la liste des partitions du disque selectionn�
//
procedure TMainForm.DumpPartitionList();
var
    chs : TCHS;
	i: integer;
    str: string;
begin
    // Charger la liste des partitions
    disque.ReadMBR;

    if disque.PartitionCount = 0 then
    begin
		Self.AddLine(NO_PART_DETECTED_STR)
    end
    else
    begin
        Self.AddLine(DUMP_PART_HEADER_STR);

        // Affiche les infos sur les partions
        for i := 0 to disque.PartitionCount - 1 do
        begin
            Self.AddLine('');	// Retour � la ligne

            // R�cup�re le CHS du 1er secteur
        	disque.LBAToCHS(chs, disque.PartitionInfo[i].FStartingSector);

            Self.AddLine(Format(HEADER_PART_NUM_STR, [i]));
            Self.AddLine('');	// Retour � la ligne

            Self.AddLine(Format(PART_TYPE_INFO_STR, [disque.PartitionInfo[i].FTypeName, disque.PartitionInfo[i].FPartitionType]));

            if disque.PartitionInfo[i].FActivePartition then
	           	str := 'Active '
            else
            	str := '';

            if disque.PartitionInfo[i].FPrimaryPartition then
                str := str + 'Primary'
            else
                str := str + 'Logical';
            Self.AddLine(PART_STATUS_STR + str);

            //
			Self.AddLine(Format(START_SECTOR_STR, [disque.PartitionInfo[i].FStartingSector, chs.Cylinder, chs.Head, chs.Sector]));
            Self.AddLine(Format(SECTOR_COUNT_STR, [disque.PartitionInfo[i].FSectorCount]));
            Self.AddLine(Format(PART_SIZE_STR, [SizeTo2DigitByteString(int64(disque.PartitionInfo[i].FSectorCount) * int64(disque.BytesPerSec))]));
        end;
    end;
end;


//
// Menu Partition Info selectionn�
//
procedure TMainForm.ItemPartInfoClick(Sender: TObject);
var
	partinfo: TStringList;
begin
	if Assigned(part) then
    begin
        partinfo := TStringList.Create;
        part.GetPartitionProperties(partinfo);
        PropertyForm.Execute(PARTITION_INFO_STR, partinfo);
        partinfo.Free;
    end;
end;


//
// Dump Hexa des cluster /secteur de la partition
//
procedure TMainForm.ItemDumpClick(Sender: TObject);
begin
	if (Sender = Self.ItemDumpDrive) then
	    FormDump.Execute(disque, nil)
    else
	    FormDump.Execute(disque, part);
end;


//
// Scan une partition FAT32 pour r�cup�rer les fichiers
//
procedure TMainForm.ItemPartScanClick(Sender: TObject);
var
	clusterstr: string;
    clusternum: Cardinal;
begin
	// Verifier si Partial ou From Root Scan
	if (Sender = Self.ItemPartialScan) then
    begin
	    // Partial scan => demander le num�ro de cluster
        clusterstr := IntToStr(part.MinValidClust);

        // Entr�e du num�ro de cluster
        if not InputQuery(ENTER_CLUSTER_TITLE_STR, Format(ENTER_ROOT_CLUSTER_MSG,
        				  [part.MinValidClust,part.MaxValidClust]), clusterstr) then
            Exit;

        // Verification de la validit� du cluster
        try
            clusternum := StrToInt(clusterstr);
        except
            clusternum := 0;
        end;

        if (clusternum < part.MinValidClust) or (clusternum > part.MaxValidClust) then
        begin
            ShowMessage(INVALID_CLUSTER_NUM_ERROR_MSG);
            exit;
        end;
    end;

	//#DEBUG il faut demander � l'utilisateur ou via options
     if (part is TFAT32Partition) then
	    (part as TFAT32Partition).FATReadFlag := rmMirrorFirst;

    // On efface au pr�alable les anciens noeuds
    Self.Unpopulate;

    Self.FProgressDlg := TWaitingForm.Create(Self);
    Self.FProgressDlg.Caption := SCAN_PART_DLG_TITLE;
    Self.FProgressDlg.Text := SCAN_PART_DLG_TEXT;
    try
    	Self.FProgressDlg.ShowModalAsync;

		// Remplir le TV
        if (Sender = Self.ItemPartialScan) then
        begin
		    Self.AddLine(Format(SCAN_PART_STR, [clusternum]));
        	Self.PopulateTreeview(int64(clusternum))
        end
        else
        begin
		    Self.AddLine(SCAN_PART_ROOT_STR);
        	Self.PopulateTreeview(-1);	// ScanFromRoot
        end;

    	Self.FProgressDlg.CloseModalAsync;
    finally
        Self.FProgressDlg.Release;
        Self.FProgressDlg := nil;
    end;

    Self.AddLine(Format(SCAN_REPORT_STR,[Self.TotalFileCount, Self.TotalDirCount,
									    SizeTo2DigitByteString(Self.FTotalSize)]));
    Self.AddLine(SCAN_BLOCKSIZE_STR + SizeTo2DigitByteString(Self.FTotalBlockSize));

    // Passe dans l'�tat Partition Scann�e (asPartScanned)
    Self.SetApplicationState(asPartScanned);
end;


//
//  Ouvre completement l'arbre
//
procedure TMainForm.ItemExpandTreeClick(Sender: TObject);
begin
	Self.TreeView.FullExpand;
end;


//
//  Ferme completement l'arbre
//
procedure TMainForm.CollapseTree1Click(Sender: TObject);
begin
	Self.TreeView.FullCollapse;
end;


//
// Affiche les infos du disque
//
procedure TMainForm.ItemDriveInfoClick(Sender: TObject);
var
	dskinfo: TStringList;
begin
	dskinfo := TStringList.Create;
	disque.GetDriveProperties(dskinfo);
    PropertyForm.Execute(Format(DRIVE_INFO_STR, [disque.ModelNumber]), dskinfo);
    dskinfo.Free;
end;


//
// Selection d'un nouveau noeud
//
procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
	dirdata : TDirectoryNodeData;
begin
	if Self.TreeView.Selected = nil then
    	exit;

	//Self.MemoStdErr.Lines.Add('DEBUG: TreeViewChange');

	dirdata := Self.TreeView.Selected.Data;
    // Afficher le nb rep, taille de l'arbre

    // Remplir le liste view
    Self.DisplayFilesFromNode(dirdata);

    // Mets � jour le nombre fichiers dans le rep
    Self.StatusBar.Panels[0].Text := Format(CURRENT_DIR_SUMMARY_STR, [dirdata.FileCount,
    										SizeTo2DigitByteString(dirdata.Size)]);
end;


//
// Click sur une colonne du LV => Tri du LV sur la colonne selectionn�e
//
procedure TMainForm.ListViewColumnClick(Sender: TObject;
  Column: TListColumn);
var
	ind: integer;

    //
    // Fonction Priv�e pour trier les elements d'une ListView
    //
    // Param contient de num�ro de colonne � trier (commence � 1) et le sens (positif = croissant)
    //
    function SortProcNew(Item1, Item2: TListItem; ParamSort: integer): integer; stdcall;
    var
		filedata1, filedata2: TFileItemData;
        senscroissant : boolean;
    begin
    	senscroissant := (ParamSort > 0);
        ParamSort := abs(ParamSort);

        // Les num�ros de colonnes commencent � 1
        // Name, Size, FileID, Date
        case ParamSort of
            1 : // Colonne Name, tri alpha num
            	begin
	                Result := lstrcmp(PChar(TListItem(Item2).Caption), PChar(TListItem(Item1).Caption));
	            end;
            2 : // Colonne Taille, tri num sur int64;
            	begin
                    filedata1 := TFileItemData(Item1.Data);
                    filedata2 := TFileItemData(Item2.Data);

                    if filedata1.FileInfo.Size > filedata2.FileInfo.Size then
                    	Result := 1
                    else
                    	Result := -1;
                end;
            3 : // Colonne FileID, tri sur num sur l'ID du fichier
            	begin
                    filedata1 := TFileItemData(Item1.Data);
                    filedata2 := TFileItemData(Item2.Data);

                    if filedata1.FileInfo.FileID > filedata2.FileInfo.FileID then
                    //if filedata1.FileInfo.FirstCluster > filedata2.FileInfo.FirstCluster then
                    	Result := 1
                    else
                    	Result := -1;
                end;
            4 : // Colonne Date;
            	begin
                    filedata1 := TFileItemData(Item1.Data);
                    filedata2 := TFileItemData(Item2.Data);

                	Result := CompareFileTime(filedata1.FileInfo.WriteDate, filedata2.FileInfo.WriteDate);
                end;
        end; // Case of

        if not senscroissant then
            Result := - Result;
    end;

    
begin
    // r�cup�re le num�ro de la colonne s�lectionn�e
    ind := Column.Index;

    // verifie si la colonne est d�ja tri�e
    // ListView.Tag contient le num de la colonne tri� et le sens (signe)
    if (abs(Self.ListView.Tag) - ind) = 1 then
        // inverse le tri de la colonne
        Self.ListView.Tag := (-Self.ListView.Tag)
    else
       // Trie ascendant de la nouvelle colonne
       Self.ListView.Tag := ind + 1;

    // Trie le ListView
    Self.ListView.Items.BeginUpdate;
    Self.ListView.CustomSort(@SortProcNew, Self.ListView.Tag);
    Self.ListView.Items.EndUpdate;
end;


//
// Menu Recup�ration des fichiers selectionn�s
//
procedure TMainForm.ItemRecoverSelectedFilesClick(Sender: TObject);
begin
	Self.Recovery(true);
end;

//
// Sauvegarde tous les fichiers
//#Todo3 Regrouper avec ItemRecoverSelectedFilesClick au dessus
procedure TMainForm.ItemRecoverAllClick(Sender: TObject);
begin
	Self.Recovery(false);
end;


procedure TMainForm.ItemExitAppClick(Sender: TObject);
begin
	Self.Close;
end;


//
// Confirmation de la sortie
//
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := MessageDlg(CONFIRM_EXIT_MSG, mtConfirmation, [mbYes,mbNo], 0) = mrYes;
end;


//
// Efface le memo
//
procedure TMainForm.ItemClearLogClick(Sender: TObject);
begin
	Self.MemoStdErr.Clear;
end;


//
// Sauve le contenu du memo
//
procedure TMainForm.ItemSaveLogClick(Sender: TObject);
begin
	if Self.SaveDialog.Execute then
        try
            Self.MemoStdErr.Lines.SaveToFile(Self.SaveDialog.FileName);
        except
			MessageDlg(SAVE_LOG_ERROR_MSG, mtError, [mbOK], 0);
        end;
end;


//
// S�lectionne tous le meno
//
procedure TMainForm.PopItemSelectAllClick(Sender: TObject);
begin
	Self.MemoStdErr.SelectAll;
end;


//
// Copie le texte s�l�ctionn� du m�no dans le clipboard
//
procedure TMainForm.PopItemCopyClick(Sender: TObject);
begin
	Self.MemoStdErr.CopyToClipboard;
end;


//
// List tous les fichiers
//
procedure TMainForm.ItemListFilesClick(Sender: TObject);
begin
	if Assigned(Self.FRootNode) then
		Self.DoSaveFileListing(Self.FRootNode, false, '');
end;


//
// List tous les fichiers selectionn�s
//#Todo3 regrouper avec ItemListFilesClick
procedure TMainForm.ItemListSelFilesClick(Sender: TObject);
begin
	if Assigned(Self.FRootNode) then
		Self.DoSaveFileListing(Self.FRootNode, true, '');
end;


//
// Event: Demande affichage propri�te fichier seulement
//
procedure TMainForm.PopItemPropClick(Sender: TObject);
var
	fileinfo: TCommonFileInfo;
begin
	//#Todo Regrouper avec Popup TV + test sur sender
    // Determiner l'objet concern�
    if (Sender = PopLVItemProp) then
    begin
		if Assigned(Self.CurListItem) then
    		fileinfo := TFileItemData(Self.CurListItem.Data).FileInfo;
    end
    else
    begin
    	if Assigned(Self.CurTreeNode) then
        	fileinfo := TDirectoryNodeData(Self.CurTreeNode.Data).DirInfo;
    end;

    // Affiche la boite de dialogue
    Self.DisplayFileProp(fileinfo);
end;


//
// Event demande affichage Propri�t�s Dir/File
//
procedure TMainForm.ItemPropClick(Sender: TObject);
var
	node: TTreeNode;
    fileinfo: TCommonFileInfo;
begin
	if Assigned(Self.ActiveControl) then
    begin
    	fileinfo := nil;

    	if Self.ActiveControl is TListView then
        	if Assigned(Self.ListView.Selected) then
            	fileinfo := TFileItemData(Self.ListView.Selected.Data).FileInfo;

    	if Self.ActiveControl is TTreeView then
        begin
        	node := TTreeView(Self.ActiveControl).Selected;
        	if Assigned(node) then
            	fileinfo := TDirectoryNodeData(node.Data).DirInfo;
		end;

		Self.DisplayFileProp(fileinfo);
	end;
end;


procedure TMainForm.ItemPartListClick(Sender: TObject);
begin
	Self.DumpPartitionList;
end;


//
// Affiche le fichier d'aide
//
procedure TMainForm.ItemHelpClick(Sender: TObject);
begin
	// Ouvre le fichier d'aide
    Shellexecute(0, 'OPEN', APPLI_CHM_FILENAME, nil, nil, SW_SHOW);
end;


//
// Affiche la fenetre des Options
//
procedure TMainForm.ItemOptionClick(Sender: TObject);
begin
	FormOptions.Execute;
end;


//
// Affiche la boite About
//
procedure TMainForm.ItemAboutClick(Sender: TObject);
begin
	FormAbout.ShowModal;
end;


//
// Event: Menu droit sur le TV => affiche le menu si n�cessaire
//
procedure TMainForm.TreeViewContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
    p: TPoint;
begin
	Self.CurTreeNode := Self.TreeView.GetNodeAt(MousePos.X, MousePos.Y);

    if Assigned(Self.CurTreeNode) then
    begin
        // Calcule la position du Popup en coordonn�e Screen
        P.X := MousePos.X;	P.Y := MousePos.Y;
        P := Self.TreeView.ClientToScreen(P);
        Self.PopupMenuTV.Popup(P.x, P.y);
    end;
end;


//
// Evenement OnPartition lors de la recherche de partition
//
procedure TMainForm.OnPartition(Sender: TObject; pinfo: PPartitionInfo; var abort: boolean);
var
	chs: TCHS;
begin
    disque.LBAToCHS(chs, pinfo.FStartingSector);

    Self.AddLine(Format(ONPARTITIONINFO_STR,
                 [pinfo.FTypeName,
                 SizeTo2DigitByteString(int64(pinfo.FSectorCount) * int64(disque.BytesPerSec)),
                 chs.Cylinder, chs.Head, chs.Sector, pinfo.FStartingSector]));
end;


//
// Evenement OnPartition lors de la recherche de partition
//
procedure TMainForm.OnSearchPartProgress(Sender: TObject; pos: Cardinal; var abort: boolean);
begin
	(Self.FProgressDlg as TElapsedProgressForm).Position := pos;
    abort := Self.FProgressDlg.Aborted; 
    Application.ProcessMessages; 
end;


//
// Demande de recherche de partitions sur le disque
//
procedure TMainForm.ItemSearchPartClick(Sender: TObject);
var
	partscan: TPartitionScanner;
begin
	partscan := TPartitionScanner.Create(disque);
    partscan.OnPartition := OnPartition;
    partscan.OnProgress := OnSearchPartProgress;

    Self.FProgressDlg := TElapsedProgressForm.Create(Self);
    Self.FProgressDlg.Caption := SEARCH_PART_DLG_TITLE;
    (Self.FProgressDlg as TElapsedProgressForm).Max := integer(partscan.Max);
	(Self.FProgressDlg as TElapsedProgressForm).Smooth := true;
    (Self.FProgressDlg as TElapsedProgressForm).AllowAbort := true;
    try
        Self.WriteLn(PARTITIONFOUND_STR);
        Self.FProgressDlg.ShowModalAsync;
    	partscan.Execute;
        Self.FProgressDlg.CloseModalAsync;
        Self.AddLine(SEARCHPARTENDED_STR);
    finally
        Self.FProgressDlg.Release;
        Self.FProgressDlg := nil;
        partscan.Free;
    end;
end;


end.
// --------------------------- Fin de l'unit� -----------------------------
