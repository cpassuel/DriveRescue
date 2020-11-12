unit TreeListManager;

interface

Uses
	Windows, Classes, ComCtrls, StrUtils,
    CommonFileInfo;


type


// StateIndex = 0 ne fonctionne pas => rajout un premier element qui ne sert pas
TNodeCheckBoxState = (ncUnused, ncUnchecked, ncChecked, ncGrayed);


//
// Structures utilisées dans les propriétés Data des TListItem et TTreeNode
//
TFileItemData = class;

TDirectoryNodeData = class
private
	function GetFileData(indice: integer): TFileItemData;
    function GetFileCount: Cardinal;
protected
    FParentDir: 	TDirectoryNodeData;
    FDirInfo:		TCommonFileInfo;
    FSelectStatus:	TNodeCheckBoxState;	// mettre une enum (checked, nochecked, grayed) TNodeCheckBoxState
	FFileList:		TList;
    //FSubDirList:	TList;
    //FTreeFileCount:	Cardinal;
    //FTreeDirCount:	Cardinal;
    FSelectedCount:	Cardinal;		// Nombre de fichiers selectionnés
    FSelectedSize:	int64;			// Tailles des fichiers selectionnés
    FDirectorySize:	int64;			// Taille des fichiers dans le rep
    FTreeSize:		int64;			// Taille des sous rep
    FNode:			TTreeNode; 		// noeud du treenode associé (permet d'avoir les fils)
    // --------------
	function CalcFilesStatus: TNodeCheckBoxState;
public
    constructor Create(parent: TDirectoryNodeData; dirinfo: TCommonFileInfo);
    destructor Destroy; override;
    procedure AddFile(finfo: TCommonFileInfo);
    procedure AssociateNode(node : TTreeNode);
    function GetNodePath: string;
    procedure CalcTreeSize;
    procedure SelectFiles;
    procedure UnselectFiles;
    procedure UpdateFileStatus(filedata: TFileItemData; const newstatus: TNodeCheckBoxState);
    // -------------------------
    property DirInfo: TCommonFileInfo read FDirInfo;
    property FileCount: Cardinal read GetFileCount;
    property SelectedCount: Cardinal read FSelectedCount;
    property Size: int64 read FDirectorySize;
    property TreeSize: int64 read FTreeSize;
    property SelectedSize: int64 read FSelectedSize;
    property Status: TNodeCheckBoxState read FSelectStatus;
    property Parent: TDirectoryNodeData read FParentDir;
    property FileData[indice: integer]: TFileItemData read GetFileData;
    property Node: TTreeNode read FNode;
    //property Directory: string read GetDirectory;
end;


//
// Info sur un fichier, relié au TDirectoryNodeData qui le contient
//
TFileItemData = class
private
protected
	FParentDir:		TDirectoryNodeData;
    FFileInfo:		TCommonFileInfo;
    //FFileInfo:		TCommonFileInfo;
	FSelectStatus:	TNodeCheckBoxState;
    //----------------------
    procedure SetSelectStatus(status: TNodeCheckBoxState);
public
    constructor Create(Parent: TDirectoryNodeData; finfo: TCommonFileInfo);
    destructor Destroy; override;
    procedure ChangeStatus(status: TNodeCheckBoxState);
    //
    //--------------
    property Selected: TNodeCheckBoxState read FSelectStatus {write SetSelectStatus};
    property FileInfo: TCommonFileInfo read FFileInfo;
    property ParentDir: TDirectoryNodeData read FParentDir;
end;


//
//
{TTreeManager = class
private
protected
	FRoot:	TFolderContainer;
public
  constructor Create(root: TFolderContainer); override;
  destructor Destroy; override;
  procedure FileMatch
  procedure Search
end;}


// Comment mettre à jour FFolder quand on ajoute un fichier à un folder ?
// Le Fichier s'ajoute de lui même au Folder dans AddToFolder() ?
{TFileContainer = class
    private
    protected
    public
        function GetFolder(): TFolderContainer; virtual; abstract;
end;}


// Gestion de la racine
// Mettre l'objet dans la structure ???
// Mise à jour de Parent quand on ajoute un Folder à un Folder ?
{TFolderContainer = class
    private
    protected
        function GetParent(): TFolderContainer; virtual; abstract;
        function GetFileCount(): Cardinal; virtual; abstract;
        function GetFolderIndex(const ind: Cardinal): TFolderContainer; virtual; abstract;
    public
        constructor Create; override;
        destructor Destroy; override;
        function GetFolderCount(): Cardinal; virtual; abstract;
        //procedure AddFile(fincont: TFileContainer);
        //procedure AddFolder(foldcont: TFolderContainer);
        //procedure RemoveFile() ?
        //procedure RemoveFolder() ?
        // ----
        property Parent: TFolderContainer read GetParent;
        property FolderCount: Cardinal read GetFolderCount;
        property FileCount: Cardinal read GetFileCount;
        property Folder[ind: Cardinal]: TFolderContainer read GetFolderIndex;
        //property File[ind: Cardinal]: read GetFileCount;
end;}


// Gestion du parcours d'arbre ttNoSubDir
TTreeTraversal = (ttFile, ttDir, ttRecurse, ttSelected, ttUnselected, ttGrayed, ttUseFilter);
TTreeTraversalFlags = set of TTreeTraversal;
//TTreeOccurence = procedure(Sender: TObject; var abort: boolean) of object;

TTraversalCallback = procedure(Sender: TObject; var abort: boolean);

procedure TreeTraversal(Node: TTreeNode; const flags : TTreeTraversalFlags; OnOccur: TTraversalCallback; const filter: string);



//
procedure PropagateNodeStateChange(node: TTreeNode);
procedure SetTreeStatus(node : TTreeNode; const status: TNodeCheckBoxState);


//
// Fonctions exportées
//
procedure FileListing(node: TTreeNode; const flags: TTreeTraversalFlags; liststr: TStringList; const filter: string);
procedure DirListing(node: TTreeNode; const flags: TTreeTraversalFlags; liststr: TStringList);

// Fonctions pour le matching de nom de fichier
function MatchesSpec(const FileName, Specification: string): boolean;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

Uses
	SysUtils, ConversionUnit;


// ------------------------------------------------------------------------
//							Gestion des listing
// ------------------------------------------------------------------------


var
	TmpFileList:		TStrings;	// Variable temp pour stocker la TstringList passée en param
    TmpListTotalSize:	int64;
    TmpListFileCount:	Cardinal;

//
//
//
procedure FileInfoHeader(headstr: TStrings);
var
	line0, line1: string;
begin
	// Ajouter le numéro de cluster
    line0 := '  FileId ';
    line1 := '-------- ';

    // Ajouter la taille
    line0 := line0 + '           Size ';
    line1 := line1 + '--------------- ';
    //line0 := line0 + '      Size ';
    //line1 := line1 + '---------- ';

    // Ajouter les fichiers
    line0 := line0 + 'Attr  ';
    line1 := line1 + '----- ';

    line0 := line0 + '     Write Date     ';
    line1 := line1 + '------------------- ';

    // Mettre la date
    //line0 := line0 + 'Date     ';
    //line1 := line1 + '-------- ';

    // Mettre l'heure
    //line0 := line0 + 'Time     ';
    //line1 := line1 + '-------- ';

	// Ajouter le nom
    line0 := line0 + 'Name';
    line1 := line1 + '--------------------------------------';

    headstr.Add(line0);
    headstr.Add(line1);
end;


procedure FileInfoSummary(infostr: TStrings);
begin
	infostr.Add('');
	infostr.Add('-----------------------------------------------------------');
    infostr.Add(Format(FILE_LIST_SUMMARY_STR, [TmpListFileCount,
											   SizeTo2DigitByteString(TmpListTotalSize)]));
end;


//
// PAa d'info rep avec TFileInfo !!!!! => Rajouter le path ???
//
function FileInfoStr(const path: string; fileinfo: TCommonFileInfo): string;
begin
	// Voir ou mettre les espaces entre les colonnes
	// Ajouter le numérro de cluster
    Result := Format('%8d ',[fileinfo.FileID]);
    //Result := Format('%8d ',[fileinfo.FirstCluster]);

    //#Todo2 changer la largeur de la colonne
    // Ajouter la taille
    Result := Result + RightStr('               ' + Int64ToThousandSepString(fileinfo.Size), 15) + ' ';
    //Result := Result + Format('%8d ',[fileinfo.Size]);

    // Ajouter les fichiers
    Result := Result + fileinfo.GetDosAttrString + ' ';

	//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/dosdatetimetofiletime.asp
	//FileTimeToSystemTime(fileinfo.WriteDate, syswritedate);
    //writedate := SystemTimeToDateTime(syswritedate);
    //function DateTimeToStr(DateTime: TDateTime): string;
    //procedure DateTimeToString(var Result: string; const Format: string; DateTime: TDateTime);

    // Mettre la date
	Result := Result + FileTimeToString(fileinfo.WriteDate) + ' ';

    // Mettre l'heure
	// 'hh:nn:ss'

	// Ajouter le nom
    Result := Result + path + fileinfo.FileName;
end;


//
// Fichier: Cluster, Taille, Attribut, Nom, Dir+Nom, date, heure
//
procedure FileListing(node: TTreeNode; const flags: TTreeTraversalFlags;
					  liststr: TStringList; const filter: string);

	//
    //
    //
	procedure OnListFile(Sender: TObject; var abort: boolean);
    begin
    	if Sender is TFileItemData then
        begin
            TmpFileList.Add(FileInfoStr(TFileItemData(Sender).ParentDir.GetNodePath,
                        	   			TFileItemData(Sender).FFileInfo));

            Inc(TmpListTotalSize, TFileItemData(Sender).FFileInfo.Size);
            Inc(TmpListFileCount);
        end;
    end;

begin
    TmpListTotalSize := 0;
    TmpListFileCount :=	0;

	// met le parametre dans une variable globale pour eviter les pb dans OnListFile
	TmpFileList := liststr;

    // Ajouter l'entête
	FileInfoHeader(TmpFileList);

    if filter <> '' then
		TreeTraversal(node, flags + [ttUseFilter], @OnListFile, filter)
    else;
		TreeTraversal(node, flags, @OnListFile, '');

    // Ajouter le summary
	FileInfoSummary(TmpFileList);
end;



//
// En tête pour le listing des rép
//
procedure DirInfoHeader(headstr: TStrings);
var
	line0, line1: string;
begin
	// Ajouter le numérro de cluster
    line0 := '  FileId ';
    line1 := '-------- ';

    // Ajouter les fichiers
    line0 := line0 + 'Attr  ';
    line1 := line1 + '----- ';

    // Mettre la date
    line0 := line0 + '     Write Date     ';
    line1 := line1 + '------------------- ';

	// Ajouter le nom
    line0 := line0 + 'Directory';
    line1 := line1 + '--------------------------------------';

    headstr.Add(line0);
    headstr.Add(line1);
end;



function DirInfoStr(const path: string; dirinfo: TCommonFileInfo): string;
begin
	// Voir ou mettre les espaces entre les colonnes
	// Ajouter le numérro de cluster
    Result := Format('%8d ',[dirinfo.FileID]);
    //Result := Format('%8d ',[dirinfo.FirstCluster]);

    // Ajouter les attibuts
    Result := Result + dirinfo.GetDosAttrString + ' ';
    //Result := Result + GetAttrString(dirinfo.Attributs) + ' ';

    // Mettre la date
	Result := Result + FileTimeToString(dirinfo.WriteDate) + ' ';

	// Ajouter le nom du rep
    Result := Result + path + dirinfo.FileName;
end;


//
//
//
procedure DirInfoSummary(infostr: TStrings);
begin
	infostr.Add('');
	infostr.Add('-----------------------------------------------------------');
    infostr.Add(Format(DIR_LIST_SUMMARY_STR, [TmpListFileCount]));
end;


//
// Repertoire: Cluster, Attribut, date, Dir+Nom
//
procedure DirListing(node: TTreeNode; const flags: TTreeTraversalFlags; liststr: TStringList);

	//
    //
    //
	procedure OnListDir(Sender: TObject; var abort: boolean);
    begin
    	if Sender is TDirectoryNodeData then
        begin
            TmpFileList.Add(DirInfoStr(TDirectoryNodeData(Sender).GetNodePath,
                        	   		   TDirectoryNodeData(Sender).DirInfo));

            Inc(TmpListFileCount);
        end;
    end;

begin
    TmpListFileCount :=	0;

	// met le parametre dans une variable globale pour eviter les pb dans OnListFile
	TmpFileList := liststr;
    
    // Ajouter l'entête
	DirInfoHeader(TmpFileList);

	TreeTraversal(node, flags, @OnListDir, '');

    // Ajouter le summary
	DirInfoSummary(TmpFileList);
end;


// -------------------------------------------------------------------------
//					Gestion des TFileItemData
// -------------------------------------------------------------------------


//
// Constructeur
//
constructor TFileItemData.Create(Parent: TDirectoryNodeData; finfo: TCommonFileInfo);
begin
	Self.FParentDir := parent;
    Self.FFileInfo := finfo;
    Self.FSelectStatus := ncUnchecked;
end;


//
//
//
destructor TFileItemData.Destroy;
begin
	Self.FFileInfo.Free;
end;


//
// Change l'état du fichier sans remonter les modif dans le rep associé
//
procedure TFileItemData.SetSelectStatus(status: TNodeCheckBoxState);
begin
    Self.FSelectStatus := status;
end;


//
// Change le status du fichier et mets à jour le repertoire associe
//
procedure TFileItemData.ChangeStatus(status: TNodeCheckBoxState);
begin
	Self.FParentDir.UpdateFileStatus(Self, status);
    Self.FSelectStatus := status;	
end;



// -------------------------------------------------------------------------
//					Gestion des TDirectoryNodeData
// -------------------------------------------------------------------------


//
//
//
constructor TDirectoryNodeData.Create(parent: TDirectoryNodeData; dirinfo: TCommonFileInfo);
begin
	Self.FFileList := TList.Create;

    Self.FSelectedCount := 0;
    Self.FSelectedSize := 0;
    
    Self.FParentDir := parent;
    Self.FDirInfo := dirinfo;
end;


//
//
//
destructor TDirectoryNodeData.Destroy;
var
	i: integer;
begin
	// Liberer les fichiers du répertoire
    for i := Self.FFileList.Count - 1 downto 0 do
		TFileItemData(Self.FFileList[i]).Free;

    Self.FFileList.Free;
    Self.FDirInfo.Free;
end;


// ==================== Propriété du TDirectoryNodeData =================

//
//
//
function TDirectoryNodeData.GetFileData(indice: integer): TFileItemData;
begin
	Result := TFileItemData(Self.FFileList[indice]);
end;


//
//
//
function TDirectoryNodeData.GetFileCount(): Cardinal;
begin
	Result := Self.FFileList.Count;
end;


//
// Renvoie le path du rep contenant le rep
//
function TDirectoryNodeData.GetNodePath: string;
var
    tmpnode : TDirectoryNodeData;
begin
	Result := '';
	tmpnode := Self;

    // Tant sur ce c'est pas le noeud racine
    // Mettre un \ en debut de path ???
    while (tmpnode <> nil) do
    begin
    	Result := tmpnode.FDirInfo.FileName + '\' + Result;
        tmpnode := tmpnode.Parent;
    end;
end;


//
// Associe le treenode au DirData;
//
procedure TDirectoryNodeData.AssociateNode(node: TTreeNode);
begin
	Self.FNode := node;
    Self.FNode.Data := Self;
end;


//
// Ajoute un fichier au répertoire
//
procedure TDirectoryNodeData.AddFile(finfo: TCommonFileInfo);
var
	fileitem: TFileItemData;
begin
    // Le fichier n'est pas selectionné par défaut;
	fileitem := TFileItemData.Create(Self, finfo);
    Self.FFileList.Add(fileitem);

    Inc(Self.FDirectorySize, finfo.Size);
end;


//
// Mets à jour les infos du DirData selon l'état de fichier
//
// filedata doit appartenir au fichier
//#Todo2 Changer le nom Update -> Notify
procedure TDirectoryNodeData.UpdateFileStatus(filedata: TFileItemData; const newstatus: TNodeCheckBoxState);
begin
    // Verifier qu'il y a bien un changement de status
    //
	if filedata.Selected <> newstatus then
    begin
    	if newstatus = ncUnchecked then
        begin
        	// Il faut deselectionner le fichier
            dec(Self.FSelectedCount);
            dec(Self.FSelectedSize, filedata.FileInfo.Size);
        end
        else
        begin
        	// Il faut selectionner le fichier
        	inc(Self.FSelectedCount);
            inc(Self.FSelectedSize, filedata.FileInfo.Size);
        end;

        // Mets à jour le selection
        if Self.SelectedCount = Self.FileCount then
        	Self.FSelectStatus := ncChecked
        else
        	if Self.SelectedCount = 0 then
            	Self.FSelectStatus := ncUnchecked
            else
            	Self.FSelectStatus := ncGrayed;
    end;
end;


//
// Calcule la taille des fichiers dans les sous rep
//
procedure TDirectoryNodeData.CalcTreeSize();
var
	i: integer;
begin
	Self.FTreeSize := Self.FDirectorySize;

    // Calculer la taille dans les sous rep
    for i := 0 to Self.FNode.Count - 1 do
        TDirectoryNodeData(Self.FNode.Item[i].Data).CalcTreeSize;

    // Ajouter la taille des sous rep
    for i := 0 to Self.FNode.Count - 1 do
    	Inc(Self.FTreeSize, TDirectoryNodeData(Self.FNode.Item[i].Data).TreeSize);
end;


//
// Calcule l'état d'un noeud à partir de l'état des fichiers qu'il contient
//
function TDirectoryNodeData.CalcFilesStatus: TNodeCheckBoxState;
var
	i : integer;
    curstatus : TNodeCheckBoxState;
    same : boolean;
begin
	if Self.FileCount = 0 then
    	Result := ncUnchecked
    else
    begin
		same := true;
        curstatus := Self.FileData[0].Selected;
    	for i := 1 to Self.FileCount - 1 do
        	if curstatus <> Self.FileData[i].Selected then
            begin
            	same := false;
                break;
            end;

        if same then
        	Result := curstatus
        else
        	Result := ncGrayed;
    end;
end;


//
// Selectionne tous les fichiers du répertoire
//
procedure TDirectoryNodeData.SelectFiles();
var
	i: integer;
begin
    Self.FSelectedCount := Self.FFileList.Count;
	Self.FSelectedSize := Self.FDirectorySize;
    Self.FSelectStatus := ncChecked;

	for i := 0 to Self.FFileList.Count - 1 do
    	Self.FileData[i].SetSelectStatus(ncChecked);
end;


//
// Deselectionne tous les fichiers associé au dir
//
procedure TDirectoryNodeData.UnselectFiles();
var
	i: integer;
begin
	Self.FSelectedSize := 0;
    Self.FSelectedCount := 0;
    Self.FSelectStatus := ncUnchecked;

	for i := 0 to Self.FFileList.Count - 1 do
    	Self.FileData[i].SetSelectStatus(ncUnchecked);
end;


// -------------------------------------------------------------------------
//						 Gestion des checkbox du treeview
// -------------------------------------------------------------------------

// séparer les fonctions sur les données de celles sur l'affichage

// Status des noeuds
// StateIndex contient l'état du noeud (-1 et 0 ne sont pas utilisable)
// (cbUnchecked, cbChecked, cbGrayed)
//
// Si tous les fils sont cbChecked => StateIndex = cbChecked
// Si tous les fils sont cbUnchecked => StateIndex = cbUnchecked
// Sinon StateIndex = cbGrayed
// Si pas de fils => dépend de l'état des fichiers liés au noeud
//


//
// Calcule d'états des fils d'un noeud
// SANS TENIR COMPTE DE L'ETAT DU NOEUD node
function CalcChildStatus(node: TTreeNode): TNodeCheckBoxState;
var
	i : integer;
    same : boolean;
	curstat: TNodeCheckBoxState;
begin
    if node.Count = 0 then
    	Result := ncUnchecked
    else
    begin
	    same := true;
        curstat := TNodeCheckBoxState(node.Item[0].StateIndex);

        // Regarde si tous les fils ont les même status
        for i := 1 to node.Count - 1 do
            if curstat <> TNodeCheckBoxState(node.Item[i].StateIndex) then
            begin
                same := false;
                break;
            end; // if

        if same then
        	Result := curstat
        else
        	Result := ncGrayed;
    end;
end;


//
// Modifie le checkbox en fonction de l'état des fichiers associés et des fils
//
procedure CalcNodeStatus(node: TTreeNode);
var
    curstatus : TNodeCheckBoxState;
begin
	// Calcule l'état lié au fichiers du noeud
    curstatus := TDirectoryNodeData(node.Data).CalcFilesStatus;

    if curstatus = CalcChildStatus(node) then
    	node.StateIndex := Ord(curstatus)
    else
    	node.StateIndex := Ord(ncGrayed);
end;


//
// Propage le changement d'état d'un noeud jusqu'à la racine
//
procedure PropagateNodeStateChange(node: TTreeNode);
var
	tmpnode: TTreeNode;
begin
	tmpnode := node.Parent;
    while (tmpnode <> nil) do
    begin
    	//Calcule l'état du noeud
        CalcNodeStatus(tmpnode);
		tmpnode := tmpnode.Parent;
    end; // while
end;


//
// Modifie le checkbox avec status
//
procedure SetNodeStatus (node: TTreeNode; const status: TNodeCheckBoxState);
var
	i : integer;
    dirdata: TDirectoryNodeData;
begin
	dirdata := TDirectoryNodeData(node.Data);
    if status = ncChecked then
	    dirdata.SelectFiles
    else
    	dirdata.UnselectFiles;

    // Mettre à jour nbfile select, taille select, ...
    node.StateIndex := Ord(status);
end;


//
// Affecte le status à un arbre et mets à jour l'affichage
// status = ncChecked ou ncUnchecked
//
procedure SetTreeStatus(node : TTreeNode; const status: TNodeCheckBoxState);
var
	i : integer;
begin
	SetNodeStatus(node, status);

    for i := 0 to (node.Count - 1) do
        SetTreeStatus(node.Item[i], status);
end;


// -------------------------------------------------------------------------
//							Parcours de l'arbre
// -------------------------------------------------------------------------


//
// Rajouter le traversal des fichier selectionnés
//
// Comment gérer le grayed pour les repertoires ???
//(ttFile, ttDir, ttRecurse, ttSelected, ttUnselected, ttUseFilter)
//ncUnchecked, ncChecked,
//#ToDo2 Retirer de la classe
//#ToDo2 Même fonction mais avec un file matching
//function MatchesMask(const StringToMatch, Mask: string): Boolean;
//http://homepages.borland.com/efg2lab/Library/Delphi/IO/Files.htm
//
// Mettre un fonction Principale et une locale recursive
//
// TreeTraversal(Node: TTreeNode; const flags : TTreeTraversalFlags; FilterMask: string; OnOccur: TNotifyEvent ? );
// Traversal(Node: TTreeNode)
//#ToDo2 Rajouter un test sur les rep vides 
procedure TreeTraversal(Node: TTreeNode; const flags : TTreeTraversalFlags;
					    OnOccur: TTraversalCallback; const filter: string);
var
	abortflag: boolean;
    
	//
    // Procedure récursive de recherche
    //
    procedure Traversal(Node: TTreeNode);
    var
        i : integer;
        fileok: boolean;
        dirdata : TDirectoryNodeData;
    begin
		dirdata := TDirectoryNodeData(node.Data);

        // Test si on traite les répertoires
        // Si les etats sont corrects => Si un noeud est unselect => tous les sous rep et fichier le sont
        // Test Selection du rep ??
        if ttDir in flags then
        begin
        	fileok := false;

            if (ttSelected in flags) and (dirdata.Status in [ncChecked, ncGrayed]) then
            	fileok := true;

            if (ttUnselected in flags) and (dirdata.Status = ncUnchecked) then
                fileok := true;

             if fileok and Assigned(OnOccur) then
                OnOccur(dirdata, abortflag);
        end;
        
        // Faire un test si on doit traiter les fichiers
        if (not abortflag) and (ttFile in flags) then
            for i := 0 to dirdata.FileCount - 1 do
            begin
                fileok := false;

                // Selon l'état des fichiers
                if (ttSelected in flags) and (dirdata.FileData[i].Selected = ncChecked) then
                    fileok := true;

                if (ttUnselected in flags) and (dirdata.FileData[i].Selected = ncUnchecked) then
                    fileok := true;

                if (ttUseFilter in flags) and fileok then
                	fileok := MatchesSpec(dirdata.FileData[i].FileInfo.FileName, filter);

                if fileok and Assigned(OnOccur) then
                    OnOccur(dirdata.FileData[i], abortflag);

                // On arrete le Traversal ?
                if abortflag then
                	break; 
            end;

        // faire un test si récursif
        if (not abortflag) and (ttRecurse in flags) then
            for i := 0 to node.Count - 1 do
                Traversal(node.Item[i]);
    end;

begin
    abortflag := false;
	Traversal(Node);
end;


// -------------------------------------------------------------------------
//							Fonctions de matching
// -------------------------------------------------------------------------


// Fonction Matching
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/shlwapi/path/pathmatchspec.asp
// ftp://delphi-jedi.org/api/Shlwapi.zip

//
// http://www.latiumsoftware.com/en/delphi/00018.php
//
function Like(AString, Pattern: string): boolean;
var
    i, n, n1, n2: integer;
    p1, p2: pchar;
label
    match, nomatch;
begin
    AString := UpperCase(AString);
    Pattern := UpperCase(Pattern);
    n1 := Length(AString);
    n2 := Length(Pattern);
    if n1 < n2 then n := n1 else n := n2;
    p1 := pchar(AString);
    p2 := pchar(Pattern);
    for i := 1 to n do begin
        if p2^ = '*' then goto match;
        if (p2^ <> '?') and (p2^ <> p1^) then goto nomatch;
        inc(p1); inc(p2);
    end;
    if n1 > n2 then begin
nomatch:
        Result := False;
        exit;
    end else if n1 < n2 then begin
        for i := n1 + 1 to n2 do begin
            if not (p2^ in ['*','?']) then goto nomatch;
            inc(p2);
        end;
    end;
match:
      Result := True;
end;


//
//
//http://www.latiumsoftware.com/en/delphi/00019.php
//
//function MatchesSpec(const FileName, Specification: string): boolean;
// Supporte les wild card '*' et '?'
function MatchesSpec(const FileName, Specification: string): boolean;
var
	SName, SExt, FName, FExt: string;
begin
	// Pas de chemin dans les parametres
    //FName := ExtractFileName(FileName);
    //SName := ExtractFileName(Specification);
    FName := FileName;
    SName := Specification;
    FExt := ExtractFileExt(FName);
    SExt := ExtractFileExt(SName);
    SetLength(FName, Length(FName) - Length(FExt));
    SetLength(SName, Length(SName) - Length(SExt));
    if SName = '' then SName := '*';
    if SExt = '' then SExt := '.*';
    if FExt = '' then FExt := '.';
    Result := Like(FName, SName) and Like(FExt, SExt);
end;


// --------------------------- Fin de l'unité -----------------------------
end.
