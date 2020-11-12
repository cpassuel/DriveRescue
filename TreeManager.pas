{-----------------------------------------------------------------------------
 Unit Name: TreeManager
 Author:    Chris
 Purpose:	Unité abstraite pour gérer une structure arboresente
 History:	10/02/2005
-----------------------------------------------------------------------------}


unit TreeManager;

interface


uses
	Windows, SysUtils;
    

type

// Mettre l'objet dans la structure ???
// Mise à jour de Parent quand on ajoute un Folder à un Folder ?

TLeafContainer = class;

//
// Classe abstraite représentant un container d'une noeud de l'arbre
//
TNodeContainer = class
    private
    protected
        function GetParent(): TNodeContainer; virtual; abstract;
        function GetName(): string; virtual; abstract;
        function GetLeafCount(): Cardinal; virtual; abstract;
        function GetLeafIndex(ind: Cardinal): TLeafContainer; virtual; abstract;
        function GetNodeCount(): Cardinal; virtual; abstract;
        function GetNodeIndex(ind: Cardinal): TNodeContainer; virtual; abstract;
    public
        constructor Create; virtual; abstract;
        destructor Destroy; virtual; abstract;
        //procedure AddFile(fincont: TLeafContainer);
        //procedure AddFolder(foldcont: TNodeContainer);
        //procedure RemoveFile() ?
        //procedure RemoveFolder() ?
        // ----
        property Parent: TNodeContainer read GetParent;
        property Name: string read GetName;
        property NodeCount: Cardinal read GetNodeCount;
        property LeafCount: Cardinal read GetLeafCount;
        property Node[ind: Cardinal]: TNodeContainer read GetNodeIndex;
        property Leaf[ind: Cardinal]: TLeafContainer read GetLeafIndex;
end;


// Comment mettre à jour FFolder quand on ajoute un fichier à un folder ?
// Le Fichier s'ajoute de lui même au Folder dans AddToFolder() ?
//
// Classe abstraite représentant un container d'une feuille de l'arbre
//
TLeafContainer = class
    private
    protected
        function GetName(): string; virtual; abstract;
        function GetNode(): TNodeContainer; virtual; abstract;
    public
        // --------
        property Node: TNodeContainer read GetNode;
        property Name: string read GetName;
end;


TProcessLeaf = procedure(leaf: TLeafContainer) of object;
TOccurenceEvent = procedure(leaf: TLeafContainer; var abort: boolean);


//
// Comment gérer les occurences ? methode ou call back ?
// Gestion de la destruction des objet ?
//
//
TTreeManager = class
    private
    	FAborted:		boolean; 	// Le parcours de l'arbre est interrompu
        FMatcheString:	string;		// Chaine pour la recherche (#Todo2 Mettre un TStringList pour plus de souplesse) 
        FOnFileMatch:	TProcessLeaf;
        FOnSearch:		TProcessLeaf;
        // -----
        procedure LeafFileMatch(leaf: TLeafContainer);
        //procedure LeafRegMatch(leaf: TLeafContainer);
    protected
	    FRoot:		TNodeContainer;	// Racine de l'arbre
        //FTraversal: 		// Mode de parcours de l'arbre
        FOnProcessLeaf: TProcessLeaf;
        FOnOccurence:	TOccurenceEvent;
        // -----
        procedure NodeTravesal(const node: TNodeContainer);  // parcours de l'arbre
        procedure TreeTravesal;		// parcours de l'arbre
    public
        constructor Create(root: TNodeContainer); virtual;
        destructor Destroy; virtual;
    	procedure FileMatch(const matchstr: string; var abort: boolean);
    //  procedure RegularMatch(const regexp: string; var abort: boolean);
    //#Todo3 une proc avec une liste de match string
    // fonction pour recherche not included
    // Parcours sans filtre
    // Comment avoir une occurence sur un noeud ???
		property OnOccurence: TOccurenceEvent read FOnOccurence write FOnOccurence;
end;


//

// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


//
//
//
constructor TTreeManager.Create(root: TNodeContainer);
begin
	Self.FRoot := root;
end;


//
//
//
destructor TTreeManager.Destroy;
begin
end;


// ------------------------------------------------------------------------
//						  Parcours de l'arbre
// ------------------------------------------------------------------------


//
// Parcours un arbre
//
procedure TTreeManager.TreeTravesal();
begin
	Self.FAborted := false;
	Self.NodeTravesal(Self.FRoot);
end;


//
// Parcours un noeud
//
procedure TTreeManager.NodeTravesal(const node: TNodeContainer);
var
	i: integer;
begin
	// faire les feuilles en premier
    for i := 0 to node.LeafCount - 1 do
    begin
    	// Process le node
        if Assigned (Self.FOnProcessLeaf) then
        	Self.FOnProcessLeaf(node.Leaf[i]);

        if Self.FAborted then
        	exit;
    end;

	// faire les noeud
    for i := 0 to node.NodeCount - 1 do
    begin
    	Self.NodeTravesal(node.Node[i]);

        if Self.FAborted then
        	exit;
    end;
end;


// ------------------------------------------------------------------------
//				  Routines pour traiter les evenement sur les noeuds
// ------------------------------------------------------------------------

//
// Déclaration des fonctions de shlwapi.dll
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/shlwapi/shlwapi.asp
//
const
  shlwapi32 = 'shlwapi.dll';

//function PathMatchSpecA(pszFile, pszSpec: PAnsiChar): BOOL; stdcall; external shlwapi32 name 'PathMatchSpecA';
//function PathMatchSpecW(pszFile, pszSpec: PWideChar): BOOL; stdcall; external shlwapi32 name 'PathMatchSpecW';
function PathMatchSpec(pszFile, pszSpec: PChar): BOOL; stdcall; external shlwapi32 name 'PathMatchSpecA';


//
// Si le noeud correnpond au joker, alors déclencher un evenement OnOccurence
//
procedure TTreeManager.LeafFileMatch(leaf: TLeafContainer);
begin
	if PathMatchSpec(Pchar(leaf.Name), PChar(Self.FMatcheString)) then
    	if Assigned(Self.OnOccurence) then
        	Self.OnOccurence(leaf, Self.FAborted);
end;


// ------------------------------------------------------------------------
//				  Routines de recherche dans l'arbre
// ------------------------------------------------------------------------


procedure TTreeManager.FileMatch(const matchstr: string; var abort: boolean);
begin
	Self.FOnProcessLeaf := LeafFileMatch;
    Self.FMatcheString := matchstr;

    Self.TreeTravesal;  
end;


end.
// --------------------------- Fin de l'unité -----------------------------

