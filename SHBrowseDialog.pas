{-----------------------------------------------------------------------------
 Unit Name: BrowseDialog
 Author:    Chris
 Purpose:   Boite de dialogue pour selectionner un répertoire
 History:   29/01/2005
-----------------------------------------------------------------------------}


unit SHBrowseDialog;


interface

uses
	Windows, Shlobj;


function BrowseDialog(handle : HWND; title : string) : string; forward;



// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation


// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/functions/shbrowseforfolder.asp
//
// Affiche une boite de dialogue pour selectionner un répertoire
//
// Parametres
//	handle	Handle de la fenetre appelante (TForm.Handle)
//	title	Titre de la boite de dialogue
//
// Retourne
//	'' si pas de répertoire selectionné
//
function BrowseDialog(handle : HWND; title : string) : string;
var
    lpItemID : PItemIDList;
    BrowseInfo : TBrowseInfo;
    DisplayName : array[0..MAX_PATH] of char;
    TempPath : array[0..MAX_PATH] of char;

begin
    Result:='';
    // Met la structure à 0
    FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);

    // initialise la structure
    with BrowseInfo do
    begin
        hwndOwner := Handle;
        pszDisplayName := @DisplayName;
        lpszTitle := PChar(title);
        ulFlags := BIF_RETURNONLYFSDIRS;
        //ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
        //lpfn := @BrowseCallbackProc; //TEST
    end;

    // Affiche la boite de dialogue
    lpItemID := SHBrowseForFolder(BrowseInfo);

    if lpItemId <> nil then
    begin
        SHGetPathFromIDList(lpItemID, TempPath);
        Result := TempPath;
        GlobalFreePtr(lpItemID);
    end;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
 