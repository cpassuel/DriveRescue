{-----------------------------------------------------------------------------
 Unit Name: FormProperties
 Author:    Chris
 Purpose:	Affiche/sauve les infos sous forme de couples Propriété, valeur (provenant d'une TStrings)
 Date:		22/01/2005
 History:   18/01/2005 Sortie en pressant ESC ajoutée
 			22/01/2005 Formatage du fichier texte
-----------------------------------------------------------------------------}


unit FormProperties;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ClipBrd;


type
  TPropertyForm = class(TForm)
    ListViewInfo: TListView;
    BitBtnOK: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnClipBrd: TBitBtn;
    procedure BitBtnSaveClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtnClipBrdClick(Sender: TObject);
  private
    { Déclarations privées }
    // convertit les donnees en une liste de chaine formate
    procedure ListViewToFormatedStrings(strinfo: TStrings);
  public
    { Déclarations publiques }
    procedure Execute(const title: string; strinfo: TStrings);
  end;

var
  PropertyForm: TPropertyForm;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

{$R *.dfm}

Uses
	MainUnit, ConversionUnit;


//
// Remplit le listview et affiche la form
//
procedure TPropertyForm.Execute(const title: string; strinfo: TStrings);
var
	i : smallint;
    litem : TListItem;
begin
	Self.Caption := title;
    Self.ListViewInfo.Clear;
    
    if Assigned(strinfo) then
    	for i := 0 to (strinfo.Count div 2) - 1 do
        begin
            litem := Self.ListViewInfo.Items.Add;
            litem.Caption := strinfo[2*i];
            litem.SubItems.Add(strinfo[2*i+1]);
        end;

    Self.ShowModal;
end;


//
// Sauvegarde les infos dans un fichier texte
//#Todo1 Utiliser la procedure ListViewToFormatedStrings dans BitBtnSaveClick
//
procedure TPropertyForm.BitBtnSaveClick(Sender: TObject);
var
	FormatedInfo: TStrings;
    i, maxlen: integer;
begin
	// Selectionner un fichier
    if MainForm.SaveDialog.Execute then
    begin
        // TStrings pour formater et écrire dans le fichier
        FormatedInfo := TStringList.Create;

        // Ajoute le titre
        FormatedInfo.Add(Self.Caption);
        FormatedInfo.Add('---------------------------------------');

        maxlen := 0;
        with Self.ListViewInfo do
        begin
	        // determiner la taille max de la partie gauche (propriete)
            for i := 0 to Items.Count - 1 do
            	if Length(Items[i].Caption) > maxlen then
            		maxlen := Length(Items[i].Caption);

            for i := 0 to Items.Count - 1 do
                FormatedInfo.Add(StringOfChar(' ', 2 + maxlen - Length(Items[i].Caption)) +
                				 Items[i].Caption + ': ' +Items[i].SubItems.Strings[0]);
        end;

        //
        FormatedInfo.Add('---------------------------------------');

        try
            FormatedInfo.SaveToFile(MainForm.SaveDialog.FileName);
        except
            MessageDlg(SAVE_LOG_ERROR_MSG, mtError, [mbOK], 0);
        end;

        FormatedInfo.Free
    end;
end;


//
// Gère l'evenement KeyUp
//
procedure TPropertyForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
	// On ferme la fenetre en appuyant sur ESC
    if Key = VK_ESCAPE then
    	Self.ModalResult := mrOk;
end;


//
//
//#Todo2 Modify TStrings to string conversion to remove first sLineBreak
//
procedure TPropertyForm.BitBtnClipBrdClick(Sender: TObject);
var
  s : string;
	FormatedInfo: TStrings;
  i : integer;
begin

  // TStrings pour formater et écrire dans le fichier
  FormatedInfo := TStringList.Create;

  Self.ListViewToFormatedStrings(FormatedInfo);

  // convertie la liste de chaine en chaine
  for i := 0 to FormatedInfo.Count - 1 do
    if i = 0 then
      s := FormatedInfo[i]
    else
      s := s + sLineBreak + FormatedInfo[i];

  FormatedInfo.Free;

  // copy to clipboard
  Clipboard.AsText := s;
end;


//
//
//
procedure TPropertyForm.ListViewToFormatedStrings(strinfo: TStrings);
var
    i, maxlen: integer;
begin
   // Ajoute le titre
   strinfo.Add(Self.Caption);
   strinfo.Add('---------------------------------------');

   maxlen := 0;
   with Self.ListViewInfo do
   begin
     // determiner la taille max de la partie gauche (propriete)
       for i := 0 to Items.Count - 1 do
       	if Length(Items[i].Caption) > maxlen then
       		maxlen := Length(Items[i].Caption);

       for i := 0 to Items.Count - 1 do
           strinfo.Add(StringOfChar(' ', 2 + maxlen - Length(Items[i].Caption)) +
           				 Items[i].Caption + ': ' +Items[i].SubItems.Strings[0]);
   end;

   //
   strinfo.Add('---------------------------------------');
end;

// --------------------------- Fin de l'unité -----------------------------
end.

