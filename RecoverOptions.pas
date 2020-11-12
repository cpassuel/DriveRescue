unit RecoverOptions;

//
// - Démarage => prendre les options par défaut ou charger un .INI et mettre à jour
//   les composants avec la valeur des options.
// - Execute => Affichage de la fenetre
// - Click Ok => mettre à jour la valeur des options avec les composants
// - Click Annuler => remettre les composants avec la valeur des options.
// - Click Default => remettre les composants avec la valeur des options par défaut.
//
// Comment gérer la fermeture par X ou Ctlr F4 ?

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;


type


// Tableau contenant toutes les options
TOptionInfo = record
	FAllowEmptyDir:		boolean;
	FAllowEmptyFile:	boolean;
    FOverWriteFile:		boolean;
    //FErrorBehaviour:	TErrorBehaviour;
    FRestoreTimeStamp:	boolean;
    FRestoreAttributs:	boolean;
    // ------------------- Listing -----------------------
    FAddListingHeader:	boolean;
    FAddListingSummary:	boolean;
    // La date de dernière ecriture est on par defaut
    ioAddCreationDate:	boolean;
    ioAddLastDate:		boolean;
    ioAddAttributes:	boolean;
end;


TFormOptions = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonDef: TButton;
    PageControl: TPageControl;
    TabSheetRec: TTabSheet;
    TabSheetGen: TTabSheet;
    GroupBoxBehav: TGroupBox;
    CheckBoxOw: TCheckBox;
    CheckBoxZeroFile: TCheckBox;
    CheckBoxZeroDir: TCheckBox;
    RadioButtonDel: TRadioButton;
    RadioButtonIgn: TRadioButton;
    RadioButtonTrunc: TRadioButton;
    RadioButtonAsk: TRadioButton;
    CheckBoxResTime: TCheckBox;
    CheckBoxResAttr: TCheckBox;
    TabSheetList: TTabSheet;
    CheckBoxListHead: TCheckBox;
    CheckBoxListSum: TCheckBox;
    CheckBoxWriDate: TCheckBox;
    CheckBoxFileAttr: TCheckBox;
    GroupBox1: TGroupBox;
    CheckBoxExpert: TCheckBox;
    CheckBoxCreDate: TCheckBox;
    CheckBoxLastDate: TCheckBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonDefClick(Sender: TObject);
  private
    { Déclarations privées }
    OptionBackup: TOptionInfo;
    // ----------------
    procedure RestoreComponentStatus(const info: TOptionInfo);
    procedure SaveComponentStatus(var info: TOptionInfo);
    procedure LoadOptions(const filename: string);
    //procedure ;

//    function GetAllowEmptyDir: boolean;
//    function GetAllowEmptyFile: boolean;
    procedure RestaureDefault;
  public
  	procedure Execute;
    { Déclarations publiques }
    // --------------------- options recovery -------------------------
    property AllowEmptyDir: boolean read OptionBackup.FAllowEmptyDir;
    property AllowEmptyFile: boolean read OptionBackup.FAllowEmptyFile;
    property OverwriteFile: boolean read OptionBackup.FOverWriteFile;
	property RestoreTimeStamp: boolean read OptionBackup.FRestoreTimeStamp;
    property RestoreAttributs: boolean read OptionBackup.FRestoreAttributs;
    // ---------------------- options listing -------------------------
    property AddListingHeader: boolean read OptionBackup.FAddListingHeader;
    property AddListingSummary: boolean read OptionBackup.FAddListingSummary;
    property AddListingCreDate: boolean read OptionBackup.ioAddCreationDate;
    property AddListingLastDate: boolean read OptionBackup.ioAddLastDate;
    property AddListingFileAttr: boolean read OptionBackup.ioAddAttributes;
  end;

  
var
  FormOptions: TFormOptions;

  
// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

{$R *.dfm}


const

//
// Valeur des options par défaut.
//
DefaultOptions: TOptionInfo = (
	FAllowEmptyDir:		false;
    FAllowEmptyFile:	true;
    FOverWriteFile:		false;
    //FErrorBehaviour:	TErrorBehaviour;
    FRestoreTimeStamp:	true;
    FRestoreAttributs:	true;
    // ------------------- Listing -----------------------
    FAddListingHeader:	true;
    FAddListingSummary:	true;
    ioAddCreationDate:	false;
    ioAddLastDate:		false;
    ioAddAttributes:	true
);


// Charger options fichier
// Sauver option fichier
// Remettre par defaut
// Sauver un etat option
// Remettre etat option


//
// Ouvre la fenetre des options
//
procedure TFormOptions.Execute();
begin
	if Self.ShowModal = mrOk then
    begin
    	// Mettre à jour les options avec l'etat des composants
        Self.SaveComponentStatus(Self.OptionBackup);
    end
    else
    begin
    	// Annulation => on restaure l'etat sauvegardé
        Self.RestoreComponentStatus(Self.OptionBackup);
    end;
end;


// ------------------------------------------------------------------------
//							Gestion des proprités
// ------------------------------------------------------------------------


//function TFormOptions.GetAllowEmptyDir(): boolean;
//begin
//	Result := Self.CheckBoxZeroDir.Checked;
//end;
//
//
//function TFormOptions.GetAllowEmptyFile(): boolean;
//begin
//	Result := Self.CheckBoxZeroFile.Checked;
//end;


// ------------------------------------------------------------------------
//							Gestion des proprités
// ------------------------------------------------------------------------


procedure TFormOptions.LoadOptions(const filename: string);
begin
	// Les options ne sont pas sauvés actuellement => utiliser les options
    // par defaut
    Self.RestaureDefault;
end;


//
// Procedure: TFormOptions.RestaureDefault
// Purpose:   
//
// Arguments: 
// Result:    None
//
procedure TFormOptions.RestaureDefault();
begin
    // Remet les options par défaut
    Self.OptionBackup := DefaultOptions;

    // Mets à jour l'affichage
    Self.RestoreComponentStatus(Self.OptionBackup);
end;


//
// Met à jour l'état des composants à partir de la structure TOptionInfo 
//
procedure TFormOptions.RestoreComponentStatus(const info: TOptionInfo);
begin
    Self.CheckBoxZeroDir.Checked	:= info.FAllowEmptyDir;
	Self.CheckBoxZeroFile.Checked	:= info.FAllowEmptyFile;

    // ------------------- Listing -----------------------
    Self.CheckBoxListHead.Checked := info.FAddListingHeader;
    Self.CheckBoxListSum.Checked := info.FAddListingSummary;
    //Self.CheckBoxWriDate.Checked := info.
    Self.CheckBoxCreDate.Checked := info.ioAddCreationDate;
    Self.CheckBoxLastDate.Checked := info.ioAddLastDate;
    Self.CheckBoxFileAttr.Checked := info.ioAddAttributes;
end;


//
// Met à jour la structure TOptionInfo à partir de l'état des composants
//
procedure TFormOptions.SaveComponentStatus(var info: TOptionInfo);
begin
	info.FAllowEmptyDir	:= Self.CheckBoxZeroDir.Checked;
	info.FAllowEmptyFile:= Self.CheckBoxZeroFile.Checked;

    // ------------------- Listing -----------------------
    info.FAddListingHeader := Self.CheckBoxListHead.Checked;
    info.FAddListingSummary := Self.CheckBoxListSum.Checked;
    // := info. Self.CheckBoxWriDate.Checked
    info.ioAddCreationDate := Self.CheckBoxCreDate.Checked;
    info.ioAddLastDate := Self.CheckBoxLastDate.Checked;
    info.ioAddAttributes := Self.CheckBoxFileAttr.Checked;
end;


// ------------------------------------------------------------------------
//						Gestion des evenements
// ------------------------------------------------------------------------


procedure TFormOptions.FormCreate(Sender: TObject);
begin
	// Charger les options ou mettre les options par defaut
    Self.LoadOptions('option.ini'); 
end;


//
// Remets les paramètres à l'état précédant
//
procedure TFormOptions.ButtonCancelClick(Sender: TObject);
begin
    Self.RestoreComponentStatus(Self.OptionBackup);
end;


//
// Remets les paramètres par défaut
//
procedure TFormOptions.ButtonDefClick(Sender: TObject);
begin
	Self.RestaureDefault;
end;



// --------------------------- Fin de l'unité -----------------------------
end.
