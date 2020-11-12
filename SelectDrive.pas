{-----------------------------------------------------------------------------
 Unit Name: SelectDrive
 Author:    Chris
 Purpose:	Afficher une fenetre pour choisir le disque à utiliser
 History:
-----------------------------------------------------------------------------}

//#ToDo Rajouter l'état du disque primary/secondary master/slave ? 

unit SelectDrive;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls,
  PhysicalDrive;

type
  TFormSelectDrive = class(TForm)
    ListView: TListView;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    function Execute(const title: string): TPhysicalDrive;
  end;

var
  FormSelectDrive: TFormSelectDrive;


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation

{$R *.dfm}

uses
	ConversionUnit;

const
	MAX_DRIVE = 7;    

//
// Initialise la boite de dialogue
//
procedure TFormSelectDrive.FormCreate(Sender: TObject);
var
	i : Byte;
    startnum: Byte;
    litem : TListItem;
    drv : TPhysicalDrive;
begin
    Self.Caption := 'Please select a drive...';

    //#Todo2 Mettre l'enum dans le Execute
	// Scanner les disques présents
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    	startnum := $80
    else
    	startnum := 0;

    for i := startnum to startnum + MAX_DRIVE do
    begin
    	if isAFixedDrive(i) then
        begin
            // Utiliser le getparam ou créer des TPhysicalDrive ???
            drv := TPhysicalDrive.Create(i);

            // Verifie si le disque existe
            if drv.LastErrorCode <> 0 then
            begin
                drv.Destroy;
                continue;
            end;

            // Remplir la ligne
            litem := Self.ListView.Items.Add;
            
            // Prendre en compte cas pas de smart
            if drv.ModelNumber = '' then
	            litem.Caption := 'Drive ID x' + IntToHex(drv.Drive, 2)
            else
	            litem.Caption := drv.ModelNumber;

            if drv.isRemovable then
                litem.SubItems.Add('Removable')
            else
                litem.SubItems.Add('Fixed');

            litem.SubItems.Add(SizeTo2DigitByteString(drv.BytesPerSec * drv.SectorCount));
            litem.SubItems.Add(IntToStr(drv.CylinderCount));
            litem.SubItems.Add(IntToStr(drv.HeadCount));
            litem.SubItems.Add(IntToStr(drv.SectorPerTrack));

            // Les disques existant sont dans data
            litem.Data := drv;
        end; // if
    end; // for
end;


//
// Affiche la boite de selection d'un disque
//
function TFormSelectDrive.Execute(const title: string): TPhysicalDrive;
var
	ret: integer;
begin
	Result := nil;

	// Afficher la fenetre
    Self.Caption := title;
    ret := Self.ShowModal;

    if ret = mrOk then
	    if Assigned(Self.ListView.Selected) then
        	Result := TPhysicalDrive(Self.ListView.Selected.Data);
end;


//
// Destruction de la fenetre
//
procedure TFormSelectDrive.FormDestroy(Sender: TObject);
var
	i : smallint;
begin
	for i := 0 to Self.ListView.Items.Count - 1 do
    	TPhysicalDrive(Self.ListView.Items[i].Data).Free;
end;

// --------------------------- Fin de l'unité -----------------------------
end.
