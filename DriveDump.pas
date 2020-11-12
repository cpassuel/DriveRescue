{-----------------------------------------------------------------------------
 Unit Name: DriveDump
 Author:    Chris
 Purpose:	Dump Hexa d'un secteur ou cluster
 History:	19/08/2004
 			25/01/2005 Ajout de la sauvergarde du cluster/secteur
            17/02/2005 Ajout visualisation disque	
-----------------------------------------------------------------------------}


unit DriveDump;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls,
  GenericPartition, GenericFat, GenericDrive;

  
type
  TFormDump = class(TForm)
    MemoHexDump: TMemo;
    ButtonRead: TButton;
    SpeedButtonUp: TSpeedButton;
    SpeedButtonDown: TSpeedButton;
    GroupBoxPos: TGroupBox;
    RadioGroupMode: TRadioGroup;
    StaticTextInfo: TStaticText;
    LabeledEditPos: TLabeledEdit;
    ButtonSaveHexa: TButton;
    ButtonSaveBin: TButton;
    SaveDialog: TSaveDialog;
    procedure SpeedButtonDownClick(Sender: TObject);
    procedure SpeedButtonUpClick(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
	procedure LabeledEditKeyPress(Sender: TObject; var Key: Char);
    procedure RadioGroupModeClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    //
    // CurrentValue;
    MinSector:	Cardinal;
    MaxSector:	Cardinal;
    MinCluster: Cardinal;
    MaxCluster: Cardinal;
    MinValue:	Cardinal;
    MaxValue:	Cardinal;
    FPart:		TGenericPartition;	// Partition à afficher
	FDrive:		TGenericDrive;		// Disque à afficher
	procedure ChangeMode();
	procedure HexDump(buffer : array of byte; const size : Cardinal);
    procedure UpdateDump(const pos: Cardinal);
  public
    procedure Execute(drive: TGenericDrive; part: TGenericPartition);
  end;

var
  FormDump: TFormDump;

//#Todo2 conserver la valeur actuelle du cluster / secteur dans un variable cardinal ?

// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

{$R *.dfm}


Uses
	ConversionUnit;


var
	DumpBuffer : array of Byte;


//
// Affiche la fenetre
// Si drive <> nil alors dump disque sinon dump partition
//
procedure TFormDump.Execute(drive: TGenericDrive; part: TGenericPartition);
begin
    Self.FPart := part;
    Self.FDrive:= drive;

    Self.MinSector := 0;

    // Initialise les bornes selon mode disque ou part
    if Assigned(Self.FPart) then
    begin
    	// ----- Mode Partition ------
	    Self.MaxSector := Self.FPart.TotalSectors - 1;

        // Récuperer les bornes des clusters ()
        Self.MinCluster := Self.FPart.MinValidClust;
        Self.MaxCluster := Self.FPart.MaxValidClust;

        // Met en mode Cluster
    	Self.Caption := DRIVE_DUMP_PART_TITLE;

        Self.RadioGroupMode.ItemIndex := 0;
    end
    else
    begin
    	// ----- Mode Drive ------
	    Self.MaxSector := Self.FDrive.SectorCount;

        // Met en mode Secteur
    	Self.Caption := DRIVE_DUMP_DRIVE_TITLE;

        // Pas de mode cluster possible
        Self.RadioGroupMode.ItemIndex := 1;
    end;

	Self.RadioGroupMode.Enabled := Assigned(Self.FPart);

    // Prends en compte le changement de mode
    Self.ChangeMode;

	Self.Show;
end;


//
// Affiche un nouveau cluster/secteur dont le numero est passé en parametre
//
procedure TFormDump.UpdateDump(const pos: Cardinal);
var
	readok: boolean;
begin
	if Self.RadioGroupMode.ItemIndex = 0 then
    begin
    	// Mode Cluster, on remplie les erreurs avec la Bad Pattern
        Self.FPart.ReadClusterToBuffer(pos, @DumpBuffer[0], caFillInvalid);
		// Cluster %d (start at sector %d)

        // Dump Hexa
        Self.HexDump(DumpBuffer, Length(DumpBuffer));

        Self.StaticTextInfo.Caption := Format(CLUSTER_NUM_STR, [pos]);
    end
    else
    begin
    	// Mode Secteur: disque ou part ??
	    if Assigned(Self.FPart) then
            readok := Self.FPart.ReadSectorToBuffer(pos, @DumpBuffer[0])
        else
        	readok := Self.FDrive.ReadSectors(pos, 1, @DumpBuffer[0]);

		if readok then
        begin
            // Dump Hexa
            Self.HexDump(DumpBuffer, Length(DumpBuffer));

            Self.StaticTextInfo.Caption := Format(SECTOR_NUM_STR, [pos]);
        end
        else
            Self.StaticTextInfo.Caption := Format(SECTOR_ERROR_STR, [pos]);
    end;
end;


//
// Dump Ascii et Hexa d'une zone memoire
//
procedure TFormDump.HexDump(buffer : array of byte; const size : Cardinal);
const
	BYTES_PER_LINE = 16;
    HexDigits : array[0..15] of Char = '0123456789ABCDEF';
var
	i,j : smallint;
    line : string;
begin
	Self.MemoHexDump.Clear;

    // Interdit la mise à jour du mémo
    Self.MemoHexDump.Lines.BeginUpdate;

	for i := 0 to (size div BYTES_PER_LINE) - 1 do
    begin
    	// Partie Adresse
        line := Format('$%.4x',[i * BYTES_PER_LINE]);
    	//line := Format('$%.4x  ',[i * BYTES_PER_LINE]);

        // Partie dump Hexa
        for j := 0 to BYTES_PER_LINE - 1 do
        begin
            if (j mod 4) = 0 then line := line + ' ';
			line := line + HexDigits[buffer[i * BYTES_PER_LINE + j] SHR $04];
        	line := line + HexDigits[buffer[i * BYTES_PER_LINE + j] AND $0F];
        end;

        // Partie dump Ascii
       	line := line + ' ';
       	//line := line + '  ';
        for j := 0 to BYTES_PER_LINE - 1 do
        begin
        	if buffer[i * BYTES_PER_LINE + j] >= 32 then
            	line := line + Char(buffer[i * BYTES_PER_LINE + j])
            else
                line := line + '.';
        end;
        Self.MemoHexDump.Lines.Add(line);
    end;

    // Se positionner sur la permière ligne
    Self.MemoHexDump.SelStart := 0;
    Self.MemoHexDump.SelLength := 0;

    // Rétablit la mise à jour du mémo
    Self.MemoHexDump.Lines.EndUpdate;
end;


// ------------------------------------------------------------------------
//						Gestion des evenements
// ------------------------------------------------------------------------


//
// Changement de mode de lecture
//
procedure TFormDump.ChangeMode();
begin
	Self.MemoHexDump.Clear;
    
	if Self.RadioGroupMode.ItemIndex = 0 then
    begin
    	// Mode Cluster
        Self.MinValue := Self.MinCluster;
        Self.MaxValue := Self.MaxCluster;

        // Mettre à jour le label bornes
        Self.LabeledEditPos.EditLabel.Caption := Format(CLUSTER_INTERVAL_STR, [Self.MinValue, Self.MaxValue]);

		// Selon le mode de lecture, mettre à jour la taille du buffer
		SetLength(DumpBuffer, Self.FPart.ClusterSize);
    end
    else
    begin
    	// Mode Secteur
        Self.MinValue := Self.MinSector;
        Self.MaxValue := Self.MaxSector;

        // Mettre à jour le label bornes
        Self.LabeledEditPos.EditLabel.Caption := Format(SECTOR_INTERVAL_STR, [Self.MinValue, Self.MaxValue]);

        // Selon le mode de lecture, mettre à jour la taille du buffer
	    if Assigned(Self.FPart) then
	        SetLength(DumpBuffer, Self.FPart.SectorSize)
        else
	        SetLength(DumpBuffer, Self.FDrive.BytesPerSec);
    end;

    // On remet la position au minimum
    Self.LabeledEditPos.Text := IntToStr(Self.MinValue);
end;


//
//	Filtre les char non numériques
//
procedure TFormDump.LabeledEditKeyPress(Sender: TObject; var Key: Char);
begin
	if not (key in [#8,'0'..'9']) then
        key := #0;
end;


//
// Lecture
//
procedure TFormDump.ButtonReadClick(Sender: TObject);
var
	value: Cardinal;
begin
	// Essayer de convertir la position
    try
		value := StrToInt(Self.LabeledEditPos.Text);
        if (value >= Self.MinValue) and (value <= Self.MaxValue) then
        begin
			Self.UpdateDump(value);
        end; 
    except
        MessageDlg(DRIVE_DUMP_BAD_POSITION_STR, mtError, [mbOK], 0);
        exit;
    end;
end;


//
// On décrémente la valeur
//
procedure TFormDump.SpeedButtonDownClick(Sender: TObject);
var
	value : cardinal;
begin
    try
	    // Convertir
		value := StrToInt(Self.LabeledEditPos.Text);

	    // Verifier bornes
    	if value > Self.MinValue then
        begin
        	dec(value);
		    Self.LabeledEditPos.Text := IntToStr(value);
			Self.UpdateDump(value);
        end;
    except
    end;
end;


//
// On incrémente la valeur
//
procedure TFormDump.SpeedButtonUpClick(Sender: TObject);
var
	value : cardinal;
begin
    try
		value := StrToInt(Self.LabeledEditPos.Text);
    	if value < Self.MaxValue then
        begin
        	inc(value);
		    Self.LabeledEditPos.Text := IntToStr(value);
			Self.UpdateDump(value);
        end;
    except
    end;
end;


//
// Mets à jour en fonction du mode choisi
//
procedure TFormDump.RadioGroupModeClick(Sender: TObject);
begin
	Self.ChangeMode;
end;


//
// Sauvegarde du cluster/secteur affiché
//
procedure TFormDump.ButtonSaveClick(Sender: TObject);
var
	fichier: TFileStream;
    str : string;
begin
    // Initialise le Filter selon le type de fichier que l'on veut sauver
	Self.SaveDialog.DefaultExt := 'TXT';
    if (Sender = ButtonSaveBin) then
		Self.SaveDialog.Filter := SAVEDIALOG_NO_FILTER_STR
    else
		Self.SaveDialog.Filter := SAVEDIALOG_FILTER_STR;

	// Demander un nom de fichier
    if Self.SaveDialog.Execute then
    begin
        try
            // ouvrir le fichier
            fichier := nil;
            fichier := TFileStream.Create(Self.SaveDialog.FileName, fmCreate or fmShareExclusive);

            // Quel est le type de sauvegarde demandée ?
            if (Sender = ButtonSaveBin) then
                fichier.WriteBuffer(DumpBuffer[0], Length(DumpBuffer))
            else
            begin
                // Ecrire un entête selon le mode
                //str := Format('Cluster %d#10#13', []);
                //fichier.Write(str, Length(str));
                Self.MemoHexDump.Lines.SaveToStream(fichier);
            end;
        except
            // Erreur fichier
            MessageDlg(SAVE_LOG_ERROR_MSG, mtError, [mbOK], 0);
        end;

        // libération
        fichier.Free;
    end;
end;


// --------------------------- Fin de l'unité -----------------------------
end.
