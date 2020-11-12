{-----------------------------------------------------------------------------
 Unit Name: SelectPartition
 Author:    Chris
 Purpose:	Selectionner une partition
 Date:		16/08/2005
 History:   20/01/2005 Ajout du mode expert
 			25/02/2005 Ajout du NTFS
			02/03/2005 Ajout test par famille de FS
-----------------------------------------------------------------------------}


unit SelectPartition;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  // Unités privées
  GenericDrive, GenericPartition, FAT32Partition, FAT16Partition, NTFSPartition,
  XFSPartition;

  
type
  TFormSelectPart = class(TForm)
    ComboBoxPart: TComboBox;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    RadioButtonTable: TRadioButton;
    RadioButtonBoot: TRadioButton;
    RadioButtonManual: TRadioButton;
    LabelPartCount: TLabel;
    LabeledEditLBA: TLabeledEdit;
    ComboBoxFSType: TComboBox;
    LabelPartFS: TLabel;
    LabelEdLBA: TLabeledEdit;
    LabelEdSectCount: TLabeledEdit;
    LabelEdRsvdSect: TLabeledEdit;
    CheckBoxMirrorFAT: TCheckBox;
    LabelEdFatSize: TLabeledEdit;
    Bevel1: TBevel;
    procedure RadioButtonClick(Sender: TObject);
	procedure FilterEditNumKey(Sender: TObject; var Key: Char);
  private
	procedure InitPartCombo(disque: TGenericDrive);
    procedure SetPartSelectionStatus(const status: boolean);
    procedure SetBootSelectionStatus(const status: boolean);
    procedure SetManualSelectionStatus(const status: boolean);
    function GetLabeledEditValue(labeled: TLabeledEdit): Cardinal;
  public
	function Execute(disque: TGenericDrive): TGenericPartition;
  end;


var
  FormSelectPart: TFormSelectPart;

//#Todo2 Rajouter un test sur le lba < à nb sect 
//#Todo3 possibilité de définir la taille des clusters ???

// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

{$R *.dfm}

Uses
	ConversionUnit;


//Lire Partition Table
//Si Table ok alors
//	Demander si partition dans la liste
//    Si oui alors
//    	Lire MBR
//        Si MBR ok alors
//        	Creer Objet
//        finsi
//Sinon
//	Faire un Scan ?
//    Demander entrer LBA
//    
//finsi


// Création de la partition (param ???)
// Renseigner la part:
// Via table part : si pas ok => mode expert (veriefier si le type est supporté)
// Via 1er secteur : si pas ok => mode expert
// Mode Expert
// Scan: trouver un BootSector ou Ext MBR, vérifier
//	(Boot Sector normal ou backup: comment savoir ??)


// Selection de la partition :
// Etape 1
// 1) Via la table des part (si elles existent)
// 2) Via scan disk
// 3) Entrée manuelle

// 1) si la partition choisie

// Selection d'une partition :
//	Par Combo si Part table Ok
//	Par secteur de départ si BootSector ou Backup BootSector Ok
//	Secteur de départ, taille part si BootSector et Backup BootSector corrompu

// Utilisation de la fat mirroir

//
// Affiche la fenetre de selection des partitions
//
function TFormSelectPart.Execute(disque: TGenericDrive): TGenericPartition;
var
	partinfo: PPartitionInfo;
    lba, fatsize, rsvdsect, sectcount, nbfat:	Cardinal;
begin
	Result := nil;

    Self.InitPartCombo(disque);

	if Self.ComboBoxPart.Items.Count = 0 then
    begin
		Self.RadioButtonTable.Enabled := false;
        Self.RadioButtonClick(Self.RadioButtonBoot);
        Self.RadioButtonBoot.Checked := true;
    end
    else
    begin
        Self.RadioButtonClick(Self.RadioButtonTable);
        Self.RadioButtonTable.Checked := true;
    end;

    Self.ComboBoxFSType.ItemIndex := 0;

    if Self.ShowModal = mrOk then
    begin
    	//
        // selon le type d'action
        //

		if Self.RadioButtonTable.Checked then
        begin
            if Self.ComboBoxPart.ItemIndex <> - 1 then
            begin
                partinfo := disque.PartitionInfo[Cardinal(Self.ComboBoxPart.Items.Objects[Self.ComboBoxPart.ItemIndex])];

                // Selon le type de partition on crée l'objet idoine
                case TGenericDrive.GetFileSystemFamily(partinfo.FPartitionType) of
                    fsFAT32: // FAT 32;
						begin
                            Result := TFAT32Partition.Create(disque, partinfo.FStartingSector);
                            if not Result.LoadFromPartInfo(partinfo) then
                            begin
                                MessageDlg(INVALID_BOOT_SECTOR_MSG, mtError, [mbOK], 0);
                                if MessageDlg(RECALC_PART_PARAM_MSG, mtConfirmation, [mbYes,mbNo], 0) = mrYes then
                                begin
                                    (Result as TFAT32Partition).ComputePartInfo(partinfo.FSectorCount, 32, 2)
                                end
                                else
                                    FreeAndNil(Result);
                            end;
                    	end;
                    fsNTFS: // NTFS
                    	begin
                            Result := TNTFSPartition.Create(disque, partinfo.FStartingSector);
                            if not Result.LoadFromPartInfo(partinfo) then
                            begin
	                            MessageDlg(INVALID_BOOT_SECTOR_MSG, mtError, [mbOK], 0);
	                            FreeAndNil(Result);
                            end
                        end;
                    fsFAT16: // FAT 16;
						begin
                            Result := TFAT16Partition.Create(disque, partinfo.FStartingSector);
                            if not Result.LoadFromPartInfo(partinfo) then
                            begin
                                MessageDlg(INVALID_BOOT_SECTOR_MSG, mtError, [mbOK], 0);
                                FreeAndNil(Result);
                            end
                    	end;
                    fsLinux: // XFS Trick;
                    begin
                      Result := TXFSPartition.Create(disque, partinfo.FStartingSector);
                            if not Result.LoadFromPartInfo(partinfo) then
                            begin
                                MessageDlg(INVALID_BOOT_SECTOR_MSG, mtError, [mbOK], 0);
                                FreeAndNil(Result);
                            end
                    end;
               else
                    MessageDlg(ATM_NOT_SUPPORTED_PART_MSG, mtInformation, [mbOK], 0);
                end; // case of
            end; // if
    	end; // RadioButtonTable

        // Mode From LBA
        if Self.RadioButtonBoot.Checked then
        begin
        	//#Todo2 Récupérer le tyep de partition attendue
            // Si format partition connu, alors créer ce type de partition
            // Sinon tester les partitions
            
        	// récupéerer le numéro de secteur
            lba := Self.GetLabeledEditValue(Self.LabeledEditLBA);

            if lba < 63 then
            begin
				MessageDlg(SECTOR_NUM_TOO_SMALL, mtError, [mbOK], 0);
                exit;
            end;

            {case Self.ComboBoxFSType.ItemIndex of
                0:	// Format partition inconnu => tester plusieurs formats
	                ;
                1:	// partition FAT 16
	                ;
                2:	// partition FAT 32
                	;
                3:	// partition NTFS
                	;
            end;}

            // Création de l'object et analyse du boot sector
            // Tester NTFS et FAT32
            Result := TNTFSPartition.Create(disque, lba);
            if not Result.LoadFromLBA(lba) then
            begin
                // Liberer la part
                FreeAndNil(Result);

                Result := TFAT32Partition.Create(disque, lba);
                if not Result.LoadFromLBA(lba) then
                begin
                	FreeAndNil(Result);
                    MessageDlg(INVALID_BOOT_SECTOR_MSG, mtError, [mbOK], 0);
                end;
            end;

        end; // RadioButtonBoot

        // Mode Expert
        //#Todo3 Renommer radiobouton ?
        if Self.RadioButtonManual.Checked then
        begin
            // Récupération des valeurs
            lba 	  := Self.GetLabeledEditValue(Self.LabelEdLBA);
            fatsize   := Self.GetLabeledEditValue(Self.LabelEdFatSize);
            rsvdsect  := Self.GetLabeledEditValue(Self.LabelEdRsvdSect);
            sectcount := Self.GetLabeledEditValue(Self.LabelEdSectCount);
            if Self.CheckBoxMirrorFAT.Checked then
            	nbfat := 2
            else
            	nbfat := 1;

            if lba < 63 then
            begin
				MessageDlg(SECTOR_NUM_TOO_SMALL, mtError, [mbOK], 0);
                exit;
            end;

            if (rsvdsect = 0) or (sectcount = 0) then
            begin
				MessageDlg(EXPERT_MODE_BAD_VALUES, mtError, [mbOK], 0);
                exit;
            end;

            // Création de la partition
            Result := TFAT32Partition.Create(disque, lba);

            // On recalcule les infos nécessaires pour la partition
            (Result as TFAT32Partition).ComputePartInfo(sectcount, rsvdsect, nbfat, fatsize);
        end;
    end;
end;


//
// Initialise la Combo avec les info de partition
//
procedure TFormSelectPart.InitPartCombo(disque: TGenericDrive);
var
	i: integer;
    chs: TCHS;
    str: string;
begin
    Self.ComboBoxPart.Clear;

    // Charger les partition infos
    disque.ReadMBR;

    // Remplit le combo box
    for i := 0 to disque.PartitionCount - 1 do
    begin
    	disque.LBAToCHS(chs, disque.PartitionInfo[i].FStartingSector);

        str := Format('Part n°%d: %s - %s [%d-%d-%d] ',
        			  [i, disque.PartitionInfo[i].FTypeName,
					  SizeTo2DigitByteString(int64(disque.PartitionInfo[i].FSectorCount) * int64(disque.BytesPerSec)),
                      chs.Cylinder, chs.Head, chs.Sector]);

        if disque.PartitionInfo[i].FPrimaryPartition then
        	str := str + '(Primary)'
        else
        	str := str + '(Logical)';

        Self.ComboBoxPart.AddItem(str, TObject(i));
    end;

    // Selectionne la 1er part si elle existe
	if disque.PartitionCount = 0 then
	    Self.ComboBoxPart.ItemIndex := -1
    else
	    Self.ComboBoxPart.ItemIndex := 0;

    // Si pas de partition on desactive le controle
    Self.ComboBoxPart.Enabled := disque.PartitionCount > 0;

    Self.LabelPartCount.Caption := Format(COMBO_PART_COUNT_STR,
									      [disque.PartitionCount]);
end;


// -------------------------------------------------------------------------
//							Gestion des events
// -------------------------------------------------------------------------


procedure TFormSelectPart.SetPartSelectionStatus(const status: boolean);
begin
	Self.ComboBoxPart.Enabled := status;
    Self.LabelPartCount.Enabled := status;
end;


procedure TFormSelectPart.SetBootSelectionStatus(const status: boolean);
begin
	Self.LabeledEditLBA.Enabled := status;
end;


procedure TFormSelectPart.SetManualSelectionStatus(const status: boolean);
begin
	Self.LabelEdLBA.Enabled := status;
    Self.LabelEdSectCount.Enabled := status;
    Self.LabelEdRsvdSect.Enabled := status;
    Self.LabelEdFatSize.Enabled := status;
    Self.CheckBoxMirrorFAT.Enabled := status;
end;


// -------------------------------------------------------------------------
//							Gestion des events
// -------------------------------------------------------------------------


//
//	Permet d'entrer seulement des valeurs numeriques
//
procedure TFormSelectPart.FilterEditNumKey(Sender: TObject; var Key: Char);
begin
	if not (key in [#8,'0'..'9']) then
        key := #0;
end;


//
// Gere de/activation selon le mode choisi
//
procedure TFormSelectPart.RadioButtonClick(Sender: TObject);
begin
    SetPartSelectionStatus(Self.RadioButtonTable = Sender);
    SetBootSelectionStatus(Self.RadioButtonBoot = Sender);
    SetManualSelectionStatus(Self.RadioButtonManual = Sender);
end;


//
// Récupère le contenu d'un TEdit => Int, si chaine vide => 0
//
function TFormSelectPart.GetLabeledEditValue(labeled: TLabeledEdit): Cardinal;
begin
    try
        Result := StrToInt(labeled.Text);
    except
    	// En cas d'erreur (par ex chaine vide) on renvoie 0
        Result := 0;
    end;
end;

// --------------------------- Fin de l'unité -----------------------------
end.
 