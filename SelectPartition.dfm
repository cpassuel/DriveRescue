object FormSelectPart: TFormSelectPart
  Left = 198
  Top = 107
  BorderStyle = bsDialog
  Caption = 'S'#233'lection de partition...'
  ClientHeight = 344
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 401
    Height = 297
  end
  object LabelPartCount: TLabel
    Left = 72
    Top = 72
    Width = 257
    Height = 13
    AutoSize = False
  end
  object LabelPartFS: TLabel
    Left = 201
    Top = 140
    Width = 91
    Height = 13
    Caption = 'Syst'#232'me de fichiers'
    Enabled = False
  end
  object ComboBoxPart: TComboBox
    Left = 72
    Top = 40
    Width = 265
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object BitBtnOk: TBitBtn
    Left = 72
    Top = 312
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 272
    Top = 312
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object RadioButtonTable: TRadioButton
    Left = 16
    Top = 16
    Width = 345
    Height = 17
    Caption = 'S'#233'lection '#224' partir de la table des partitions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = RadioButtonClick
  end
  object RadioButtonBoot: TRadioButton
    Left = 16
    Top = 104
    Width = 329
    Height = 17
    Caption = 'S'#233'lection '#224' partir du d'#233'but de la partition (LBA)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = RadioButtonClick
  end
  object RadioButtonManual: TRadioButton
    Left = 16
    Top = 184
    Width = 225
    Height = 17
    Caption = 'Mode Expert (FAT32 seulement)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = RadioButtonClick
  end
  object LabeledEditLBA: TLabeledEdit
    Left = 72
    Top = 136
    Width = 89
    Height = 21
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'LBA'
    LabelPosition = lpLeft
    MaxLength = 10
    TabOrder = 6
    OnKeyPress = FilterEditNumKey
  end
  object ComboBoxFSType: TComboBox
    Left = 296
    Top = 136
    Width = 81
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      'Don'#39't know'
      'FAT 16'
      'FAT 32'
      'NTFS')
  end
  object LabelEdLBA: TLabeledEdit
    Left = 72
    Top = 216
    Width = 89
    Height = 21
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'LBA'
    LabelPosition = lpLeft
    TabOrder = 8
    OnKeyPress = FilterEditNumKey
  end
  object LabelEdSectCount: TLabeledEdit
    Left = 296
    Top = 216
    Width = 81
    Height = 21
    EditLabel.Width = 57
    EditLabel.Height = 13
    EditLabel.Caption = 'Nb secteurs'
    LabelPosition = lpLeft
    TabOrder = 9
    OnKeyPress = FilterEditNumKey
  end
  object LabelEdRsvdSect: TLabeledEdit
    Left = 296
    Top = 248
    Width = 33
    Height = 21
    EditLabel.Width = 85
    EditLabel.Height = 13
    EditLabel.Caption = 'Reserved Sectors'
    LabelPosition = lpLeft
    TabOrder = 10
    Text = '32'
    OnKeyPress = FilterEditNumKey
  end
  object CheckBoxMirrorFAT: TCheckBox
    Left = 13
    Top = 247
    Width = 73
    Height = 17
    Alignment = taLeftJustify
    Caption = 'FAT Mirroir'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object LabelEdFatSize: TLabeledEdit
    Left = 72
    Top = 272
    Width = 57
    Height = 21
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'FAT Size'
    LabelPosition = lpLeft
    TabOrder = 12
    OnKeyPress = FilterEditNumKey
  end
end
