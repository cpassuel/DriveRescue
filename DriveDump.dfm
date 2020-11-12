object FormDump: TFormDump
  Left = 288
  Top = 182
  BorderStyle = bsSingle
  ClientHeight = 316
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MemoHexDump: TMemo
    Left = 0
    Top = 0
    Width = 434
    Height = 245
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBoxPos: TGroupBox
    Left = 168
    Top = 248
    Width = 177
    Height = 65
    Caption = ' Position '
    TabOrder = 1
    object SpeedButtonDown: TSpeedButton
      Left = 122
      Top = 35
      Width = 23
      Height = 22
      Caption = '<'
      OnClick = SpeedButtonDownClick
    end
    object SpeedButtonUp: TSpeedButton
      Left = 146
      Top = 35
      Width = 23
      Height = 22
      Caption = '>'
      OnClick = SpeedButtonUpClick
    end
    object ButtonRead: TButton
      Left = 88
      Top = 32
      Width = 33
      Height = 25
      Caption = '&Go'
      TabOrder = 0
      OnClick = ButtonReadClick
    end
    object LabeledEditPos: TLabeledEdit
      Left = 8
      Top = 32
      Width = 73
      Height = 21
      EditLabel.Width = 3
      EditLabel.Height = 13
      EditLabel.Caption = 'LabeledEditPos'
      LabelPosition = lpAbove
      LabelSpacing = 3
      MaxLength = 10
      TabOrder = 1
    end
  end
  object RadioGroupMode: TRadioGroup
    Left = 8
    Top = 248
    Width = 153
    Height = 41
    Caption = ' Mode '
    Columns = 2
    Items.Strings = (
      '&Cluster'
      '&Sector')
    TabOrder = 2
    OnClick = RadioGroupModeClick
  end
  object StaticTextInfo: TStaticText
    Left = 8
    Top = 296
    Width = 153
    Height = 17
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvSpace
    BevelKind = bkTile
    TabOrder = 3
  end
  object ButtonSaveHexa: TButton
    Left = 352
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Save &Hexa...'
    TabOrder = 4
    OnClick = ButtonSaveClick
  end
  object ButtonSaveBin: TButton
    Left = 352
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Save &Bin...'
    TabOrder = 5
    OnClick = ButtonSaveClick
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 376
    Top = 208
  end
end
