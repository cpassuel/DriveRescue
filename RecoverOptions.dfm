object FormOptions: TFormOptions
  Left = 198
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Options --- Non Impl'#233'ment'#233' ---'
  ClientHeight = 271
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOk: TButton
    Left = 110
    Top = 240
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 190
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Annuler'
    ModalResult = 2
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ButtonDef: TButton
    Left = 270
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Defaut'
    TabOrder = 2
    OnClick = ButtonDefClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 337
    Height = 225
    ActivePage = TabSheetRec
    TabIndex = 1
    TabOrder = 3
    object TabSheetGen: TTabSheet
      Caption = '&General'
      ImageIndex = 1
      object CheckBoxExpert: TCheckBox
        Left = 24
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Mode Expert'
        Enabled = False
        TabOrder = 0
      end
    end
    object TabSheetRec: TTabSheet
      Caption = '&Recovery'
      object GroupBoxBehav: TGroupBox
        Left = 152
        Top = 56
        Width = 137
        Height = 113
        Caption = ' Error Behaviour '
        TabOrder = 0
        object RadioButtonDel: TRadioButton
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Erase File'
          TabOrder = 0
          TabStop = True
        end
        object RadioButtonIgn: TRadioButton
          Left = 8
          Top = 64
          Width = 105
          Height = 17
          Caption = 'Ignore Bad sector'
          TabOrder = 1
          TabStop = True
        end
        object RadioButtonTrunc: TRadioButton
          Left = 8
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Truncate File'
          TabOrder = 2
          TabStop = True
        end
        object RadioButtonAsk: TRadioButton
          Left = 8
          Top = 88
          Width = 113
          Height = 17
          Caption = '&Ask for action'
          TabOrder = 3
        end
      end
      object CheckBoxOw: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Overwrite files'
        TabOrder = 1
      end
      object CheckBoxZeroFile: TCheckBox
        Left = 16
        Top = 16
        Width = 121
        Height = 17
        Caption = 'Recover  empty files'
        TabOrder = 2
      end
      object CheckBoxZeroDir: TCheckBox
        Left = 16
        Top = 40
        Width = 113
        Height = 17
        Caption = 'Recover empty dir'
        TabOrder = 3
      end
      object CheckBoxResTime: TCheckBox
        Left = 16
        Top = 112
        Width = 113
        Height = 17
        Caption = 'Restore Timestamp'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 4
      end
      object CheckBoxResAttr: TCheckBox
        Left = 16
        Top = 136
        Width = 105
        Height = 17
        Caption = 'Restore Attributs'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 5
      end
    end
    object TabSheetList: TTabSheet
      Caption = '&Listing'
      ImageIndex = 2
      object CheckBoxListHead: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Add File Header'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 0
      end
      object CheckBoxListSum: TCheckBox
        Left = 16
        Top = 40
        Width = 105
        Height = 17
        Caption = 'Add File Summary'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 1
      end
      object GroupBox1: TGroupBox
        Left = 144
        Top = 16
        Width = 145
        Height = 113
        Caption = ' Info '
        TabOrder = 2
        object CheckBoxWriDate: TCheckBox
          Left = 8
          Top = 16
          Width = 121
          Height = 17
          Caption = 'Last Write'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 0
        end
        object CheckBoxFileAttr: TCheckBox
          Left = 8
          Top = 88
          Width = 121
          Height = 17
          Caption = 'File Attributes'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 1
        end
        object CheckBoxCreDate: TCheckBox
          Left = 8
          Top = 40
          Width = 121
          Height = 17
          Caption = 'Creation Date'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 2
        end
        object CheckBoxLastDate: TCheckBox
          Left = 8
          Top = 64
          Width = 121
          Height = 17
          Caption = 'Last Access Date'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 3
        end
      end
    end
  end
end
