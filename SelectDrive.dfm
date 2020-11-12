object FormSelectDrive: TFormSelectDrive
  Left = 253
  Top = 242
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 148
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 8
    Top = 8
    Width = 465
    Height = 105
    Columns = <
      item
        Caption = 'Model'
        Width = 150
      end
      item
        Caption = 'State'
        Width = 70
      end
      item
        Caption = 'Size'
        Width = 79
      end
      item
        Caption = 'Cylinders'
        Width = 54
      end
      item
        Caption = 'Heads'
        Width = 54
      end
      item
        Caption = 'Sectors'
        Width = 54
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object BitBtnOk: TBitBtn
    Left = 88
    Top = 120
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 320
    Top = 120
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
