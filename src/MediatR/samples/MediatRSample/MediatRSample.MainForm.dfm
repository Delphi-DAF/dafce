object MainForm: TMainForm
  Left = 0
  Top = 0
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'Mediator VCL Sample'
  ClientHeight = 681
  ClientWidth = 1283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 192
  TextHeight = 32
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1283
    Height = 130
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alTop
    TabOrder = 0
    object lblCustomerName: TLabel
      Left = 32
      Top = 48
      Width = 175
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Nombre Cliente:'
    end
    object edtCustomerName: TEdit
      Left = 236
      Top = 42
      Width = 694
      Height = 40
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 0
    end
    object btnAddCustomer: TButton
      Left = 942
      Top = 40
      Width = 260
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Agregar Cliente'
      Default = True
      TabOrder = 1
      OnClick = btnAddCustomerClick
    end
  end
  object lvCustomers: TListView
    Left = 0
    Top = 130
    Width = 1283
    Height = 551
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Nombre'
      end
      item
        AutoSize = True
        Caption = 'ID'
      end>
    TabOrder = 1
    ViewStyle = vsReport
  end
end
