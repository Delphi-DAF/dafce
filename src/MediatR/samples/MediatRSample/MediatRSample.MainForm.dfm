object MainForm: TMainForm
  Left = 0
  Top = 0
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'MediatR Sample'#13#10'Commands '#183' Queries '#183' Notifications '#183' Behaviors'
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
    BevelOuter = bvNone
    TabOrder = 0
    object lblName: TLabel
      Left = 24
      Top = 20
      Width = 72
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Name:'
    end
    object lblFilter: TLabel
      Left = 24
      Top = 82
      Width = 78
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Filter:'
    end
    object edtName: TEdit
      Left = 110
      Top = 14
      Width = 568
      Height = 40
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 0
    end
    object btnAdd: TButton
      Left = 690
      Top = 12
      Width = 170
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Add'
      Default = True
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 872
      Top = 12
      Width = 260
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Remove Selected'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object edtFilter: TEdit
      Left = 110
      Top = 76
      Width = 360
      Height = 40
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 3
    end
    object btnSearch: TButton
      Left = 482
      Top = 74
      Width = 148
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Search'
      TabOrder = 4
      OnClick = btnSearchClick
    end
    object btnAll: TButton
      Left = 642
      Top = 74
      Width = 100
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'All'
      TabOrder = 5
      OnClick = btnAllClick
    end
  end
  object grpLog: TGroupBox
    Left = 0
    Top = 481
    Width = 1283
    Height = 200
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    Caption = 'Pipeline Log'
    TabOrder = 1
    object memoLog: TMemo
      Left = 2
      Top = 36
      Width = 1279
      Height = 162
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object lvCustomers: TListView
    Left = 0
    Top = 130
    Width = 1283
    Height = 351
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'ID'
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
end
