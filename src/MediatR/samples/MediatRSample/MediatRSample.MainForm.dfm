object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MediatR Sample'#13#10'Commands '#183' Queries '#183' Notifications '#183' Behaviors'
  ClientHeight = 340
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object TopArea: TPanel
    Left = 0
    Top = 0
    Width = 641
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object NameLabel: TLabel
      Left = 12
      Top = 10
      Width = 35
      Height = 15
      Caption = 'Name:'
    end
    object FilterLabel: TLabel
      Left = 12
      Top = 41
      Width = 29
      Height = 15
      Caption = 'Filter:'
    end
    object NameCtl: TEdit
      Left = 55
      Top = 7
      Width = 284
      Height = 23
      TabOrder = 0
    end
    object AddCtl: TButton
      Left = 345
      Top = 6
      Width = 85
      Height = 25
      Caption = 'Add'
      Default = True
      TabOrder = 1
      OnClick = AddCtlClick
    end
    object RemoveCtl: TButton
      Left = 436
      Top = 6
      Width = 130
      Height = 25
      Caption = 'Remove Selected'
      TabOrder = 2
      OnClick = RemoveCtlClick
    end
    object FilterCtl: TEdit
      Left = 55
      Top = 38
      Width = 180
      Height = 23
      TabOrder = 3
    end
    object SearchCtl: TButton
      Left = 241
      Top = 37
      Width = 74
      Height = 25
      Caption = 'Search'
      TabOrder = 4
      OnClick = SearchCtlClick
    end
    object AllCtl: TButton
      Left = 321
      Top = 37
      Width = 50
      Height = 25
      Caption = 'All'
      TabOrder = 5
      OnClick = AllCtlClick
    end
  end
  object LogArea: TGroupBox
    Left = 0
    Top = 240
    Width = 641
    Height = 100
    Align = alBottom
    Caption = 'Pipeline Log'
    TabOrder = 1
    object LogCtl: TMemo
      Left = 2
      Top = 17
      Width = 637
      Height = 81
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitWidth = 639
      ExplicitHeight = 82
    end
  end
  object CustomersCtl: TListView
    Left = 0
    Top = 65
    Width = 641
    Height = 175
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
