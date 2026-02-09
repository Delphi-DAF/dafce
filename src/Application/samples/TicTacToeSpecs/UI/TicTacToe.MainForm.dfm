object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'TicTacToe'
  ClientHeight = 420
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object lblStatus: TLabel
    Left = 16
    Top = 300
    Width = 272
    Height = 40
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object pnlBoard: TPanel
    Left = 16
    Top = 16
    Width = 272
    Height = 272
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 0
  end
  object btnNewGame: TButton
    Left = 72
    Top = 360
    Width = 160
    Height = 44
    Caption = 'Nueva Partida'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnNewGameClick
  end
end
