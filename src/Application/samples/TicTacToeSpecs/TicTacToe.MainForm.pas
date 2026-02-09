unit TicTacToe.MainForm;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
  TicTacToe.ViewPort,
  TicTacToe.ViewModel;

type
  /// <summary>
  /// VCL Form implementing IGameViewPort (Humble Object).
  /// All presentation logic lives in the ViewModel;
  /// the Form only renders state and forwards user gestures.
  /// </summary>
  TMainForm = class(TForm, IGameViewPort)
    pnlBoard: TPanel;
    lblStatus: TLabel;
    btnNewGame: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewGameClick(Sender: TObject);
  private
    FViewModel: TGameViewModel;
    FCells: array[0..2, 0..2] of TButton;
    procedure CreateCellButtons;
    procedure CellClick(Sender: TObject);
    procedure UpdateCell(R, C: Integer);
    { IGameViewPort }
    procedure Refresh;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  CellSize = 88;
  CellGap  = 4;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FViewModel := TGameViewModel.Create;
  FViewModel.ViewPort := Self;
  CreateCellButtons;
  Refresh;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FViewModel.ViewPort := nil;
  FViewModel.Free;
end;

procedure TMainForm.CreateCellButtons;
var
  R, C: Integer;
  Btn: TButton;
begin
  for R := 0 to 2 do
    for C := 0 to 2 do
    begin
      Btn := TButton.Create(Self);
      Btn.Parent := pnlBoard;
      Btn.Left := C * (CellSize + CellGap);
      Btn.Top  := R * (CellSize + CellGap);
      Btn.Width  := CellSize;
      Btn.Height := CellSize;
      Btn.Font.Size  := 32;
      Btn.Font.Style := [fsBold];
      Btn.Tag := R * 3 + C;
      Btn.OnClick := CellClick;
      FCells[R, C] := Btn;
    end;
end;

procedure TMainForm.CellClick(Sender: TObject);
var
  R, C: Integer;
begin
  R := TButton(Sender).Tag div 3;
  C := TButton(Sender).Tag mod 3;
  try
    FViewModel.CellClick(R, C);
  except
    on E: Exception do
      lblStatus.Caption := E.Message;
  end;
end;

procedure TMainForm.btnNewGameClick(Sender: TObject);
begin
  FViewModel.NewGame;
end;

procedure TMainForm.UpdateCell(R, C: Integer);
var
  Text: string;
begin
  Text := FViewModel.CellText(R, C);
  if FViewModel.IsSelected(R, C) then
    Text := '[' + Text + ']';
  FCells[R, C].Caption := Text;

  if FViewModel.CellText(R, C) = 'X' then
    FCells[R, C].Font.Color := clBlue
  else if FViewModel.CellText(R, C) = 'O' then
    FCells[R, C].Font.Color := clRed
  else
    FCells[R, C].Font.Color := clBtnText;
end;

procedure TMainForm.Refresh;
var
  R, C: Integer;
begin
  for R := 0 to 2 do
    for C := 0 to 2 do
      UpdateCell(R, C);
  lblStatus.Caption := FViewModel.StatusText;
end;

end.
