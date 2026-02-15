unit TicTacToe.MainForm;

interface

uses
  System.SysUtils, System.Classes, System.Types,
  Winapi.Windows,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
  TicTacToe.ViewPort,
  TicTacToe.ViewModel;

type
  /// <summary>
  /// VCL Form implementing IGameViewPort (Humble Object).
  /// Custom-painted dark-theme board with rounded cells,
  /// hand-drawn X/O strokes, hover highlights and selection glow.
  /// Movement lines show valid paths. Piece reserves show remaining pieces.
  /// All presentation logic lives in the ViewModel.
  /// </summary>
  TMainForm = class(TForm, IGameViewPort)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FViewModel: TGameViewModel;
    FBoard: TPaintBox;
    FReserveX: TPaintBox;
    FReserveO: TPaintBox;
    FLblTitle: TLabel;
    FLblStatus: TLabel;
    FLblPhase: TLabel;
    FBtnNewGame: TPanel;
    FHoverRow: Integer;
    FHoverCol: Integer;
    procedure BuildUI;
    procedure PaintBoard(Sender: TObject);
    procedure BoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BoardMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BoardMouseLeave(Sender: TObject);
    procedure BtnNewGameClick(Sender: TObject);
    procedure BtnMouseEnter(Sender: TObject);
    procedure BtnMouseLeave(Sender: TObject);
    function HitTestCell(X, Y: Integer; out Row, Col: Integer): Boolean;
    function GetCellRect(Row, Col: Integer): TRect;
    procedure PaintCellBg(ACanvas: TCanvas; ARect: TRect;
      const CellVal: string; Row, Col: Integer);
    procedure PaintX(ACanvas: TCanvas; ARect: TRect);
    procedure PaintO(ACanvas: TCanvas; ARect: TRect);
    procedure PaintMovementLines(ACanvas: TCanvas);
    procedure PaintReserveX(Sender: TObject);
    procedure PaintReserveO(Sender: TObject);
    procedure PaintMiniX(ACanvas: TCanvas; CX, CY, Size: Integer; AColor: TColor);
    procedure PaintMiniO(ACanvas: TCanvas; CX, CY, Size: Integer; AColor: TColor);
    { IGameViewPort }
    procedure Refresh;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  CellSize     = 100;
  CellGap      = 8;
  BoardExtent  = CellSize * 3 + CellGap * 2;
  BoardPadding = 28;
  Radius       = 12;
  PieceInset   = 22;
  StrokeWidth  = 7;

  // Movement lines
  LineColor    = $00606060;  // RGB(96,96,96) - subtle gray
  LineWidth    = 2;

  // Piece reserves
  ReserveWidth  = 60;
  ReserveGap    = 16;
  MiniPieceSize = 14;       // radius of mini piece
  MiniPieceGap  = 8;        // vertical gap between mini pieces

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FHoverRow := -1;
  FHoverCol := -1;
  DoubleBuffered := True;
  FViewModel := TGameViewModel.Create;
  FViewModel.ViewPort := Self;
  BuildUI;
  Refresh;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FViewModel.ViewPort := nil;
  FViewModel.Free;
end;

procedure TMainForm.BuildUI;
var
  W, BoardLeft, ReserveHeight, Y: Integer;
begin
  // Total width: reserve + gap + board + gap + reserve + padding
  W := BoardPadding + ReserveWidth + ReserveGap + BoardExtent
     + ReserveGap + ReserveWidth + BoardPadding;
  BoardLeft := BoardPadding + ReserveWidth + ReserveGap;
  ReserveHeight := BoardExtent;
  Y := 20;

  // Title
  FLblTitle := TLabel.Create(Self);
  FLblTitle.Parent := Self;
  FLblTitle.Transparent := True;
  FLblTitle.AutoSize := False;
  FLblTitle.Alignment := taCenter;
  FLblTitle.SetBounds(0, Y, W, 44);
  FLblTitle.Font.Name := 'Segoe UI';
  FLblTitle.Font.Size := 24;
  FLblTitle.Font.Style := [fsBold];
  FLblTitle.Font.Color := $00FFFFFF;
  FLblTitle.Caption := 'TIC '#$00B7' TAC '#$00B7' TOE';
  Inc(Y, 44 + 16);

  // Reserve X (left side)
  FReserveX := TPaintBox.Create(Self);
  FReserveX.Parent := Self;
  FReserveX.SetBounds(BoardPadding, Y, ReserveWidth, ReserveHeight);
  FReserveX.OnPaint := PaintReserveX;

  // Board (custom-painted)
  FBoard := TPaintBox.Create(Self);
  FBoard.Parent := Self;
  FBoard.SetBounds(BoardLeft, Y, BoardExtent, BoardExtent);
  FBoard.OnPaint := PaintBoard;
  FBoard.OnMouseDown := BoardMouseDown;
  FBoard.OnMouseMove := BoardMouseMove;
  FBoard.OnMouseLeave := BoardMouseLeave;

  // Reserve O (right side)
  FReserveO := TPaintBox.Create(Self);
  FReserveO.Parent := Self;
  FReserveO.SetBounds(BoardLeft + BoardExtent + ReserveGap, Y,
    ReserveWidth, ReserveHeight);
  FReserveO.OnPaint := PaintReserveO;

  Inc(Y, BoardExtent + 24);

  // Status
  FLblStatus := TLabel.Create(Self);
  FLblStatus.Parent := Self;
  FLblStatus.Transparent := True;
  FLblStatus.AutoSize := False;
  FLblStatus.Alignment := taCenter;
  FLblStatus.SetBounds(0, Y, W, 36);
  FLblStatus.Font.Name := 'Segoe UI';
  FLblStatus.Font.Size := 16;
  FLblStatus.Font.Style := [fsBold];
  FLblStatus.Font.Color := RGB(200, 200, 200);
  Inc(Y, 36 + 4);

  // Phase indicator (subtle)
  FLblPhase := TLabel.Create(Self);
  FLblPhase.Parent := Self;
  FLblPhase.Transparent := True;
  FLblPhase.AutoSize := False;
  FLblPhase.Alignment := taCenter;
  FLblPhase.SetBounds(0, Y, W, 22);
  FLblPhase.Font.Name := 'Segoe UI';
  FLblPhase.Font.Size := 10;
  FLblPhase.Font.Color := RGB(120, 120, 120);
  Inc(Y, 22 + 16);

  // New Game button (styled flat panel)
  FBtnNewGame := TPanel.Create(Self);
  FBtnNewGame.Parent := Self;
  FBtnNewGame.ParentBackground := False;
  FBtnNewGame.BevelOuter := bvNone;
  FBtnNewGame.SetBounds((W - 180) div 2, Y, 180, 46);
  FBtnNewGame.Color := RGB(0, 122, 204);
  FBtnNewGame.Font.Name := 'Segoe UI';
  FBtnNewGame.Font.Size := 13;
  FBtnNewGame.Font.Style := [fsBold];
  FBtnNewGame.Font.Color := $00FFFFFF;
  FBtnNewGame.Caption := 'Nueva Partida';
  FBtnNewGame.Cursor := crHandPoint;
  FBtnNewGame.OnClick := BtnNewGameClick;
  FBtnNewGame.OnMouseEnter := BtnMouseEnter;
  FBtnNewGame.OnMouseLeave := BtnMouseLeave;
  Inc(Y, 46 + 24);

  ClientWidth := W;
  ClientHeight := Y;
end;

// === Board geometry ===

function TMainForm.GetCellRect(Row, Col: Integer): TRect;
begin
  Result := Rect(
    Col * (CellSize + CellGap),
    Row * (CellSize + CellGap),
    Col * (CellSize + CellGap) + CellSize,
    Row * (CellSize + CellGap) + CellSize);
end;

function TMainForm.HitTestCell(X, Y: Integer; out Row, Col: Integer): Boolean;
var
  R, C: Integer;
begin
  for R := 0 to 2 do
    for C := 0 to 2 do
      if PtInRect(GetCellRect(R, C), Point(X, Y)) then
      begin
        Row := R;
        Col := C;
        Exit(True);
      end;
  Result := False;
end;

// === Custom painting ===

procedure TMainForm.PaintCellBg(ACanvas: TCanvas; ARect: TRect;
  const CellVal: string; Row, Col: Integer);
var
  Bg: TColor;
  IsHover, IsSel: Boolean;
begin
  IsHover := (Row = FHoverRow) and (Col = FHoverCol);
  IsSel := FViewModel.IsSelected(Row, Col);

  // Background tint depends on content
  if CellVal = 'X' then
    Bg := RGB(55, 42, 38)
  else if CellVal = 'O' then
    Bg := RGB(38, 50, 55)
  else if IsHover and not FViewModel.IsGameOver then
    Bg := RGB(75, 75, 75)
  else
    Bg := RGB(58, 58, 58);

  ACanvas.Brush.Color := Bg;
  ACanvas.Pen.Style := psClear;
  ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
    Radius, Radius);

  // Selection glow (golden border)
  if IsSel then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := RGB(255, 200, 60);
    ACanvas.Pen.Width := 3;
    InflateRect(ARect, -2, -2);
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
      Radius, Radius);
    ACanvas.Pen.Width := 1;
  end;
  ACanvas.Brush.Style := bsSolid;
end;

procedure TMainForm.PaintX(ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := RGB(255, 155, 71);  // warm orange
  ACanvas.Pen.Width := StrokeWidth;
  ACanvas.MoveTo(ARect.Left + PieceInset, ARect.Top + PieceInset);
  ACanvas.LineTo(ARect.Right - PieceInset, ARect.Bottom - PieceInset);
  ACanvas.MoveTo(ARect.Right - PieceInset, ARect.Top + PieceInset);
  ACanvas.LineTo(ARect.Left + PieceInset, ARect.Bottom - PieceInset);
  ACanvas.Pen.Width := 1;
end;

procedure TMainForm.PaintO(ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := RGB(78, 205, 196);  // cool teal
  ACanvas.Pen.Width := StrokeWidth;
  ACanvas.Ellipse(
    ARect.Left + PieceInset, ARect.Top + PieceInset,
    ARect.Right - PieceInset, ARect.Bottom - PieceInset);
  ACanvas.Pen.Width := 1;
  ACanvas.Brush.Style := bsSolid;
end;

procedure TMainForm.PaintMovementLines(ACanvas: TCanvas);

  function CellCenter(Row, Col: Integer): TPoint;
  var
    R: TRect;
  begin
    R := GetCellRect(Row, Col);
    Result := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
  end;

  procedure Line(R1, C1, R2, C2: Integer);
  var
    P1, P2: TPoint;
  begin
    P1 := CellCenter(R1, C1);
    P2 := CellCenter(R2, C2);
    ACanvas.MoveTo(P1.X, P1.Y);
    ACanvas.LineTo(P2.X, P2.Y);
  end;

begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := LineColor;
  ACanvas.Pen.Width := LineWidth;

  // Outer square (perimeter): a1-a2-a3, a3-b3-c3, c3-c2-c1, c1-b1-a1
  Line(0,0, 0,1); Line(0,1, 0,2);  // top row
  Line(0,2, 1,2); Line(1,2, 2,2);  // right col
  Line(2,2, 2,1); Line(2,1, 2,0);  // bottom row
  Line(2,0, 1,0); Line(1,0, 0,0);  // left col

  // Central cross: a2-b2-c2 (vertical) and b1-b2-b3 (horizontal)
  Line(0,1, 1,1); Line(1,1, 2,1);  // vertical center
  Line(1,0, 1,1); Line(1,1, 1,2);  // horizontal center

  // Major diagonals: a1-b2-c3 (D1) and a3-b2-c1 (D2)
  Line(0,0, 1,1); Line(1,1, 2,2);  // D1
  Line(0,2, 1,1); Line(1,1, 2,0);  // D2

  ACanvas.Pen.Width := 1;
end;

procedure TMainForm.PaintBoard(Sender: TObject);
var
  R, C: Integer;
  CR: TRect;
  Val: string;
begin
  // Board background (visible as grid gaps)
  FBoard.Canvas.Brush.Color := RGB(44, 44, 44);
  FBoard.Canvas.FillRect(Rect(0, 0, FBoard.Width, FBoard.Height));

  // Movement lines (drawn under cells)
  PaintMovementLines(FBoard.Canvas);

  for R := 0 to 2 do
    for C := 0 to 2 do
    begin
      CR := GetCellRect(R, C);
      Val := FViewModel.CellText(R, C);
      PaintCellBg(FBoard.Canvas, CR, Val, R, C);
      if Val = 'X' then
        PaintX(FBoard.Canvas, CR)
      else if Val = 'O' then
        PaintO(FBoard.Canvas, CR);
    end;
end;

// === Piece reserves ===

procedure TMainForm.PaintMiniX(ACanvas: TCanvas; CX, CY, Size: Integer;
  AColor: TColor);
begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := 3;
  ACanvas.MoveTo(CX - Size, CY - Size);
  ACanvas.LineTo(CX + Size, CY + Size);
  ACanvas.MoveTo(CX + Size, CY - Size);
  ACanvas.LineTo(CX - Size, CY + Size);
  ACanvas.Pen.Width := 1;
end;

procedure TMainForm.PaintMiniO(ACanvas: TCanvas; CX, CY, Size: Integer;
  AColor: TColor);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := 3;
  ACanvas.Ellipse(CX - Size, CY - Size, CX + Size, CY + Size);
  ACanvas.Pen.Width := 1;
  ACanvas.Brush.Style := bsSolid;
end;

procedure TMainForm.PaintReserveX(Sender: TObject);
var
  C: TCanvas;
  I, CX, CY, Remaining: Integer;
  PieceColor: TColor;
begin
  C := FReserveX.Canvas;
  C.Brush.Color := Self.Color;
  C.FillRect(Rect(0, 0, FReserveX.Width, FReserveX.Height));

  // Label
  C.Font.Name := 'Segoe UI';
  C.Font.Size := 11;
  C.Font.Style := [fsBold];
  C.Font.Color := RGB(255, 155, 71);
  C.Brush.Style := bsClear;
  C.TextOut((FReserveX.Width - C.TextWidth('X')) div 2, 8, 'X');
  C.Brush.Style := bsSolid;

  Remaining := FViewModel.XRemainingPieces;
  CX := FReserveX.Width div 2;

  for I := 0 to 2 do
  begin
    CY := 40 + I * (MiniPieceSize * 2 + MiniPieceGap);
    if I < Remaining then
      PieceColor := RGB(255, 155, 71)   // active: warm orange
    else
      PieceColor := RGB(70, 60, 55);    // spent: dim brown
    PaintMiniX(C, CX, CY, MiniPieceSize, PieceColor);
  end;
end;

procedure TMainForm.PaintReserveO(Sender: TObject);
var
  C: TCanvas;
  I, CX, CY, Remaining: Integer;
  PieceColor: TColor;
begin
  C := FReserveO.Canvas;
  C.Brush.Color := Self.Color;
  C.FillRect(Rect(0, 0, FReserveO.Width, FReserveO.Height));

  // Label
  C.Font.Name := 'Segoe UI';
  C.Font.Size := 11;
  C.Font.Style := [fsBold];
  C.Font.Color := RGB(78, 205, 196);
  C.Brush.Style := bsClear;
  C.TextOut((FReserveO.Width - C.TextWidth('O')) div 2, 8, 'O');
  C.Brush.Style := bsSolid;

  Remaining := FViewModel.ORemainingPieces;
  CX := FReserveO.Width div 2;

  for I := 0 to 2 do
  begin
    CY := 40 + I * (MiniPieceSize * 2 + MiniPieceGap);
    if I < Remaining then
      PieceColor := RGB(78, 205, 196)   // active: cool teal
    else
      PieceColor := RGB(50, 65, 65);    // spent: dim teal
    PaintMiniO(C, CX, CY, MiniPieceSize, PieceColor);
  end;
end;

// === Mouse interaction ===

procedure TMainForm.BoardMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if Button <> mbLeft then Exit;
  if not HitTestCell(X, Y, Row, Col) then Exit;
  try
    FViewModel.CellClick(Row, Col);
  except
    on E: Exception do
    begin
      FLblStatus.Font.Color := RGB(255, 100, 100);
      FLblStatus.Caption := E.Message;
    end;
  end;
end;

procedure TMainForm.BoardMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Row, Col: Integer;
begin
  if HitTestCell(X, Y, Row, Col) then
  begin
    if (Row <> FHoverRow) or (Col <> FHoverCol) then
    begin
      FHoverRow := Row;
      FHoverCol := Col;
      FBoard.Invalidate;
    end;
  end
  else if FHoverRow >= 0 then
  begin
    FHoverRow := -1;
    FHoverCol := -1;
    FBoard.Invalidate;
  end;
end;

procedure TMainForm.BoardMouseLeave(Sender: TObject);
begin
  if FHoverRow >= 0 then
  begin
    FHoverRow := -1;
    FHoverCol := -1;
    FBoard.Invalidate;
  end;
end;

// === Button ===

procedure TMainForm.BtnNewGameClick(Sender: TObject);
begin
  FViewModel.NewGame;
end;

procedure TMainForm.BtnMouseEnter(Sender: TObject);
begin
  FBtnNewGame.Color := RGB(28, 151, 234);
end;

procedure TMainForm.BtnMouseLeave(Sender: TObject);
begin
  FBtnNewGame.Color := RGB(0, 122, 204);
end;

// === IGameViewPort ===

procedure TMainForm.Refresh;
begin
  if FViewModel.IsGameOver then
    FLblStatus.Font.Color := RGB(255, 215, 0)    // gold
  else
    FLblStatus.Font.Color := RGB(200, 200, 200);  // light gray

  FLblStatus.Caption := FViewModel.StatusText;

  if not FViewModel.IsGameOver then
    FLblPhase.Caption := FViewModel.PhaseText
  else
    FLblPhase.Caption := 'Partida terminada';

  FBoard.Invalidate;
  FReserveX.Invalidate;
  FReserveO.Invalidate;
end;

end.
