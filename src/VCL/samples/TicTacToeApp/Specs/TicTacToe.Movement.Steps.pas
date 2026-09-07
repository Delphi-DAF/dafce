unit TicTacToe.Movement.Steps;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  TicTacToe.Game,
  TicTacToe.ViewModel,
  TicTacToe.SpecHelpers;

type
  /// <summary>
  /// Step bindings for movement scenarios.
  /// </summary>
  TMovementSteps = class
  public
    // === Given ===
    [Given('el juego está en fase de colocación')]
    procedure GameInPlacementPhase(Ctx: TGameWorld);

    // === When ===
    [When('(\w+) mueve \(([a-c][1-3]),([a-c][1-3])\)')]
    procedure MovePiece(Ctx: TGameWorld; Player, FromCell, ToCell: string);

    [When('(\w+) intenta mover \(([a-c][1-3]),([a-c][1-3])\)')]
    procedure TryMove(Ctx: TGameWorld; Player, FromCell, ToCell: string);

    // === Then ===
    [ThenAttribute('([a-c][1-3]) está vacía')]
    procedure CellIsEmpty(Ctx: TGameWorld; Cell: string);
  end;

{ TMovementSteps }

procedure TMovementSteps.GameInPlacementPhase(Ctx: TGameWorld);
begin
  Ctx.ViewModel.NewGame;
  Ctx.ViewModel.PlacePiece(0, 0); // X coloca una ficha
end;

procedure TMovementSteps.MovePiece(Ctx: TGameWorld; Player, FromCell, ToCell: string);
var
  F, T: TPosition;
begin
  F := ParseCell(FromCell);
  T := ParseCell(ToCell);
  Ctx.ViewModel.MovePiece(F.Row, F.Col, T.Row, T.Col);
end;

procedure TMovementSteps.TryMove(Ctx: TGameWorld; Player, FromCell, ToCell: string);
var
  F, T: TPosition;
begin
  F := ParseCell(FromCell);
  T := ParseCell(ToCell);
  Ctx.ViewModel.MovePiece(F.Row, F.Col, T.Row, T.Col);
end;

procedure TMovementSteps.CellIsEmpty(Ctx: TGameWorld; Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Expect(Ctx.ViewModel.IsEmpty(P.Row, P.Col)).ToEqual(True);
end;

initialization
  Bindings.RegisterSteps<TMovementSteps>;

end.
