unit TicTacToe.Movement.Steps;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  TicTacToe.Game,
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
    [When('(\w+) mueve de \((\d+),(\d+)\) a \((\d+),(\d+)\)')]
    procedure MovePiece(Ctx: TGameWorld; Player: string; FR, FC, TR, TC: Integer);

    [When('(\w+) intenta mover de \((\d+),(\d+)\) a \((\d+),(\d+)\)')]
    procedure TryMove(Ctx: TGameWorld; Player: string; FR, FC, TR, TC: Integer);

    // === Then ===
    [ThenAttribute('\((\d+),(\d+)\) está vacía')]
    procedure CellIsEmpty(Ctx: TGameWorld; Row, Col: Integer);
  end;

{ TMovementSteps }

procedure TMovementSteps.GameInPlacementPhase(Ctx: TGameWorld);
begin
  Ctx.Game.Free;
  Ctx.Game := TTicTacToeGame.Create;
  Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X coloca una ficha
end;

procedure TMovementSteps.MovePiece(Ctx: TGameWorld; Player: string; FR, FC, TR, TC: Integer);
begin
  Ctx.Game.MovePiece(TPosition.Create(FR, FC), TPosition.Create(TR, TC));
end;

procedure TMovementSteps.TryMove(Ctx: TGameWorld; Player: string; FR, FC, TR, TC: Integer);
begin
  Ctx.Game.MovePiece(TPosition.Create(FR, FC), TPosition.Create(TR, TC));
end;

procedure TMovementSteps.CellIsEmpty(Ctx: TGameWorld; Row, Col: Integer);
begin
  Expect(Ctx.Game.Board[Row, Col]).ToEqual(TPlayer.None);
end;

initialization
  Bindings.RegisterSteps<TMovementSteps>;

end.
