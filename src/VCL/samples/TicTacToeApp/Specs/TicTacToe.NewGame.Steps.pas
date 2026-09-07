unit TicTacToe.NewGame.Steps;

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
  /// Step bindings for new game / reset scenarios.
  /// </summary>
  TNewGameSteps = class
  public
    [When('el jugador inicia nueva partida')]
    procedure StartNewGame(Ctx: TGameWorld);

    [ThenAttribute('el tablero está vacío')]
    procedure BoardIsEmpty(Ctx: TGameWorld);

    [ThenAttribute('el turno es de (\w+)')]
    procedure TurnIs(Ctx: TGameWorld; Player: string);

    [ThenAttribute('el juego está en fase de colocación')]
    procedure PhaseIsPlacement(Ctx: TGameWorld);

    [ThenAttribute('la partida no ha terminado')]
    procedure GameNotOver(Ctx: TGameWorld);
  end;

{ TNewGameSteps }

procedure TNewGameSteps.StartNewGame(Ctx: TGameWorld);
begin
  Ctx.ViewModel.NewGame;
end;

procedure TNewGameSteps.BoardIsEmpty(Ctx: TGameWorld);
var
  R, C, EmptyCount: Integer;
begin
  EmptyCount := 0;
  for R := 0 to 2 do
    for C := 0 to 2 do
      if Ctx.ViewModel.IsEmpty(R, C) then
        Inc(EmptyCount);
  Expect(EmptyCount).ToEqual(9);
end;

procedure TNewGameSteps.TurnIs(Ctx: TGameWorld; Player: string);
begin
  Expect(PlayerName(Ctx.ViewModel.CurrentPlayer)).ToEqual(Player);
end;

procedure TNewGameSteps.PhaseIsPlacement(Ctx: TGameWorld);
begin
  Expect(Ctx.ViewModel.Phase).ToEqual(TGamePhase.Placement);
end;

procedure TNewGameSteps.GameNotOver(Ctx: TGameWorld);
begin
  Expect(Ctx.ViewModel.IsGameOver).ToEqual(False);
end;

initialization
  Bindings.RegisterSteps<TNewGameSteps>;

end.
