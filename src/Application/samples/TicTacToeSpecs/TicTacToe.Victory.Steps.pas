unit TicTacToe.Victory.Steps;

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
  /// Step bindings for victory condition scenarios.
  /// </summary>
  TVictorySteps = class
  public
    // === Then ===
    [ThenAttribute('(\w+) gana')]
    procedure PlayerWins(Ctx: TGameWorld; Player: string);

    [ThenAttribute('el estado es En Progreso')]
    procedure StatusInProgress(Ctx: TGameWorld);
  end;

{ TVictorySteps }

procedure TVictorySteps.PlayerWins(Ctx: TGameWorld; Player: string);
begin
  if SameText(Player, 'X') then
    Expect(Ctx.Game.Status).ToEqual(TGameStatus.XWins)
  else
    Expect(Ctx.Game.Status).ToEqual(TGameStatus.OWins);
end;

procedure TVictorySteps.StatusInProgress(Ctx: TGameWorld);
begin
  Expect(Ctx.Game.Status).ToEqual(TGameStatus.InProgress);
end;

initialization
  Bindings.RegisterSteps<TVictorySteps>;

end.
