unit TicTacToe.UX.Steps;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  TicTacToe.ViewModel,
  TicTacToe.SpecHelpers;

type
  /// <summary>
  /// Step bindings for UX / presentation layer scenarios.
  /// Tests ViewModel display queries and CellClick interaction.
  /// </summary>
  TUXSteps = class
  public
    // === When ===
    [When('el jugador hace click en \((\d+),(\d+)\)')]
    procedure PlayerClicksCell(Ctx: TGameWorld; Row, Col: Integer);

    [When('el jugador selecciona \((\d+),(\d+)\) y hace click en \((\d+),(\d+)\)')]
    procedure SelectAndClick(Ctx: TGameWorld; SR, SC, TR, TC: Integer);

    // === Then ===
    [ThenAttribute('la celda \((\d+),(\d+)\) muestra ''(.*)''')]
    procedure CellShows(Ctx: TGameWorld; Row, Col: Integer; Expected: string);

    [ThenAttribute('el estado muestra ''(.+)''')]
    procedure StatusShows(Ctx: TGameWorld; Expected: string);

    [ThenAttribute('la fase muestra ''(.+)''')]
    procedure PhaseShows(Ctx: TGameWorld; Expected: string);

    [ThenAttribute('la partida ha terminado')]
    procedure GameIsOver(Ctx: TGameWorld);

    [ThenAttribute('\((\d+),(\d+)\) está seleccionada')]
    procedure CellIsSelected(Ctx: TGameWorld; Row, Col: Integer);

    [ThenAttribute('\((\d+),(\d+)\) no está seleccionada')]
    procedure CellIsNotSelected(Ctx: TGameWorld; Row, Col: Integer);
  end;

{ TUXSteps }

procedure TUXSteps.PlayerClicksCell(Ctx: TGameWorld; Row, Col: Integer);
begin
  Ctx.ViewModel.CellClick(Row, Col);
end;

procedure TUXSteps.SelectAndClick(Ctx: TGameWorld; SR, SC, TR, TC: Integer);
begin
  Ctx.ViewModel.CellClick(SR, SC);
  Ctx.ViewModel.CellClick(TR, TC);
end;

procedure TUXSteps.CellShows(Ctx: TGameWorld; Row, Col: Integer; Expected: string);
begin
  Expect(Ctx.ViewModel.CellText(Row, Col)).ToEqual(Expected);
end;

procedure TUXSteps.StatusShows(Ctx: TGameWorld; Expected: string);
begin
  Expect(Ctx.ViewModel.StatusText).ToEqual(Expected);
end;

procedure TUXSteps.PhaseShows(Ctx: TGameWorld; Expected: string);
begin
  Expect(Ctx.ViewModel.PhaseText).ToEqual(Expected);
end;

procedure TUXSteps.GameIsOver(Ctx: TGameWorld);
begin
  Expect(Ctx.ViewModel.IsGameOver).ToEqual(True);
end;

procedure TUXSteps.CellIsSelected(Ctx: TGameWorld; Row, Col: Integer);
begin
  Expect(Ctx.ViewModel.IsSelected(Row, Col)).ToEqual(True);
end;

procedure TUXSteps.CellIsNotSelected(Ctx: TGameWorld; Row, Col: Integer);
begin
  Expect(Ctx.ViewModel.IsSelected(Row, Col)).ToEqual(False);
end;

initialization
  Bindings.RegisterSteps<TUXSteps>;

end.
