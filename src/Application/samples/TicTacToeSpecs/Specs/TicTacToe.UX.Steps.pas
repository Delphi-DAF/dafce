unit TicTacToe.UX.Steps;

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
  /// Step bindings for UX / presentation layer scenarios.
  /// Tests ViewModel display queries and CellClick interaction.
  /// </summary>
  TUXSteps = class
  public
    // === When ===
    [When('el jugador hace click en ([a-c][1-3])')]
    procedure PlayerClicksCell(Ctx: TGameWorld; Cell: string);

    [When('el jugador selecciona ([a-c][1-3]) y hace click en ([a-c][1-3])')]
    procedure SelectAndClick(Ctx: TGameWorld; SelCell, TargetCell: string);

    // === Then ===
    [ThenAttribute('la celda ([a-c][1-3]) muestra ''(.*)''')]
    procedure CellShows(Ctx: TGameWorld; Cell, Expected: string);

    [ThenAttribute('el estado muestra ''(.+)''')]
    procedure StatusShows(Ctx: TGameWorld; Expected: string);

    [ThenAttribute('la fase muestra ''(.+)''')]
    procedure PhaseShows(Ctx: TGameWorld; Expected: string);

    [ThenAttribute('la partida ha terminado')]
    procedure GameIsOver(Ctx: TGameWorld);

    [ThenAttribute('([a-c][1-3]) está seleccionada')]
    procedure CellIsSelected(Ctx: TGameWorld; Cell: string);

    [ThenAttribute('([a-c][1-3]) no está seleccionada')]
    procedure CellIsNotSelected(Ctx: TGameWorld; Cell: string);
  end;

{ TUXSteps }

procedure TUXSteps.PlayerClicksCell(Ctx: TGameWorld; Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Ctx.ViewModel.CellClick(P.Row, P.Col);
end;

procedure TUXSteps.SelectAndClick(Ctx: TGameWorld; SelCell, TargetCell: string);
var
  S, T: TPosition;
begin
  S := ParseCell(SelCell);
  T := ParseCell(TargetCell);
  Ctx.ViewModel.CellClick(S.Row, S.Col);
  Ctx.ViewModel.CellClick(T.Row, T.Col);
end;

procedure TUXSteps.CellShows(Ctx: TGameWorld; Cell, Expected: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Expect(Ctx.ViewModel.CellText(P.Row, P.Col)).ToEqual(Expected);
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

procedure TUXSteps.CellIsSelected(Ctx: TGameWorld; Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Expect(Ctx.ViewModel.IsSelected(P.Row, P.Col)).ToEqual(True);
end;

procedure TUXSteps.CellIsNotSelected(Ctx: TGameWorld; Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Expect(Ctx.ViewModel.IsSelected(P.Row, P.Col)).ToEqual(False);
end;

initialization
  Bindings.RegisterSteps<TUXSteps>;

end.
