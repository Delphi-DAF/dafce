unit TicTacToe.Placement.Steps;

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
  /// Step bindings for placement scenarios and shared steps.
  /// Regex captures are auto-converted to method parameters.
  /// </summary>
  TPlacementSteps = class
  public
    // === Given ===
    [Given('un tablero vac[ií]o')]
    procedure EmptyBoard(Ctx: TGameWorld);

    [Given('un tablero con (\w+) en ([a-c][1-3])')]
    procedure BoardWithPiece(Ctx: TGameWorld; Player, Cell: string);

    [Given('el siguiente tablero.*')]
    procedure BoardFromTable(Ctx: TGameWorld);

    // === When ===
    [When('el jugador (\w+) coloca una ficha en ([a-c][1-3])')]
    procedure PlayerPlacesAt(Ctx: TGameWorld; Player, Cell: string);

    [When('(\w+) coloca en ([a-c][1-3]) y (\w+) coloca en ([a-c][1-3])')]
    procedure TwoPlayersPlace(Ctx: TGameWorld; P1, C1, P2, C2: string);

    [When('(\w+) coloca en ([a-c][1-3])')]
    procedure PlaceAt(Ctx: TGameWorld; Player, Cell: string);

    [When('(\w+) intenta colocar en ([a-c][1-3])')]
    procedure TryPlaceAt(Ctx: TGameWorld; Player, Cell: string);

    // === Then (shared across features) ===
    [ThenAttribute('la casilla ([a-c][1-3]) pertenece a (\w+)')]
    procedure CellBelongsTo(Ctx: TGameWorld; Cell, Player: string);

    [ThenAttribute('([a-c][1-3]) pertenece a (\w+)')]
    procedure CellIs(Ctx: TGameWorld; Cell, Player: string);

    [ThenAttribute('el turno pasa a (\w+)')]
    procedure TurnPassesTo(Ctx: TGameWorld; Player: string);

    [ThenAttribute('el turno vuelve a (\w+)')]
    procedure TurnReturnsTo(Ctx: TGameWorld; Player: string);

    [ThenAttribute('el turno sigue siendo de (\w+)')]
    procedure TurnStays(Ctx: TGameWorld; Player: string);

    [ThenAttribute('se produce un error.*')]
    procedure ErrorRaised(Ctx: TGameWorld);

    [ThenAttribute('el juego está en fase de movimiento')]
    procedure PhaseIsMovement(Ctx: TGameWorld);

    [ThenAttribute('(\w+) tiene (\d+) fichas')]
    procedure PieceCount(Ctx: TGameWorld; Player: string; Count: Integer);
  end;

{ TPlacementSteps }

procedure TPlacementSteps.EmptyBoard(Ctx: TGameWorld);
begin
  Ctx.ViewModel.NewGame;
end;

procedure TPlacementSteps.BoardWithPiece(Ctx: TGameWorld; Player, Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Ctx.ViewModel.NewGame;
  Ctx.ViewModel.PlacePiece(P.Row, P.Col);
end;

procedure TPlacementSteps.BoardFromTable(Ctx: TGameWorld);
begin
  SetupBoardFromTable(Ctx);
end;

procedure TPlacementSteps.PlayerPlacesAt(Ctx: TGameWorld; Player, Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Ctx.ViewModel.PlacePiece(P.Row, P.Col);
end;

procedure TPlacementSteps.TwoPlayersPlace(Ctx: TGameWorld; P1, C1, P2, C2: string);
var
  Pos1, Pos2: TPosition;
begin
  Pos1 := ParseCell(C1);
  Pos2 := ParseCell(C2);
  Ctx.ViewModel.PlacePiece(Pos1.Row, Pos1.Col);
  Ctx.ViewModel.PlacePiece(Pos2.Row, Pos2.Col);
end;

procedure TPlacementSteps.PlaceAt(Ctx: TGameWorld; Player, Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Ctx.ViewModel.PlacePiece(P.Row, P.Col);
end;

procedure TPlacementSteps.TryPlaceAt(Ctx: TGameWorld; Player, Cell: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Ctx.ViewModel.PlacePiece(P.Row, P.Col);
end;

procedure TPlacementSteps.CellBelongsTo(Ctx: TGameWorld; Cell, Player: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Expect(Ctx.ViewModel.CellOwner(P.Row, P.Col)).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.CellIs(Ctx: TGameWorld; Cell, Player: string);
var
  P: TPosition;
begin
  P := ParseCell(Cell);
  Expect(Ctx.ViewModel.CellOwner(P.Row, P.Col)).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.TurnPassesTo(Ctx: TGameWorld; Player: string);
begin
  Expect(Ctx.ViewModel.CurrentPlayer).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.TurnReturnsTo(Ctx: TGameWorld; Player: string);
begin
  Expect(Ctx.ViewModel.CurrentPlayer).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.TurnStays(Ctx: TGameWorld; Player: string);
begin
  Expect(Ctx.ViewModel.CurrentPlayer).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.ErrorRaised(Ctx: TGameWorld);
begin
  Expect(Raised).ToBe(EInvalidMove);
end;

procedure TPlacementSteps.PhaseIsMovement(Ctx: TGameWorld);
begin
  Expect(Ctx.ViewModel.Phase).ToEqual(TGamePhase.Movement);
end;

procedure TPlacementSteps.PieceCount(Ctx: TGameWorld; Player: string; Count: Integer);
begin
  if SameText(Player, 'X') then
    Expect(Ctx.ViewModel.XPieceCount).ToEqual(Count)
  else
    Expect(Ctx.ViewModel.OPieceCount).ToEqual(Count);
end;

initialization
  Bindings.RegisterSteps<TPlacementSteps>;

end.
