unit TicTacToe.Placement.Steps;

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
  /// Step bindings for placement scenarios and shared steps.
  /// Regex captures are auto-converted to method parameters.
  /// </summary>
  TPlacementSteps = class
  public
    // === Given ===
    [Given('un tablero vac[ií]o')]
    procedure EmptyBoard(Ctx: TGameWorld);

    [Given('un tablero con (\w+) en \((\d+),(\d+)\)')]
    procedure BoardWithPiece(Ctx: TGameWorld; Player: string; Row, Col: Integer);

    // === When ===
    [When('el jugador (\w+) coloca una ficha en \((\d+),(\d+)\)')]
    procedure PlayerPlacesAt(Ctx: TGameWorld; Player: string; Row, Col: Integer);

    [When('(\w+) coloca en \((\d+),(\d+)\) y (\w+) coloca en \((\d+),(\d+)\)')]
    procedure TwoPlayersPlace(Ctx: TGameWorld; P1: string; R1, C1: Integer; P2: string; R2, C2: Integer);

    [When('(\w+) coloca en \((\d+),(\d+)\)')]
    procedure PlaceAt(Ctx: TGameWorld; Player: string; Row, Col: Integer);

    [When('(\w+) intenta colocar en \((\d+),(\d+)\)')]
    procedure TryPlaceAt(Ctx: TGameWorld; Player: string; Row, Col: Integer);

    // === Then (shared across features) ===
    [ThenAttribute('la casilla \((\d+),(\d+)\) pertenece a (\w+)')]
    procedure CellBelongsTo(Ctx: TGameWorld; Row, Col: Integer; Player: string);

    [ThenAttribute('\((\d+),(\d+)\) pertenece a (\w+)')]
    procedure CellIs(Ctx: TGameWorld; Row, Col: Integer; Player: string);

    [ThenAttribute('el turno pasa a (\w+)')]
    procedure TurnPassesTo(Ctx: TGameWorld; Player: string);

    [ThenAttribute('el turno vuelve a (\w+)')]
    procedure TurnReturnsTo(Ctx: TGameWorld; Player: string);

    [ThenAttribute('el turno sigue siendo de (\w+)')]
    procedure TurnStays(Ctx: TGameWorld; Player: string);

    [ThenAttribute('se produce un error.*')]
    procedure ErrorRaised(Ctx: TGameWorld);

    [ThenAttribute('el juego est. en fase de movimiento')]
    procedure PhaseIsMovement(Ctx: TGameWorld);

    [ThenAttribute('(\w+) tiene (\d+) fichas')]
    procedure PieceCount(Ctx: TGameWorld; Player: string; Count: Integer);
  end;

{ TPlacementSteps }

procedure TPlacementSteps.EmptyBoard(Ctx: TGameWorld);
begin
  Ctx.Game.Free;
  Ctx.Game := TTicTacToeGame.Create;
end;

procedure TPlacementSteps.BoardWithPiece(Ctx: TGameWorld; Player: string; Row, Col: Integer);
begin
  Ctx.Game.Free;
  Ctx.Game := TTicTacToeGame.Create;
  Ctx.Game.PlacePiece(TPosition.Create(Row, Col));
end;

procedure TPlacementSteps.PlayerPlacesAt(Ctx: TGameWorld; Player: string; Row, Col: Integer);
begin
  Ctx.Game.PlacePiece(TPosition.Create(Row, Col));
end;

procedure TPlacementSteps.TwoPlayersPlace(Ctx: TGameWorld; P1: string; R1, C1: Integer; P2: string; R2, C2: Integer);
begin
  Ctx.Game.PlacePiece(TPosition.Create(R1, C1));
  Ctx.Game.PlacePiece(TPosition.Create(R2, C2));
end;

procedure TPlacementSteps.PlaceAt(Ctx: TGameWorld; Player: string; Row, Col: Integer);
begin
  Ctx.Game.PlacePiece(TPosition.Create(Row, Col));
end;

procedure TPlacementSteps.TryPlaceAt(Ctx: TGameWorld; Player: string; Row, Col: Integer);
begin
  Ctx.Game.PlacePiece(TPosition.Create(Row, Col));
end;

procedure TPlacementSteps.CellBelongsTo(Ctx: TGameWorld; Row, Col: Integer; Player: string);
begin
  Expect(Ctx.Game.Board[Row, Col]).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.CellIs(Ctx: TGameWorld; Row, Col: Integer; Player: string);
begin
  Expect(Ctx.Game.Board[Row, Col]).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.TurnPassesTo(Ctx: TGameWorld; Player: string);
begin
  Expect(Ctx.Game.CurrentPlayer).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.TurnReturnsTo(Ctx: TGameWorld; Player: string);
begin
  Expect(Ctx.Game.CurrentPlayer).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.TurnStays(Ctx: TGameWorld; Player: string);
begin
  Expect(Ctx.Game.CurrentPlayer).ToEqual(ParsePlayer(Player));
end;

procedure TPlacementSteps.ErrorRaised(Ctx: TGameWorld);
begin
  Expect(Raised).ToBe(EInvalidMove);
end;

procedure TPlacementSteps.PhaseIsMovement(Ctx: TGameWorld);
begin
  Expect(Ctx.Game.Phase).ToEqual(TGamePhase.Movement);
end;

procedure TPlacementSteps.PieceCount(Ctx: TGameWorld; Player: string; Count: Integer);
begin
  if SameText(Player, 'X') then
    Expect(Ctx.Game.XPieceCount).ToEqual(Count)
  else
    Expect(Ctx.Game.OPieceCount).ToEqual(Count);
end;

initialization
  Bindings.RegisterSteps<TPlacementSteps>;

end.
