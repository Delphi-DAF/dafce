unit TicTacToe.Game;

interface

uses
  System.SysUtils;

type
  TPlayer = (None, PlayerX, PlayerO);
  TGamePhase = (Placement, Movement);
  TGameStatus = (InProgress, XWins, OWins);

  TPosition = record
    Row: Integer;
    Col: Integer;
    class function Create(ARow, ACol: Integer): TPosition; static;
    function IsValid: Boolean;
    function IsAdjacentTo(const Other: TPosition): Boolean;
    class operator Equal(const A, B: TPosition): Boolean;
    class operator NotEqual(const A, B: TPosition): Boolean;
  end;

  TBoard = array[0..2, 0..2] of TPlayer;

  EInvalidMove = class(Exception);

  TTicTacToeGame = class
  private
    FBoard: TBoard;
    FCurrentPlayer: TPlayer;
    FPhase: TGamePhase;
    FXPieceCount: Integer;
    FOPieceCount: Integer;
    function GetCell(Row, Col: Integer): TPlayer;
    function GetStatus: TGameStatus;
    function CheckVictory: TPlayer;
    function CountPieces(Player: TPlayer): Integer;
    procedure UpdatePhase;
  public
    constructor Create;

    // Placement phase
    procedure PlacePiece(const Pos: TPosition);

    // Movement phase
    procedure MovePiece(const FromPos, ToPos: TPosition);

    // Query methods
    function IsEmpty(const Pos: TPosition): Boolean;
    function GetOwner(const Pos: TPosition): TPlayer;
    function CanPlace: Boolean;
    function CanMove(Player: TPlayer): Boolean;

    property Board[Row, Col: Integer]: TPlayer read GetCell;
    property CurrentPlayer: TPlayer read FCurrentPlayer;
    property Phase: TGamePhase read FPhase;
    property Status: TGameStatus read GetStatus;
    property XPieceCount: Integer read FXPieceCount;
    property OPieceCount: Integer read FOPieceCount;
  end;

const
  MaxPiecesPerPlayer = 3;

implementation

uses
  System.Math;

{ TPosition }

class function TPosition.Create(ARow, ACol: Integer): TPosition;
begin
  Result.Row := ARow;
  Result.Col := ACol;
end;

function TPosition.IsValid: Boolean;
begin
  Result := (Row >= 0) and (Row <= 2) and (Col >= 0) and (Col <= 2);
end;

function TPosition.IsAdjacentTo(const Other: TPosition): Boolean;
var
  DeltaRow, DeltaCol: Integer;
begin
  DeltaRow := Abs(Row - Other.Row);
  DeltaCol := Abs(Col - Other.Col);
  // Adjacent means at most 1 step in each direction, but not same position
  Result := (DeltaRow <= 1) and (DeltaCol <= 1) and not (Self = Other);
end;

class operator TPosition.Equal(const A, B: TPosition): Boolean;
begin
  Result := (A.Row = B.Row) and (A.Col = B.Col);
end;

class operator TPosition.NotEqual(const A, B: TPosition): Boolean;
begin
  Result := not (A = B);
end;

{ TTicTacToeGame }

constructor TTicTacToeGame.Create;
var
  R, C: Integer;
begin
  inherited Create;
  for R := 0 to 2 do
    for C := 0 to 2 do
      FBoard[R, C] := TPlayer.None;
  FCurrentPlayer := TPlayer.PlayerX;
  FPhase := TGamePhase.Placement;
  FXPieceCount := 0;
  FOPieceCount := 0;
end;

function TTicTacToeGame.GetCell(Row, Col: Integer): TPlayer;
begin
  Result := FBoard[Row, Col];
end;

function TTicTacToeGame.IsEmpty(const Pos: TPosition): Boolean;
begin
  Result := FBoard[Pos.Row, Pos.Col] = TPlayer.None;
end;

function TTicTacToeGame.GetOwner(const Pos: TPosition): TPlayer;
begin
  Result := FBoard[Pos.Row, Pos.Col];
end;

function TTicTacToeGame.CountPieces(Player: TPlayer): Integer;
var
  R, C: Integer;
begin
  Result := 0;
  for R := 0 to 2 do
    for C := 0 to 2 do
      if FBoard[R, C] = Player then
        Inc(Result);
end;

function TTicTacToeGame.CanPlace: Boolean;
begin
  Result := (FPhase = TGamePhase.Placement);
end;

function TTicTacToeGame.CanMove(Player: TPlayer): Boolean;
begin
  Result := (FPhase = TGamePhase.Movement) and (CountPieces(Player) = MaxPiecesPerPlayer);
end;

procedure TTicTacToeGame.UpdatePhase;
begin
  if (FXPieceCount = MaxPiecesPerPlayer) and (FOPieceCount = MaxPiecesPerPlayer) then
    FPhase := TGamePhase.Movement;
end;

procedure TTicTacToeGame.PlacePiece(const Pos: TPosition);
begin
  if not Pos.IsValid then
    raise EInvalidMove.Create('Invalid position');

  if GetStatus <> TGameStatus.InProgress then
    raise EInvalidMove.Create('Game is over');

  if not CanPlace then
    raise EInvalidMove.Create('Cannot place pieces in movement phase');

  // Check if current player already has max pieces
  if FCurrentPlayer = TPlayer.PlayerX then
  begin
    if FXPieceCount >= MaxPiecesPerPlayer then
      raise EInvalidMove.Create('Maximum pieces reached, must move instead');
  end
  else
  begin
    if FOPieceCount >= MaxPiecesPerPlayer then
      raise EInvalidMove.Create('Maximum pieces reached, must move instead');
  end;

  if not IsEmpty(Pos) then
    raise EInvalidMove.Create('Cell is already occupied');

  FBoard[Pos.Row, Pos.Col] := FCurrentPlayer;

  if FCurrentPlayer = TPlayer.PlayerX then
    Inc(FXPieceCount)
  else
    Inc(FOPieceCount);

  UpdatePhase;

  // Switch player
  if FCurrentPlayer = TPlayer.PlayerX then
    FCurrentPlayer := TPlayer.PlayerO
  else
    FCurrentPlayer := TPlayer.PlayerX;
end;

procedure TTicTacToeGame.MovePiece(const FromPos, ToPos: TPosition);
begin
  if not FromPos.IsValid or not ToPos.IsValid then
    raise EInvalidMove.Create('Invalid position');

  if GetStatus <> TGameStatus.InProgress then
    raise EInvalidMove.Create('Game is over');

  if FPhase <> TGamePhase.Movement then
    raise EInvalidMove.Create('Cannot move pieces in placement phase');

  if GetOwner(FromPos) <> FCurrentPlayer then
    raise EInvalidMove.Create('Cannot move opponent''s piece');

  if not IsEmpty(ToPos) then
    raise EInvalidMove.Create('Target cell is occupied');

  if not FromPos.IsAdjacentTo(ToPos) then
    raise EInvalidMove.Create('Can only move to adjacent cells');

  // Perform the move
  FBoard[ToPos.Row, ToPos.Col] := FCurrentPlayer;
  FBoard[FromPos.Row, FromPos.Col] := TPlayer.None;

  // Switch player
  if FCurrentPlayer = TPlayer.PlayerX then
    FCurrentPlayer := TPlayer.PlayerO
  else
    FCurrentPlayer := TPlayer.PlayerX;
end;

function TTicTacToeGame.CheckVictory: TPlayer;
var
  R, C: Integer;
  Player: TPlayer;
begin
  // Check rows
  for R := 0 to 2 do
  begin
    Player := FBoard[R, 0];
    if (Player <> TPlayer.None) and (FBoard[R, 1] = Player) and (FBoard[R, 2] = Player) then
      Exit(Player);
  end;

  // Check columns
  for C := 0 to 2 do
  begin
    Player := FBoard[0, C];
    if (Player <> TPlayer.None) and (FBoard[1, C] = Player) and (FBoard[2, C] = Player) then
      Exit(Player);
  end;

  // Check diagonals
  Player := FBoard[0, 0];
  if (Player <> TPlayer.None) and (FBoard[1, 1] = Player) and (FBoard[2, 2] = Player) then
    Exit(Player);

  Player := FBoard[0, 2];
  if (Player <> TPlayer.None) and (FBoard[1, 1] = Player) and (FBoard[2, 0] = Player) then
    Exit(Player);

  Result := TPlayer.None;
end;

function TTicTacToeGame.GetStatus: TGameStatus;
var
  Winner: TPlayer;
begin
  Winner := CheckVictory;
  case Winner of
    TPlayer.PlayerX: Result := TGameStatus.XWins;
    TPlayer.PlayerO: Result := TGameStatus.OWins;
  else
    Result := TGameStatus.InProgress;
  end;
end;

end.
