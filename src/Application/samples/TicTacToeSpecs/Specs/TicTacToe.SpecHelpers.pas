unit TicTacToe.SpecHelpers;

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.DataTable,
  TicTacToe.Game,
  TicTacToe.ViewModel;

const
  X = 'X';
  O = 'O';
  _ = '.';

type
  /// <summary>
  /// World context for TicTacToe specs.
  /// Contains the game instance and test state.
  /// </summary>
  TGameWorld = class
  public
    ViewModel: TGameViewModel;

    constructor Create;
    destructor Destroy; override;
  end;

/// <summary>
/// Helper to convert position to TValue for Examples tables
/// </summary>
function Pos(ARow, ACol: Integer): TValue;

/// <summary>
/// Parses cell notation (a1..c3) to TPosition.
/// Rows: a=0, b=1, c=2. Columns: 1=0, 2=1, 3=2.
/// </summary>
function ParseCell(const Cell: string): TPosition;

/// <summary>
/// Converts row,col to cell notation (a1..c3).
/// </summary>
function CellToStr(Row, Col: Integer): string;

/// <summary>
/// Sets up the board from SpecContext.DataTable.
/// Table format: visual 3x3 grid (row, col implicit).
///   [['X', 'X', '.'],
///    ['O', 'O', '.'],
///    ['X', 'O', '.']]
/// Resets the game and places pieces alternating X/O.
/// </summary>
procedure SetupBoardFromTable(Ctx: TGameWorld);

/// <summary>
/// Helper to parse player from string
/// </summary>
function ParsePlayer(const S: string): TPlayer;

/// <summary>
/// Helper to parse phase from string
/// </summary>
function ParsePhase(const S: string): TGamePhase;

/// <summary>
/// Helper to parse status from string
/// </summary>
function ParseStatus(const S: string): TGameStatus;

/// <summary>
/// Helper to get player name
/// </summary>
function PlayerName(P: TPlayer): string;

/// <summary>
/// Helper to get phase name
/// </summary>
function PhaseName(Ph: TGamePhase): string;

/// <summary>
/// Helper to get status name
/// </summary>
function StatusName(St: TGameStatus): string;

implementation

{ TGameWorld }

constructor TGameWorld.Create;
begin
  inherited Create;
  ViewModel := TGameViewModel.Create;
end;

destructor TGameWorld.Destroy;
begin
  ViewModel.Free;
  inherited;
end;

{ Helper functions }

function Pos(ARow, ACol: Integer): TValue;
begin
  Result := TValue.From(TPosition.Create(ARow, ACol));
end;

function ParseCell(const Cell: string): TPosition;
var
  S: string;
begin
  S := LowerCase(Trim(Cell));
  if Length(S) <> 2 then
    raise Exception.CreateFmt('Invalid cell notation: %s', [Cell]);
  case S[1] of
    'a': Result.Row := 0;
    'b': Result.Row := 1;
    'c': Result.Row := 2;
  else
    raise Exception.CreateFmt('Invalid row in cell: %s', [Cell]);
  end;
  case S[2] of
    '1': Result.Col := 0;
    '2': Result.Col := 1;
    '3': Result.Col := 2;
  else
    raise Exception.CreateFmt('Invalid column in cell: %s', [Cell]);
  end;
end;

function CellToStr(Row, Col: Integer): string;
begin
  Result := Chr(Ord('a') + Row) + IntToStr(Col + 1);
end;

procedure SetupBoardFromTable(Ctx: TGameWorld);
var
  Table: TDataTableObj;
  Row: TArray<TValue>;
  R, C, I, XI, OI: Integer;
  XPos, OPos: array[0..2] of TPosition;
  Cell: string;
begin
  Ctx.ViewModel.NewGame;
  Table := SpecContext.DataTable;
  XI := 0;
  OI := 0;
  R := 0;
  for Row in Table.Raw do
  begin
    // Skip header row (column labels: '', '1', '2', '3')
    if R = 0 then
    begin
      Inc(R);
      Continue;
    end;
    // Data rows: first column is row label ('a','b','c'), skip it
    for C := 1 to 3 do
    begin
      Cell := Trim(Row[C].AsString);
      if SameText(Cell, 'X') then
      begin
        XPos[XI] := TPosition.Create(R - 1, C - 1);
        Inc(XI);
      end
      else if SameText(Cell, 'O') then
      begin
        OPos[OI] := TPosition.Create(R - 1, C - 1);
        Inc(OI);
      end;
    end;
    Inc(R);
  end;
  I := 0;
  while (I < XI) or (I < OI) do
  begin
    if I < XI then
      Ctx.ViewModel.PlacePiece(XPos[I].Row, XPos[I].Col);
    if I < OI then
      Ctx.ViewModel.PlacePiece(OPos[I].Row, OPos[I].Col);
    Inc(I);
  end;
end;

function ParsePlayer(const S: string): TPlayer;
begin
  if SameText(S, 'X') or SameText(S, 'PlayerX') then
    Result := TPlayer.PlayerX
  else if SameText(S, 'O') or SameText(S, 'PlayerO') then
    Result := TPlayer.PlayerO
  else if SameText(S, 'None') or (S = '') then
    Result := TPlayer.None
  else
    raise Exception.CreateFmt('Unknown player: %s', [S]);
end;

function ParsePhase(const S: string): TGamePhase;
begin
  if SameText(S, 'Placement') or SameText(S, 'Colocacion') then
    Result := TGamePhase.Placement
  else if SameText(S, 'Movement') or SameText(S, 'Movimiento') then
    Result := TGamePhase.Movement
  else
    raise Exception.CreateFmt('Unknown phase: %s', [S]);
end;

function ParseStatus(const S: string): TGameStatus;
begin
  if SameText(S, 'InProgress') or SameText(S, 'EnCurso') then
    Result := TGameStatus.InProgress
  else if SameText(S, 'XWins') or SameText(S, 'GanaX') then
    Result := TGameStatus.XWins
  else if SameText(S, 'OWins') or SameText(S, 'GanaO') then
    Result := TGameStatus.OWins
  else
    raise Exception.CreateFmt('Unknown status: %s', [S]);
end;

function PlayerName(P: TPlayer): string;
begin
  case P of
    TPlayer.None: Result := 'None';
    TPlayer.PlayerX: Result := 'X';
    TPlayer.PlayerO: Result := 'O';
  end;
end;

function PhaseName(Ph: TGamePhase): string;
begin
  case Ph of
    TGamePhase.Placement: Result := 'Placement';
    TGamePhase.Movement: Result := 'Movement';
  end;
end;

function StatusName(St: TGameStatus): string;
begin
  case St of
    TGameStatus.InProgress: Result := 'InProgress';
    TGameStatus.XWins: Result := 'XWins';
    TGameStatus.OWins: Result := 'OWins';
  end;
end;

end.
