unit TicTacToe.ViewModel;

interface

uses
  System.SysUtils,
  TicTacToe.Game,
  TicTacToe.ViewPort;

type
  /// <summary>
  /// Presentation logic for TicTacToe.
  /// Wraps the domain Game and exposes UI-friendly state + commands.
  /// Notifies the ViewPort on every state change.
  /// Exceptions from the domain propagate to the caller (framework/UI catches).
  /// </summary>
  TGameViewModel = class
  private
    FGame: TTicTacToeGame;
    FViewPort: IGameViewPort;
    FSelectedRow: Integer;
    FSelectedCol: Integer;
    FHasSelection: Boolean;
    procedure Notify;
  public
    constructor Create;
    destructor Destroy; override;

    // === Commands ===
    procedure NewGame;
    procedure PlacePiece(Row, Col: Integer);
    procedure MovePiece(FromRow, FromCol, ToRow, ToCol: Integer);
    /// <summary>
    /// Smart command for UI: in Placement phase places a piece;
    /// in Movement phase implements select-then-move (two clicks).
    /// </summary>
    procedure CellClick(Row, Col: Integer);

    // === Domain queries (delegated) ===
    function CellOwner(Row, Col: Integer): TPlayer;
    function IsEmpty(Row, Col: Integer): Boolean;
    function CurrentPlayer: TPlayer;
    function Phase: TGamePhase;
    function Status: TGameStatus;
    function XPieceCount: Integer;
    function OPieceCount: Integer;

    // === Presentation queries ===
    function CellText(Row, Col: Integer): string;
    function StatusText: string;
    function TurnText: string;
    function PhaseText: string;
    function IsGameOver: Boolean;
    function IsSelected(Row, Col: Integer): Boolean;

    // === ViewPort binding ===
    procedure SetViewPort(const Value: IGameViewPort);
    property ViewPort: IGameViewPort read FViewPort write SetViewPort;

    // === Direct game access (for spec setup / Strangler Fig migration) ===
    property Game: TTicTacToeGame read FGame;
  end;

implementation

{ TGameViewModel }

constructor TGameViewModel.Create;
begin
  inherited Create;
  FGame := TTicTacToeGame.Create;
  FHasSelection := False;
  FSelectedRow := -1;
  FSelectedCol := -1;
end;

destructor TGameViewModel.Destroy;
begin
  FGame.Free;
  inherited;
end;

procedure TGameViewModel.Notify;
begin
  if Assigned(FViewPort) then
    FViewPort.Refresh;
end;

procedure TGameViewModel.SetViewPort(const Value: IGameViewPort);
begin
  FViewPort := Value;
end;

// === Commands ===

procedure TGameViewModel.NewGame;
begin
  FGame.Free;
  FGame := TTicTacToeGame.Create;
  FHasSelection := False;
  FSelectedRow := -1;
  FSelectedCol := -1;
  Notify;
end;

procedure TGameViewModel.PlacePiece(Row, Col: Integer);
begin
  FGame.PlacePiece(TPosition.Create(Row, Col));
  Notify;
end;

procedure TGameViewModel.MovePiece(FromRow, FromCol, ToRow, ToCol: Integer);
begin
  FGame.MovePiece(
    TPosition.Create(FromRow, FromCol),
    TPosition.Create(ToRow, ToCol));
  FHasSelection := False;
  Notify;
end;

procedure TGameViewModel.CellClick(Row, Col: Integer);
begin
  if IsGameOver then Exit;

  case Phase of
    TGamePhase.Placement:
      PlacePiece(Row, Col);

    TGamePhase.Movement:
    begin
      if FHasSelection then
      begin
        // Second click: attempt move
        try
          MovePiece(FSelectedRow, FSelectedCol, Row, Col);
        except
          // On error, clear selection and re-raise
          FHasSelection := False;
          Notify;
          raise;
        end;
      end
      else
      begin
        // First click: select own piece
        if CellOwner(Row, Col) = CurrentPlayer then
        begin
          FSelectedRow := Row;
          FSelectedCol := Col;
          FHasSelection := True;
          Notify;
        end;
      end;
    end;
  end;
end;

// === Domain queries ===

function TGameViewModel.CellOwner(Row, Col: Integer): TPlayer;
begin
  Result := FGame.Board[Row, Col];
end;

function TGameViewModel.IsEmpty(Row, Col: Integer): Boolean;
begin
  Result := FGame.IsEmpty(TPosition.Create(Row, Col));
end;

function TGameViewModel.CurrentPlayer: TPlayer;
begin
  Result := FGame.CurrentPlayer;
end;

function TGameViewModel.Phase: TGamePhase;
begin
  Result := FGame.Phase;
end;

function TGameViewModel.Status: TGameStatus;
begin
  Result := FGame.Status;
end;

function TGameViewModel.XPieceCount: Integer;
begin
  Result := FGame.XPieceCount;
end;

function TGameViewModel.OPieceCount: Integer;
begin
  Result := FGame.OPieceCount;
end;

// === Presentation queries ===

function TGameViewModel.CellText(Row, Col: Integer): string;
begin
  case FGame.Board[Row, Col] of
    TPlayer.PlayerX: Result := 'X';
    TPlayer.PlayerO: Result := 'O';
  else
    Result := '';
  end;
end;

function TGameViewModel.TurnText: string;
begin
  case FGame.CurrentPlayer of
    TPlayer.PlayerX: Result := 'X';
    TPlayer.PlayerO: Result := 'O';
  else
    Result := '';
  end;
end;

function TGameViewModel.PhaseText: string;
begin
  case FGame.Phase of
    TGamePhase.Placement: Result := 'Colocación';
    TGamePhase.Movement:  Result := 'Movimiento';
  end;
end;

function TGameViewModel.StatusText: string;
begin
  case FGame.Status of
    TGameStatus.InProgress:
      Result := Format('Turno de %s · %s', [TurnText, PhaseText]);
    TGameStatus.XWins:
      Result := '¡X gana!';
    TGameStatus.OWins:
      Result := '¡O gana!';
  end;
end;

function TGameViewModel.IsGameOver: Boolean;
begin
  Result := FGame.Status <> TGameStatus.InProgress;
end;

function TGameViewModel.IsSelected(Row, Col: Integer): Boolean;
begin
  Result := FHasSelection and (FSelectedRow = Row) and (FSelectedCol = Col);
end;

end.
