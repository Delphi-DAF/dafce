unit TicTacToe.Placement.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  TicTacToe.Game,
  TicTacToe.SpecHelpers;

initialization

  Feature('''
  TicTacToe: Colocación de fichas

    Como jugador
    Quiero colocar mis fichas en el tablero
    Para intentar formar una línea de 3

    Reglas:
    - Cada jugador puede colocar hasta 3 fichas
    - Se alternan los turnos comenzando por X
    - No se puede colocar en una casilla ocupada
  ''')

  .UseWorld<TGameWorld>

  // ===== Escenarios de colocación válida =====

  .Scenario('Colocar ficha en casilla vacía')
    .Given('un tablero vacío', procedure(Ctx: TGameWorld)
      begin
        // El constructor ya crea un tablero vacío
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
      end)
    .When('el jugador X coloca una ficha en (0,0)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(0, 0));
      end)
    .&Then('la casilla (0,0) pertenece a X', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[0, 0]).ToEqual(TPlayer.PlayerX);
      end)
    .&Then('el turno pasa a O', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.CurrentPlayer).ToEqual(TPlayer.PlayerO);
      end)

  .Scenario('Alternancia de turnos')
    .Given('un tablero vacío', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
      end)
    .When('X coloca en (0,0) y O coloca en (1,1)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(0, 0));
        Ctx.Game.PlacePiece(TPosition.Create(1, 1));
      end)
    .&Then('(0,0) pertenece a X', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[0, 0]).ToEqual(TPlayer.PlayerX);
      end)
    .&Then('(1,1) pertenece a O', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[1, 1]).ToEqual(TPlayer.PlayerO);
      end)
    .&Then('el turno vuelve a X', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.CurrentPlayer).ToEqual(TPlayer.PlayerX);
      end)

  // ===== Escenarios de colocación inválida =====

  .Scenario('Rechazar colocación en casilla ocupada')
    .Given('un tablero con X en (0,0)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0));
      end)
    .When('O intenta colocar en (0,0)', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.PlacePiece(TPosition.Create(0, 0));
        except
          on E: EInvalidMove do
          begin
            Ctx.ExceptionRaised := True;
            Ctx.LastException := EInvalidMove.Create(E.Message);
          end;
        end;
      end)
    .&Then('se produce un error de movimiento inválido', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.ExceptionRaised).ToEqual(True);
      end)
    .&Then('el turno sigue siendo de O', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.CurrentPlayer).ToEqual(TPlayer.PlayerO);
      end)

  // ===== Transición de fase =====

  .Scenario('Transición a fase de movimiento')
    .Given('X y O han colocado 3 fichas cada uno', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        // X: (0,0), (0,1), (2,2)
        // O: (1,0), (1,1), (2,0)
        // Colocamos en orden alterno sin formar línea
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 0)); // O
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
        Ctx.Game.PlacePiece(TPosition.Create(2, 2)); // X
        Ctx.Game.PlacePiece(TPosition.Create(2, 0)); // O
      end)
    .&Then('el juego está en fase de movimiento', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Phase).ToEqual(TGamePhase.Movement);
      end)
    .&Then('X tiene 3 fichas', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.XPieceCount).ToEqual(3);
      end)
    .&Then('O tiene 3 fichas', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.OPieceCount).ToEqual(3);
      end)

  .Scenario('Rechazar colocación cuando se alcanza el máximo de fichas')
    .Given('X y O han colocado 3 fichas cada uno', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 0)); // O
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
        Ctx.Game.PlacePiece(TPosition.Create(2, 2)); // X
        Ctx.Game.PlacePiece(TPosition.Create(2, 0)); // O
      end)
    .When('X intenta colocar una cuarta ficha', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.PlacePiece(TPosition.Create(0, 2));
        except
          on E: EInvalidMove do
          begin
            Ctx.ExceptionRaised := True;
            Ctx.LastException := EInvalidMove.Create(E.Message);
          end;
        end;
      end)
    .&Then('se produce un error', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.ExceptionRaised).ToEqual(True);
      end);

end.
