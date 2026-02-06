unit TicTacToe.Victory.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  TicTacToe.Game,
  TicTacToe.SpecHelpers;

initialization

  Feature('''
  TicTacToe: Condiciones de victoria

    Como jugador
    Quiero que el juego detecte cuando hay un ganador
    Para saber cuándo termina la partida

    Reglas:
    - Gana quien forme una línea de 3 fichas propias
    - La línea puede ser horizontal, vertical o diagonal
    - La victoria puede ocurrir en fase de colocación o movimiento
  ''')

  .UseWorld<TGameWorld>

  // ===== Victoria en fase de colocación =====

  .Scenario('Victoria horizontal en fila superior')
    .Given('X tiene fichas en (0,0) y (0,1)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 0)); // O
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
      end)
    .When('X coloca en (0,2)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(0, 2)); // X completa fila 0
      end)
    .&Then('X gana', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.XWins);
      end)

  .Scenario('Victoria vertical en columna izquierda')
    .Given('X tiene fichas en (0,0) y (1,0)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // O
        Ctx.Game.PlacePiece(TPosition.Create(1, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
      end)
    .When('X coloca en (2,0)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(2, 0)); // X completa columna 0
      end)
    .&Then('X gana', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.XWins);
      end)

  .Scenario('Victoria diagonal principal')
    .Given('X tiene fichas en (0,0) y (1,1)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // O
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(0, 2)); // O
      end)
    .When('X coloca en (2,2)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(2, 2)); // X completa diagonal
      end)
    .&Then('X gana', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.XWins);
      end)

  .Scenario('Victoria diagonal secundaria')
    .Given('X tiene fichas en (0,2) y (1,1)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 2)); // X
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // O
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // O
      end)
    .When('X coloca en (2,0)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(2, 0)); // X completa anti-diagonal
      end)
    .&Then('X gana', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.XWins);
      end)

  .Scenario('O puede ganar')
    .Given('O tiene fichas en (1,0) y (1,1)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 0)); // O
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
        Ctx.Game.PlacePiece(TPosition.Create(2, 2)); // X (no completa línea)
      end)
    .When('O coloca en (1,2)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(1, 2)); // O completa fila 1
      end)
    .&Then('O gana', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.OWins);
      end)

  // ===== Juego en progreso =====

  .Scenario('Juego sin ganador continúa en progreso')
    .Given('ningún jugador tiene 3 en línea', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
      end)
    .&Then('el estado es En Progreso', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.InProgress);
      end)

  // ===== No se puede jugar después de victoria =====

  .Scenario('Rechazar movimientos después de victoria')
    .Given('X ha ganado', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 0)); // O
        Ctx.Game.PlacePiece(TPosition.Create(0, 1)); // X
        Ctx.Game.PlacePiece(TPosition.Create(1, 1)); // O
        Ctx.Game.PlacePiece(TPosition.Create(0, 2)); // X gana
      end)
    .When('O intenta colocar una ficha', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.PlacePiece(TPosition.Create(2, 0));
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
