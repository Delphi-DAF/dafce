unit TicTacToe.Movement.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.DataTable,
  TicTacToe.Game,
  TicTacToe.SpecHelpers;

initialization

  Feature('''
  TicTacToe: Movimiento de fichas

    Como jugador
    Quiero mover mis fichas a casillas adyacentes
    Para intentar formar una línea de 3

    Reglas:
    - Solo se puede mover en fase de movimiento (después de 6 colocaciones)
    - Solo se pueden mover fichas propias
    - Solo se puede mover a casillas adyacentes vacías
    - Adyacente incluye las 8 direcciones (horizontal, vertical, diagonal)
  ''')

  .UseWorld<TGameWorld>

  // ===== Movimientos válidos =====

  .Scenario('Mover ficha propia a casilla adyacente vacía')
    .Given('el siguiente tablero:',
      [['X', 'X', '.'],
       ['O', 'O', '.'],
       ['X', 'O', '.']],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .When('X mueve de (0,1) a (0,2)', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.MovePiece(TPosition.Create(0, 1), TPosition.Create(0, 2));
      end)
    .&Then('(0,1) está vacía', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[0, 1]).ToEqual(TPlayer.None);
      end)
    .&Then('(0,2) pertenece a X', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[0, 2]).ToEqual(TPlayer.PlayerX);
      end)
    .&Then('el turno pasa a O', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.CurrentPlayer).ToEqual(TPlayer.PlayerO);
      end)

  .Scenario('Mover ficha en diagonal')
    .Given('el siguiente tablero:',
      [['X', 'X', '.'],
       ['O', 'O', '.'],
       ['X', 'O', '.']],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .When('X mueve de (2,0) a (1,1) - pero está ocupada', procedure(Ctx: TGameWorld)
      begin
        // (1,1) está ocupada por O, así que movemos a (2,1) que sí está... no, también O
        // Vamos a (0,1) -> (1,2) diagonal
        Ctx.Game.MovePiece(TPosition.Create(0, 1), TPosition.Create(1, 2));
      end)
    .&Then('(0,1) está vacía', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[0, 1]).ToEqual(TPlayer.None);
      end)
    .&Then('(1,2) pertenece a X', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[1, 2]).ToEqual(TPlayer.PlayerX);
      end)

  // ===== Movimientos inválidos =====

  .Scenario('Rechazar mover ficha del oponente')
    .Given('el siguiente tablero en turno de X:',
      [['X', 'X', '.'],
       ['O', 'O', '.'],
       ['X', 'O', '.']],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .When('X intenta mover una ficha de O', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.MovePiece(TPosition.Create(1, 0), TPosition.Create(0, 2));
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
      end)
    .&Then('la ficha de O sigue en su lugar', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[1, 0]).ToEqual(TPlayer.PlayerO);
      end)

  .Scenario('Rechazar mover a casilla ocupada')
    .Given('el siguiente tablero:',
      [['X', 'X', '.'],
       ['O', 'O', '.'],
       ['X', 'O', '.']],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .When('X intenta mover a una casilla ocupada por O', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.MovePiece(TPosition.Create(0, 0), TPosition.Create(1, 0));
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
      end)

  .Scenario('Rechazar mover a casilla no adyacente')
    .Given('el siguiente tablero:',
      [['X', 'X', '.'],
       ['O', 'O', '.'],
       ['X', 'O', '.']],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .When('X intenta mover de (0,0) a (2,2)', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.MovePiece(TPosition.Create(0, 0), TPosition.Create(2, 2));
        except
          on E: EInvalidMove do
          begin
            Ctx.ExceptionRaised := True;
            Ctx.LastException := EInvalidMove.Create(E.Message);
          end;
        end;
      end)
    .&Then('se produce un error de no adyacencia', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.ExceptionRaised).ToEqual(True);
      end)
    .&Then('la ficha sigue en su posición original', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Board[0, 0]).ToEqual(TPlayer.PlayerX);
      end)

  .Scenario('Rechazar mover durante fase de colocación')
    .Given('el juego está en fase de colocación', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.Free;
        Ctx.Game := TTicTacToeGame.Create;
        Ctx.Game.PlacePiece(TPosition.Create(0, 0)); // X coloca una ficha
      end)
    .When('O intenta mover una ficha', procedure(Ctx: TGameWorld)
      begin
        Ctx.ClearException;
        try
          Ctx.Game.MovePiece(TPosition.Create(0, 0), TPosition.Create(0, 1));
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
