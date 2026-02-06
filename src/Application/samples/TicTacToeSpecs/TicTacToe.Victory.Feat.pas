unit TicTacToe.Victory.Feat;

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
    .Given('el siguiente tablero:',
      [[X, X, _],
       [O, O, _],
       [_, _, _]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
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
    .Given('el siguiente tablero:',
      [[X, O, _],
       [X, O, _],
       [_, _, _]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
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
    .Given('el siguiente tablero:',
      [[X, O, O],
       [_, X, _],
       [_, _, _]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
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
    .Given('el siguiente tablero:',
      [[O, O, X],
       [_, X, _],
       [_, _, _]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
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
    .Given('el siguiente tablero:',
      [[X, X, _],
       [O, O, _],
       [_, _, X]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
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
    .Given('el siguiente tablero:',
      [[X, _, _],
       [_, O, _],
       [_, _, _]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .&Then('el estado es En Progreso', procedure(Ctx: TGameWorld)
      begin
        Expect(Ctx.Game.Status).ToEqual(TGameStatus.InProgress);
      end)

  // ===== No se puede jugar después de victoria =====

  .Scenario('Rechazar movimientos después de victoria')
    .Given('X ha ganado:',
      [[X, X, X],
       [O, O, _],
       [_, _, _]],
      procedure(Ctx: TGameWorld)
      begin
        SetupBoardFromTable(Ctx);
      end)
    .When('O intenta colocar una ficha', procedure(Ctx: TGameWorld)
      begin
        Ctx.Game.PlacePiece(TPosition.Create(2, 0));
      end)
    .&Then('se produce un error', procedure(Ctx: TGameWorld)
      begin
        Expect(Raised).ToBe(EInvalidMove);
      end);

end.
