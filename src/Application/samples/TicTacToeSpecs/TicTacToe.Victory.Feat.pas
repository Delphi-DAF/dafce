unit TicTacToe.Victory.Feat;

interface

implementation

uses
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
  ''')

  .UseWorld<TGameWorld>

  .Rule('Gana quien forme una línea de 3 fichas propias')

    .Scenario('Victoria horizontal en fila superior')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [_, _, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X coloca en (0,2)')
      .&Then('X gana')

    .Scenario('Victoria vertical en columna izquierda')
      .Given('el siguiente tablero:',
        [[X, O, _],
         [X, O, _],
         [_, _, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X coloca en (2,0)')
      .&Then('X gana')

  .Rule('La línea puede ser diagonal')

    .Scenario('Victoria diagonal principal')
      .Given('el siguiente tablero:',
        [[X, O, O],
         [_, X, _],
         [_, _, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X coloca en (2,2)')
      .&Then('X gana')

    .Scenario('Victoria diagonal secundaria')
      .Given('el siguiente tablero:',
        [[O, O, X],
         [_, X, _],
         [_, _, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X coloca en (2,0)')
      .&Then('X gana')

  .Rule('Ambos jugadores pueden ganar')

    .Scenario('O puede ganar')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [_, _, X]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('O coloca en (1,2)')
      .&Then('O gana')

  .Rule('La partida continúa mientras no haya ganador')

    .Scenario('Juego sin ganador continúa en progreso')
      .Given('el siguiente tablero:',
        [[X, _, _],
         [_, O, _],
         [_, _, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .&Then('el estado es En Progreso')

  .Rule('No se puede jugar después de una victoria')

    .Scenario('Rechazar movimientos después de victoria')
      .Given('el siguiente tablero:',
        [[X, X, X],
         [O, O, _],
         [_, _, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('O intenta colocar en (2,0)')
      .&Then('se produce un error');

end.
