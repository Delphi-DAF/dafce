unit TicTacToe.Placement.Feat;

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
  TicTacToe: Colocación de fichas

    Como jugador
    Quiero colocar mis fichas en el tablero
    Para intentar formar una línea de 3
  ''')

  .UseWorld<TGameWorld>

  .Rule('Se pueden colocar fichas en casillas vacías')

    .Scenario('Colocar ficha en casilla vacía')
      .Given('un tablero vacío')
      .When('el jugador X coloca una ficha en (0,0)')
      .&Then('la casilla (0,0) pertenece a X')
      .&And('el turno pasa a O')

    .Scenario('Alternancia de turnos')
      .Given('un tablero vacío')
      .When('X coloca en (0,0) y O coloca en (1,1)')
      .&Then('(0,0) pertenece a X')
      .&And('(1,1) pertenece a O')
      .&And('el turno vuelve a X')

  .Rule('No se puede colocar en casilla ocupada')

    .Scenario('Rechazar colocación en casilla ocupada')
      .Given('un tablero con X en (0,0)')
      .When('O intenta colocar en (0,0)')
      .&Then('se produce un error de movimiento inválido')
      .&And('el turno sigue siendo de O')

  .Rule('Cada jugador coloca hasta 3 fichas')

    .Scenario('Transición a fase de movimiento')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [O, _, X]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .&Then('el juego está en fase de movimiento')
      .&And('X tiene 3 fichas')
      .&And('O tiene 3 fichas')

    .Scenario('Rechazar colocación cuando se alcanza el máximo de fichas')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [O, _, X]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X intenta colocar en (0,2)')
      .&Then('se produce un error');

end.
