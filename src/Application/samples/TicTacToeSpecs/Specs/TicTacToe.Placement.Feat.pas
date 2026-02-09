unit TicTacToe.Placement.Feat;

interface

type
  /// <summary>
  /// Marker type for unit identification.
  /// Convention: Use .Category(TUnitMarker) to enable Cat: filter.
  /// </summary>
  TUnitMarker = class end;

implementation

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.DataTable,
  TicTacToe.SpecHelpers;

initialization

  Feature('''
  Colocación de fichas @e2e

    Como jugador
    Quiero colocar mis fichas en el tablero
    Para intentar formar una línea de 3
  ''')
  .Category(TUnitMarker)
  .UseWorld<TGameWorld>

  .Rule('Se pueden colocar fichas en casillas vacías')

    .Background
      .Given('un tablero vacío')

    .Scenario('Colocar ficha en casilla vacía')
      .When('el jugador X coloca una ficha en (0,0)')
      .&Then('la casilla (0,0) pertenece a X')
      .&And('el turno pasa a O')

    .Scenario('Alternancia de turnos')
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

    .Background
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [O, _, X]])

    .Scenario('Transición a fase de movimiento')
      .&Then('el juego está en fase de movimiento')
      .&And('X tiene 3 fichas')
      .&And('O tiene 3 fichas')

    .Scenario('Rechazar colocación cuando se alcanza el máximo de fichas')
      .When('X intenta colocar en (0,2)')
      .&Then('se produce un error');

end.
