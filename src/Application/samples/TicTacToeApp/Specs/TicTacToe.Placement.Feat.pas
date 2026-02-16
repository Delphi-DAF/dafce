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

  .Rule('Se coloca ficha en casilla vacía y el turno pasa al rival')

    .Background
      .Given('un tablero vacío')

    .Scenario('Colocar ficha en casilla vacía')
      .When('el jugador X coloca una ficha en a1')
      .&Then('la casilla a1 pertenece a X')
      .&And('el turno pasa a O')

    .Scenario('Alternancia de turnos')
      .When('X coloca en a1 y O coloca en b2')
      .&Then('a1 pertenece a X')
      .&And('b2 pertenece a O')
      .&And('el turno vuelve a X')

  .Rule('No se puede colocar en casilla ocupada')

    .Scenario('Rechazar colocación en casilla ocupada')
      .Given('un tablero con X en a1')
      .When('O intenta colocar en a1')
      .&Then('se produce un error de movimiento inválido')
      .&And('el turno sigue siendo de O')

  .Rule('Con 3 fichas por jugador se pasa a fase de movimiento')

    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', O, _, X]])

    .Scenario('Transición a fase de movimiento')
      .&Then('el juego está en fase de movimiento')
      .&And('X tiene 3 fichas')
      .&And('O tiene 3 fichas')

    .Scenario('Rechazar colocación cuando se alcanza el máximo de fichas')
      .When('X intenta colocar en a3')
      .&Then('se produce un error');

end.
