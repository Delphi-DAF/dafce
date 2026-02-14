unit TicTacToe.Movement.Feat;

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
  Movimiento de fichas @e2e

    Como jugador
    Quiero mover mis fichas a casillas adyacentes
    Para intentar formar una línea de 3
  ''')
  .Category(TUnitMarker)
  .UseWorld<TGameWorld>

  .Rule('Movimiento horizontal')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, _, X],
         ['b', O, O, _],
         ['c', X, O, _]])

    .ScenarioOutline('Mover ficha en horizontal')
      .When('<jugador> mueve (<origen>,<destino>)')
      .&Then('<origen> está vacía')
      .&And('<destino> pertenece a <jugador>')
      .&And('el turno pasa a <rival>')
      .Examples([
        ['jugador', 'origen', 'destino', 'rival'],
        ['X',       'a1',     'a2',      'O'],
        ['X',       'a3',     'a2',      'O']
      ])

  .Rule('Movimiento vertical')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, O, X],
         ['b', O, _, _],
         ['c', O, X, _]])

    .ScenarioOutline('Mover ficha en vertical')
      .When('<jugador> mueve (<origen>,<destino>)')
      .&Then('<origen> está vacía')
      .&And('<destino> pertenece a <jugador>')
      .&And('el turno pasa a <rival>')
      .Examples([
        ['jugador', 'origen', 'destino', 'rival'],
        ['X',       'a3',     'b3',      'O'],
        ['X',       'c2',     'b2',      'O']
      ])

  .Rule('Movimiento diagonal (solo diagonales mayores)')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', O, X, _],
         ['b', X, X, O],
         ['c', _, O, _]])

    .ScenarioOutline('Mover ficha por diagonal mayor')
      .When('<jugador> mueve (<origen>,<destino>)')
      .&Then('<origen> está vacía')
      .&And('<destino> pertenece a <jugador>')
      .Examples([
        ['jugador', 'origen', 'destino'],
        ['X',       'b2',     'a3'],
        ['X',       'b2',     'c1']
      ])

    .Scenario('Rechazar mover por diagonal menor')
      .When('X intenta mover (a2,b1)')
      .&Then('se produce un error')
      .&And('a2 pertenece a X')

  .Rule('Restricciones de movimiento')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', X, O, _]])

    .ScenarioOutline('Rechazar movimiento inválido')
      .When('X intenta mover (<origen>,<destino>)')
      .&Then('se produce un error')
      .Examples([
        ['origen', 'destino'],
        ['b1',     'a3'],
        ['a1',     'b1'],
        ['a1',     'c3']
      ])

  .Rule('Solo se puede mover en fase de movimiento')

    .Scenario('Rechazar mover durante fase de colocación')
      .Given('el juego está en fase de colocación')
      .When('O intenta mover (a1,a2)')
      .&Then('se produce un error');

end.
