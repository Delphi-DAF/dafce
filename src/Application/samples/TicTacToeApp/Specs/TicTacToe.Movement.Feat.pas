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

  .Rule('Se permite mover a casilla vacía adyacente en la misma fila')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, _, _],
         ['b', O, O, X],
         ['c', _, X, O]])

    .ScenarioOutline('Mover ficha en horizontal')
      .When('<jugador> mueve (<origen>,<destino>)')
      .&Then('<origen> está vacía')
      .&And('<destino> pertenece a <jugador>')
      .&And('el turno pasa a <rival>')
      .Examples([
        ['jugador', 'origen', 'destino', 'rival'],
        ['X',       'a1',     'a2',      'O'],
        ['X',       'c2',     'c1',      'O']
      ])

    .Scenario('Rechazar saltar casilla en horizontal')
      .When('X intenta mover (a1,a3)')
      .&Then('se produce un error')
      .&And('a1 pertenece a X')

  .Rule('Se permite mover a casilla vacía adyacente en la misma columna')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, O, _],
         ['b', _, O, X],
         ['c', _, X, O]])

    .ScenarioOutline('Mover ficha en vertical')
      .When('<jugador> mueve (<origen>,<destino>)')
      .&Then('<origen> está vacía')
      .&And('<destino> pertenece a <jugador>')
      .&And('el turno pasa a <rival>')
      .Examples([
        ['jugador', 'origen', 'destino', 'rival'],
        ['X',       'a1',     'b1',      'O'],
        ['X',       'b3',     'a3',      'O']
      ])

    .Scenario('Rechazar saltar casilla en vertical')
      .When('X intenta mover (a1,c1)')
      .&Then('se produce un error')
      .&And('a1 pertenece a X')

  .Rule('Se permite mover a casilla vacía adyacente en diagonal mayor')
    .Background
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', _, X, _],
         ['b', X, X, O],
         ['c', O, _, O]])

    .ScenarioOutline('Mover ficha por diagonal mayor')
      .When('<jugador> mueve (<origen>,<destino>)')
      .&Then('<origen> está vacía')
      .&And('<destino> pertenece a <jugador>')
      .&And('el turno pasa a <rival>')
      .Examples([
        ['jugador', 'origen', 'destino', 'rival'],
        ['X',       'b2',     'a1',      'O'],
        ['X',       'b2',     'a3',      'O']
      ])

    .Scenario('Rechazar mover por diagonal menor')
      .When('X intenta mover (b1,c2)')
      .&Then('se produce un error')
      .&And('b1 pertenece a X')

  .Rule('Se rechaza mover ficha ajena, a casilla ocupada o fuera de alcance')
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
        ['a1',     'a2'],
        ['a1',     'b1'],
        ['a1',     'c3']
      ])

  .Rule('Solo se puede mover en fase de movimiento')

    .Scenario('Rechazar mover durante fase de colocación')
      .Given('el juego está en fase de colocación')
      .When('O intenta mover (a1,a2)')
      .&Then('se produce un error');

end.
