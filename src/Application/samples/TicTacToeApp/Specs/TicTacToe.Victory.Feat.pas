unit TicTacToe.Victory.Feat;

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
  Condiciones de victoria @e2e

    Como jugador
    Quiero que el juego detecte cuando hay un ganador
    Para saber cuándo termina la partida
  ''')
  .Category(TUnitMarker)
  .UseWorld<TGameWorld>

  .Rule('Gana quien forme una línea de 3 fichas propias')

    .Scenario('Victoria horizontal en fila superior')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', _, _, _]])
      .When('X coloca en a3')
      .&Then('X gana')

    .Scenario('Victoria vertical en columna izquierda')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, O, _],
         ['b', X, O, _],
         ['c', _, _, _]])
      .When('X coloca en c1')
      .&Then('X gana')

  .Rule('La línea puede ser diagonal')

    .Scenario('Victoria diagonal principal')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, O, O],
         ['b', _, X, _],
         ['c', _, _, _]])
      .When('X coloca en c3')
      .&Then('X gana')

    .Scenario('Victoria diagonal secundaria')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', O, O, X],
         ['b', _, X, _],
         ['c', _, _, _]])
      .When('X coloca en c1')
      .&Then('X gana')

  .Rule('Ambos jugadores pueden ganar')

    .Scenario('O puede ganar')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', _, _, X]])
      .When('O coloca en b3')
      .&Then('O gana')

  .Rule('La partida continúa mientras no haya ganador')

    .Scenario('Juego sin ganador continúa en progreso')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, _, _],
         ['b', _, O, _],
         ['c', _, _, _]])
      .&Then('el estado es En Progreso')

  .Rule('No se puede jugar después de una victoria')

    .Scenario('Rechazar movimientos después de victoria')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, X],
         ['b', O, O, _],
         ['c', _, _, _]])
      .When('O intenta colocar en c1')
      .&Then('se produce un error')

  .Rule('Se puede ganar en cualquier fase del juego')

    .Scenario('Victoria al mover una ficha')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', _, X, _],
         ['b', O, X, _],
         ['c', O, O, X]])
      .When('X mueve (a2,a1)')
      .&Then('X gana');

end.
