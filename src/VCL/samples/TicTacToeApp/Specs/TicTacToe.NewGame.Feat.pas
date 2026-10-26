unit TicTacToe.NewGame.Feat;

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
  Nueva partida @e2e

    Como jugador
    Quiero poder iniciar una nueva partida
    Para jugar desde cero en cualquier momento
  ''')
  .Category(TUnitMarker)
  .UseWorld<TGameWorld>

  .Rule('La partida comienza con tablero vacío, turno de X y fase de colocación')

    .Scenario('Estado inicial del juego')
      .When('el jugador inicia nueva partida')
      .&Then('el tablero está vacío')
      .&And('el turno es de X')
      .&And('el juego está en fase de colocación')
      .&And('la partida no ha terminado')

  .Rule('Nueva partida reinicia el juego desde cualquier estado')

    .Scenario('Reiniciar durante fase de colocación')
      .Given('un tablero vacío')
      .When('X coloca en a1')
      .&And('el jugador inicia nueva partida')
      .&Then('el tablero está vacío')
      .&And('el turno es de X')

    .Scenario('Reiniciar durante fase de movimiento')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', X, O, _]])
      .When('el jugador inicia nueva partida')
      .&Then('el tablero está vacío')
      .&And('el juego está en fase de colocación')

    .Scenario('Reiniciar después de victoria')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', _, _, _]])
      .When('X coloca en a3')
      .&And('el jugador inicia nueva partida')
      .&Then('el tablero está vacío')
      .&And('la partida no ha terminado');

end.
