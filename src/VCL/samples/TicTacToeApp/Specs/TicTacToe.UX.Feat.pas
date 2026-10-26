unit TicTacToe.UX.Feat;

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
  Experiencia de Usuario @integration

    Como jugador
    Quiero ver información visual clara del estado del juego
    Para poder jugar intuitivamente
  ''')
  .Category(TUnitMarker)
  .UseWorld<TGameWorld>

  .Rule('Cada celda muestra el símbolo del jugador que la ocupa')

    .Scenario('Las celdas muestran el símbolo del jugador')
      .Given('un tablero vacío')
      .When('X coloca en a1')
      .&Then('la celda a1 muestra ''X''')
      .&And('la celda b2 muestra ''''')

  .Rule('El estado muestra turno, fase y resultado de la partida')

    .Scenario('Inicio de partida muestra turno y fase')
      .Given('un tablero vacío')
      .&Then('el estado muestra ''Turno de X · Colocación''')

    .Scenario('El turno se refleja en el estado')
      .Given('un tablero vacío')
      .When('X coloca en a1')
      .&Then('el estado muestra ''Turno de O · Colocación''')

    .Scenario('Victoria se muestra en el estado')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', _, _, _]])
      .When('X coloca en a3')
      .&Then('el estado muestra ''¡X gana!''')
      .&And('la partida ha terminado')

    .Scenario('Fase de movimiento se refleja en el estado')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', X, O, _]])
      .&Then('la fase muestra ''Movimiento''')
      .&And('el estado muestra ''Turno de X · Movimiento''')

  .Rule('Click en celda coloca o mueve ficha según la fase')

    .Scenario('Click coloca pieza en fase de colocación')
      .Given('un tablero vacío')
      .When('el jugador hace click en b2')
      .&Then('la celda b2 muestra ''X''')

    .Scenario('Click selecciona pieza propia en fase de movimiento')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', X, O, _]])
      .When('el jugador hace click en a1')
      .&Then('a1 está seleccionada')

    .Scenario('Click mueve pieza seleccionada')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', X, X, _],
         ['b', O, O, _],
         ['c', X, O, _]])
      .When('el jugador selecciona a2 y hace click en a3')
      .&Then('la celda a3 muestra ''X''')
      .&And('la celda a2 muestra ''''');

end.
