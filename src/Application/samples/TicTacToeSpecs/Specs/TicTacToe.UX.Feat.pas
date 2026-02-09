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

  .Rule('Representación visual de las celdas')

    .Scenario('Las celdas muestran el símbolo del jugador')
      .Given('un tablero vacío')
      .When('X coloca en (0,0)')
      .&Then('la celda (0,0) muestra ''X''')
      .&And('la celda (1,1) muestra ''''')

  .Rule('Información de estado del juego')

    .Scenario('Inicio de partida muestra turno y fase')
      .Given('un tablero vacío')
      .&Then('el estado muestra ''Turno de X · Colocación''')

    .Scenario('El turno se refleja en el estado')
      .Given('un tablero vacío')
      .When('X coloca en (0,0)')
      .&Then('el estado muestra ''Turno de O · Colocación''')

    .Scenario('Victoria se muestra en el estado')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [_, _, _]])
      .When('X coloca en (0,2)')
      .&Then('el estado muestra ''¡X gana!''')
      .&And('la partida ha terminado')

    .Scenario('Fase de movimiento se refleja en el estado')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]])
      .&Then('la fase muestra ''Movimiento''')
      .&And('el estado muestra ''Turno de X · Movimiento''')

  .Rule('Interacción inteligente con celdas')

    .Scenario('Click coloca pieza en fase de colocación')
      .Given('un tablero vacío')
      .When('el jugador hace click en (1,1)')
      .&Then('la celda (1,1) muestra ''X''')

    .Scenario('Click selecciona pieza propia en fase de movimiento')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]])
      .When('el jugador hace click en (0,0)')
      .&Then('(0,0) está seleccionada')

    .Scenario('Click mueve pieza seleccionada')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]])
      .When('el jugador selecciona (0,1) y hace click en (0,2)')
      .&Then('la celda (0,2) muestra ''X''')
      .&And('la celda (0,1) muestra ''''');

end.
