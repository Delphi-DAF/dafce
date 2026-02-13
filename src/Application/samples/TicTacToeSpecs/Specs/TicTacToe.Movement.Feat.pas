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

  .Background
    .Given('el siguiente tablero:',
      [['', '1', '2', '3'],
       ['a', X, X, _],
       ['b', O, O, _],
       ['c', X, O, _]])

  .Rule('Se puede mover en horizontal o vertical a casilla adyacente vacía')

    .Scenario('Mover ficha en horizontal')
      .When('X mueve (a2,a3)')
      .&Then('a2 está vacía')
      .&And('a3 pertenece a X')
      .&And('el turno pasa a O')

  .Rule('Se puede mover en diagonal solo por las diagonales mayores')

    .Scenario('Mover ficha por diagonal mayor')
      .Given('el siguiente tablero:',
        [['', '1', '2', '3'],
         ['a', O, X, _],
         ['b', _, X, O],
         ['c', X, _, O]])
      .When('X mueve (b2,a3)')
      .&Then('b2 está vacía')
      .&And('a3 pertenece a X')

    .Scenario('Rechazar mover por diagonal menor')
      .When('X intenta mover (a2,b3)')
      .&Then('se produce un error')
      .&And('a2 pertenece a X')

  .Rule('Solo se pueden mover fichas propias')

    .Scenario('Rechazar mover ficha del oponente')
      .When('X intenta mover (b1,a3)')
      .&Then('se produce un error')
      .&And('b1 pertenece a O')

  .Rule('No se puede mover a casilla ocupada ni no adyacente')

    .Scenario('Rechazar mover a casilla ocupada')
      .When('X intenta mover (a1,b1)')
      .&Then('se produce un error')

    .Scenario('Rechazar mover a casilla no adyacente')
      .When('X intenta mover (a1,c3)')
      .&Then('se produce un error')
      .&And('a1 pertenece a X')

  .Rule('Solo se puede mover en fase de movimiento')

    .Scenario('Rechazar mover durante fase de colocación')
      .Given('el juego está en fase de colocación')
      .When('O intenta mover (a1,a2)')
      .&Then('se produce un error');

end.
