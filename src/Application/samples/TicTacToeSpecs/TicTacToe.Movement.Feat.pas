unit TicTacToe.Movement.Feat;

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
  TicTacToe: Movimiento de fichas

    Como jugador
    Quiero mover mis fichas a casillas adyacentes
    Para intentar formar una línea de 3
  ''')

  .UseWorld<TGameWorld>

  .Rule('Solo se puede mover a casillas adyacentes vacías')

    .Scenario('Mover ficha propia a casilla adyacente vacía')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X mueve de (0,1) a (0,2)')
      .&Then('(0,1) está vacía')
      .&And('(0,2) pertenece a X')
      .&And('el turno pasa a O')

    .Scenario('Mover ficha en diagonal')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X mueve de (0,1) a (1,2)')
      .&Then('(0,1) está vacía')
      .&And('(1,2) pertenece a X')

  .Rule('Solo se pueden mover fichas propias')

    .Scenario('Rechazar mover ficha del oponente')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X intenta mover de (1,0) a (0,2)')
      .&Then('se produce un error')
      .&And('(1,0) pertenece a O')

  .Rule('No se puede mover a casilla ocupada ni no adyacente')

    .Scenario('Rechazar mover a casilla ocupada')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X intenta mover de (0,0) a (1,0)')
      .&Then('se produce un error')

    .Scenario('Rechazar mover a casilla no adyacente')
      .Given('el siguiente tablero:',
        [[X, X, _],
         [O, O, _],
         [X, O, _]],
        procedure(Ctx: TGameWorld) begin SetupBoardFromTable(Ctx) end)
      .When('X intenta mover de (0,0) a (2,2)')
      .&Then('se produce un error')
      .&And('(0,0) pertenece a X')

  .Rule('Solo se puede mover en fase de movimiento')

    .Scenario('Rechazar mover durante fase de colocación')
      .Given('el juego está en fase de colocación')
      .When('O intenta mover de (0,0) a (0,1)')
      .&Then('se produce un error');

end.
