unit Mastermind.DetectGameOver.Feat;

interface
uses
  System.Rtti,
  System.SysUtils,
  Daf.MiniSpec,
  Mastermind.Game,
  Mastermind.SpecHelpers;

  // Reorganizamos un poco el codigo para reusar el contexto entre features.
implementation

initialization

  Feature('''
  La partida termina al acertar el códgio secreto

    Como jugador,
    Quiero que el sistema me indique que he acertado el código
    Para *poder dejar* de procrastinar jugando a Mastermind.
    @game-state
  ''')

  .UseWorld<TGameContext>
  .ScenarioOutline('Detectar partida terminda')
    .Given('el código secreto es <Codigo>', procedure(Ctx: TGameContext)
      begin
        Ctx.Game := TMastermind.Create(Ctx.Codigo);
      end)
    .When('realizo la jugada <Jugada>', procedure(Ctx: TGameContext)
      begin
        Ctx.Game.Guess := Ctx.Jugada;
      end)
    .&Then('el juego indica si ha terminado la partida', procedure(Ctx: TGameContext)
      begin
        Expect(Ctx.Game.GameOver).ToEqual(Ctx.GameOver);
      end)
      .Examples([
      ['Codigo',     'Jugada',   'GameOver'],
      [V([1,2,3,4]), V([1,2,3,4]), True],
      [V([1,2,3,4]), V([1,2,3,0]), False],
      [V([1,2,3,4]), V([1,2,0,4]), False],
      [V([1,2,3,4]), V([1,0,3,4]), False],
      [V([1,2,3,4]), V([0,2,3,4]), False],
      [V([1,2,3,4]), V([5,6,7,8]), False],
      [V([1,2,3,4]), V([0,0,0,0]), False]
      ]);
end.
