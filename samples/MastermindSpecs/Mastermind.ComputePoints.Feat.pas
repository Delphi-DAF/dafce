  unit Mastermind.ComputePoints.Feat;

interface

implementation

uses
  Daf.MiniSpec,
  Mastermind.Game,
  Mastermind.SpecHelpers;

initialization

  Feature('''
  Mastermind: comprobar aciertos exactos

    Como jugador
    Quiero ver la puntuación completa de mis jugadas
    Para poder mejorar mi próxima jugada
  ''')

  .UseWorld<TGameContext>
  .ScenarioOutline('Obtener puntuacion completa @points')
    .Given('el código secreto es <Codigo>', procedure(Ctx: TGameContext)
      begin
        Ctx.Game := TMastermind.Create(Ctx.Codigo);
      end)
    .When('realizo la jugada <Jugada>', procedure(Ctx: TGameContext)
      begin
        Ctx.Game.Guess := Ctx.Jugada;
      end)
    .&Then('El juego indica <correctos> correctos', procedure(Ctx: TGameContext)
      begin
        Expect(Ctx.Game.Corrects).ToEqual(Ctx.correctos);
        Expect(Ctx.Game.Misplaceds).ToEqual(Ctx.desplazados);
      end)
    .&Then('y <desplazados> desplazados ', procedure(Ctx: TGameContext)
      begin
        Expect(Ctx.Game.Misplaceds).ToEqual(Ctx.desplazados);
      end)
    .Examples([
      [   'Codigo',   'Jugada',  'Correctos', 'Desplazados'],
      [ V([1,2,3,4]), V([1,2,3,4]),     4,           0],
      [ V([1,2,3,4]), V([1,2,3,0]),     3,           0],
      [ V([1,2,3,4]), V([1,2,0,4]),     3,           0],
      [ V([1,2,3,4]), V([1,0,3,4]),     3,           0],
      [ V([1,2,3,4]), V([0,2,3,4]),     3,           0],
      [ V([1,2,3,4]), V([0,2,0,4]),     2,           0],
      [ V([1,2,3,4]), V([0,0,0,0]),     0,           0],
      [ V([1,2,3,4]), V([4,3,2,1]),     0,           4],
      [ V([1,2,3,4]), V([4,4,4,4]),     1,           3],
      [ V([1,1,4,4]), V([4,4,4,4]),     2,           6],
      [ V([1,1,4,4]), V([1,4,1,4]),     2,           6],
      [ V([1,1,4,4]), V([1,4,2,0]),     1,           3]
      ]);
end.
