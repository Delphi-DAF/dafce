unit OutlinePlaceholderStep.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  TOutlineWorld = class
  public
    A, B, Expected: Integer;
    Actual: Integer;
  end;

  /// For the generic-type scenarios below. It has no field named T, so if a step
  /// description mentioning `AsType<T>` were read as a placeholder, the step would
  /// fail — which is exactly what used to happen.
  TGenericProseWorld = class
  public
    Count: Integer;
    /// Para el escenario que construye un paso y lo corre a mano: el paso, y lo que correrlo dijo.
    Item: ISpecItem;
    Report: string;
  end;

initialization

Feature('''
Scenario Outline - placeholder-only Given step @outline-placeholder

  A Given step defined with only a placeholder description and no lambda
  must not be marked as Undefined. The ExampleInit mechanism injects field
  values via RTTI; the step itself is display-only and must pass.
''')
.UseWorld<TOutlineWorld>

.ScenarioOutline('<A> + <B> = <Expected>')
  .Given('operands are <A> and <B>')
  .When('they are summed', procedure(World: TOutlineWorld)
    begin
      World.Actual := World.A + World.B;
    end)
  .&Then('the sum is <Expected>', procedure(World: TOutlineWorld)
    begin
      Expect(World.Actual).ToEqual(World.Expected);
    end)
  .Examples(
    [['A', 'B', 'Expected'],
     [1, 1, 2],
     [3, 4, 7],
     [0, 0, 0]]);

Feature('''
A generic type in a step description is prose, not a placeholder @outline-placeholder

  A placeholder never continues a word, so `<T>` glued to an identifier is the
  angle brackets of a generic type and the description keeps it verbatim. Without
  this, every step naming a generic — and a framework whose suites are full of
  them — failed with a placeholder error pointing at the World.
''')
.UseWorld<TGenericProseWorld>

.Scenario('AGenericTypeIsLeftAlone')
  // Nada que montar: el estado que el escenario necesita es una propiedad del TIPO del World --no
  // tiene ningun campo llamado T-- y eso esta compilado.
  .Given('a world with no field named T').NoAction
  // Cada paso comprueba SU PROPIA descripcion, que es el sujeto: TSpecItem.Run expande los
  // placeholders antes de llamar a la lambda, asi que lo que se lee aqui es el texto ya resuelto
  // -- y aqui la resolucion correcta es no tocar nada. Cada una de las tres formas habria lanzado
  // 'T not found in World' antes del arreglo.
  .&Then('AsType<T> returns the typed value', procedure(World: TGenericProseWorld)
    begin
      Expect(SpecContext.Step.Description).ToEqual('AsType<T> returns the typed value');
    end)
  .&Then('a TList<T> is empty', procedure(World: TGenericProseWorld)
    begin
      Expect(SpecContext.Step.Description).ToEqual('a TList<T> is empty');
    end)
  .&Then('a TDictionary<string, TList<T>> nests them', procedure(World: TGenericProseWorld)
    begin
      Expect(SpecContext.Step.Description)
        .ToEqual('a TDictionary<string, TList<T>> nests them');
    end);

Feature('''
A placeholder after a separator still expands @outline-placeholder

  The boundary is asserted only before the placeholder, so the separators that
  actually precede one in practice — start of description, space, and the
  parentheses and commas of a coordinate — all keep working.
''')
.UseWorld<TGenericProseWorld>

.Scenario('SeparatorsBeforeAPlaceholderStillCount')
  .Given('a world whose Count is 7', procedure(World: TGenericProseWorld)
    begin
      World.Count := 7;
    end)
  // Cada paso comprueba SU PROPIA descripcion ya sustituida, que es lo que el escenario afirma.
  // Antes comparaba World.Count, que es el valor que el Given acababa de escribir: la sustitucion
  // salia en el informe y no en ninguna asercion.
  // El valor sustituido va envuelto en #{...} por contrato --es lo que permite a un reporter
  // distinguir un valor de Example del resto de la frase y darle su propio formato--, asi que la
  // marca forma parte de lo que se afirma: sin ella no hubo sustitucion, hubo texto.
  .&Then('<Count> at the very start expands', procedure(World: TGenericProseWorld)
    begin
      Expect(SpecContext.Step.Description).ToEqual('#{7} at the very start expands');
    end)
  .&Then('and after a space <Count> too', procedure(World: TGenericProseWorld)
    begin
      Expect(SpecContext.Step.Description).ToEqual('and after a space #{7} too');
    end)
  .&Then('and inside punctuation (<Count>,<Count>) as well', procedure(World: TGenericProseWorld)
    begin
      Expect(SpecContext.Step.Description).ToEqual('and inside punctuation (#{7},#{7}) as well');
    end);

Feature('''
A placeholder that names nothing is still an error @outline-placeholder

  Tightening the pattern must not buy silence: a misspelled placeholder in an
  Outline would print garbage and pass, so it keeps raising — and the message now
  names both readings, because the two are indistinguishable to the runner.
''')
.UseWorld<TGenericProseWorld>

.Scenario('AnUnknownPlaceholderNamesTheTwoReadings')
  .Given('a step whose description carries a placeholder that names nothing',
    procedure(World: TGenericProseWorld)
    begin
      // A step cannot assert on its own description failing: the runner raises
      // before the lambda runs. So the contract is exercised on a step built
      // here and run by hand, which is the same public path.
      World.Item := TSpecItem.Create(sikThen, nil, 'a bogus <NoSuchField> here');
    end)
  .When('it is run', procedure(World: TGenericProseWorld)
    begin
      try
        World.Item.Run(World);
        World.Report := '<the step ran without complaining>';
      except
        on E: Exception do
          World.Report := E.Message;
      end;
    end)
  .&Then('running a step whose placeholder names nothing raises, naming both readings',
    procedure(World: TGenericProseWorld)
    begin
      Expect(World.Report).ToMatch('NoSuchField.*generic|generic.*NoSuchField');
    end);

end.
