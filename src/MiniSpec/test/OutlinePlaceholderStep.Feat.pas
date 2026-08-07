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
  .Given('a world with no field named T', procedure(World: TGenericProseWorld)
    begin
      World.Count := 1;
    end)
  // Each of these would have raised 'T not found in World' before the fix: one
  // per shape that turns up in real descriptions.
  .&Then('AsType<T> returns the typed value', procedure(World: TGenericProseWorld)
    begin
      Expect(World.Count).ToEqual(1);
    end)
  .&Then('a TList<T> is empty', procedure(World: TGenericProseWorld)
    begin
      Expect(World.Count).ToEqual(1);
    end)
  .&Then('a TDictionary<string, TList<T>> nests them', procedure(World: TGenericProseWorld)
    begin
      Expect(World.Count).ToEqual(1);
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
  // The substitution shows up in the report, not in an assertion: what these
  // scenarios prove is that the step does not raise, which is what regressed.
  .&Then('<Count> at the very start expands', procedure(World: TGenericProseWorld)
    begin
      Expect(World.Count).ToEqual(7);
    end)
  .&Then('and after a space <Count> too', procedure(World: TGenericProseWorld)
    begin
      Expect(World.Count).ToEqual(7);
    end)
  .&Then('and inside punctuation (<Count>,<Count>) as well', procedure(World: TGenericProseWorld)
    begin
      Expect(World.Count).ToEqual(7);
    end);

Feature('''
A placeholder that names nothing is still an error @outline-placeholder

  Tightening the pattern must not buy silence: a misspelled placeholder in an
  Outline would print garbage and pass, so it keeps raising — and the message now
  names both readings, because the two are indistinguishable to the runner.
''')
.UseWorld<TGenericProseWorld>

.Scenario('AnUnknownPlaceholderNamesTheTwoReadings')
  .&Then('running a step whose placeholder names nothing raises, naming both readings',
    procedure(World: TGenericProseWorld)
    begin
      // A step cannot assert on its own description failing: the runner raises
      // before the lambda runs. So the contract is exercised on a step built
      // here and run by hand, which is the same public path.
      var Item: ISpecItem := TSpecItem.Create(sikThen, nil, 'a bogus <NoSuchField> here');
      ExpectException(
        procedure
        begin
          Item.Run(World);
        end).ToMatchMessage('NoSuchField.*generic|generic.*NoSuchField');
    end);

end.
