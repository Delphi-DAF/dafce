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

end.
