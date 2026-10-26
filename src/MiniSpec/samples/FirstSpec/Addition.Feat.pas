unit Addition.Feat;

interface

implementation

uses
  Daf.MiniSpec,
  Calculator.Engine;  // Here's TCalculator, the system we're specifying

type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;  // The SUT (System Under Test)
    destructor Destroy; override;
  end;

destructor TCalculatorWorld.Destroy;
begin
  Calculator.Free;
  inherited;
end;

initialization

Feature('Calculator - Addition')

.UseWorld<TCalculatorWorld>

.Background
  .Given('I have a calculator', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Calculator := TCalculator.Create;
    end)

.Scenario('Add two positive numbers')
  .When('I add 2 and 3', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Calculator.Add(2, 3);
    end)
  .&Then('the result is 5', procedure(Ctx: TCalculatorWorld)
    begin
      Expect(Ctx.Calculator.Result).ToEqual(5);
    end)

end.
