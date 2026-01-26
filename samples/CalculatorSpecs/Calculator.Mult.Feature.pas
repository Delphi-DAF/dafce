unit Calculator.Mult.Feature;

interface

uses
  System.SysUtils,
  Calculator.Engine,
  Calculator.SpecHelpers,
  Daf.MiniSpec;

implementation

initialization

  Feature('''
  Calculator Multiplication @arithmetic

    As a user
    I need to multiply and divide numbers
    In order to complete my calculations
  ''')
  .UseWorld<TCalculatorWorld>
  .Background
    .Given('I have a calculator', procedure(World: TCalculatorWorld)
    begin
      World.Calculator := TCalculator.Create;
    end)

  .Rule('Multiplication')
    .ScenarioOutline('<A> * <B> = <Result>')
      .Given('the numbers <A> and <B>')
      .When('they are multiplied', procedure(World: TCalculatorWorld)
        begin
          World.Calculator.Mult(World.A, World.B);
        end)
      .&Then('the result is <Result>', procedure(World: TCalculatorWorld)
        begin
          Expect(World.Calculator.Result).ToEqual(World.Result);
        end)
      .Examples(
        [['A', 'B', 'Result'],
        [1, 1, 1],
        [10, 20, 200],
        [5, -3, -15],
        [3, 0, 0]])

  .Rule('Division')
    .ScenarioOutline('<A> / <B> = <Result>')
      .Given('the numbers <A> and <B>')
      .When('they are divided', procedure(World: TCalculatorWorld)
        begin
          World.Calculator.Divide(World.A, World.B);
        end)
      .&Then('the result is <Result>', procedure(World: TCalculatorWorld)
        begin
          Expect(World.Calculator.Result).ToEqual(World.Result);
        end)
      .Examples(
        [['A', 'B', 'Result'],
        [10, 2, 5],
        [100, 10, 10],
        [7, 2, 3],
        [-15, 3, -5]])

    .Scenario('Division by zero raises exception')
      .Given('the numbers 10 and 0', procedure(World: TCalculatorWorld)
        begin
          World.A := 10;
          World.B := 0;
        end)
      .When('division is attempted', procedure(World: TCalculatorWorld)
        begin
          World.Calculator.Divide(World.A, World.B);
        end)
      .&Then('an exception is raised', procedure(World: TCalculatorWorld)
        begin
          Expect(Raised).ToBe(EDivByZero);
        end);
end.
