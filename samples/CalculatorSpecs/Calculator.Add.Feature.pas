unit Calculator.Add.Feature;

interface
implementation
uses
  Calculator.Engine,
  Calculator.SpecHelpers,
  Daf.MiniSpec;

initialization

  // TIP: To see a failing test, change one of the expected Results below
  //      For example: [1, 1, 999] will fail because 1+1 <> 999

  Feature('''
  Calculator Addition @arithmetic

    As a user
    I need to add and subtract numbers
    In order to complete my calculations
  ''')
  .UseWorld<TCalculatorWorld>
  .Background
    .Given('I have a calculator', procedure(World: TCalculatorWorld)
    begin
      World.Calculator := TCalculator.Create;
    end)

  .Rule('Addition')
    .Scenario('Adding two positive numbers')
      .Given('the numbers 2 and 3', procedure(World: TCalculatorWorld)
        begin
          World.A := 2;
          World.B := 3;
        end)
      .When('they are added', procedure(World: TCalculatorWorld)
        begin
          World.Calculator.Add(World.A, World.B);
        end)
      .&Then('the result is 5', procedure(World: TCalculatorWorld)
        begin
          Expect(World.Calculator.Result).ToEqual(5);
        end)

    .ScenarioOutline('Adding <A> and <B> should be <Result>')
      .Given('the numbers <A> and <B>')
      .When('they are added', procedure(World: TCalculatorWorld)
        begin
          World.Calculator.Add(World.A, World.B);
        end)
      .&Then('the result is <Result>', procedure(World: TCalculatorWorld)
        begin
          Expect(World.Calculator.Result).ToEqual(World.Result);
        end)
      .Examples(
        [['A', 'B', 'Result'],
        [1, 1, 2],
        [10, 20, 30],
        [5, -2, 3],
        [0, 0, 0]])

  .Rule('Subtraction')
    .ScenarioOutline('Subtracting: <A> - <B> = <Result>')
      .Given('the numbers <A> and <B>')
      .When('they are subtracted', procedure(World: TCalculatorWorld)
        begin
          World.Calculator.Subtract(World.A, World.B);
        end)
      .&Then('the result is <Result>', procedure(World: TCalculatorWorld)
        begin
          Expect(World.Calculator.Result).ToEqual(World.Result);
        end)
      .Examples(
        [['A', 'B', 'Result'],
        [5, 3, 2],
        [10, 20, -10],
        [0, 0, 0],
        [-5, -3, -2]]);
end.
