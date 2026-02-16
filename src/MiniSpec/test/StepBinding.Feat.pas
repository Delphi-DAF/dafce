unit StepBinding.Feat;

interface

implementation

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.DataTable;

type
  TBindingWorld = class
  public
    A, B, Result: Integer;
  end;

  /// <summary>
  /// Step bindings for calculator operations.
  /// These can be reused across multiple features.
  /// </summary>
  TCalculatorBindings = class
  public
    [Given('I have the number (\d+)')]
    procedure HaveNumber(Ctx: TBindingWorld; N: Integer);

    [Given('the numbers (\d+) and (\d+)')]
    procedure SetNumbers(Ctx: TBindingWorld; A, B: Integer);

    [When('I add them')]
    procedure AddNumbers(Ctx: TBindingWorld);

    [When('I multiply them')]
    procedure MultiplyNumbers(Ctx: TBindingWorld);

    [ThenAttribute('the result should be (\d+)')]
    procedure CheckResult(Ctx: TBindingWorld; Expected: Integer);
  end;

procedure TCalculatorBindings.HaveNumber(Ctx: TBindingWorld; N: Integer);
begin
  Ctx.A := N;
end;

procedure TCalculatorBindings.SetNumbers(Ctx: TBindingWorld; A, B: Integer);
begin
  Ctx.A := A;
  Ctx.B := B;
end;

procedure TCalculatorBindings.AddNumbers(Ctx: TBindingWorld);
begin
  Ctx.Result := Ctx.A + Ctx.B;
end;

procedure TCalculatorBindings.MultiplyNumbers(Ctx: TBindingWorld);
begin
  Ctx.Result := Ctx.A * Ctx.B;
end;

procedure TCalculatorBindings.CheckResult(Ctx: TBindingWorld; Expected: Integer);
begin
  Expect(Ctx.Result).ToEqual(Expected);
end;

initialization
  // Register bindings
  Bindings.RegisterSteps<TCalculatorBindings>;

  Feature('''
  Step Binding Demo @binding

    As a test author
    I want to define step bindings as methods with regex patterns
    So I can reuse steps across multiple features without duplicating code
  ''')
  .UseWorld<TBindingWorld>

  .Scenario('Steps can be bound via [Given] attribute with regex')
    .Given('the numbers 10 and 5')   // No lambda - uses binding
    .When('I add them')              // No lambda - uses binding
    .&Then('the result should be 15') // No lambda - uses binding

  .Scenario('Bindings work with different values')
    .Given('the numbers 7 and 3')
    .When('I add them')
    .&Then('the result should be 10')

  .Scenario('Different operations can share setup bindings')
    .Given('the numbers 6 and 4')
    .When('I multiply them')
    .&Then('the result should be 24')

  .Scenario('Mixed: bindings and lambdas can coexist')
    .Given('the numbers 100 and 50')  // Binding
    .When('I add them', procedure(W: TBindingWorld)
      begin
        // Override with lambda - this takes precedence
        W.Result := W.A + W.B + 1;  // Add 1 extra to verify lambda is used
      end)
    .&Then('the result should be 151') // 100 + 50 + 1

  .ScenarioOutline('Outline: bindings resolve after placeholder substitution')
    .Given('the numbers <A> and <B>')  // <A>,<B> can''t match (\d+) until substituted
    .When('I add them')
    .&Then('the result should be <Result>')
    .Examples([
      ['A',  'B',  'Result'],
      [ 1,    2,    3],
      [10,   20,   30],
      [100, 200,  300]
    ])

  .ScenarioOutline('Outline: different bindings per outline')
    .Given('the numbers <X> and <Y>')
    .When('I multiply them')
    .&Then('the result should be <Product>')
    .Examples([
      ['X', 'Y', 'Product'],
      [ 3,   4,   12],
      [ 7,   8,   56]
    ])

  .ScenarioOutline('Outline: Given with DataTable propagates to each example')
    .Given('the following numbers:',
      [['A',  'B'],
       [ 10,   20]],
      procedure(W: TBindingWorld)
      begin
        var DT := SpecContext.Step.DataTable;
        W.A := DT.Cell(0, 0).AsInteger;
        W.B := DT.Cell(0, 1).AsInteger;
      end)
    .When('I <operation> them')
    .&Then('the result should be <Expected>')
    .Examples([
      ['operation', 'Expected'],
      ['add',             30],
      ['multiply',       200]
    ]);

end.
