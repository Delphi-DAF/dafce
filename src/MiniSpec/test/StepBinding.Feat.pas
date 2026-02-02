unit StepBinding.Feat;

interface

implementation

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Binding;

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
    .&Then('the result should be 151'); // 100 + 50 + 1

end.
