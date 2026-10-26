unit Doubles.Feat;
{******************************************************************************
  MiniSpec Doubles - Specification Tests
  Tests for Stub<T>, Mock<T>, Spy<T> and Arg matchers using BDD/Gherkin style
******************************************************************************}

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Doubles;

type
  {$REGION 'Test Interfaces'}

  {$M+}  // Enable extended RTTI for interfaces - required by TVirtualInterface
  ICalculator = interface
    ['{A1B2C3D4-1234-5678-9ABC-DEF012345678}']
    function Add(A, B: Integer): Integer;
    function Subtract(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
    function Divide(A, B: Integer): Integer;
  end;

  ILogger = interface
    ['{B2C3D4E5-2345-6789-ABCD-EF0123456789}']
    procedure Log(const Message: string);
    procedure LogLevel(Level: Integer; const Message: string);
    function GetLastMessage: string;
  end;

  IUserService = interface
    ['{C3D4E5F6-3456-789A-BCDE-F01234567890}']
    function FindById(Id: Integer): string;
    function FindByName(const Name: string): Integer;
    procedure Save(Id: Integer; const Name: string);
  end;

  {$ENDREGION}

  {$REGION 'Real Implementations for Spy tests'}

  TRealCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(A, B: Integer): Integer;
    function Subtract(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
    function Divide(A, B: Integer): Integer;
  end;

  {$ENDREGION}

  {$REGION 'World Classes'}

  TStubWorld = class
  public
    StubCalc: Stub<ICalculator>;
    StubLogger: Stub<ILogger>;
    StubUser: Stub<IUserService>;
    Calc: ICalculator;
    Logger: ILogger;
    UserSvc: IUserService;
    IntResult: Integer;
    StrResult: string;
    Captured: TCapturedRaise;
    CallbackExecuted: Boolean;
    CallbackArgs: TArray<TValue>;
  end;

  TMockWorld = class
  public
    MockCalc: Mock<ICalculator>;
    MockLogger: Mock<ILogger>;
    Calc: ICalculator;
    Logger: ILogger;
    IntResult: Integer;
  end;

  TSpyWorld = class
  public
    RealCalc: ICalculator;
    SpyCalc: Spy<ICalculator>;
    Calc: ICalculator;
    IntResult: Integer;
  end;

  TArgWorld = class
  public
    StubLogger: Stub<ILogger>;
    StubUser: Stub<IUserService>;
    Logger: ILogger;
    UserSvc: IUserService;
    StrResult: string;
    IntResult: Integer;
    OtherResult: Integer;
    OtherStr: string;
    Captured: TCapturedRaise;
  end;

  {$ENDREGION}

implementation

{ TRealCalculator }

function TRealCalculator.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TRealCalculator.Subtract(A, B: Integer): Integer;
begin
  Result := A - B;
end;

function TRealCalculator.Multiply(A, B: Integer): Integer;
begin
  Result := A * B;
end;

function TRealCalculator.Divide(A, B: Integer): Integer;
begin
  Result := A div B;
end;

initialization

{$REGION 'Stub Feature'}

Feature('Stub<T> - Creating test stubs for interfaces @doubles @stub')

.UseWorld<TStubWorld>

.Rule('Basic stub creation')

  .Scenario('Create a stub for an interface')
    .Given('I create a new stub for ICalculator', procedure(W: TStubWorld)
      begin
        W.StubCalc := Stub<ICalculator>.New;
      end)
    .When('I get the instance', procedure(W: TStubWorld)
      begin
        W.Calc := W.StubCalc.Instance;
      end)
    .&Then('the instance is not nil', procedure(W: TStubWorld)
      begin
        Expect(Assigned(W.Calc)).ToBeTrue;
      end)

  .Scenario('Implicit conversion to interface')
    .Given('I create a stub for ICalculator', procedure(W: TStubWorld)
      begin
        W.StubCalc := Stub<ICalculator>.New;
      end)
    .When('I assign it directly to the interface', procedure(W: TStubWorld)
      begin
        W.Calc := W.StubCalc; // Uses implicit conversion
      end)
    .&Then('the instance is not nil', procedure(W: TStubWorld)
      begin
        Expect(Assigned(W.Calc)).ToBeTrue;
      end)

  .Scenario('Unconfigured methods return default values')
    .Given('I create a stub without configuration', procedure(W: TStubWorld)
      begin
        W.Calc := Stub<ICalculator>.New;
      end)
    .When('I call an unconfigured method', procedure(W: TStubWorld)
      begin
        W.IntResult := W.Calc.Add(5, 3);
      end)
    .&Then('it returns the default value (0 for Integer)', procedure(W: TStubWorld)
      begin
        Expect(W.IntResult).ToEqual(0);
      end)

.Rule('Configuring return values')

  .Scenario('Setup a method to return a specific value')
    .Given('I create a stub with Add returning 42', procedure(W: TStubWorld)
      begin
        W.Calc := Stub<ICalculator>.New
          .Setup('Add').Returns(42);
      end)
    .When('I call Add with any arguments', procedure(W: TStubWorld)
      begin
        W.IntResult := W.Calc.Add(1, 1);
      end)
    .&Then('it returns 42', procedure(W: TStubWorld)
      begin
        Expect(W.IntResult).ToEqual(42);
      end)

  .Scenario('Setup different returns based on arguments')
    .Given('I create a stub with conditional returns', procedure(W: TStubWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindById').WithArgs([1]).Returns('Alice')
          .Setup('FindById').WithArgs([2]).Returns('Bob')
          .Setup('FindById').Returns('Unknown');
        W.UserSvc := W.StubUser;
      end)
    .When('I call FindById with different IDs', procedure(W: TStubWorld)
      begin
        // Will test in Then
      end)
    .&Then('it returns Alice for ID 1', procedure(W: TStubWorld)
      begin
        Expect(W.UserSvc.FindById(1)).ToEqual('Alice');
      end)
    .&And('it returns Bob for ID 2', procedure(W: TStubWorld)
      begin
        Expect(W.UserSvc.FindById(2)).ToEqual('Bob');
      end)
    .&And('it returns Unknown for other IDs', procedure(W: TStubWorld)
      begin
        Expect(W.UserSvc.FindById(999)).ToEqual('Unknown');
      end)

.Rule('Raising exceptions')

  .Scenario('Setup a method to raise an exception')
    .Given('I create a stub that raises on Divide', procedure(W: TStubWorld)
      begin
        W.Calc := Stub<ICalculator>.New
          .Setup('Divide').Raises(EDivByZero.Create('Cannot divide by zero'));
      end)
    .When('I call Divide', procedure(W: TStubWorld)
      begin
        W.Calc.Divide(10, 0);
      end)
    .&Then('it raises EDivByZero', procedure(W: TStubWorld)
      begin
        // Raised consumes what the When left, so it is read once and held.
        W.Captured := Raised;
        ExpectException(W.Captured).ToBe(EDivByZero);
      end)
    .&And('the message is correct', procedure(W: TStubWorld)
      begin
        ExpectException(W.Captured).ToMatchMessage('^Cannot divide by zero$');
      end)

.Rule('Custom behavior with Executes')

  .Scenario('Execute custom code when method is called')
    .Given('I create a stub with Executes callback', procedure(W: TStubWorld)
      begin
        W.CallbackExecuted := False;
        W.StubLogger := Stub<ILogger>.New
          .Setup('Log').Executes(procedure(Args: TArray<TValue>)
            begin
              W.CallbackExecuted := True;
              W.CallbackArgs := Args;
            end);
        W.Logger := W.StubLogger;
      end)
    .When('I call Log', procedure(W: TStubWorld)
      begin
        W.Logger.Log('Test message');
      end)
    .&Then('the callback is executed', procedure(W: TStubWorld)
      begin
        Expect(W.CallbackExecuted).ToBeTrue;
      end)
    .&And('the arguments are captured', procedure(W: TStubWorld)
      begin
        Expect(Length(W.CallbackArgs)).ToEqual(1);
        Expect(W.CallbackArgs[0].AsString).ToEqual('Test message');
      end)

.Rule('Tracking method calls')

  .Scenario('Check if a method was called')
    .Given('I create a stub and call a method', procedure(W: TStubWorld)
      begin
        W.StubCalc := Stub<ICalculator>.New
          .Setup('Add').Returns(0);
        W.Calc := W.StubCalc;
        W.Calc.Add(2, 3);
      end)
    .When('I check WasCalled', procedure(W: TStubWorld)
      begin
        // Checked in Then
      end)
    .&Then('WasCalled returns true for Add', procedure(W: TStubWorld)
      begin
        Expect(W.StubCalc.WasCalled('Add')).ToBeTrue;
      end)
    .&And('WasCalled returns false for Subtract', procedure(W: TStubWorld)
      begin
        Expect(W.StubCalc.WasCalled('Subtract')).ToBeFalse;
      end)

  .Scenario('Count method calls')
    .Given('I create a stub and call Add multiple times', procedure(W: TStubWorld)
      begin
        W.StubCalc := Stub<ICalculator>.New
          .Setup('Add').Returns(0);
        W.Calc := W.StubCalc;
        W.Calc.Add(1, 1);
        W.Calc.Add(2, 2);
        W.Calc.Add(3, 3);
      end)
    .When('I check CallsTo', procedure(W: TStubWorld)
      begin
        // Checked in Then
      end)
    .&Then('CallsTo returns 3', procedure(W: TStubWorld)
      begin
        Expect(W.StubCalc.CallsTo('Add')).ToEqual(3);
      end)

  .Scenario('Get last call details')
    .Given('I create a stub and make several calls', procedure(W: TStubWorld)
      begin
        W.StubCalc := Stub<ICalculator>.New
          .Setup('Add').Returns(0);
        W.Calc := W.StubCalc;
        W.Calc.Add(1, 2);
        W.Calc.Add(10, 20);
      end)
    .When('I get LastCallTo', procedure(W: TStubWorld)
      begin
        // Checked in Then
      end)
    .&Then('it returns the last call arguments', procedure(W: TStubWorld)
      var
        LastCall: TInvocation;
      begin
        LastCall := W.StubCalc.LastCallTo('Add');
        Expect(LastCall.Args[0].AsInteger).ToEqual(10);
        Expect(LastCall.Args[1].AsInteger).ToEqual(20);
      end);

{$ENDREGION}

{$REGION 'Mock Feature'}

Feature('Mock<T> - Creating mocks with expectations @doubles @mock')

.UseWorld<TMockWorld>

.Rule('Basic mock creation')

  .Scenario('Create a mock for an interface')
    .Given('I create a new mock for ICalculator', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New;
      end)
    .When('I get the instance', procedure(W: TMockWorld)
      begin
        W.Calc := W.MockCalc.Instance;
      end)
    .&Then('the instance is not nil', procedure(W: TMockWorld)
      begin
        Expect(Assigned(W.Calc)).ToBeTrue;
      end)

  .Scenario('Implicit conversion to interface')
    .Given('I create a mock for ILogger', procedure(W: TMockWorld)
      begin
        W.MockLogger := Mock<ILogger>.New;
      end)
    .When('I assign it directly to the interface', procedure(W: TMockWorld)
      begin
        W.Logger := W.MockLogger; // Uses implicit conversion
      end)
    .&Then('the instance is not nil', procedure(W: TMockWorld)
      begin
        Expect(Assigned(W.Logger)).ToBeTrue;
      end)

.Rule('Expectation: Once')

  .Scenario('Expects method called exactly once - passes')
    .Given('I create a mock expecting Add called once', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Add').Once;
        W.Calc := W.MockCalc;
      end)
    .When('I call Add exactly once', procedure(W: TMockWorld)
      begin
        W.Calc.Add(1, 2);
      end)
    .&Then('Verify passes', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBeNone;
      end)

  .Scenario('Expects method called once - fails when called twice')
    .Given('I create a mock expecting Add called once', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Add').Once;
        W.Calc := W.MockCalc;
      end)
    .When('I call Add twice', procedure(W: TMockWorld)
      begin
        W.Calc.Add(1, 2);
        W.Calc.Add(3, 4);
      end)
    .&Then('Verify fails', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBe(EExpectationFailed);
      end)

.Rule('Expectation: Never')

  .Scenario('Expects method never called - passes')
    .Given('I create a mock expecting Divide never called', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Divide').Never;
        W.Calc := W.MockCalc;
      end)
    .When('I do not call Divide', procedure(W: TMockWorld)
      begin
        W.Calc.Add(1, 2); // Call something else
      end)
    .&Then('Verify passes', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBeNone;
      end)

  .Scenario('Expects method never called - fails when called')
    .Given('I create a mock expecting Divide never called', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Divide').Never;
        W.Calc := W.MockCalc;
      end)
    .When('I call Divide', procedure(W: TMockWorld)
      begin
        W.Calc.Divide(10, 2);
      end)
    .&Then('Verify fails', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBe(EExpectationFailed);
      end)

.Rule('Expectation: AtLeastOnce')

  .Scenario('Expects method called at least once - passes with multiple calls')
    .Given('I create a mock expecting Add at least once', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Add').AtLeastOnce;
        W.Calc := W.MockCalc;
      end)
    .When('I call Add three times', procedure(W: TMockWorld)
      begin
        W.Calc.Add(1, 2);
        W.Calc.Add(3, 4);
        W.Calc.Add(5, 6);
      end)
    .&Then('Verify passes', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBeNone;
      end)

.Rule('Expectation: Exactly(N)')

  .Scenario('Expects method called exactly N times - passes')
    .Given('I create a mock expecting Add called exactly 3 times', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Add').Exactly(3);
        W.Calc := W.MockCalc;
      end)
    .When('I call Add three times', procedure(W: TMockWorld)
      begin
        W.Calc.Add(1, 1);
        W.Calc.Add(2, 2);
        W.Calc.Add(3, 3);
      end)
    .&Then('Verify passes', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBeNone;
      end)

  .Scenario('Expects method called exactly N times - fails with different count')
    .Given('I create a mock expecting Add called exactly 3 times', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Expects('Add').Exactly(3);
        W.Calc := W.MockCalc;
      end)
    .When('I call Add only twice', procedure(W: TMockWorld)
      begin
        W.Calc.Add(1, 1);
        W.Calc.Add(2, 2);
      end)
    .&Then('Verify fails', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBe(EExpectationFailed);
      end)

.Rule('Combined Setup and Expectations')

  .Scenario('Mock with both return values and expectations')
    .Given('I create a mock with Setup and Expects', procedure(W: TMockWorld)
      begin
        W.MockCalc := Mock<ICalculator>.New
          .Setup('Add').Returns(100)
          .Expects('Add').Once;
        W.Calc := W.MockCalc;
      end)
    .When('I call Add', procedure(W: TMockWorld)
      begin
        W.IntResult := W.Calc.Add(5, 5);
      end)
    .&Then('it returns the configured value', procedure(W: TMockWorld)
      begin
        Expect(W.IntResult).ToEqual(100);
      end)
    .&And('Verify passes', procedure(W: TMockWorld)
      begin
        ExpectException(
          procedure
          begin
            W.MockCalc.Verify;
          end).ToBeNone;
      end);

{$ENDREGION}

{$REGION 'Spy Feature'}

Feature('Spy<T> - Wrapping real objects to track calls @doubles @spy')

.UseWorld<TSpyWorld>

.Rule('Basic spy creation')

  .Scenario('Create a spy wrapping a real calculator')
    .Given('I have a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
      end)
    .When('I create a spy on it', procedure(W: TSpyWorld)
      begin
        W.SpyCalc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
        W.Calc := W.SpyCalc;
      end)
    .&Then('the spy instance is not nil', procedure(W: TSpyWorld)
      begin
        Expect(Assigned(W.Calc)).ToBeTrue;
      end)

  .Scenario('Implicit conversion from spy to interface')
    .Given('I have a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
      end)
    .When('I create a spy and assign directly', procedure(W: TSpyWorld)
      begin
        W.SpyCalc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
        W.Calc := W.SpyCalc; // Uses implicit conversion
      end)
    .&Then('the instance works correctly', procedure(W: TSpyWorld)
      begin
        Expect(Assigned(W.Calc)).ToBeTrue;
      end)

.Rule('Spy delegates to real implementation')

  .Scenario('Spy calls through to real object')
    .Given('I have a spy on a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
        W.Calc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
      end)
    .When('I call Add(2, 3)', procedure(W: TSpyWorld)
      begin
        W.IntResult := W.Calc.Add(2, 3);
      end)
    .&Then('it returns the real result (5)', procedure(W: TSpyWorld)
      begin
        Expect(W.IntResult).ToEqual(5);
      end)

  .Scenario('Spy calls all real methods')
    .Given('I have a spy on a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
        W.Calc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
      end)
    .When('I call various methods', procedure(W: TSpyWorld)
      begin
        // All calls go to real implementation
      end)
    .&Then('Add returns real result', procedure(W: TSpyWorld)
      begin
        Expect(W.Calc.Add(10, 5)).ToEqual(15);
      end)
    .&And('Subtract returns real result', procedure(W: TSpyWorld)
      begin
        Expect(W.Calc.Subtract(10, 3)).ToEqual(7);
      end)
    .&And('Multiply returns real result', procedure(W: TSpyWorld)
      begin
        Expect(W.Calc.Multiply(4, 5)).ToEqual(20);
      end)
    .&And('Divide returns real result', procedure(W: TSpyWorld)
      begin
        Expect(W.Calc.Divide(20, 4)).ToEqual(5);
      end)

.Rule('Spy tracks method calls')

  .Scenario('Spy tracks WasCalled')
    .Given('I have a spy on a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
        W.SpyCalc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
        W.Calc := W.SpyCalc;
      end)
    .When('I call Add', procedure(W: TSpyWorld)
      begin
        W.Calc.Add(1, 2);
      end)
    .&Then('WasCalled returns true for Add', procedure(W: TSpyWorld)
      begin
        Expect(W.SpyCalc.WasCalled('Add')).ToBeTrue;
      end)
    .&And('WasCalled returns false for Subtract', procedure(W: TSpyWorld)
      begin
        Expect(W.SpyCalc.WasCalled('Subtract')).ToBeFalse;
      end)

  .Scenario('Spy counts method calls')
    .Given('I have a spy on a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
        W.SpyCalc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
        W.Calc := W.SpyCalc;
      end)
    .When('I call Add multiple times', procedure(W: TSpyWorld)
      begin
        W.Calc.Add(1, 1);
        W.Calc.Add(2, 2);
        W.Calc.Add(3, 3);
        W.Calc.Add(4, 4);
      end)
    .&Then('CallsTo returns the correct count', procedure(W: TSpyWorld)
      begin
        Expect(W.SpyCalc.CallsTo('Add')).ToEqual(4);
      end)

  .Scenario('Spy captures call arguments')
    .Given('I have a spy on a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
        W.SpyCalc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
        W.Calc := W.SpyCalc;
      end)
    .When('I call Add with specific arguments', procedure(W: TSpyWorld)
      begin
        W.Calc.Add(100, 200);
      end)
    .&Then('LastCallTo captures the arguments', procedure(W: TSpyWorld)
      var
        LastCall: TInvocation;
      begin
        LastCall := W.SpyCalc.LastCallTo('Add');
        Expect(LastCall.Args[0].AsInteger).ToEqual(100);
        Expect(LastCall.Args[1].AsInteger).ToEqual(200);
      end)

  .Scenario('Spy returns all invocations')
    .Given('I have a spy on a real calculator', procedure(W: TSpyWorld)
      begin
        W.RealCalc := TRealCalculator.Create;
        W.SpyCalc := Spy<ICalculator>.&On(W.RealCalc).TrackAll;
        W.Calc := W.SpyCalc;
      end)
    .When('I call multiple methods', procedure(W: TSpyWorld)
      begin
        W.Calc.Add(1, 2);
        W.Calc.Subtract(5, 3);
        W.Calc.Multiply(2, 4);
      end)
    .&Then('Invocations returns all calls', procedure(W: TSpyWorld)
      var
        Invs: TArray<TInvocation>;
      begin
        Invs := W.SpyCalc.Invocations;
        Expect(Length(Invs)).ToEqual(3);
        Expect(Invs[0].MethodName).ToEqual('Add');
        Expect(Invs[1].MethodName).ToEqual('Subtract');
        Expect(Invs[2].MethodName).ToEqual('Multiply');
      end);

{$ENDREGION}

{$REGION 'Arg Matchers Feature'}

Feature('Arg matchers - Flexible argument matching @doubles @matchers')

.UseWorld<TArgWorld>

  .Scenario('Arg.Any matches any value')
    .Given('I create a stub with Arg.Any matcher', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindById').WithArgs([Arg.Any]).Returns('AnyUser');
        W.UserSvc := W.StubUser;
      end)
    .When('I call with different values', procedure(W: TArgWorld)
      begin
        W.StrResult := W.UserSvc.FindById(1) + ',' +
                       W.UserSvc.FindById(999) + ',' +
                       W.UserSvc.FindById(-1);
      end)
    .&Then('all calls match and return the configured value', procedure(W: TArgWorld)
      begin
        Expect(W.StrResult).ToEqual('AnyUser,AnyUser,AnyUser');
      end)

  .Scenario('Arg.NotNull refuses a value that is empty')
    .Given('I create a stub whose setup demands a non-empty name', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindByName').WithArgs([Arg.NotNull]).Returns(7);
        W.UserSvc := W.StubUser;
      end)
    .When('I call it with a name and with an empty string', procedure(W: TArgWorld)
      begin
        W.IntResult := W.UserSvc.FindByName('ana');
        W.OtherResult := W.UserSvc.FindByName('');
      end)
    .&Then('the name matches and the empty string is left to the default', procedure(W: TArgWorld)
      begin
        // Nothing matched the second call, so what comes back is the zero of the
        // return type. That is also the control: a matcher that accepted everything
        // would return 7 twice.
        Expect(W.IntResult).ToEqual(7);
        Expect(W.OtherResult).ToEqual(0);
      end)

  .Scenario('Arg.Contains matches on a substring')
    .Given('I create a stub whose setup demands a substring', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindByName').WithArgs([Arg.Contains('@admin')]).Returns(1);
        W.UserSvc := W.StubUser;
      end)
    .When('I call it with a name that carries it and with one that does not', procedure(W: TArgWorld)
      begin
        W.IntResult := W.UserSvc.FindByName('root@admin.co');
        W.OtherResult := W.UserSvc.FindByName('user@corp.co');
      end)
    .&Then('only the one carrying the substring matches', procedure(W: TArgWorld)
      begin
        Expect(W.IntResult).ToEqual(1);
        Expect(W.OtherResult).ToEqual(0);
      end)

  .Scenario('The setup declared last is the one that answers')
    .Given('I create a stub with two setups whose matchers both accept the value', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindByName').WithArgs([Arg.Contains('@admin')]).Returns(1)
          .Setup('FindByName').WithArgs([Arg.NotNull]).Returns(2);
        W.UserSvc := W.StubUser;
      end)
    .When('I call it with a value both of them accept', procedure(W: TArgWorld)
      begin
        W.IntResult := W.UserSvc.FindByName('root@admin.co');
      end)
    .&Then('the later setup wins, and declaring the narrow one first does not protect it', procedure(W: TArgWorld)
      begin
        Expect(W.IntResult).ToEqual(2);
      end)

  .Scenario('A setup with matchers is preferred to one without')
    .Given('I create a stub with a catch-all setup and a narrower one', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindByName').Returns(9)
          .Setup('FindByName').WithArgs([Arg.NotNull]).Returns(1);
        W.UserSvc := W.StubUser;
      end)
    .When('I call it with a value the matcher accepts and with one it refuses', procedure(W: TArgWorld)
      begin
        W.IntResult := W.UserSvc.FindByName('ana');
        W.OtherResult := W.UserSvc.FindByName('');
      end)
    .&Then('the matcher answers what it accepts and the catch-all takes the rest', procedure(W: TArgWorld)
      begin
        Expect(W.IntResult).ToEqual(1);
        Expect(W.OtherResult).ToEqual(9);
      end)

  .Scenario('Arg.Matching accepts exactly what its predicate accepts')
    .Given('I create a stub whose setup demands an id within a range', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindById').WithArgs([Arg.Matching<Integer>(
            function(Id: Integer): Boolean
            begin
              Result := (Id >= 1) and (Id <= 10);
            end)]).Returns('in range');
        W.UserSvc := W.StubUser;
      end)
    .When('I call it with an id inside the range and one outside', procedure(W: TArgWorld)
      begin
        W.StrResult := W.UserSvc.FindById(7);
        W.OtherStr := W.UserSvc.FindById(99);
      end)
    .&Then('only the one the predicate accepts matches', procedure(W: TArgWorld)
      begin
        // Both halves are the control: a predicate nobody calls leaves the first empty,
        // and one whose answer is discarded answers the second one too.
        Expect(W.StrResult).ToEqual('in range');
        Expect(W.OtherStr).ToEqual('');
      end)

  .Scenario('A predicate declared over another type does not match')
    .Given('I create a stub with a catch-all and a predicate over strings', procedure(W: TArgWorld)
      begin
        W.StubUser := Stub<IUserService>.New
          .Setup('FindById').Returns('fallback')
          .Setup('FindById').WithArgs([Arg.Matching<string>(
            function(Name: string): Boolean
            begin
              Result := True;
            end)]).Returns('matched');
        W.UserSvc := W.StubUser;
      end)
    .When('I call the method, whose argument is an integer', procedure(W: TArgWorld)
      begin
        W.StrResult := W.UserSvc.FindById(7);
      end)
    .&Then('the catch-all answers, and the predicate is never asked', procedure(W: TArgWorld)
      begin
        // The predicate says yes to everything, so 'matched' is what comes back if the
        // argument reaches it at all.
        Expect(W.StrResult).ToEqual('fallback');
      end)

  .Scenario('A matcher handed no predicate is refused where it is declared')
    .When('I ask for a matcher and give it nothing', procedure(W: TArgWorld)
      begin
        Arg.Matching<Integer>(nil);
      end)
    .&Then('it breaks at the setup, and not on a call that would silently not match', procedure(W: TArgWorld)
      begin
        W.Captured := Raised;
        ExpectException(W.Captured).ToBe(EDoubleError);
        ExpectException(W.Captured).ToHaveMessage('requires a predicate');
      end);

{$ENDREGION}

end.
