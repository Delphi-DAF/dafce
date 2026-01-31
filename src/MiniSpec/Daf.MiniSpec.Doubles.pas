unit Daf.MiniSpec.Doubles;
{******************************************************************************
  MiniSpec Test Doubles — Elegant mocking for Delphi

  A minimalist, fluent API for test doubles that matches MiniSpec's elegance.

  Usage examples:

    // Simple stub
    var Calc: ICalculator := Stub<ICalculator>.New
      .Setup('Add').Returns(42);

    // Stub with argument matching
    var Service: IUserService := Stub<IUserService>.New
      .Setup('FindById').WithArgs([1]).Returns(TUser.Create('John'))
      .Setup('FindById').WithArgs([2]).Returns(TUser.Create('Jane'))
      .Setup('FindById').Returns(nil)  // Default fallback
      .Instance;

    // Mock with expectations
    var Logger: ILogger := Mock<ILogger>.New
      .Expects('Log').Once
      .Expects('Flush').AtLeastOnce;
    // ... use logger ...
    Logger.Verify;  // Raises if expectations not met

    // Spy on existing instance
    var Spy: ICalculator := Spy<ICalculator>.On(RealCalculator)
      .Track('Add')
      .Instance;
    // ... use spy ...
    Expect(Spy.CallsTo('Add')).ToEqual(3);

******************************************************************************}
interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections;

type
  /// <summary>
  /// Exception raised when mock expectations are not met
  /// </summary>
  EExpectationFailed = class(Exception);

  /// <summary>
  /// Exception raised when a double is used incorrectly
  /// </summary>
  EDoubleError = class(Exception);

  /// <summary>
  /// Invocation count expectation
  /// </summary>
  TCallExpectation = (
    ceAny,        // Any number of calls (default for stubs)
    ceNever,      // Must not be called
    ceOnce,       // Exactly once
    ceAtLeastOnce,// One or more times
    ceAtMostOnce, // Zero or one time
    ceExactly     // Exact count (requires Count parameter)
  );

  /// <summary>
  /// Record of a single method invocation
  /// </summary>
  TInvocation = record
    MethodName: string;
    Args: TArray<TValue>;
    Timestamp: TDateTime;
    class function Create(const AMethod: string; const AArgs: TArray<TValue>): TInvocation; static;
  end;

  /// <summary>
  /// Configuration for stubbed method behavior
  /// </summary>
  TMethodSetup = record
    MethodName: string;
    ArgMatchers: TArray<TValue>;
    HasArgMatchers: Boolean;
    ReturnValue: TValue;
    RaiseException: Exception;
    ExecProc: TProc<TArray<TValue>>;
    class function Create(const AName: string): TMethodSetup; static;
    function Matches(const Args: TArray<TValue>): Boolean;
  end;

  /// <summary>
  /// Expectation for mock verification
  /// </summary>
  TMethodExpectation = record
    MethodName: string;
    Expectation: TCallExpectation;
    ExpectedCount: Integer;
    ArgMatchers: TArray<TValue>;
    HasArgMatchers: Boolean;
    ActualCount: Integer;
    class function Create(const AName: string): TMethodExpectation; static;
    function Matches(const Args: TArray<TValue>): Boolean;
    procedure RecordCall;
    function IsSatisfied: Boolean;
    function DescribeFailure: string;
  end;

  // Forward declarations
  TDoubleInterceptor = class;
  TSpyInterceptor = class;

  /// <summary>
  /// Virtual method interceptor using RTTI
  /// </summary>
  TDoubleInterceptor = class(TVirtualInterface)
  private
    FInvocations: TList<TInvocation>;
    FSetups: TList<TMethodSetup>;
    FExpectations: TList<TMethodExpectation>;
    FCurrentSetupIdx: Integer;
    FCurrentExpectIdx: Integer;
  protected
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
    function FindSetup(const MethodName: string; const Args: TArray<TValue>): Integer;
  public
    constructor Create(TypeInfo: PTypeInfo);
    destructor Destroy; override;
    procedure AddSetup(const Setup: TMethodSetup);
    procedure AddExpectation(const Expect: TMethodExpectation);
    function GetInvocations: TArray<TInvocation>;
    function GetSetups: TList<TMethodSetup>;
    function GetExpectations: TList<TMethodExpectation>;
    procedure ClearInvocations;
    property CurrentSetupIdx: Integer read FCurrentSetupIdx write FCurrentSetupIdx;
    property CurrentExpectIdx: Integer read FCurrentExpectIdx write FCurrentExpectIdx;
  end;

  /// <summary>
  /// Spy interceptor - delegates calls to original and tracks invocations
  /// </summary>
  TSpyInterceptor = class(TVirtualInterface)
  private
    FOriginal: IInterface;
    FInvocations: TList<TInvocation>;
    FTrackedMethods: TList<string>;
    FTrackAll: Boolean;
    FRttiContext: TRttiContext;
    FInterfaceType: TRttiInterfaceType;
  protected
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
  public
    constructor Create(TypeInfo: PTypeInfo; const Original: IInterface);
    destructor Destroy; override;
    function GetInvocations: TArray<TInvocation>;
    procedure ClearInvocations;
    procedure AddTrackedMethod(const MethodName: string);
    procedure SetTrackAll(Value: Boolean);
    function IsTracking(const MethodName: string): Boolean;
  end;

  /// <summary>
  /// Stub builder - fluent API for creating stubs
  /// </summary>
  Stub<T: IInterface> = record
  private
    FInterceptor: TDoubleInterceptor;
    FInstance: T;
    function GetCurrentSetup: TMethodSetup;
    procedure UpdateCurrentSetup(const Setup: TMethodSetup);
  public
    class function New: Stub<T>; static;
    class operator Implicit(const Value: Stub<T>): T;
    // Fluent configuration
    function Setup(const MethodName: string): Stub<T>;
    function WithArgs(const Args: array of const): Stub<T>; overload;
    function WithArgs(const Args: TArray<TValue>): Stub<T>; overload;
    function Returns(const Value: TValue): Stub<T>; overload;
    function Returns(const Value: Variant): Stub<T>; overload;
    function Raises(E: Exception): Stub<T>;
    function Executes(Proc: TProc<TArray<TValue>>): Stub<T>;
    // Get instance
    function Instance: T;
    // Query calls
    function CallsTo(const MethodName: string): Integer;
    function WasCalled(const MethodName: string): Boolean;
    function LastCallTo(const MethodName: string): TInvocation;
    function Invocations: TArray<TInvocation>;
  end;

  /// <summary>
  /// Mock builder - fluent API for creating mocks with expectations
  /// </summary>
  Mock<T: IInterface> = record
  private
    FInterceptor: TDoubleInterceptor;
    FInstance: T;
    FInExpectMode: Boolean;
    function GetCurrentSetup: TMethodSetup;
    procedure UpdateCurrentSetup(const Setup: TMethodSetup);
    function GetCurrentExpect: TMethodExpectation;
    procedure UpdateCurrentExpect(const Expect: TMethodExpectation);
  public
    class function New: Mock<T>; static;
    class operator Implicit(const Value: Mock<T>): T;
    // Fluent configuration
    function Setup(const MethodName: string): Mock<T>;
    function WithArgs(const Args: array of const): Mock<T>; overload;
    function WithArgs(const Args: TArray<TValue>): Mock<T>; overload;
    function Returns(const Value: TValue): Mock<T>; overload;
    function Returns(const Value: Variant): Mock<T>; overload;
    // Expectations
    function Expects(const MethodName: string): Mock<T>;
    function Never: Mock<T>;
    function Once: Mock<T>;
    function Twice: Mock<T>;
    function AtLeastOnce: Mock<T>;
    function AtMostOnce: Mock<T>;
    function Exactly(Count: Integer): Mock<T>;
    // Get instance
    function Instance: T;
    // Verification
    procedure Verify;
    procedure VerifyAndReset;
    // Query calls
    function CallsTo(const MethodName: string): Integer;
    function WasCalled(const MethodName: string): Boolean;
    function LastCallTo(const MethodName: string): TInvocation;
    function Invocations: TArray<TInvocation>;
  end;

  /// <summary>
  /// Spy builder - wraps real instance and tracks calls
  /// </summary>
  Spy<T: IInterface> = record
  private
    FOriginal: T;
    FSpyInterceptor: TSpyInterceptor;
    FInstance: T;
  public
    class function &On(const Target: T): Spy<T>; static;
    class operator Implicit(const Value: Spy<T>): T;
    function Track(const MethodName: string): Spy<T>;
    function TrackAll: Spy<T>;
    function Instance: T;
    function Original: T;
    // Query calls
    function CallsTo(const MethodName: string): Integer;
    function WasCalled(const MethodName: string): Boolean;
    function LastCallTo(const MethodName: string): TInvocation;
    function Invocations: TArray<TInvocation>;
  end;

{$REGION 'Argument Matchers'}

type
  TArgMatchKind = (amkExact, amkAny, amkNotNull, amkContains, amkPredicate, amkRange);

  TArgMatcher = record
    Kind: TArgMatchKind;
    Value: TValue;
    ExtraValue: TValue;
    function Matches(const Actual: TValue): Boolean;
    class function CreateExact(const V: TValue): TArgMatcher; static;
    class function CreateAny: TArgMatcher; static;
    class function CreateNotNull: TArgMatcher; static;
    class function CreateContains(const SubStr: string): TArgMatcher; static;
  end;

  /// <summary>
  /// Argument matchers for flexible stubbing/expectations
  /// </summary>
  Arg = record
    class function Any: TValue; static;
    class function NotNull: TValue; static;
    class function Contains(const SubStr: string): TValue; static;
  end;

{$ENDREGION}

{ Helper functions - declared in interface for use by generic types }
function VarRecToTValue(const V: TVarRec): TValue;
function ArgsToTValueArray(const Args: array of const): TArray<TValue>;
function TValuesEqual(const A, B: TValue): Boolean;

implementation

uses
  System.Variants;

type
  /// <summary>
  /// Internal: Marker interface for argument matchers
  /// </summary>
  IArgMatcher = interface
    ['{EBFCF3A4-5678-9ABC-DEF0-123456789ABC}']
    function GetMatcher: TArgMatcher;
  end;

  TArgMatcherWrapper = class(TInterfacedObject, IArgMatcher)
  private
    FMatcher: TArgMatcher;
  public
    constructor Create(const AMatcher: TArgMatcher);
    function GetMatcher: TArgMatcher;
  end;

{ Helper Functions }

function VarRecToTValue(const V: TVarRec): TValue;
begin
  case V.VType of
    vtInteger:       Result := TValue.From<Integer>(V.VInteger);
    vtBoolean:       Result := TValue.From<Boolean>(V.VBoolean);
    vtChar:          Result := TValue.From<Char>(Char(V.VChar));
    vtExtended:      Result := TValue.From<Extended>(V.VExtended^);
    vtString:        Result := TValue.From<string>(string(V.VString^));
    vtPointer:       Result := TValue.From<Pointer>(V.VPointer);
    vtPChar:         Result := TValue.From<string>(string(V.VPChar));
    vtObject:        Result := TValue.From<TObject>(V.VObject);
    vtClass:         Result := TValue.From<TClass>(V.VClass);
    vtWideChar:      Result := TValue.From<Char>(V.VWideChar);
    vtPWideChar:     Result := TValue.From<string>(string(V.VPWideChar));
    vtAnsiString:    Result := TValue.From<string>(string(AnsiString(V.VAnsiString)));
    vtCurrency:      Result := TValue.From<Currency>(V.VCurrency^);
    vtVariant:       Result := TValue.FromVariant(V.VVariant^);
    vtInterface:     Result := TValue.From<IInterface>(IInterface(V.VInterface));
    vtWideString:    Result := TValue.From<string>(string(WideString(V.VWideString)));
    vtInt64:         Result := TValue.From<Int64>(V.VInt64^);
    vtUnicodeString: Result := TValue.From<string>(string(V.VUnicodeString));
  else
    Result := TValue.Empty;
  end;
end;

function ArgsToTValueArray(const Args: array of const): TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(Args));
  for I := 0 to High(Args) do
    Result[I] := VarRecToTValue(Args[I]);
end;

function TValuesEqual(const A, B: TValue): Boolean;
var
  VA, VB: Variant;
begin
  // Handle empty values
  if A.IsEmpty and B.IsEmpty then
    Exit(True);
  if A.IsEmpty or B.IsEmpty then
    Exit(False);

  // Try to compare as variants for common types
  try
    if A.TryAsType<Variant>(VA) and B.TryAsType<Variant>(VB) then
      Exit(VA = VB);
  except
    // Fall through to other comparisons
  end;

  // Compare by kind and raw data
  if A.Kind <> B.Kind then
    Exit(False);

  case A.Kind of
    tkInteger, tkInt64:
      Result := A.AsInt64 = B.AsInt64;
    tkFloat:
      Result := A.AsExtended = B.AsExtended;
    tkString, tkLString, tkWString, tkUString:
      Result := A.AsString = B.AsString;
    tkEnumeration:
      Result := A.AsOrdinal = B.AsOrdinal;
    tkClass:
      Result := A.AsObject = B.AsObject;
    tkInterface:
      Result := A.AsInterface = B.AsInterface;
  else
    Result := False;
  end;
end;

{ TInvocation }

class function TInvocation.Create(const AMethod: string; const AArgs: TArray<TValue>): TInvocation;
begin
  Result.MethodName := AMethod;
  Result.Args := AArgs;
  Result.Timestamp := Now;
end;

{ TMethodSetup }

class function TMethodSetup.Create(const AName: string): TMethodSetup;
begin
  Result := Default(TMethodSetup);
  Result.MethodName := AName;
  Result.HasArgMatchers := False;
  Result.ReturnValue := TValue.Empty;
  Result.RaiseException := nil;
  Result.ExecProc := nil;
end;

function TMethodSetup.Matches(const Args: TArray<TValue>): Boolean;
var
  I: Integer;
  Matcher: IArgMatcher;
  ArgMatcher: TArgMatcher;
begin
  if not HasArgMatchers then
    Exit(True);  // No matchers = matches anything

  if Length(Args) <> Length(ArgMatchers) then
    Exit(False);

  for I := 0 to High(Args) do
  begin
    // Check if this is an argument matcher
    if ArgMatchers[I].IsType<IInterface> and
       Supports(ArgMatchers[I].AsInterface, IArgMatcher, Matcher) then
    begin
      ArgMatcher := Matcher.GetMatcher;
      if not ArgMatcher.Matches(Args[I]) then
        Exit(False);
    end
    else
    begin
      // Exact match
      if not TValuesEqual(Args[I], ArgMatchers[I]) then
        Exit(False);
    end;
  end;

  Result := True;
end;

{ TMethodExpectation }

class function TMethodExpectation.Create(const AName: string): TMethodExpectation;
begin
  Result := Default(TMethodExpectation);
  Result.MethodName := AName;
  Result.Expectation := ceAny;
  Result.ExpectedCount := 0;
  Result.HasArgMatchers := False;
  Result.ActualCount := 0;
end;

function TMethodExpectation.Matches(const Args: TArray<TValue>): Boolean;
var
  I: Integer;
  Matcher: IArgMatcher;
  ArgMatcher: TArgMatcher;
begin
  if not HasArgMatchers then
    Exit(True);

  if Length(Args) <> Length(ArgMatchers) then
    Exit(False);

  for I := 0 to High(Args) do
  begin
    if ArgMatchers[I].IsType<IInterface> and
       Supports(ArgMatchers[I].AsInterface, IArgMatcher, Matcher) then
    begin
      ArgMatcher := Matcher.GetMatcher;
      if not ArgMatcher.Matches(Args[I]) then
        Exit(False);
    end
    else
    begin
      if not TValuesEqual(Args[I], ArgMatchers[I]) then
        Exit(False);
    end;
  end;

  Result := True;
end;

procedure TMethodExpectation.RecordCall;
begin
  Inc(ActualCount);
end;

function TMethodExpectation.IsSatisfied: Boolean;
begin
  case Expectation of
    ceAny:         Result := True;
    ceNever:       Result := ActualCount = 0;
    ceOnce:        Result := ActualCount = 1;
    ceAtLeastOnce: Result := ActualCount >= 1;
    ceAtMostOnce:  Result := ActualCount <= 1;
    ceExactly:     Result := ActualCount = ExpectedCount;
  else
    Result := True;
  end;
end;

function TMethodExpectation.DescribeFailure: string;
begin
  case Expectation of
    ceNever:       Result := Format('"%s" expected never, but called %d times',
                           [MethodName, ActualCount]);
    ceOnce:        Result := Format('"%s" expected once, but called %d times',
                           [MethodName, ActualCount]);
    ceAtLeastOnce: Result := Format('"%s" expected at least once, but never called',
                           [MethodName]);
    ceAtMostOnce:  Result := Format('"%s" expected at most once, but called %d times',
                           [MethodName, ActualCount]);
    ceExactly:     Result := Format('"%s" expected %d times, but called %d times',
                           [MethodName, ExpectedCount, ActualCount]);
  else
    Result := '';
  end;
end;

{ TArgMatcher }

function TArgMatcher.Matches(const Actual: TValue): Boolean;
begin
  case Kind of
    amkAny:
      Result := True;

    amkNotNull:
      begin
        if Actual.IsEmpty then
          Exit(False);
        if Actual.IsObject then
          Exit(Actual.AsObject <> nil);
        if Actual.Kind = tkInterface then
          Exit(not Actual.IsEmpty);
        if Actual.Kind = tkUString then
          Exit(Actual.AsString <> '');
        Result := True;
      end;

    amkContains:
      begin
        if Actual.Kind <> tkUString then
          Exit(False);
        Result := Actual.AsString.Contains(ExtraValue.AsString);
      end;

    amkExact:
      Result := TValuesEqual(Actual, Value);

  else
    Result := False;
  end;
end;

class function TArgMatcher.CreateExact(const V: TValue): TArgMatcher;
begin
  Result := Default(TArgMatcher);
  Result.Kind := amkExact;
  Result.Value := V;
end;

class function TArgMatcher.CreateAny: TArgMatcher;
begin
  Result := Default(TArgMatcher);
  Result.Kind := amkAny;
end;

class function TArgMatcher.CreateNotNull: TArgMatcher;
begin
  Result := Default(TArgMatcher);
  Result.Kind := amkNotNull;
end;

class function TArgMatcher.CreateContains(const SubStr: string): TArgMatcher;
begin
  Result := Default(TArgMatcher);
  Result.Kind := amkContains;
  Result.ExtraValue := TValue.From<string>(SubStr);
end;

{ TArgMatcherWrapper }

constructor TArgMatcherWrapper.Create(const AMatcher: TArgMatcher);
begin
  inherited Create;
  FMatcher := AMatcher;
end;

function TArgMatcherWrapper.GetMatcher: TArgMatcher;
begin
  Result := FMatcher;
end;

{ TSpyInterceptor }

constructor TSpyInterceptor.Create(TypeInfo: PTypeInfo; const Original: IInterface);
begin
  inherited Create(TypeInfo, DoInvoke);
  FOriginal := Original;
  FInvocations := TList<TInvocation>.Create;
  FTrackedMethods := TList<string>.Create;
  FTrackAll := False;
  FRttiContext := TRttiContext.Create;
  FInterfaceType := FRttiContext.GetType(TypeInfo) as TRttiInterfaceType;
end;

destructor TSpyInterceptor.Destroy;
begin
  FTrackedMethods.Free;
  FInvocations.Free;
  FRttiContext.Free;
  inherited;
end;

procedure TSpyInterceptor.AddTrackedMethod(const MethodName: string);
begin
  if not FTrackedMethods.Contains(MethodName) then
    FTrackedMethods.Add(MethodName);
end;

procedure TSpyInterceptor.SetTrackAll(Value: Boolean);
begin
  FTrackAll := Value;
end;

function TSpyInterceptor.IsTracking(const MethodName: string): Boolean;
begin
  Result := FTrackAll or FTrackedMethods.Contains(MethodName);
end;

procedure TSpyInterceptor.DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
  out Result: TValue);
var
  RealArgs: TArray<TValue>;
  I: Integer;
  OriginalMethod: TRttiMethod;
begin
  // Skip 'Self' in Args[0]
  if Length(Args) > 0 then
  begin
    SetLength(RealArgs, Length(Args) - 1);
    for I := 1 to High(Args) do
      RealArgs[I - 1] := Args[I];
  end
  else
    RealArgs := nil;

  // Record invocation
  FInvocations.Add(TInvocation.Create(Method.Name, RealArgs));

  // Find and invoke the method on the original object
  OriginalMethod := FInterfaceType.GetMethod(Method.Name);
  if Assigned(OriginalMethod) then
    Result := OriginalMethod.Invoke(TValue.From<IInterface>(FOriginal), Args)
  else
    Result := TValue.Empty;
end;

function TSpyInterceptor.GetInvocations: TArray<TInvocation>;
begin
  Result := FInvocations.ToArray;
end;

procedure TSpyInterceptor.ClearInvocations;
begin
  FInvocations.Clear;
end;

{ TDoubleInterceptor }

constructor TDoubleInterceptor.Create(TypeInfo: PTypeInfo);
begin
  inherited Create(TypeInfo, DoInvoke);
  FInvocations := TList<TInvocation>.Create;
  FSetups := TList<TMethodSetup>.Create;
  FExpectations := TList<TMethodExpectation>.Create;
  FCurrentSetupIdx := -1;
  FCurrentExpectIdx := -1;
end;

destructor TDoubleInterceptor.Destroy;
var
  I: Integer;
begin
  for I := 0 to FSetups.Count - 1 do
    if Assigned(FSetups[I].RaiseException) then
      FSetups[I].RaiseException.Free;

  FInvocations.Free;
  FSetups.Free;
  FExpectations.Free;
  inherited;
end;

procedure TDoubleInterceptor.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  SetupIdx, ExpIdx: Integer;
  Setup: TMethodSetup;
  Expect: TMethodExpectation;
  RealArgs: TArray<TValue>;
  I: Integer;
begin
  // Skip 'Self' in Args[0] for interface methods
  if Length(Args) > 0 then
  begin
    SetLength(RealArgs, Length(Args) - 1);
    for I := 1 to High(Args) do
      RealArgs[I - 1] := Args[I];
  end
  else
    RealArgs := nil;

  // Record invocation
  FInvocations.Add(TInvocation.Create(Method.Name, RealArgs));

  // Update expectations
  for ExpIdx := 0 to FExpectations.Count - 1 do
  begin
    Expect := FExpectations[ExpIdx];
    if SameText(Expect.MethodName, Method.Name) and Expect.Matches(RealArgs) then
    begin
      Expect.RecordCall;
      FExpectations[ExpIdx] := Expect;
    end;
  end;

  // Find matching setup
  SetupIdx := FindSetup(Method.Name, RealArgs);
  if SetupIdx >= 0 then
  begin
    Setup := FSetups[SetupIdx];

    // Execute callback if set
    if Assigned(Setup.ExecProc) then
      Setup.ExecProc(RealArgs);

    // Raise exception if configured
    if Assigned(Setup.RaiseException) then
      raise ExceptClass(Setup.RaiseException.ClassType).Create(Setup.RaiseException.Message);

    // Return configured value
    Result := Setup.ReturnValue;
  end
  else
  begin
    // Default return value
    Result := TValue.Empty;
  end;
end;

function TDoubleInterceptor.FindSetup(const MethodName: string;
  const Args: TArray<TValue>): Integer;
var
  I: Integer;
begin
  // Search in reverse order (last setup wins for same method)
  for I := FSetups.Count - 1 downto 0 do
  begin
    if SameText(FSetups[I].MethodName, MethodName) and FSetups[I].Matches(Args) then
      Exit(I);
  end;
  Result := -1;
end;

procedure TDoubleInterceptor.AddSetup(const Setup: TMethodSetup);
begin
  FSetups.Add(Setup);
  FCurrentSetupIdx := FSetups.Count - 1;
end;

procedure TDoubleInterceptor.AddExpectation(const Expect: TMethodExpectation);
begin
  FExpectations.Add(Expect);
  FCurrentExpectIdx := FExpectations.Count - 1;
end;

function TDoubleInterceptor.GetInvocations: TArray<TInvocation>;
begin
  Result := FInvocations.ToArray;
end;

function TDoubleInterceptor.GetSetups: TList<TMethodSetup>;
begin
  Result := FSetups;
end;

function TDoubleInterceptor.GetExpectations: TList<TMethodExpectation>;
begin
  Result := FExpectations;
end;

procedure TDoubleInterceptor.ClearInvocations;
begin
  FInvocations.Clear;
end;

{ Stub<T> }

class function Stub<T>.New: Stub<T>;
var
  LTypeInfo: PTypeInfo;
begin
  Result := Default(Stub<T>);
  LTypeInfo := TypeInfo(T);
  if LTypeInfo^.Kind <> tkInterface then
    raise EDoubleError.Create('Stub<T> requires an interface type');
  Result.FInterceptor := TDoubleInterceptor.Create(LTypeInfo);
end;

class operator Stub<T>.Implicit(const Value: Stub<T>): T;
begin
  Result := Value.Instance;
end;

function Stub<T>.GetCurrentSetup: TMethodSetup;
begin
  if FInterceptor.CurrentSetupIdx < 0 then
    raise EDoubleError.Create('No method setup active. Call Setup() first.');
  Result := FInterceptor.GetSetups[FInterceptor.CurrentSetupIdx];
end;

procedure Stub<T>.UpdateCurrentSetup(const Setup: TMethodSetup);
begin
  FInterceptor.GetSetups[FInterceptor.CurrentSetupIdx] := Setup;
end;

function Stub<T>.Setup(const MethodName: string): Stub<T>;
begin
  FInterceptor.AddSetup(TMethodSetup.Create(MethodName));
  Result := Self;
end;

function Stub<T>.WithArgs(const Args: array of const): Stub<T>;
var
  Current: TMethodSetup;
begin
  Current := GetCurrentSetup;
  Current.ArgMatchers := ArgsToTValueArray(Args);
  Current.HasArgMatchers := True;
  UpdateCurrentSetup(Current);
  Result := Self;
end;

function Stub<T>.WithArgs(const Args: TArray<TValue>): Stub<T>;
var
  Current: TMethodSetup;
begin
  Current := GetCurrentSetup;
  Current.ArgMatchers := Args;
  Current.HasArgMatchers := True;
  UpdateCurrentSetup(Current);
  Result := Self;
end;

function Stub<T>.Returns(const Value: TValue): Stub<T>;
var
  Current: TMethodSetup;
begin
  Current := GetCurrentSetup;
  Current.ReturnValue := Value;
  UpdateCurrentSetup(Current);
  Result := Self;
end;

function Stub<T>.Returns(const Value: Variant): Stub<T>;
begin
  Result := Returns(TValue.FromVariant(Value));
end;

function Stub<T>.Raises(E: Exception): Stub<T>;
var
  Current: TMethodSetup;
begin
  Current := GetCurrentSetup;
  Current.RaiseException := E;
  UpdateCurrentSetup(Current);
  Result := Self;
end;

function Stub<T>.Executes(Proc: TProc<TArray<TValue>>): Stub<T>;
var
  Current: TMethodSetup;
begin
  Current := GetCurrentSetup;
  Current.ExecProc := Proc;
  UpdateCurrentSetup(Current);
  Result := Self;
end;

function Stub<T>.Instance: T;
var
  LTypeInfo: PTypeInfo;
begin
  if FInstance = nil then
  begin
    LTypeInfo := TypeInfo(T);
    FInterceptor.QueryInterface(GetTypeData(LTypeInfo)^.GUID, FInstance);
  end;
  Result := FInstance;
end;

function Stub<T>.CallsTo(const MethodName: string): Integer;
var
  Inv: TInvocation;
begin
  Result := 0;
  for Inv in FInterceptor.GetInvocations do
    if SameText(Inv.MethodName, MethodName) then
      Inc(Result);
end;

function Stub<T>.WasCalled(const MethodName: string): Boolean;
begin
  Result := CallsTo(MethodName) > 0;
end;

function Stub<T>.LastCallTo(const MethodName: string): TInvocation;
var
  AllInvocations: TArray<TInvocation>;
  I: Integer;
begin
  Result := Default(TInvocation);
  AllInvocations := FInterceptor.GetInvocations;
  for I := High(AllInvocations) downto 0 do
    if SameText(AllInvocations[I].MethodName, MethodName) then
      Exit(AllInvocations[I]);
end;

function Stub<T>.Invocations: TArray<TInvocation>;
begin
  Result := FInterceptor.GetInvocations;
end;

{ Mock<T> }

class function Mock<T>.New: Mock<T>;
var
  LTypeInfo: PTypeInfo;
begin
  Result := Default(Mock<T>);
  LTypeInfo := TypeInfo(T);
  if LTypeInfo^.Kind <> tkInterface then
    raise EDoubleError.Create('Mock<T> requires an interface type');
  Result.FInterceptor := TDoubleInterceptor.Create(LTypeInfo);
  Result.FInExpectMode := False;
end;

class operator Mock<T>.Implicit(const Value: Mock<T>): T;
begin
  Result := Value.Instance;
end;

function Mock<T>.GetCurrentSetup: TMethodSetup;
begin
  if FInterceptor.CurrentSetupIdx < 0 then
    raise EDoubleError.Create('No method setup active. Call Setup() first.');
  Result := FInterceptor.GetSetups[FInterceptor.CurrentSetupIdx];
end;

procedure Mock<T>.UpdateCurrentSetup(const Setup: TMethodSetup);
begin
  FInterceptor.GetSetups[FInterceptor.CurrentSetupIdx] := Setup;
end;

function Mock<T>.GetCurrentExpect: TMethodExpectation;
begin
  if FInterceptor.CurrentExpectIdx < 0 then
    raise EDoubleError.Create('No expectation active. Call Expects() first.');
  Result := FInterceptor.GetExpectations[FInterceptor.CurrentExpectIdx];
end;

procedure Mock<T>.UpdateCurrentExpect(const Expect: TMethodExpectation);
begin
  FInterceptor.GetExpectations[FInterceptor.CurrentExpectIdx] := Expect;
end;

function Mock<T>.Setup(const MethodName: string): Mock<T>;
begin
  FInExpectMode := False;
  FInterceptor.AddSetup(TMethodSetup.Create(MethodName));
  Result := Self;
end;

function Mock<T>.WithArgs(const Args: array of const): Mock<T>;
var
  CurrentSetup: TMethodSetup;
  CurrentExpect: TMethodExpectation;
  ArgValues: TArray<TValue>;
begin
  ArgValues := ArgsToTValueArray(Args);

  if FInExpectMode then
  begin
    CurrentExpect := GetCurrentExpect;
    CurrentExpect.ArgMatchers := ArgValues;
    CurrentExpect.HasArgMatchers := True;
    UpdateCurrentExpect(CurrentExpect);
  end
  else
  begin
    CurrentSetup := GetCurrentSetup;
    CurrentSetup.ArgMatchers := ArgValues;
    CurrentSetup.HasArgMatchers := True;
    UpdateCurrentSetup(CurrentSetup);
  end;
  Result := Self;
end;

function Mock<T>.WithArgs(const Args: TArray<TValue>): Mock<T>;
var
  CurrentSetup: TMethodSetup;
  CurrentExpect: TMethodExpectation;
begin
  if FInExpectMode then
  begin
    CurrentExpect := GetCurrentExpect;
    CurrentExpect.ArgMatchers := Args;
    CurrentExpect.HasArgMatchers := True;
    UpdateCurrentExpect(CurrentExpect);
  end
  else
  begin
    CurrentSetup := GetCurrentSetup;
    CurrentSetup.ArgMatchers := Args;
    CurrentSetup.HasArgMatchers := True;
    UpdateCurrentSetup(CurrentSetup);
  end;
  Result := Self;
end;

function Mock<T>.Returns(const Value: TValue): Mock<T>;
var
  Current: TMethodSetup;
begin
  FInExpectMode := False;
  Current := GetCurrentSetup;
  Current.ReturnValue := Value;
  UpdateCurrentSetup(Current);
  Result := Self;
end;

function Mock<T>.Returns(const Value: Variant): Mock<T>;
begin
  Result := Returns(TValue.FromVariant(Value));
end;

function Mock<T>.Expects(const MethodName: string): Mock<T>;
begin
  FInExpectMode := True;
  FInterceptor.AddExpectation(TMethodExpectation.Create(MethodName));
  Result := Self;
end;

function Mock<T>.Never: Mock<T>;
var
  Current: TMethodExpectation;
begin
  Current := GetCurrentExpect;
  Current.Expectation := ceNever;
  UpdateCurrentExpect(Current);
  Result := Self;
end;

function Mock<T>.Once: Mock<T>;
var
  Current: TMethodExpectation;
begin
  Current := GetCurrentExpect;
  Current.Expectation := ceOnce;
  UpdateCurrentExpect(Current);
  Result := Self;
end;

function Mock<T>.Twice: Mock<T>;
var
  Current: TMethodExpectation;
begin
  Current := GetCurrentExpect;
  Current.Expectation := ceExactly;
  Current.ExpectedCount := 2;
  UpdateCurrentExpect(Current);
  Result := Self;
end;

function Mock<T>.AtLeastOnce: Mock<T>;
var
  Current: TMethodExpectation;
begin
  Current := GetCurrentExpect;
  Current.Expectation := ceAtLeastOnce;
  UpdateCurrentExpect(Current);
  Result := Self;
end;

function Mock<T>.AtMostOnce: Mock<T>;
var
  Current: TMethodExpectation;
begin
  Current := GetCurrentExpect;
  Current.Expectation := ceAtMostOnce;
  UpdateCurrentExpect(Current);
  Result := Self;
end;

function Mock<T>.Exactly(Count: Integer): Mock<T>;
var
  Current: TMethodExpectation;
begin
  Current := GetCurrentExpect;
  Current.Expectation := ceExactly;
  Current.ExpectedCount := Count;
  UpdateCurrentExpect(Current);
  Result := Self;
end;

function Mock<T>.Instance: T;
var
  LTypeInfo: PTypeInfo;
begin
  if FInstance = nil then
  begin
    LTypeInfo := TypeInfo(T);
    FInterceptor.QueryInterface(GetTypeData(LTypeInfo)^.GUID, FInstance);
  end;
  Result := FInstance;
end;

procedure Mock<T>.Verify;
var
  Expect: TMethodExpectation;
  Failures: TStringBuilder;
begin
  Failures := TStringBuilder.Create;
  try
    for Expect in FInterceptor.GetExpectations do
    begin
      if not Expect.IsSatisfied then
      begin
        if Failures.Length > 0 then
          Failures.AppendLine;
        Failures.Append(Expect.DescribeFailure);
      end;
    end;

    if Failures.Length > 0 then
      raise EExpectationFailed.Create('Mock verification failed:'#13#10 + Failures.ToString);
  finally
    Failures.Free;
  end;
end;

procedure Mock<T>.VerifyAndReset;
begin
  Verify;
  FInterceptor.ClearInvocations;
end;

function Mock<T>.CallsTo(const MethodName: string): Integer;
var
  Inv: TInvocation;
begin
  Result := 0;
  for Inv in FInterceptor.GetInvocations do
    if SameText(Inv.MethodName, MethodName) then
      Inc(Result);
end;

function Mock<T>.WasCalled(const MethodName: string): Boolean;
begin
  Result := CallsTo(MethodName) > 0;
end;

function Mock<T>.LastCallTo(const MethodName: string): TInvocation;
var
  AllInvocations: TArray<TInvocation>;
  I: Integer;
begin
  Result := Default(TInvocation);
  AllInvocations := FInterceptor.GetInvocations;
  for I := High(AllInvocations) downto 0 do
    if SameText(AllInvocations[I].MethodName, MethodName) then
      Exit(AllInvocations[I]);
end;

function Mock<T>.Invocations: TArray<TInvocation>;
begin
  Result := FInterceptor.GetInvocations;
end;

{ Spy<T> }

class function Spy<T>.&On(const Target: T): Spy<T>;
var
  LTypeInfo: PTypeInfo;
begin
  Result := Default(Spy<T>);
  Result.FOriginal := Target;
  LTypeInfo := TypeInfo(T);
  if LTypeInfo^.Kind <> tkInterface then
    raise EDoubleError.Create('Spy<T> requires an interface type');
  Result.FSpyInterceptor := TSpyInterceptor.Create(LTypeInfo, IInterface(Target));
end;

class operator Spy<T>.Implicit(const Value: Spy<T>): T;
begin
  Result := Value.Instance;
end;

function Spy<T>.Track(const MethodName: string): Spy<T>;
begin
  FSpyInterceptor.AddTrackedMethod(MethodName);
  Result := Self;
end;

function Spy<T>.TrackAll: Spy<T>;
begin
  FSpyInterceptor.SetTrackAll(True);
  Result := Self;
end;

function Spy<T>.Instance: T;
var
  LTypeInfo: PTypeInfo;
begin
  if FInstance = nil then
  begin
    LTypeInfo := TypeInfo(T);
    FSpyInterceptor.QueryInterface(GetTypeData(LTypeInfo)^.GUID, FInstance);
  end;
  Result := FInstance;
end;

function Spy<T>.Original: T;
begin
  Result := FOriginal;
end;

function Spy<T>.CallsTo(const MethodName: string): Integer;
var
  Inv: TInvocation;
begin
  Result := 0;
  for Inv in FSpyInterceptor.GetInvocations do
    if SameText(Inv.MethodName, MethodName) then
      Inc(Result);
end;

function Spy<T>.WasCalled(const MethodName: string): Boolean;
begin
  Result := CallsTo(MethodName) > 0;
end;

function Spy<T>.LastCallTo(const MethodName: string): TInvocation;
var
  AllInvocations: TArray<TInvocation>;
  I: Integer;
begin
  Result := Default(TInvocation);
  AllInvocations := FSpyInterceptor.GetInvocations;
  for I := High(AllInvocations) downto 0 do
    if SameText(AllInvocations[I].MethodName, MethodName) then
      Exit(AllInvocations[I]);
end;

function Spy<T>.Invocations: TArray<TInvocation>;
begin
  Result := FSpyInterceptor.GetInvocations;
end;

{ Arg }

class function Arg.Any: TValue;
begin
  Result := TValue.From<IInterface>(TArgMatcherWrapper.Create(TArgMatcher.CreateAny));
end;

class function Arg.NotNull: TValue;
begin
  Result := TValue.From<IInterface>(TArgMatcherWrapper.Create(TArgMatcher.CreateNotNull));
end;

class function Arg.Contains(const SubStr: string): TValue;
begin
  Result := TValue.From<IInterface>(TArgMatcherWrapper.Create(TArgMatcher.CreateContains(SubStr)));
end;

end.
