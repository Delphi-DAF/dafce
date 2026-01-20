unit Daf.MiniSpec.Expects;

interface
uses
  System.SysUtils,
  System.Math;

type
  ExpectFail = class(Exception);

  /// <summary>
  /// Tipo para capturar una excepción lanzada por un procedimiento
  /// </summary>
  TExceptionCapture = record
  strict private
    FExceptionClass: ExceptClass;
    FExceptionMessage: string;
    FWasRaised: Boolean;
  public
    constructor Create(Proc: TProc);
    property WasRaised: Boolean read FWasRaised;
    property ExceptionClass: ExceptClass read FExceptionClass;
    property ExceptionMessage: string read FExceptionMessage;
  end;

  TExpect = record
  strict private
    FValue: Variant;
  public
    class function Fail(Text: string): ExpectFail;overload;static;
    class function Fail(TextFmt: string; Args: array of const): ExpectFail;overload;static;
    constructor Create(const AValue: Variant);
    procedure ToEqual(const AExpected: Variant);
    procedure ToNotEqual(const AExpected: Variant);
    procedure ToBeGreaterThan(const Value: Variant);
    procedure ToBeGreaterOrEqual(const AExpected: Variant);
    procedure ToBeLessOrEqual(const AExpected: Variant);
    procedure ToBeLessThan(const AExpected: Variant);
    procedure ToBeTrue;
    procedure ToBeFalse;
    procedure ToBeEmpty;
    procedure ToBeNull;
    procedure ToNotBeNull;
    procedure ToContain(const SubStr: string);
    procedure ToStartWith(const Prefix: string);
    procedure ToEndWith(const Suffix: string);
    procedure ToMatch(const Pattern: string);
    procedure ToHaveCount(Expected: Integer);
    /// <summary>
    /// Verifica que el valor esté cerca del esperado dentro de una tolerancia absoluta
    /// </summary>
    procedure ToBeCloseTo(const Expected: Double; Tolerance: Double = 0.0001);
    /// <summary>
    /// Verifica que el valor esté dentro de un porcentaje del esperado
    /// </summary>
    procedure ToBeWithinPercent(const Expected: Double; Percent: Double);
    /// <summary>
    /// Verifica que el valor esté en el rango [Min, Max] (inclusivo)
    /// </summary>
    procedure ToBeBetween(const Min, Max: Variant);
  end;

  TExpectException = record
  strict private
    FCapture: TExceptionCapture;
  public
    constructor Create(const ACapture: TExceptionCapture);
    /// <summary>
    /// Verifica que se lanzó una excepción
    /// </summary>
    procedure ToRaise;
    /// <summary>
    /// Verifica que se lanzó una excepción del tipo especificado
    /// </summary>
    procedure ToRaiseType(ExceptClass: ExceptClass);
    /// <summary>
    /// Verifica que se lanzó una excepción con el mensaje indicado
    /// </summary>
    procedure ToRaiseWithMessage(const Msg: string);
    /// <summary>
    /// Verifica que NO se lanzó ninguna excepción
    /// </summary>
    procedure ToNotRaise;
  end;

implementation
uses
  System.StrUtils,
  System.Variants,
  System.RegularExpressions;

{ TExceptionCapture }

constructor TExceptionCapture.Create(Proc: TProc);
begin
  FWasRaised := False;
  FExceptionClass := nil;
  FExceptionMessage := '';
  try
    Proc;
  except
    on E: Exception do
    begin
      FWasRaised := True;
      FExceptionClass := ExceptClass(E.ClassType);
      FExceptionMessage := E.Message;
    end;
  end;
end;

{ TExpect }

class function TExpect.Fail(Text: string): ExpectFail;
begin
  Result := ExpectFail.Create(Text);
end;

class function TExpect.Fail(TextFmt: string; Args: array of const): ExpectFail;
begin
  Result := ExpectFail.CreateFmt(TextFmt, Args);
end;

constructor TExpect.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TExpect.ToEqual(const AExpected: Variant);
begin
  if FValue <> AExpected then
    raise Fail('Expected %s but got %s', [VarToStr(AExpected), VarToStr(FValue)]);
end;

procedure TExpect.ToNotEqual(const AExpected: Variant);
begin
  if FValue = AExpected then
    raise Fail('Expected value to not equal %s', [VarToStr(AExpected)]);
end;

procedure TExpect.ToBeLessThan(const AExpected: Variant);
begin
  if not (FValue < AExpected) then
    raise Fail('Expected less than %s but got %s', [VarToStr(AExpected), VarToStr(FValue)]);
end;

procedure TExpect.ToBeLessOrEqual(const AExpected: Variant);
begin
  if not (FValue <= AExpected) then
    raise Fail('Expected less or equal to %s but got %s', [VarToStr(AExpected), VarToStr(FValue)]);
end;

procedure TExpect.ToBeGreaterOrEqual(const AExpected: Variant);
begin
  if not (FValue >= AExpected) then
    raise Fail('Expected greater or equal to %s but got %s', [VarToStr(AExpected), VarToStr(FValue)]);
end;

procedure TExpect.ToBeTrue;
begin
  if not FValue then
    raise Fail('Expected value to be True');
end;

procedure TExpect.ToBeFalse;
begin
  if FValue then
    raise Fail('Expected value to be False');
end;

procedure TExpect.ToBeGreaterThan(const Value: Variant);
begin
  if FValue <= Value then
    raise Fail('Expected greater than %s but got %s', [VarToStr(Value), VarToStr(FValue)]);
end;

procedure TExpect.ToBeNull;
begin
  if not VarIsNull(FValue) then
    raise Fail('Expected value to be Null');
end;

procedure TExpect.ToNotBeNull;
begin
  if VarIsNull(FValue) then
    raise Fail('Expected value to not be Null');
end;

procedure TExpect.ToContain(const SubStr: string);
begin
  if not ContainsText(VarToStr(FValue), SubStr) then
    raise Fail('Expected "%s" to contain "%s"', [VarToStr(FValue), SubStr]);
end;

procedure TExpect.ToStartWith(const Prefix: string);
var
  S: string;
begin
  S := VarToStr(FValue);
  if not S.StartsWith(Prefix, True) then
    raise Fail('Expected "%s" to start with "%s"', [S, Prefix]);
end;

procedure TExpect.ToEndWith(const Suffix: string);
var
  S: string;
begin
  S := VarToStr(FValue);
  if not S.EndsWith(Suffix, True) then
    raise Fail('Expected "%s" to end with "%s"', [S, Suffix]);
end;

procedure TExpect.ToMatch(const Pattern: string);
var
  S: string;
begin
  S := VarToStr(FValue);
  if not TRegEx.IsMatch(S, Pattern) then
    raise Fail('Expected "%s" to match pattern "%s"', [S, Pattern]);
end;

procedure TExpect.ToBeEmpty;
begin
  if not VarIsEmpty(FValue) and (VarToStr(FValue) <> '') then
    raise Fail('Expected value to be empty');
end;

procedure TExpect.ToBeBetween(const Min, Max: Variant);
begin
  if (FValue < Min) or (FValue > Max) then
    raise Fail('Expected %s to be between %s and %s', [VarToStr(FValue), VarToStr(Min), VarToStr(Max)]);
end;

procedure TExpect.ToHaveCount(Expected: Integer);
begin
  if VarArrayDimCount(FValue) = 0 then
    raise Fail('Expected array value');
  if VarArrayHighBound(FValue, 1) - VarArrayLowBound(FValue, 1) + 1 <> Expected then
    raise Fail('Expected count %d but got %d', [Expected, VarArrayHighBound(FValue, 1) - VarArrayLowBound(FValue, 1) + 1]);
end;

procedure TExpect.ToBeCloseTo(const Expected: Double; Tolerance: Double);
var
  Actual: Double;
  Diff: Double;
begin
  Actual := FValue;
  Diff := Abs(Actual - Expected);
  if Diff > Tolerance then
    raise Fail('Expected %g to be close to %g (tolerance: %g, diff: %g)',
      [Actual, Expected, Tolerance, Diff]);
end;

procedure TExpect.ToBeWithinPercent(const Expected: Double; Percent: Double);
var
  Tolerance: Double;
begin
  Tolerance := Abs(Expected * Percent / 100);
  ToBeCloseTo(Expected, Tolerance);
end;

{ TExpectException }

constructor TExpectException.Create(const ACapture: TExceptionCapture);
begin
  FCapture := ACapture;
end;

procedure TExpectException.ToRaise;
begin
  if not FCapture.WasRaised then
    raise TExpect.Fail('Expected an exception to be raised');
end;

procedure TExpectException.ToRaiseType(ExceptClass: ExceptClass);
begin
  if not FCapture.WasRaised then
    raise TExpect.Fail('Expected exception of type %s but none was raised', [ExceptClass.ClassName]);
  if not FCapture.ExceptionClass.InheritsFrom(ExceptClass) then
    raise TExpect.Fail('Expected exception of type %s but got %s',
      [ExceptClass.ClassName, FCapture.ExceptionClass.ClassName]);
end;

procedure TExpectException.ToRaiseWithMessage(const Msg: string);
begin
  if not FCapture.WasRaised then
    raise TExpect.Fail('Expected exception with message "%s" but none was raised', [Msg]);
  if not ContainsText(FCapture.ExceptionMessage, Msg) then
    raise TExpect.Fail('Expected exception message to contain "%s" but got "%s"',
      [Msg, FCapture.ExceptionMessage]);
end;

procedure TExpectException.ToNotRaise;
begin
  if FCapture.WasRaised then
    raise TExpect.Fail('Expected no exception but got %s: %s',
      [FCapture.ExceptionClass.ClassName, FCapture.ExceptionMessage]);
end;

end.
