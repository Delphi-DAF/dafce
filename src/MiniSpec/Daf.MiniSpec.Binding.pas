unit Daf.MiniSpec.Binding;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.RegularExpressions,
  System.Generics.Collections;

type
  TStepKind = (skGiven, skWhen, skThen);

  /// <summary>
  /// Marks a method as a Given step binding with a regex pattern.
  /// </summary>
  GivenAttribute = class(TCustomAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern: string);
    property Pattern: string read FPattern;
  end;

  /// <summary>
  /// Marks a method as a When step binding with a regex pattern.
  /// </summary>
  WhenAttribute = class(TCustomAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern: string);
    property Pattern: string read FPattern;
  end;

  /// <summary>
  /// Marks a method as a Then step binding with a regex pattern.
  /// </summary>
  ThenAttribute = class(TCustomAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern: string);
    property Pattern: string read FPattern;
  end;

  EBindingError = class(Exception);

  TStepBinding = record
    Kind: TStepKind;
    Pattern: string;
    Regex: TRegEx;
    Method: TRttiMethod;
    StepsClass: TClass;
  end;

  /// <summary>
  /// Service for registering and resolving step bindings.
  /// Steps are methods marked with [Given], [When], or [Then] attributes.
  /// The pattern in the attribute is a regex that matches the step text.
  /// Capture groups in the regex are passed as arguments to the method.
  /// </summary>
  TBindingService = class
  private
    FBindings: TList<TStepBinding>;
    FInstances: TDictionary<TClass, TObject>;
    FRttiContext: TRttiContext;
    function ConvertCapture(const Value: string; ParamType: TRttiType): TValue;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// Registers all step bindings from a steps class.
    /// The class must have methods marked with [Given], [When], or [Then] attributes.
    /// </summary>
    procedure RegisterSteps<T: class, constructor>;
    /// <summary>
    /// Finds a binding that matches the given step kind and text.
    /// Returns True if found, with the binding and captured values.
    /// </summary>
    function FindBinding(Kind: TStepKind; const Text: string;
      out Binding: TStepBinding; out Captures: TArray<string>): Boolean;
    /// <summary>
    /// Invokes a binding with the given world object and captured values.
    /// </summary>
    procedure Invoke(const Binding: TStepBinding; World: TObject;
      const Captures: TArray<string>);
    /// <summary>
    /// Clears all registered bindings and cached instances.
    /// </summary>
    procedure Clear;
    /// <summary>
    /// Returns the number of registered bindings.
    /// </summary>
    function Count: Integer;
  end;

/// <summary>
/// Returns the global binding service singleton.
/// </summary>
function Bindings: TBindingService;

implementation

var
  FInstance: TBindingService;

function Bindings: TBindingService;
begin
  if not Assigned(FInstance) then
    FInstance := TBindingService.Create;
  Result := FInstance;
end;

{ GivenAttribute }

constructor GivenAttribute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

{ WhenAttribute }

constructor WhenAttribute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

{ ThenAttribute }

constructor ThenAttribute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

{ TBindingService }

constructor TBindingService.Create;
begin
  inherited;
  FBindings := TList<TStepBinding>.Create;
  FInstances := TDictionary<TClass, TObject>.Create;
  FRttiContext := TRttiContext.Create;
end;

destructor TBindingService.Destroy;
begin
  Clear;
  FBindings.Free;
  FInstances.Free;
  FRttiContext.Free;
  inherited;
end;

procedure TBindingService.RegisterSteps<T>;
var
  RttiType: TRttiType;
  Method: TRttiMethod;
  Attr: TCustomAttribute;
  Binding: TStepBinding;
begin
  RttiType := FRttiContext.GetType(TypeInfo(T));
  for Method in RttiType.GetMethods do
  begin
    for Attr in Method.GetAttributes do
    begin
      Binding := Default(TStepBinding);
      Binding.Method := Method;
      Binding.StepsClass := T;

      if Attr is GivenAttribute then
      begin
        Binding.Kind := skGiven;
        Binding.Pattern := GivenAttribute(Attr).Pattern;
      end
      else if Attr is WhenAttribute then
      begin
        Binding.Kind := skWhen;
        Binding.Pattern := WhenAttribute(Attr).Pattern;
      end
      else if Attr is ThenAttribute then
      begin
        Binding.Kind := skThen;
        Binding.Pattern := ThenAttribute(Attr).Pattern;
      end
      else
        Continue;

      Binding.Regex := TRegEx.Create('^' + Binding.Pattern + '$', [roIgnoreCase]);
      FBindings.Add(Binding);
    end;
  end;
end;

function TBindingService.FindBinding(Kind: TStepKind; const Text: string;
  out Binding: TStepBinding; out Captures: TArray<string>): Boolean;
var
  B: TStepBinding;
  Match: TMatch;
  I: Integer;
begin
  Result := False;
  for B in FBindings do
  begin
    if B.Kind <> Kind then
      Continue;
    Match := B.Regex.Match(Text);
    if Match.Success then
    begin
      Binding := B;
      SetLength(Captures, Match.Groups.Count - 1);
      for I := 1 to Match.Groups.Count - 1 do
        Captures[I - 1] := Match.Groups[I].Value;
      Exit(True);
    end;
  end;
end;

function TBindingService.ConvertCapture(const Value: string; ParamType: TRttiType): TValue;
begin
  case ParamType.TypeKind of
    tkInteger:
      Result := TValue.From(StrToInt(Value));
    tkInt64:
      Result := TValue.From(StrToInt64(Value));
    tkFloat:
      Result := TValue.From(StrToFloat(Value));
    tkString, tkUString, tkLString, tkWString:
      Result := TValue.From(Value);
    tkEnumeration:
      if ParamType.Handle = TypeInfo(Boolean) then
        Result := TValue.From(SameText(Value, 'true') or (Value = '1'))
      else
        raise EBindingError.CreateFmt('Unsupported enum type: %s', [ParamType.Name]);
  else
    raise EBindingError.CreateFmt('Unsupported parameter type: %s', [ParamType.Name]);
  end;
end;

procedure TBindingService.Invoke(const Binding: TStepBinding; World: TObject;
  const Captures: TArray<string>);
var
  StepsInstance: TObject;
  Args: TArray<TValue>;
  Params: TArray<TRttiParameter>;
  I: Integer;
begin
  // Obtener o crear instancia de la clase de Steps
  if not FInstances.TryGetValue(Binding.StepsClass, StepsInstance) then
  begin
    StepsInstance := Binding.StepsClass.Create;
    FInstances.Add(Binding.StepsClass, StepsInstance);
  end;

  // Construir array de argumentos
  Params := Binding.Method.GetParameters;

  // Validar cantidad de parámetros
  if Length(Params) <> Length(Captures) + 1 then
    raise EBindingError.CreateFmt(
      'Parameter count mismatch for "%s": expected %d, got %d captures',
      [Binding.Pattern, Length(Params) - 1, Length(Captures)]);

  SetLength(Args, Length(Params));

  // Primer argumento siempre es el World
  Args[0] := TValue.From(World);

  // Resto son los captures convertidos
  for I := 1 to High(Params) do
    Args[I] := ConvertCapture(Captures[I - 1], Params[I].ParamType);

  // Invocar
  Binding.Method.Invoke(StepsInstance, Args);
end;

procedure TBindingService.Clear;
var
  Obj: TObject;
begin
  for Obj in FInstances.Values do
    Obj.Free;
  FInstances.Clear;
  FBindings.Clear;
end;

function TBindingService.Count: Integer;
begin
  Result := FBindings.Count;
end;

initialization

finalization
  FInstance.Free;

end.
