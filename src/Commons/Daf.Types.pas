unit Daf.Types;

interface

uses
  System.TypInfo,
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>
  ///   A value that may be given either directly or as a thunk, and that is
  ///   re-evaluated on <b>every</b> read.
  /// </summary>
  /// <remarks>
  ///   Named for what it does, and deliberately not for what it resembles.
  ///   It is <i>not</i> .NET's <c>Expression&lt;TDelegate&gt;</c>, which is an
  ///   inspectable expression tree — that world is <c>IExpr</c> /
  ///   <c>TSpecification&lt;T&gt;</c> in <c>Daf.Specification</c>, and calling
  ///   this one an expression claimed a correspondence that does not hold.
  ///   Nor is it <c>Lazy&lt;T&gt;</c>, which memoizes: here the thunk runs
  ///   again on each read, which is exactly what macro resolution wants.
  /// </remarks>
  TDeferred<T> = record
  private
    FFunc: TFunc<T>;
  public
    class operator Initialize(out Dest: TDeferred<T>);
    class operator Implicit(const Value: T): TDeferred<T>;
    class operator Implicit(const Func: TFunc<T>): TDeferred<T>;
    class operator Implicit(const Deferred: TDeferred<T>): T;
  end;

  TMacroExpression = TDeferred<string>;
  TMacrosList = class
  strict private
    FExpresions: TOrderedDictionary<string, TMacroExpression>;
  private
    function GetKeys(Index: Integer):string;
    function GetValueFromIndex(Index: Integer):string;
    function GetCount: Integer;
    function IndexOf(const Key:string): Integer;
    function GetExpression(const Name:string): TMacroExpression;
    procedure SetExpression(const Name:string; Value: TMacroExpression);
  public
    constructor Create;
    destructor Destroy; override;
    function ResolveMacros(const Source: string; const MaxRecursionLevel: Integer = 100): string;
    function Contains(const Key:string): Boolean;
    property Count: Integer read GetCount;
    property Keys[Index: Integer]:string read GetKeys;
    property Value[const Name:string]: TMacroExpression read GetExpression write SetExpression;default;
    property ValueFromIndex[Index: Integer]:string read GetValueFromIndex;
  end;

  /// <summary>
  ///   Clase para leer y almacenar las variables de entorno en un diccionario.
  ///   Llama a Refresh() para actualizar la lista en cualquier momento.
  ///   Usa
  /// </summary>
  TEnvVars = class
  private
    FBuffer: PWideChar;
    FVars: TDictionary<string, string>;
    function GetBuffer: PWideChar;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Lee todas las variables de entorno desde el sistema
    ///   y las almacena en FVars.
    /// </summary>
    procedure Refresh;
    procedure ClearBuffer;
    procedure WriteTo(var Buffer: PWideChar);

    /// <summary>
    ///   Devuelve el diccionario con las variables de entorno recopiladas.
    ///   Si deseas recargarlas, llama primero a Refresh.
    /// </summary>
    property Vars: TDictionary<string, string> read FVars;
    property Buffer: PWideChar read GetBuffer;
  end;


  Coalesce = record
    /// Returns the first arg not nil or nil
    class function From<T: IInterface>(const Values: TArray<T>): T; overload; static;
    class function From<T: IInterface>(const Value1, Value2: T): T; overload; static;
  end;

  Debugger = record
    class procedure Write(const Text:string); overload; static;
    class procedure Write(const FmtStr:string; Args: array of const); overload; static;
    class function Enabled: Boolean; static;
    class function CurrentProcessId: Cardinal; static;
    class function CurrentThreadId: Cardinal; static;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.Messages,
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
{$IFDEF MACOS}
{$ENDIF MACOS}
{$IFDEF ANDROID}
{$ENDIF ANDROID}
{$IFDEF LINUX}
{$ENDIF LINUX}
{$ENDIF POSIX}
  System.RegularExpressions,
  System.Generics.Defaults,
  System.StrUtils;

{ TDeferred<T> }

class operator TDeferred<T>.Implicit(const Value: T): TDeferred<T>;
begin
  Result.FFunc := function: T
    begin
      Result := Value
    end;
end;

class operator TDeferred<T>.Implicit(const Func: TFunc<T>): TDeferred<T>;
begin
  Result.FFunc := Func;
end;

class operator TDeferred<T>.Implicit(const Deferred: TDeferred<T>): T;
begin
  Result := Deferred.FFunc();
end;

class operator TDeferred<T>.Initialize(out Dest: TDeferred<T>);
begin
  Dest.FFunc := function: T
    begin
      Result := Default(T);
    end;
end;

{ TEnvVars }

constructor TEnvVars.Create;
begin
  inherited Create;
  FVars := TDictionary<string,string>.Create(TIStringComparer.Ordinal);
  Refresh;
end;

destructor TEnvVars.Destroy;
begin
  ClearBuffer;
  FVars.Free;
  inherited;
end;

function TEnvVars.GetBuffer: PWideChar;
begin
  WriteTo(FBuffer);
  Result := FBuffer;
end;

procedure TEnvVars.ClearBuffer;
begin
  FreeMem(FBuffer);
  FBuffer := nil;
end;

procedure TEnvVars.WriteTo(var Buffer: PWideChar);
begin
  var Temp := '';
  for var Pair in Vars do
    Temp := Temp + Pair.Key + '=' + Pair.Value + #0;

  Temp := Temp + #0;

  // Temp continas #0 in the middle, cannot use StrNew or similars
  var Len := 1 + Length(Temp);
  GetMem(Buffer, Len * SizeOf(Char));
  Move(PByte(Temp)^, Buffer^, Len * SizeOf(Char));
end;

procedure TEnvVars.Refresh;
begin
  ClearBuffer;
  FVars.Clear;

  FBuffer := GetEnvironmentStringsW;

  if FBuffer = nil then
    Exit;

  try
    var EnvBlock := FBuffer;
    while EnvBlock^ <> #0 do
    begin
      var EnvLine := string(EnvBlock);

      var i := EnvLine.IndexOf('=');
      if i > 0 then
      begin
        var Key := EnvLine.Substring(0, i);
        var Value := EnvLine.Substring(i + 1);
        FVars.AddOrSetValue(Key.ToUpper, Value);
      end;

      Inc(EnvBlock, Length(EnvLine) + 1);
    end;
  finally
    FreeEnvironmentStringsW(FBuffer);
    FBuffer := nil;
  end;
end;

{ TMacrosList }

function TMacrosList.Contains(const Key:string): Boolean;
begin
  Result := IndexOf(Key)>= 0;
end;

constructor TMacrosList.Create;
begin
  inherited;
  FExpresions := TOrderedDictionary<string, TMacroExpression>.Create;
end;

destructor TMacrosList.Destroy;
begin
  FExpresions.Free;
  inherited;
end;

function TMacrosList.GetCount: Integer;
begin
  Result := FExpresions.Count;
end;

function TMacrosList.GetKeys(Index: Integer):string;
begin
  Result := FExpresions.KeyList[Index];
end;

function TMacrosList.IndexOf(const Key:string): Integer;
begin
  Result := FExpresions.IndexOf(Key);
end;

function TMacrosList.ResolveMacros(const Source:string; const MaxRecursionLevel: Integer = 100):string;
begin
  if Source.IsEmpty or  (MaxRecursionLevel = 0) then Exit(Source);

  Result := Source;
  var Matches := TRegEx.Matches(Source, '\$\{(.*?)\}');
  if Matches.Count = 0  then Exit;

  for var Match in Matches do
  begin
    var Token := Match.Groups[1].Value;
    var Resolved := Self[Token];
    Result := Result.Replace(Match.Value, Resolved);
  end;
  Result := ResolveMacros(Result, MaxRecursionLevel - 1);
end;

function TMacrosList.GetValueFromIndex(Index: Integer):string;
begin
  Result := FExpresions.ValueList[Index];
end;

function TMacrosList.GetExpression(const Name:string): TMacroExpression;
begin
  var idx := IndexOf(Name);
  if idx < 0 then
    Result := ''
  else
    Result := ValueFromIndex[idx];
end;

procedure TMacrosList.SetExpression(const Name:string; Value: TMacroExpression);
begin
  FExpresions.AddOrSetValue(Name, Value);
end;

{ Coalesce }

class function Coalesce.From<T>(const Values: TArray<T>): T;
begin
  for var Value in Values do
    if Value <> nil then
      Exit(Value);
  Result := nil;
end;

class function Coalesce.From<T>(const Value1, Value2: T): T;
begin
  Result := From<T>([Value1, Value2]);
end;

{ Debug }

class function Debugger.CurrentProcessId: Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := GetCurrentProcessId;
{$ELSE}
  Result := 0;
{$ENDIF MSWINDOWS}
end;

class function Debugger.CurrentThreadId: Cardinal;
begin
  Result := TThread.Current.ThreadID;
end;

class function Debugger.Enabled: Boolean;
begin
{$WARN SYMBOL_PLATFORM OFF}
  Result := System.DebugHook > 0;
{$WARN SYMBOL_PLATFORM ON}
end;

class procedure Debugger.Write(const Text:string);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(Text));
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
{$IFDEF MACOS}
{$ENDIF MACOS}
{$IFDEF ANDROID}
{$ENDIF ANDROID}
{$IFDEF LINUX}
{$ENDIF LINUX}
{$ENDIF POSIX}
end;

class procedure Debugger.Write(const FmtStr:string; Args: array of const);
begin
  Write(Format(FmtStr, Args));
end;

end.
