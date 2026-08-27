unit DAF.NNLog;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.IOUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  Daf.Extensions.Logging,
  Daf.Logging,
  DAF.Logging.Provider,
  Daf.NNLog.Configuration;

type
  {$M+}
  TTargetClass = class of TTarget;
  TTarget = class
  private
    FName: string;
    FLayout: string;
  protected
    function RenderEvent(Entry: TLogEntry): string;
  public
    const DefaultLayout = '${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}';
    constructor Create;virtual;
    procedure Write(const Entry: TLogEntry);virtual;abstract;
    property Layout: string read FLayout write FLayout;
    property Name: string read FName write FName;
  end;

  TLogRule = class
  private
    FName: string;
    FMinLevel: TLogLevel;
    FEnabled: Boolean;
    FIsFinal: Boolean;
    FMaxLevel: TLogLevel;
    FLevels: TArray<TLogLevel>;
    FWriteTo: TArray<TTarget>;
    FMatchAll: Boolean;
    FNameRegex: TRegEx;
    procedure SetName(const Value: string);
    function NameMatch(const Category: string): Boolean;
  public
    function Matches(const Category: string; const Level: TLogLevel): Boolean;
    procedure Log(const LogEntry: TLogEntry);
    property Name: string read FName write SetName;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property MaxLevel: TLogLevel read FMaxLevel write FMaxLevel;
    property Levels: TArray<TLogLevel> read FLevels write FLevels;
    property WriteTo: TArray<TTarget> read FWriteTo write FWriteTo;
    property Enabled: Boolean read FEnabled write FEnabled;
    property IsFinal: Boolean read FIsFinal write FIsFinal;
  end;
  {$M-}

  TNNLogLogger = class(TLogger)
  private
    FRules: TArray<TLogRule>;
  public
    constructor Create(const Category: string; const Rules: TArray<TLogRule>);
    function IsEnabled(const Level: TLogLevel): Boolean; override;
    procedure Log(const LogEntry: TLogEntry); override;
  end;

  TNNLogProvider = class(TInterfacedObject, ILoggerProvider)
  private
    FTargets: TArray<TTarget>;
    FRules: TArray<TLogRule>;
  public
    property Rules: TArray<TLogRule> read FRules;
    constructor Create(const Targets: TArray<TTarget>; const Rules: TArray<TLogRule>);
    destructor Destroy;override;
    function CreateLogger(const Category: string): ILogger;
  end;

implementation
uses
  DAF.NNLog.Layout;

{ TLogRule }

procedure TLogRule.Log(const LogEntry: TLogEntry);
begin
  for var Target in WriteTo do
    Target.Write(LogEntry);
end;

procedure TLogRule.SetName(const Value: string);
begin
  FName := Value;
  FMatchAll := (Value = '') or (Value = '*');
  if FMatchAll then
    Exit;
  // Un regex anclado a la categoria COMPLETA: sin ancla, IsMatch busca dentro y un patron
  // corto se aplicaria a todo lo que lo contenga. La caja la decide el patron con (?i); por
  // defecto distingue mayusculas.
  FNameRegex := TRegEx.Create('\A(?:' + Value + ')\z');
  // TRegEx compila de forma perezosa; se fuerza aqui para que un patron imposible falle al
  // construir la regla --y por tanto al cargar la configuracion--, no en el primer registro.
  FNameRegex.IsMatch('');
end;

function TLogRule.NameMatch(const Category: string): Boolean;
begin
  if FMatchAll then
    Exit(True);
  Result := FNameRegex.IsMatch(Category);
end;

function TLogRule.Matches(const Category: string; const Level: TLogLevel): Boolean;
begin
  // La categoria ya se emparejo al construir el logger (D3, TNNLogLogger.Create); aqui solo
  // quedan las comprobaciones de nivel, que es lo que se paga por linea.
  Result := (Level >= MinLevel) and (Level <= MaxLevel) and
    ((Length(Levels) = 0) or TArray.Contains<TLogLevel>(Levels, Level));
end;

{ TNNLogLogger }

constructor TNNLogLogger.Create(const Category: string; const Rules: TArray<TLogRule>);
begin
  inherited Create(Category);
  // La categoria se empareja UNA vez, aqui, no una por linea: el logger se queda solo con las
  // reglas que le aplican (D3). En el camino caliente no queda ni un regex.
  FRules := nil;
  for var Rule in Rules do
    if Rule.NameMatch(Category) then
      FRules := FRules + [Rule];
end;

function TNNLogLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  // EXACTAMENTE el mismo predicado que la escritura de abajo (D3): las reglas ya estan filtradas por
  // categoria en Create, asi que aqui solo queda "hay alguna regla activa que acepte este nivel".
  // Si usara un predicado propio podria cerrarse sobre una linea que la escritura si aceptaria.
  for var Rule in FRules do
    if Rule.Enabled and Rule.Matches(Category, Level) then
      Exit(True);
  Result := False;
end;

procedure TNNLogLogger.Log(const LogEntry: TLogEntry);
var
  Rule: TLogRule;
begin
  for Rule in FRules do
  begin
    if Rule.Enabled and Rule.Matches(LogEntry.Category, LogEntry.Level) then
    begin
      Rule.Log(LogEntry);
      if Rule.IsFinal then
        Break;
    end;
  end;
end;

{ TNNLogProvider }

constructor TNNLogProvider.Create(const Targets: TArray<TTarget>; const Rules: TArray<TLogRule>);
begin
  inherited Create;
  FTargets := Targets;
  FRules := Rules;
end;

function TNNLogProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TNNLogLogger.Create(Category, FRules);
end;

destructor TNNLogProvider.Destroy;
begin
  for var Item in FTargets do
    Item.Free;
  for var Item in FRules do
    Item.Free;
  inherited;
end;

{ TTarget }

constructor TTarget.Create;
begin
  inherited Create;
  FLayout := DefaultLayout;
end;

function TTarget.RenderEvent(Entry: TLogEntry): string;
begin
  Result := TLogLayoutEngine.ResolveLayout(Layout, Entry);
end;

end.

