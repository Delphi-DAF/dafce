unit Daf.NNLog;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.TypInfo,
  System.IOUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  Daf.Extensions.Logging,
  Daf.Logging,
  Daf.Logging.Provider,
  Daf.NNLog.Configuration;

type
  {$M+}
  TTargetClass = class of TTarget;
  TTarget = class
  private
    FName: string;
    FLayout: string;
    // Por instancia, no global: dos destinos no comparten estado, y hacerles esperar el uno por el
    // otro cambiaria una corrupcion por un cuello de botella.
    FLock: TCriticalSection;
    // Mayor que cero mientras dura una racha de fallos: el primero se informa, los demas solo se cuentan.
    FLostLines: Integer;
  protected
    function RenderEvent(Entry: TLogEntry): string;
    // Lo que implementa cada destino. La escritura publica no es virtual: el candado no es cosa
    // de quien escribe un destino, porque el recurso compartido es el destino mismo -- una regla
    // guarda sus destinos y todos los loggers despachan por ellos.
    procedure DoWrite(const Entry: TLogEntry); virtual; abstract;
    /// <summary>Como se nombra el destino en un aviso: su Name, o su clase si no tiene.</summary>
    function DisplayName: string;
  public
    const DefaultLayout = '${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}';
    constructor Create;virtual;
    destructor Destroy; override;
    procedure Write(const Entry: TLogEntry);
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
    constructor Create;
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
  Daf.NNLog.Layout;

{ TLogRule }

constructor TLogRule.Create;
begin
  inherited Create;
  // Los mismos tres valores que el cargador escribe a mano antes de entregar la regla, y estan aqui
  // porque son del TIPO. Sin constructor, una regla construida en codigo --y TLogRule es publico--
  // nacia con el cero de TObject, que no es "desactivada por defecto": es una regla que ni siquiera
  // puede contestar. Al no pasar por el setter de Name, FMatchAll se queda en False y el regex de
  // categoria sin construir, asi que NameMatch deriva un TRegEx vacio y revienta.
  Name := EmptyStr;
  Enabled := True;
  MinLevel := Low(TLogLevel);
  MaxLevel := Pred(High(TLogLevel));
end;

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
  // La categoria ya se emparejo al construir el logger (TNNLogLogger.Create); aqui solo
  // quedan las comprobaciones de nivel, que es lo que se paga por linea.
  Result := (Level >= MinLevel) and (Level <= MaxLevel) and
    ((Length(Levels) = 0) or TArray.Contains<TLogLevel>(Levels, Level));
end;

{ TNNLogLogger }

constructor TNNLogLogger.Create(const Category: string; const Rules: TArray<TLogRule>);
begin
  inherited Create(Category);
  // La categoria se empareja UNA vez, aqui, no una por linea: el logger se queda solo con las
  // reglas que le aplican. En el camino caliente no queda ni un regex.
  FRules := nil;
  for var Rule in Rules do
    if Rule.NameMatch(Category) then
      FRules := FRules + [Rule];
end;

function TNNLogLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  // EXACTAMENTE el mismo predicado que la escritura de abajo: las reglas ya estan filtradas por
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
  FLock := TCriticalSection.Create;
end;

destructor TTarget.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TTarget.DisplayName: string;
begin
  if FName.IsEmpty then
    Result := ClassName
  else
    Result := FName;
end;

procedure TTarget.Write(const Entry: TLogEntry);
begin
  FLock.Enter;
  try
    try
      DoWrite(Entry);
    except
      // Un logger que lanza convierte un problema de observabilidad en una caida, y se auto-envenena:
      // el manejador de errores de quien registra tambien registra, asi que vuelve a fallar dentro
      // del camino de error. El fallo no se pierde -- sale por el canal de arranque, que es el que
      // existe para cuando el logging es justo lo que no funciona.
      on E: Exception do
      begin
        if FLostLines = 0 then
          BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" no pudo escribir', [DisplayName]));
        Inc(FLostLines);
        Exit;
      end;
    end;
    if FLostLines > 0 then
    begin
      var Lost := Format('%d lineas', [FLostLines]);
      if FLostLines = 1 then
        Lost := '1 linea';
      // Warning y no Information: el canal de arranque descarta lo que esta por debajo.
      BootstrapLog.LoggerFor(ClassName).LogWarning(Format('el destino "%s" vuelve a escribir; perdio %s', [DisplayName, Lost]));
      FLostLines := 0;
    end;
  finally
    FLock.Leave;
  end;
end;

function TTarget.RenderEvent(Entry: TLogEntry): string;
begin
  Result := TLogLayoutEngine.ResolveLayout(Layout, Entry);
end;

end.

