unit CustomProvider.Pipeline;

{
  Un proveedor de log escrito desde fuera del framework, que es lo que
  Daf.Logging.Abstractions promete a quien quiera el suyo. Dos, en realidad: uno que
  escribe a consola con un nivel minimo, y otro que solo cuenta, para que el sample
  pueda enseñar una entrada llegando a dos destinos.

  El servicio de abajo esta aqui y no en el .dpr por obligacion del contenedor: un tipo
  declarado en el .dpr no esta en la seccion interface de ninguna unit, y resolverlo
  aborta en ejecucion con ENonPublicType.
}

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.Extensions.Logging,
  Daf.Logging;

type
  /// <summary>
  ///   Un logger con nivel minimo. Hereda de TLogger, asi que recibe hecha toda la
  ///   superficie de ILogger --las cuatro sobrecargas por nivel, los ambitos-- y solo
  ///   pone lo suyo: donde escribe, y hasta donde.
  /// </summary>
  TPrefixLogger = class(TLogger)
  strict private
    FPrefix: string;
    FMinLevel: TLogLevel;
  public
    constructor Create(const ACategory, APrefix: string; AMinLevel: TLogLevel);
    /// La puerta. Contestarla de verdad es lo que permite a un llamante saltarse la
    /// construccion de unos argumentos que cuestan mas que la propia linea.
    function IsEnabled(const Level: TLogLevel): Boolean; override;
    procedure Log(const Entry: TLogEntry); override;
  end;

  TPrefixLoggerProvider = class(TInterfacedObject, ILoggerProvider)
  strict private
    FPrefix: string;
    FMinLevel: TLogLevel;
  public
    constructor Create(const APrefix: string; AMinLevel: TLogLevel);
    function CreateLogger(const Category: string): ILogger;
  end;

  /// <summary>
  ///   Un segundo destino: no escribe, solo cuenta. Contesta la puerta con su propio
  ///   minimo, y eso importa mas de lo que parece -- el logger compuesto dice que si
  ///   cuando lo dice ALGUNO de sus destinos, y un destino que no sobreescribe
  ///   IsEnabled contesta True. Con uno solo asi, la puerta del pipeline no cierra
  ///   nunca.
  /// </summary>
  TCountingLogger = class(TLogger)
  strict private
    FMinLevel: TLogLevel;
  public
    constructor Create(const ACategory: string; AMinLevel: TLogLevel);
    function IsEnabled(const Level: TLogLevel): Boolean; override;
    procedure Log(const Entry: TLogEntry); override;
  end;

  TCountingLoggerProvider = class(TInterfacedObject, ILoggerProvider)
  strict private
    FMinLevel: TLogLevel;
  public
    constructor Create(AMinLevel: TLogLevel);
    function CreateLogger(const Category: string): ILogger;
  end;

  /// <summary>
  ///   Un servicio que pide su registrador por constructor. Pide ILogger&lt;T&gt; y no
  ///   ILogger, asi que le llega uno que YA lleva su categoria: nadie escribe el nombre
  ///   de la clase en ningun sitio.
  /// </summary>
  IPayroll = interface(IInvokable)
    ['{4B0E7F2A-91C6-4D38-8A57-E2B40D169C83}']
    procedure Run(Employees: Integer);
  end;

  TPayrollService = class(TInterfacedObject, IPayroll)
  strict private
    FLog: ILogger<TPayrollService>;
  public
    constructor Create(const Log: ILogger<TPayrollService>);
    procedure Run(Employees: Integer);
  end;

var
  /// Cuantas entradas vio el destino contador, y cuantas veces se llego a construir el
  /// argumento caro. Globales porque el sample los lee desde el .dpr.
  Counted: Integer = 0;
  ExpensiveBuilds: Integer = 0;

implementation

{ TPrefixLogger }

constructor TPrefixLogger.Create(const ACategory, APrefix: string; AMinLevel: TLogLevel);
begin
  inherited Create(ACategory);
  FPrefix := APrefix;
  FMinLevel := AMinLevel;
end;

function TPrefixLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  Result := Level >= FMinLevel;
end;

procedure TPrefixLogger.Log(const Entry: TLogEntry);
begin
  if not IsEnabled(Entry.Level) then
    Exit;
  // Entry.Message no lleva el ambito: viaja aparte en Entry.Scope, ya con su separador delante.
  Writeln(Format('  %s [%-5s] %-42s %s%s',
    [FPrefix, Entry.Level.ToString, Entry.Category, Entry.Message, Entry.Scope]));
end;

{ TPrefixLoggerProvider }

constructor TPrefixLoggerProvider.Create(const APrefix: string; AMinLevel: TLogLevel);
begin
  inherited Create;
  FPrefix := APrefix;
  FMinLevel := AMinLevel;
end;

function TPrefixLoggerProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TPrefixLogger.Create(Category, FPrefix, FMinLevel);
end;

{ TCountingLogger }

constructor TCountingLogger.Create(const ACategory: string; AMinLevel: TLogLevel);
begin
  inherited Create(ACategory);
  FMinLevel := AMinLevel;
end;

function TCountingLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  Result := Level >= FMinLevel;
end;

procedure TCountingLogger.Log(const Entry: TLogEntry);
begin
  Inc(Counted);
end;

{ TCountingLoggerProvider }

constructor TCountingLoggerProvider.Create(AMinLevel: TLogLevel);
begin
  inherited Create;
  FMinLevel := AMinLevel;
end;

function TCountingLoggerProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TCountingLogger.Create(Category, FMinLevel);
end;

{ TPayrollService }

constructor TPayrollService.Create(const Log: ILogger<TPayrollService>);
begin
  inherited Create;
  FLog := Log;
end;

procedure TPayrollService.Run(Employees: Integer);
begin
  FLog.LogInformation('Nomina de {Employees} empleado(s)', [TValue.From<Integer>(Employees)]);
end;

end.
