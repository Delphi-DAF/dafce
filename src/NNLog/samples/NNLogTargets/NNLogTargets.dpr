program NNLogTargets;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Hosting,
  Daf.NNLog,
  Daf.NNLog.Configuration,
  Daf.NNLog.Loader,
  // El cargador resuelve un destino por RTTI, y el RTTI solo ve lo que el enlazador
  // metio: sin nombrar aqui la unit del destino de consola, 'Console' no se encuentra
  // y la carga aborta con "Unknown target type".
  Daf.NNLog.Targets.Console,
  Daf.NNLog.Targets.Audit in 'Daf.NNLog.Targets.Audit.pas';

type
  /// El canal de arranque reparte una linea YA formada, asi que un destino suyo son dos
  /// metodos: si puede escribir ahora, y escribir. DAF trae los de depuracion, error
  /// estandar y fichero; este va a la salida normal para que el sample se lea seguido.
  TStdOutBootstrapSink = class(TInterfacedObject, IBootstrapSink)
  public
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

function TStdOutBootstrapSink.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TStdOutBootstrapSink.Write(const Line: string);
begin
  Writeln('  >> ', Line);
end;

procedure Section(const Title: string);
begin
  Writeln;
  Writeln('== ', Title);
end;

/// El entorno que el cargador necesita para los tokens que salen de el
/// (${environment}, ${ApplicationName}, ${contentRootPath}...).
function BuildEnvironment: IHostEnvironment;
begin
  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  Result := THostEnvironment.Create(Builder.Build as IConfiguration) as IHostEnvironment;
end;

/// Unos ajustes escritos a mano. El layout va sin ${timestamp} a proposito, para que la
/// salida del sample sea la misma en cada pasada.
function CodeSettings: TNNLogConfiguration;
begin
  Result := TNNLogConfiguration.Create;

  var Con := TTargetConfig.Create;
  Con.AddOrSetValue('TargetType', 'Console');
  Con.AddOrSetValue('Name', 'console');
  Con.AddOrSetValue('Layout', '  [${level}] ${category} | ${message}');
  Con.AddOrSetValue('UseColors', 'false');
  Result.Targets.Add('console', Con);

  var Aud := TTargetConfig.Create;
  Aud.AddOrSetValue('TargetType', 'Audit');
  Aud.AddOrSetValue('Name', 'audit');
  // ${message} es el mensaje sin el ambito abierto, y ${scope} lo pone detras. Y ${nada}
  // se queda sin sustituir a proposito, que es lo que hace visible un token mal escrito en
  // vez de dejarlo en blanco.
  Aud.AddOrSetValue('Layout', '${category} pedido=${event-properties:OrderId} | ${message}${scope} ${nada}');
  // Prefix es propiedad escribible de TAuditTarget, asi que el cargador la asigna.
  Aud.AddOrSetValue('Prefix', 'AUD');
  Result.Targets.Add('audit', Aud);

  // La de auditoria va PRIMERA y es final: lo que case con ella no llega a la siguiente.
  // El orden de las reglas es el orden del array.
  var RAudit := TLogRuleConfig.Create;
  RAudit.Name := 'Acme\.Audit';
  RAudit.MinLevel := 'Information';
  RAudit.WriteTo := 'audit';
  RAudit.IsFinal := True;

  var RApp := TLogRuleConfig.Create;
  RApp.Name := 'Acme\..*';
  RApp.MinLevel := 'Warning';
  RApp.WriteTo := 'console';

  Result.Rules := [RAudit, RApp];
end;

/// Los MISMOS ajustes de consola, leidos de una seccion en vez de escritos a mano. Es la
/// via de una aplicacion con fichero de configuracion.
function SectionSettings: IConfiguration;
begin
  var Dict := TDictionary<string, string>.Create;
  Dict.AddOrSetValue('Targets:console:TargetType', 'Console');
  Dict.AddOrSetValue('Targets:console:Name', 'console');
  Dict.AddOrSetValue('Targets:console:Layout', '  [${level}] ${category} | ${message}');
  Dict.AddOrSetValue('Targets:console:UseColors', 'false');
  Dict.AddOrSetValue('Rules:0:Name', 'Acme\..*');
  Dict.AddOrSetValue('Rules:0:MinLevel', 'Warning');
  Dict.AddOrSetValue('Rules:0:WriteTo', 'console');

  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  // La fuente en memoria pasa a POSEER el diccionario; no volver a liberarlo.
  MemoryConfig.AddCollection(Builder, Dict);
  Result := Builder.Build as IConfiguration;
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  // El canal de arranque no trae destinos puestos: es donde informa el logging cuando lo
  // que falla es el logging, y adonde va lo decide quien despliega.
  BootstrapLog.AddSink(TStdOutBootstrapSink.Create);

  var Env := BuildEnvironment;

  var Settings := CodeSettings;
  var Provider: ILoggerProvider;
  try
    Provider := TNNLogLoader.BuildProvider(Env, Settings);
  finally
    // El proveedor no se queda con los ajustes: ya construyo sus destinos y sus reglas.
    Settings.Free;
  end;

  Section('El nombre de una regla es una expresion regular sobre la categoria entera');
  Provider.CreateLogger('Acme.Orders').LogWarning('stock por debajo del minimo');
  Provider.CreateLogger('Otros.Cosas').LogWarning('esta categoria no casa con ninguna regla');
  Provider.CreateLogger('Acme.Orders').LogInformation('esto queda bajo el minLevel de su regla');

  Section('Una regla final corta el recorrido');
  // Acme.Audit casa con las dos reglas, pero la primera es final: no llega a consola.
  Provider.CreateLogger('Acme.Audit').LogInformation('pago aceptado {OrderId}',
    [TValue.From<Integer>(4711)]);

  Section('Los tokens del layout, y uno que no existe');
  var Audit := Provider.CreateLogger('Acme.Audit');
  Audit.BeginScope('lote=12',
    procedure
    begin
      Audit.LogInformation('cierre de lote {OrderId}', [TValue.From<Integer>(99)]);
    end);

  Section('Un destino que no puede escribir ya no tumba a quien registraba');
  TAuditTarget.FailOnce := True;
  Provider.CreateLogger('Acme.Audit').LogInformation('pago aceptado {OrderId}',
    [TValue.From<Integer>(4712)]);
  Writeln('  el programa sigue vivo, y la siguiente linea si se escribe:');
  Provider.CreateLogger('Acme.Audit').LogInformation('pago aceptado {OrderId}',
    [TValue.From<Integer>(4713)]);

  Section('Los mismos ajustes, leidos de una seccion');
  var FromSection := TNNLogLoader.BuildProvider(Env, SectionSettings);
  FromSection.CreateLogger('Acme.Orders').LogWarning('stock por debajo del minimo');

  Writeln;
end.
