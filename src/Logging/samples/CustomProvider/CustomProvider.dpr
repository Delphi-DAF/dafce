program CustomProvider;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  System.Rtti,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  Daf.DependencyInjection,
  Daf.Logging,
  Daf.Logging.Builder,
  CustomProvider.Pipeline in 'CustomProvider.Pipeline.pas';

procedure Section(const Title: string);
begin
  Writeln;
  Writeln('== ', Title);
end;

/// El argumento caro del ejemplo de IsEnabled: un volcado que cuesta mas que la linea.
function ExpensiveDump: string;
begin
  Inc(ExpensiveBuilds);
  Result := 'volcado-de-estado';
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  var Services: IServiceCollection := TServiceCollection.Create;
  Services.AddSingleton<IPayroll, TPayrollService>;

  // Dos proveedores en el mismo pipeline. El de consola descarta por debajo de
  // Information; el contador se queda con todo lo que le llegue.
  AddLogging(Services,
    procedure(Builder: ILoggingBuilder)
    begin
      Builder.AddProvider(TPrefixLoggerProvider.Create('con', TLogLevel.Information));
      Builder.AddProvider(TCountingLoggerProvider.Create(TLogLevel.Information));
    end);

  var Provider := Services.BuildServiceProvider;
  try
    var Factory := Provider.GetRequiredService<ILoggerFactory>;
    var Log := Factory.CreateLogger('Acme.Facturacion');

    Section('Una plantilla con huecos, y sus pares clave-valor');
    Log.LogInformation('Factura {Number} por {Amount} EUR',
      [TValue.From<Integer>(1042), TValue.From<Currency>(350.5)]);
    var State := TLogState.Create('Factura {Number} por {Amount} EUR',
      [TValue.From<Integer>(1042), TValue.From<Currency>(350.5)]);
    for var Pair in State.KeyValuePairs do
      Writeln(Format('    %-8s = %s', [Pair.Key, Pair.Value.ToString]));

    Section('Un ambito viaja con todo lo que se escriba dentro');
    Log.BeginScope('peticion=7',
      procedure
      begin
        Log.LogWarning('el cliente no tiene direccion');
        Log.BeginScope('reintento=2',
          procedure
          begin
            Log.LogError('no se pudo enviar');
          end);
      end);

    Section('IsEnabled es una puerta, no un adorno');
    // Los DOS proveedores descartan por debajo de Information, asi que la puerta puede
    // cerrar. Basta con que uno solo de ellos no sepa contestarla para que se quede
    // abierta: la respuesta del compuesto es la de cualquiera que diga que si.
    if Log.IsEnabled(TLogLevel.Debug) then
      Log.LogDebug('estado: {Dump}', [TValue.From<string>(ExpensiveDump)]);
    if Log.IsEnabled(TLogLevel.Error) then
      Log.LogError('estado: {Dump}', [TValue.From<string>(ExpensiveDump)]);
    Writeln(Format('    el argumento caro se construyo %d vez/veces de 2 intentos',
                   [ExpensiveBuilds]));

    Section('Un servicio recibe un registrador que ya lleva su categoria');
    Provider.GetRequiredService<IPayroll>.Run(12);

    Section('Una entrada, dos destinos');
    Writeln(Format('    el destino contador lleva %d entrada(s)', [Counted]));
  finally
    Provider.ShutDown;
  end;

  Section('Componer sin un solo proveedor es un error, no un silencio');
  var Bare: IServiceCollection := TServiceCollection.Create;
  try
    AddLogging(Bare,
      procedure(Builder: ILoggingBuilder)
      begin
        // a proposito: nadie registra nada
      end);
  except
    on E: ELoggingCompositionError do
      Writeln('    ', E.Message);
  end;

  Section('Y el silencio deliberado se dice en voz alta');
  var Quiet: IServiceCollection := TServiceCollection.Create;
  AddLogging(Quiet,
    procedure(Builder: ILoggingBuilder)
    begin
      Builder.AddNullProvider;
    end);
  var QuietProvider := Quiet.BuildServiceProvider;
  try
    QuietProvider.GetRequiredService<ILoggerFactory>.CreateLogger('Acme').LogError('nadie lee esto');
    Writeln('    compuesto con AddNullProvider: la linea se escribe y se descarta');
  finally
    QuietProvider.ShutDown;
  end;

  Writeln;
end.
