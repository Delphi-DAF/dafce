unit CliVariant.Feat;

interface

uses
  System.SysUtils,
  System.StrUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Cli;

type
  // En la seccion de interfaz por obligacion: el contenedor resuelve por RTTI, y un tipo que
  // solo esta en implementation no esta ahi para encontrarlo.
  IGreeter = interface(IInvokable)
    ['{9C5F3F81-1E63-4C70-9725-BFC4073C6324}']
    function Greet: string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  public
    function Greet: string;
  end;

  /// El trabajo declarado como servicio, que es la via de la suite: se registra como cualquier
  /// otro y no hace falta una via de registro propia.
  TGreetingHandler = class(TInterfacedObject, ICliHandler)
  private
    FGreeter: IGreeter;
  public
    constructor Create(const Greeter: IGreeter);
    function Execute(const Parsed: ICommandLineParseResult): Integer;
  end;

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given
  /// a ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara
  /// es la cadena que el propio montaje acaba de componer, no el comportamiento.
  TCliWorld = class
  public
    Builder: ICliApplicationBuilder;
    App: ICliApplication;
    /// La primera instancia vive aqui y no en el Given porque es ella quien sostiene el
    /// cerrojo: si muriera al acabar el montaje, la segunda arrancaria tan tranquila.
    Holder: ICliApplication;
    Verbose: IOption<Boolean>;
    FileArg: IArgument<string>;
    Parsed: ICommandLineParseResult;
    ExitCode: Integer;
    Report: string;
    PolicySaw: string;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.Extensions.Configuration,
  Daf.Configuration.CommandLine,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging;

function TGreeter.Greet: string;
begin
  Result := 'hello';
end;

constructor TGreetingHandler.Create(const Greeter: IGreeter);
begin
  inherited Create;
  FGreeter := Greeter;
end;

function TGreetingHandler.Execute(const Parsed: ICommandLineParseResult): Integer;
begin
  if FGreeter.Greet = 'hello' then
    Result := 7
  else
    Result := 0;
end;

/// Busca un acceso global: cualquier metodo de clase que devuelva la aplicacion o su proveedor
/// de servicios. Es la unica forma de escribir "esto NO existe" que se pone roja cuando alguien
/// lo anade -- comprobarlo leyendo el codigo no lo haria.
function GlobalAccessorsIn(const AClass: TClass): string;
begin
  Result := '';
  var Ctx := TRttiContext.Create;
  try
    for var M in Ctx.GetType(AClass).GetDeclaredMethods do
    begin
      if not (M.MethodKind in [mkClassFunction]) then Continue;
      if M.ReturnType = nil then Continue;
      if (M.ReturnType.Handle = TypeInfo(ICliApplication)) or
         (M.ReturnType.Handle = TypeInfo(IServiceProvider)) then
        Result := Result + M.Name + ' ';
    end;
  finally
    Ctx.Free;
  end;
  Result := Result.Trim;
end;

// --- Feature definition ---

initialization

Feature('''
Feature La variante CLI de la familia @cli @application-family

  Como quien escribe una herramienta de linea de comandos
  Quiero que el arranque cubra el borde del Run
  Para no repetir en cada .dpr la instancia unica, la version y el codigo de salida
''')

.UseWorld<TCliWorld>

.Rule('La gramatica es la unica duena de argv')

  // Dos lectores del mismo argv con reglas distintas, y discrepan por construccion: la
  // gramatica ve un flag y un posicional; el proveedor de configuracion veria verbose con el
  // posicional por valor. Las dos contestan sin error, y una de las dos es falsa.
  .Scenario('Una bandera seguida de un posicional no acaba en la configuracion')
    .Given('una gramatica con una bandera y un argumento posicional',
      procedure(W: TCliWorld)
      begin
        W.Verbose := CommandLine.Option<Boolean>('--verbose', 'Cuenta lo que hace');
        W.FileArg := CommandLine.Argument<string>('file', 'Que fichero');

        W.Builder := TCliApplication.CreateBuilder(['--verbose', 'informe.txt']);
        W.Builder.RootCommand.Add([W.Verbose, W.FileArg]);
        W.Builder.OnExecute(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := 0;
          end);
      end)
    .When('se invoca con la bandera y el posicional detras',
      procedure(W: TCliWorld)
      begin
        W.Parsed := W.Builder.RootCommand.Parse(['--verbose', 'informe.txt']);
      end)
    .&Then('la gramatica liga los dos simbolos y la configuracion no ha inventado la clave',
      procedure(W: TCliWorld)
      begin
        Expect(Format('verbose=%s file=%s config=%s',
          [BoolToStr(W.Verbose.ValueIn(W.Parsed), True), W.FileArg.ValueIn(W.Parsed),
           BoolToStr(W.Builder.Configuration.Exists('verbose'), True)]))
          .ToEqual('verbose=True file=informe.txt config=False');
      end)

  .Scenario('Quien quiere los argumentos como configuracion los pide')
    .Given('un arranque de consola que todavia no ha mirado argv',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        Expect(W.Builder.Configuration.Exists('Db:Server')).ToBeFalse;
      end)
    .When('se anaden los argumentos explicitamente como fuente',
      procedure(W: TCliWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder.Configuration, ['--Db:Server=localhost']);
      end)
    .&Then('sus claves se leen como las de cualquier otra fuente',
      procedure(W: TCliWorld)
      begin
        Expect(W.Builder.Configuration['Db:Server']).ToEqual('localhost');
      end)

.Rule('El borde del Run')

  .Scenario('Una aplicacion que decide no arrancar termina sin contenedor')
    .Given('una instancia ya corriendo con ese identificador, y una segunda construida igual',
      procedure(W: TCliWorld)
      begin
        var First := TCliApplication.CreateBuilder;
        First.SingleInstance('daf-cli-spec');
        W.Holder := First.Build;
        // Se ejecuta: Stop es lo que suelta el contenedor, y una aplicacion construida y
        // abandonada lo deja vivo -- el informe de fugas lo dice. El cerrojo, en cambio, lo
        // sigue sosteniendo W.Holder hasta que se destruya, que es lo que este escenario mide.
        W.Holder.Run;

        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.SingleInstance('daf-cli-spec');
        W.App := W.Builder.Build;
      end)
    .When('arranca la segunda',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('termina sin haber construido el contenedor y lo dice por su codigo de salida',
      procedure(W: TCliWorld)
      begin
        // Sin contenedor: no se llego a construir nada.
        Expect(Format('%d/%s', [W.ExitCode, BoolToStr(W.App.Services = nil, True)]))
          .ToEqual('3/True');
      end)

  .Scenario('Una excepcion no capturada aplica la politica y sale distinto de cero')
    .Given('una aplicacion cuya politica traduce a codigo de salida y cuyo trabajo lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnUnhandled(
          function(const E: Exception; const Services: IServiceProvider): Integer
          begin
            W.PolicySaw := E.Message;
            Result := 42;
          end);
        W.Builder.OnExecute(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('se aplica la politica declarada',
      procedure(W: TCliWorld)
      begin
        Expect(W.PolicySaw).ToEqual('el trabajo fallo');
      end)
    .&And('el proceso termina con un codigo que dice que fallo',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(42);
      end)

  .Scenario('La version la resuelve la gramatica, no la aplicacion')
    .Given('una aplicacion invocada pidiendo la version',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--version']);
        W.Builder.RootCommand.WithVersion('2.0.0');
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('contesta el parse y termina bien, sin levantar contenedor',
      procedure(W: TCliWorld)
      begin
        // Ni siquiera se construyo el contenedor: la gramatica contesto sola.
        Expect(Format('%d/%s', [W.ExitCode, BoolToStr(W.App.Services = nil, True)]))
          .ToEqual('0/True');
      end)

  .Scenario('Una invocacion que no encaja tiene su texto y su codigo')
    .Given('una invocacion con un token que la gramatica no reconoce',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--que-no-existe']);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('sale con el codigo de error de invocacion, distinto del de fallo del trabajo',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(2);
      end)

  .Scenario('El trabajo puede venir del contenedor')
    .Given('un handler registrado como cualquier otro servicio',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IGreeter, TGreeter>;
        W.Builder.Services.AddSingleton<ICliHandler, TGreetingHandler>;
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('se resuelve con sus dependencias y su resultado es el codigo de salida',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(7);
      end)

  // Dos formas de decir lo mismo son el defecto, no una precedencia que haya que recordar.
  .Scenario('Declarar el trabajo dos veces es un error de composicion')
    .Given('una aplicacion con OnExecute y con un ICliHandler registrado',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IGreeter, TGreeter>;
        W.Builder.Services.AddSingleton<ICliHandler, TGreetingHandler>;
        W.Builder.OnExecute(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := 0;
          end);
      end)
    .When('se construye',
      procedure(W: TCliWorld)
      begin
        try
          W.App := W.Builder.Build;
          W.Report := '<construyo sin protestar>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('falla al componer, nombrando el problema',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToStartWith('ECliCompositionError: ');
        Expect(W.Report).ToContain('declares its work twice');
      end)

  .Scenario('Registrar mas de un ICliHandler es un error de composicion')
    .Given('una aplicacion con dos ICliHandler registrados',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IGreeter, TGreeter>;
        W.Builder.Services.AddSingleton<ICliHandler, TGreetingHandler>;
        W.Builder.Services.AddSingleton<ICliHandler, TGreetingHandler>;
      end)
    .When('se construye',
      procedure(W: TCliWorld)
      begin
        try
          W.App := W.Builder.Build;
          W.Report := '<construyo sin protestar>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('falla al componer, diciendo cuantos hay',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToStartWith('ECliCompositionError: ');
        Expect(W.Report).ToContain('registers 2 ICliHandler');
      end)

.Rule('Las opciones del contenedor llegan por el builder de la variante')

  .Scenario('Fijar la validacion en el builder CLI la aplica al construir')
    .Given('una aplicacion CLI con un trabajo cuya dependencia nadie registra, y la validacion fijada',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<ICliHandler, TGreetingHandler>;
        W.Builder.ServiceProviderOptions := TServiceProviderOptions.Create(True, True);
      end)
    .When('se construye',
      procedure(W: TCliWorld)
      begin
        // El fallo del host sale tambien por el canal de arranque; se aparta para que no se
        // confunda con la salida de las specs.
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            W.App := W.Builder.Build;
            W.Report := '<construyo sin protestar>';
          except
            on E: Exception do
              W.Report := E.Message;
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('falla nombrando la dependencia que falta',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToContain('IGreeter');
      end)

.Rule('Sin acceso global')

  // En consola la plataforma no tiene ningun Application global que el usuario espere, .NET
  // tampoco lo tiene, y anadirlo seria un service locator sin nada que lo justifique. Donde si
  // lo hay --VCL y FMX-- es porque la plataforma ya lo puso.
  .Scenario('Una aplicacion de consola no tiene donde pedir servicios por la puerta de atras')
    .Given('la variante CLI compilada').NoAction
    .When('se busca un acceso global a la aplicacion o a su proveedor',
      procedure(W: TCliWorld)
      begin
        W.Report := (GlobalAccessorsIn(TCliApplication) + ' ' +
                     GlobalAccessorsIn(TCliApplicationBuilder)).Trim;
        if W.Report.IsEmpty then
          W.Report := '<ninguno>';
      end)
    .&Then('no hay ninguno',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('<ninguno>');
      end);

end.
