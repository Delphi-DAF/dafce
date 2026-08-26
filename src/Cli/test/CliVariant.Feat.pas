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

  TCliWorld = class
  public
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
  Daf.Extensions.Hosting;

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
        var Verbose := CommandLine.Option<Boolean>('--verbose', 'Cuenta lo que hace');
        var FileArg := CommandLine.Argument<string>('file', 'Que fichero');

        var B := TCliApplication.CreateBuilder(['--verbose', 'informe.txt']);
        B.RootCommand.Add([Verbose, FileArg]);
        B.OnExecute(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := 0;
          end);

        var Parsed := B.RootCommand.Parse(['--verbose', 'informe.txt']);
        W.Report := Format('verbose=%s file=%s config=%s',
          [BoolToStr(Verbose.ValueIn(Parsed), True), FileArg.ValueIn(Parsed),
           BoolToStr(B.Configuration.Exists('verbose'), True)]);
      end)
    .When('se invoca con la bandera y el posicional detras').NoAction
    .&Then('la gramatica liga los dos simbolos y la configuracion no ha inventado la clave',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('verbose=True file=informe.txt config=False');
      end)

  .Scenario('Quien quiere los argumentos como configuracion los pide')
    .Given('un arranque que anade los argumentos explicitamente',
      procedure(W: TCliWorld)
      begin
        var B := TCliApplication.CreateBuilder;
        CommandLineConfig.AddArgs(B.Configuration, ['--Db:Server=localhost']);
        W.Report := B.Configuration['Db:Server'];
      end)
    .When('se lee la configuracion').NoAction
    .&Then('sus claves se leen como las de cualquier otra fuente',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('localhost');
      end)

.Rule('El borde del Run')

  .Scenario('Una aplicacion que decide no arrancar termina sin contenedor')
    .Given('una instancia ya corriendo con ese identificador',
      procedure(W: TCliWorld)
      begin
        var First := TCliApplication.CreateBuilder;
        First.SingleInstance('daf-cli-spec');
        var FirstApp := First.Build;
        // Se ejecuta: Stop es lo que suelta el contenedor, y una aplicacion construida y
        // abandonada lo deja vivo -- el informe de fugas lo dice. El cerrojo, en cambio, lo
        // sigue sosteniendo FirstApp hasta que se destruya, que es lo que este escenario mide.
        FirstApp.Run;

        var Second := TCliApplication.CreateBuilder;
        Second.SingleInstance('daf-cli-spec');
        var SecondApp := Second.Build;

        W.ExitCode := SecondApp.Run;
        // Sin contenedor: no se llego a construir nada.
        W.Report := BoolToStr(SecondApp.Services = nil, True);
      end)
    .When('arranca la segunda').NoAction
    .&Then('termina sin haber construido el contenedor y lo dice por su codigo de salida',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%d/%s', [W.ExitCode, W.Report])).ToEqual('3/True');
      end)

  .Scenario('Una excepcion no capturada aplica la politica y sale distinto de cero')
    .Given('una aplicacion cuya politica traduce a codigo de salida',
      procedure(W: TCliWorld)
      begin
        var B := TCliApplication.CreateBuilder;
        B.OnUnhandled(
          function(const E: Exception; const Services: IServiceProvider): Integer
          begin
            W.PolicySaw := E.Message;
            Result := 42;
          end);
        B.OnExecute(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.ExitCode := B.Build.Run;
      end)
    .When('el trabajo lanza').NoAction
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
        var B := TCliApplication.CreateBuilder(['--version']);
        B.RootCommand.WithVersion('2.0.0');
        var App := B.Build;
        W.ExitCode := App.Run;
        // Ni siquiera se construyo el contenedor: la gramatica contesto sola.
        W.Report := BoolToStr(App.Services = nil, True);
      end)
    .When('se ejecuta').NoAction
    .&Then('contesta el parse y termina bien, sin levantar contenedor',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%d/%s', [W.ExitCode, W.Report])).ToEqual('0/True');
      end)

  .Scenario('Una invocacion que no encaja tiene su texto y su codigo')
    .Given('una invocacion con un token que la gramatica no reconoce',
      procedure(W: TCliWorld)
      begin
        var B := TCliApplication.CreateBuilder(['--que-no-existe']);
        W.ExitCode := B.Build.Run;
      end)
    .When('se ejecuta').NoAction
    .&Then('sale con el codigo de error de invocacion, distinto del de fallo del trabajo',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(2);
      end)

  .Scenario('El trabajo puede venir del contenedor')
    .Given('un handler registrado como cualquier otro servicio',
      procedure(W: TCliWorld)
      begin
        var B := TCliApplication.CreateBuilder;
        B.Services.AddSingleton<IGreeter, TGreeter>;
        B.Services.AddSingleton<ICliHandler, TGreetingHandler>;
        W.ExitCode := B.Build.Run;
      end)
    .When('se ejecuta').NoAction
    .&Then('se resuelve con sus dependencias y su resultado es el codigo de salida',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(7);
      end)

  // Dos formas de decir lo mismo son el defecto, no una precedencia que haya que recordar.
  .Scenario('Declarar el trabajo dos veces es un error de composicion')
    .Given('una aplicacion con OnExecute y con un ICliHandler registrado').NoAction
    .When('se construye').NoAction
    .&Then('falla al componer, nombrando el problema',
      procedure(W: TCliWorld)
      begin
        ExpectException(
          procedure
          begin
            var B := TCliApplication.CreateBuilder;
            B.Services.AddSingleton<IGreeter, TGreeter>;
            B.Services.AddSingleton<ICliHandler, TGreetingHandler>;
            B.OnExecute(
              function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
              begin
                Result := 0;
              end);
            B.Build;
          end).ToBe(ECliCompositionError);
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
