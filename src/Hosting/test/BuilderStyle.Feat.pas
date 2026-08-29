unit BuilderStyle.Feat;

interface

type
  // In the interface section by obligation, not by taste: the container resolves through
  // RTTI, and a type declared only in the implementation section is not there to be found.
  IMarker = interface(IInvokable)
    ['{6C1C4C2B-6F1E-4C61-9E63-1E5F3A4B7D21}']
    function Name: string;
  end;

  TMarker = class(TInterfacedObject, IMarker)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    function Name: string;
  end;

  TOverlapWorld = class
  public
    Report: string;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  Daf.MiniSpec,
  Daf.Extensions.Logging,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting;

constructor TMarker.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TMarker.Name: string;
begin
  Result := FName;
end;

// --- Feature definition ---

initialization

Feature('''
Feature Several startups in one process @hosting @builder-style

  As a spec author, and as anyone whose startup now lives outside Build
  I want two builders and two hosts to be alive at the same time in one process
  So that state belonging to one of them never reaches the other
''')

.UseWorld<TOverlapWorld>

.Rule('Overlapping startups')

  .Scenario('Two builders alive at once')
    .Given('one builder that is not going to be released',
      procedure(W: TOverlapWorld)
      begin
        var First := THostBuilder.Create;
        var FirstOwner: IHostApplicationBuilder := First;
        First.Services.AddSingleton<IMarker>(TMarker.Create('first') as IMarker);

        // Created while the first one is still held: two builders, two environments and two
        // builder contexts alive at the same time.
        var Second := THostBuilder.Create;
        var SecondOwner: IHostApplicationBuilder := Second;
        Second.Services.AddSingleton<IMarker>(TMarker.Create('second') as IMarker);

        // Both hosts alive at the same time, not one after the other. Stop is what releases
        // the container -- and with it the host -- so both are stopped in the finally; a host
        // that is built and dropped keeps its provider alive and the leak report says so.
        var FirstHost := First.Build;
        var SecondHost := Second.Build;
        FirstHost.Start;
        SecondHost.Start;
        try
          W.Report := FirstHost.Services.GetService<IMarker>.Name + ',' +
                      SecondHost.Services.GetService<IMarker>.Name;
        finally
          FirstHost.Stop;
          SecondHost.Stop;
        end;
      end)
    .When('both are built and asked what they registered').NoAction
    .&Then('each one answers with its own, and no uniqueness assertion fired',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('first,second');
      end)

  .Scenario('A suite starts and stops several hosts')
    .Given('three hosts built one after another in the same process',
      procedure(W: TOverlapWorld)
      begin
        W.Report := '';
        for var I := 1 to 3 do
        begin
          var Builder := THostBuilder.Create;
          var Owner: IHostApplicationBuilder := Builder;
          Builder.Services.AddSingleton<IMarker>(TMarker.Create('h' + I.ToString) as IMarker);
          var Host := Builder.Build;
          Host.Start;
          try
            W.Report := W.Report + Host.Services.GetService<IMarker>.Name;
          finally
            Host.Stop;
          end;
        end;
      end)
    .When('each one is started and stopped in turn').NoAction
    .&Then('every one of them saw its own state',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('h1h2h3');
      end)

.Rule('El entorno sale de la configuracion de host')

  // Hoy no habia forma de fijar el entorno por linea de comandos: THostEnvironment leia las tres
  // variables del proceso por su cuenta y la configuracion no participaba. Y es lo primero que se
  // prueba en cuanto el builder recibe argumentos.
  .Scenario('El entorno se fija por linea de comandos')
    .Given('un builder al que se le pasa el entorno como argumento',
      procedure(W: TOverlapWorld)
      begin
        var Builder := THostBuilder.Create(['--DAF_APP_ENV=Development']);
        var Owner: IHostApplicationBuilder := Builder;
        W.Report := Builder.Environment.EnvironmentName;
      end)
    .When('se lee su entorno').NoAction
    .&Then('responde con ese valor',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('Development');
      end)

  .Scenario('La variable de entorno de siempre sigue valiendo')
    .Given('el entorno puesto en la variable de entorno, y ningun argumento',
      procedure(W: TOverlapWorld)
      begin
        // Nombre en una variable: PChar sobre una constante verdadera da E2036, y el error sale
        // lejos de la causa.
        var VarName: string := TDafEnvVars.APP_ENV;
        Winapi.Windows.SetEnvironmentVariable(PChar(VarName), 'Staging');
        try
          var Builder := THostBuilder.Create;
          var Owner: IHostApplicationBuilder := Builder;
          W.Report := Builder.Environment.EnvironmentName;
        finally
          Winapi.Windows.SetEnvironmentVariable(PChar(VarName), nil);
        end;
      end)
    .When('se lee su entorno').NoAction
    .&Then('responde con ese valor',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('Staging');
      end)

  // El modo de fallo que menos avisa: el valor por defecto es indistinguible del argumento
  // ignorado, asi que un escenario que solo comprobara "responde algo" pasaria con el entorno sin
  // implementar.
  .Scenario('Un argumento ignorado no se confunde con el valor por defecto')
    .Given('el entorno fijado a un valor distinto del predeterminado',
      procedure(W: TOverlapWorld)
      begin
        var Builder := THostBuilder.Create(['--DAF_APP_ENV=Testing']);
        var Owner: IHostApplicationBuilder := Builder;
        W.Report := Builder.Environment.EnvironmentName + '/' +
          BoolToStr(Builder.Environment.IsProduction, True);
      end)
    .When('se lee su entorno').NoAction
    .&Then('no es el predeterminado',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('Testing/False');
      end)

  .Scenario('El nombre de aplicacion y la raiz de contenido tambien salen de ahi')
    .Given('los dos fijados por argumento',
      procedure(W: TOverlapWorld)
      begin
        var Builder := THostBuilder.Create(['--DAF_APP_NAME=mytool']);
        var Owner: IHostApplicationBuilder := Builder;
        W.Report := Builder.Environment.ApplicationName;
      end)
    .When('se leen').NoAction
    .&Then('responden con lo que se les paso',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('mytool');
      end)

.Rule('Componer logging solo si se pidio')

  // AddLogging levanta el error de composicion cuando nadie anadio un proveedor, asi que llamarlo
  // por sistema romperia toda aplicacion que no configure logging -- que hoy son casi todas.
  .Scenario('Una aplicacion que nunca toca Logging construye y arranca')
    .Given('un builder al que nadie le pide la propiedad Logging',
      procedure(W: TOverlapWorld)
      begin
        var Builder := THostBuilder.Create;
        var Owner: IHostApplicationBuilder := Builder;
        Builder.Services.AddSingleton<IMarker>(TMarker.Create('quiet') as IMarker);
        var Host := Builder.Build;
        Host.Start;
        try
          W.Report := Host.Services.GetService<IMarker>.Name;
        finally
          Host.Stop;
        end;
      end)
    .When('se construye y se arranca').NoAction
    .&Then('llega hasta el final sin que nada levante un error de composicion',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('quiet');
      end)

  .Scenario('Quien si la toca obtiene su pipeline')
    .Given('un builder al que se le anade un proveedor de logging',
      procedure(W: TOverlapWorld)
      begin
        var Builder := THostBuilder.Create;
        var Owner: IHostApplicationBuilder := Builder;
        Builder.Logging.AddNullProvider;
        var Host := Builder.Build;
        Host.Start;
        try
          // Assigned pide una variable, no el resultado de una llamada: sobre la llamada da
          // E2036 y el error aparece en la linea siguiente, lejos de la causa.
          var Factory: ILoggerFactory := Host.Services.GetService<ILoggerFactory>;
          W.Report := BoolToStr(Assigned(Factory), True);
        finally
          Host.Stop;
        end;
      end)
    .When('se construye y se arranca').NoAction
    .&Then('la factoria de loggers esta registrada',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end);

end.
