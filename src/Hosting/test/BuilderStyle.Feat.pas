unit BuilderStyle.Feat;

interface

uses
  Daf.Hosting;

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

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given a
  /// ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara es la
  /// cadena que el propio montaje acaba de componer, no el comportamiento.
  TOverlapWorld = class
  public
    First: IGenericHostBuilder;
    Second: IGenericHostBuilder;
    Builder: IGenericHostBuilder;
    Names: TArray<string>;
    Report: string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows,
  Daf.MiniSpec,
  Daf.Extensions.Logging,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting;

// El directorio de trabajo es del proceso: se cambia solo mientras se crea el builder, que es
// cuando el host lo lee. Y sin DAF_CONTENTROOT, que el entorno de desarrollo puede traer puesta.
function CreateInDirectory(const Dir: string): IGenericHostBuilder;
begin
  var VarName: string := 'DAF_CONTENTROOT';
  var Ambient := GetEnvironmentVariable(VarName);
  var Previous := TDirectory.GetCurrentDirectory;
  Winapi.Windows.SetEnvironmentVariable(PChar(VarName), nil);
  TDirectory.SetCurrentDirectory(Dir);
  try
    Result := THostBuilder.Create;
  finally
    TDirectory.SetCurrentDirectory(Previous);
    if Ambient <> '' then
      Winapi.Windows.SetEnvironmentVariable(PChar(VarName), PChar(Ambient));
  end;
end;

function SystemDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  SetString(Result, Buffer, GetSystemDirectory(Buffer, Length(Buffer)));
end;

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
    .Given('two builders alive at the same time, neither of them released',
      procedure(W: TOverlapWorld)
      begin
        W.First := THostBuilder.Create;
        W.First.Services.AddSingleton<IMarker>(TMarker.Create('first') as IMarker);

        // Created while the first one is still held: two builders, two environments and two
        // builder contexts alive at the same time.
        W.Second := THostBuilder.Create;
        W.Second.Services.AddSingleton<IMarker>(TMarker.Create('second') as IMarker);
      end)
    .When('both are built and asked what they registered',
      procedure(W: TOverlapWorld)
      begin
        // Both hosts alive at the same time, not one after the other. Stop is what releases
        // the container -- and with it the host -- so both are stopped in the finally; a host
        // that is built and dropped keeps its provider alive and the leak report says so.
        var FirstHost := W.First.Build;
        var SecondHost := W.Second.Build;
        FirstHost.Start;
        SecondHost.Start;
        try
          W.Report := FirstHost.Services.GetService<IMarker>.Name + ',' +
                      SecondHost.Services.GetService<IMarker>.Name;
        finally
          FirstHost.Stop;
          // Stop libera el host -- THost es TNoRefCountObject y se destruye al parar --, asi que
          // la referencia se suelta sin pasar por _Release: el finalizador de la variable lo
          // llamaria sobre memoria ya devuelta. El porque completo, en Daf.Cli.
          Pointer(FirstHost) := nil;
          SecondHost.Stop;
          Pointer(SecondHost) := nil;
        end;
      end)
    .&Then('each one answers with its own, and no uniqueness assertion fired',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('first,second');
      end)

  .Scenario('A suite starts and stops several hosts')
    .Given('three startups planned in the same process, each with its own state',
      procedure(W: TOverlapWorld)
      begin
        W.Names := ['h1', 'h2', 'h3'];
      end)
    .When('each one is started and stopped in turn',
      procedure(W: TOverlapWorld)
      begin
        for var N in W.Names do
        begin
          var Builder: IGenericHostBuilder := THostBuilder.Create;
          Builder.Services.AddSingleton<IMarker>(TMarker.Create(N) as IMarker);
          var Host := Builder.Build;
          Host.Start;
          try
            W.Report := W.Report + Host.Services.GetService<IMarker>.Name;
          finally
            Host.Stop;
            Pointer(Host) := nil;
          end;
        end;
      end)
    .&Then('every one of them saw its own state',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('h1h2h3');
      end)

.Rule('El entorno sale de la configuracion de host')

  // Las claves son las de .NET: environment, applicationName y contentRoot. Llegan por los
  // argumentos, o por una variable de entorno con DAF_ delante, que se lee sin el prefijo.
  .Scenario('El entorno se fija por linea de comandos')
    .Given('un builder al que se le pasa el entorno como argumento',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := THostBuilder.Create(['--environment', 'Development']);
      end)
    .When('se lee su entorno',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.EnvironmentName;
      end)
    .&Then('responde con ese valor',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('Development');
      end)

  .Scenario('Una variable DAF_ fija la clave que queda al quitarle el prefijo')
    .Given('el entorno puesto en DAF_ENVIRONMENT, y ningun argumento',
      procedure(W: TOverlapWorld)
      begin
        // Nombre en una variable: PChar sobre una constante verdadera da E2036, y el error sale
        // lejos de la causa.
        var VarName: string := 'DAF_ENVIRONMENT';
        Winapi.Windows.SetEnvironmentVariable(PChar(VarName), 'Staging');
        try
          // El entorno se resuelve al construir, no al consultarlo, asi que queda fijado antes
          // de que el finally quite la variable.
          W.Builder := THostBuilder.Create;
        finally
          Winapi.Windows.SetEnvironmentVariable(PChar(VarName), nil);
        end;
      end)
    .When('se lee su entorno',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.EnvironmentName;
      end)
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
        W.Builder := THostBuilder.Create(['--environment=Testing']);
      end)
    .When('se lee su entorno',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.EnvironmentName + '/' +
          BoolToStr(W.Builder.Environment.IsProduction, True);
      end)
    .&Then('no es el predeterminado',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('Testing/False');
      end)

  .Scenario('El nombre de aplicacion y la raiz de contenido tambien salen de ahi')
    .Given('los dos fijados por argumento',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := THostBuilder.Create(['--applicationName=mytool', '--contentRoot=C:\DafSpecRoot']);
      end)
    .When('se leen',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.ApplicationName + '|' + W.Builder.Environment.ContentRootPath;
      end)
    .&Then('responden con lo que se les paso',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('mytool|C:\DafSpecRoot\');
      end)

.Rule('Sin fijarla, la raiz de contenido es la que elige .NET')

  .Scenario('Sin fijarla, es el directorio de trabajo')
    .Given('un builder creado desde un directorio que no es el del ejecutable',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := CreateInDirectory(TPath.GetTempPath);
      end)
    .When('se lee su raiz de contenido',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.ContentRootPath;
      end)
    .&Then('es ese directorio',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual(IncludeTrailingPathDelimiter(TPath.GetTempPath));
      end)

  // Un servicio de Windows arranca en el directorio del sistema, donde no hay nada suyo.
  .Scenario('Si el directorio de trabajo es el del sistema, es la carpeta del ejecutable')
    .Given('un builder creado desde el directorio del sistema, como el de un servicio',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := CreateInDirectory(SystemDirectory);
      end)
    .When('se lee su raiz de contenido',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.ContentRootPath;
      end)
    .&Then('es la carpeta del ejecutable',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual(W.Builder.Environment.BinPath);
      end)

  .Scenario('Una raiz de contenido relativa cuelga de la carpeta del ejecutable')
    .Given('un builder al que se le pasa una raiz de contenido relativa',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := THostBuilder.Create(['--contentRoot=conf']);
      end)
    .When('se lee su raiz de contenido',
      procedure(W: TOverlapWorld)
      begin
        W.Report := W.Builder.Environment.ContentRootPath;
      end)
    .&Then('es esa carpeta dentro de la del ejecutable',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual(
          IncludeTrailingPathDelimiter(TPath.Combine(W.Builder.Environment.BinPath, 'conf')));
      end)

.Rule('Componer logging solo si se pidio')

  // AddLogging levanta el error de composicion cuando nadie anadio un proveedor, asi que llamarlo
  // por sistema romperia toda aplicacion que no configure logging -- que hoy son casi todas.
  .Scenario('Una aplicacion que nunca toca Logging construye y arranca')
    .Given('un builder al que nadie le pide la propiedad Logging',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := THostBuilder.Create;
        W.Builder.Services.AddSingleton<IMarker>(TMarker.Create('quiet') as IMarker);
      end)
    .When('se construye y se arranca',
      procedure(W: TOverlapWorld)
      begin
        var Host := W.Builder.Build;
        Host.Start;
        try
          W.Report := Host.Services.GetService<IMarker>.Name;
        finally
          Host.Stop;
        end;
      end)
    .&Then('llega hasta el final sin que nada levante un error de composicion',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('quiet');
      end)

  .Scenario('Quien si la toca obtiene su pipeline')
    .Given('un builder al que se le anade un proveedor de logging',
      procedure(W: TOverlapWorld)
      begin
        W.Builder := THostBuilder.Create;
        W.Builder.Logging.AddNullProvider;
      end)
    .When('se construye y se arranca',
      procedure(W: TOverlapWorld)
      begin
        var Host := W.Builder.Build;
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
    .&Then('la factoria de loggers esta registrada',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end);

end.
