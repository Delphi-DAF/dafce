program ConsoleSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Daf.Rtti,
  Daf.Extensions.Hosting,
  Daf.Threading,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.Configuration.Builder,
  Daf.Options.Configuration,
  Daf.Configuration.Json,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,

  ConsoleSample.AppSettings in 'ConsoleSample.AppSettings.pas',
  ConsoleSample.Greeter in 'ConsoleSample.Greeter.pas';

// El caso que motivaba el estilo diferido: LEER configuracion para decidir que configuracion
// cargar. Costaba un callback, porque solo dentro de el existia el entorno resuelto. Ahora el
// builder ya responde en cuanto se crea, y la composicion se lee de arriba abajo.
begin
  ReportMemoryLeaksOnShutdown := True;

  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  var SampleFilesPath := TPath.Combine(B.Environment.BinPath, B.Environment.ApplicationName);
  JsonConfig.AddFile(B.Configuration, TPath.Combine(SampleFilesPath, 'appsettings.json'));
  TShutdownHook.PresCtlCMsg := 'Pulse ctl-c para salir';

  B.Services.AddTransient<IWrapperImpl, TWrapper>;
  // Los ajustes se declaran una vez, y el contenedor los construye, los posee y los libera.
  // El servicio los pide por constructor como cualquier otra dependencia: ni lambda, ni
  // Free. El `as IConfiguration` es porque el gestor de configuracion es a la vez lector y
  // constructor, y Delphi solo admite un padre por interfaz.
  ConfigOptions.Bind<TAppSettings>(B.Services, B.Configuration as IConfiguration);
  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Services.GetService<IGreeter>.Greet;

      // CreateInstance devuelve un objeto NO contado: el dueno es quien llama. Sin este Free se
      // fugan DOS cosas, no una -- el accesor, y el IWrapperImpl que sostiene dentro, porque su
      // campo de interfaz no se finaliza nunca y el refcount no baja a cero.
      var WA := TActivatorUtilities.CreateInstance<TWrapperAccessor>(Services);
      try
        // Y no un Assert: con las aserciones desactivadas la llamada desaparece y la demostracion
        // de este sample no demuestra nada, sin que nada lo diga.
        if not WA.ChecWrapperAccess then
          raise Exception.Create('el contenedor no relleno el IWrapper del constructor');
      finally
        WA.Free;
      end;
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
