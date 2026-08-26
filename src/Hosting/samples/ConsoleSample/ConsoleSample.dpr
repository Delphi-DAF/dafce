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
  Daf.Configuration.Binder,
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
  B.Services.AddSingleton<IGreeter>(
    function(Services: IServiceProvider): IGreeter
    begin
      var Config := Services.GetRequiredService<IConfiguration>;
      var Settings := TConfigurationBinder.Bind<TAppSettings>(Config);
      Result := TGreeter.Create(Settings.Title, Settings.Version, Settings.Message);
      Settings.Free;
    end);

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Services.GetService<IGreeter>.Greet;

      var WA := TActivatorUtilities.CreateInstance<TWrapperAccessor>(Services);
      Assert(WA.ChecWrapperAccess);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
