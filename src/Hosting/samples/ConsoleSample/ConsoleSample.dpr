program ConsoleSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Daf.Rtti,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  Daf.Threading,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.Configuration.Builder,
  Daf.Configuration.Binder,
  Daf.Configuration.Json,

  ConsoleSample.AppSettings in 'ConsoleSample.AppSettings.pas',
  ConsoleSample.Greeter in 'ConsoleSample.Greeter.pas';

begin
  ReportMemoryLeaksOnShutdown := True;

  var Host: IHost := THostBuilder.Create.ConfigureAppConfiguration(
    procedure(Context: IHostBuilderContext; Config: IConfigurationBuilder)
    begin
      var Env := Context.Environment;
      var SampleFilesPath := TPath.Combine(Env.BinPath, Env.ApplicationName);
      var RunTimeFilePath := TPath.Combine(SampleFilesPath, 'appsettings.json');

      JsonConfig.AddFile(Config, RunTimeFilePath);
      TShutdownHook.PresCtlCMsg := 'Pulse ctl-c para salir';
    end)
    .ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddTransient<IWrapperImpl, TWrapper>;
      Services.AddSingleton<IGreeter>(
        function(Services: IServiceProvider): IGreeter
        begin
          var Config := Services.GetRequiredService<IConfiguration>;
          var Settings := TConfigurationBinder.Bind<TAppSettings>(Config);
          Result := TGreeter.Create(Settings.Title, Settings.Version, Settings.Message);
          Settings.Free;
        end);
    end)
    .Build;

    Host.Start;
    try
      var Greeter := Host.Services.GetService<IGreeter>;
      Greeter.Greet;

      var WA := TActivatorUtilities.CreateInstance<TWrapperAccessor>(Host.Services);
      Assert(WA.ChecWrapperAccess);
      Host.WaitForShutdown;
    finally
      Host.Stop;
    end;
end.
