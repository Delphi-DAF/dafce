program HostedService;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  System.IOUtils,
  Daf.Extensions.Logging,
  Daf.Logging.Provider,
  Daf.Logging.Builder,
  Daf.Extensions.Hosting,
  Daf.NNLog.Loader,
  Daf.NNLog.Targets.Console,
  Daf.NNLog.Targets.File_,
  Daf.NNLog.Targets.UDP,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Configuration,
  Daf.Configuration.Json,
  Daf.Application.Builder,
  HostedWorker in 'HostedWorker.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    var Builder := TDafApplication.CreateHostBuilder;
    Builder.ConfigureAppConfiguration(procedure(Context: IHostBuilderContext; Builder: IConfigurationBuilder)
      begin
        var Env := Context.Environment;
        var JsonFilePath := TPath.Combine(Env.BinPath, Env.ApplicationName, 'appsettings.json');
        JsonConfig.AddFile(Builder, JsonFilePath);
      end);

    Builder.ConfigureServices(procedure(Context: IHostBuilderContext; Services: IServiceCollection)
      begin
        AddLogging(Services, procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(TNNLogLoader.BuildProvider(Context, 'Logging:NLog'));
          end);

        Services.AddHostedService<THostedWorker>;
        end);

    var App := Builder.Build;

    App.Run(True);
  except
    on E: exception do
    begin
      Write(E.message);
      ReadLn;
    end;
  end;
end.

