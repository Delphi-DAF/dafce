program HostedService;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  System.IOUtils,
  Daf.Extensions.Logging,
  Daf.Logging.Provider,
  Daf.NNLog.Loader,
  Daf.NNLog.Targets.Console,
  Daf.NNLog.Targets.File_,
  Daf.NNLog.Targets.UDP,
  Daf.Extensions.DependencyInjection,
  // Por AddHostedService, que es metodo del record helper que declara esta unit.
  Daf.Extensions.Hosting,
  Daf.Extensions.Configuration,
  Daf.Configuration.Json,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  HostedWorker in 'HostedWorker.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

    // El builder expone el entorno antes de construir el host, asi que la ruta del
    // appsettings se resuelve aqui mismo.
    var JsonFilePath := TPath.Combine(B.Environment.BinPath, B.Environment.ApplicationName, 'appsettings.json');
    JsonConfig.AddFile(B.Configuration, JsonFilePath);

    // Y la configuracion tambien: el proveedor de log se compone leyendo lo que se acaba de anadir.
    B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')));

    B.Services.AddHostedService<THostedWorker>;

    ExitCode := B.Build.Run;
  except
    on E: Exception do
    begin
      Write(E.Message);
      ReadLn;
    end;
  end;
end.
