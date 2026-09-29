program StartupReport;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.IOUtils,
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  StartupReport.Payroll in 'StartupReport.Payroll.pas';

// Lo que el .dpr decide antes de que exista el host: donde queda el informe de arranque, y que se
// ve cuando el arranque falla. La consola se lleva el error y un codigo de salida distinto de
// cero; el fichero se lleva el registro que sobrevive.
//
//   StartupReport         -> arranca el servicio, y sale con Ctrl+C
//   StartupReport --fail  -> el arranque aborta: rc=1, y el motivo en startup.log
begin
  // La aplicacion sabe donde viven sus datos; DAF no.
  BootstrapLog.AddSink(TFileBootstrapSink.Create(TPath.Combine(DataFolder, 'startup.log')));
  var Host: IHost;
  try
    var Builder := THost.CreateBuilder;

    AddPayroll(Builder.Services);

    Host := Builder.Build;
    Host.Start;
  except
    on E: Exception do
    begin
      BootstrapLog.LoggerFor('Acme.Payroll').LogCritical(E, 'el arranque no pudo completarse');
      ExitCode := 1;   // consola y desatendido: el codigo de salida es parte del informe
      raise;           // escritorio: esto es lo que hace que la plataforma lo muestre
    end;
  end;
  try
    Host.WaitForShutdown;
  finally
    StopAndNil(Host);
  end;
end.
