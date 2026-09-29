program HostLifetime;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  HostLifetime.Diagnostics in 'HostLifetime.Diagnostics.pas';

// Las tres notificaciones del ciclo de vida del host, y un componente que guarda una copia del
// IHost y la suelta al notificarse la parada. Pide la parada por codigo, asi que termina solo.
//
//   HostLifetime  -> arranca, pide la parada, y para
begin
  ReportMemoryLeaksOnShutdown := True;
  var Builder := THost.CreateBuilder;
  var Host := Builder.Build;

  var Lifetime := Host.Services.GetRequiredService<IHostApplicationLifetime>;
  Lifetime.ApplicationStarted.Register(
    procedure
    begin
      Writeln('ApplicationStarted : el host esta arrancado');
    end);
  Lifetime.ApplicationStopping.Register(
    procedure
    begin
      Writeln('ApplicationStopping: se ha pedido la parada');
    end);
  Lifetime.ApplicationStopped.Register(
    procedure
    begin
      Writeln('ApplicationStopped : el host se ha parado');
    end);

  var Diagnostics := THostDiagnostics.Create(Host);
  try
    Host.Start;
    try
      Writeln(Diagnostics.Describe);
      // Lo mismo que un Ctrl+C: WaitForShutdown vuelve en cuanto se pide la parada.
      Lifetime.StopApplication;
      Host.WaitForShutdown;
    finally
      StopAndNil(Host);
    end;
    Writeln(Diagnostics.Describe);
  finally
    Diagnostics.Free;
  end;
end.
