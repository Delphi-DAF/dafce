program HostQuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.Extensions.Hosting,
  Daf.Hosting,
  HostQuickStart.Worker in 'HostQuickStart.Worker.pas';

// El host generico, sin ninguna variante de la familia por encima: se compone, se construye, y lo
// registrado arranca y para con el.
//
//   HostQuickStart  -> arranca el servicio, informa, y lo para
begin
  var Builder := THostBuilder.Create;
  // El builder es contado por referencia: sin un dueno declarado nadie lo libera.
  var Owner: IHostApplicationBuilder := Builder;

  Builder.AddHostedService<TGreetingWorker>;

  var Host := Builder.Build;
  Host.Start;
  try
    Writeln('El host esta arrancado');
  finally
    Host.Stop;
  end;
end.
