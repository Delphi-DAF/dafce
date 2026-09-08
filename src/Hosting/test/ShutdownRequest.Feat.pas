unit ShutdownRequest.Feat;

interface

type
  TShutdownWorld = class
  public
    Report: string;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Threading,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting;

/// Monta un host, lo arranca, deja que el llamador mire, y lo para. El Pointer(Host) := nil es
/// porque Stop destruye el host --THost es TNoRefCountObject--, asi que el finalizador de la
/// variable llamaria _Release sobre memoria ya devuelta.
function WithStartedHost(const Inspect: TFunc<IHost, string>): string;
begin
  var Builder := THost.CreateBuilder;
  var Host := Builder.Build;
  Host.Start;
  try
    Result := Inspect(Host);
  finally
    Host.Stop;
    Pointer(Host) := nil;
  end;
end;

initialization

Feature('''
Feature Pedir la parada de la aplicacion @hosting @lifetime

  Como quien escribe el .dpr de un servicio que se para solo
  Quiero que pedir la parada por codigo valga lo mismo que un Ctrl+C
  Para no descubrir en produccion que StopApplication no paraba nada
''')

.UseWorld<TShutdownWorld>

.Rule('El camino de senal y el de codigo son el mismo')

  // En el linaje hay UNA sola senal de apagado: el manejador de consola llama a StopApplication,
  // y quien espera la parada observa ApplicationStopping. Aqui el manejador invoca
  // TShutdownHook.OnShutdownRequested, asi que ese TProc es el punto donde las dos mitades se
  // unen -- o no se unen.
  .Scenario('Un host arrancado reclama el camino de senal')
    .Given('un host arrancado',
      procedure(W: TShutdownWorld)
      begin
        W.Report := WithStartedHost(
          function(Host: IHost): string
          begin
            Result := BoolToStr(Assigned(TShutdownHook.OnShutdownRequested), True);
          end);
      end)
    .When('se pregunta quien atiende la peticion de parada').NoAction
    .&Then('hay alguien atendiendola',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)

  .Scenario('Pedir la parada por ese camino cancela ApplicationStopping')
    .Given('un host arrancado al que se le pide la parada como lo haria un Ctrl+C',
      procedure(W: TShutdownWorld)
      begin
        W.Report := WithStartedHost(
          function(Host: IHost): string
          begin
            var Lifetime: IHostApplicationLifetime :=
              Host.Services.GetService<IHostApplicationLifetime>;
            if not Assigned(Lifetime) then
              Exit('sin IHostApplicationLifetime registrado');
            // Llamar a un TProc sin asignar revienta, y una violacion de acceso no dice cual
            // era el hueco. Nombrarlo si.
            if not Assigned(TShutdownHook.OnShutdownRequested) then
              Exit('OnShutdownRequested sin asignar: un Ctrl+C no pide la parada a nadie');
            // Los parentesis no son adorno: un TProc a pelo es el valor, no la llamada,
            // y da E2014 con la cascada dos lambdas mas abajo.
            TShutdownHook.OnShutdownRequested();
            Result := BoolToStr(Lifetime.ApplicationStopping.IsCancellationRequested, True);
          end);
      end)
    .When('se consulta el token de parada').NoAction
    .&Then('esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end);

end.
