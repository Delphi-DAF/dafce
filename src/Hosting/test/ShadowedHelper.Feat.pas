unit ShadowedHelper.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  StartupTasks.Feat;

initialization

Feature('''
Feature Registrar cuando el record helper esta ensombrecido @hosting @helpers

  Como quien compone una aplicacion que usa varios modulos de DAF a la vez
  Quiero una via no generica para registrar tareas de arranque y servicios alojados
  Para que el orden de mi clausula uses no decida que puedo llamar
''')

.UseWorld<TStartupWorld>

.Rule('La fachada registra lo mismo que el helper')

  // Delphi solo hace visible UN record helper por tipo, y en este arbol hay dos que extienden
  // IServiceCollection: el de Hosting y el de MediatR. Quien haga uses de los dos ve uno solo,
  // segun el orden, asi que la clase Hosting es la unica via que le queda. Es la misma
  // convencion que MediatR, cuya fachada si tiene llamadores; esta no tenia ninguno.
  .Scenario('Una tarea de arranque y un servicio alojado registrados solo por la fachada')
    .Given('los dos puestos por Hosting.AddStartupTask y Hosting.AddHostedService',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Hosting.AddStartupTask(Services, TSeedingTask);
            Hosting.AddHostedService(Services, TConsumerService);
          end);
        W.StartHost;
      end)
    .When('se arranca el host').NoAction
    // Un solo Expect cubre las dos fachadas y el orden: si alguna registrase bajo el contrato
    // equivocado, su paso no aparece, y el texto dice cual falta en vez de un booleano pelado.
    .&Then('la tarea corrio, y antes que el servicio',
      procedure(W: TStartupWorld)
      begin
        Expect(TTrace.Steps).ToEqual('task,service');
      end)

  .Scenario('El arranque llega hasta el final')
    .Given('un host compuesto solo con las fachadas',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Hosting.AddStartupTask(Services, TSeedingTask);
            Hosting.AddHostedService(Services, TConsumerService);
          end);
        W.StartHost;
      end)
    .When('se consulta si la aplicacion se declaro arrancada').NoAction
    .&Then('lo esta, y sin fallo',
      procedure(W: TStartupWorld)
      begin
        Expect(W.Failure).ToEqual('');
        Expect(W.StartedNotified).ToBeTrue;
      end);

end.
