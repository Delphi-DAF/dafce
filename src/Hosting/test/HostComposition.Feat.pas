unit HostComposition.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  StartupTasks.Feat;

type
  TTwoHostsWorld = class
  public
    FirstCount, SecondCount: Integer;
    FirstTrace, SecondTrace: string;
  end;

/// Monta un host con la misma composicion, lo arranca, lo para y devuelve cuantos tipos de
/// servicio quedaron registrados. Es la medida de "el segundo host se monta como el primero".
function BuildStartAndStop(out Trace: string): Integer;
begin
  TTrace.Reset;
  var Registered := 0;
  var Builder: IHostBuilder := THostBuilder.Create;
  Builder.ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<ISeed, TSeed>;
      Services.AddStartupTask<TSeedingTask>;
      Services.AddHostedService<TConsumerService>;
      Registered := Services.Count;
    end);

  var Host := Builder.Build;
  Host.Start;
  try
    Trace := TTrace.Steps;
  finally
    Host.Stop;
  end;
  Result := Registered;
end;

initialization

Feature('''
Feature Host composition is reproducible @hosting @composition

  As a spec author
  I want a process to be able to build a host, tear it down and build another one
  So that a harness sees exactly the services it registered, in any order
''')

.UseWorld<TTwoHostsWorld>

.Rule('Composing twice in one process gives the same result')

  .Scenario('Two hosts in the same process are put together the same way')
    .When('a host is built, torn down, and another one is built with the same composition',
      procedure(W: TTwoHostsWorld)
      begin
        W.FirstCount := BuildStartAndStop(W.FirstTrace);
        W.SecondCount := BuildStartAndStop(W.SecondTrace);
      end)
    .&Then('the second has the same registered services as the first',
      procedure(W: TTwoHostsWorld)
      begin
        Expect(W.FirstCount).ToBeGreaterThan(0);
        Expect(W.SecondCount).ToEqual(W.FirstCount);
      end)
    .&And('and it starts in the same order',
      procedure(W: TTwoHostsWorld)
      begin
        Expect(W.FirstTrace).ToEqual('task,service');
        Expect(W.SecondTrace).ToEqual(W.FirstTrace);
      end);

end.
