unit Lifecycle.Feat;

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
  TLifecycleWorld = class
  public
    Trace: string;
  end;

initialization

Feature('''
Feature A host stops in the reverse order it started @hosting @lifetime

  As a developer whose service releases what another one is still using
  I want the hosted services stopped in the reverse order of their registration
  So that stopping never leaves a running service without what it depends on
''')

.UseWorld<TLifecycleWorld>

.Rule('What starts first stops last')

  .Scenario('Two hosted services leave a trace of their start and of their stop')
    .When('a host with two hosted services is started and then stopped',
      procedure(W: TLifecycleWorld)
      begin
        TTrace.Reset;
        var Builder := THostBuilder.Create;
        var Owner: IHostApplicationBuilder := Builder;
        Builder.Services.AddHostedService<TFirstService>;
        Builder.Services.AddHostedService<TSecondService>;
        var Host := Builder.Build;
        Host.Start;
        Host.Stop;
        W.Trace := TTrace.Steps;
      end)
    .&Then('stopping mirrors starting, which is the order the container releases in',
      procedure(W: TLifecycleWorld)
      begin
        // The whole statement in one value on purpose: asserting the halves apart would let a
        // stop order that is merely different, rather than reversed, pass.
        Expect(W.Trace).ToEqual('up:1,up:2,down:2,down:1');
      end);

end.
