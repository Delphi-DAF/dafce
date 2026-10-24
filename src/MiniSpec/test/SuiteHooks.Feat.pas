unit SuiteHooks.Feat;

// Los hooks de una suite se ejecutan una vez, antes y despues de todas sus features, como los hooks
// globales de Cucumber. Las ejecuciones son anidadas porque sus hooks lanzan a proposito, sin tocar
// la suite de fuera.

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Reporter.Console;

type
  /// Lo que se ejecuto en la ejecucion anidada, en orden. Es de clase porque los hooks de suite no
  /// reciben World.
  THookedWorld = class
  public
    class var Trace: string;
    class procedure Note(const What: string); static;
  end;

  /// El informe de consola tal cual, sin pasar por la consola.
  THooksConsole = class(TConsoleReporter)
  strict private
    FText: TStringBuilder;
  protected
    procedure Emit(const Text: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Text: string;
  end;

  TSuiteHooksWorld = class
  strict private
    FRun: TSpecRun;
    // La ejecucion mantiene vivo el listener, que guarda su interfaz: esta referencia es para leerlo.
    FConsole: THooksConsole;
  public
    Ran: string;
    Printed: string;
    constructor Create;
    destructor Destroy; override;
    property Run: TSpecRun read FRun;
    property Console: THooksConsole read FConsole;
  end;

class procedure THookedWorld.Note(const What: string);
begin
  if Trace <> '' then
    Trace := Trace + ' ';
  Trace := Trace + What;
end;

constructor THooksConsole.Create;
begin
  inherited Create;
  FText := TStringBuilder.Create;
end;

destructor THooksConsole.Destroy;
begin
  FText.Free;
  inherited;
end;

procedure THooksConsole.Emit(const Text: string);
begin
  FText.Append(Text);
end;

function THooksConsole.Text: string;
begin
  Result := FText.ToString;
end;

constructor TSuiteHooksWorld.Create;
begin
  inherited;
  FRun := TSpecRun.Create('Nested');
  FConsole := THooksConsole.Create;
  FRun.AddListener(FConsole);
end;

destructor TSuiteHooksWorld.Destroy;
begin
  FRun.Free;
  inherited;
end;

initialization

Feature('''
Suite hooks @suite-hooks

  As whoever prepares and cleans up what a whole suite shares
  I want the hooks of the suite to run once around all its features, and a hook that raises to count
  So a suite whose set-up or clean-up failed cannot come out green
''')

.UseWorld<TSuiteHooksWorld>

.Rule('The hooks of a suite run once, before and after its features')

  .Scenario('The After of the suite runs after everything')
    .Given('a run with a Before and an After hook of the suite, and a feature with a scenario that fails',
      procedure(W: TSuiteHooksWorld)
      begin
        THookedWorld.Trace := '';
        W.Run.Suite.AddBeforeHook('it sets up', procedure begin THookedWorld.Note('before'); end);
        W.Run.Suite.AddAfterHook('it cleans up', procedure begin THookedWorld.Note('after'); end);
        Feature('Its scenario fails', W.Run.Suite)
          .UseWorld<THookedWorld>
          .Scenario('It fails on purpose')
            .When('it notes that it ran', procedure(H: THookedWorld)
              begin
                THookedWorld.Note('scenario');
              end)
            .&Then('one is not two', procedure(H: THookedWorld)
              begin
                Expect(1).ToEqual(2);
              end);
      end)
    .When('the suite is executed', procedure(W: TSuiteHooksWorld)
      begin
        W.Run.Execute;
        W.Ran := THookedWorld.Trace;
      end)
    .&Then('what ran, in order, is the Before, the scenario and the After', procedure(W: TSuiteHooksWorld)
      begin
        Expect(W.Ran).ToEqual('before scenario after');
      end)

.Rule('A Before hook of the suite that raises stops the run')

  .Scenario('The Before of the suite raises')
    .Given('a run whose suite has two Before hooks, the first of which raises, an After hook, and a feature with a scenario',
      procedure(W: TSuiteHooksWorld)
      begin
        THookedWorld.Trace := '';
        W.Run.Suite.AddBeforeHook('it cannot set up',
          procedure begin raise Exception.Create('the suite cannot start'); end);
        W.Run.Suite.AddBeforeHook('it sets up the rest', procedure begin THookedWorld.Note('before2'); end);
        W.Run.Suite.AddAfterHook('it cleans up', procedure begin THookedWorld.Note('after'); end);
        Feature('Its scenario would pass', W.Run.Suite)
          .UseWorld<THookedWorld>
          .Scenario('It passes')
            .When('it notes that it ran', procedure(H: THookedWorld)
              begin
                THookedWorld.Note('scenario');
              end);
      end)
    .When('the suite is executed', procedure(W: TSuiteHooksWorld)
      begin
        W.Run.Execute;
        W.Ran := THookedWorld.Trace;
        W.Printed := W.Console.Text;
      end)
    .&Then('the second Before and the After run, and the scenario does not', procedure(W: TSuiteHooksWorld)
      begin
        Expect(W.Ran).ToEqual('before2 after');
      end)
    .&And('the console report carries the message of the hook', procedure(W: TSuiteHooksWorld)
      begin
        Expect(W.Printed).ToContain('the suite cannot start');
      end)
    .&And('it counts one failure, and the exit code is 1', procedure(W: TSuiteHooksWorld)
      begin
        Expect(Format('%d failed, exit code %d', [W.Run.FailCount, W.Run.ExitCode])).ToEqual('1 failed, exit code 1');
      end)

.Rule('An After hook of the suite that raises counts as a failure')

  .Scenario('The After of the suite raises')
    .Given('a run with a scenario that passes, and two After hooks of the suite, the first of which raises',
      procedure(W: TSuiteHooksWorld)
      begin
        THookedWorld.Trace := '';
        W.Run.Suite.AddAfterHook('it cannot clean up',
          procedure begin raise Exception.Create('the suite cannot clean up'); end);
        W.Run.Suite.AddAfterHook('it cleans up the rest', procedure begin THookedWorld.Note('after2'); end);
        Feature('Its scenario passes', W.Run.Suite)
          .UseWorld<THookedWorld>
          .Scenario('It passes')
            .When('it notes that it ran', procedure(H: THookedWorld)
              begin
                THookedWorld.Note('scenario');
              end);
      end)
    .When('the suite is executed', procedure(W: TSuiteHooksWorld)
      begin
        W.Run.Execute;
        W.Ran := THookedWorld.Trace;
        W.Printed := W.Console.Text;
      end)
    .&Then('the scenario counts as passed, and there is one failure more', procedure(W: TSuiteHooksWorld)
      begin
        Expect(Format('%d passed, %d failed', [W.Run.PassCount, W.Run.FailCount])).ToEqual('1 passed, 1 failed');
      end)
    .&And('the second After hook runs', procedure(W: TSuiteHooksWorld)
      begin
        Expect(W.Ran).ToEqual('scenario after2');
      end)
    .&And('the console report carries the message of the hook, and the exit code is 1', procedure(W: TSuiteHooksWorld)
      begin
        Expect(W.Printed).ToContain('the suite cannot clean up');
        Expect(W.Run.ExitCode).ToEqual(1);
      end);

end.
