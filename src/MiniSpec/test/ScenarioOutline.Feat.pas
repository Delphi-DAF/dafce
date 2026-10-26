unit ScenarioOutline.Feat;

// Un Scenario Outline se ejecuta una vez por fila de sus Examples, y una sola, como un escenario, si
// no los tiene. Las ejecuciones son anidadas porque sus escenarios quedan pendientes o undefined a
// proposito, sin tocar la suite de fuera.

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Reporter.Console;

type
  /// Cuenta lo que se ejecuta en la ejecucion anidada. Es de clase porque cada fila tiene su World.
  TRowsWorld = class
  public
    // N antes de los class var: un campo declarado detras de un class var tambien es de clase.
    N: Integer;
    class var WhenRuns: Integer;
    class var ThenRuns: Integer;
    class procedure Reset; static;
  end;

  /// El informe de consola tal cual, sin pasar por la consola.
  TOutlineConsole = class(TConsoleReporter)
  strict private
    FText: TStringBuilder;
  protected
    procedure Emit(const Text: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Text: string;
  end;

  TScenarioOutlineWorld = class
  strict private
    FRun: TSpecRun;
    // La ejecucion mantiene vivo el listener, que guarda su interfaz: esta referencia es para leerlo.
    FConsole: TOutlineConsole;
  public
    Ran: string;
    Printed: string;
    constructor Create;
    destructor Destroy; override;
    property Run: TSpecRun read FRun;
    property Console: TOutlineConsole read FConsole;
  end;

class procedure TRowsWorld.Reset;
begin
  WhenRuns := 0;
  ThenRuns := 0;
end;

constructor TOutlineConsole.Create;
begin
  inherited Create;
  FText := TStringBuilder.Create;
end;

destructor TOutlineConsole.Destroy;
begin
  FText.Free;
  inherited;
end;

procedure TOutlineConsole.Emit(const Text: string);
begin
  FText.Append(Text);
end;

function TOutlineConsole.Text: string;
begin
  Result := FText.ToString;
end;

constructor TScenarioOutlineWorld.Create;
begin
  inherited;
  FRun := TSpecRun.Create('Nested');
  FConsole := TOutlineConsole.Create;
  FRun.AddListener(FConsole);
end;

destructor TScenarioOutlineWorld.Destroy;
begin
  FRun.Free;
  inherited;
end;

initialization

Feature('''
Scenario Outline @scenario-outline

  As whoever writes a Scenario Outline
  I want each row to keep what its steps declare, and an outline without rows to run once
  So a pending step or a missing table cannot leave an outline passing or out of the report
''')

.UseWorld<TScenarioOutlineWorld>

.Rule('A step declared Pending in an outline is pending in every row')

  .Scenario('A pending When in an outline of two rows')
    .Given('a run with a Scenario Outline of two rows whose When is declared Pending',
      procedure(W: TScenarioOutlineWorld)
      begin
        TRowsWorld.Reset;
        Feature('Its outline has a pending When', W.Run.Suite)
          .UseWorld<TRowsWorld>
          .ScenarioOutline('Row <N>')
            .When('it does what is not written yet', procedure(R: TRowsWorld)
              begin
                Inc(TRowsWorld.WhenRuns);
              end).Pending
            .&Then('it checks what it got', procedure(R: TRowsWorld)
              begin
                Inc(TRowsWorld.ThenRuns);
              end)
            .Examples([['N'], [1], [2]]);
      end)
    .When('the suite is executed', procedure(W: TScenarioOutlineWorld)
      begin
        W.Run.Execute;
        W.Ran := Format('%d When, %d Then', [TRowsWorld.WhenRuns, TRowsWorld.ThenRuns]);
      end)
    .&Then('both rows count as pending, and none as passed', procedure(W: TScenarioOutlineWorld)
      begin
        Expect(Format('%d pending, %d passed', [W.Run.PendingCount, W.Run.PassCount])).ToEqual('2 pending, 0 passed');
      end)
    .&And('neither the When nor the Then runs in any row', procedure(W: TScenarioOutlineWorld)
      begin
        Expect(W.Ran).ToEqual('0 When, 0 Then');
      end)

.Rule('A Scenario Outline without Examples runs as a scenario')

  .Scenario('An outline missing its Examples')
    .Given('a run whose feature has a Scenario Outline without Examples, with a When that records it ran and a Then that passes',
      procedure(W: TScenarioOutlineWorld)
      begin
        TRowsWorld.Reset;
        Feature('Its outline has no rows', W.Run.Suite)
          .UseWorld<TRowsWorld>
          .ScenarioOutline('An outline without rows')
            .When('it records that it ran', procedure(R: TRowsWorld)
              begin
                Inc(TRowsWorld.WhenRuns);
              end)
            .&Then('it passes', procedure(R: TRowsWorld)
              begin
              end);
      end)
    .When('the suite is executed', procedure(W: TScenarioOutlineWorld)
      begin
        W.Run.Execute;
        W.Ran := Format('%d passed, the When ran %d time(s)', [W.Run.PassCount, TRowsWorld.WhenRuns]);
        W.Printed := W.Console.Text;
      end)
    .&Then('the outline counts as one passed scenario, and its When ran once', procedure(W: TScenarioOutlineWorld)
      begin
        Expect(W.Ran).ToEqual('1 passed, the When ran 1 time(s)');
      end)
    .&And('the console report names it with its steps', procedure(W: TScenarioOutlineWorld)
      begin
        Expect(W.Printed).ToContain('An outline without rows');
        Expect(W.Printed).ToContain('it records that it ran');
      end)

  .Scenario('A step with no binding in an outline without Examples')
    .Given('a run with a Scenario Outline without Examples that has a step with neither lambda nor binding',
      procedure(W: TScenarioOutlineWorld)
      begin
        TRowsWorld.Reset;
        Feature('Its outline has no rows nor binding', W.Run.Suite)
          .UseWorld<TRowsWorld>
          .ScenarioOutline('An outline without rows nor binding')
            .When('a step that no binding of ScenarioOutline.Feat answers')
            .&Then('it passes', procedure(R: TRowsWorld)
              begin
              end);
      end)
    .When('the suite is executed', procedure(W: TScenarioOutlineWorld)
      begin
        W.Run.Execute;
      end)
    .&Then('the outline counts as undefined, as that step would in a Scenario', procedure(W: TScenarioOutlineWorld)
      begin
        Expect(Format('%d undefined, %d passed', [W.Run.UndefinedCount, W.Run.PassCount])).ToEqual('1 undefined, 0 passed');
      end);

end.
