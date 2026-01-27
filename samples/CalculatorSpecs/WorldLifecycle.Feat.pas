unit WorldLifecycle.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

var
  LifecycleLog: TStringList;
  CreateCount: Integer = 0;
  DestroyCount: Integer = 0;

type
  TLifecycleWorld = class(TFeatureWorld)
  private
    FLogEntry: string;
    function BuildLogEntry(const Action: string): string;
  public
    N: Integer;
    X: string;
    constructor Create;
    destructor Destroy; override;
    procedure LogCreate;
  end;

function TLifecycleWorld.BuildLogEntry(const Action: string): string;
var
  Ctx: ISpecContext;
  FeatureName, RuleName, ScenarioName: string;
  Rule: IRule;
  Scenario: IScenario;
  Outline: IScenarioOutline;
begin
  Ctx := Self as ISpecContext;

  // Feature title (sin tags)
  if Assigned(Ctx.CurrentFeature) then
  begin
    FeatureName := Ctx.CurrentFeature.Title;
    // Quitar tags del título si los hay
    if Pos('@', FeatureName) > 0 then
      FeatureName := Trim(Copy(FeatureName, 1, Pos('@', FeatureName) - 1));
  end
  else
    FeatureName := '?';

  // Rule - solo si es explícito (no sikImplicitRule)
  Rule := Ctx.CurrentRule;
  if Assigned(Rule) and (Rule.Kind = sikRule) then
    RuleName := Rule.Description + '/'
  else
    RuleName := '';

  // Scenario
  Scenario := Ctx.CurrentScenario;
  if Assigned(Scenario) then
  begin
    case Scenario.Kind of
      sikExample:
        begin
          // Es un Example de un Outline - mostrar solo "OutlineDesc: ExampleValues"
          if Supports(Scenario.Parent, IScenarioOutline, Outline) then
          begin
            // Extraer solo la parte con los valores del example
            ScenarioName := Outline.Description;
            // Reemplazar placeholders con valores reales del Example
            // El Description del Example tiene formato "OutlineDesc" con valores sustituidos
            // pero a veces tiene formato "#{N}" sin sustituir
            if Pos('#{', Scenario.Description) > 0 then
              ScenarioName := ScenarioName + ' [' + Scenario.Description + ']'
            else
              ScenarioName := Scenario.Description;
          end
          else
            ScenarioName := Scenario.Description;
        end;
      sikScenarioOutline:
        // No deberíamos llegar aquí - el CurrentScenario debería ser el Example
        ScenarioName := Scenario.Description + ' (outline)';
    else
      ScenarioName := Scenario.Description;
    end;
  end
  else
    ScenarioName := '?';

  Result := Format('%s: %s/%s%s', [Action, FeatureName, RuleName, ScenarioName]);
end;

constructor TLifecycleWorld.Create;
begin
  inherited;
  Inc(CreateCount);
  FLogEntry := '';
end;

procedure TLifecycleWorld.LogCreate;
begin
  if FLogEntry = '' then
  begin
    FLogEntry := BuildLogEntry('Create');
    LifecycleLog.Add(FLogEntry);
  end;
end;

destructor TLifecycleWorld.Destroy;
begin
  Inc(DestroyCount);
  if FLogEntry <> '' then
    LifecycleLog.Add(StringReplace(FLogEntry, 'Create:', 'Destroy:', []))
  else
    LifecycleLog.Add('Destroy: (no context - World created but no step executed)');
  inherited;
end;

initialization

LifecycleLog := TStringList.Create;

Feature('''
World Lifecycle @meta @lifecycle

  As a framework developer
  I want to verify World instances are properly created and destroyed
  So I can ensure there are no memory leaks
''')

.UseWorld<TLifecycleWorld>

// === Scenarios directly in feature (no rule) ===
.Scenario('Direct scenario without rule')
  .Given('a spec runs directly in the feature', procedure(World: TLifecycleWorld)
    begin
      World.LogCreate;
      Expect(Assigned(World)).ToEqual(True);
    end)
  .When('I check the feature', procedure(World: TLifecycleWorld)
    begin
      Expect((World as ISpecContext).CurrentFeature).ToNotBeNull;
    end)
  .&Then('the feature title is correct', procedure(World: TLifecycleWorld)
    begin
      Expect((World as ISpecContext).CurrentFeature.Title).ToContain('World Lifecycle');
    end)

.Scenario('Another direct scenario')
  .Given('another spec runs directly', procedure(World: TLifecycleWorld)
    begin
      World.LogCreate;
    end)
  .When('executed', procedure(World: TLifecycleWorld)
    begin
      // noop
    end)
  .&Then('it gets its own World', procedure(World: TLifecycleWorld)
    begin
      Expect(Assigned(World)).ToEqual(True);
    end)

// === Outline directly in feature (no rule) - must be before Rules ===
.ScenarioOutline('Direct outline: X=<X>')
  .Given('an outline runs directly', procedure(World: TLifecycleWorld)
    begin
      World.LogCreate;
    end)
  .When('I check the value', procedure(World: TLifecycleWorld)
    begin
      Expect(World.X <> '').ToEqual(True);
    end)
  .&Then('each example works', procedure(World: TLifecycleWorld)
    begin
      Expect(Assigned(World)).ToEqual(True);
    end)
  .Examples(
    [['X'],
     ['A'],
     ['B']])

// === First Rule with scenarios ===
.Rule('Basic Rules')
  .Scenario('Scenario in first rule')
    .Given('a spec runs inside a rule', procedure(World: TLifecycleWorld)
      begin
        World.LogCreate;
      end)
    .When('I check the scenario', procedure(World: TLifecycleWorld)
      begin
        Expect((World as ISpecContext).CurrentScenario).ToNotBeNull;
      end)
    .&Then('the scenario description is correct', procedure(World: TLifecycleWorld)
      begin
        Expect((World as ISpecContext).CurrentScenario.Description).ToContain('first rule');
      end)

  .Scenario('Another scenario in first rule')
    .Given('another spec in the same rule', procedure(World: TLifecycleWorld)
      begin
        World.LogCreate;
      end)
    .When('executed', procedure(World: TLifecycleWorld)
      begin
        // noop
      end)
    .&Then('it has its own World', procedure(World: TLifecycleWorld)
      begin
        Expect(Assigned(World)).ToEqual(True);
      end)

// === Second Rule with scenario and outline ===
.Rule('Advanced Rules')
  .Scenario('Scenario in second rule')
    .Given('a spec in the second rule', procedure(World: TLifecycleWorld)
      begin
        World.LogCreate;
      end)
    .When('executed', procedure(World: TLifecycleWorld)
      begin
        // noop
      end)
    .&Then('it works', procedure(World: TLifecycleWorld)
      begin
        Expect((World as ISpecContext).CurrentScenario.Description).ToContain('second rule');
      end)

  .ScenarioOutline('Outline in rule: N=<N>')
    .Given('example <N> starts', procedure(World: TLifecycleWorld)
      begin
        World.LogCreate;
      end)
    .When('I check the value', procedure(World: TLifecycleWorld)
      begin
        Expect(World.N > 0).ToEqual(True);
      end)
    .&Then('each example has its own World', procedure(World: TLifecycleWorld)
      begin
        Expect(Assigned(World)).ToEqual(True);
      end)
    .Examples(
      [['N'],
       [1],
       [2],
       [3]])
;

finalization
  WriteLn;
  WriteLn('=== World Lifecycle Log ===');
  WriteLn(LifecycleLog.Text);
  WriteLn(Format('Create: %d, Destroy: %d', [CreateCount, DestroyCount]));

  if CreateCount <> DestroyCount then
    raise Exception.CreateFmt(
      'World lifecycle mismatch! Create: %d, Destroy: %d',
      [CreateCount, DestroyCount]);

  WriteLn('OK: All Worlds properly destroyed');
  LifecycleLog.Free;

end.
