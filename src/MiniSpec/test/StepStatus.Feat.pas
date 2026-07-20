unit StepStatus.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  TStepStatusContext = class
  end;

var
  WhenRanAfterUndefined: Boolean;
  ThenRanAfterUndefined: Boolean;
  WhenRanAfterPending:   Boolean;
  ThenRanAfterPending:   Boolean;

initialization

WhenRanAfterUndefined := False;
ThenRanAfterUndefined := False;
WhenRanAfterPending   := False;
ThenRanAfterPending   := False;

Feature('''
Step Status Gherkin Alignment @step-status

  Steps not executed because of a previous Pending or Undefined step
  must be treated as Skipped — they do not run.
''')

.UseWorld<TStepStatusContext>

.After('verify steps after Undefined and Pending did not execute', procedure
  begin
    Expect(WhenRanAfterUndefined).ToBeFalse;
    Expect(ThenRanAfterUndefined).ToBeFalse;
    Expect(WhenRanAfterPending).ToBeFalse;
    Expect(ThenRanAfterPending).ToBeFalse;
  end)

.Scenario('Steps after Undefined step do not execute')
  .Given('a step with no implementation', procedure(Ctx: TStepStatusContext)
    begin
      SpecContext.Step.MarkAsUndefined;
    end)
  .When('this step should not run', procedure(Ctx: TStepStatusContext)
    begin
      WhenRanAfterUndefined := True;
    end)
  .&Then('this step should not run either', procedure(Ctx: TStepStatusContext)
    begin
      ThenRanAfterUndefined := True;
    end)

.Scenario('Steps after Pending step do not execute')
  .Given('a step intentionally marked pending', procedure(Ctx: TStepStatusContext)
    begin
      SpecContext.Step.MarkAsPending;
    end)
  .When('this step should not run', procedure(Ctx: TStepStatusContext)
    begin
      WhenRanAfterPending := True;
    end)
  .&Then('this step should not run either', procedure(Ctx: TStepStatusContext)
    begin
      ThenRanAfterPending := True;
    end);

// ── Tag placement restriction ──────────────────────────────────────────────────

Feature('''
Tag Placement Restriction @tag-placement

  Per the Gherkin spec, tags are only valid on Feature, Rule,
  Scenario, ScenarioOutline and Examples.
  Tags written in step descriptions must be ignored silently.
''')

.UseWorld<TStepStatusContext>

.Scenario('At-sign in step description is not extracted as a tag')
  .Given('a step whose description contains @not-a-tag literally', procedure(Ctx: TStepStatusContext)
    begin
      Expect(SpecContext.Step.Tags.Contains('not-a-tag')).ToBeFalse;
    end)
  .When('we inspect the step tags', procedure(Ctx: TStepStatusContext)
    begin
      Expect(SpecContext.Step.Tags.Contains('not-a-tag')).ToBeFalse;
    end)
  .&Then('no tag was extracted from the step description', procedure(Ctx: TStepStatusContext)
    begin
      Expect(SpecContext.Step.Tags.Contains('not-a-tag')).ToBeFalse;
    end);

end.
