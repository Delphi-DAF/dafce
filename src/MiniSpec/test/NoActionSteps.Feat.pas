unit NoActionSteps.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  TNoActionContext = class
  public
    GivenExecuted: Boolean;
    WhenExecuted: Boolean;
    ThenExecuted: Boolean;
  end;

var
  /// <summary>
  /// Tracking variables to verify NoAction step behavior
  /// </summary>
  NoActionGivenExecuted: Boolean;
  NoActionWhenExecuted: Boolean;
  WhenAfterNoActionGivenExecuted: Boolean;
  ThenAfterNoActionGivenExecuted: Boolean;
  ThenAfterNoActionWhenExecuted: Boolean;

initialization

NoActionGivenExecuted := False;
NoActionWhenExecuted := False;
WhenAfterNoActionGivenExecuted := False;
ThenAfterNoActionGivenExecuted := False;
ThenAfterNoActionWhenExecuted := False;

Feature('''
NoAction Steps @noaction

  As a test author
  I want to mark steps as no-action (descriptive only)
  So I can have steps that appear in reports but don't need implementation
''')

.UseWorld<TNoActionContext>

.After('Verify NoAction steps allowed subsequent steps to execute', procedure
  begin
    // NoAction steps should NOT execute their original code
    Expect(NoActionGivenExecuted).ToBeFalse;
    Expect(NoActionWhenExecuted).ToBeFalse;
    // But subsequent steps SHOULD execute (unlike Pending)
    Expect(WhenAfterNoActionGivenExecuted).ToBeTrue;
    Expect(ThenAfterNoActionGivenExecuted).ToBeTrue;
    Expect(ThenAfterNoActionWhenExecuted).ToBeTrue;
  end)

.Scenario('NoAction Given allows When and Then to execute')
  .Given('a step marked as no-action', procedure(Ctx: TNoActionContext)
    begin
      NoActionGivenExecuted := True;  // This should NOT execute
    end).NoAction
  .When('this should execute', procedure(Ctx: TNoActionContext)
    begin
      WhenAfterNoActionGivenExecuted := True;
    end)
  .&Then('this should also execute', procedure(Ctx: TNoActionContext)
    begin
      ThenAfterNoActionGivenExecuted := True;
    end)

.Scenario('NoAction When allows Then to execute')
  .Given('a normal step', procedure(Ctx: TNoActionContext)
    begin
      Ctx.GivenExecuted := True;
    end)
  .When('a step marked as no-action', procedure(Ctx: TNoActionContext)
    begin
      NoActionWhenExecuted := True;  // This should NOT execute
    end).NoAction
  .&Then('this should execute', procedure(Ctx: TNoActionContext)
    begin
      ThenAfterNoActionWhenExecuted := True;
    end);

end.
