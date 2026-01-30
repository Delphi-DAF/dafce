unit PendingSteps.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  TPendingContext = class
  public
    GivenExecuted: Boolean;
    WhenExecuted: Boolean;
    ThenExecuted: Boolean;
  end;

var
  /// <summary>
  /// Tracking variables to verify step execution
  /// </summary>
  WhenExecutedAfterPendingGiven: Boolean;
  ThenExecutedAfterPendingGiven: Boolean;
  ThenExecutedAfterPendingWhen: Boolean;

initialization

WhenExecutedAfterPendingGiven := False;
ThenExecutedAfterPendingGiven := False;
ThenExecutedAfterPendingWhen := False;

Feature('''
Pending Steps @pending

  As a test author
  I want to mark steps as pending
  So I can write specs before implementing them
''')

.UseWorld<TPendingContext>

.After('Verify pending steps behavior', procedure
  begin
    // Verify that When and Then did NOT execute after pending Given
    Expect(WhenExecutedAfterPendingGiven).ToBeFalse;
    Expect(ThenExecutedAfterPendingGiven).ToBeFalse;
    // Verify that Then did NOT execute after pending When
    Expect(ThenExecutedAfterPendingWhen).ToBeFalse;
  end)

.Scenario('Pending Given stops execution of When and Then')
  .Given('a step marked as pending', procedure(Ctx: TPendingContext)
    begin
      Ctx.GivenExecuted := True;
    end).Pending
  .When('this should not execute', procedure(Ctx: TPendingContext)
    begin
      WhenExecutedAfterPendingGiven := True;
    end)
  .&Then('this should not execute either', procedure(Ctx: TPendingContext)
    begin
      ThenExecutedAfterPendingGiven := True;
    end)

.Scenario('Pending When stops execution of Then')
  .Given('a normal step', procedure(Ctx: TPendingContext)
    begin
      Ctx.GivenExecuted := True;
    end)
  .When('a step marked as pending', procedure(Ctx: TPendingContext)
    begin
      Ctx.WhenExecuted := True;
    end).Pending
  .&Then('this should not execute', procedure(Ctx: TPendingContext)
    begin
      ThenExecutedAfterPendingWhen := True;
    end);

end.
