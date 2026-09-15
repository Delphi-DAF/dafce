unit SpecSuite.Feat;

interface

var
  /// Marcadores para verificar ejecución de Suite hooks
  SuiteBeforeHookCalled: Boolean;
  SuiteAfterHookCalled: Boolean;
  SuiteTitle: string;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Contexto individual para cada escenario de esta feature.
  /// </summary>
  TScenarioContext = class
  public
    /// Lo que el When lee del arranque, para que el Then lo describa. Texto y no Boolean: un
    /// campo Boolean sin asignar vale False, asi que una comprobacion de False pasaria igual
    /// aunque el When no llegara a correr.
    Report: string;
  end;

initialization

Feature('''
Feature SpecSuite @suite

  As a test author
  I want to group all features under a Suite
  So I can provide a title and run hooks before/after all tests
''')

.UseWorld<TScenarioContext>

.Scenario('Suite has the title set via Category()')
  // El evento ocurrio fuera del escenario y no hay forma de provocarlo desde aqui:
  // Category('MiniSpec Framework Tests') se llama en el programa, antes de que el runner arranque.
  // Lo que a este escenario le toca es constatar que ya ocurrio.
  .Given('a runner started by a program that called Category').NoAction
  .When('the suite is asked for the title it was configured with',
    procedure(Ctx: TScenarioContext)
    begin
      Ctx.Report := SuiteTitle;
    end)
  .&Then('the Suite Title matches what was set',
    procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.Report).ToEqual('MiniSpec Framework Tests');
    end)

.Scenario('Suite Before hook runs before all Features')
  // Lo mismo: el hook se registro con .Before() en el programa y corrio antes de la primera
  // feature. El escenario no puede dispararlo, solo comprobar que ocurrio.
  .Given('a runner whose suite registered a Before hook').NoAction
  .When('the marker the hook leaves behind is read',
    procedure(Ctx: TScenarioContext)
    begin
      Ctx.Report := BoolToStr(SuiteBeforeHookCalled, True);
    end)
  .&Then('the Before hook marker is set',
    procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.Report).ToEqual('True');
    end);

// Note: Suite After hook is verified via Assert after .Run() in the program

end.
