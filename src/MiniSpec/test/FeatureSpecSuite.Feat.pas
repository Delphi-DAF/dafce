unit FeatureSpecSuite.Feat;

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
  /// World simple - ya no necesita heredar de nada especial.
  /// </summary>
  TSuiteWorld = class
  end;

initialization

Feature('''
Feature SpecSuite @suite

  As a test author
  I want to group all features under a Suite
  So I can provide a title and run hooks before/after all tests
''')

.UseWorld<TSuiteWorld>

.Scenario('Suite has the title set via Category()')
  .Given('the Suite was configured with Category',
    procedure(World: TSuiteWorld)
    begin
      // Category('MiniSpec Framework Tests') was called in program
    end)
  .&Then('the Suite Title matches what was set',
    procedure(World: TSuiteWorld)
    begin
      Assert(SuiteTitle = 'MiniSpec Framework Tests',
        Format('Expected "MiniSpec Framework Tests" but got "%s"', [SuiteTitle]));
    end)

.Scenario('Suite Before hook runs before all Features')
  .Given('the Suite has a Before hook registered',
    procedure(World: TSuiteWorld)
    begin
      // .Before() was called in program before .Run()
    end)
  .&Then('the Before hook marker is set',
    procedure(World: TSuiteWorld)
    begin
      Assert(SuiteBeforeHookCalled,
        'Suite Before hook should have been called before any Feature runs');
    end);

// Note: Suite After hook is verified via Assert after .Run() in the program

end.
