unit SuiteContext.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Test context shared across all features in a suite
  /// </summary>
  TSharedSuiteContext = class(TSuiteContext)
  public
    constructor Create; override;
  end;

constructor TSharedSuiteContext.Create;
begin
  inherited;
end;

initialization

// Placeholder test - SuiteContext integration is tested implicitly by other tests
// The SuiteContext infrastructure is available for test suites that need it

end.
