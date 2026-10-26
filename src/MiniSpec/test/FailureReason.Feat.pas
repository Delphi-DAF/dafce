unit FailureReason.Feat;

// Lo que fijan estas specs es que el motivo de un escenario que falla llegue a cada informe: el
// paso que lo hizo fallar y su mensaje, sea un escenario normal o una fila de un Scenario Outline.
// Una fila no imprime sus pasos, asi que su motivo solo llega al informe si el escenario lo sabe.
// La ejecucion anidada es la de FailureVisibility: los escenarios fallan sin poner roja la suite.

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Reporter.Console,
  Daf.MiniSpec.Reporter.JUnit,
  Daf.MiniSpec.Reporter.Json,
  Daf.MiniSpec.Reporter.Gherkin;

type
  TRowWorld = class
  public
    N: Integer;
  end;

  /// El informe de consola tal cual, sin pasar por la consola.
  TCapturingConsole = class(TConsoleReporter)
  strict private
    FText: TStringBuilder;
  protected
    procedure Emit(const Text: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Text: string;
  end;

  TFailureReasonWorld = class
  strict private
    FRun: TSpecRun;
    FDir: string;
    // Los listeners los mantiene vivos la ejecucion, que guarda sus interfaces: estas referencias
    // son para leerlos, no para poseerlos.
    FConsole: TCapturingConsole;
    FJUnit: TJUnitReporter;
    FJson: TJsonReporter;
    FGherkin: TGherkinReporter;
    function Configured(const Listener: TCustomListener; const Output: string): TCustomListener;
  public
    /// La fila que se esta comprobando: que informe se lee, de que fallo y que tiene que decir.
    Report: string;
    Which: string;
    Expected: string;
    Text: string;
    constructor Create;
    destructor Destroy; override;
    procedure AttachReporter;
    procedure DeclareFailingFeature;
    procedure ExecuteAndRead;
  end;

{ TCapturingConsole }

constructor TCapturingConsole.Create;
begin
  inherited Create;
  FText := TStringBuilder.Create;
end;

destructor TCapturingConsole.Destroy;
begin
  FText.Free;
  inherited;
end;

procedure TCapturingConsole.Emit(const Text: string);
begin
  FText.Append(Text);
end;

function TCapturingConsole.Text: string;
begin
  Result := FText.ToString;
end;

{ TFailureReasonWorld }

constructor TFailureReasonWorld.Create;
begin
  inherited;
  FDir := TPath.Combine(TPath.GetTempPath, 'minispec-failure-reason-' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FDir);
  FRun := TSpecRun.Create('Nested');
end;

procedure TFailureReasonWorld.AttachReporter;
begin
  // Solo el de la fila: JUnit y gherkin anuncian en la consola el fichero que escriben.
  if Report = 'console' then
  begin
    FConsole := TCapturingConsole.Create;
    FRun.AddListener(FConsole);
  end
  else if Report = 'junit' then
  begin
    FJUnit := TJUnitReporter.Create;
    FRun.AddListener(Configured(FJUnit, TPath.Combine(FDir, 'results.xml')));
  end
  else if Report = 'json' then
  begin
    FJson := TJsonReporter.Create;
    FRun.AddListener(FJson);
  end
  else if Report = 'gherkin-results' then
  begin
    FGherkin := TGherkinReporter.Create(True);
    FRun.AddListener(Configured(FGherkin, FDir));
  end;
end;

destructor TFailureReasonWorld.Destroy;
begin
  FRun.Free;
  TDirectory.Delete(FDir, True);
  inherited;
end;

function TFailureReasonWorld.Configured(const Listener: TCustomListener; const Output: string): TCustomListener;
begin
  var Options := TRunnerOptions.Create;
  try
    Options.Add('output', Output);
    Listener.Configure(Options);
  finally
    Options.Free;
  end;
  Result := Listener;
end;

procedure TFailureReasonWorld.DeclareFailingFeature;
begin
  Feature('Two failures', FRun.Suite)
    .UseWorld<TRowWorld>
    .Scenario('A plain scenario that fails')
      .When('it runs', procedure(R: TRowWorld) begin R.N := 1; end)
      .&Then('the plain check fails', procedure(R: TRowWorld)
        begin
          Expect('plain').ToEqual('boom');
        end)
    .ScenarioOutline('Row <N>')
      .When('the row runs', procedure(R: TRowWorld) begin end)
      .&Then('the row check fails', procedure(R: TRowWorld)
        begin
          Expect(R.N).ToEqual(0);
        end)
      .Examples([['N'], [7]])
    .Scenario('A scenario whose When raises')
      .When('it raises', procedure(R: TRowWorld)
        begin
          raise Exception.Create('boom-when');
        end)
      .&Then('nothing expects it', procedure(R: TRowWorld) begin end);
end;

procedure TFailureReasonWorld.ExecuteAndRead;
begin
  FRun.Execute;
  if Assigned(FConsole) then
    Text := FConsole.Text
  else if Assigned(FJUnit) then
    Text := FJUnit.GetContent
  else if Assigned(FJson) then
    Text := FJson.GetContent
  else if Assigned(FGherkin) then
  begin
    var Files := TDirectory.GetFiles(FDir, '*.feature');
    if Length(Files) = 1 then
      Text := TFile.ReadAllText(Files[0], TEncoding.UTF8)
    else
      Text := Format('%d .feature files', [Length(Files)]);
  end
  else
    Text := 'no reporter called ' + Report;
end;

initialization

Feature('''
Failure reason @failure-reason

  As whoever reads why a suite went red
  I want every report to say which step failed and with what message
  So a failing row of a Scenario Outline, or a failure read in CI, is not a bare cross
''')

.UseWorld<TFailureReasonWorld>

.Rule('A failing scenario carries the step that failed it and its message into every report')

  .ScenarioOutline('The <Report> report says why <Which> failed')
    .Given('a run reported by <Report> where a scenario, a row of an outline and a raise fail',
      procedure(W: TFailureReasonWorld)
      begin
        W.AttachReporter;
        W.DeclareFailingFeature;
      end)
    .When('the run is executed and its <Report> report read',
      procedure(W: TFailureReasonWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('it contains <Expected>',
      procedure(W: TFailureReasonWorld)
      begin
        Expect(W.Text).ToContain(W.Expected);
      end)
    .Examples(
      [['Report',          'Which',          'Expected'],
       ['console',         'the step',       'ERROR: "Expected boom but got plain"'],
       ['console',         'the row',        'ERROR: "Then the row check fails: Expected 0 but got 7"'],
       ['junit',           'the scenario',   'message="Then the plain check fails: Expected boom but got plain"'],
       ['junit',           'the row',        'message="Then the row check fails: Expected 0 but got 7"'],
       ['junit',           'the raise',      'message="Unhandled exception from When step: boom-when"'],
       ['json',            'the scenario',   '"error": "Then the plain check fails: Expected boom but got plain"'],
       ['json',            'the row',        '"error": "Then the row check fails: Expected 0 but got 7"'],
       ['gherkin-results', 'the step',       'Then the plain check fails # FAIL: Expected boom but got plain'],
       ['gherkin-results', 'the row',        '| 7 | # FAIL: Then the row check fails: Expected 0 but got 7']]);

end.
