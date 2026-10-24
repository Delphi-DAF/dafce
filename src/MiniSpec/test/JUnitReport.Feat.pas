unit JUnitReport.Feat;

// El informe JUnit dice de cada escenario, de cada fila y de cada hook que fallo lo mismo que el
// recuento de la suite. Las ejecuciones son anidadas porque sus escenarios quedan pendientes,
// undefined o fallan a proposito, sin tocar la suite de fuera.

interface

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  System.RegularExpressions,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Reporter.JUnit;

type
  TCaseWorld = class
  public
    N: Integer;
  end;

  TJUnitReportWorld = class
  strict private
    FRun: TSpecRun;
    FDir: string;
    // La ejecucion mantiene vivo el listener, que guarda su interfaz: esta referencia es para leerlo.
    FJUnit: TJUnitReporter;
  public
    Xml: string;
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteAndRead;
    property Run: TSpecRun read FRun;
  end;

constructor TJUnitReportWorld.Create;
begin
  inherited;
  FDir := TPath.Combine(TPath.GetTempPath, 'minispec-junit-report-' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FDir);
  FRun := TSpecRun.Create('Nested');
  FJUnit := TJUnitReporter.Create;
  var Options := TRunnerOptions.Create;
  try
    Options.Add('output', TPath.Combine(FDir, 'results.xml'));
    FJUnit.Configure(Options);
  finally
    Options.Free;
  end;
  FRun.AddListener(FJUnit);
end;

destructor TJUnitReportWorld.Destroy;
begin
  FRun.Free;
  TDirectory.Delete(FDir, True);
  inherited;
end;

procedure TJUnitReportWorld.ExecuteAndRead;
begin
  FRun.Execute;
  Xml := FJUnit.GetContent;
end;

function EscapedName(const Name: string): string;
begin
  Result := Name.Replace('&', '&amp;').Replace('<', '&lt;').Replace('>', '&gt;').Replace('"', '&quot;');
end;

/// El elemento testcase con ese nombre, entero, o un texto que dice que no esta.
function CaseOf(const Xml, Name: string): string;
begin
  var Start := Pos('<testcase name="' + EscapedName(Name) + '"', Xml);
  if Start = 0 then
    Exit('no testcase named ' + Name);
  var TagEnd := PosEx('>', Xml, Start);
  if Xml[TagEnd - 1] = '/' then
    Exit(Copy(Xml, Start, TagEnd - Start + 1));
  var Close := PosEx('</testcase>', Xml, Start);
  Result := Copy(Xml, Start, Close + Length('</testcase>') - Start);
end;

/// El valor de un atributo del elemento Tag que lleva ese nombre.
function AttrOf(const Xml, Tag, Name, Attr: string): string;
begin
  var M := TRegEx.Match(Xml, '<' + Tag + ' name="' + TRegEx.Escape(EscapedName(Name)) + '"[^>]*>');
  if not M.Success then
    Exit('no ' + Tag + ' named ' + Name);
  var A := TRegEx.Match(M.Value, '\b' + Attr + '="([^"]*)"');
  if A.Success then
    Result := A.Groups[1].Value
  else
    Result := 'no attribute ' + Attr;
end;

initialization

Feature('''
JUnit report @junit-report

  As whoever reads a suite in CI through its JUnit report
  I want every case to say what the run counted, and every failing hook to be a case
  So a pending, undefined or filtered scenario, or a hook that raised, reads the same as in the console
''')

.UseWorld<TJUnitReportWorld>

.Rule('JUnit does not report a Pending or Undefined scenario as passed')

  .Scenario('An Undefined scenario comes out as a failure')
    .Given('a run reported in JUnit with a scenario whose When has no binding',
      procedure(W: TJUnitReportWorld)
      begin
        Feature('Its When is undefined', W.Run.Suite)
          .UseWorld<TCaseWorld>
          .Scenario('It has an undefined When')
            .When('a step that no binding of JUnitReport.Feat answers');
      end)
    .When('it runs and the report is read', procedure(W: TJUnitReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('that scenario carries a failure whose message names the When', procedure(W: TJUnitReportWorld)
      begin
        Expect(CaseOf(W.Xml, 'It has an undefined When')).ToContain('<failure');
        Expect(CaseOf(W.Xml, 'It has an undefined When')).ToContain('a step that no binding of JUnitReport.Feat answers');
      end)
    .&And('the failures attribute counts it', procedure(W: TJUnitReportWorld)
      begin
        Expect(AttrOf(W.Xml, 'testsuite', 'Its When is undefined', 'failures')).ToEqual('1');
      end)

  .Scenario('A Pending scenario comes out as skipped')
    .Given('a run reported in JUnit with a scenario whose When is declared Pending',
      procedure(W: TJUnitReportWorld)
      begin
        Feature('Its When is pending', W.Run.Suite)
          .UseWorld<TCaseWorld>
          .Scenario('It has a pending When')
            .When('it does what is not written yet', procedure(C: TCaseWorld) begin end).Pending;
      end)
    .When('it runs and the report is read', procedure(W: TJUnitReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('that scenario carries a skipped whose message names the When', procedure(W: TJUnitReportWorld)
      begin
        Expect(CaseOf(W.Xml, 'It has a pending When')).ToContain('<skipped');
        Expect(CaseOf(W.Xml, 'It has a pending When')).ToContain('it does what is not written yet');
      end)
    .&And('the skipped attribute counts it, and failures does not', procedure(W: TJUnitReportWorld)
      begin
        Expect(AttrOf(W.Xml, 'testsuite', 'Its When is pending', 'skipped') + ' skipped, ' +
          AttrOf(W.Xml, 'testsuite', 'Its When is pending', 'failures') + ' failures').ToEqual('1 skipped, 0 failures');
      end)

  .Scenario('A Pending row of an outline comes out as skipped')
    .Given('a run reported in JUnit with a Scenario Outline of one row whose When is declared Pending',
      procedure(W: TJUnitReportWorld)
      begin
        Feature('Its row is pending', W.Run.Suite)
          .UseWorld<TCaseWorld>
          .ScenarioOutline('Row <N>')
            .When('the row does what is not written yet', procedure(C: TCaseWorld) begin end).Pending
            .Examples([['N'], [1]]);
      end)
    .When('it runs and the report is read', procedure(W: TJUnitReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('the row carries a skipped, and does not come out as passed', procedure(W: TJUnitReportWorld)
      begin
        Expect(CaseOf(W.Xml, 'Row <N> [Example 1]')).ToContain('<skipped');
      end)

.Rule('JUnit does not list a scenario that did not run')

  .Scenario('A scenario the filter leaves out does not come out')
    .Given('a run reported in JUnit with a feature of two scenarios, and a filter that matches only one',
      procedure(W: TJUnitReportWorld)
      begin
        Feature('One of its scenarios is filtered', W.Run.Suite)
          .UseWorld<TCaseWorld>
          .Scenario('Kept')
            .When('it runs', procedure(C: TCaseWorld) begin end)
          .Scenario('Left out')
            .When('it runs', procedure(C: TCaseWorld) begin end);
        W.Run.Options.SpecMatcher :=
          function(const Scenario: IScenario): Boolean
          begin
            Result := Scenario.Description = 'Kept';
          end;
      end)
    .When('it runs and the report is read', procedure(W: TJUnitReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('the feature carries one case, and its tests attribute is 1', procedure(W: TJUnitReportWorld)
      begin
        Expect(CaseOf(W.Xml, 'Left out')).ToEqual('no testcase named Left out');
        Expect(AttrOf(W.Xml, 'testsuite', 'One of its scenarios is filtered', 'tests')).ToEqual('1');
      end)

.Rule('JUnit carries the failure of a hook as a failed case')

  .Scenario('An After hook of the suite that raises')
    .Given('a run reported in JUnit with a scenario that passes and an After hook of the suite that raises',
      procedure(W: TJUnitReportWorld)
      begin
        W.Run.Suite.AddAfterHook('it cannot clean up',
          procedure begin raise Exception.Create('the suite cannot clean up'); end);
        Feature('Its scenario passes', W.Run.Suite)
          .UseWorld<TCaseWorld>
          .Scenario('It passes')
            .When('it runs', procedure(C: TCaseWorld) begin end);
      end)
    .When('it runs and the report is read', procedure(W: TJUnitReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('there is a case with a failure that names that hook and carries its message', procedure(W: TJUnitReportWorld)
      begin
        Expect(CaseOf(W.Xml, 'it cannot clean up')).ToContain('<failure');
        Expect(CaseOf(W.Xml, 'it cannot clean up')).ToContain('the suite cannot clean up');
      end)
    .&And('the failures attribute of the root is 1', procedure(W: TJUnitReportWorld)
      begin
        Expect(AttrOf(W.Xml, 'testsuites', 'MiniSpec', 'failures')).ToEqual('1');
      end)

  .Scenario('A Before hook of a feature that raises')
    .Given('a run reported in JUnit with a feature whose Before hook raises',
      procedure(W: TJUnitReportWorld)
      begin
        Feature('Its Before raises', W.Run.Suite)
          .UseWorld<TCaseWorld>
          .Before('it cannot prepare the feature',
            procedure begin raise Exception.Create('the feature cannot start'); end)
          .Scenario('It would pass')
            .When('it runs', procedure(C: TCaseWorld) begin end);
      end)
    .When('it runs and the report is read', procedure(W: TJUnitReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('the block of that feature carries a case with a failure that names the hook', procedure(W: TJUnitReportWorld)
      begin
        var Block := Copy(W.Xml, Pos('<testsuite name="Its Before raises"', W.Xml), MaxInt);
        Expect(CaseOf(Block, 'it cannot prepare the feature')).ToContain('<failure');
        Expect(CaseOf(Block, 'it cannot prepare the feature')).ToContain('the feature cannot start');
      end);

end.
