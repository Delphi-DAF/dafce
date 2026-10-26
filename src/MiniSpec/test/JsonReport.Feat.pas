unit JsonReport.Feat;

// El informe JSON dice de cada escenario, cada fila y cada step en que estado acabo, con los mismos
// estados que el recuento de la suite. Las ejecuciones son anidadas porque sus escenarios quedan
// pendientes, undefined o fallan a proposito, sin tocar la suite de fuera.

interface

implementation

uses
  System.SysUtils,
  System.JSON,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Reporter.Json;

type
  TItemWorld = class
  public
    N: Integer;
  end;

  TJsonReportWorld = class
  strict private
    FRun: TSpecRun;
    // La ejecucion mantiene vivo el listener, que guarda su interfaz: esta referencia es para leerlo.
    FJson: TJsonReporter;
  public
    Root: TJSONValue;
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteAndRead;
    property Run: TSpecRun read FRun;
  end;

constructor TJsonReportWorld.Create;
begin
  inherited;
  FRun := TSpecRun.Create('Nested');
  FJson := TJsonReporter.Create;
  FRun.AddListener(FJson);
end;

destructor TJsonReportWorld.Destroy;
begin
  Root.Free;
  FRun.Free;
  inherited;
end;

procedure TJsonReportWorld.ExecuteAndRead;
begin
  FRun.Execute;
  Root := TJSONObject.ParseJSONValue(FJson.GetContent);
end;

/// El primer objeto del informe cuya descripcion es esa, buscado en todo el arbol.
function ItemNamed(const Value: TJSONValue; const Description: string): TJSONObject;
begin
  Result := nil;
  if Value is TJSONObject then
  begin
    var Obj := TJSONObject(Value);
    var D := Obj.GetValue('description');
    if Assigned(D) and (D.Value = Description) then
      Exit(Obj);
    for var Pair in Obj do
    begin
      Result := ItemNamed(Pair.JsonValue, Description);
      if Assigned(Result) then
        Exit;
    end;
  end
  else if Value is TJSONArray then
    for var Element in TJSONArray(Value) do
    begin
      Result := ItemNamed(Element, Description);
      if Assigned(Result) then
        Exit;
    end;
end;

function FieldOf(const Value: TJSONValue; const Description, Field: string): string;
begin
  var Obj := ItemNamed(Value, Description);
  if not Assigned(Obj) then
    Exit('no item described as ' + Description);
  var V := Obj.GetValue(Field);
  if Assigned(V) then
    Result := V.Value
  else
    Result := 'no ' + Field;
end;

function RootField(const Value: TJSONValue; const Field: string): string;
begin
  var V: TJSONValue := nil;
  if Value is TJSONObject then
    V := TJSONObject(Value).GetValue(Field);
  if Assigned(V) then
    Result := V.Value
  else
    Result := 'no ' + Field;
end;

initialization

Feature('''
JSON report @json-report

  As whoever reads a suite through its JSON report
  I want every scenario, row and step to say the state it ended in, and the totals to be complete
  So a pending or undefined scenario, or a skipped step, does not read as a failure or as a pass
''')

.UseWorld<TJsonReportWorld>

.Rule('The JSON gives each element the state it ended in')

  .Scenario('A Pending scenario')
    .Given('a run reported in JSON with a scenario whose When is declared Pending',
      procedure(W: TJsonReportWorld)
      begin
        Feature('Its When is pending', W.Run.Suite)
          .UseWorld<TItemWorld>
          .Scenario('It has a pending When')
            .When('it does what is not written yet', procedure(I: TItemWorld) begin end).Pending;
      end)
    .When('it runs and the report is read', procedure(W: TJsonReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('the scenario has status pending, and its When status pending and success false', procedure(W: TJsonReportWorld)
      begin
        Expect(Format('scenario %s, When %s success=%s', [FieldOf(W.Root, 'It has a pending When', 'status'),
          FieldOf(W.Root, 'it does what is not written yet', 'status'),
          FieldOf(W.Root, 'it does what is not written yet', 'success')])).ToEqual('scenario pending, When pending success=false');
      end)
    .&And('failCount is 0', procedure(W: TJsonReportWorld)
      begin
        Expect(RootField(W.Root, 'failCount')).ToEqual('0');
      end)

  .Scenario('An Undefined scenario')
    .Given('a run reported in JSON with a scenario whose When has no binding',
      procedure(W: TJsonReportWorld)
      begin
        Feature('Its When is undefined', W.Run.Suite)
          .UseWorld<TItemWorld>
          .Scenario('It has an undefined When')
            .When('a step that no binding of JsonReport.Feat answers');
      end)
    .When('it runs and the report is read', procedure(W: TJsonReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('the scenario has status undefined, and its When status undefined', procedure(W: TJsonReportWorld)
      begin
        Expect(Format('scenario %s, When %s', [FieldOf(W.Root, 'It has an undefined When', 'status'),
          FieldOf(W.Root, 'a step that no binding of JsonReport.Feat answers', 'status')]))
          .ToEqual('scenario undefined, When undefined');
      end)
    .&And('undefinedCount is 1', procedure(W: TJsonReportWorld)
      begin
        Expect(RootField(W.Root, 'undefinedCount')).ToEqual('1');
      end)

  .Scenario('A step skipped after a failure')
    .Given('a run reported in JSON with a scenario whose When fails, followed by a Then',
      procedure(W: TJsonReportWorld)
      begin
        Feature('Its When fails', W.Run.Suite)
          .UseWorld<TItemWorld>
          .Scenario('It has a failing When')
            .When('it checks something false', procedure(I: TItemWorld)
              begin
                Expect(1).ToEqual(2);
              end)
            .&Then('it is never reached', procedure(I: TItemWorld) begin end);
      end)
    .When('it runs and the report is read', procedure(W: TJsonReportWorld)
      begin
        W.ExecuteAndRead;
      end)
    .&Then('the Then has status skipped and success false', procedure(W: TJsonReportWorld)
      begin
        Expect(Format('%s success=%s', [FieldOf(W.Root, 'it is never reached', 'status'),
          FieldOf(W.Root, 'it is never reached', 'success')])).ToEqual('skipped success=false');
      end);

end.
