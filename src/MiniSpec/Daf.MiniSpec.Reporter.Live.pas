unit Daf.MiniSpec.Reporter.Live;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.JSON,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdContext,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Reporter;

type
  /// <summary>
  /// Live reporter - broadcasts test results via SSE to browser dashboard.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TLiveReporter = class(TCustomListener)
  private
    FServer: TIdHTTPServer;
    FPort: Integer;
    FEvents: TStringList;
    FEventsLock: TCriticalSection;
    FLiveClients: TList<TIdContext>;
    FClientsLock: TCriticalSection;
    FReportFinished: Boolean;
    FWaitTimeout: Integer;  // ms to wait for browser, 0 = no wait
    FContext: IRunContext;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure BroadcastEvent(const EventJson: string);
    function BuildEventJson(const EventType: string; const Data: TJSONObject): string;
    function HasConnectedClients: Boolean;
    function StepsToJsonArray(const Steps: TList<IScenarioStep>; const StepType: string): TJSONArray;
  public
    constructor Create(APort: Integer = 8080);
    destructor Destroy;override;
    procedure Configure(const Options: TReporterOptions);override;
    function ShowHelp: Boolean;override;
    function UseConsole: Boolean;override;
    function GetContent: string;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnBeginFeature(const Context: IRunContext; const Feature: IFeature);override;
    procedure OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters);override;
    procedure OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
    property Port: Integer read FPort;
    property WaitTimeout: Integer read FWaitTimeout;  // ms, 0 = no wait
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  IdGlobal,
  Daf.MiniSpec,
  Daf.MiniSpec.LiveDashboard;

{ TLiveReporter }

constructor TLiveReporter.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FWaitTimeout := 3000;  // default 3 seconds
  FEvents := TStringList.Create;
  FEventsLock := TCriticalSection.Create;
  FLiveClients := TList<TIdContext>.Create;
  FClientsLock := TCriticalSection.Create;
  FReportFinished := False;
  FContext := nil;

  FServer := TIdHTTPServer.Create(nil);
  FServer.DefaultPort := FPort;
  FServer.OnCommandGet := HandleRequest;
end;

destructor TLiveReporter.Destroy;
begin
  if FServer.Active then
    FServer.Active := False;
  FServer.Free;
  FClientsLock.Free;
  FLiveClients.Free;
  FEventsLock.Free;
  FEvents.Free;
  inherited;
end;

procedure TLiveReporter.Configure(const Options: TReporterOptions);
begin
  inherited;
  if Assigned(Options) and Options.ContainsKey('port') then
  begin
    FPort := StrToIntDef(Options['port'], FPort);
    FServer.DefaultPort := FPort;
  end;
  // wait=<ms> por defecto 3000ms, wait=0 para deshabilitar
  var WaitOpt := GetCliOption('wait', '3000');  // default 3 seconds
  FWaitTimeout := StrToIntDef(WaitOpt, 3000);
end;

function TLiveReporter.ShowHelp: Boolean;
begin
  WriteLn('Live Reporter - Real-time test results in browser');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  port=<number>   HTTP server port (default: 8080)');
  WriteLn('  wait=<ms>       Wait for browser connection (default: 3000ms, 0 to disable)');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  -r live                  Start with default wait (3s)');
  WriteLn('  -r live:port=9000        Custom port, default wait');
  WriteLn('  -r live:wait=5000        Wait 5 seconds for browser');
  WriteLn('  -r live:wait=0           No wait for browser');
  WriteLn('  -r live:port=9000,wait=0 Custom port, no wait');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  1. Run tests with -r live');
  WriteLn('  2. Open http://localhost:<port> in browser');
  WriteLn('  3. Watch results update in real-time');
  Result := True;
end;

function TLiveReporter.UseConsole: Boolean;
begin
  Result := True;
end;

function TLiveReporter.GetContent: string;
begin
  Result := Format('Live Dashboard served at http://localhost:%d', [FPort]);
end;

function TLiveReporter.HasConnectedClients: Boolean;
begin
  FClientsLock.Enter;
  try
    Result := FLiveClients.Count > 0;
  finally
    FClientsLock.Leave;
  end;
end;

function TLiveReporter.BuildEventJson(const EventType: string; const Data: TJSONObject): string;
var
  Wrapper: TJSONObject;
begin
  Wrapper := TJSONObject.Create;
  try
    Wrapper.AddPair('event', EventType);
    for var Pair in Data do
      Wrapper.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
    Result := Wrapper.ToJSON;
  finally
    Wrapper.Free;
  end;
end;

function TLiveReporter.StepsToJsonArray(const Steps: TList<IScenarioStep>; const StepType: string): TJSONArray;
var
  StepObj: TJSONObject;
  Step: IScenarioStep;
begin
  Result := TJSONArray.Create;
  if Steps = nil then Exit;
  for Step in Steps do
  begin
    StepObj := TJSONObject.Create;
    StepObj.AddPair('type', StepType);
    StepObj.AddPair('text', Step.Description);
    StepObj.AddPair('success', TJSONBool.Create(Step.RunInfo.IsSuccess));
    StepObj.AddPair('ms', TJSONNumber.Create(Step.RunInfo.ExecTimeMs));
    if not Step.RunInfo.IsSuccess and (Step.RunInfo.ErrMsg <> '') then
      StepObj.AddPair('error', Step.RunInfo.ErrMsg);
    Result.AddElement(StepObj);
  end;
end;

procedure TLiveReporter.BroadcastEvent(const EventJson: string);
var
  Client: TIdContext;
  SSEData: string;
begin
  SSEData := 'data: ' + EventJson + #10#10;

  FEventsLock.Enter;
  try
    FEvents.Add(EventJson);
  finally
    FEventsLock.Leave;
  end;

  FClientsLock.Enter;
  try
    for Client in FLiveClients do
    begin
      try
        Client.Connection.IOHandler.Write(SSEData, IndyTextEncoding_UTF8);
      except
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TLiveReporter.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Doc: string;
begin
  Doc := ARequestInfo.Document;

  if SameText(Doc, '/events') or Doc.EndsWith('/events', True) then
  begin
    AContext.Connection.IOHandler.WriteLn('HTTP/1.1 200 OK');
    AContext.Connection.IOHandler.WriteLn('Content-Type: text/event-stream');
    AContext.Connection.IOHandler.WriteLn('Cache-Control: no-cache');
    AContext.Connection.IOHandler.WriteLn('Connection: keep-alive');
    AContext.Connection.IOHandler.WriteLn('Access-Control-Allow-Origin: *');
    AContext.Connection.IOHandler.WriteLn('');

    FClientsLock.Enter;
    try
      FLiveClients.Add(AContext);
    finally
      FClientsLock.Leave;
    end;

    try
      AContext.Connection.IOHandler.Write(': welcome'#10#10, IndyTextEncoding_UTF8);
    except
    end;

    FEventsLock.Enter;
    try
      for var EventJson in FEvents do
      begin
        try
          AContext.Connection.IOHandler.Write('data: ' + EventJson + #10#10, IndyTextEncoding_UTF8);
        except
          Break;
        end;
      end;
    finally
      FEventsLock.Leave;
    end;

    while not FReportFinished and AContext.Connection.Connected do
      Sleep(200);

    FClientsLock.Enter;
    try
      FLiveClients.Remove(AContext);
    finally
      FClientsLock.Leave;
    end;
  end
  else
  begin
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ContentText := StringReplace(LIVE_DASHBOARD_HTML,
      '{{MINISPEC_VERSION}}', TMiniSpec.Version, [rfReplaceAll]);
  end;
end;

procedure TLiveReporter.OnBeginReport(const Context: IRunContext);
var
  Data: TJSONObject;
  WaitCount: Integer;
begin
  FContext := Context;
  FReportFinished := False;
  FEvents.Clear;

  try
    FServer.Active := True;
    WriteLn(Format('Live Dashboard: http://localhost:%d', [FPort]));

    if FWaitTimeout > 0 then
    begin
      WriteLn(Format('Waiting for browser connection (%d ms)...', [FWaitTimeout]));
      WaitCount := 0;
      var MaxWait := FWaitTimeout div 100;
      while not HasConnectedClients and (WaitCount < MaxWait) do
      begin
        Sleep(100);
        Inc(WaitCount);
      end;

      if HasConnectedClients then
        WriteLn('Browser connected. Running specs...')
      else
        WriteLn('Timeout. Running specs anyway...');
    end;
  except
    on E: Exception do
      WriteLn('Live server error: ' + E.Message);
  end;

  Data := TJSONObject.Create;
  try
    Data.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
    BroadcastEvent(BuildEventJson('report:start', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.OnEndReport(const Context: IRunContext);
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  try
    Data.AddPair('pass', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Data.AddPair('skip', TJSONNumber.Create(Context.ReportCounters.SkipCount));
    Data.AddPair('completedAt', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Context.CompletedAt));
    BroadcastEvent(BuildEventJson('report:end', Data));
  finally
    Data.Free;
  end;

  FReportFinished := True;
  FServer.Active := False;
  FContext := nil;
end;

procedure TLiveReporter.OnBeginFeature(const Context: IRunContext; const Feature: IFeature);
var
  Data: TJSONObject;
  TagsArray, BgSteps: TJSONArray;
begin
  if not Assigned(Feature) then Exit;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Feature.Title);
    Data.AddPair('narrative', Feature.Narrative);
    TagsArray := TJSONArray.Create;
    for var Tag in Feature.Tags.ToArray do
      TagsArray.Add(Tag);
    Data.AddPair('tags', TagsArray);
    if Feature.BackGround <> nil then
    begin
      BgSteps := StepsToJsonArray(Feature.BackGround.StepsGiven, 'Given');
      Data.AddPair('background', BgSteps);
    end;
    BroadcastEvent(BuildEventJson('feature:start', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters);
var
  Data: TJSONObject;
begin
  if not Assigned(Feature) then Exit;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Feature.Title);
    Data.AddPair('pass', TJSONNumber.Create(Counters.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Counters.FailCount));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));
    BroadcastEvent(BuildEventJson('feature:end', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters);
var
  Data: TJSONObject;
  StepsArray, TempArr: TJSONArray;
  IsSkipped: Boolean;
begin
  IsSkipped := Scenario.RunInfo.State = srsSkiped;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Scenario.Description);
    Data.AddPair('skipped', TJSONBool.Create(IsSkipped));
    if IsSkipped then
      Data.AddPair('success', TJSONBool.Create(True))
    else
      Data.AddPair('success', TJSONBool.Create(Scenario.RunInfo.IsSuccess));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));
    if not IsSkipped and not Scenario.RunInfo.IsSuccess and (Scenario.RunInfo.ErrMsg <> '') then
      Data.AddPair('error', Scenario.RunInfo.ErrMsg);

    StepsArray := TJSONArray.Create;
    TempArr := StepsToJsonArray(Scenario.StepsGiven, 'Given');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Scenario.StepsWhen, 'When');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Scenario.StepsThen, 'Then');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    Data.AddPair('steps', StepsArray);

    Data.AddPair('totalPass', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Data.AddPair('totalFail', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Data.AddPair('totalSkip', TJSONNumber.Create(Context.ReportCounters.SkipCount));
    BroadcastEvent(BuildEventJson('scenario:end', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  Data: TJSONObject;
  StepsArray, HeadersArray, ExamplesArray, RowArray, TempArr: TJSONArray;
  ExampleObj: TJSONObject;
begin
  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Outline.Description);
    Data.AddPair('type', 'outline');

    HeadersArray := TJSONArray.Create;
    for var H in Outline.Headers do
      HeadersArray.Add(H);
    Data.AddPair('headers', HeadersArray);

    StepsArray := TJSONArray.Create;
    TempArr := StepsToJsonArray(Outline.StepsGiven, 'Given');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Outline.StepsWhen, 'When');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Outline.StepsThen, 'Then');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    Data.AddPair('steps', StepsArray);

    ExamplesArray := TJSONArray.Create;
    for var Example in Outline.Examples do
    begin
      if Example.RunInfo.State in [srsFinished, srsSkiped] then
      begin
        var IsSkipped := Example.RunInfo.State = srsSkiped;

        ExampleObj := TJSONObject.Create;
        RowArray := TJSONArray.Create;
        for var Val in Example.ExampleMeta.Values do
          RowArray.Add(Val2Str(Val));
        ExampleObj.AddPair('values', RowArray);
        ExampleObj.AddPair('skipped', TJSONBool.Create(IsSkipped));
        if IsSkipped then
          ExampleObj.AddPair('success', TJSONBool.Create(True))
        else
          ExampleObj.AddPair('success', TJSONBool.Create(Example.RunInfo.IsSuccess));
        ExampleObj.AddPair('ms', TJSONNumber.Create(Example.RunInfo.ExecTimeMs));
        if not IsSkipped and not Example.RunInfo.IsSuccess and (Example.RunInfo.ErrMsg <> '') then
          ExampleObj.AddPair('error', Example.RunInfo.ErrMsg);
        ExamplesArray.AddElement(ExampleObj);
      end;
    end;
    Data.AddPair('examples', ExamplesArray);
    Data.AddPair('pass', TJSONNumber.Create(Counters.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Counters.FailCount));
    Data.AddPair('success', TJSONBool.Create(Counters.IsSuccess));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));

    Data.AddPair('totalPass', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Data.AddPair('totalFail', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Data.AddPair('totalSkip', TJSONNumber.Create(Context.ReportCounters.SkipCount));

    BroadcastEvent(BuildEventJson('outline:end', Data));
  finally
    Data.Free;
  end;
end;

end.
