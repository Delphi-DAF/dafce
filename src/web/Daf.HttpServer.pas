unit Daf.HttpServer;

interface
uses
  System.Classes,
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  Daf.Extensions.Configuration,
  Daf.Extensions.Logging,
  Daf.Extensions.HttpServer;

type
  TDafHttpServer = class(TInterfacedObject, IDafHttpServer)
  private
    FWebBrokerBridge: TIdHTTPWebBrokerBridge;
    FOnStopping: TNotifyEvent;
    FOnStarting: TNotifyEvent;
    FOnStarted: TNotifyEvent;
    FOnStopped: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FLogger: ILogger;
    function GetPort: Word;
    procedure SetPort(const Value: Word);
    procedure SetActive(const Value: Boolean);
    function GetActive: Boolean;
    function GetOnStarting: TNotifyEvent;
    procedure SetOnStarting(const Value: TNotifyEvent);
    function GetOnStarted: TNotifyEvent;
    procedure SetOnStarted(const Value: TNotifyEvent);
    function GetOnStopping: TNotifyEvent;
    procedure SetOnStopping(const Value: TNotifyEvent);
    function GetOnStopped: TNotifyEvent;
    procedure SetOnStopped(const Value: TNotifyEvent);
    procedure ClearBindings;
    function CanBindPort: Boolean;overload;
    function CanBindPort(const APort: Integer): Boolean; overload;
    function GetSessionID: string;
    function GetOnError: TNotifyEvent;
    procedure SetOnError(const Value: TNotifyEvent);
    procedure FireOnError(const E: Exception);
    function GetOnDestroy: TNotifyEvent;
    procedure SetOnDestroy(const Value: TNotifyEvent);
  public
    constructor Create(const WebModuleClass: TComponentClass; const Config: IConfiguration; const LoggerFactory: ILoggerFactory = nil);
    procedure BeforeDestruction;override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Logger: ILogger read FLogger;
    property SessionID: string read GetSessionID;
    property Port: Word read GetPort write SetPort;
    property Active: Boolean read GetActive;
    property OnStarting: TNotifyEvent read GetOnStarting write SetOnStarting;
    property OnStarted: TNotifyEvent read GetOnStarted write SetOnStarted;
    property OnStopping: TNotifyEvent read GetOnStopping write SetOnStopping;
    property OnStopped: TNotifyEvent read GetOnStopped write SetOnStopped;
    property OnError: TNotifyEvent read GetOnError write SetOnError;
    property OnDestroy: TNotifyEvent read GetOnDestroy write SetOnDestroy;
  end;

implementation
uses
  Web.WebReq,
  IdSocketHandle;

{ TDafHttpServer }

constructor TDafHttpServer.Create(const WebModuleClass: TComponentClass; const Config: IConfiguration; const LoggerFactory: ILoggerFactory = nil);
begin
  inherited Create;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  FWebBrokerBridge := TIdHTTPWebBrokerBridge.Create(nil);
  Port := Word.Parse(Config['Port']);
  if Assigned(LoggerFactory) then
    FLogger := LoggerFactory.CreateLogger(Self.ClassType)
  else
    FLogger := TNullLogger.Create;
end;

procedure TDafHttpServer.BeforeDestruction;
begin
  inherited;
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FOnDestroy := nil;
end;

destructor TDafHttpServer.Destroy;
begin
  Stop;
  WebRequestHandler.WebModuleClass := nil;
  // WebBroker keeps the modules it built for reuse, whatever class is installed later, and each
  // one is wired to the router of the application it served. Freed with the server, a later one
  // in the same process does not dispatch to a router that no longer exists.
  WebRequestHandler.FreeModules;
  FreeAndNil(FWebBrokerBridge);
  inherited;
end;

procedure TDafHttpServer.FireOnError(const E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(E);
end;

function TDafHttpServer.GetActive: Boolean;
begin
  Result := Assigned(FWebBrokerBridge) and FWebBrokerBridge.Active;
end;

function TDafHttpServer.GetOnStarting: TNotifyEvent;
begin
  Result := FOnStarting
end;

procedure TDafHttpServer.SetOnStarting(const Value: TNotifyEvent);
begin
  FOnStarting := Value;
end;

function TDafHttpServer.GetOnDestroy: TNotifyEvent;
begin
  Result := FOnDestroy;
end;

function TDafHttpServer.GetOnError: TNotifyEvent;
begin
  Result := FOnError;
end;

function TDafHttpServer.GetOnStarted: TNotifyEvent;
begin
  Result := FOnStarted;
end;

procedure TDafHttpServer.SetOnDestroy(const Value: TNotifyEvent);
begin
  FOnDestroy := Value;
end;

procedure TDafHttpServer.SetOnError(const Value: TNotifyEvent);
begin
  FOnError := Value;
end;

procedure TDafHttpServer.SetOnStarted(const Value: TNotifyEvent);
begin
  FOnStarted := Value;
end;

function TDafHttpServer.GetOnStopped: TNotifyEvent;
begin
  Result := FOnStopped;
end;

function TDafHttpServer.GetOnStopping: TNotifyEvent;
begin
  Result := FOnStopping;
end;

procedure TDafHttpServer.SetActive(const Value: Boolean);
begin
  if Assigned(FWebBrokerBridge) then
    FWebBrokerBridge.Active := Value;
end;

procedure TDafHttpServer.SetOnStopped(const Value: TNotifyEvent);
begin
  FOnStopped := Value;
end;

procedure TDafHttpServer.SetOnStopping(const Value: TNotifyEvent);
begin
  FOnStopping := Value;
end;

function TDafHttpServer.CanBindPort: Boolean;
begin
  Result := CanBindPort(Port);
end;

function TDafHttpServer.CanBindPort(const APort: Integer): Boolean;
var
  Probe: TIdSocketHandle;
begin
  if not Assigned(FWebBrokerBridge) then
    Exit(False);

  // While this server holds the port itself, a bind would fail against its own
  // socket. The question being asked is whether it can serve there, and it can.
  if FWebBrokerBridge.Active then
    Exit(FWebBrokerBridge.DefaultPort = APort);

  // The bridge is an Indy TCP server, so one of its own bindings answers the
  // question — the same Add / AllocateSocket / Bind that Startup performs, and
  // no remoting stack for the sake of one boolean. Closing the socket and
  // freeing the binding is what leaves the port free for the Start this
  // usually precedes.
  Result := True;
  Probe := FWebBrokerBridge.Bindings.Add;
  try
    Probe.Port := APort;
    try
      Probe.AllocateSocket;
      Probe.Bind;
    except
      Result := False;
    end;
    if Probe.HandleAllocated then
      Probe.CloseSocket;
  finally
    Probe.Free;
  end;
end;

function TDafHttpServer.GetPort: Word;
begin
  if not Assigned(FWebBrokerBridge) then Exit(0);
  Result := FWebBrokerBridge.DefaultPort
end;

function TDafHttpServer.GetSessionID: string;
begin
  if not Assigned(FWebBrokerBridge) then Exit('');
  Result := FWebBrokerBridge.SessionIDCookieName;
end;

procedure TDafHttpServer.SetPort(const Value: Word);
begin
  if not Assigned(FWebBrokerBridge) then Exit;
  FWebBrokerBridge.DefaultPort := Value;
end;

procedure TDafHttpServer.Start;
begin
  if Active then
    Exit;

  if not CanBindPort(Port) then  begin
    FireOnError(Exception.CreateFmt('Cannot start: port %d is in use', [Port]));
    Abort;
  end;

  if Assigned(FOnStarting) then
    OnStarting(Self);
  ClearBindings;
  SetActive(True);
  if Assigned(FOnStarted) then
    OnStarted(Self);
end;

procedure TDafHttpServer.Stop;
begin
  if not Active then
    Exit;

  if Assigned(FOnStopping) then
    OnStopping(Self);

  SetActive(False);
  ClearBindings;
  if Assigned(FOnStopped) then
    OnStopped(Self);
end;

procedure TDafHttpServer.ClearBindings;
begin
  if not Assigned(FWebBrokerBridge) then Exit();
  FWebBrokerBridge.Bindings.Clear;
end;


end.
