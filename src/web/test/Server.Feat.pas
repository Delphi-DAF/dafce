unit Server.Feat;

{
  The half of the module that needs a real socket. The rules that bind one are
  tagged @socket so a firewalled runner can skip them — `-f "~@socket"` — and
  still get the routing specs, which are the valuable ones.

  Every port here is probed free before it is used. A suite pinned to 8080
  fails on the first machine where something else holds it, and a suite that
  fails for reasons of its own gets disabled rather than fixed.
}

interface

uses
  System.SysUtils,
  Daf.Extensions.HttpServer,
  Web.Doubles;

type
  TServerWorld = class
  public
    Port: Word;
    Server: IDafHttpServer;
    Held: THeldPort;
    Trace: string;
    FreeAnswer: Boolean;
    HeldAnswer: Boolean;
    BindableAfterStop: Boolean;
    LibraryNamedMembers: string;
    /// The emitter and the message the error handler received.
    ErrorSeen: string;
    destructor Destroy; override;
    procedure NewServerOn(const APort: Word);
    procedure WireEvents;
    procedure Starting(Sender: TObject);
    procedure Started(Sender: TObject);
    procedure Stopping(Sender: TObject);
    procedure Stopped(Sender: TObject);
    procedure ServerError(Sender: TObject; const Error: Exception);
  end;

implementation

uses
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  Web.HTTPApp,
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.HttpServer,
  Daf.MiniSpec;

{ TServerWorld }

destructor TServerWorld.Destroy;
begin
  Server := nil;
  Held.Free;
  inherited;
end;

procedure TServerWorld.NewServerOn(const APort: Word);
begin
  var Data := TDictionary<string, string>.Create;
  Data.Add('Port', APort.ToString);
  var Config: IConfiguration := TConfigurationBuilder.Create.Add(MemoryConfig.Source(Data)).Build;
  // No request is ever made in these specs, so the web module class is never
  // constructed; what the server needs from it is somewhere to point.
  Server := TDafHttpServer.Create(TWebModule, Config);
  Port := APort;
end;

procedure TServerWorld.WireEvents;
begin
  Server.OnStarting := Starting;
  Server.OnStarted := Started;
  Server.OnStopping := Stopping;
  Server.OnStopped := Stopped;
end;

procedure TServerWorld.Starting(Sender: TObject);
begin
  Trace := Trace + 'starting,';
end;

procedure TServerWorld.Started(Sender: TObject);
begin
  Trace := Trace + 'started,';
end;

procedure TServerWorld.Stopping(Sender: TObject);
begin
  Trace := Trace + 'stopping,';
end;

procedure TServerWorld.Stopped(Sender: TObject);
begin
  Trace := Trace + 'stopped,';
end;

procedure TServerWorld.ServerError(Sender: TObject; const Error: Exception);
begin
  ErrorSeen := Sender.ClassName + ' | ' + Error.Message;
end;

initialization

Feature('''
Feature HTTP server @httpserver

  As an application hosting controllers
  I want a server that starts, stops and can say whether its port is free
  So a port already in use is a clear message and not a socket error from inside
''')

.UseWorld<TServerWorld>

.Rule('The server answers whether a port is free without a remoting stack @socket')

  .Scenario('A free port is reported as bindable')
    .Given('a server on a port nothing is listening on',
      procedure(W: TServerWorld)
      begin
        W.NewServerOn(FindFreePort);
      end)
    .When('the check is asked about that port',
      procedure(W: TServerWorld)
      begin
        W.FreeAnswer := W.Server.CanBindPort(W.Port);
      end)
    .&Then('it answers true',
      procedure(W: TServerWorld)
      begin
        Expect(W.FreeAnswer).ToEqual(True);
      end)
    .&And('a subsequent start on that port succeeds, proving the check released it',
      procedure(W: TServerWorld)
      begin
        W.Server.Start;
        Expect(W.Server.Active).ToEqual(True);
        W.Server.Stop;
      end)

  .Scenario('A held port is reported as unavailable')
    .Given('another socket genuinely listening on a port',
      procedure(W: TServerWorld)
      begin
        // Genuinely held, not a double that answers false on request: a check
        // that always says yes would pass a careless spec and fail in production.
        W.Held := THeldPort.Create(FindFreePort);
        W.NewServerOn(W.Held.Port);
      end)
    .When('the check is asked about it',
      procedure(W: TServerWorld)
      begin
        W.HeldAnswer := W.Server.CanBindPort(W.Held.Port);
      end)
    .&Then('it answers false',
      procedure(W: TServerWorld)
      begin
        Expect(W.HeldAnswer).ToEqual(False);
      end)

.Rule('The server has no lifecycle it does not implement @socket')

  .Scenario('Start and stop move the server between states')
    .Given('a server on a free port',
      procedure(W: TServerWorld)
      begin
        W.NewServerOn(FindFreePort);
      end)
    .When('it is started and then stopped',
      procedure(W: TServerWorld)
      begin
        W.Server.Start;
        Expect(W.Server.Active).ToEqual(True);
        W.Server.Stop;
        W.BindableAfterStop := W.Server.CanBindPort(W.Port);
      end)
    .&Then('it is inactive afterwards and the port is bindable again',
      procedure(W: TServerWorld)
      begin
        Expect(W.Server.Active).ToEqual(False);
        Expect(W.BindableAfterStop).ToEqual(True);
      end)

  .Scenario('Lifecycle events fire in order')
    .Given('a server on a free port with every lifecycle handler assigned',
      procedure(W: TServerWorld)
      begin
        W.NewServerOn(FindFreePort);
        W.WireEvents;
      end)
    .When('it is started and stopped',
      procedure(W: TServerWorld)
      begin
        W.Server.Start;
        W.Server.Stop;
      end)
    .&Then('starting, started, stopping and stopped each fired once, in that order',
      procedure(W: TServerWorld)
      begin
        Expect(W.Trace).ToEqual('starting,started,stopping,stopped,');
      end)

.Rule('Starting on a port that cannot be bound raises an exception that names the port @socket')

  .Scenario('Starting on a held port')
    .Given('another socket genuinely listening on a port, and a server on that port',
      procedure(W: TServerWorld)
      begin
        W.Held := THeldPort.Create(FindFreePort);
        W.NewServerOn(W.Held.Port);
      end)
    .When('the server is started',
      procedure(W: TServerWorld)
      begin
        try
          W.Server.Start;
          W.Trace := '<started>';
        except
          on E: Exception do
            W.Trace := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('it raises an exception that names the port',
      procedure(W: TServerWorld)
      begin
        Expect(W.Trace).ToContain(Format('port %d', [W.Port]));
      end)
    .&And('the server reports itself inactive',
      procedure(W: TServerWorld)
      begin
        Expect(W.Server.Active).ToEqual(False);
      end)

  .Scenario('Starting on a held port with an error handler')
    .Given('another socket listening on the port, and a server on it whose error handler notes its emitter and message',
      procedure(W: TServerWorld)
      begin
        W.Held := THeldPort.Create(FindFreePort);
        W.NewServerOn(W.Held.Port);
        W.Server.OnError := W.ServerError;
      end)
    .When('the server is started',
      procedure(W: TServerWorld)
      begin
        try
          W.Server.Start;
          W.Trace := '<started>';
        except
          on E: Exception do
            W.Trace := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the handler received the server as its emitter and an exception that names the port',
      procedure(W: TServerWorld)
      begin
        Expect(W.ErrorSeen).ToEqual(Format('TDafHttpServer | Cannot start: port %d is in use', [W.Port]));
      end)
    .&And('the start raises an exception with that same message',
      procedure(W: TServerWorld)
      begin
        Expect(W.Trace).ToEqual(Format('Exception: Cannot start: port %d is in use', [W.Port]));
      end)

.Rule('The contract does not name its implementation''s library')

  .Scenario('The contract survives replacing the server')
    .When('the members of IDafHttpServer are read',
      procedure(W: TServerWorld)
      begin
        var Ctx := TRttiContext.Create;
        try
          for var Member in Ctx.GetType(TypeInfo(IDafHttpServer)).GetMethods do
            if Member.Name.ToLower.Contains('indy') then
              W.LibraryNamedMembers := W.LibraryNamedMembers + Member.Name + ' ';
        finally
          Ctx.Free;
        end;
      end)
    .&Then('none of them names Indy, so swapping the implementation is not a change of contract',
      procedure(W: TServerWorld)
      begin
        Expect(W.LibraryNamedMembers.Trim).ToEqual('');
      end);

end.
