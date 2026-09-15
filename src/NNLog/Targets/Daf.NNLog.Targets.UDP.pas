unit Daf.NNLog.Targets.UDP;

interface

uses
  System.SysUtils, System.Classes, IdUDPClient,
  Daf.Extensions.Logging, Daf.NNLog;

type
  TUDPTarget = class(TTarget)
  private
    FClient: TIdUDPClient;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    function GetHost: string;
    function GetPort: Integer;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
  end;

implementation

{ TUDPTarget }

constructor TUDPTarget.Create;
begin
  inherited;
  FClient := TIdUDPClient.Create(nil);
  Host := '127.0.0.1';
  Port := 12201; // default for https://graylog.org/
end;

destructor TUDPTarget.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TUDPTarget.GetHost: string;
begin
  Result := FClient.Host;
end;

function TUDPTarget.GetPort: Integer;
begin
  Result := FClient.Port;
end;

procedure TUDPTarget.SetHost(const Value: string);
begin
  FClient.Host := Value;
end;

procedure TUDPTarget.SetPort(const Value: Integer);
begin
  FClient.Port := Value;
end;

procedure TUDPTarget.DoWrite(const Entry: TLogEntry);
begin
  var Line := RenderEvent(Entry);
  FClient.Send(Line);
end;

end.

