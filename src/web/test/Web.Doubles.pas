unit Web.Doubles;

{
  WebBroker offers no request or response a console process can construct: both
  are abstract, and every concrete pair in the RTL belongs to a transport. The
  doubles here are what let routing and action results — which are pure logic —
  be specified without a socket. The request is simply told its method, path,
  query and body; the response records what was written to it instead of
  sending it anywhere.

  The port helpers serve the specs that do need a socket. They ask the OS for a
  free port instead of trusting a constant: a suite that fails because someone
  else's process holds 8080 gets disabled, not fixed.
}

interface

uses
  System.Classes,
  System.SysUtils,
  Web.HTTPApp,
  IdSimpleServer;

type
  /// A request that answers from what it was told, and nothing else.
  TSpecWebRequest = class(TWebRequest)
  private
    FMethod: string;
    FPathInfo: string;
    FQuery: string;
    FContentType: string;
    FBody: TBytes;
  protected
    function GetRawContent: TBytes; override;
    function GetStringVariable(Index: Integer): string; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Int64; override;
  public
    constructor Create(const AMethod, APathInfo: string; const AQuery: string = '';
      const ABody: string = ''; const AContentType: string = 'application/json');
    function GetFieldByName(const Name: string): string; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): string; override;
    function TranslateURI(const URI: string): string; override;
    function WriteClient(var Buffer; Count: Integer): Integer; override;
    function WriteString(const AString: string): Boolean; override;
    function WriteHeaders(StatusCode: Integer; const ReasonString, Headers: string): Boolean; override;
  end;

  /// A response that keeps what was written to it, so a spec can read it back.
  TSpecWebResponse = class(TWebResponse)
  private
    FStrings: array [0 .. 11] of string;
    FIntegers: array [0 .. 0] of Int64;
    FDates: array [0 .. 2] of TDateTime;
    FContent: string;
    FLogMessage: string;
    FStatusCode: Integer;
    FSent: Boolean;
    FRedirectedTo: string;
  protected
    function GetStringVariable(Index: Integer): string; override;
    procedure SetStringVariable(Index: Integer; const Value: string); override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    function GetIntegerVariable(Index: Integer): Int64; override;
    procedure SetIntegerVariable(Index: Integer; Value: Int64); override;
    function GetContent: string; override;
    procedure SetContent(const Value: string); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(Value: Integer); override;
    function GetLogMessage: string; override;
    procedure SetLogMessage(const Value: string); override;
  public
    constructor Create(HTTPRequest: TWebRequest);
    procedure SendResponse; override;
    procedure SendRedirect(const URI: string); override;
    function Sent: Boolean; override;
    property RedirectedTo: string read FRedirectedTo;
  end;

  /// Holds a port for real, so a port check can be asked about one that is
  /// genuinely taken rather than one a mock claims is.
  THeldPort = class
  private
    FListener: TIdSimpleServer;
    FPort: Word;
  public
    constructor Create(const APort: Word);
    destructor Destroy; override;
    property Port: Word read FPort;
  end;

/// A port nothing is listening on, found by binding it and letting go.
function FindFreePort: Word;

implementation

{ TSpecWebRequest }

constructor TSpecWebRequest.Create(const AMethod, APathInfo, AQuery, ABody, AContentType: string);
begin
  inherited Create;
  FMethod := AMethod;
  FPathInfo := APathInfo;
  FQuery := AQuery;
  FContentType := AContentType;
  FBody := TEncoding.UTF8.GetBytes(ABody);
  // The inherited constructor already ran it, against the fields it could not
  // see yet. Now there is a method to derive the type from.
  UpdateMethodType;
end;

function TSpecWebRequest.GetRawContent: TBytes;
begin
  Result := FBody;
end;

function TSpecWebRequest.GetStringVariable(Index: Integer): string;
begin
  case Index of
    0: Result := FMethod;
    3: Result := FQuery;
    4: Result := FPathInfo;
    15: Result := FContentType;
  else
    Result := '';
  end;
end;

function TSpecWebRequest.GetDateVariable(Index: Integer): TDateTime;
begin
  Result := 0;
end;

function TSpecWebRequest.GetIntegerVariable(Index: Integer): Int64;
begin
  // 16 is ContentLength, and DoReadTotalContent asserts it matches RawContent.
  if Index = 16 then
    Result := Length(FBody)
  else
    Result := 0;
end;

function TSpecWebRequest.GetFieldByName(const Name: string): string;
begin
  Result := '';
end;

function TSpecWebRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  Result := 0;
end;

function TSpecWebRequest.ReadString(Count: Integer): string;
begin
  Result := '';
end;

function TSpecWebRequest.TranslateURI(const URI: string): string;
begin
  Result := URI;
end;

function TSpecWebRequest.WriteClient(var Buffer; Count: Integer): Integer;
begin
  Result := 0;
end;

function TSpecWebRequest.WriteString(const AString: string): Boolean;
begin
  Result := True;
end;

function TSpecWebRequest.WriteHeaders(StatusCode: Integer; const ReasonString, Headers: string): Boolean;
begin
  Result := True;
end;

{ TSpecWebResponse }

constructor TSpecWebResponse.Create(HTTPRequest: TWebRequest);
begin
  inherited Create(HTTPRequest);
  FStatusCode := 200;
end;

function TSpecWebResponse.GetStringVariable(Index: Integer): string;
begin
  Result := FStrings[Index];
end;

procedure TSpecWebResponse.SetStringVariable(Index: Integer; const Value: string);
begin
  FStrings[Index] := Value;
end;

function TSpecWebResponse.GetDateVariable(Index: Integer): TDateTime;
begin
  Result := FDates[Index];
end;

procedure TSpecWebResponse.SetDateVariable(Index: Integer; const Value: TDateTime);
begin
  FDates[Index] := Value;
end;

function TSpecWebResponse.GetIntegerVariable(Index: Integer): Int64;
begin
  Result := FIntegers[Index];
end;

procedure TSpecWebResponse.SetIntegerVariable(Index: Integer; Value: Int64);
begin
  FIntegers[Index] := Value;
end;

function TSpecWebResponse.GetContent: string;
begin
  Result := FContent;
end;

procedure TSpecWebResponse.SetContent(const Value: string);
begin
  FContent := Value;
end;

function TSpecWebResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

procedure TSpecWebResponse.SetStatusCode(Value: Integer);
begin
  FStatusCode := Value;
end;

function TSpecWebResponse.GetLogMessage: string;
begin
  Result := FLogMessage;
end;

procedure TSpecWebResponse.SetLogMessage(const Value: string);
begin
  FLogMessage := Value;
end;

procedure TSpecWebResponse.SendResponse;
begin
  FSent := True;
end;

procedure TSpecWebResponse.SendRedirect(const URI: string);
begin
  FRedirectedTo := URI;
  FSent := True;
end;

function TSpecWebResponse.Sent: Boolean;
begin
  Result := FSent;
end;

{ THeldPort }

constructor THeldPort.Create(const APort: Word);
begin
  inherited Create;
  FPort := APort;
  FListener := TIdSimpleServer.Create(nil);
  FListener.BoundPort := APort;
  FListener.BeginListen;
end;

destructor THeldPort.Destroy;
begin
  FListener.Free;
  inherited;
end;

function FindFreePort: Word;
begin
  // A high, unregistered range: low ports are the ones something else is
  // plausibly already using.
  for var Candidate := 51000 to 51199 do
  begin
    var Probe := TIdSimpleServer.Create(nil);
    try
      Probe.BoundPort := Candidate;
      try
        Probe.BeginListen;
        Probe.EndListen;
        Exit(Word(Candidate));
      except
        // Taken. Next.
      end;
    finally
      Probe.Free;
    end;
  end;
  raise Exception.Create('no free port in 51000..51199 for the socket specs to bind');
end;

end.
