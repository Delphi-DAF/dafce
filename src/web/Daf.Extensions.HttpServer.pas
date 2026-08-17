unit Daf.Extensions.HttpServer;

interface
uses
  System.Classes,
  System.SysUtils;

type
  /// <remarks>
  /// There is deliberately no member here reporting a version. The one that
  /// existed returned Indy's, under a name that said so, and a contract naming
  /// the library its implementation happens to use is a dependency that cannot
  /// be replaced. What a caller actually wants — which pieces the platform is
  /// made of, and at which versions: Indy, OpenSSL, WebBroker — is an API of
  /// its own, and it is designed as one rather than smuggled in as a string.
  /// </remarks>
  IDafHttpServer = interface(IInvokable)
    function GetSessionID: string;
    function CanBindPort(const APort: Integer): Boolean;overload;
    function CanBindPort: Boolean;overload;
    function GetPort: Word;
    procedure SetPort(const Value: Word);
    function GetActive: Boolean;
    procedure Start;
    procedure Stop;
    function GetOnStarting: TNotifyEvent;
    procedure SetOnStarting(const Value: TNotifyEvent);
    function GetOnStarted: TNotifyEvent;
    procedure SetOnStarted(const Value: TNotifyEvent);
    function GetOnStopping: TNotifyEvent;
    procedure SetOnStopping(const Value: TNotifyEvent);
    function GetOnStopped: TNotifyEvent;
    procedure SetOnStopped(const Value: TNotifyEvent);
    function GetOnError: TNotifyEvent;
    procedure SetOnError(const Value: TNotifyEvent);
    function GetOnDestroy: TNotifyEvent;
    procedure SetOnDestroy(const Value: TNotifyEvent);

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
end.
