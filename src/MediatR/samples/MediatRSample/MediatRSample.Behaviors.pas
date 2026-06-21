unit MediatRSample.Behaviors;

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.MediatR.Contracts,
  MediatRSample.AppServices,
  MediatRSample.Requests;

type
  // Outermost behavior: logs every request entry/exit and elapsed time.
  TLoggingBehavior = class(TPipelineBehavior)
  private
    FLog: IAppLog;
  public
    constructor Create(const Log: IAppLog);
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  // Inner behavior: short-circuits TAddCustomerCommand when the name is blank.
  // Demonstrates pipeline short-circuit without raising an exception.
  TValidationBehavior = class(TPipelineBehavior)
  private
    FLog: IAppLog;
  public
    constructor Create(const Log: IAppLog);
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

implementation

uses
  Winapi.Windows;

{ TLoggingBehavior }

constructor TLoggingBehavior.Create(const Log: IAppLog);
begin
  inherited Create;
  FLog := Log;
end;

function TLoggingBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
var
  Start: Cardinal;
begin
  FLog.Log(Format('[Pipeline] >> %s', [Request.ClassName]));
  Start := GetTickCount;
  try
    Result := Next();
    FLog.Log(Format('[Pipeline] << %s  (%d ms)', [Request.ClassName, GetTickCount - Start]));
  except
    on E: Exception do
    begin
      FLog.Log(Format('[Pipeline] !! %s  %s: %s', [Request.ClassName, E.ClassName, E.Message]));
      raise;
    end;
  end;
end;

{ TValidationBehavior }

constructor TValidationBehavior.Create(const Log: IAppLog);
begin
  inherited Create;
  FLog := Log;
end;

function TValidationBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  if Request is TAddCustomerCommand then
  begin
    var Cmd := TAddCustomerCommand(Request);
    if Trim(Cmd.CustomerName).IsEmpty then
    begin
      FLog.Log('[Validation] TAddCustomerCommand rejected — name is empty');
      Exit(TValue.Empty);  // short-circuit: handler never called
    end;
  end;
  Result := Next();
end;

end.
