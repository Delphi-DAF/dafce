unit MediatRSample.Behaviors;

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.MediatR.Contracts,
  MediatRSample.AppServices,
  MediatRSample.Customer,
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

  // Typed void behavior: short-circuits TAddCustomerCommand when the name is blank.
  // The framework routes it only to TAddCustomerCommand — no runtime type check needed.
  TValidationBehavior = class(TPipelineBehavior<TAddCustomerCommand>)
  private
    FLog: IAppLog;
  public
    constructor Create(const Log: IAppLog);
    procedure Handle(Request: TAddCustomerCommand; Next: TProc); override;
  end;

  // Typed response behavior: logs the number of results returned by TCustomerQuery.
  // Demonstrates TPipelineBehavior<TResponse, TRequest> — framework routes it only to queries.
  TQueryResultBehavior = class(TPipelineBehavior<TCustomer.TList, TCustomerQuery>)
  private
    FLog: IAppLog;
  public
    constructor Create(const Log: IAppLog);
    function Handle(Request: TCustomerQuery; Next: TFunc<TValue>): TCustomer.TList; override;
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

procedure TValidationBehavior.Handle(Request: TAddCustomerCommand; Next: TProc);
begin
  if Trim(Request.CustomerName).IsEmpty then
  begin
    FLog.Log('[Validation] TAddCustomerCommand rejected — name is empty');
    Exit;  // short-circuit: Next not called, handler never runs
  end;
  Next;
end;

{ TQueryResultBehavior }

constructor TQueryResultBehavior.Create(const Log: IAppLog);
begin
  inherited Create;
  FLog := Log;
end;

function TQueryResultBehavior.Handle(Request: TCustomerQuery; Next: TFunc<TValue>): TCustomer.TList;
begin
  Result := Next().AsType<TCustomer.TList>;
  FLog.Log(Format('[Query] TCustomerQuery → %d customer(s)', [Result.Count]));
end;

end.
