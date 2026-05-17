unit Daf.MediatR.Mediator;

interface

uses
  System.TypInfo,
  System.Rtti,
  Daf.Extensions.DependencyInjection,
  Daf.MediatR.Contracts;

type
  TMediator = class(TInterfacedObject, IMediatorImpl)
  private
    FServiceProvider: IServiceProvider;
  public
    constructor Create(ServiceProvider: IServiceProvider);
    procedure InvokeHandler(HandlerType: PTypeInfo; Instance: TObject; out Result);
  end;

implementation

uses
  Daf.Rtti,
  System.SysUtils;

type
  INotificationInvoker = INotificationHandler<TNotification>;
  IRequestInvoker = IRequestHandler<TRequest>;

  IResponseInvoker = interface(IInterface)
    procedure Handle(Request: TObject; out Result);
  end;

  IHandlerWrapper = interface(IInterface)
    procedure Handle(out Result);
  end;

  THandlerWrapper = class(TInterfacedObject, IHandlerWrapper)
  private
    FServiceProvider: IServiceProvider;
    FHandlerType: PTypeInfo;
    FInstance: TObject;
  public
    constructor Create(ServiceProvider: IServiceProvider; HandlerType: PTypeInfo; Instance: TObject);
    procedure Handle(out Result); virtual; abstract;
    destructor Destroy; override;
  end;

  TRequestWrapper = class(THandlerWrapper)
  public
    procedure Handle(out Result); override;
  end;

  TResponseWrapper = class(THandlerWrapper)
  public
    procedure Handle(out Result); override;
  end;

  TNotificationWrapper = class(THandlerWrapper)
  public
    procedure Handle(out Result); override;
  end;

  { TMediator }

constructor TMediator.Create(ServiceProvider: IServiceProvider);
begin
  inherited Create;
  FServiceProvider := ServiceProvider;
end;

procedure TMediator.InvokeHandler(HandlerType: PTypeInfo; Instance: TObject; out Result);
begin
  var
    Handler: IHandlerWrapper;
  if _T.Extends(HandlerType, TypeInfo(IBaseResponseHandler)) then
    Handler := TResponseWrapper.Create(FServiceProvider, HandlerType, Instance)
  else if _T.Extends(HandlerType, TypeInfo(IBaseRequesteHandler)) then
    Handler := TRequestWrapper.Create(FServiceProvider, HandlerType, Instance)
  else
  begin
    Handler := TNotificationWrapper.Create(FServiceProvider, HandlerType, Instance);
    Handler.Handle(Result);
    Exit;
  end;

  var Behaviors := FServiceProvider.GetServices(TypeInfo(IPipelineBehaviorInvoker));
  if (Behaviors = nil) or (Behaviors.Count = 0) then
  begin
    Handler.Handle(Result);
    Exit;
  end;

  var ExecutedCount := 0;
  for var I := 0 to Behaviors.Count - 1 do
  begin
    var Behavior := Behaviors[I] as IPipelineBehaviorInvoker;
    if not Behavior.Before(Instance) then
      Break;
    Inc(ExecutedCount);
  end;

  if ExecutedCount = Behaviors.Count then
    Handler.Handle(Result);

  for var I := ExecutedCount - 1 downto 0 do
    (Behaviors[I] as IPipelineBehaviorInvoker).After(Instance);
end;

{ THandlerWrapper }

constructor THandlerWrapper.Create(ServiceProvider: IServiceProvider; HandlerType: PTypeInfo; Instance: TObject);
begin
  inherited Create;
  FServiceProvider := ServiceProvider;
  FHandlerType := HandlerType;
  FInstance := Instance;
end;

destructor THandlerWrapper.Destroy;
begin
  FInstance.Free;
  inherited;
end;

{ TRequestWrapper }

procedure TRequestWrapper.Handle(out Result);
begin
  var
    Handler: IRequestInvoker;
  FServiceProvider.GetRequiredService(FHandlerType, Handler);
  Handler.Handle(TRequest(FInstance));
end;

{ TResponseWrapper }

procedure TResponseWrapper.Handle(out Result);
begin
  var
    Handler: IResponseInvoker;
  FServiceProvider.GetRequiredService(FHandlerType, Handler);
  Handler.Handle(FInstance, Result);
end;

{ TNotificationWrapper }

procedure TNotificationWrapper.Handle(out Result);
begin
  var
  IntfHandlers := FServiceProvider.GetServices(FHandlerType);
  for var IntfHandler in IntfHandlers do
    (IntfHandler as INotificationInvoker).Handle(TNotification(FInstance));
end;

end.
