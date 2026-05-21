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

function GetBehaviorRequestType(BehaviorClass: TClass): PTypeInfo;
var
  RC: TRttiContext;
  T: TRttiType;
  M: TRttiMethod;
  Params: TArray<TRttiParameter>;
begin
  Result := nil;
  RC := TRttiContext.Create;
  try
    T := RC.GetType(BehaviorClass);
    for M in T.GetDeclaredMethods do
    begin
      if M.Name = 'Handle' then
      begin
        Params := M.GetParameters;
        if (Length(Params) >= 1) and (Params[0].ParamType <> nil) and
           (Params[0].ParamType.Handle <> TypeInfo(TObject)) then
        begin
          Result := Params[0].ParamType.Handle;
          Exit;
        end;
      end;
    end;
  finally
    RC.Free;
  end;
end;

// Each call creates its own scope so BehaviorRef/Prev are not shared
// across loop iterations (Delphi anonymous methods capture by reference).
function MakeBehaviorChain(Behavior: TPipelineBehavior; Instance: TObject;
  Prev: TFunc<TValue>): TFunc<TValue>;
begin
  Result := function: TValue
  begin
    Result := Behavior.Invoke(Instance, Prev);
  end;
end;

function GetHandlerResponseType(HandlerType: PTypeInfo): PTypeInfo;
var
  RC: TRttiContext;
  T: TRttiType;
  M: TRttiMethod;
  P: TRttiParameter;
begin
  Result := nil;
  RC := TRttiContext.Create;
  try
    T := RC.GetType(HandlerType);
    for M in T.GetMethods do
      if M.Name = 'Handle' then
        for P in M.GetParameters do
          if pfOut in P.Flags then
          begin
            if P.ParamType <> nil then
              Result := P.ParamType.Handle;
            Exit;
          end;
  finally
    RC.Free;
  end;
end;

  { TMediator }

constructor TMediator.Create(ServiceProvider: IServiceProvider);
begin
  inherited Create;
  FServiceProvider := ServiceProvider;
end;

procedure TMediator.InvokeHandler(HandlerType: PTypeInfo; Instance: TObject; out Result);
var
  Handler: IHandlerWrapper;
  Chain: TFunc<TValue>;
  FinalValue: TValue;
  ResponseTypeInfo: PTypeInfo;
  ResultPtr: Pointer;
begin
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

  var Behaviors_list := FServiceProvider.GetServices(TypeInfo(IPipelineBehavior));
  if (Behaviors_list = nil) or (Behaviors_list.Count = 0) then
  begin
    Handler.Handle(Result);
    Exit;
  end;

  // Capture pointer to out Result before closures shadow it
  ResultPtr := @Result;
  ResponseTypeInfo := nil;
  if _T.Extends(HandlerType, TypeInfo(IBaseResponseHandler)) then
    ResponseTypeInfo := GetHandlerResponseType(HandlerType);

  // Terminal: runs handler and boxes result as TValue
  Chain := function: TValue
  begin
    Handler.Handle(ResultPtr^);
    if ResponseTypeInfo <> nil then
      TValue.Make(ResultPtr, ResponseTypeInfo, Result)
    else
      Result := Default(TValue);
  end;

  // Build chain right-to-left (index 0 = outermost).
  // MakeBehaviorChain creates a fresh scope per iteration, avoiding the
  // Delphi anonymous-method variable-capture bug (shared ActRec in loop).
  for var I := Behaviors_list.Count - 1 downto 0 do
  begin
    var Behavior := Behaviors_list[I] as IPipelineBehavior;
    var BehaviorObj := (Behavior as TObject) as TPipelineBehavior;
    var ReqTypeInfo := GetBehaviorRequestType(BehaviorObj.ClassType);
    if ReqTypeInfo <> nil then
    begin
      var ReqClass := GetTypeData(ReqTypeInfo)^.ClassType;
      if not Instance.ClassType.InheritsFrom(ReqClass) then
        Continue;
    end;
    Chain := MakeBehaviorChain(BehaviorObj, Instance, Chain);
  end;

  FinalValue := Chain();

  // Write back modified response (behaviors may have changed it)
  if (ResponseTypeInfo <> nil) and not FinalValue.IsEmpty then
    FinalValue.ExtractRawData(ResultPtr);
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
