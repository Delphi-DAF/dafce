unit Daf.MediatR.Contracts;

interface

uses
  System.TypInfo,
  System.SysUtils,
  System.Rtti,
  Daf.MemUtils,
  Daf.Extensions.DependencyInjection;

type

  IMediatorImpl = interface(IInterface)
    ['{16327968-C6F0-4CA0-B787-509026A480A6}']
    procedure InvokeHandler(PInfo: PTypeInfo; Instance: TObject; out Result);
  end;

  IBaseRequest = interface(IInvokable)
    ['{2516649C-A526-47EE-826A-F585188F5BB0}']
  end;

  IRequest = interface(IBaseRequest)
    ['{9BB79B88-1E5F-4432-A5E4-01BD38043809}']
  end;

  IRequest<TResponse> = interface(IRequest)
    ['{2332F920-F8EB-45B2-A9B1-8D0A7FB1B5C4}']
  end;

  INotification = interface(IBaseRequest)
    ['{BD7BFE9A-5463-425E-A970-A5B51DE4150A}']
  end;

  IBaseHandler = interface(IInvokable)
    ['{CC98E878-08A3-4730-B532-B16E32B37EAB}']
  end;

  IBaseRequesteHandler = interface(IBaseHandler)
    ['{A5873CEB-3CA9-4D5E-A3E2-859F4669D841}']
  end;

  IBaseResponseHandler = interface(IBaseHandler)
    ['{30EB7479-212A-4F0C-9014-0B43A3DA9F02}']
  end;

  IBaseNotificationHandler = interface(IBaseHandler)
    ['{D7E2B7EA-E72E-4332-89AE-D405380ADB23}']
  end;

  // Typed Next delegate for void pipeline behaviors.
  TNext = record
  private
    FProc: TProc;
  public
    constructor Create(const Proc: TProc);
    procedure Call;
  end;

  // Typed Next delegate for response pipeline behaviors.
  // Centralizes TValue → TResponse unboxing so behaviors don't need AsType.
  TNext<TResponse> = record
  private
    FFunc: TFunc<TValue>;
  public
    constructor Create(const Func: TFunc<TValue>);
    function Call: TResponse;
  end;

  // Marker interface — DI scan/registration target for all pipeline behaviors.
  // Invoke is intentionally absent: mediator casts to TPipelineBehavior (class) at runtime.
  IPipelineBehavior = interface(IInvokable)
    ['{5F0EFB5C-BB72-4A9E-BD2D-64AB79E4C8CA}']
  end;

  // Typed pipeline behavior for void requests
  IPipelineBehavior<TRequest: class, IRequest> = interface(IPipelineBehavior)
    ['{375D7E25-BF87-433E-AF9B-BB056D4D2A7B}']
    procedure Handle(Request: TRequest; Next: TNext);
  end;

  // Typed pipeline behavior for request/response handlers
  IPipelineBehavior<TResponse; TRequest: class, IRequest<TResponse>> = interface(IPipelineBehavior)
    ['{9BDE0B6A-2D89-4F26-A5ED-64C9CFB4A3B8}']
    function Handle(Request: TRequest; Next: TNext<TResponse>): TResponse;
  end;

  IRequestHandler<TRequest: class, IRequest> = interface(IBaseRequesteHandler)
    ['{8618570D-30FB-49F1-8015-50E7E845975D}']
    procedure Handle(Request: TRequest);
  end;

  IRequestHandler<TResponse;TRequest:class, IRequest<TResponse>> = interface(IBaseResponseHandler)
    ['{A1A470A0-DE1B-45E2-9C22-2BECBDCD5C73}']
    procedure Handle(Request: TRequest; out Result: TResponse);
  end;

  INotificationHandler<TNotification: class, INotification> = interface(IBaseHandler)
    ['{F7DB0B37-0CB7-4CBD-B11A-283B65E23DA1}']
    procedure Handle(Notification: TNotification);
  end;

  IMediator = record
  strict private
    FImpl: IMediatorImpl;
  public
    class operator Implicit(Impl: IMediatorImpl): IMediator;
    class operator Implicit(Med: IMediator): IMediatorImpl;
    class operator Equal(Med: IMediator; P: Pointer): Boolean;
    class operator NotEqual(Med: IMediator; P: Pointer): Boolean;

    procedure Send<TRequest: class, IRequest>(Request: TRequest);overload;
    function Send<TResponse; TRequest: class, IRequest<TResponse>>(Request: TRequest): TResponse; overload;
    procedure Publish<TNotification: class, INotification>(Notification: TNotification);
  end;

  // Tell Mediator to consider some class as an abstract handler to ignore.
  // This is needed because abstract keyword don't generate RTTI at this moment
  MediatorAbstractAttribute = class(TCustomAttribute)
  public
  end;

  [MediatorAbstract]
  TBaseHandler = class abstract(TInterfacedObject, IBaseHandler)
  end;

  // Base class for all pipeline behaviors.
  // Invoke is the framework dispatch entry point — not virtual, not in any interface.
  // Global behaviors override Handle(TObject,...); typed behaviors override typed Handle.
  [MediatorAbstract]
  TPipelineBehavior = class abstract(TInterfacedObject, IPipelineBehavior)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; virtual;
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; virtual;
  end;

  [MediatorAbstract]
  TPipelineBehavior<TRequest: class, IRequest> = class abstract(TPipelineBehavior, IPipelineBehavior<TRequest>)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
    procedure Handle(Request: TRequest; Next: TNext); reintroduce; virtual; abstract;
  end;

  [MediatorAbstract]
  TPipelineBehavior<TResponse; TRequest: class, IRequest<TResponse>> = class abstract(TPipelineBehavior, IPipelineBehavior<TResponse, TRequest>)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
    function HandleBridge(Request: TRequest; Next: TFunc<TValue>): TResponse;
    function Handle(Request: TRequest; Next: TNext<TResponse>): TResponse; reintroduce; virtual; abstract;
  end;

  [MediatorAbstract]
  TRequestHandler<TRequest: class, IRequest> = class abstract(TBaseHandler, IRequestHandler<TRequest>)
  strict private
    FMediator: IMediator;
    FRequest: TRequest;
  public
    constructor Create(const Mediator: IMediatorImpl);
    procedure Handle(Request: TRequest);virtual;
    property Mediator: IMediator read FMediator;
    property Request: TRequest read FRequest;
  end;

  [MediatorAbstract]
  TResponseHandler<TResponse;TRequest: class, IRequest<TResponse>> = class abstract(TBaseHandler, IRequestHandler<TResponse, TRequest>)
    strict private
    FMediator: IMediator;
    FRequest: TRequest;
  public
    constructor Create(const Mediator: IMediatorImpl);
    procedure Handle(Request: TRequest; out Result: TResponse); virtual;
    property Mediator: IMediator read FMediator;
    property Request: TRequest read FRequest;
  end;

  [MediatorAbstract]
  TNotificacionHandler<TNotification: class, INotification> = class abstract(TBaseHandler, INotificationHandler<TNotification>)
  strict private
    FMediator: IMediator;
    FNotification: TNotification;
  private
    FRequest: TNotification;
  public
    constructor Create(const Mediator: IMediatorImpl);
    procedure Handle(Notification: TNotification); virtual;
    property Notification: TNotification read FRequest;
  end;

  [MediatorAbstract]
  TRequest<TResponse> = class abstract(TInterfacedObject, IRequest<TResponse>)
  end;

  [MediatorAbstract]
  TRequest = class abstract(TInterfacedObject, IRequest)
  end;

  [MediatorAbstract]
  TNotification = class abstract(TInterfacedObject, INotification)
  end;

  IMediatorHelper = record helper for IMediator
  public
    function SendARC<TResponse: class; TRequest: class, IRequest<TResponse>>(const Request: TRequest): ARC<TResponse>;
  end;

implementation

function IMediatorHelper.SendARC<TResponse, TRequest>(const Request: TRequest): ARC<TResponse>;
begin
  Result := ARC.From(Send<TResponse, TRequest>(Request))
end;

{ IMediator }

class operator IMediator.Implicit(Impl: IMediatorImpl): IMediator;
begin
  Result.FImpl := Impl;
end;

class operator IMediator.Equal(Med: IMediator; P: Pointer): Boolean;
begin
  Result := Pointer(Med.FImpl) = P;
end;

class operator IMediator.NotEqual(Med: IMediator; P: Pointer): Boolean;
begin
  Result := Pointer(Med.FImpl) <> P;
end;

class operator IMediator.Implicit(Med: IMediator): IMediatorImpl;
begin
  Result := Med.FImpl;
end;

procedure IMediator.Publish<TNotification>(Notification: TNotification);
begin
  var
    Result: Nativeint;
  var
  THandler := TypeInfo(INotificationHandler<TNotification>);
  FImpl.InvokeHandler(THandler, Notification, Result);
end;

procedure IMediator.Send<TRequest>(Request: TRequest);
begin
  var
    Result: NativeInt;
  var
  THandler := TypeInfo(IRequestHandler<TRequest>);
  FImpl.InvokeHandler(THandler, Request, Result);
end;

function IMediator.Send<TResponse, TRequest>(Request: TRequest): TResponse;
begin
  var
  THandler := TypeInfo(IRequestHandler<TResponse, TRequest>);
  FImpl.InvokeHandler(THandler, Request, Result);
end;

{ TRequestHandler<TRequest> }

constructor TRequestHandler<TRequest>.Create(const Mediator: IMediatorImpl);
begin
  inherited Create;
  FMediator := Mediator;
end;

procedure TRequestHandler<TRequest>.Handle(Request: TRequest);
begin
  FRequest := Request;
end;

{ TResponseHandler<TResponse, TRequest> }

constructor TResponseHandler<TResponse, TRequest>.Create(const Mediator: IMediatorImpl);
begin
  inherited Create;
  FMediator := Mediator;
end;

procedure TResponseHandler<TResponse, TRequest>.Handle(Request: TRequest; out Result: TResponse);
begin
  FRequest := Request;
  Result := Default (TResponse);
end;

{ TNotificacionHandler<TNotification> }

constructor TNotificacionHandler<TNotification>.Create(const Mediator: IMediatorImpl);
begin
  inherited Create;
  FMediator := Mediator;
end;

procedure TNotificacionHandler<TNotification>.Handle(Notification: TNotification);
begin
  FNotification := Notification;
end;

{ TNext }

constructor TNext.Create(const Proc: TProc);
begin
  FProc := Proc;
end;

procedure TNext.Call;
begin
  FProc;
end;

{ TNext<TResponse> }

constructor TNext<TResponse>.Create(const Func: TFunc<TValue>);
begin
  FFunc := Func;
end;

function TNext<TResponse>.Call: TResponse;
begin
  Result := FFunc().AsType<TResponse>;
end;

{ TPipelineBehavior }

function TPipelineBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  raise EAbstractError.CreateFmt('%s must override Handle', [ClassName]);
end;

function TPipelineBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Result := Handle(Request, Next);
end;

{ TPipelineBehavior<TRequest> }

function TPipelineBehavior<TRequest>.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Handle(TRequest(Request), TNext.Create(procedure begin Next(); end));
  Result := Default(TValue);
end;

{ TPipelineBehavior<TResponse, TRequest> }

function TPipelineBehavior<TResponse, TRequest>.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
var
  RC: TRttiContext;
  M: TRttiMethod;
  NextVal, RequestVal: TValue;
begin
  // Use RTTI to call HandleBridge — avoids unconstrained generic return type (Delphi E2008 limitation).
  // HandleBridge is non-abstract and lives on the generic base class, so GetMethods is needed.
  TValue.Make(@Next, TypeInfo(TFunc<TValue>), NextVal);
  TValue.Make(@Request, Request.ClassInfo, RequestVal);
  RC := TRttiContext.Create;
  try
    for M in RC.GetType(Self.ClassType).GetMethods do
      if M.Name = 'HandleBridge' then
      begin
        Result := M.Invoke(Self, [RequestVal, NextVal]);
        Exit;
      end;
  finally
    RC.Free;
  end;
  Result := Default(TValue);
end;

function TPipelineBehavior<TResponse, TRequest>.HandleBridge(Request: TRequest; Next: TFunc<TValue>): TResponse;
begin
  Result := Handle(Request, TNext<TResponse>.Create(Next));
end;

end.
