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

  // Next delegate for global (untyped) pipeline behaviors.
  TNextValue = record
  private
    FFunc: TFunc<TValue>;
  public
    constructor Create(const Func: TFunc<TValue>);
    function Call: TValue;
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
  IPipelineBehavior<TRequestType: class, IRequest> = interface(IPipelineBehavior)
    ['{375D7E25-BF87-433E-AF9B-BB056D4D2A7B}']
    procedure Handle(Request: TRequestType; Next: TNext);
  end;

  // Typed pipeline behavior for request/response handlers
  IPipelineBehavior<TResponse; TRequestType: class, IRequest<TResponse>> = interface(IPipelineBehavior)
    ['{9BDE0B6A-2D89-4F26-A5ED-64C9CFB4A3B8}']
    function Handle(Request: TRequestType; Next: TNext<TResponse>): TResponse;
  end;

  IRequestHandler<TRequestType: class, IRequest> = interface(IBaseRequesteHandler)
    ['{8618570D-30FB-49F1-8015-50E7E845975D}']
    procedure Handle(Request: TRequestType);
  end;

  IRequestHandler<TResponse;TRequestType:class, IRequest<TResponse>> = interface(IBaseResponseHandler)
    ['{A1A470A0-DE1B-45E2-9C22-2BECBDCD5C73}']
    procedure Handle(Request: TRequestType; out Result: TResponse);
  end;

  INotificationHandler<TNotificationType: class, INotification> = interface(IBaseHandler)
    ['{F7DB0B37-0CB7-4CBD-B11A-283B65E23DA1}']
    procedure Handle(Notification: TNotificationType);
  end;

  IMediator = record
  strict private
    FImpl: IMediatorImpl;
  public
    class operator Implicit(Impl: IMediatorImpl): IMediator;
    class operator Implicit(Med: IMediator): IMediatorImpl;
    class operator Equal(Med: IMediator; P: Pointer): Boolean;
    class operator NotEqual(Med: IMediator; P: Pointer): Boolean;

    procedure Send<TRequestType: class, IRequest>(Request: TRequestType);overload;
    function Send<TResponse; TRequestType: class, IRequest<TResponse>>(Request: TRequestType): TResponse; overload;
    procedure Publish<TNotificationType: class, INotification>(Notification: TNotificationType);
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
  // Invoke is the framework dispatch entry point, and is not in any interface.
  // Global behaviors override Handle(TObject,...); typed behaviors override typed Handle.
  [MediatorAbstract]
  TPipelineBehavior = class abstract(TInterfacedObject, IPipelineBehavior)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; virtual;
    function Handle(Request: TObject; Next: TNextValue): TValue; virtual;
  end;

  [MediatorAbstract]
  TPipelineBehavior<TRequestType: class, IRequest> = class abstract(TPipelineBehavior, IPipelineBehavior<TRequestType>)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
    procedure Handle(Request: TRequestType; Next: TNext); reintroduce; virtual; abstract;
  end;

  [MediatorAbstract]
  TPipelineBehavior<TResponse; TRequestType: class, IRequest<TResponse>> = class abstract(TPipelineBehavior, IPipelineBehavior<TResponse, TRequestType>)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
    function HandleBridge(Request: TRequestType; Next: TFunc<TValue>): TResponse;
    function Handle(Request: TRequestType; Next: TNext<TResponse>): TResponse; reintroduce; virtual; abstract;
  end;

  [MediatorAbstract]
  TRequestHandler<TRequestType: class, IRequest> = class abstract(TBaseHandler, IRequestHandler<TRequestType>)
  strict private
    FMediator: IMediator;
    FRequest: TRequestType;
  public
    constructor Create(const Mediator: IMediatorImpl);
    procedure Handle(Request: TRequestType);virtual;
    property Mediator: IMediator read FMediator;
    property Request: TRequestType read FRequest;
  end;

  [MediatorAbstract]
  TResponseHandler<TResponse;TRequestType: class, IRequest<TResponse>> = class abstract(TBaseHandler, IRequestHandler<TResponse, TRequestType>)
    strict private
    FMediator: IMediator;
    FRequest: TRequestType;
  public
    constructor Create(const Mediator: IMediatorImpl);
    procedure Handle(Request: TRequestType; out Result: TResponse); virtual;
    property Mediator: IMediator read FMediator;
    property Request: TRequestType read FRequest;
  end;

  [MediatorAbstract]
  TNotificationHandler<TNotificationType: class, INotification> = class abstract(TBaseHandler, INotificationHandler<TNotificationType>)
  strict private
    FMediator: IMediator;
    FNotification: TNotificationType;
  private
    FRequest: TNotificationType;
  public
    constructor Create(const Mediator: IMediatorImpl);
    procedure Handle(Notification: TNotificationType); virtual;
    property Notification: TNotificationType read FRequest;
  end;

  [MediatorAbstract]
  TNotificacionHandler<TNotificationType: class, INotification> = class abstract(TNotificationHandler<TNotificationType>)
  end deprecated 'Use TNotificationHandler';

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
    function SendARC<TResponse: class; TRequestType: class, IRequest<TResponse>>(const Request: TRequestType): ARC<TResponse>;
  end;

implementation

function IMediatorHelper.SendARC<TResponse, TRequestType>(const Request: TRequestType): ARC<TResponse>;
begin
  Result := ARC.From(Send<TResponse, TRequestType>(Request))
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

procedure IMediator.Publish<TNotificationType>(Notification: TNotificationType);
begin
  var
    Result: Nativeint;
  var
  THandler := TypeInfo(INotificationHandler<TNotificationType>);
  FImpl.InvokeHandler(THandler, Notification, Result);
end;

procedure IMediator.Send<TRequestType>(Request: TRequestType);
begin
  var
    Result: NativeInt;
  var
  THandler := TypeInfo(IRequestHandler<TRequestType>);
  FImpl.InvokeHandler(THandler, Request, Result);
end;

function IMediator.Send<TResponse, TRequestType>(Request: TRequestType): TResponse;
begin
  var
  THandler := TypeInfo(IRequestHandler<TResponse, TRequestType>);
  FImpl.InvokeHandler(THandler, Request, Result);
end;

{ TRequestHandler<TRequestType> }

constructor TRequestHandler<TRequestType>.Create(const Mediator: IMediatorImpl);
begin
  inherited Create;
  FMediator := Mediator;
end;

procedure TRequestHandler<TRequestType>.Handle(Request: TRequestType);
begin
  FRequest := Request;
end;

{ TResponseHandler<TResponse, TRequestType> }

constructor TResponseHandler<TResponse, TRequestType>.Create(const Mediator: IMediatorImpl);
begin
  inherited Create;
  FMediator := Mediator;
end;

procedure TResponseHandler<TResponse, TRequestType>.Handle(Request: TRequestType; out Result: TResponse);
begin
  FRequest := Request;
  Result := Default (TResponse);
end;

{ TNotificationHandler<TNotificationType> }

constructor TNotificationHandler<TNotificationType>.Create(const Mediator: IMediatorImpl);
begin
  inherited Create;
  FMediator := Mediator;
end;

procedure TNotificationHandler<TNotificationType>.Handle(Notification: TNotificationType);
begin
  FNotification := Notification;
end;

{ TNextValue }

constructor TNextValue.Create(const Func: TFunc<TValue>);
begin
  FFunc := Func;
end;

function TNextValue.Call: TValue;
begin
  Result := FFunc();
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

function TPipelineBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  raise EAbstractError.CreateFmt('%s must override Handle', [ClassName]);
end;

function TPipelineBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Result := Handle(Request, TNextValue.Create(Next));
end;

{ TPipelineBehavior<TRequestType> }

function TPipelineBehavior<TRequestType>.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Handle(TRequestType(Request), TNext.Create(procedure begin Next(); end));
  Result := Default(TValue);
end;

{ TPipelineBehavior<TResponse, TRequestType> }

function TPipelineBehavior<TResponse, TRequestType>.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
var
  RC: TRttiContext;
  NextVal, RequestVal: TValue;
begin
  // Use RTTI to call HandleBridge — avoids unconstrained generic return type (Delphi E2008 limitation).
  // RTTI.Invoke boxes TResponse → TValue in the runtime layer so the generic body never sees it.
  TValue.Make(@Next, TypeInfo(TFunc<TValue>), NextVal);
  TValue.Make(@Request, Request.ClassInfo, RequestVal);
  RC := TRttiContext.Create;
  try
    Result := RC.GetType(Self.ClassType).GetMethod('HandleBridge').Invoke(Self, [RequestVal, NextVal]);
  finally
    RC.Free;
  end;
end;

function TPipelineBehavior<TResponse, TRequestType>.HandleBridge(Request: TRequestType; Next: TFunc<TValue>): TResponse;
begin
  Result := Handle(Request, TNext<TResponse>.Create(Next));
end;

end.
