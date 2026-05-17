unit Daf.MediatR.Contracts;

interface

uses
  System.TypInfo,
  System.SysUtils,
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

  IBasePipelineBehavior = interface(IInvokable)
    ['{5F0EFB5C-BB72-4A9E-BD2D-64AB79E4C8CA}']
  end;

  IPipelineBehaviorInvoker = interface(IBasePipelineBehavior)
    ['{8C8F35D2-AB67-4EFB-8C94-C5DBF5B47DBE}']
    function Before(Request: TObject): Boolean;
    procedure After(Request: TObject);
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

end.
