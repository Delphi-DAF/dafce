unit MediatRSample.Notifications;

// Audit notification handlers — pure domain concern, no UI knowledge.
// Each notification type gets logged via IAppLog.
// A second set of handlers (TForm*Handler) in MainForm.pas handles the UI refresh.

interface

uses
  System.SysUtils,
  Daf.MediatR.Contracts,
  MediatRSample.AppServices,
  MediatRSample.Requests;

type
  TCustomerAddedAuditHandler = class(TNotificacionHandler<TCustomerAddedEvent>)
  private
    FLog: IAppLog;
  public
    constructor Create(const Mediator: IMediator; const Log: IAppLog);
    procedure Handle(Notification: TCustomerAddedEvent); override;
  end;

  TCustomerRemovedAuditHandler = class(TNotificacionHandler<TCustomerRemovedEvent>)
  private
    FLog: IAppLog;
  public
    constructor Create(const Mediator: IMediator; const Log: IAppLog);
    procedure Handle(Notification: TCustomerRemovedEvent); override;
  end;

implementation

{ TCustomerAddedAuditHandler }

constructor TCustomerAddedAuditHandler.Create(const Mediator: IMediator; const Log: IAppLog);
begin
  inherited Create(Mediator);
  FLog := Log;
end;

procedure TCustomerAddedAuditHandler.Handle(Notification: TCustomerAddedEvent);
begin
  FLog.Log(Format('[Notification] CustomerAdded  — #%d "%s"',
    [Notification.Customer.Id, Notification.Customer.Name]));
end;

{ TCustomerRemovedAuditHandler }

constructor TCustomerRemovedAuditHandler.Create(const Mediator: IMediator; const Log: IAppLog);
begin
  inherited Create(Mediator);
  FLog := Log;
end;

procedure TCustomerRemovedAuditHandler.Handle(Notification: TCustomerRemovedEvent);
begin
  FLog.Log(Format('[Notification] CustomerRemoved — #%d "%s"',
    [Notification.CustomerId, Notification.CustomerName]));
end;

end.
