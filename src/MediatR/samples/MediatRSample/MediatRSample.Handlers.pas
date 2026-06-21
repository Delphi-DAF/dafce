unit MediatRSample.Handlers;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Daf.MediatR.Contracts,
  MediatRSample.Customer,
  MediatRSample.Requests;

type
  TAddCustomerCommandHandler = class(TRequestHandler<TAddCustomerCommand>)
  private
    FCustomerStore: ICustomerStore;
  public
    constructor Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
    procedure Handle(Command: TAddCustomerCommand); override;
  end;

  TRemoveCustomerCommandHandler = class(TRequestHandler<TRemoveCustomerCommand>)
  private
    FCustomerStore: ICustomerStore;
  public
    constructor Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
    procedure Handle(Command: TRemoveCustomerCommand); override;
  end;

  TCustomerQueryHandler = class(TResponseHandler<TCustomer.TList, TCustomerQuery>)
  private
    FCustomerStore: ICustomerStore;
  public
    constructor Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
    procedure Handle(Request: TCustomerQuery; out Result: TCustomer.TList); override;
  end;

implementation

{ TAddCustomerCommandHandler }

constructor TAddCustomerCommandHandler.Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
begin
  inherited Create(Mediator);
  FCustomerStore := CustomerStore;
end;

procedure TAddCustomerCommandHandler.Handle(Command: TAddCustomerCommand);
begin
  var NextID := FCustomerStore.GetNextID;
  var Customer := TCustomer.Create(NextID, Command.CustomerName);
  FCustomerStore.Add(Customer);
  Mediator.Publish(TCustomerAddedEvent.Create(Customer));
end;

{ TRemoveCustomerCommandHandler }

constructor TRemoveCustomerCommandHandler.Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
begin
  inherited Create(Mediator);
  FCustomerStore := CustomerStore;
end;

procedure TRemoveCustomerCommandHandler.Handle(Command: TRemoveCustomerCommand);
begin
  var Customer := FCustomerStore.FindById(Command.CustomerId);
  if not Assigned(Customer) then
    Exit;
  var CustomerName := Customer.Name;
  FCustomerStore.Remove(Command.CustomerId);
  Mediator.Publish(TCustomerRemovedEvent.Create(Command.CustomerId, CustomerName));
end;

{ TCustomerQueryHandler }

constructor TCustomerQueryHandler.Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
begin
  inherited Create(Mediator);
  FCustomerStore := CustomerStore;
end;

procedure TCustomerQueryHandler.Handle(Request: TCustomerQuery; out Result: TCustomer.TList);
begin
  Result := FCustomerStore.FindAll(Request.Filter);
end;

end.
