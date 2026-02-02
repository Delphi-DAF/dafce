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
    procedure Handle(Command: TAddCustomerCommand);override;
    property CustomerStore: ICustomerStore read FCustomerStore;
  end;

  TCustomerQueryHandler = class(TResponseHandler<TCustomer.TList, TCustomerQuery>)
  private
    FCustomerStore: ICustomerStore;
  public
    constructor Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
    procedure Handle(Request: TCustomerQuery; out Result: TCustomer.TList); override;
    property CustomerStore: ICustomerStore read FCustomerStore;
  end;

implementation

{ TAddCustomerCommandHandler }

constructor TAddCustomerCommandHandler.Create(const Mediator: IMediator;  const CustomerStore: ICustomerStore);
begin
  inherited Create(Mediator);
  FCustomerStore := CustomerStore;
end;

procedure TAddCustomerCommandHandler.Handle(Command: TAddCustomerCommand);
begin
  var NextID := CustomerStore.GetNextID;
  var Customer := TCustomer.Create(NextID, Command.CustomerName);
  CustomerStore.Add(Customer);
  var Event := TCustomerAddedEvent.Create(Customer);
  Mediator.Publish(Event);
end;

{ TCustomerQueryHandler }

constructor TCustomerQueryHandler.Create(const Mediator: IMediator; const CustomerStore: ICustomerStore);
begin
  inherited Create(Mediator);
  FCustomerStore := CustomerStore;
end;

procedure TCustomerQueryHandler.Handle(Request: TCustomerQuery; out Result: TCustomer.TList);
begin
  Result := CustomerStore.FindAll(Request.Filter);
end;

end.