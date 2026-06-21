unit MediatRSample.Requests;

interface

uses
  System.SysUtils,
  Daf.MediatR.Contracts,
  MediatRSample.Customer;

type
  // --- Commands (mutate state) ---

  TAddCustomerCommand = class(TRequest)
  private
    FCustomerName: string;
  public
    constructor Create(const ACustomerName: string);
    property CustomerName: string read FCustomerName;
  end;

  TRemoveCustomerCommand = class(TRequest)
  private
    FCustomerId: TCustomerID;
  public
    constructor Create(const ACustomerId: TCustomerID);
    property CustomerId: TCustomerID read FCustomerId;
  end;

  // --- Queries (read state, return a value) ---

  TCustomerQuery = class(TRequest<TCustomer.TList>)
  strict private
    FFilter: TPredicate<TCustomer>;
  public
    constructor Create(const Filter: TPredicate<TCustomer> = nil);
    property Filter: TPredicate<TCustomer> read FFilter;
  end;

  // --- Notifications (broadcast events, no return) ---

  TCustomerAddedEvent = class(TNotification)
  private
    FCustomer: TCustomer;
  public
    constructor Create(const ACustomer: TCustomer);
    property Customer: TCustomer read FCustomer;
  end;

  TCustomerRemovedEvent = class(TNotification)
  private
    FCustomerId: TCustomerID;
    FCustomerName: string;
  public
    constructor Create(const ACustomerId: TCustomerID; const ACustomerName: string);
    property CustomerId: TCustomerID read FCustomerId;
    property CustomerName: string read FCustomerName;
  end;

implementation

{ TAddCustomerCommand }

constructor TAddCustomerCommand.Create(const ACustomerName: string);
begin
  inherited Create;
  FCustomerName := ACustomerName;
end;

{ TRemoveCustomerCommand }

constructor TRemoveCustomerCommand.Create(const ACustomerId: TCustomerID);
begin
  inherited Create;
  FCustomerId := ACustomerId;
end;

{ TCustomerQuery }

constructor TCustomerQuery.Create(const Filter: TPredicate<TCustomer> = nil);
begin
  inherited Create;
  if Assigned(Filter) then
    FFilter := Filter
  else
    FFilter := function(C: TCustomer): Boolean
    begin
      Result := True;
    end;
end;

{ TCustomerAddedEvent }

constructor TCustomerAddedEvent.Create(const ACustomer: TCustomer);
begin
  inherited Create;
  FCustomer := ACustomer;
end;

{ TCustomerRemovedEvent }

constructor TCustomerRemovedEvent.Create(const ACustomerId: TCustomerID; const ACustomerName: string);
begin
  inherited Create;
  FCustomerId := ACustomerId;
  FCustomerName := ACustomerName;
end;

end.
