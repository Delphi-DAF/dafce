unit MediatRSample.Requests;

interface

uses
  System.SysUtils,
  Daf.MediatR.Contracts,
  MediatRSample.Customer;

type
  TAddCustomerCommand = class(TRequest)
  private
    FCustomerName: string;
  public
    constructor Create(const ACustomerName: string);
    property CustomerName: string read FCustomerName;
  end;

  TCustomerQuery = class(TRequest<TCustomer.TList>)
  strict private
    FFilter: TPredicate<TCustomer>;
  public
    constructor Create(const Filter: TPredicate<TCustomer> = nil);
    property Filter: TPredicate<TCustomer> read FFilter;
  end;

  TCustomerAddedEvent = class(TNotification)
  private
    FCustomer: TCustomer;
  public
    constructor Create(const ACustomer: TCustomer);
    property Customer: TCustomer read FCustomer;
  end;

implementation

{ TAddCustomerCommand }

constructor TAddCustomerCommand.Create(const ACustomerName: string);
begin
  inherited Create;
  FCustomerName := ACustomerName;
end;

{ TCustomerQuery }

constructor TCustomerQuery.Create(const Filter: TPredicate<TCustomer> = nil);
begin
  inherited Create;
  if Assigned(Filter) then
    FFilter := Filter
  else
    FFilter := function(Customer: TCustomer): Boolean
    begin
      Result := True;
    end
end;

{ TCustomerAddedEvent }

constructor TCustomerAddedEvent.Create(const ACustomer: TCustomer);
begin
  inherited Create;
  FCustomer := ACustomer;
end;

end.
