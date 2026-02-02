unit MediatRSample.Customer;

interface
uses
  System.SysUtils,
  System.Generics.Collections;

type
  TCustomerID = Integer;
  TCustomer = class
  public type
    TList = TObjectList<TCustomer>;
  public
    Id: TCustomerID;
    Name: string;
    constructor Create(const Id: TCustomerID; const Name: string);
  end;

  ICustomerStore = interface(IInvokable)
    ['{AF30CEB0-1D70-465D-BABD-38727B5F2F8D}']
    procedure Add(Customer: TCustomer);
    function GetNextID: TCustomerID;
    function FindAll(const Filter: TPredicate<TCustomer>): TCustomer.TList;
  end;

  TCustomerStore = class(TInterfacedObject, ICustomerStore)
  strict private
    FStorage: TCustomer.TList;
    FNextId: Integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Add(Customer: TCustomer);
    function GetNextID: TCustomerID;
    function FindAll(const Filter: TPredicate<TCustomer>): TCustomer.TList;
  end;

implementation

{ TCustomer }

constructor TCustomer.Create(const Id: Integer; const Name: string);
begin
  inherited Create;
  Self.Id := Id;
  Self.Name := Name;
end;

{ TCustomerStore }

constructor TCustomerStore.Create;
begin
  inherited Create;
  FStorage := TObjectList<TCustomer>.Create;
  FNextID := 1;
end;

destructor TCustomerStore.Destroy;
begin
  FStorage.Free;
  inherited;
end;

function TCustomerStore.FindAll(const Filter: TPredicate<TCustomer>): TCustomer.TList;
begin
  //Result no debe poseer en este caso los elementos: son de FStorage.
  Result := TCustomer.TList.Create(False);
  for var C in FStorage do
  begin
    if Filter(C) then
    Result.Add(C);
  end;
end;

function TCustomerStore.GetNextID: TCustomerID;
begin
  Result := FNextId;
  Inc(FNextId);
end;

procedure TCustomerStore.Add(Customer: TCustomer);
begin
  FStorage.Add(Customer);
end;

end.
