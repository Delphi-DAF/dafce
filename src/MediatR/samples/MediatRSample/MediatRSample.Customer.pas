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
    procedure Remove(const Id: TCustomerID);
    function GetNextID: TCustomerID;
    function FindAll(const Filter: TPredicate<TCustomer>): TCustomer.TList;
    function FindById(const Id: TCustomerID): TCustomer;
  end;

  TCustomerStore = class(TInterfacedObject, ICustomerStore)
  strict private
    FStorage: TCustomer.TList;
    FNextId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Customer: TCustomer);
    procedure Remove(const Id: TCustomerID);
    function GetNextID: TCustomerID;
    function FindAll(const Filter: TPredicate<TCustomer>): TCustomer.TList;
    function FindById(const Id: TCustomerID): TCustomer;
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

procedure TCustomerStore.Add(Customer: TCustomer);
begin
  FStorage.Add(Customer);
end;

procedure TCustomerStore.Remove(const Id: TCustomerID);
begin
  for var I := FStorage.Count - 1 downto 0 do
    if FStorage[I].Id = Id then
    begin
      FStorage.Delete(I);
      Exit;
    end;
end;

function TCustomerStore.GetNextID: TCustomerID;
begin
  Result := FNextId;
  Inc(FNextId);
end;

function TCustomerStore.FindAll(const Filter: TPredicate<TCustomer>): TCustomer.TList;
begin
  // Result does not own the elements — they belong to FStorage.
  Result := TCustomer.TList.Create(False);
  for var C in FStorage do
    if Filter(C) then
      Result.Add(C);
end;

function TCustomerStore.FindById(const Id: TCustomerID): TCustomer;
begin
  Result := nil;
  for var C in FStorage do
    if C.Id = Id then
      Exit(C);
end;

end.
