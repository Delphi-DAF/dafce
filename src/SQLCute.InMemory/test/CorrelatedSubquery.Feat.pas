unit CorrelatedSubquery.Feat;

{
  A subquery in the WHERE that names a column of the outer query. A qualified
  name whose prefix names no side of the subquery is read from the outer row,
  and only from the subquery's WHERE; an unqualified name resolves inside. A
  subquery that reads nothing from the outer row is evaluated once.
}

interface

uses
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Rows;

type
  /// <summary>Age is Variant so it can carry SQL NULL.</summary>
  TCorrPerson = class
  private
    FId: Integer;
    FName: string;
    FCountry: string;
    FAge: Variant;
  public
    constructor Create(const AId: Integer; const AName, ACountry: string; const AAge: Variant);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Country: string read FCountry write FCountry;
    property Age: Variant read FAge write FAge;
  end;

  TCorrOrder = class
  private
    FId: Integer;
    FUserId: Integer;
    FTotal: Integer;
  public
    constructor Create(const AId, AUserId, ATotal: Integer);
    property Id: Integer read FId write FId;
    property UserId: Integer read FUserId write FUserId;
    property Total: Integer read FTotal write FTotal;
  end;

  /// <summary>
  ///   A context that answers from another and counts the times it is asked for
  ///   orders. Not ref-counted, like the context it wraps: the World frees it.
  /// </summary>
  TCountingContext = class(TInterfacedObject, IQueryContext)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    FInner: TQueryContext;
  public
    OrdersResolved: Integer;
    constructor Create(const AInner: TQueryContext);
    function Resolve(const Name: string): TArray<IRow>;
    function Has(const Name: string): Boolean;
    function ColumnsOf(const Name: string; out Columns: TArray<string>): Boolean;
  end;

  TCorrWorld = class
  public
    People: TObjectList<TCorrPerson>;
    Orders: TObjectList<TCorrOrder>;
    Ctx: TQueryContext;
    Counter: TCountingContext;
    Query: IQuery;
    Outcome: string;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  Daf.MiniSpec;

{ TCorrPerson }

constructor TCorrPerson.Create(const AId: Integer; const AName, ACountry: string; const AAge: Variant);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCountry := ACountry;
  FAge := AAge;
end;

{ TCorrOrder }

constructor TCorrOrder.Create(const AId, AUserId, ATotal: Integer);
begin
  inherited Create;
  FId := AId;
  FUserId := AUserId;
  FTotal := ATotal;
end;

{ TCountingContext }

constructor TCountingContext.Create(const AInner: TQueryContext);
begin
  inherited Create;
  FInner := AInner;
end;

function TCountingContext._AddRef: Integer;
begin
  Result := -1;
end;

function TCountingContext._Release: Integer;
begin
  Result := -1;
end;

function TCountingContext.Resolve(const Name: string): TArray<IRow>;
begin
  if SameText(Name, 'orders') then
    Inc(OrdersResolved);
  Result := FInner.Resolve(Name);
end;

function TCountingContext.Has(const Name: string): Boolean;
begin
  Result := FInner.Has(Name);
end;

function TCountingContext.ColumnsOf(const Name: string; out Columns: TArray<string>): Boolean;
begin
  Result := FInner.ColumnsOf(Name, Columns);
end;

{ TCorrWorld }

destructor TCorrWorld.Destroy;
begin
  Counter.Free;
  Ctx.Free;
  Orders.Free;
  People.Free;
  inherited;
end;

// Seven people, two with a NULL age, and four orders: two of person 1, one of
// person 2 and one of person 5.
procedure MapSeed(W: TCorrWorld);
begin
  W.People := TObjectList<TCorrPerson>.Create(True);
  W.People.Add(TCorrPerson.Create(1, 'Ann',  'US', 30));
  W.People.Add(TCorrPerson.Create(2, 'Bob',  'US', 17));
  W.People.Add(TCorrPerson.Create(3, 'Cara', 'UK', 25));
  W.People.Add(TCorrPerson.Create(4, 'Dan',  'UK', Null));
  W.People.Add(TCorrPerson.Create(5, 'Eve',  'US', 40));
  W.People.Add(TCorrPerson.Create(6, 'Finn', 'UK', 25));
  W.People.Add(TCorrPerson.Create(7, 'Gil',  'ES', Null));
  W.Orders := TObjectList<TCorrOrder>.Create(True);
  W.Orders.Add(TCorrOrder.Create(10, 1, 100));
  W.Orders.Add(TCorrOrder.Create(11, 1, 200));
  W.Orders.Add(TCorrOrder.Create(12, 2, 50));
  W.Orders.Add(TCorrOrder.Create(13, 5, 300));
  W.Ctx := TQueryContext.Create;
  W.Ctx.Map<TCorrPerson>('people', W.People).Map<TCorrOrder>('orders', W.Orders);
end;

function IdsOf(const Query: IQuery; const Context: IQueryContext): string;
begin
  Result := '';
  for var R in TInMemory.Rows(Query, Context) do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + VarToStr(R.Get('people.id').AsVariant);
  end;
  Result := 'ids=' + Result;
end;

// Runs the world's query, over the counting context when there is one, and
// writes the ids of the people it returns, or what it raised, as text.
procedure RunOverContext(W: TCorrWorld);
begin
  try
    if W.Counter <> nil then
      W.Outcome := IdsOf(W.Query, W.Counter)
    else
      W.Outcome := IdsOf(W.Query, W.Ctx);
  except
    on E: Exception do
      W.Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function OrdersQuery: IQuery;
begin
  Result := TQuery.New.From('orders');
end;

initialization

Feature('''
Feature SQLCute in-memory correlated subqueries @inmemory @subquery

  As a developer running in memory the query I also send to the database
  I want a subquery of the WHERE to read the row of the outer query
  So EXISTS, NOT EXISTS, IN and NOT IN give in memory what an engine gives
''')

.UseWorld<TCorrWorld>

.Rule('A subquery of the WHERE sees the row of the outer query')

  .Scenario('Correlated EXISTS')
    .Given('a context with seven people, people.id 1 to 7, and four orders, of the people 1, 1, 2 and 5',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereExists(
          OrdersQuery.WhereColumns('orders.user_id', '=', 'people.id'));
      end)
    .When('people with an EXISTS over orders where orders.user_id = people.id runs in memory',
      RunOverContext)
    .&Then('the people 1, 2 and 5 come out',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToEqual('ids=1,2,5');
      end)

  .Scenario('Correlated NOT EXISTS')
    .Given('the same context',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereNotExists(
          OrdersQuery.WhereColumns('orders.user_id', '=', 'people.id'));
      end)
    .When('the same query with NOT EXISTS runs in memory', RunOverContext)
    .&Then('the people 3, 4, 6 and 7 come out',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToEqual('ids=3,4,6,7');
      end)

  .Scenario('Correlated IN')
    .Given('the same context, with orders of 100 and 200 of person 1, of 50 of person 2 and of 300 of person 5',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereInQuery('id',
          OrdersQuery.Select('user_id').WhereColumns('orders.user_id', '=', 'people.id')
            .Where('orders.total', '>', 150));
      end)
    .When('people whose id is in the user_id of their own orders over 150 runs in memory',
      RunOverContext)
    .&Then('the people 1 and 5 come out',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToEqual('ids=1,5');
      end)

  .Scenario('An outer column that is NULL does not match')
    .Given('the same context, with the age NULL in people 4 and 7 and under 100 in the rest',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereExists(
          OrdersQuery.WhereColumns('orders.total', '>', 'people.age'));
      end)
    .When('people with an EXISTS over orders where orders.total > people.age runs in memory',
      RunOverContext)
    .&Then('the people 1, 2, 3, 5 and 6 come out',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToEqual('ids=1,2,3,5,6');
      end)

.Rule('An unqualified name resolves inside the subquery')

  .Scenario('The inner id hides the outer one')
    .Given('a context with seven people, people.id 1 to 7, and orders of id 10 to 13',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereExists(OrdersQuery.Where('id', 10));
      end)
    .When('people with an EXISTS over orders where id = 10 runs in memory', RunOverContext)
    .&Then('the seven people come out',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToEqual('ids=1,2,3,4,5,6,7');
      end)

  .Scenario('Qualified, it is read from outside')
    .Given('the same context',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereExists(OrdersQuery.Where('people.id', 10));
      end)
    .When('people with an EXISTS over orders where people.id = 10 runs in memory', RunOverContext)
    .&Then('no person comes out',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToEqual('ids=');
      end)

  .Scenario('An outer column with no prefix is rejected')
    .Given('a context with people, which has age, and orders, which has not',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereExists(
          OrdersQuery.WhereColumns('orders.total', '>', 'age'));
      end)
    .When('people with an EXISTS over orders where orders.total > age runs in memory',
      RunOverContext)
    .&Then('it raises EInMemoryQueryError naming age',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('"age"');
      end)

.Rule('The outer row is seen only from the WHERE of the subquery')

  .Scenario('An outer column in the SELECT of the subquery')
    .Given('a context with people and orders',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Query := TQuery.New.From('people').WhereInQuery('id', OrdersQuery.Select('people.id'));
      end)
    .When('people whose id is in a subquery over orders that selects people.id runs in memory',
      RunOverContext)
    .&Then('it raises EInMemoryQueryError naming people.id',
      procedure(W: TCorrWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('"people.id"');
      end)

.Rule('A subquery without correlation is evaluated once')

  .Scenario('The IN of an uncorrelated subquery asks for its source once')
    .Given('a context with seven people and four orders that counts how many times it is asked for orders',
      procedure(W: TCorrWorld)
      begin
        MapSeed(W);
        W.Counter := TCountingContext.Create(W.Ctx);
        W.Query := TQuery.New.From('people').WhereInQuery('id', OrdersQuery.Select('user_id'));
      end)
    .When('people whose id is in the user_id of orders runs in memory', RunOverContext)
    .&Then('the people 1, 2 and 5 come out, and orders was asked for once',
      procedure(W: TCorrWorld)
      begin
        Expect(Format('%s orders=%d', [W.Outcome, W.Counter.OrdersResolved]))
          .ToEqual('ids=1,2,5 orders=1');
      end);

end.
