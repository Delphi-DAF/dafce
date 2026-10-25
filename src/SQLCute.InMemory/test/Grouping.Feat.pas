unit Grouping.Feat;

{
  GROUP BY in memory: what HAVING evaluates over a group, what a grouped query
  rejects, and the exactness of SUM.
}

interface

uses
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.InMemory;

type
  TGroupedPerson = class
  private
    FName: string;
    FCountry: string;
  public
    constructor Create(const AName, ACountry: string);
    property Name: string read FName write FName;
    property Country: string read FCountry write FCountry;
  end;

  TGroupedOrder = class
  private
    FUserId: Integer;
    FTotal: Integer;
  public
    constructor Create(const AUserId, ATotal: Integer);
    property UserId: Integer read FUserId write FUserId;
    property Total: Integer read FTotal write FTotal;
  end;

  TBigAmount = class
  private
    FKind: string;
    FAmount: Int64;
  public
    constructor Create(const AAmount: Int64);
    property Kind: string read FKind write FKind;
    property Amount: Int64 read FAmount write FAmount;
  end;

  TMoneyAmount = class
  private
    FKind: string;
    FAmount: Currency;
  public
    constructor Create(const AAmount: Currency);
    property Kind: string read FKind write FKind;
    property Amount: Currency read FAmount write FAmount;
  end;

  /// <summary>Materialization target whose Total is a Double.</summary>
  TTotalView = class
  private
    FUserId: Integer;
    FTotal: Double;
  public
    property UserId: Integer read FUserId write FUserId;
    property Total: Double read FTotal write FTotal;
  end;

  TGroupingWorld = class
  public
    People: TObjectList<TGroupedPerson>;
    Orders: TObjectList<TGroupedOrder>;
    Bigs: TObjectList<TBigAmount>;
    Monies: TObjectList<TMoneyAmount>;
    Ctx: TQueryContext;
    Query: IQuery;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory.Compiler,   // EInMemoryQueryError
  Daf.SQLCute.InMemory.Rows;       // IRow, TDictRow

{ TGroupedPerson }

constructor TGroupedPerson.Create(const AName, ACountry: string);
begin
  inherited Create;
  FName := AName;
  FCountry := ACountry;
end;

{ TGroupedOrder }

constructor TGroupedOrder.Create(const AUserId, ATotal: Integer);
begin
  inherited Create;
  FUserId := AUserId;
  FTotal := ATotal;
end;

{ TBigAmount }

constructor TBigAmount.Create(const AAmount: Int64);
begin
  inherited Create;
  FKind := 'k';
  FAmount := AAmount;
end;

{ TMoneyAmount }

constructor TMoneyAmount.Create(const AAmount: Currency);
begin
  inherited Create;
  FKind := 'k';
  FAmount := AAmount;
end;

{ TGroupingWorld }

constructor TGroupingWorld.Create;
begin
  inherited;
  Ctx := TQueryContext.Create;
end;

destructor TGroupingWorld.Destroy;
begin
  Ctx.Free;
  Monies.Free;
  Bigs.Free;
  Orders.Free;
  People.Free;
  inherited;
end;

procedure MapPeople(W: TGroupingWorld);
begin
  W.People := TObjectList<TGroupedPerson>.Create(True);
  W.People.Add(TGroupedPerson.Create('Ann', 'US'));
  W.People.Add(TGroupedPerson.Create('Bob', 'US'));
  W.People.Add(TGroupedPerson.Create('Cara', 'UK'));
  W.People.Add(TGroupedPerson.Create('Dan', 'UK'));
  W.People.Add(TGroupedPerson.Create('Eve', 'ES'));
  W.Ctx.Map<TGroupedPerson>('people', W.People);
end;

procedure MapOrders(W: TGroupingWorld);
begin
  W.Orders := TObjectList<TGroupedOrder>.Create(True);
  W.Orders.Add(TGroupedOrder.Create(1, 100));
  W.Orders.Add(TGroupedOrder.Create(1, 250));
  W.Orders.Add(TGroupedOrder.Create(2, 120));
  W.Orders.Add(TGroupedOrder.Create(3, 300));
  W.Ctx.Map<TGroupedOrder>('orders', W.Orders);
end;

// Runs the world's query over its context and writes, comma-separated, the
// given column of each row — or the exception it raised.
procedure Collect(W: TGroupingWorld; const Column: string);
begin
  W.Outcome := '';
  try
    for var R in TInMemory.Rows(W.Query, W.Ctx) do
    begin
      if W.Outcome <> '' then
        W.Outcome := W.Outcome + ',';
      W.Outcome := W.Outcome + VarToStr(R.Get(Column).AsVariant);
    end;
  except
    on E: Exception do
      W.Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

// The type and value of the single sum the world's query returns, or the
// exception it raised.
procedure CollectSum(W: TGroupingWorld);
begin
  W.Outcome := '';
  try
    for var R in TInMemory.Rows(W.Query, W.Ctx) do
    begin
      var V := R.Get('total');
      if V.TypeInfo = TypeInfo(Currency) then
        W.Outcome := 'Currency ' + CurrToStrF(V.AsCurrency, ffFixed, 4, TFormatSettings.Invariant)
      else if V.Kind in [tkInteger, tkInt64] then
        W.Outcome := 'integer ' + IntToStr(V.AsInt64)
      else
        W.Outcome := string(V.TypeInfo.Name) + ' ' + FloatToStr(V.AsExtended, TFormatSettings.Invariant);
    end;
  except
    on E: Exception do
      W.Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature SQLCute in-memory grouping @inmemory @grouping

  As a developer grouping rows in memory
  I want HAVING to evaluate what SQL evaluates, and to be told what it cannot
  So a grouped query either gives the rows a database gives or says why not
''')

.UseWorld<TGroupingWorld>

.Rule('HavingRaw with an aggregate is evaluated over the group')

  .Scenario('A raw aggregate filters groups')
    .Given('a context with people from three countries, two of them with more than one member',
      procedure(W: TGroupingWorld)
      begin
        MapPeople(W);
        W.Query := TQuery.New.From('people').Select('country').GroupBy('country')
          .HavingRaw('count(*) > 1').OrderBy('country');
      end)
    .When('the query with HavingRaw(''count(*) > 1'') runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'country');
      end)
    .&Then('exactly the two countries with more than one member come out',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToEqual('UK,US');
      end)

  .Scenario('An aggregate the query does not select')
    .Given('a context with orders from several customers',
      procedure(W: TGroupingWorld)
      begin
        MapOrders(W);
        W.Query := TQuery.New.From('orders').Select('user_id').GroupBy('user_id')
          .HavingRaw('sum(total) >= 300').OrderBy('user_id');
      end)
    .When('the query with HavingRaw(''sum(total) >= 300'') runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'user_id');
      end)
    .&Then('the customers whose orders add up to 300 or more come out',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToEqual('1,3');
      end)

  .Scenario('Any other raw expression is rejected, naming it')
    .Given('a context with people',
      procedure(W: TGroupingWorld)
      begin
        MapPeople(W);
        W.Query := TQuery.New.From('people').Select('country').GroupBy('country')
          .HavingRaw('count(*) > 1 OR 1 = 1');
      end)
    .When('the query with HavingRaw(''count(*) > 1 OR 1 = 1'') runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'country');
      end)
    .&Then('it raises EInMemoryQueryError quoting the expression',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('count(*) > 1 OR 1 = 1');
      end)

  .Scenario('The output alias still works')
    .Given('a context with people, and the first query with SelectCount and Having on its alias',
      procedure(W: TGroupingWorld)
      begin
        MapPeople(W);
        W.Query := TQuery.New.From('people').Select('country').SelectCount('*', 'n')
          .GroupBy('country').Having('n', '>', 1).OrderBy('country');
      end)
    .When('the query with Having(''n'', ''>'', 1) runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'country');
      end)
    .&Then('the same two countries come out',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToEqual('UK,US');
      end)

  .Scenario('An aggregate in the column of Having does not resolve')
    .Given('a context with people, and the first query with Having on count(*)',
      procedure(W: TGroupingWorld)
      begin
        MapPeople(W);
        W.Query := TQuery.New.From('people').Select('country').GroupBy('country')
          .Having('count(*)', '>', 1);
      end)
    .When('the query with Having(''count(*)'', ''>'', 1) runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'country');
      end)
    .&Then('it raises EInMemoryQueryError naming count(*)',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('count(*)');
      end)

.Rule('What the engine cannot evaluate over a group is rejected')

  .Scenario('An expression over an aggregate is rejected')
    .Given('a context with orders, and a grouped query that selects SUM(total) * 2',
      procedure(W: TGroupingWorld)
      begin
        MapOrders(W);
        W.Query := TQuery.New.From('orders').Select('user_id').SelectRaw('SUM(total) * 2 AS doble')
          .GroupBy('user_id');
      end)
    .When('the query runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'doble');
      end)
    .&Then('it raises EInMemoryQueryError instead of returning SUM(total) as doble',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('SUM(total) * 2');
      end)

  .Scenario('A LIKE in HAVING is rejected')
    .Given('a context with people, and a grouped query with Having(country, like, U%)',
      procedure(W: TGroupingWorld)
      begin
        MapPeople(W);
        W.Query := TQuery.New.From('people').Select('country').GroupBy('country')
          .Having('country', 'like', 'U%');
      end)
    .When('the query runs in memory',
      procedure(W: TGroupingWorld)
      begin
        Collect(W, 'country');
      end)
    .&Then('it raises EInMemoryQueryError instead of evaluating it',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)

.Rule('SUM keeps the exactness of its column')

  .Scenario('A sum of integers above 2^53 is exact')
    .Given('an Int64 column with the values 9007199254740993 and 0',
      procedure(W: TGroupingWorld)
      begin
        W.Bigs := TObjectList<TBigAmount>.Create(True);
        W.Bigs.Add(TBigAmount.Create(9007199254740993));
        W.Bigs.Add(TBigAmount.Create(0));
        W.Ctx.Map<TBigAmount>('amounts', W.Bigs);
        W.Query := TQuery.New.From('amounts').Select('kind').SelectSum('amount', 'total').GroupBy('kind');
      end)
    .When('its SelectSum runs in memory, grouped into a single group', CollectSum)
    .&Then('the sum is the integer 9007199254740993',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToEqual('integer 9007199254740993');
      end)

  .Scenario('A sum of Currency does not lose the fourth decimal')
    .Given('a Currency column whose values, added as Double, lose the fourth decimal',
      procedure(W: TGroupingWorld)
      begin
        W.Monies := TObjectList<TMoneyAmount>.Create(True);
        W.Monies.Add(TMoneyAmount.Create(100000000000000.0001));
        W.Monies.Add(TMoneyAmount.Create(0.0002));
        W.Ctx.Map<TMoneyAmount>('amounts', W.Monies);
        W.Query := TQuery.New.From('amounts').Select('kind').SelectSum('amount', 'total').GroupBy('kind');
      end)
    .When('its SelectSum runs in memory, grouped into a single group', CollectSum)
    .&Then('the sum is a Currency equal to the exact sum',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToEqual('Currency 100000000000000.0003');
      end)

  .Scenario('An overflow is rejected')
    .Given('an Int64 column with two values whose sum passes High(Int64)',
      procedure(W: TGroupingWorld)
      begin
        W.Bigs := TObjectList<TBigAmount>.Create(True);
        W.Bigs.Add(TBigAmount.Create(High(Int64)));
        W.Bigs.Add(TBigAmount.Create(1));
        W.Ctx.Map<TBigAmount>('amounts', W.Bigs);
        W.Query := TQuery.New.From('amounts').Select('kind').SelectSum('amount', 'total').GroupBy('kind');
      end)
    .When('its SelectSum runs in memory', CollectSum)
    .&Then('it raises EInMemoryQueryError',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)

  .Scenario('A text in a sum is rejected')
    .Given('dynamic rows whose amount is 10 in one and ''abc'' in the other',
      procedure(W: TGroupingWorld)
      begin
        W.Ctx.Map('amounts', [
          TDictRow.Make(['kind', 'amount'], ['k', 10]),
          TDictRow.Make(['kind', 'amount'], ['k', 'abc'])]);
        W.Query := TQuery.New.From('amounts').Select('kind').SelectSum('amount', 'total').GroupBy('kind');
      end)
    .When('the SelectSum of amount runs in memory, grouped into a single group', CollectSum)
    .&Then('it raises EInMemoryQueryError instead of returning 10',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)

  .Scenario('An integer sum is materialized into a Double member')
    .Given('a context with orders, and a grouped sum of an Integer column',
      procedure(W: TGroupingWorld)
      begin
        MapOrders(W);
        W.Query := TQuery.New.From('orders').Select('user_id').SelectSum('total', 'total')
          .GroupBy('user_id').OrderBy('user_id');
      end)
    .When('it is projected into a class whose Total is a Double',
      procedure(W: TGroupingWorld)
      begin
        W.Outcome := '';
        for var V in TInMemory.Project<TTotalView>(W.Query, W.Ctx) do
          try
            if W.Outcome <> '' then
              W.Outcome := W.Outcome + ',';
            W.Outcome := W.Outcome + Format('%d=%s', [V.UserId, FloatToStr(V.Total, TFormatSettings.Invariant)]);
          finally
            V.Free;
          end;
      end)
    .&Then('each customer carries its total',
      procedure(W: TGroupingWorld)
      begin
        Expect(W.Outcome).ToEqual('1=350,2=120,3=300');
      end);

end.
