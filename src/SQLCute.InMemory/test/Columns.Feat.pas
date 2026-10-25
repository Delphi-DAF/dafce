unit Columns.Feat;

{
  How the in-memory engine resolves a column name: a name no side of the row
  carries is an error, a qualified name looks only at the side its prefix names,
  ORDER BY takes an output alias, and like between two columns reads its pattern
  from the right-hand one.
}

interface

uses
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.InMemory;

type
  TColPerson = class
  private
    FId: Integer;
    FName: string;
    FCountry: string;
    FAge: Integer;
  public
    constructor Create(const AId: Integer; const AName, ACountry: string; const AAge: Integer);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Country: string read FCountry write FCountry;
    property Age: Integer read FAge write FAge;
  end;

  /// <summary>An order: it has no Name, so a qualified orders.name resolves nowhere.</summary>
  TColOrder = class
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

  /// <summary>A name and the LIKE pattern it is matched against.</summary>
  TPatternRow = class
  private
    FName: string;
    FPattern: string;
  public
    constructor Create(const AName, APattern: string);
    property Name: string read FName write FName;
    property Pattern: string read FPattern write FPattern;
  end;

  TColumnsWorld = class
  public
    People: TObjectList<TColPerson>;
    Orders: TObjectList<TColOrder>;
    Patterns: TObjectList<TPatternRow>;
    Ctx: TQueryContext;
    Query: IQuery;
    Outcome: string;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory.Compiler,   // EInMemoryQueryError
  Daf.SQLCute.InMemory.Rows;       // IRow

{ TColPerson }

constructor TColPerson.Create(const AId: Integer; const AName, ACountry: string; const AAge: Integer);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCountry := ACountry;
  FAge := AAge;
end;

{ TColOrder }

constructor TColOrder.Create(const AId, AUserId, ATotal: Integer);
begin
  inherited Create;
  FId := AId;
  FUserId := AUserId;
  FTotal := ATotal;
end;

{ TPatternRow }

constructor TPatternRow.Create(const AName, APattern: string);
begin
  inherited Create;
  FName := AName;
  FPattern := APattern;
end;

{ TColumnsWorld }

destructor TColumnsWorld.Destroy;
begin
  Ctx.Free;
  Patterns.Free;
  Orders.Free;
  People.Free;
  inherited;
end;

procedure MapPeopleAndOrders(W: TColumnsWorld);
begin
  W.People := TObjectList<TColPerson>.Create(True);
  W.People.Add(TColPerson.Create(1, 'Ann', 'US', 30));
  W.People.Add(TColPerson.Create(2, 'Bob', 'US', 17));
  W.People.Add(TColPerson.Create(3, 'Dan', 'UK', 40));   // no orders
  W.Orders := TObjectList<TColOrder>.Create(True);
  W.Orders.Add(TColOrder.Create(10, 1, 100));
  W.Orders.Add(TColOrder.Create(11, 2, 50));
  W.Ctx := TQueryContext.Create;
  W.Ctx.Map<TColPerson>('people', W.People).Map<TColOrder>('orders', W.Orders);
end;

// Runs the world's query over its context, reading every column each row
// carries, and writes the outcome as text.
procedure RunOverContext(W: TColumnsWorld);
begin
  try
    var N := 0;
    for var R in TInMemory.Rows(W.Query, W.Ctx) do
      Inc(N);
    W.Outcome := Format('rows=%d', [N]);
  except
    on E: Exception do
      W.Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function ValueText(const V: TValue): string;
begin
  if V.IsEmpty then
    Result := 'NULL'
  else
    Result := VarToStr(V.AsVariant);
end;

initialization

Feature('''
Feature SQLCute in-memory column resolution @inmemory @columns

  As a developer running a query in memory
  I want a column the row does not carry to fail with its name
  So a typo or a wrong prefix is not read as NULL and filtered away in silence
''')

.UseWorld<TColumnsWorld>

.Rule('A column that does not resolve is an error')

  .Scenario('A typo in WHERE is rejected')
    .Given('a context with people',
      procedure(W: TColumnsWorld)
      begin
        MapPeopleAndOrders(W);
        W.Query := TQuery.New.From('people').Where('contry', 'US');
      end)
    .When('From(people).Where(contry, US) runs in memory', RunOverContext)
    .&Then('it raises EInMemoryQueryError naming contry',
      procedure(W: TColumnsWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('contry');
      end)

  .Scenario('A prefix does not take the column of another table')
    .Given('a context with people, which has name, and orders, which has not',
      procedure(W: TColumnsWorld)
      begin
        MapPeopleAndOrders(W);
        W.Query := TQuery.New.From('people').Join('orders', 'people.id', 'orders.user_id')
          .Where('orders.name', 'Ann');
      end)
    .When('the join filters on orders.name in memory', RunOverContext)
    .&Then('it raises EInMemoryQueryError instead of reading people.name',
      procedure(W: TColumnsWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('orders.name');
      end)

  .Scenario('A correlated subquery is rejected')
    .Given('a context with people and orders',
      procedure(W: TColumnsWorld)
      begin
        MapPeopleAndOrders(W);
        W.Query := TQuery.New.From('people').WhereExists(
          TQuery.New.From('orders').WhereColumns('orders.user_id', '=', 'people.id'));
      end)
    .When('people with an EXISTS over orders that compares orders.user_id with people.id runs in memory',
      RunOverContext)
    .&Then('it raises EInMemoryQueryError instead of returning an empty list',
      procedure(W: TColumnsWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('people.id');
      end)

  .Scenario('The absent side of an outer join is still NULL')
    .Given('a context with a person who has no orders',
      procedure(W: TColumnsWorld)
      begin
        MapPeopleAndOrders(W);
      end)
    .When('people LEFT JOIN orders runs in memory and total is read with no prefix',
      procedure(W: TColumnsWorld)
      begin
        W.Outcome := '';
        try
          for var R in TInMemory.Rows(
            TQuery.New.From('people').LeftJoin('orders', 'people.id', 'orders.user_id')
              .Select('name').Select('total'), W.Ctx) do
            if ValueText(R.Get('name')) = 'Dan' then
              W.Outcome := 'Dan total=' + ValueText(R.Get('total'));
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('that person comes out with total NULL, and nothing raises',
      procedure(W: TColumnsWorld)
      begin
        Expect(W.Outcome).ToEqual('Dan total=NULL');
      end)

.Rule('ORDER BY takes the alias of an output column')

  .Scenario('Ordering by an alias')
    .Given('a list of people with different ages',
      procedure(W: TColumnsWorld)
      begin
        MapPeopleAndOrders(W);
      end)
    .When('From(people).SelectAs(age, years).OrderByDesc(years) runs in memory',
      procedure(W: TColumnsWorld)
      begin
        W.Outcome := '';
        try
          for var R in TInMemory.Rows<TColPerson>(
            TQuery.New.From('people').SelectAs('age', 'years').OrderByDesc('years'), W.People) do
          begin
            if W.Outcome <> '' then
              W.Outcome := W.Outcome + ',';
            W.Outcome := W.Outcome + ValueText(R.Get('years'));
          end;
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the rows come out from the oldest to the youngest',
      procedure(W: TColumnsWorld)
      begin
        Expect(W.Outcome).ToEqual('40,30,17');
      end)

.Rule('Comparing two columns with like takes the right one as the pattern')

  .Scenario('The pattern comes from the other column')
    .Given('rows with name and pattern: (Ann, A%) and (Bob, X%)',
      procedure(W: TColumnsWorld)
      begin
        W.Patterns := TObjectList<TPatternRow>.Create(True);
        W.Patterns.Add(TPatternRow.Create('Ann', 'A%'));
        W.Patterns.Add(TPatternRow.Create('Bob', 'X%'));
      end)
    .When('From(rows).WhereColumns(name, like, pattern) runs in memory',
      procedure(W: TColumnsWorld)
      begin
        W.Outcome := '';
        for var P in TInMemory.Elements<TPatternRow>(
          TQuery.New.From('rows').WhereColumns('name', 'like', 'pattern'), W.Patterns) do
          W.Outcome := W.Outcome + P.Name;
      end)
    .&Then('it selects only the row of Ann',
      procedure(W: TColumnsWorld)
      begin
        Expect(W.Outcome).ToEqual('Ann');
      end);

end.
