unit Context.Feat;

{
  Phase 3A — the general multi-source lane. A per-call IQueryContext maps table
  names to sources; the compiler resolves FROM / JOIN against it and combines
  sides into TJoinRow composites (qualified names). Covers name mapping, INNER
  and LEFT joins on a column condition, and fail-fast on an unmapped name.
}

interface

uses
  System.Generics.Collections;

type
  TPerson = class
  private
    FId: Integer;
    FName: string;
    FCountry: string;
  public
    constructor Create(const AId: Integer; const AName, ACountry: string);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Country: string read FCountry write FCountry;
  end;

  TOrder = class
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

  TCtxWorld = class
  public
    People: TObjectList<TPerson>;
    Orders: TObjectList<TOrder>;
    Names: TArray<string>;
    Count: Integer;
    FirstPerson: string;
    FirstTotal: Integer;
    CaraTotalIsNull: Boolean;
    GroupCountry: string;
    GroupCount: Integer;
    Countries: TArray<string>;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Rows,
  Daf.SQLCute.InMemory.Compiler;

{ TPerson }

constructor TPerson.Create(const AId: Integer; const AName, ACountry: string);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCountry := ACountry;
end;

{ TOrder }

constructor TOrder.Create(const AId, AUserId, ATotal: Integer);
begin
  inherited Create;
  FId := AId;
  FUserId := AUserId;
  FTotal := ATotal;
end;

{ TCtxWorld }

constructor TCtxWorld.Create;
begin
  inherited;
  People := TObjectList<TPerson>.Create(True);
  People.Add(TPerson.Create(1, 'Ann', 'US'));
  People.Add(TPerson.Create(2, 'Bob', 'US'));
  People.Add(TPerson.Create(3, 'Cara', 'UK'));   // no orders
  Orders := TObjectList<TOrder>.Create(True);
  Orders.Add(TOrder.Create(10, 1, 100));   // Ann
  Orders.Add(TOrder.Create(11, 1, 200));   // Ann
  Orders.Add(TOrder.Create(12, 2, 50));    // Bob
end;

destructor TCtxWorld.Destroy;
begin
  Orders.Free;
  People.Free;
  inherited;
end;

function AsStr(const R: IRow; const Column: string): string;
begin
  var V := R.Get(Column);
  if V.IsEmpty then
    Exit('');
  Result := VarToStr(V.AsVariant);
end;

initialization

Feature('''
Feature SQLCute in-memory multi-source @inmemory @context

  As a developer with several already-loaded lists
  I want to run a joined SQLCute query over a per-call context
  So JOINs resolve named sources and expose every side by qualified name
''')

.UseWorld<TCtxWorld>

.Rule('A per-call context resolves FROM and JOIN names to mapped sources')

  .Scenario('An inner join matches on a column condition and exposes both sides')
    .When('people INNER JOIN orders ON people.id = orders.user_id runs over the context',
      procedure(W: TCtxWorld)
      begin
        var Ctx := TQueryContext.Create;
        try
          Ctx.Map<TPerson>('people', W.People).Map<TOrder>('orders', W.Orders);
          W.Names := nil;
          for var R in TInMemory.Rows(
            TQuery.New.From('people').Join('orders', 'people.id', 'orders.user_id'), Ctx) do
          begin
            W.Names := W.Names + [AsStr(R, 'people.name')];
            if Length(W.Names) = 1 then
            begin
              W.FirstPerson := AsStr(R, 'people.name');
              W.FirstTotal := R.Get('orders.total').AsInteger;
            end;
          end;
          W.Count := Length(W.Names);
        finally
          Ctx.Free;
        end;
      end)
    .&Then('there is one row per matching (person, order) pair, with both sides visible',
      procedure(W: TCtxWorld)
      begin
        Expect(W.Count).ToEqual(3);          // Ann×2, Bob×1; Cara has no orders
        Expect(W.FirstPerson).ToEqual('Ann');
        Expect(W.FirstTotal).ToEqual(100);
      end)

  .Scenario('A left join keeps unmatched left rows with the right side NULL')
    .When('the same join is a LEFT JOIN',
      procedure(W: TCtxWorld)
      begin
        var Ctx := TQueryContext.Create;
        try
          Ctx.Map<TPerson>('people', W.People).Map<TOrder>('orders', W.Orders);
          W.Count := 0;
          W.CaraTotalIsNull := False;
          for var R in TInMemory.Rows(
            TQuery.New.From('people').LeftJoin('orders', 'people.id', 'orders.user_id'), Ctx) do
          begin
            Inc(W.Count);
            if AsStr(R, 'people.name') = 'Cara' then
              W.CaraTotalIsNull := R.Get('orders.total').IsEmpty;
          end;
        finally
          Ctx.Free;
        end;
      end)
    .&Then('Cara still appears, with orders.* reported as NULL',
      procedure(W: TCtxWorld)
      begin
        Expect(W.Count).ToEqual(4);          // Ann×2, Bob×1, Cara×1 (NULL order)
        Expect(W.CaraTotalIsNull).ToBeTrue;
      end)

  .Scenario('A query referencing an unmapped source fails fast')
    .&Then('joining orders without mapping it raises a descriptive error',
      procedure(W: TCtxWorld)
      begin
        ExpectException(
          procedure
          begin
            var Ctx := TQueryContext.Create;
            try
              Ctx.Map<TPerson>('people', W.People);   // 'orders' intentionally unmapped
              for var R in TInMemory.Rows(
                TQuery.New.From('people').Join('orders', 'people.id', 'orders.user_id'), Ctx) do
                ;
            finally
              Ctx.Free;
            end;
          end).ToBe(EInMemoryQueryError);
      end)

.Rule('GROUP BY with aggregates and HAVING emits flat rows')

  .Scenario('Grouped counts filtered by HAVING')
    .When('people grouped by country selecting COUNT(*), HAVING count > 1',
      procedure(W: TCtxWorld)
      begin
        var Ctx := TQueryContext.Create;
        try
          Ctx.Map<TPerson>('people', W.People);
          W.Count := 0;
          W.GroupCountry := '';
          W.GroupCount := 0;
          for var R in TInMemory.Rows(
            TQuery.New.From('people').Select('country').SelectCount('*', 'count')
              .GroupBy('country').Having('count', '>', 1), Ctx) do
          begin
            Inc(W.Count);
            W.GroupCountry := AsStr(R, 'country');
            W.GroupCount := R.Get('count').AsInteger;
          end;
        finally
          Ctx.Free;
        end;
      end)
    .&Then('only the US group (2 members) survives, as a flat country+count row',
      procedure(W: TCtxWorld)
      begin
        Expect(W.Count).ToEqual(1);          // US has 2, UK has 1 -> filtered out
        Expect(W.GroupCountry).ToEqual('US');
        Expect(W.GroupCount).ToEqual(2);
      end)

.Rule('Set operations combine result sets')

  .Scenario('UNION returns the distinct union of both result sets')
    .When('the country projection is UNIONed with itself',
      procedure(W: TCtxWorld)
      begin
        var Ctx := TQueryContext.Create;
        try
          Ctx.Map<TPerson>('people', W.People);
          W.Countries := nil;
          for var R in TInMemory.Rows(
            TQuery.New.From('people').Select('country')
              .Union(TQuery.New.From('people').Select('country')), Ctx) do
            W.Countries := W.Countries + [AsStr(R, 'country')];
        finally
          Ctx.Free;
        end;
      end)
    .&Then('duplicate rows collapse: the distinct countries US and UK',
      procedure(W: TCtxWorld)
      begin
        Expect(Length(W.Countries)).ToEqual(2);   // US×3, UK×1 across both sides -> {US, UK}
        var HasUS := False;
        var HasUK := False;
        for var C in W.Countries do
        begin
          if C = 'US' then HasUS := True;
          if C = 'UK' then HasUK := True;
        end;
        Expect(HasUS).ToBeTrue;
        Expect(HasUK).ToBeTrue;
      end)

.Rule('Subqueries filter against context sources')

  .Scenario('WHERE IN (subquery) keeps only elements matched by the subquery')
    .When('people whose id is IN (SELECT user_id FROM orders) are selected',
      procedure(W: TCtxWorld)
      begin
        var Ctx := TQueryContext.Create;
        try
          Ctx.Map<TPerson>('people', W.People).Map<TOrder>('orders', W.Orders);
          W.Names := nil;
          for var R in TInMemory.Rows(
            TQuery.New.From('people').WhereInQuery('id',
              TQuery.New.From('orders').Select('user_id')), Ctx) do
            W.Names := W.Names + [AsStr(R, 'name')];
          W.Count := Length(W.Names);
        finally
          Ctx.Free;
        end;
      end)
    .&Then('only people with at least one order remain (Ann, Bob — not Cara)',
      procedure(W: TCtxWorld)
      begin
        Expect(W.Count).ToEqual(2);
        var HasCara := False;
        for var N in W.Names do
          if N = 'Cara' then HasCara := True;
        Expect(HasCara).ToBeFalse;
      end);

end.
