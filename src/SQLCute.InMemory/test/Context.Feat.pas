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
  public
    constructor Create(const AId: Integer; const AName: string);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
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

constructor TPerson.Create(const AId: Integer; const AName: string);
begin
  inherited Create;
  FId := AId;
  FName := AName;
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
  People.Add(TPerson.Create(1, 'Ann'));
  People.Add(TPerson.Create(2, 'Bob'));
  People.Add(TPerson.Create(3, 'Cara'));   // no orders
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
      end);

end.
