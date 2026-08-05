program InMemoryJoinSample;

{
  SQLCute In-Memory Sample — the general multi-source lane.

  Two already-loaded lists (customers and orders) are mapped into a per-call
  IQueryContext, and SQLCute queries that span both are executed against it:
  INNER and LEFT joins with qualified columns, GROUP BY with aggregates and
  HAVING, a WHERE IN subquery, and a set operation. Every query is also shown
  as the SQL the other back-end would send to a database.

  The companion sample InMemoryQuerySample covers the single-list fast lane.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  System.Generics.Collections,
  Daf.Comparers,
  Daf.SQLCute,
  Daf.SQLCute.Core,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Compiler,
  Daf.SQLCute.InMemory.Rows;

type
  TCustomer = class
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
    FCustomerId: Integer;
    FProduct: string;
    FTotal: Integer;
  public
    constructor Create(const AId, ACustomerId: Integer; const AProduct: string;
      const ATotal: Integer);
    property Id: Integer read FId write FId;
    /// <summary>Matches the SQL column <c>customer_id</c> (snake_case bridge).</summary>
    property CustomerId: Integer read FCustomerId write FCustomerId;
    property Product: string read FProduct write FProduct;
    property Total: Integer read FTotal write FTotal;
  end;

constructor TCustomer.Create(const AId: Integer; const AName, ACountry: string);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCountry := ACountry;
end;

constructor TOrder.Create(const AId, ACustomerId: Integer; const AProduct: string;
  const ATotal: Integer);
begin
  inherited Create;
  FId := AId;
  FCustomerId := ACustomerId;
  FProduct := AProduct;
  FTotal := ATotal;
end;

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

procedure Title(const Text: string);
begin
  Writeln;
  Writeln('=== ' + Text + ' ===');
end;

procedure ShowSql(const Query: IQuery);
begin
  var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
  var R := Query.Compile(Compiler);
  Writeln('  SQL: ' + R.SQL);
end;

function Cell(const Row: IRow; const Column: string): string;
begin
  var V := Row.Get(Column);
  if V.IsEmpty or IsNullValue(V) then
    Exit('NULL');
  Result := VarToStr(V.AsVariant);
end;

// Runs a query against the context and prints the requested columns per row.
procedure Run(const Query: IQuery; const Context: IQueryContext;
  const Columns: array of string);
begin
  ShowSql(Query);
  var Cols: TArray<string> := nil;
  for var C in Columns do
    Cols := Cols + [C];
  for var R in TInMemory.Rows(Query, Context) do
  begin
    var Line := '  ';
    for var C in Cols do
      Line := Line + Format('%s=%-10s', [C, Cell(R, C)]);
    Writeln(Line);
  end;
end;

// ---------------------------------------------------------------------------
//  Demos
// ---------------------------------------------------------------------------

procedure InnerJoin(const Ctx: IQueryContext);
begin
  Title('INNER JOIN — one row per matching (customer, order) pair');
  Run(TQuery.New.From('customers')
        .Join('orders', 'customers.id', 'orders.customer_id')
        .Select(['customers.name', 'orders.product', 'orders.total'])
        .OrderBy('customers.name').OrderBy('orders.total'),
      Ctx, ['customers.name', 'orders.product', 'orders.total']);
end;

procedure LeftJoin(const Ctx: IQueryContext);
begin
  Title('LEFT JOIN — customers with no orders survive, right side NULL');
  Run(TQuery.New.From('customers')
        .LeftJoin('orders', 'customers.id', 'orders.customer_id')
        .Select(['customers.name', 'orders.product'])
        .OrderBy('customers.name'),
      Ctx, ['customers.name', 'orders.product']);
end;

procedure GroupAndAggregate(const Ctx: IQueryContext);
begin
  Title('GROUP BY with aggregates and HAVING — flat rows, NULLs skipped');
  Run(TQuery.New.From('customers')
        .Select('country').SelectCount('*', 'customers')
        .GroupBy('country')
        .Having('customers', '>', 1)
        .OrderBy('country'),
      Ctx, ['country', 'customers']);

  Title('Aggregates over the joined rows');
  Run(TQuery.New.From('customers')
        .Join('orders', 'customers.id', 'orders.customer_id')
        .Select('customers.name')
        .SelectSum('orders.total', 'spent')
        .SelectMax('orders.total', 'biggest')
        .GroupBy('customers.name')
        .OrderBy('customers.name'),
      Ctx, ['customers.name', 'spent', 'biggest']);
end;

procedure Subquery(const Ctx: IQueryContext);
begin
  Title('WHERE IN (subquery) — customers that placed at least one order');
  Run(TQuery.New.From('customers')
        .WhereInQuery('id', TQuery.New.From('orders').Select('customer_id'))
        .Select('name')
        .OrderBy('name'),
      Ctx, ['name']);
end;

procedure SetOperation(const Ctx: IQueryContext);
begin
  Title('UNION — duplicate rows collapse across both result sets');
  Run(TQuery.New.From('customers').Select('country').Where('country', 'US')
        .Union(TQuery.New.From('customers').Select('country').Where('country', 'ES')),
      Ctx, ['country']);
end;

procedure UnmappedSource(const Ctx: IQueryContext);
begin
  Title('An unmapped source fails fast');
  try
    for var R in TInMemory.Rows(
        TQuery.New.From('customers').Join('invoices', 'customers.id', 'invoices.customer_id'),
        Ctx) do
      ;
    Writeln('  (unexpected: no error)');
  except
    on E: EInMemoryQueryError do
      Writeln('  EInMemoryQueryError: ' + E.Message);
  end;
end;

// ---------------------------------------------------------------------------

var
  Customers: TObjectList<TCustomer>;
  Orders: TObjectList<TOrder>;
  Context: TQueryContext;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Customers := TObjectList<TCustomer>.Create(True {owns});
    Orders := TObjectList<TOrder>.Create(True {owns});
    try
      Customers.Add(TCustomer.Create(1, 'Ann',  'US'));
      Customers.Add(TCustomer.Create(2, 'Bob',  'US'));
      Customers.Add(TCustomer.Create(3, 'Cara', 'UK'));   // no orders
      Customers.Add(TCustomer.Create(4, 'Dan',  'ES'));
      Orders.Add(TOrder.Create(10, 1, 'Keyboard', 120));
      Orders.Add(TOrder.Create(11, 1, 'Monitor',  340));
      Orders.Add(TOrder.Create(12, 2, 'Mouse',     45));
      Orders.Add(TOrder.Create(13, 4, 'Laptop',  1500));

      // The catalog is per call and explicit — never a global registry — and
      // the caller owns it.
      Context := TQueryContext.Create;
      try
        Context.Map<TCustomer>('customers', Customers)
               .Map<TOrder>('orders', Orders);

        InnerJoin(Context);
        LeftJoin(Context);
        GroupAndAggregate(Context);
        Subquery(Context);
        SetOperation(Context);
        UnmappedSource(Context);
      finally
        Context.Free;
      end;

      Writeln;
      Writeln('Done.');
    finally
      Orders.Free;
      Customers.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
