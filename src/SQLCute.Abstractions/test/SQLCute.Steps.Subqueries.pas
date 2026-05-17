unit SQLCute.Steps.Subqueries;

{
  CTE, subquery, WHERE EXISTS and raw query steps
}

interface

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.SQLCute,
  SQLCute.SpecHelpers;

type
  TSQLCuteStepsSubqueries = class
  public
    [Given('a CTE query')]
    procedure GivenCTEQuery(W: TSQLCuteWorld);

    [Given('a FROM subquery query')]
    procedure GivenFromSubquery(W: TSQLCuteWorld);

    [Given('a WhereExists query')]
    procedure GivenWhereExists(W: TSQLCuteWorld);

    [Given('a WithRecursive query')]
    procedure GivenWithRecursive(W: TSQLCuteWorld);

    [Given('a SelectRaw query')]
    procedure GivenSelectRaw(W: TSQLCuteWorld);

    [Given('an OrderByRaw query')]
    procedure GivenOrderByRaw(W: TSQLCuteWorld);

    [Given('a SELECT subquery column query')]
    procedure GivenSelectSubqueryColumn(W: TSQLCuteWorld);

    [Given('a FromRaw with alias query')]
    procedure GivenFromRawAlias(W: TSQLCuteWorld);

    [Given('a FromRaw with bindings query')]
    procedure GivenFromRawWithBindings(W: TSQLCuteWorld);

    [Given('a FromRaw binding order query')]
    procedure GivenFromRawBindingOrder(W: TSQLCuteWorld);

    [Given('a WithRaw CTE query')]
    procedure GivenWithRawCte(W: TSQLCuteWorld);

    [Given('a WithRaw CTE with bindings query')]
    procedure GivenWithRawCteBindings(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsSubqueries implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsSubqueries.GivenCTEQuery(W: TSQLCuteWorld);
var
  CTEQuery: IQuery;
begin
  CTEQuery := TQuery.New.From('orders').Where('created', '>', '2024-01-01');
  W.Query := TQuery.New.&With('recent', CTEQuery).From('recent');
end;

procedure TSQLCuteStepsSubqueries.GivenFromSubquery(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('users').Select(['id', 'name']);
  W.Query := TQuery.New.From(Sub, 'u');
end;

procedure TSQLCuteStepsSubqueries.GivenWhereExists(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('orders').Where('user_id', 42);
  W.Query := TQuery.New.From('users').WhereExists(Sub);
end;

procedure TSQLCuteStepsSubqueries.GivenWithRecursive(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .WithRecursive('nums', TQuery.New.From('base').Select('n'))
    .From('nums')
    .Select('n');
end;

procedure TSQLCuteStepsSubqueries.GivenSelectRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Select('id')
    .SelectRaw('UPPER(name) AS uname');
end;

procedure TSQLCuteStepsSubqueries.GivenOrderByRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .OrderByRaw('FIELD(status, ''active'', ''pending'', ''closed'')');
end;

procedure TSQLCuteStepsSubqueries.GivenSelectSubqueryColumn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Select(TQuery.New.From('products').SelectMax('price'), 'max_price');
end;

procedure TSQLCuteStepsSubqueries.GivenFromRawAlias(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.FromRaw('generate_series(1,10)', [], 't');
end;

procedure TSQLCuteStepsSubqueries.GivenFromRawWithBindings(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.FromRaw('generate_series(?,?)', [1, 10], 't');
end;

procedure TSQLCuteStepsSubqueries.GivenFromRawBindingOrder(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .FromRaw('fn(?)', [42], 't')
    .Where('n', '>', 5);
end;

procedure TSQLCuteStepsSubqueries.GivenWithRawCte(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .WithRaw('cte', 'SELECT 1 AS n', [])
    .From('cte');
end;

procedure TSQLCuteStepsSubqueries.GivenWithRawCteBindings(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .WithRaw('cte', 'SELECT ? AS n', [99])
    .From('cte');
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsSubqueries>;

end.
