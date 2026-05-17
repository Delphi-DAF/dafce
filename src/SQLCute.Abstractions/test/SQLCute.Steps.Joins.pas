unit SQLCute.Steps.Joins;

{
  JOIN types and JOIN callback steps
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
  TSQLCuteStepsJoins = class
  public
    [Given('a query from "(\w+)" inner joined to "(\w+)" on "(.+)"')]
    procedure GivenInnerJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);

    [Given('a query from "(\w+)" joining "(\w+)" on columns "([\w.]+)" and "([\w.]+)"')]
    procedure GivenInnerJoinCols(W: TSQLCuteWorld; Table, JoinTable, Col1, Col2: string);

    [Given('a query from "(\w+)" left joined to "(\w+)" on "(.+)"')]
    procedure GivenLeftJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);

    [Given('a query with two chained JOINs')]
    procedure GivenTwoChainedJoins(W: TSQLCuteWorld);

    [Given('a combined JOIN GROUP HAVING ORDER LIMIT query')]
    procedure GivenCombinedJoinGroupQuery(W: TSQLCuteWorld);

    [Given('a RightJoin query')]
    procedure GivenRightJoin(W: TSQLCuteWorld);

    [Given('a CrossJoin query')]
    procedure GivenCrossJoin(W: TSQLCuteWorld);

    [Given('a FullOuterJoin query')]
    procedure GivenFullOuterJoin(W: TSQLCuteWorld);

    [Given('a Join with not-equal operator query')]
    procedure GivenJoinWithNotEqualOp(W: TSQLCuteWorld);

    [Given('a LeftJoin with >= operator query')]
    procedure GivenLeftJoinWithGteOp(W: TSQLCuteWorld);

    [Given('a JOIN callback query with two AND conditions')]
    procedure GivenJoinCallbackTwoAndConditions(W: TSQLCuteWorld);

    [Given('a LEFT JOIN callback query with OR conditions')]
    procedure GivenLeftJoinCallbackOrConditions(W: TSQLCuteWorld);

    [Given('a JOIN callback query with a nested group condition')]
    procedure GivenJoinCallbackNestedGroup(W: TSQLCuteWorld);

    [Given('a LEFT JOIN subquery callback query')]
    procedure GivenLeftJoinSubqueryCallback(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsJoins implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsJoins.GivenInnerJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);
begin
  W.Query := TQuery.New.From(Table).Join(JoinTable, Cond);
end;

procedure TSQLCuteStepsJoins.GivenInnerJoinCols(W: TSQLCuteWorld; Table, JoinTable, Col1, Col2: string);
begin
  W.Query := TQuery.New.From(Table).Join(JoinTable, Col1, Col2);
end;

procedure TSQLCuteStepsJoins.GivenLeftJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);
begin
  W.Query := TQuery.New.From(Table).LeftJoin(JoinTable, Cond);
end;

procedure TSQLCuteStepsJoins.GivenTwoChainedJoins(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users',    'orders.user_id',    'users.id')
    .LeftJoin('products', 'orders.product_id', 'products.id');
end;

procedure TSQLCuteStepsJoins.GivenCombinedJoinGroupQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users', 'orders.user_id', 'users.id')
    .Select(['user_id'])
    .SelectCount('*', 'total')
    .Where('status', 'active')
    .GroupBy('user_id')
    .Having('COUNT(*)', '>', 5)
    .OrderByDesc('total')
    .Limit(10);
end;

procedure TSQLCuteStepsJoins.GivenRightJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .RightJoin('posts', 'users.id', 'posts.user_id');
end;

procedure TSQLCuteStepsJoins.GivenCrossJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .CrossJoin('tags');
end;

procedure TSQLCuteStepsJoins.GivenFullOuterJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .FullOuterJoin('logs', 'users.id', 'logs.user_id');
end;

procedure TSQLCuteStepsJoins.GivenJoinWithNotEqualOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users', 'orders.user_id', 'users.id', '<>');
end;

procedure TSQLCuteStepsJoins.GivenLeftJoinWithGteOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .LeftJoin('tiers', 'orders.amount', 'tiers.min_amount', '>=');
end;

procedure TSQLCuteStepsJoins.GivenJoinCallbackTwoAndConditions(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users',
      function(Q: IQuery): IQuery
      begin
        Result := Q.WhereColumns('orders.user_id', '=', 'users.id')
                   .WhereColumns('orders.tenant', '=', 'users.tenant');
      end);
end;

procedure TSQLCuteStepsJoins.GivenLeftJoinCallbackOrConditions(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .LeftJoin('promos',
      function(Q: IQuery): IQuery
      begin
        Result := Q.WhereColumns('orders.promo_id', '=', 'promos.id')
                   .OrWhereColumns('orders.alt_promo', '=', 'promos.id');
      end);
end;

procedure TSQLCuteStepsJoins.GivenJoinCallbackNestedGroup(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('t')
    .Join('u',
      function(Q: IQuery): IQuery
      begin
        Result := Q.WhereColumns('t.id', '=', 'u.id')
                   .Where(function(Q2: IQuery): IQuery
                     begin
                       Result := Q2.Where('t.active', True).OrWhere('u.role', 'admin');
                     end);
      end);
end;

procedure TSQLCuteStepsJoins.GivenLeftJoinSubqueryCallback(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .LeftJoin(
      TQuery.New.From('users').Select('id').Select('name'),
      'u',
      function(Q: IQuery): IQuery
      begin
        Result := Q.WhereColumns('orders.user_id', '=', 'u.id');
      end);
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsJoins>;

end.
