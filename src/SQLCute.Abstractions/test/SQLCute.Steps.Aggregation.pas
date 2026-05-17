unit SQLCute.Steps.Aggregation;

{
  GROUP BY, HAVING, COUNT and aggregate SELECT steps
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
  TSQLCuteStepsAggregation = class
  public
    [Given('a distinct query from "(\w+)"')]
    procedure GivenDistinctFrom(W: TSQLCuteWorld; Table: string);

    [Given('a query from "(\w+)" grouped by "(\w+)"')]
    procedure GivenGroupBy(W: TSQLCuteWorld; Table, Col: string);

    [Given('a GROUP BY and HAVING query')]
    procedure GivenGroupByAndHaving(W: TSQLCuteWorld);

    [Given('a count query from "(\w+)"')]
    procedure GivenCountFrom(W: TSQLCuteWorld; Table: string);

    [Given('a GroupByRaw query')]
    procedure GivenGroupByRaw(W: TSQLCuteWorld);

    [Given('a HavingRaw query')]
    procedure GivenHavingRaw(W: TSQLCuteWorld);

    [Given('a multi-column GroupBy query')]
    procedure GivenGroupByMultiColumn(W: TSQLCuteWorld);

    [Given('a SelectAs query')]
    procedure GivenSelectAs(W: TSQLCuteWorld);

    [Given('a SelectSum query')]
    procedure GivenSelectSum(W: TSQLCuteWorld);

    [Given('a SelectAvg query')]
    procedure GivenSelectAvg(W: TSQLCuteWorld);

    [Given('a SelectMin query')]
    procedure GivenSelectMin(W: TSQLCuteWorld);

    [Given('a SelectMax query')]
    procedure GivenSelectMax(W: TSQLCuteWorld);

    [Given('a From with alias query')]
    procedure GivenFromAlias(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsAggregation implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsAggregation.GivenDistinctFrom(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.Distinct.From(Table);
end;

procedure TSQLCuteStepsAggregation.GivenGroupBy(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).GroupBy(Col);
end;

procedure TSQLCuteStepsAggregation.GivenGroupByAndHaving(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .GroupBy('user_id')
    .Having('COUNT(*)', '>', 3);
end;

procedure TSQLCuteStepsAggregation.GivenCountFrom(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.From(Table).SelectCount;
end;

procedure TSQLCuteStepsAggregation.GivenGroupByRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('events')
    .Select('event')
    .GroupByRaw('DATE(created_at)');
end;

procedure TSQLCuteStepsAggregation.GivenHavingRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Select('dept_id')
    .GroupBy('dept_id')
    .HavingRaw('SUM(total) > 1000');
end;

procedure TSQLCuteStepsAggregation.GivenGroupByMultiColumn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('sales')
    .GroupBy(['year', 'month', 'dept_id']);
end;

procedure TSQLCuteStepsAggregation.GivenSelectAs(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').SelectAs('name', 'full_name');
end;

procedure TSQLCuteStepsAggregation.GivenSelectSum(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('orders').SelectSum('total');
end;

procedure TSQLCuteStepsAggregation.GivenSelectAvg(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('results').SelectAvg('score');
end;

procedure TSQLCuteStepsAggregation.GivenSelectMin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products').SelectMin('price');
end;

procedure TSQLCuteStepsAggregation.GivenSelectMax(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products').SelectMax('price');
end;

procedure TSQLCuteStepsAggregation.GivenFromAlias(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users', 'u').Select('u.id');
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsAggregation>;

end.
