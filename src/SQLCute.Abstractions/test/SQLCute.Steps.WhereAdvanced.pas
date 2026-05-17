unit SQLCute.Steps.WhereAdvanced;

{
  Advanced WHERE: OR variants, groups, columns, conditional steps
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
  TSQLCuteStepsWhereAdvanced = class
  public
    [Given('a WhereNotExists query')]
    procedure GivenWhereNotExists(W: TSQLCuteWorld);

    [Given('an OrWhereExists query')]
    procedure GivenOrWhereExists(W: TSQLCuteWorld);

    [Given('an OrWhereNotExists query')]
    procedure GivenOrWhereNotExists(W: TSQLCuteWorld);

    [Given('an OrWhereNull query')]
    procedure GivenOrWhereNull(W: TSQLCuteWorld);

    [Given('an OrWhereNotNull query')]
    procedure GivenOrWhereNotNull(W: TSQLCuteWorld);

    [Given('a WhereTrue query')]
    procedure GivenWhereTrue(W: TSQLCuteWorld);

    [Given('a WhereFalse query')]
    procedure GivenWhereFalse(W: TSQLCuteWorld);

    [Given('a WhereNot query')]
    procedure GivenWhereNot(W: TSQLCuteWorld);

    [Given('an OrWhereNot query')]
    procedure GivenOrWhereNot(W: TSQLCuteWorld);

    [Given('a WhereNotBetween query')]
    procedure GivenWhereNotBetween(W: TSQLCuteWorld);

    [Given('an OrWhereBetween query')]
    procedure GivenOrWhereBetween(W: TSQLCuteWorld);

    [Given('an OrWhereNotBetween query')]
    procedure GivenOrWhereNotBetween(W: TSQLCuteWorld);

    [Given('a WhereNot with operator query')]
    procedure GivenWhereNotWithOp(W: TSQLCuteWorld);

    [Given('an OrWhereNot with operator query')]
    procedure GivenOrWhereNotWithOp(W: TSQLCuteWorld);

    [Given('a WHERE group query')]
    procedure GivenWhereGroup(W: TSQLCuteWorld);

    [Given('a WHERE group with outer AND condition')]
    procedure GivenWhereGroupWithOuter(W: TSQLCuteWorld);

    [Given('a WHERE group followed by outer condition')]
    procedure GivenWhereGroupFollowedByOuter(W: TSQLCuteWorld);

    [Given('an OR WHERE group query')]
    procedure GivenOrWhereGroup(W: TSQLCuteWorld);

    [Given('a WhereColumns equality query')]
    procedure GivenWhereColumnsEquality(W: TSQLCuteWorld);

    [Given('a WhereColumns with operator query')]
    procedure GivenWhereColumnsWithOp(W: TSQLCuteWorld);

    [Given('a WhereColumns combined with WHERE query')]
    procedure GivenWhereColumnsCombined(W: TSQLCuteWorld);

    [Given('a When true query')]
    procedure GivenWhenTrue(W: TSQLCuteWorld);

    [Given('a When false query')]
    procedure GivenWhenFalse(W: TSQLCuteWorld);

    [Given('a When with false callback query')]
    procedure GivenWhenWithFalseCallback(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsWhereAdvanced implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsWhereAdvanced.GivenWhereNotExists(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .WhereNotExists(TQuery.New.From('orders').Where('user_id', 99));
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereExists(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereExists(TQuery.New.From('orders').Where('user_id', 99));
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereNotExists(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNotExists(TQuery.New.From('orders').Where('user_id', 99));
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereNull(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Where('name', 'alice').OrWhereNull('deleted_at');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereNotNull(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Where('name', 'alice').OrWhereNotNull('email');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereTrue(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereTrue('active');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereFalse(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereFalse('active');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereNot(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereNot('status', 'banned');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereNot(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('status', 'active')
    .OrWhereNot('archived', True);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereNotBetween(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('orders').WhereNotBetween('total', 100, 500);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereBetween(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('age', '>', 60)
    .OrWhereBetween('score', 10, 20);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereNotBetween(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNotBetween('score', 1, 5);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereNotWithOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereNot('age', '>', 18);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereNotWithOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNot('score', '<', 10);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereGroup(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('age', '>', 18).Where('active', True);
      end);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereGroupWithOuter(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('orders')
    .Where('status', 'open')
    .Where(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('total', '>', 100).OrWhere('priority', 1);
      end);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereGroupFollowedByOuter(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('city', 'Madrid').OrWhere('city', 'Barcelona');
      end)
    .Where('age', '>', 18);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenOrWhereGroup(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('vip', True)
    .OrWhere(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('age', '>', 18).Where('active', True);
      end);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereColumnsEquality(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('employees')
    .WhereColumns('manager_id', '=', 'employee_id');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereColumnsWithOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products')
    .WhereColumns('price', '>', 'min_price');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhereColumnsCombined(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products')
    .Where('active', True)
    .WhereColumns('price', '>', 'min_price');
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhenTrue(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .When(True,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('active', True);
      end);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhenFalse(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .When(False,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('active', True);
      end);
end;

procedure TSQLCuteStepsWhereAdvanced.GivenWhenWithFalseCallback(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .When(False,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('active', True);
      end,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('archived', True);
      end);
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsWhereAdvanced>;

end.
