unit SQLCute.Steps.WhereIn;

{
  WhereIn / WhereNotIn / OrWhereIn variant steps
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
  TSQLCuteStepsWhereIn = class
  public
    [Given('a WhereIn query')]
    procedure GivenWhereIn(W: TSQLCuteWorld);

    [Given('a WhereIn combined with WHERE')]
    procedure GivenWhereInAndWhere(W: TSQLCuteWorld);

    [Given('a WhereNotIn query')]
    procedure GivenWhereNotIn(W: TSQLCuteWorld);

    [Given('an OrWhereIn query')]
    procedure GivenOrWhereIn(W: TSQLCuteWorld);

    [Given('an OrWhereNotIn query')]
    procedure GivenOrWhereNotIn(W: TSQLCuteWorld);

    [Given('a combined WhereIn WhereNotIn and WHERE query')]
    procedure GivenCombinedWhereInQuery(W: TSQLCuteWorld);

    [Given('a WhereNotInQuery query')]
    procedure GivenWhereNotInQuery(W: TSQLCuteWorld);

    [Given('an OrWhereInQuery query')]
    procedure GivenOrWhereInQuery(W: TSQLCuteWorld);

    [Given('a WhereInQuery query')]
    procedure GivenWhereInQuery(W: TSQLCuteWorld);

    [Given('an OrWhereNotInQuery query')]
    procedure GivenOrWhereNotInQuery(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsWhereIn implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsWhereIn.GivenWhereIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .WhereIn('id', [1, 2, 3]);
end;

procedure TSQLCuteStepsWhereIn.GivenWhereInAndWhere(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .WhereIn('role', ['admin', 'editor'])
    .Where('active', True);
end;

procedure TSQLCuteStepsWhereIn.GivenWhereNotIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('products')
    .WhereNotIn('status', ['discontinued', 'archived']);
end;

procedure TSQLCuteStepsWhereIn.GivenOrWhereIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('active', True)
    .OrWhereIn('id', [10, 20]);
end;

procedure TSQLCuteStepsWhereIn.GivenOrWhereNotIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('active', True)
    .OrWhereNotIn('status', ['banned', 'deleted']);
end;

procedure TSQLCuteStepsWhereIn.GivenCombinedWhereInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Select(['id', 'name'])
    .WhereIn('status', ['pending', 'shipped'])
    .WhereNotIn('user_id', [99, 100])
    .Where('created_at', '>', '2024-01-01');
end;

procedure TSQLCuteStepsWhereIn.GivenWhereNotInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .WhereNotInQuery('id', TQuery.New.From('banned').Select('user_id'));
end;

procedure TSQLCuteStepsWhereIn.GivenOrWhereInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereInQuery('id', TQuery.New.From('vip').Select('id'));
end;

procedure TSQLCuteStepsWhereIn.GivenWhereInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .WhereInQuery('id', TQuery.New.From('vip').Select('id'));
end;

procedure TSQLCuteStepsWhereIn.GivenOrWhereNotInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNotInQuery('id', TQuery.New.From('banned').Select('user_id'));
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsWhereIn>;

end.
