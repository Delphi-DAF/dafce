unit SQLCute.Steps.StringOps;

{
  LIKE and string-match WHERE steps
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
  TSQLCuteStepsStringOps = class
  public
    [Given('a WhereLike case-insensitive query')]
    procedure GivenWhereLikeCaseInsensitive(W: TSQLCuteWorld);

    [Given('a WhereLike case-sensitive query')]
    procedure GivenWhereLikeCaseSensitive(W: TSQLCuteWorld);

    [Given('a WhereNotLike query')]
    procedure GivenWhereNotLike(W: TSQLCuteWorld);

    [Given('an OrWhereLike query')]
    procedure GivenOrWhereLike(W: TSQLCuteWorld);

    [Given('an OrWhereNotLike query')]
    procedure GivenOrWhereNotLike(W: TSQLCuteWorld);

    [Given('a WhereStarts case-insensitive query')]
    procedure GivenWhereStartsCaseInsensitive(W: TSQLCuteWorld);

    [Given('a WhereStarts case-sensitive query')]
    procedure GivenWhereStartsCaseSensitive(W: TSQLCuteWorld);

    [Given('a WhereEnds query')]
    procedure GivenWhereEnds(W: TSQLCuteWorld);

    [Given('a WhereContains query')]
    procedure GivenWhereContains(W: TSQLCuteWorld);

    [Given('a WhereNotContains query')]
    procedure GivenWhereNotContains(W: TSQLCuteWorld);

    [Given('an OrWhereStarts query')]
    procedure GivenOrWhereStarts(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsStringOps implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsStringOps.GivenWhereLikeCaseInsensitive(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereLike('name', '%alice%');
end;

procedure TSQLCuteStepsStringOps.GivenWhereLikeCaseSensitive(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereLike('name', '%Alice%', True);
end;

procedure TSQLCuteStepsStringOps.GivenWhereNotLike(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereNotLike('name', '%test%');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereLike(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereLike('name', '%admin%');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereNotLike(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('role', 'user').OrWhereNotLike('email', '%spam%');
end;

procedure TSQLCuteStepsStringOps.GivenWhereStartsCaseInsensitive(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereStarts('code', 'ABC');
end;

procedure TSQLCuteStepsStringOps.GivenWhereStartsCaseSensitive(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereStarts('code', 'ABC', True);
end;

procedure TSQLCuteStepsStringOps.GivenWhereEnds(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereEnds('email', '@corp.com');
end;

procedure TSQLCuteStepsStringOps.GivenWhereContains(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereContains('bio', 'engineer');
end;

procedure TSQLCuteStepsStringOps.GivenWhereNotContains(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereNotContains('title', 'spam');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereStarts(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereStarts('name', 'super');
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsStringOps>;

end.
