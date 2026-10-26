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

    [Given('a WhereNotStarts query')]
    procedure GivenWhereNotStarts(W: TSQLCuteWorld);

    [Given('an OrWhereNotStarts query')]
    procedure GivenOrWhereNotStarts(W: TSQLCuteWorld);

    [Given('a WhereNotEnds query')]
    procedure GivenWhereNotEnds(W: TSQLCuteWorld);

    [Given('an OrWhereEnds query')]
    procedure GivenOrWhereEnds(W: TSQLCuteWorld);

    [Given('an OrWhereNotEnds query')]
    procedure GivenOrWhereNotEnds(W: TSQLCuteWorld);

    [Given('an OrWhereContains query')]
    procedure GivenOrWhereContains(W: TSQLCuteWorld);

    [Given('an OrWhereNotContains query')]
    procedure GivenOrWhereNotContains(W: TSQLCuteWorld);

    [Given('a WhereContains query with "!" as escape character')]
    procedure GivenWhereContainsEscaped(W: TSQLCuteWorld);

    [When('a WhereLike is built with a quote as escape character')]
    procedure WhenWhereLikeQuoteEscape(W: TSQLCuteWorld);

    [ThenAttribute('the call raises (\w+)')]
    procedure ThenTheCallRaises(W: TSQLCuteWorld; ExpectedClass: string);

    [Given('a WhereLike query with a non-ASCII pattern')]
    procedure GivenWhereLikeNonAscii(W: TSQLCuteWorld);

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

procedure TSQLCuteStepsStringOps.GivenWhereNotStarts(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereNotStarts('code', 'TMP');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereNotStarts(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereNotStarts('code', 'TMP');
end;

procedure TSQLCuteStepsStringOps.GivenWhereNotEnds(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereNotEnds('file', '.BAK');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereEnds(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereEnds('file', '.PDF');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereNotEnds(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereNotEnds('file', '.BAK');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereContains(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereContains('bio', 'ENGINEER');
end;

procedure TSQLCuteStepsStringOps.GivenOrWhereNotContains(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').Where('active', True).OrWhereNotContains('title', 'SPAM');
end;

procedure TSQLCuteStepsStringOps.GivenWhereContainsEscaped(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereContains('code', '50!%', False, '!');
end;

procedure TSQLCuteStepsStringOps.WhenWhereLikeQuoteEscape(W: TSQLCuteWorld);
begin
  W.Outcome := 'no exception';
  try
    W.Query.From('t').WhereLike('code', 'a%', False, '''');
  except
    on E: Exception do W.Outcome := E.ClassName;
  end;
end;

procedure TSQLCuteStepsStringOps.ThenTheCallRaises(W: TSQLCuteWorld; ExpectedClass: string);
begin
  Expect(W.Outcome).ToEqual(ExpectedClass);
end;

procedure TSQLCuteStepsStringOps.GivenWhereLikeNonAscii(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('t').WhereLike('name', 'ÑANDÚ%');
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsStringOps>;

end.
