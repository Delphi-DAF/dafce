unit SQLCute.Steps.SetOps;

{
  UNION, INTERSECT, EXCEPT and pagination steps
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
  TSQLCuteStepsSetOps = class
  public
    [Given('a UNION query')]
    procedure GivenUnionQuery(W: TSQLCuteWorld);

    [Given('a UnionAll query')]
    procedure GivenUnionAll(W: TSQLCuteWorld);

    [Given('an Intersect query')]
    procedure GivenIntersect(W: TSQLCuteWorld);

    [Given('an Except query')]
    procedure GivenExcept(W: TSQLCuteWorld);

    [Given('an IntersectAll query')]
    procedure GivenIntersectAll(W: TSQLCuteWorld);

    [Given('an ExceptAll query')]
    procedure GivenExceptAll(W: TSQLCuteWorld);

    [Given('a CombineRaw query')]
    procedure GivenCombineRaw(W: TSQLCuteWorld);

    [Given('a ForPage query page (\d+) per-page (\d+)')]
    procedure GivenForPage(W: TSQLCuteWorld; Page, PerPage: Integer);

    [Given('a Take query')]
    procedure GivenTake(W: TSQLCuteWorld);

    [Given('a Skip query')]
    procedure GivenSkip(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsSetOps implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsSetOps.GivenUnionQuery(W: TSQLCuteWorld);
var
  Other: IQuery;
begin
  Other := TQuery.New.From('archived_users').Select('id');
  W.Query := TQuery.New.From('active_users').Select('id').Union(Other);
end;

procedure TSQLCuteStepsSetOps.GivenUnionAll(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a')
    .Select('id')
    .UnionAll(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteStepsSetOps.GivenIntersect(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a')
    .Select('id')
    .Intersect(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteStepsSetOps.GivenExcept(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a')
    .Select('id')
    .&Except(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteStepsSetOps.GivenIntersectAll(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a').Select('id')
    .IntersectAll(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteStepsSetOps.GivenExceptAll(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a').Select('id')
    .ExceptAll(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteStepsSetOps.GivenCombineRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a').Select('id')
    .CombineRaw('UNION SELECT id FROM b WHERE status = ''active''');
end;

procedure TSQLCuteStepsSetOps.GivenForPage(W: TSQLCuteWorld; Page, PerPage: Integer);
begin
  W.Query := TQuery.New.From('users').ForPage(Page, PerPage);
end;

procedure TSQLCuteStepsSetOps.GivenTake(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Take(5);
end;

procedure TSQLCuteStepsSetOps.GivenSkip(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Take(10).Skip(20);
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsSetOps>;

end.
