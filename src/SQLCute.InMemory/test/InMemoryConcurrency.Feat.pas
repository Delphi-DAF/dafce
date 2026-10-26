unit InMemoryConcurrency.Feat;

{
  Two executions at once in memory: each resolves its subqueries against its
  own context, and gives the result it gives alone.
}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.InMemory;

type
  TRacePerson = class
  private
    FId: Integer;
    FName: string;
  public
    constructor Create(const AId: Integer; const AName: string);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
  end;

  TRaceRef = class
  private
    FUserId: Integer;
  public
    constructor Create(const AUserId: Integer);
    property UserId: Integer read FUserId write FUserId;
  end;

  TConcurrencyWorld = class
  private
    FFailures: TArray<string>;
    function RaceBody(const Idx: Integer; const Done: TCountdownEvent): TProc;
  public
    People: TObjectList<TRacePerson>;
    RefsA: TObjectList<TRaceRef>;
    RefsB: TObjectList<TRaceRef>;
    Contexts: TArray<TQueryContext>;
    Query: IQuery;
    Expected: TArray<string>;
    Report: string;
    destructor Destroy; override;
    class function Run(const Q: IQuery; const Ctx: TQueryContext): string; static;
    procedure Race;
  end;

implementation

uses
  System.Classes,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory.Rows;   // IRow

const
  Rounds = 300;

{ TRacePerson }

constructor TRacePerson.Create(const AId: Integer; const AName: string);
begin
  inherited Create;
  FId := AId;
  FName := AName;
end;

{ TRaceRef }

constructor TRaceRef.Create(const AUserId: Integer);
begin
  inherited Create;
  FUserId := AUserId;
end;

{ TConcurrencyWorld }

destructor TConcurrencyWorld.Destroy;
begin
  for var C in Contexts do
    C.Free;
  RefsB.Free;
  RefsA.Free;
  People.Free;
  inherited;
end;

class function TConcurrencyWorld.Run(const Q: IQuery; const Ctx: TQueryContext): string;
begin
  Result := '';
  for var R in TInMemory.Rows(Q, Ctx) do
    Result := Result + VarToStr(R.Get('name').AsVariant) + ' ';
end;

function TConcurrencyWorld.RaceBody(const Idx: Integer; const Done: TCountdownEvent): TProc;
begin
  // The index comes in as a parameter: a captured loop variable is shared by every thread.
  Result :=
    procedure
    begin
      try
        for var Round := 1 to Rounds do
        begin
          var Got := Run(Query, Contexts[Idx]);
          if (Got <> Expected[Idx]) and (FFailures[Idx] = '') then
            FFailures[Idx] := Format('thread %d: %s', [Idx, Got]);
        end;
      except
        on E: Exception do FFailures[Idx] := Format('thread %d: %s', [Idx, E.ClassName]);
      end;
      Done.Signal;
    end;
end;

procedure TConcurrencyWorld.Race;
begin
  SetLength(FFailures, Length(Contexts));
  var Done := TCountdownEvent.Create(Length(Contexts));
  try
    for var T := 0 to High(Contexts) do
      TThread.CreateAnonymousThread(RaceBody(T, Done)).Start;
    if Done.WaitFor(60000) <> wrSignaled then
      FFailures := FFailures + ['TIMEOUT'];
  finally
    Done.Free;
  end;
  Report := '';
  for var F in FFailures do
    if F <> '' then
      Report := Report + F + ' ';
  if Report = '' then
    Report := '<all match>';
end;

initialization

Feature('''
Feature SQLCute in-memory concurrent executions @inmemory @concurrency

  As an application running queries from several threads
  I want each execution to resolve its subqueries against its own context
  So two threads never see each other's data
''')

.UseWorld<TConcurrencyWorld>

.Rule('Two concurrent executions share no state')

  .Scenario('Two threads with different contexts')
    .Given('two contexts with different data, and the same query with WhereInQuery, run once over each',
      procedure(W: TConcurrencyWorld)
      begin
        W.People := TObjectList<TRacePerson>.Create(True);
        W.People.Add(TRacePerson.Create(1, 'Ann'));
        W.People.Add(TRacePerson.Create(2, 'Bob'));
        W.RefsA := TObjectList<TRaceRef>.Create(True);
        W.RefsA.Add(TRaceRef.Create(1));
        W.RefsB := TObjectList<TRaceRef>.Create(True);
        W.RefsB.Add(TRaceRef.Create(2));
        W.Contexts := [TQueryContext.Create, TQueryContext.Create];
        W.Contexts[0].Map<TRacePerson>('people', W.People).Map<TRaceRef>('refs', W.RefsA);
        W.Contexts[1].Map<TRacePerson>('people', W.People).Map<TRaceRef>('refs', W.RefsB);
        W.Query := TQuery.New.From('people').WhereInQuery('id', TQuery.New.From('refs').Select('user_id'));
        W.Expected := [TConcurrencyWorld.Run(W.Query, W.Contexts[0]),
                       TConcurrencyWorld.Run(W.Query, W.Contexts[1])];
      end)
    .When('two threads run it many times at once, each over its own context',
      procedure(W: TConcurrencyWorld)
      begin
        W.Race;
      end)
    .&Then('every execution gives the result of its own context',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(string.Join('|', W.Expected)).ToEqual('Ann |Bob ');
        Expect(W.Report).ToEqual('<all match>');
      end);

end.
