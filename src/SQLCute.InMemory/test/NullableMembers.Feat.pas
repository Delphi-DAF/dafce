unit NullableMembers.Feat;

{
  A TNullable member of an element, read in memory: NULL when absent and its
  value when present, in WHERE, ORDER BY and the aggregates.
}

interface

uses
  System.Generics.Collections,
  Daf.Nullable;

type
  /// <summary>A person whose middle name and score may be absent.</summary>
  TScoredPerson = class
  private
    FName: string;
    FTeam: string;
    FMiddleName: TNullable<string>;
    FScore: TNullable<Integer>;
  public
    constructor Create(const AName: string; const AMiddleName: TNullable<string>; const AScore: TNullable<Integer>);
    property Name: string read FName write FName;
    property Team: string read FTeam write FTeam;
    property MiddleName: TNullable<string> read FMiddleName write FMiddleName;
    property Score: TNullable<Integer> read FScore write FScore;
  end;

  TNullableMembersWorld = class
  public
    People: TObjectList<TScoredPerson>;
    Names: string;
    Sum: string;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.InMemory;

{ TScoredPerson }

constructor TScoredPerson.Create(const AName: string; const AMiddleName: TNullable<string>;
  const AScore: TNullable<Integer>);
begin
  inherited Create;
  FName := AName;
  FTeam := 'A';
  FMiddleName := AMiddleName;
  FScore := AScore;
end;

{ TNullableMembersWorld }

destructor TNullableMembersWorld.Destroy;
begin
  People.Free;
  inherited;
end;

function RunNames(W: TNullableMembersWorld; const Q: IQuery): string;
begin
  Result := '';
  for var P in TInMemory.Elements<TScoredPerson>(Q, W.People) do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + P.Name;
  end;
end;

initialization

Feature('''
Feature SQLCute in-memory TNullable members @inmemory @nullable

  As a developer whose elements carry TNullable members
  I want an absent member to read as NULL and a present one as its value
  So a query treats them as it treats a Variant holding Null
''')

.UseWorld<TNullableMembersWorld>

.Background
  .Given('people whose MiddleName is absent in Bob and Cara, and whose Score is 30, absent and 10',
    procedure(W: TNullableMembersWorld)
    begin
      W.People := TObjectList<TScoredPerson>.Create(True);
      W.People.Add(TScoredPerson.Create('Ann', TNullable<string>.From('Lee'), TNullable<Integer>.From(30)));
      W.People.Add(TScoredPerson.Create('Bob', TNullable<string>.Null, TNullable<Integer>.Null));
      W.People.Add(TScoredPerson.Create('Cara', TNullable<string>.Null, TNullable<Integer>.From(10)));
    end)

.Rule('A TNullable member is NULL or its value')

  .Scenario('WhereNull finds the absent ones')
    .When('From(people).WhereNull(MiddleName) runs in memory',
      procedure(W: TNullableMembersWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').WhereNull('MiddleName'));
      end)
    .&Then('it selects exactly Bob and Cara',
      procedure(W: TNullableMembersWorld)
      begin
        Expect(W.Names).ToEqual('Bob,Cara');
      end)

  .Scenario('Ordering by a TNullable orders by its value')
    .When('From(people).OrderBy(Score) runs in memory',
      procedure(W: TNullableMembersWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').OrderBy('Score'));
      end)
    .&Then('the order is the absent one, 10, 30',
      procedure(W: TNullableMembersWorld)
      begin
        Expect(W.Names).ToEqual('Bob,Cara,Ann');
      end)

  .Scenario('SUM adds the present ones')
    .When('the sum of Score runs in memory, grouped into a single group',
      procedure(W: TNullableMembersWorld)
      begin
        var Ctx := TQueryContext.Create;
        try
          Ctx.Map<TScoredPerson>('people', W.People);
          W.Sum := '';
          for var R in TInMemory.Rows(
            TQuery.New.From('people').Select('team').SelectSum('Score', 'total').GroupBy('team'), Ctx) do
            W.Sum := W.Sum + R.Get('total').ToString;
        finally
          Ctx.Free;
        end;
      end)
    .&Then('the sum is 40',
      procedure(W: TNullableMembersWorld)
      begin
        Expect(W.Sum).ToEqual('40');
      end);

end.
