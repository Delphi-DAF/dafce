unit EmptyIn.Feat;

{
  The empty IN set, in the lane that actually executes it. `x IN (a, b)` is
  `x = a OR x = b`, so an empty list is the empty disjunction: False — and True
  for the negated form — without consulting the column at all.

  The discriminator in every scenario below is the row whose column is NULL. A
  rule that reads the column answers Unknown for that row and drops it from IN
  and from NOT IN alike.
  The group the builder substitutes — (col IS NULL AND col IS NOT NULL) — is made
  of the only two predicates in the model that never answer Unknown, so the NULL
  row is decided like any other.

  The parity scenario keeps both lanes in one place on purpose: this suite is the
  only one that compiles SQL and executes the same AST in memory, and the whole
  point of the substitution is that neither back-end was taught the rule apart.
}

interface

uses
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.InMemory;

type
  /// <summary>Seed entity; Price is Variant so one row can carry SQL NULL.</summary>
  TPart = class
  private
    FName: string;
    FPrice: Variant;
  public
    constructor Create(const AName: string; const APrice: Variant);
    property Name: string read FName write FName;
    property Price: Variant read FPrice write FPrice;
  end;

  /// <summary>A person whose Id is Variant so one of them can carry SQL NULL.</summary>
  TIdPerson = class
  private
    FName: string;
    FId: Variant;
  public
    constructor Create(const AName: string; const AId: Variant);
    property Name: string read FName write FName;
    property Id: Variant read FId write FId;
  end;

  TUserRef = class
  private
    FUserId: Integer;
  public
    constructor Create(const AUserId: Integer);
    property UserId: Integer read FUserId write FUserId;
  end;

  TEmptyInWorld = class
  public
    Parts: TObjectList<TPart>;
    People: TObjectList<TIdPerson>;
    Refs: TObjectList<TUserRef>;
    Ctx: TQueryContext;
    Empty: IQuery;
    Names: TArray<string>;
    OtherNames: TArray<string>;
    Sql: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute.Compiler;

{ TPart }

constructor TPart.Create(const AName: string; const APrice: Variant);
begin
  inherited Create;
  FName := AName;
  FPrice := APrice;
end;

{ TIdPerson }

constructor TIdPerson.Create(const AName: string; const AId: Variant);
begin
  inherited Create;
  FName := AName;
  FId := AId;
end;

{ TUserRef }

constructor TUserRef.Create(const AUserId: Integer);
begin
  inherited Create;
  FUserId := AUserId;
end;

{ TEmptyInWorld }

// Crane carries the NULL price. Without it every scenario here would pass under
// the old rule too, because a set that is empty already yields False for a
// non-NULL column.
constructor TEmptyInWorld.Create;
begin
  inherited;
  Parts := TObjectList<TPart>.Create(True {owns});
  Parts.Add(TPart.Create('Anvil', 120));
  Parts.Add(TPart.Create('Bolt', 5));
  Parts.Add(TPart.Create('Crane', Null));
  Parts.Add(TPart.Create('Drill', 900));
end;

destructor TEmptyInWorld.Destroy;
begin
  Ctx.Free;
  Refs.Free;
  People.Free;
  Parts.Free;
  inherited;
end;

// Cara carries the NULL id, and the subquery returns no rows.
procedure MapPeopleAndEmptySubquery(W: TEmptyInWorld);
begin
  W.People := TObjectList<TIdPerson>.Create(True);
  W.People.Add(TIdPerson.Create('Ann', 1));
  W.People.Add(TIdPerson.Create('Bob', 2));
  W.People.Add(TIdPerson.Create('Cara', Null));
  W.Refs := TObjectList<TUserRef>.Create(True);
  W.Refs.Add(TUserRef.Create(1));
  W.Ctx := TQueryContext.Create;
  W.Ctx.Map<TIdPerson>('people', W.People).Map<TUserRef>('refs', W.Refs);
  W.Empty := TQuery.New.From('refs').Select('user_id').Where('user_id', -1);
end;

function RunOverContext(W: TEmptyInWorld; const Q: IQuery): TArray<string>;
begin
  Result := nil;
  for var R in TInMemory.Rows(Q, W.Ctx) do
    Result := Result + [VarToStr(R.Get('name').AsVariant)];
end;

function RunNames(W: TEmptyInWorld; const Q: IQuery): TArray<string>;
begin
  Result := nil;
  for var P in TInMemory.Elements<TPart>(Q, W.Parts) do
    Result := Result + [P.Name];
end;

initialization

Feature('''
Feature The empty IN set @inmemory @emptyin @sqlcute

  As a caller whose value set is computed and can come out empty
  I want IN over nothing to match nothing and NOT IN over nothing to match everything
  So a degenerate set answers the same in both back-ends, NULL columns included
''')

.UseWorld<TEmptyInWorld>

.Rule('An empty value set is the empty disjunction')

  .Scenario('EmptyInMatchesNothing')
    .When('WhereIn(price, []) runs over the list',
      procedure(W: TEmptyInWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('parts').WhereIn('price', []));
      end)
    .&Then('no row is returned, the NULL-priced row excluded like any other',
      procedure(W: TEmptyInWorld)
      begin
        Expect(Length(W.Names)).ToEqual(0);
      end)

  .Scenario('EmptyNotInMatchesEverything')
    .When('WhereNotIn(price, []) runs over the list',
      procedure(W: TEmptyInWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('parts').WhereNotIn('price', []));
      end)
    .&Then('every row is returned, the NULL-priced row included like any other',
      procedure(W: TEmptyInWorld)
      begin
        Expect(Length(W.Names)).ToEqual(4);
        Expect(W.Names[2]).ToEqual('Crane');
      end)

  // The OR forms go through OrWhere / OrWhereNot, so what is under test is that
  // the group carries the connector the normalizer reads, not just its value.
  .Scenario('EmptyOrInAddsNothingToItsPredecessor')
    .When('Where(name = Anvil).OrWhereIn(price, []) runs',
      procedure(W: TEmptyInWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('parts')
          .Where('name', 'Anvil')
          .OrWhereIn('price', []));
      end)
    .&Then('only Anvil comes back: the OR arm is false for every row',
      procedure(W: TEmptyInWorld)
      begin
        Expect(Length(W.Names)).ToEqual(1);
        Expect(W.Names[0]).ToEqual('Anvil');
      end)

  .Scenario('EmptyOrNotInSatisfiesEveryRow')
    .When('Where(name = Anvil).OrWhereNotIn(price, []) runs',
      procedure(W: TEmptyInWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('parts')
          .Where('name', 'Anvil')
          .OrWhereNotIn('price', []));
      end)
    .&Then('every row comes back: the OR arm is true for every row',
      procedure(W: TEmptyInWorld)
      begin
        Expect(Length(W.Names)).ToEqual(4);
      end)

.Rule('Both back-ends derive the empty set from the same clauses')

  .Scenario('EmptyInIsAcceptedByTheInMemoryLane')
    .When('a query with an empty IN set is handed to the fast lane',
      procedure(W: TEmptyInWorld)
      begin
        // A raw fragment would be rejected outright here. Running at all is the
        // assertion; the count is what the empty disjunction says it must be.
        W.Names := RunNames(W, TQuery.New.From('parts')
          .WhereIn('price', [])
          .OrderBy('name'));
      end)
    .&Then('it executes rather than being rejected as a raw WHERE fragment',
      procedure(W: TEmptyInWorld)
      begin
        Expect(Length(W.Names)).ToEqual(0);
      end)

  .Scenario('BothLanesAgreeOnTheEmptySet')
    .When('one query with an empty IN set is compiled to SQL and run in memory',
      procedure(W: TEmptyInWorld)
      begin
        var Q := TQuery.New.From('parts').WhereIn('price', []);
        var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
        W.Sql := Compiler.Compile(Q).SQL;
        W.Names := RunNames(W, Q);
      end)
    .&Then('the in-memory lane returns no rows',
      procedure(W: TEmptyInWorld)
      begin
        Expect(Length(W.Names)).ToEqual(0);
      end)
    .&Then('and the SQL expresses the same always-false condition on the column',
      procedure(W: TEmptyInWorld)
      begin
        Expect(W.Sql).ToEqual('SELECT * FROM parts WHERE price IS NULL AND price IS NOT NULL');
      end)

.Rule('NULL against an empty subquery')

  .Scenario('NOT IN an empty subquery also selects the NULL')
    .Given('a context with people, one of them with a NULL id, and a subquery that returns no rows',
      MapPeopleAndEmptySubquery)
    .When('WhereNotInQuery(id, subquery) runs in memory over the people',
      procedure(W: TEmptyInWorld)
      begin
        W.Names := RunOverContext(W, TQuery.New.From('people').WhereNotInQuery('id', W.Empty));
      end)
    .&Then('it selects all of them, the one with the NULL id too',
      procedure(W: TEmptyInWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('Ann,Bob,Cara');
      end)

  .Scenario('Negating an IN of an empty subquery also selects the NULL')
    .Given('the same context and the same subquery', MapPeopleAndEmptySubquery)
    .When('WhereNot runs in memory over a group with WhereInQuery(id, subquery)',
      procedure(W: TEmptyInWorld)
      begin
        var Sub := W.Empty;
        W.Names := RunOverContext(W, TQuery.New.From('people').WhereNot(
          function(Q: IQuery): IQuery
          begin
            Result := Q.WhereInQuery('id', Sub);
          end));
      end)
    .&Then('it selects all of them, the one with the NULL id too',
      procedure(W: TEmptyInWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('Ann,Bob,Cara');
      end);

end.
