unit Projection.Feat;

{
  Row model + typed projection. Exercises IRow / TRttiRow / TDictRow
  and qualified-name resolution; the SELECT projection lanes (preserve T vs
  IEnumerable<IRow> vs materialize into a result class); and NULL -> TNullable
  materialization (three-valued NULL surfacing in the output).
}

interface

uses
  System.Generics.Collections,
  Daf.Nullable,
  Daf.SQLCute;

type
  TPerson = class
  private
    FName: string;
    FAge: Variant;
    FCountry: string;
  public
    constructor Create(const AName: string; const AAge: Variant; const ACountry: string);
    property Name: string read FName write FName;
    property Age: Variant read FAge write FAge;
    property Country: string read FCountry write FCountry;
  end;

  /// <summary>Materialization target: Age is TNullable to distinguish 0 from NULL.</summary>
  TPersonView = class
  private
    FName: string;
    FAge: TNullable<Integer>;
  public
    property Name: string read FName write FName;
    property Age: TNullable<Integer> read FAge write FAge;
  end;

  /// <summary>Materialization target for the alias scenario.</summary>
  TAliasView = class
  private
    FDisplayName: string;
  public
    property DisplayName: string read FDisplayName write FDisplayName;
  end;

  /// <summary>Materialization target with PUBLIC FIELDS (no properties) — exercises the field SetValue helper.</summary>
  TFieldView = class
  public
    Name: string;
    Age: TNullable<Integer>;
  end;

  TProjWorld = class
  public
    People: TObjectList<TPerson>;
    Views: TObjectList<TPersonView>;
    Aliases: TObjectList<TAliasView>;
    FieldViews: TObjectList<TFieldView>;
    Names: TArray<string>;
    Count: Integer;
    Flag1, Flag2: Boolean;
    Str1: string;
    /// The projecting query the preserve-T lane refuses, and what it answered.
    Query: IQuery;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Rows,
  Daf.SQLCute.InMemory.Compiler;

{ TPerson }

constructor TPerson.Create(const AName: string; const AAge: Variant; const ACountry: string);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
  FCountry := ACountry;
end;

{ TProjWorld }

constructor TProjWorld.Create;
begin
  inherited;
  People := TObjectList<TPerson>.Create(True);
  People.Add(TPerson.Create('Ann', 30,   'US'));
  People.Add(TPerson.Create('Bob', 17,   'US'));
  People.Add(TPerson.Create('Dan', Null, 'UK'));   // NULL age
  Views := TObjectList<TPersonView>.Create(True);
  Aliases := TObjectList<TAliasView>.Create(True);
  FieldViews := TObjectList<TFieldView>.Create(True);
end;

destructor TProjWorld.Destroy;
begin
  FieldViews.Free;
  Aliases.Free;
  Views.Free;
  People.Free;
  inherited;
end;

initialization

Feature('''
Feature SQLCute in-memory row model & projection @inmemory @projection

  As a developer
  I want dynamic IRow access and optional typed projection over a single source
  So a query can reshape rows or materialize a result class, NULL-aware
''')

.UseWorld<TProjWorld>

.Rule('IRow exposes columns as TValue over objects and bags')

  .Scenario('TRttiRow reads an object member and probes columns')
    .When('a TRttiRow wraps a person',
      procedure(W: TProjWorld)
      begin
        var Acc: IRttiAccessors<TPerson> := TRttiAccessors<TPerson>.Create;
        var Row: IRow := TRttiRow<TPerson>.Create(Acc, W.People[0]);   // Ann
        W.Str1 := Row.Get('name').AsString;
        W.Flag1 := Row.Has('age');
        W.Flag2 := Row.Has('missing');
      end)
    .&Then('Get returns the member value and Has reflects membership',
      procedure(W: TProjWorld)
      begin
        Expect(W.Str1).ToEqual('Ann');
        Expect(W.Flag1).ToBeTrue;
        Expect(W.Flag2).ToBeFalse;
      end)

  .Scenario('A qualified column name resolves to the bare member')
    .When('Get is called with a table-qualified name',
      procedure(W: TProjWorld)
      begin
        var Acc: IRttiAccessors<TPerson> := TRttiAccessors<TPerson>.Create;
        var Row: IRow := TRttiRow<TPerson>.Create(Acc, W.People[0]);
        W.Str1 := Row.Get('people.name').AsString;
      end)
    .&Then('the table prefix is stripped and the member resolves',
      procedure(W: TProjWorld)
      begin
        Expect(W.Str1).ToEqual('Ann');
      end)

  .Scenario('TDictRow resolves columns against a name->value map')
    .When('a bag row is built with age and name',
      procedure(W: TProjWorld)
      begin
        var Row := TDictRow.Make(['age', 'name'], [30, 'Bob']);
        W.Count := Row.Get('age').AsVariant;
        W.Str1 := Row.Get('name').AsString;
        W.Flag1 := Row.Has('missing');
      end)
    .&Then('present keys return values and absent keys report False',
      procedure(W: TProjWorld)
      begin
        Expect(W.Count).ToEqual(30);
        Expect(W.Str1).ToEqual('Bob');
        Expect(W.Flag1).ToBeFalse;
      end)

.Rule('SELECT projection picks the lane')

  .Scenario('Unprojected single-source query yields the original T')
    .When('Elements preserves T with no SELECT',
      procedure(W: TProjWorld)
      begin
        W.Names := nil;
        for var P in TInMemory.Elements<TPerson>(
          TQuery.New.From('people').Where('age', '>', 18), W.People) do
          W.Names := W.Names + [P.Name];
      end)
    .&Then('the elements are TPerson instances (Ann only)',
      procedure(W: TProjWorld)
      begin
        Expect(Length(W.Names)).ToEqual(1);
        Expect(W.Names[0]).ToEqual('Ann');
      end)

  .Scenario('Elements rejects a projecting query')
    .Given('a query that projects a pair of columns',
      procedure(W: TProjWorld)
      begin
        W.Query := TQuery.New.From('people').Select(['name', 'age']);
      end)
    .When('it is run on the preserve-T lane',
      procedure(W: TProjWorld)
      begin
        try
          TInMemory.Elements<TPerson>(W.Query, W.People);
          W.Outcome := '<the lane projected onto T>';
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('a SELECT projection on the preserve-T lane raises',
      procedure(W: TProjWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)

  .Scenario('Without a result class, projection yields IRow carrying the selected columns')
    .When('Rows runs with a name and age projection',
      procedure(W: TProjWorld)
      begin
        W.Count := 0;
        W.Flag1 := True;    // every row exposes name
        W.Flag2 := False;   // no row exposes country
        for var R in TInMemory.Rows<TPerson>(
          TQuery.New.From('people').Select(['name', 'age']), W.People) do
        begin
          Inc(W.Count);
          W.Flag1 := W.Flag1 and R.Has('name');
          W.Flag2 := W.Flag2 or R.Has('country');
        end;
      end)
    .&Then('rows expose the projected columns only',
      procedure(W: TProjWorld)
      begin
        Expect(W.Count).ToEqual(3);
        Expect(W.Flag1).ToBeTrue;    // name present
        Expect(W.Flag2).ToBeFalse;   // country projected away
      end)

.Rule('Materialization into a result class (RTTI, NULL-aware)')

  .Scenario('Projection into a result class populates its members')
    .When('Project into a result class runs with a name and age projection',
      procedure(W: TProjWorld)
      begin
        for var V in TInMemory.Project<TPerson, TPersonView>(
          TQuery.New.From('people').Select(['name', 'age']), W.People) do
          W.Views.Add(V);
      end)
    .&Then('each view carries Name, and Age is present or NULL per source',
      procedure(W: TProjWorld)
      begin
        Expect(W.Views.Count).ToEqual(3);
        // Ann (30)
        Expect(W.Views[0].Name).ToEqual('Ann');
        Expect(W.Views[0].Age.HasValue).ToBeTrue;
        Expect(W.Views[0].Age.Value).ToEqual(30);
        // Dan (NULL age) -> TNullable stays IsNull
        Expect(W.Views[2].Name).ToEqual('Dan');
        Expect(W.Views[2].Age.IsNull).ToBeTrue;
      end)

  .Scenario('An alias maps the column value onto the matching member')
    .When('Project with SelectAs(name -> DisplayName) runs',
      procedure(W: TProjWorld)
      begin
        for var V in TInMemory.Project<TPerson, TAliasView>(
          TQuery.New.From('people').SelectAs('name', 'DisplayName'), W.People) do
          W.Aliases.Add(V);
      end)
    .&Then('the aliased column populates DisplayName',
      procedure(W: TProjWorld)
      begin
        Expect(W.Aliases.Count).ToEqual(3);
        Expect(W.Aliases[0].DisplayName).ToEqual('Ann');
      end)

  .Scenario('Materialization into a class with public fields populates the fields')
    .When('Project into a field-based class runs with a name and age projection',
      procedure(W: TProjWorld)
      begin
        for var V in TInMemory.Project<TPerson, TFieldView>(
          TQuery.New.From('people').Select(['name', 'age']), W.People) do
          W.FieldViews.Add(V);
      end)
    .&Then('public fields are set via the field SetValue helper, NULL-aware',
      procedure(W: TProjWorld)
      begin
        Expect(W.FieldViews.Count).ToEqual(3);
        // Ann (30) — string field + TNullable field via the Implicit operator
        Expect(W.FieldViews[0].Name).ToEqual('Ann');
        Expect(W.FieldViews[0].Age.HasValue).ToBeTrue;
        Expect(W.FieldViews[0].Age.Value).ToEqual(30);
        // Dan (NULL age) — the field stays IsNull
        Expect(W.FieldViews[2].Name).ToEqual('Dan');
        Expect(W.FieldViews[2].Age.IsNull).ToBeTrue;
      end)

.Rule('A generic bag is a first-class source')

  .Scenario('Querying a list of IRow bags filters by column')
    .When('a list of IRow bags is filtered with age over 18',
      procedure(W: TProjWorld)
      begin
        var Bag := TList<IRow>.Create;
        try
          Bag.Add(TDictRow.Make(['age', 'name'], [30, 'X']));
          Bag.Add(TDictRow.Make(['age', 'name'], [17, 'Y']));
          Bag.Add(TDictRow.Make(['age', 'name'], [40, 'Z']));
          W.Count := 0;
          for var R in TInMemory.Rows<IRow>(
            TQuery.New.From('bag').Where('age', '>', 18), Bag) do
            Inc(W.Count);
        finally
          Bag.Free;
        end;
      end)
    .&Then('only the matching bags are returned',
      procedure(W: TProjWorld)
      begin
        Expect(W.Count).ToEqual(2);   // X(30), Z(40)
      end);

end.
