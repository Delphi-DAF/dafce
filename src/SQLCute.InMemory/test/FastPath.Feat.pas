unit FastPath.Feat;

{
  The single-source fast lane. A SQLCute IQuery is compiled onto a typed
  TList<T> via TInMemory.Elements<T>, preserving T end-to-end: no IRow, no catalog.
  Covers WHERE (comparisons, precedence, IS NULL, BETWEEN, IN/NOT IN, column-to-
  column, LIKE), three-valued NULL logic, ORDER BY / LIMIT / OFFSET / DISTINCT,
  plan reuse, and multi-source fail-fast.
}

interface

uses
  System.Generics.Collections,
  Daf.SQLCute;

type
  /// <summary>A person whose Age is Variant so it can carry SQL NULL in the fast lane.</summary>
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

  /// <summary>Two numeric columns for the column-to-column comparison scenario.</summary>
  TRow2 = class
  private
    FA: Integer;
    FB: Integer;
  public
    constructor Create(const AA, AB: Integer);
    property A: Integer read FA write FA;
    property B: Integer read FB write FB;
  end;

  /// <summary>A user with a logical column, for the WhereTrue scenario.</summary>
  TActiveUser = class
  private
    FName: string;
    FActive: Boolean;
  public
    constructor Create(const AName: string; const AActive: Boolean);
    property Name: string read FName write FName;
    property Active: Boolean read FActive write FActive;
  end;

  /// <summary>A code and a name, for the LIKE scenarios.</summary>
  TTagged = class
  private
    FCode: string;
    FName: string;
  public
    constructor Create(const ACode, AName: string);
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
  end;

  TFastPathWorld = class
  public
    People: TObjectList<TPerson>;
    Users: TObjectList<TActiveUser>;
    Items: TObjectList<TTagged>;
    Names: TArray<string>;
    Count: Integer;
    SecondCount: Integer;
    /// The query the fast lane cannot take, and what it answered when handed it.
    Query: IQuery;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Compiler;   // EInMemoryQueryError

{ TPerson }

constructor TPerson.Create(const AName: string; const AAge: Variant; const ACountry: string);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
  FCountry := ACountry;
end;

{ TRow2 }

constructor TTagged.Create(const ACode, AName: string);
begin
  inherited Create;
  FCode := ACode;
  FName := AName;
end;

constructor TActiveUser.Create(const AName: string; const AActive: Boolean);
begin
  inherited Create;
  FName := AName;
  FActive := AActive;
end;

constructor TRow2.Create(const AA, AB: Integer);
begin
  inherited Create;
  FA := AA;
  FB := AB;
end;

{ TFastPathWorld }

constructor TFastPathWorld.Create;
begin
  inherited;
  People := TObjectList<TPerson>.Create(True {owns});
  People.Add(TPerson.Create('Ann',  30,   'US'));
  People.Add(TPerson.Create('Bob',  17,   'US'));
  People.Add(TPerson.Create('Cara', 25,   'UK'));
  People.Add(TPerson.Create('Dan',  Null, 'UK'));   // NULL age
  People.Add(TPerson.Create('Eve',  40,   'US'));
  People.Add(TPerson.Create('Finn', 25,   'UK'));
end;

destructor TFastPathWorld.Destroy;
begin
  Items.Free;
  Users.Free;
  People.Free;
  inherited;
end;

// Collect the Name of every person the query yields over the world's list.
function RunNames(W: TFastPathWorld; const Q: IQuery): TArray<string>;
begin
  Result := nil;
  for var P in TInMemory.Elements<TPerson>(Q, W.People) do
    Result := Result + [P.Name];
end;

initialization

Feature('''
Feature SQLCute in-memory fast path @inmemory @fastpath

  As a developer with an already-loaded TList<T>
  I want to run a SQLCute query over it, preserving the element type
  So the same query filters a database or an in-memory collection (symmetry)
''')

.UseWorld<TFastPathWorld>

.Rule('WHERE conditions evaluate against the source element')

  .Scenario('Filtering a typed list preserves the element type')
    .When('From(people).Where(age > 18) runs over the list',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').Where('age', '>', 18));
        W.Count := Length(W.Names);
      end)
    .&Then('only persons older than 18 remain (NULL age excluded)',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(4);   // Ann, Cara, Eve, Finn — not Bob(17), not Dan(NULL)
      end)

  .Scenario('Flat mixed AND/OR uses SQL precedence (AND before OR)')
    .When('country=UK AND age=25 OR country=US AND age=40 runs, no explicit group',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people')
          .Where('country', 'UK').Where('age', 25)
          .OrWhere('country', 'US').Where('age', 40));
        W.Count := Length(W.Names);
      end)
    .&Then('it is (UK AND 25) OR (US AND 40): Cara, Finn, Eve',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(3);
      end)

  .Scenario('An explicit OrWhere group is honored as a unit')
    .When('country=US OR (country=UK AND age=25) runs with an explicit group',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people')
          .Where('country', 'US')
          .OrWhere(
            function(Q: IQuery): IQuery
            begin
              Result := Q.Where('country', 'UK').Where('age', 25);
            end));
        W.Count := Length(W.Names);
      end)
    .&Then('the group binds tighter than the OR: Ann, Bob, Eve, Cara, Finn',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(5);   // 3 US + Cara/Finn (UK, 25); Dan (UK, NULL) out
      end)

  .Scenario('IS NULL selects absent values')
    .When('WhereNull(age) runs',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').WhereNull('age'));
        W.Count := Length(W.Names);
      end)
    .&Then('only Dan (NULL age) is returned',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(1);
        Expect(W.Names[0]).ToEqual('Dan');
      end)

  .Scenario('NOT of an unknown comparison stays unknown (not true)')
    .When('WhereNot(age > 18) runs',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').WhereNot('age', '>', 18));
        W.Count := Length(W.Names);
      end)
    .&Then('Bob is kept but Dan (NULL) is excluded: NOT(unknown) = unknown',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(1);
        Expect(W.Names[0]).ToEqual('Bob');
      end)

  .Scenario('A negated group drops exactly the elements its conditions match')
    .When('WhereNot(country = US AND age > 18) runs over the list',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people')
          .WhereNot(
            function(Q: IQuery): IQuery
            begin
              Result := Q.Where('country', 'US').Where('age', '>', 18);
            end));
        W.Count := Length(W.Names);
      end)
    .&Then('Ann and Eve (US adults) are dropped, the other four kept',
      procedure(W: TFastPathWorld)
      begin
        // Dan is UK, so the group is No before his NULL age matters.
        Expect(W.Count).ToEqual(4);   // Bob, Cara, Dan, Finn
      end)

  // The 3VL discriminator: under two-valued logic Dan's group would be False
  // and NOT would keep him, giving 4. Unknown propagates through NOT, so 3.
  .Scenario('A negated group containing an unknown comparison stays unknown')
    .When('WhereNot(country = UK AND age > 18) runs — Dan is UK with a NULL age',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people')
          .WhereNot(
            function(Q: IQuery): IQuery
            begin
              Result := Q.Where('country', 'UK').Where('age', '>', 18);
            end));
        W.Count := Length(W.Names);
      end)
    .&Then('Dan is excluded: (Yes AND Unknown) = Unknown, and NOT Unknown = Unknown',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(3);
        Expect(W.Names[0]).ToEqual('Ann');
        Expect(W.Names[1]).ToEqual('Bob');
        Expect(W.Names[2]).ToEqual('Eve');
      end)

  .Scenario('BETWEEN keeps values in range and skips NULL')
    .When('WhereBetween(age, 25, 30) runs',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').WhereBetween('age', 25, 30));
        W.Count := Length(W.Names);
      end)
    .&Then('Ann(30), Cara(25) and Finn(25) remain',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(3);
      end)

  .Scenario('IN and NOT IN filter by a value set')
    .When('WhereIn(age, [17, 40]) then a separate NOT IN(country, [US]) run',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').WhereIn('age', [17, 40]));
        W.Count := Length(W.Names);                                   // Bob, Eve
        W.SecondCount := Length(RunNames(W, TQuery.New.From('people')
          .WhereNotIn('country', ['US'])));                           // Cara, Dan, Finn
      end)
    .&Then('IN yields 2 and NOT IN yields 3',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(2);
        Expect(W.SecondCount).ToEqual(3);
      end)

  .Scenario('Column-to-column comparison evaluates two members')
    .When('WhereColumns(a > b) runs over rows (5,3),(2,8),(7,7)',
      procedure(W: TFastPathWorld)
      begin
        var Rows := TObjectList<TRow2>.Create(True);
        try
          Rows.Add(TRow2.Create(5, 3));
          Rows.Add(TRow2.Create(2, 8));
          Rows.Add(TRow2.Create(7, 7));
          W.Count := 0;
          for var R in TInMemory.Elements<TRow2>(
            TQuery.New.From('rows').WhereColumns('a', '>', 'b'), Rows) do
            Inc(W.Count);
        finally
          Rows.Free;
        end;
      end)
    .&Then('only (5,3) satisfies a > b',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(1);
      end)

  .Scenario('LIKE contains honors case sensitivity')
    .When('WhereContains(name, "AN") runs case-insensitive then case-sensitive',
      procedure(W: TFastPathWorld)
      begin
        W.Count := Length(RunNames(W, TQuery.New.From('people')
          .WhereContains('name', 'AN', False)));   // Ann, Dan
        W.SecondCount := Length(RunNames(W, TQuery.New.From('people')
          .WhereContains('name', 'AN', True)));     // none (no uppercase "AN")
      end)
    .&Then('insensitive matches 2, sensitive matches 0',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(2);
        Expect(W.SecondCount).ToEqual(0);
      end)

.Rule('ORDER BY, LIMIT, OFFSET and DISTINCT apply while preserving T')

  .Scenario('OrderBy sorts ascending by a column')
    .When('OrderBy(name) runs',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').OrderBy('name'));
      end)
    .&Then('names come out ascending',
      procedure(W: TFastPathWorld)
      begin
        Expect(Length(W.Names)).ToEqual(6);
        Expect(W.Names[0]).ToEqual('Ann');
        Expect(W.Names[5]).ToEqual('Finn');
      end)

  .Scenario('NULLs sort first in ascending order by default')
    .When('OrderBy(age) runs ascending',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').OrderBy('age'));
      end)
    .&Then('Dan (NULL age) sorts before every non-NULL value',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Names[0]).ToEqual('Dan');
      end)

  .Scenario('An explicit NullsLast overrides the NULL-smallest default')
    .When('OrderBy(age).NullsLast runs ascending',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').OrderBy('age').NullsLast);
      end)
    .&Then('Dan (NULL age) trails every non-NULL value',
      procedure(W: TFastPathWorld)
      begin
        Expect(Length(W.Names)).ToEqual(6);
        Expect(W.Names[High(W.Names)]).ToEqual('Dan');
      end)

  .Scenario('An explicit NullsFirst on a descending sort brings NULLs to the front')
    .When('OrderByDesc(age).NullsFirst runs',
      procedure(W: TFastPathWorld)
      begin
        // Descending would place NULL last by default (NULL is the smallest
        // value), so this is the option overriding the default, not agreeing.
        W.Names := RunNames(W, TQuery.New.From('people').OrderByDesc('age').NullsFirst);
      end)
    .&Then('Dan (NULL age) leads and the oldest follows',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Names[0]).ToEqual('Dan');
        Expect(W.Names[1]).ToEqual('Eve');   // 40
      end)

  .Scenario('Ordered, paginated result')
    .When('OrderBy(name) then ForPage(2, 2) runs',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').OrderBy('name').ForPage(2, 2));
      end)
    .&Then('the second page of two is Cara, Dan',
      procedure(W: TFastPathWorld)
      begin
        Expect(Length(W.Names)).ToEqual(2);
        Expect(W.Names[0]).ToEqual('Cara');
        Expect(W.Names[1]).ToEqual('Dan');
      end)

  .Scenario('Limit yields exactly N in source order')
    .When('Limit(3) runs with no ORDER BY',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').Limit(3));
      end)
    .&Then('exactly the first three elements are produced',
      procedure(W: TFastPathWorld)
      begin
        Expect(Length(W.Names)).ToEqual(3);
        Expect(W.Names[0]).ToEqual('Ann');
        Expect(W.Names[2]).ToEqual('Cara');
      end)

  .Scenario('DISTINCT collapses duplicate rows, including duplicate NULLs')
    .When('Distinct runs over a list with duplicate rows and duplicate NULL ages',
      procedure(W: TFastPathWorld)
      begin
        var Dups := TObjectList<TPerson>.Create(True);
        try
          Dups.Add(TPerson.Create('X', 1,    'US'));
          Dups.Add(TPerson.Create('X', 1,    'US'));   // exact duplicate
          Dups.Add(TPerson.Create('Y', 2,    'US'));
          Dups.Add(TPerson.Create('Z', Null, 'US'));
          Dups.Add(TPerson.Create('Z', Null, 'US'));   // duplicate NULL age
          W.Count := 0;
          for var P in TInMemory.Elements<TPerson>(
            TQuery.New.From('people').Distinct, Dups) do
            Inc(W.Count);
        finally
          Dups.Free;
        end;
      end)
    .&Then('three distinct rows remain (duplicate NULLs collapse to one)',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(3);
      end)

.Rule('A plan is reusable, and a multi-source query on the single-list entry fails fast')

  .Scenario('A compiled plan is reused over two different sources')
    .When('one plan runs over the world list and a second smaller list',
      procedure(W: TFastPathWorld)
      begin
        var Plan := TInMemory.Compile<TPerson>(
          TQuery.New.From('people').Where('age', '>', 18));
        W.Count := 0;
        for var P in Plan.Over(W.People) do
          Inc(W.Count);                       // Ann, Cara, Eve, Finn = 4
        var Other := TObjectList<TPerson>.Create(True);
        try
          Other.Add(TPerson.Create('Zoe', 50, 'US'));
          Other.Add(TPerson.Create('Kid', 10, 'US'));
          W.SecondCount := 0;
          for var P in Plan.Over(Other) do
            Inc(W.SecondCount);               // Zoe only = 1
        finally
          Other.Free;
        end;
      end)
    .&Then('each execution derives results from its own source only',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Count).ToEqual(4);
        Expect(W.SecondCount).ToEqual(1);
      end)

  .Scenario('A join query executed with a single list is rejected')
    .Given('a JOIN query, and the single list the fast lane takes',
      procedure(W: TFastPathWorld)
      begin
        W.Query := TQuery.New.From('people').Join('orders', 'people.id', 'orders.person_id');
      end)
    .When('it is executed over that one list',
      procedure(W: TFastPathWorld)
      begin
        try
          TInMemory.Elements<TPerson>(W.Query, W.People);
          W.Outcome := '<the fast lane took the join>';
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('compiling a JOIN for the fast lane raises a descriptive error',
      procedure(W: TFastPathWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)

.Rule('Comparing with Null selects the rows whose value is NULL')

  .Scenario('In memory it selects the rows with NULL')
    .Given('a list of people in which two have their age at NULL',
      procedure(W: TFastPathWorld)
      begin
        W.People.Add(TPerson.Create('Gus', Null, 'US'));
      end)
    .When('From(''people'').Where(''age'', Null) runs in memory',
      procedure(W: TFastPathWorld)
      begin
        W.Names := RunNames(W, TQuery.New.From('people').Where('age', Null));
      end)
    .&Then('it selects exactly those two',
      procedure(W: TFastPathWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('Dan,Gus');
      end)

.Rule('WhereTrue still selects the true rows in memory')

  .Scenario('In memory WhereTrue still selects the true ones')
    .Given('a list of users with Active True in two of them',
      procedure(W: TFastPathWorld)
      begin
        W.Users := TObjectList<TActiveUser>.Create(True);
        W.Users.Add(TActiveUser.Create('Ann', True));
        W.Users.Add(TActiveUser.Create('Bob', False));
        W.Users.Add(TActiveUser.Create('Cara', True));
      end)
    .When('From(''users'').WhereTrue(''Active'') runs in memory',
      procedure(W: TFastPathWorld)
      begin
        W.Names := nil;
        for var U in TInMemory.Elements<TActiveUser>(TQuery.New.From('users').WhereTrue('Active'), W.Users) do
          W.Names := W.Names + [U.Name];
      end)
    .&Then('it selects exactly those two',
      procedure(W: TFastPathWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('Ann,Cara');
      end)

.Rule('LIKE keeps the wildcards of the value unless they are escaped')

  .Scenario('Without an escape character the wildcards of the value stay active')
    .Given('a list with the codes A_1 and AB1',
      procedure(W: TFastPathWorld)
      begin
        W.Items := TObjectList<TTagged>.Create(True);
        W.Items.Add(TTagged.Create('A_1', ''));
        W.Items.Add(TTagged.Create('AB1', ''));
      end)
    .When('From(''items'').WhereStarts(''code'', ''A_'') runs in memory',
      procedure(W: TFastPathWorld)
      begin
        W.Names := nil;
        for var T in TInMemory.Elements<TTagged>(TQuery.New.From('items').WhereStarts('code', 'A_'), W.Items) do
          W.Names := W.Names + [T.Code];
      end)
    .&Then('it selects both',
      procedure(W: TFastPathWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('A_1,AB1');
      end)

  .Scenario('In memory the escaped character is literal')
    .Given('a list with the codes A_1 and AB1',
      procedure(W: TFastPathWorld)
      begin
        W.Items := TObjectList<TTagged>.Create(True);
        W.Items.Add(TTagged.Create('A_1', ''));
        W.Items.Add(TTagged.Create('AB1', ''));
      end)
    .When('From(''items'').WhereStarts(''code'', ''A!_'', False, ''!'') runs in memory',
      procedure(W: TFastPathWorld)
      begin
        W.Names := nil;
        for var T in TInMemory.Elements<TTagged>(TQuery.New.From('items').WhereStarts('code', 'A!_', False, '!'), W.Items) do
          W.Names := W.Names + [T.Code];
      end)
    .&Then('it selects only A_1',
      procedure(W: TFastPathWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('A_1');
      end)

.Rule('The case-insensitive comparison folds the case of all Unicode')

  .Scenario('In memory a non-ASCII uppercase letter matches its lowercase')
    .Given('a list with the names Ñandú and Nata',
      procedure(W: TFastPathWorld)
      begin
        W.Items := TObjectList<TTagged>.Create(True);
        W.Items.Add(TTagged.Create('', 'Ñandú'));
        W.Items.Add(TTagged.Create('', 'Nata'));
      end)
    .When('From(''animals'').WhereStarts(''name'', ''ñan'') runs in memory',
      procedure(W: TFastPathWorld)
      begin
        W.Names := nil;
        for var T in TInMemory.Elements<TTagged>(TQuery.New.From('animals').WhereStarts('name', 'ñan'), W.Items) do
          W.Names := W.Names + [T.Name];
      end)
    .&Then('it selects only Ñandú',
      procedure(W: TFastPathWorld)
      begin
        Expect(string.Join(',', W.Names)).ToEqual('Ñandú');
      end);

end.
