unit SQLCute.Kind.Feat;

{
  The statement kind a query declares: its default (a retrieval), how the DML
  facades set it, how declaring another kind replaces the previous one leaving
  no trace, and the completeness guard that keeps every clause type classified
  on the statement axis. The completeness Rule is calqued from
  SQLCute.WhereTree.Feat's WHERE-family check — same shape, other axis.
}

interface

uses
  Daf.SQLCute;

type
  TKindWorld = class
  public
    Query: IQuery;
    Compiler: IQueryCompiler;
    LastResult: TSQLResult;
    /// The concrete clause types the process knows, read back over RTTI.
    ClauseTypes: TArray<string>;
    /// What the classification has to say, a line per type left undecided.
    Report: string;
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

const
  /// The sentinel a clean classification reports. Compared with ToEqual rather
  /// than ToBeEmpty so a failure prints the report, naming the types.
  AllClassified = 'every clause type is classified on the statement axis';

  /// The clause types that mark a DML statement. This list and TQueryImpl's own
  /// StatementKindOf are two statements of one fact; the spec below is what
  /// keeps them from drifting apart.
  StatementKindCarriers: array[0..2] of string = (
    'TInsertClause', 'TUpdateSetClause', 'TDeleteClause');

  /// Clause types deliberately outside the statement axis: projection, source,
  /// conditions, ordering, paging, joins, grouping, set operations, CTEs. A
  /// retrieval is built from these; landing a type here is a reviewable
  /// decision — which is the whole point.
  StatementFreeClauses: array[0..18] of string = (
    'TSelectClause', 'TFromClause', 'TFromRawClause', 'TFromSubqueryClause',
    'TWhereClause', 'TNestedWhereClause', 'TWhereInClause', 'TDateWhereClause',
    'TOrderByClause', 'TLimitClause', 'TOffsetClause', 'TRawClause',
    'TJoinClause', 'TJoinSubqueryClause', 'TGroupByClause', 'THavingClause',
    'TUnionClause', 'TWithClause', 'TDistinctClause');

constructor TKindWorld.Create;
begin
  inherited;
  Query := TQuery.New;
  Compiler := TAnsiSqlCompiler.Create;
end;

// The concrete descendants of TAbstractClause the process knows about. Derived
// from the types themselves rather than a list of names, so *adding* a type is
// what trips the spec. The runner sets {$STRONGLINKTYPES ON}, so nothing is
// quietly dropped from RTTI.
function ConcreteClauseClasses: TArray<string>;
begin
  Result := nil;
  var Ctx := TRttiContext.Create;
  try
    for var T in Ctx.GetTypes do
      if T is TRttiInstanceType then
      begin
        var C := TRttiInstanceType(T).MetaclassType;
        if (C <> TAbstractClause) and C.InheritsFrom(TAbstractClause) then
          Result := Result + [C.ClassName];
      end;
  finally
    Ctx.Free;
  end;
end;

function NameIsIn(const Name: string; const Names: array of string): Boolean;
begin
  for var N in Names do
    if N = Name then
      Exit(True);
  Result := False;
end;

// How many of the two classifications claim this type. Exactly one is the only
// acceptable answer: none means a decision was never made, more than one means
// two were.
function ClassificationCount(const Name: string): Integer;
begin
  Result := 0;
  if NameIsIn(Name, StatementKindCarriers) then Inc(Result);
  if NameIsIn(Name, StatementFreeClauses) then Inc(Result);
end;

function HasClauseOfType(const Clauses: TArray<TAbstractClause>; const Name: string): Boolean;
begin
  for var C in Clauses do
    if C.ClassName = Name then
      Exit(True);
  Result := False;
end;

initialization

Feature('''
Feature Statement kind @sqlcute @kind

  As a SQLCute query
  I want to declare which statement I am
  So a back-end consults that instead of deducing it from my clauses
''')

.UseWorld<TKindWorld>

.Rule('A query declares its statement kind')

  .Scenario('A freshly built query is a retrieval')
    .When('a query is built without declaring any modification',
      procedure(W: TKindWorld)
      begin
        W.Query.From('t').Where('a', '=', 1);
      end)
    .&Then('its statement kind is the retrieval kind',
      procedure(W: TKindWorld)
      begin
        Expect(Ord(W.Query.Kind)).ToEqual(Ord(TQueryKind.Select));
      end)

  .Scenario('Declaring an insertion sets Insert')
    .When('an insertion is declared',
      procedure(W: TKindWorld)
      begin
        W.Query.From('users').AsInsert(['name'], ['Ann']);
      end)
    .&Then('the statement kind is Insert',
      procedure(W: TKindWorld)
      begin
        Expect(Ord(W.Query.Kind)).ToEqual(Ord(TQueryKind.Insert));
      end)

  .Scenario('Declaring an update sets Update')
    .When('an update is declared',
      procedure(W: TKindWorld)
      begin
        W.Query.From('users').Where('id', '=', 1).AsUpdate(['status'], ['x']);
      end)
    .&Then('the statement kind is Update',
      procedure(W: TKindWorld)
      begin
        Expect(Ord(W.Query.Kind)).ToEqual(Ord(TQueryKind.Update));
      end)

  .Scenario('Declaring a deletion sets Delete')
    .When('a deletion is declared',
      procedure(W: TKindWorld)
      begin
        W.Query.From('users').Where('id', '=', 1).AsDelete;
      end)
    .&Then('the statement kind is Delete',
      procedure(W: TKindWorld)
      begin
        Expect(Ord(W.Query.Kind)).ToEqual(Ord(TQueryKind.Delete));
      end)

  // Two AsUpdate calls are how an UPDATE declares its columns in two goes. The
  // second must not reset anything: same kind, so it only adds columns.
  .Scenario('Declaring the same kind twice accumulates, it does not change')
    .When('an update declares its columns in two calls',
      procedure(W: TKindWorld)
      begin
        W.Query.From('users').Where('id', '=', 1)
          .AsUpdate(['name'], ['Jane'])
          .AsUpdate(['email'], ['jane@example.com']);
        W.LastResult := W.Query.Compile(W.Compiler);
      end)
    .&Then('the kind is still Update',
      procedure(W: TKindWorld)
      begin
        Expect(Ord(W.Query.Kind)).ToEqual(Ord(TQueryKind.Update));
      end)
    .&Then('and the statement carries both columns',
      procedure(W: TKindWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('UPDATE users SET name = ?, email = ? WHERE id = ?');
        Expect(Length(W.LastResult.Bindings)).ToEqual(3);
      end)

.Rule('Declaring another kind replaces the previous one, leaving no trace')

  // A change of observable behaviour: when the kind was deduced, the first
  // declared won. The scenario asserts on the emitted SQL, which is what a
  // consumer sees.
  .Scenario('The last kind declared is the one compiled')
    .When('an insertion then a deletion are declared on the same query',
      procedure(W: TKindWorld)
      begin
        W.Query.From('users').AsInsert(['name'], ['Ann']).AsDelete;
        W.LastResult := W.Query.Compile(W.Compiler);
      end)
    .&Then('the statement kind is Delete',
      procedure(W: TKindWorld)
      begin
        Expect(Ord(W.Query.Kind)).ToEqual(Ord(TQueryKind.Delete));
      end)
    .&Then('and what is compiled is a delete',
      procedure(W: TKindWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('DELETE FROM users');
      end)

  .Scenario('No clause of the replaced kind remains')
    .When('a deletion replaces an insertion',
      procedure(W: TKindWorld)
      begin
        W.Query.From('users').AsInsert(['name'], ['Ann']).AsDelete;
      end)
    .&Then('the tree keeps no clause of the replaced kind',
      procedure(W: TKindWorld)
      begin
        for var C in W.Query.Clauses do
          Expect(C is TInsertClause).ToBeFalse;
      end)
    .&Then('and it does carry the replacing kind''s clause',
      procedure(W: TKindWorld)
      begin
        Expect(HasClauseOfType(W.Query.Clauses, 'TDeleteClause')).ToBeTrue;
      end)

// ---------------------------------------------------------------------------
// The enumerated kind on its own only moves the deduction: whoever adds a clause
// of a new statement kind still has to remember to set that kind, and
// remembering is exactly what fails. So the classification below is exhaustive
// by construction — checked against the types the process actually knows, not a
// list of names someone remembered to update. Its teeth are the same the WHERE
// family already has, applied to the axis that was uncovered.

.Rule('Every clause type is classified on the statement axis')

  .Scenario('EveryClauseTypeIsClassifiedOnStatementAxis')
    .When('the concrete clause types the process knows are read back over RTTI',
      procedure(W: TKindWorld)
      begin
        W.ClauseTypes := ConcreteClauseClasses;
        W.Report := '';
        for var Name in W.ClauseTypes do
          case ClassificationCount(Name) of
            1: ;   // decided, one way or the other
            0:
              W.Report := W.Report + Format(
                'Clause type %s is classified nowhere on the statement axis. ' +
                'Decide: does it mark a DML statement, or is it structural (part ' +
                'of a retrieval)? Add it to StatementKindCarriers or ' +
                'StatementFreeClauses in this unit — and if it marks a statement, ' +
                'teach TQueryImpl''s StatementKindOf about it too, or replacing ' +
                'that kind will leave it in the tree. ', [Name]);
          else
            W.Report := W.Report + Format(
              'Clause type %s is claimed by more than one classification. ', [Name]);
          end;
        if W.Report = '' then
          W.Report := AllClassified;
      end)
    .&Then('each appears in exactly one classification, and the report names any that does not',
      procedure(W: TKindWorld)
      begin
        Expect(W.Report).ToEqual(AllClassified);
      end)

  .Scenario('StatementClassificationIsNotVacuous')
    .When('the clause types are enumerated',
      procedure(W: TKindWorld)
      begin
        W.ClauseTypes := ConcreteClauseClasses;
        W.Report := '';
        // The walk has to find every type the lists claim to have decided about.
        // Without this a walk that returned nothing would satisfy the scenario
        // above vacuously — every one of no types is classified.
        for var Name in StatementKindCarriers do
          if not NameIsIn(Name, W.ClauseTypes) then
            W.Report := W.Report + Format('%s was not enumerated. ', [Name]);
        for var Name in StatementFreeClauses do
          if not NameIsIn(Name, W.ClauseTypes) then
            W.Report := W.Report + Format('%s was not enumerated. ', [Name]);
        if W.Report = '' then
          W.Report := AllClassified;
      end)
    .&Then('the enumeration found types, so a broken RTTI walk cannot pass as success',
      procedure(W: TKindWorld)
      begin
        Expect(Length(W.ClauseTypes)).ToBeGreaterThan(0);
      end)
    .&Then('and it reached every type the classification decided about',
      procedure(W: TKindWorld)
      begin
        Expect(W.Report).ToEqual(AllClassified);
      end);

end.
