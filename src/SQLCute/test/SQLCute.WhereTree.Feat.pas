unit SQLCute.WhereTree.Feat;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.WhereTree;

type
  TWhereTreeWorld = class
  public
    Query: IQuery;
    Root: TWhereNode;
    /// The concrete clause types the process knows about, read back over RTTI.
    ClauseTypes: TArray<string>;
    /// What the classification has to say, as a line per type left undecided.
    Report: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.SQLCute.Clauses;

const
  /// The sentinel a clean classification reports. Compared with ToEqual rather
  /// than ToBeEmpty so a failure prints the report, naming the types.
  AllClassified = 'every clause type is classified';

  /// Types that become part of the normalized WHERE tree. This list and
  /// TWhereTree's own IsWhereClause are two statements of one fact; the spec
  /// below is what keeps them from drifting apart.
  WhereTreeParticipants: array[0..3] of string = (
    'TWhereClause', 'TNestedWhereClause', 'TWhereInClause', 'TDateWhereClause');

  /// Types that become part of the normalized HAVING tree. Classified on the
  /// same terms, so the two families cannot diverge in how a new type is noticed.
  HavingTreeParticipants: array[0..0] of string = (
    'THavingClause');

  /// Types deliberately outside both condition trees: projection, source, join,
  /// grouping, ordering, paging, set operations, CTEs and DML. Landing a type
  /// here is a decision, and a reviewable one — which is the whole point.
  ConditionFreeClauses: array[0..16] of string = (
    'TSelectClause', 'TFromClause', 'TFromRawClause', 'TFromSubqueryClause',
    'TJoinClause', 'TJoinSubqueryClause', 'TGroupByClause', 'TOrderByClause',
    'TLimitClause', 'TOffsetClause', 'TRawClause', 'TUnionClause', 'TWithClause',
    'TDistinctClause', 'TInsertClause', 'TUpdateSetClause', 'TDeleteClause');

constructor TWhereTreeWorld.Create;
begin
  inherited;
  Query := TQuery.New;
end;

destructor TWhereTreeWorld.Destroy;
begin
  Root.Free;
  inherited;
end;

// The concrete descendants of TAbstractClause the process knows about. Derived
// from the types themselves rather than from a list of names, so *adding* a type
// is what trips the spec. The runner sets {$STRONGLINKTYPES ON}, so nothing is
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

// How many of the three classifications claim this type. Exactly one is the
// only acceptable answer: none means a decision was never made, more than one
// means two were.
function ClassificationCount(const Name: string): Integer;
begin
  Result := 0;
  if NameIsIn(Name, WhereTreeParticipants) then Inc(Result);
  if NameIsIn(Name, HavingTreeParticipants) then Inc(Result);
  if NameIsIn(Name, ConditionFreeClauses) then Inc(Result);
end;

// The clauses of the tree's leaves, by identity. A leaf's Clause points back
// into the query's AST, so pointer equality is what "this condition reached the
// tree" means.
function LeafClausesOf(N: TWhereNode): TArray<TAbstractClause>;
begin
  Result := nil;
  if N = nil then
    Exit;
  if N.Kind = TWhereNodeKind.Leaf then
  begin
    Result := [N.Clause];
    Exit;
  end;
  for var Child in N.Children do
    Result := Result + LeafClausesOf(Child);
end;

function Contains(const Clauses: TArray<TAbstractClause>; C: TAbstractClause): Boolean;
begin
  for var X in Clauses do
    if X = C then
      Exit(True);
  Result := False;
end;

// Every WHERE participant the query carries, nested groups flattened. A group is
// not itself a leaf — it contributes its subclauses — so it is the subclauses
// that have to be reachable.
function WhereParticipantsOf(const Clauses: TArray<TAbstractClause>): TArray<TAbstractClause>;
begin
  Result := nil;
  for var C in Clauses do
    if C is TNestedWhereClause then
      Result := Result + WhereParticipantsOf(TNestedWhereClause(C).SubClauses)
    else if NameIsIn(C.ClassName, WhereTreeParticipants) then
      Result := Result + [C];
end;

function LeafClassNames(N: TWhereNode): TArray<string>;
begin
  Result := nil;
  for var C in LeafClausesOf(N) do
    Result := Result + [C.ClassName];
end;

initialization

Feature('''
Feature WhereTree @wheretree @sqlcute

  As the shared WHERE normalizer
  I want a flat clause list turned into a precedence-correct boolean tree
  So both back-ends apply SQL precedence (AND before OR) by construction
''')

.UseWorld<TWhereTreeWorld>

.Rule('Precedence grouping')

  .Scenario('A single condition becomes a leaf')
    .When('a single equality condition is built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').Where('a', '=', 1);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the tree is a single leaf',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(W.Root <> nil).ToBeTrue;
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end)

  .Scenario('A run of ANDs becomes one conjunction with no OR wrapper')
    .When('two AND-joined conditions are built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').Where('a', '=', 1).Where('b', '=', 2);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the root is a conjunction of two leaves',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Conjunction));
        Expect(Length(W.Root.Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end)

  .Scenario('Mixed AND/OR groups AND before OR')
    .When('a AND b OR c AND d is built with no explicit group',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t')
          .Where('a', '=', 1)
          .Where('b', '=', 2)
          .OrWhere('c', '=', 3)
          .Where('d', '=', 4);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the root is (a AND b) OR (c AND d)',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Disjunction));
        Expect(Length(W.Root.Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Conjunction));
        Expect(Length(W.Root.Children[0].Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[1].Kind)).ToEqual(Ord(TWhereNodeKind.Conjunction));
        Expect(Length(W.Root.Children[1].Children)).ToEqual(2);
      end)

  .Scenario('WhereNot wraps the condition in a negation node')
    .When('a negated condition is built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').WhereNot('a', '>', 18);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the tree is a negation of a leaf',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Neg));
        Expect(Length(W.Root.Children)).ToEqual(1);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end)

  // The negation wraps the group's subtree whole — it is not pushed down onto
  // the inner conditions, so the subtree under Neg is exactly the one the same
  // group yields without WhereNot (a Disjunction of two leaves).
  .Scenario('WhereNot(callback) wraps the whole group subtree in a negation node')
    .When('a negated group of two OR-joined conditions is built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').WhereNot(
          function(Q: IQuery): IQuery
          begin
            Result := Q.Where('a', '=', 1).OrWhere('b', '=', 2);
          end);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the root is a negation of the disjunction the unnegated group yields',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Neg));
        Expect(Length(W.Root.Children)).ToEqual(1);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Disjunction));
        Expect(Length(W.Root.Children[0].Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[0].Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
        Expect(Ord(W.Root.Children[0].Children[1].Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end)

// ---------------------------------------------------------------------------
// The normalizer accepts clauses through a whitelist and skips everything else
// in silence. Today that whitelist matches the AST exactly, so nothing is lost.
// The day a conditional clause type is added and the whitelist is not updated,
// the condition vanishes from the tree and the query *widens* — the worst way a
// filter can fail, and a completely mute one.
//
// So the classification below is exhaustive by construction: it is checked
// against the types the process actually knows, not against a list of names
// someone remembered to update.

.Rule('Every clause type is deliberately classified')

  .Scenario('EveryClauseTypeIsClassified')
    .When('the concrete clause types the process knows are read back over RTTI',
      procedure(W: TWhereTreeWorld)
      begin
        W.ClauseTypes := ConcreteClauseClasses;
        W.Report := '';
        for var Name in W.ClauseTypes do
          case ClassificationCount(Name) of
            1: ;   // decided, one way or the other
            0:
              W.Report := W.Report + Format(
                'Clause type %s is classified nowhere. Decide: does it participate ' +
                'in the normalized WHERE tree, in the HAVING tree, or in neither? ' +
                'Add it to WhereTreeParticipants, HavingTreeParticipants or ' +
                'ConditionFreeClauses in this unit — and if it participates, teach ' +
                'TWhereTree''s IsWhereClause / IsHavingClause about it too, or the ' +
                'condition will be dropped from the tree in silence. ', [Name]);
          else
            W.Report := W.Report + Format(
              'Clause type %s is claimed by more than one classification. ', [Name]);
          end;
        if W.Report = '' then
          W.Report := AllClassified;
      end)
    .&Then('each of them appears in exactly one classification, and the report names any that does not',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(W.Report).ToEqual(AllClassified);
      end)

  .Scenario('ClassificationIsNotVacuous')
    .When('the clause types are enumerated',
      procedure(W: TWhereTreeWorld)
      begin
        W.ClauseTypes := ConcreteClauseClasses;
        W.Report := '';
        // The walk has to find every type the lists claim to have decided about.
        // Without this a walk that returned nothing would satisfy the scenario
        // above vacuously — every one of no types is classified.
        for var Name in WhereTreeParticipants do
          if not NameIsIn(Name, W.ClauseTypes) then
            W.Report := W.Report + Format('%s was not enumerated. ', [Name]);
        for var Name in HavingTreeParticipants do
          if not NameIsIn(Name, W.ClauseTypes) then
            W.Report := W.Report + Format('%s was not enumerated. ', [Name]);
        for var Name in ConditionFreeClauses do
          if not NameIsIn(Name, W.ClauseTypes) then
            W.Report := W.Report + Format('%s was not enumerated. ', [Name]);
        if W.Report = '' then
          W.Report := AllClassified;
      end)
    .&Then('the enumeration found types, so a broken RTTI walk cannot pass as success',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Length(W.ClauseTypes)).ToBeGreaterThan(0);
      end)
    .&Then('and it reached every type the classification decided about',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(W.Report).ToEqual(AllClassified);
      end)

.Rule('A condition clause cannot be dropped from the tree without notice')

  .Scenario('EveryConditionClauseReachesTheTree')
    .When('a query carries one condition of each participating clause type',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t')
          .Where('a', '=', 1)                       // TWhereClause
          .WhereIn('b', [1, 2])                     // TWhereInClause
          .WhereDate('c', '2024-01-01')             // TDateWhereClause
          .Where(                                   // TNestedWhereClause
            function(Q: IQuery): IQuery
            begin
              Result := Q.Where('d', '=', 4);
            end);
        W.Root := TWhereTree.Build(W.Query.Clauses);
        W.Report := '';
        // Identity, not shape: a leaf's Clause points back into the AST, so this
        // asks whether *this* condition survived, not whether one like it did.
        for var C in WhereParticipantsOf(W.Query.Clauses) do
          if not Contains(LeafClausesOf(W.Root), C) then
            W.Report := W.Report + Format(
              'A %s the query carries is missing from the normalized tree. ', [C.ClassName]);
        // And the query really did exercise every participating type, so the
        // check above cannot pass by having tested less than it claims.
        for var Name in WhereTreeParticipants do
        begin
          var Seen := False;
          for var C in W.Query.Clauses do
            Seen := Seen or (C.ClassName = Name);
          if not Seen then
            W.Report := W.Report + Format(
              'No %s was built, so this scenario no longer covers it. ', [Name]);
        end;
        if W.Report = '' then
          W.Report := AllClassified;
      end)
    .&Then('normalizing yields a tree containing every one of them',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(W.Report).ToEqual(AllClassified);
      end)

  .Scenario('NonConditionClausesAreIgnored')
    .When('a query carries projection, source, join, grouping, ordering and paging clauses',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query
          .Select(['a', 'b'])
          .From('t')
          .Distinct
          .Join('u', 't.id', 'u.t_id')
          .GroupBy('a')
          .OrderBy('a')
          .Limit(10)
          .Offset(5)
          .Where('a', '=', 1);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('normalizing ignores them without error, leaving only the condition',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
        Expect(W.Root.Clause.ClassName).ToEqual('TWhereClause');
      end)

.Rule('HAVING is classified on the same terms as WHERE')

  .Scenario('EveryHavingClauseReachesItsTree')
    .When('a query carries a HAVING condition alongside a WHERE one',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('orders')
          .GroupBy('user_id')
          .Where('active', '=', True)
          .Having('total', '>', 100);
        W.Root := TWhereTree.BuildHaving(W.Query.Clauses);
        W.Report := '';
        // Every name on the HAVING list has to show up in the tree this query
        // produces. Adding a HAVING clause type without giving it a condition
        // here leaves its name in the report — the same pressure the WHERE
        // family is under, which is the point of classifying both.
        for var Name in HavingTreeParticipants do
          if not NameIsIn(Name, LeafClassNames(W.Root)) then
            W.Report := W.Report + Format(
              '%s is classified as a HAVING participant but did not reach the ' +
              'HAVING tree. ', [Name]);
        if W.Report = '' then
          W.Report := AllClassified;
      end)
    .&Then('the HAVING tree holds every type classified as a HAVING participant',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(W.Report).ToEqual(AllClassified);
      end)
    .&Then('and nothing from the WHERE family leaks into it',
      procedure(W: TWhereTreeWorld)
      begin
        for var Name in LeafClassNames(W.Root) do
          Expect(NameIsIn(Name, WhereTreeParticipants)).ToBeFalse;
      end);

end.
