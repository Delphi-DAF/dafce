unit Daf.SQLCute.Expressions;

{
  Daf.SQLCute.Expressions — the SQL-side half of the expression layer: the one
  lowering (expression tree -> WHERE clauses on an IQuery) and the in-memory
  IRepository<T> adapter.

  There is exactly one lowering and one target. Both back-ends are already
  downstream of the SQLCute AST — the dialect compilers emit SQL from it and
  TInMemory compiles an executable plan from it — so "one query, two back-ends"
  is a property this unit inherits rather than one it implements. There is no
  expression -> Daf.Linq translator here, and there never should be.

  The name is honest precisely because of that: the only thing this unit
  produces is an IQuery.

  ---------------------------------------------------------------------------
  What the emitter has to get right, and why it is the whole risk

  TWhereTree.Build reconstructs a boolean tree from a flat clause list; it does
  not preserve one. It reads that list as OR-of-AND-runs:

      list   := clause (connector clause)*      =>  OR( AND(...), AND(...), ... )
      clause := leaf | nested(list)  [+ IsNot]

  which expresses exactly one level of alternation. Anything deeper has to be
  nested explicitly or it silently changes meaning: emit `(a or b) and c` as the
  flat list [a AND, b OR, c AND] and the normalizer groups it as `a OR (b AND c)`
  — a different predicate.

  And because both back-ends consume that same normalized tree, the parity
  harness cannot catch this class of bug: SQL and in-memory agree, on the wrong
  answer. That is why emission is asserted here, at the emitter, and why the
  assertion is semantic rather than structural.
}

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Variants,
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.Query.Expressions;

type
  /// <summary>
  ///   An expression that cannot be lowered — a non-predicate node where a
  ///   condition belongs, or an operator handed the wrong number of operands.
  ///   It is raised rather than skipped: a clause silently dropped here would
  ///   widen the query, which is the worst possible failure mode for a filter.
  /// </summary>
  EExprLoweringError = class(Exception);

const
  /// <summary>
  ///   The model's comparison operators mapped onto SQLCute's. This table is the
  ///   entire price of the model keeping its own enum (Decision 6) — and it is a
  ///   table, not a traversal, so Principle 3 has nothing to object to.
  /// </summary>
  /// <remarks>
  ///   Three operators collapse onto <c>Like</c> because that is genuinely what
  ///   SQLCute emits for them: <c>StartsWith</c>, <c>EndsWith</c> and
  ///   <c>Contains</c> are LIKE with the pattern built for you. Nothing maps to
  ///   <c>Raw</c>, <c>Exists</c> or <c>NotExists</c>, and nothing can: the model
  ///   has no member that would.
  /// </remarks>
  WhereOpFor: array[TCompareOp] of TWhereOp = (
    TWhereOp.Equal, TWhereOp.NotEqual,
    TWhereOp.Less, TWhereOp.LessOrEqual, TWhereOp.Greater, TWhereOp.GreaterOrEqual,
    TWhereOp.Like, TWhereOp.NotLike,
    TWhereOp.Like, TWhereOp.Like, TWhereOp.Like,
    TWhereOp.&In, TWhereOp.NotIn,
    TWhereOp.&Between, TWhereOp.NotBetween,
    TWhereOp.IsNull, TWhereOp.IsNotNull);

type
  /// <summary>
  ///   The lowering. One expression in, WHERE clauses on one <c>IQuery</c> out,
  ///   and nothing else — no SQL text, no predicate, no second target.
  /// </summary>
  TExprLowering = record
  public
    /// <summary>
    ///   Lowers <c>Expression</c> into WHERE clauses on <c>Query</c> and returns
    ///   <c>Query</c>. Members become columns verbatim.
    /// </summary>
    class function Where(const Query: IQuery; const Expression: IExpr): IQuery; overload; static;
    /// <summary>
    ///   Same, resolving each member to its column through <c>Names</c> — the one
    ///   place and the one time a column name is decided.
    /// </summary>
    class function Where(const Query: IQuery; const Expression: IExpr;
      const Names: INameResolution; EntityType: PTypeInfo): IQuery; overload; static;
    /// <summary>
    ///   Lowers a whole specification — filter, ordering and page — onto
    ///   <c>Query</c>. They travel together so a caller never has to remember to
    ///   apply the ordering separately from the rule it belongs to.
    /// </summary>
    class function Apply<T>(const Query: IQuery; const Spec: TSpecification<T>;
      const Names: INameResolution = nil): IQuery; static;
  end;

/// <summary>
///   The column a member resolves to: through <c>Names</c> when one is supplied,
///   verbatim otherwise. The single point at which persistence vocabulary enters,
///   which is why it is one function and not a rule repeated per call site.
/// </summary>
function ColumnFor(const Names: INameResolution; EntityType: PTypeInfo;
  const Member: string): string;

implementation

type
  /// How a member becomes a column while walking the tree. No strategy means
  /// identity, which is the shipped convention.
  TEmitCtx = record
    Names: INameResolution;
    EntityType: PTypeInfo;
    function ColumnOf(const Node: IExpr): string;
    function ValueOf(const Node: IExpr): Variant;
    function ValuesOf(const Nodes: TArray<IExpr>): TArray<Variant>;
  end;

const
  /// The operator text <c>IQuery.Where(Column, Op, Value)</c> parses back into a
  /// <c>TWhereOp</c>, for the operators emitted that way. Empty where a dedicated
  /// fluent verb carries the operator instead (the LIKE, IN, BETWEEN and NULL
  /// families all have one).
  OpTextFor: array[TCompareOp] of string = (
    '=', '<>', '<', '<=', '>', '>=',
    '', '', '', '', '',
    '', '',
    '', '',
    '', '');

{ ColumnFor }

function ColumnFor(const Names: INameResolution; EntityType: PTypeInfo;
  const Member: string): string;
begin
  if Names = nil then
    Result := Member
  else
    Result := Names.ColumnFor(EntityType, Member);
end;

{ TEmitCtx }

function TEmitCtx.ColumnOf(const Node: IExpr): string;
begin
  if (Node = nil) or (Node.Kind <> TExprKind.Member) then
    raise EExprLoweringError.Create(
      'The left-hand side of a comparison must be a member reference.');
  Result := ColumnFor(Names, EntityType, (Node as IMemberExpr).Name);
end;

function TEmitCtx.ValueOf(const Node: IExpr): Variant;
begin
  // A param leaf is a value leaf with a name, so one branch serves both.
  if (Node = nil) or not ((Node.Kind = TExprKind.Value) or (Node.Kind = TExprKind.Param)) then
    raise EExprLoweringError.Create(
      'The right-hand side of a comparison must be a value or a parameter.');
  Result := (Node as IValueExpr).Value;
end;

function TEmitCtx.ValuesOf(const Nodes: TArray<IExpr>): TArray<Variant>;
begin
  SetLength(Result, Length(Nodes));
  for var I := 0 to High(Nodes) do
    Result[I] := ValueOf(Nodes[I]);
end;

{ Emission }

procedure EmitTop(const Node: IExpr; const Q: IQuery; const Ctx: TEmitCtx); forward;

// The spine of one logical operator, left to right. A node that is not that
// operator is a spine of one, which is what makes the callers uniform.
function FlattenLogical(const Node: IExpr; Op: TLogicalOp): TArray<IExpr>;
begin
  if (Node <> nil) and (Node.Kind = TExprKind.Logical) and ((Node as ILogicalExpr).Op = Op) then
  begin
    var L := Node as ILogicalExpr;
    Result := FlattenLogical(L.Left, Op) + FlattenLogical(L.Right, Op);
  end
  else
    Result := [Node];
end;

procedure RequireArity(const C: ICompareExpr; Expected: Integer);
begin
  if Length(C.Operands) <> Expected then
    raise EExprLoweringError.CreateFmt(
      'Operator %s takes %d operand(s), got %d.',
      [GetEnumName(TypeInfo(TCompareOp), Ord(C.Op)), Expected, Length(C.Operands)]);
end;

// One condition, emitted as a single clause. Negation is the leaf's own — the
// IsNot flag that WhereNot(Column, ...) sets, the paired negated operator, or
// TWhereInClause.Negated — so no gratuitous NOT (...) wrapper appears and no
// rewriting happens. Every clause goes through the fluent surface, which is also
// what keeps TWhereInClause.Connector the string 'AND'/'OR' it is compared as.
procedure EmitLeaf(const C: ICompareExpr; const Q: IQuery; const Ctx: TEmitCtx;
  Connector: TLogicalOp; Negated: Boolean);
begin
  var Col := Ctx.ColumnOf(C.Target);
  var IsOr := Connector = TLogicalOp.opOr;
  var CS := C.CaseSensitive;

  case C.Op of
    TCompareOp.Equal .. TCompareOp.GreaterOrEqual:
      begin
        RequireArity(C, 1);
        var Op := OpTextFor[C.Op];
        var V := Ctx.ValueOf(C.Operands[0]);
        if Negated then
          if IsOr then Q.OrWhereNot(Col, Op, V) else Q.WhereNot(Col, Op, V)
        else
          if IsOr then Q.OrWhere(Col, Op, V) else Q.Where(Col, Op, V);
      end;

    TCompareOp.Like, TCompareOp.NotLike:
      begin
        RequireArity(C, 1);
        var P := VarToStr(Ctx.ValueOf(C.Operands[0]));
        if (C.Op = TCompareOp.NotLike) <> Negated then
          if IsOr then Q.OrWhereNotLike(Col, P, CS) else Q.WhereNotLike(Col, P, CS)
        else
          if IsOr then Q.OrWhereLike(Col, P, CS) else Q.WhereLike(Col, P, CS);
      end;

    TCompareOp.StartsWith:
      begin
        RequireArity(C, 1);
        var V := VarToStr(Ctx.ValueOf(C.Operands[0]));
        if Negated then
          if IsOr then Q.OrWhereNotStarts(Col, V, CS) else Q.WhereNotStarts(Col, V, CS)
        else
          if IsOr then Q.OrWhereStarts(Col, V, CS) else Q.WhereStarts(Col, V, CS);
      end;

    TCompareOp.EndsWith:
      begin
        RequireArity(C, 1);
        var V := VarToStr(Ctx.ValueOf(C.Operands[0]));
        if Negated then
          if IsOr then Q.OrWhereNotEnds(Col, V, CS) else Q.WhereNotEnds(Col, V, CS)
        else
          if IsOr then Q.OrWhereEnds(Col, V, CS) else Q.WhereEnds(Col, V, CS);
      end;

    TCompareOp.Contains:
      begin
        RequireArity(C, 1);
        var V := VarToStr(Ctx.ValueOf(C.Operands[0]));
        if Negated then
          if IsOr then Q.OrWhereNotContains(Col, V, CS) else Q.WhereNotContains(Col, V, CS)
        else
          if IsOr then Q.OrWhereContains(Col, V, CS) else Q.WhereContains(Col, V, CS);
      end;

    TCompareOp.&In, TCompareOp.NotIn:
      begin
        if Length(C.Operands) = 0 then
          raise EExprLoweringError.Create('An IN condition needs at least one value.');
        var Vals := Ctx.ValuesOf(C.Operands);
        if (C.Op = TCompareOp.NotIn) <> Negated then
          if IsOr then Q.OrWhereNotIn(Col, Vals) else Q.WhereNotIn(Col, Vals)
        else
          if IsOr then Q.OrWhereIn(Col, Vals) else Q.WhereIn(Col, Vals);
      end;

    TCompareOp.&Between, TCompareOp.NotBetween:
      begin
        RequireArity(C, 2);
        var Lo := Ctx.ValueOf(C.Operands[0]);
        var Hi := Ctx.ValueOf(C.Operands[1]);
        if (C.Op = TCompareOp.NotBetween) <> Negated then
          if IsOr then Q.OrWhereNotBetween(Col, Lo, Hi) else Q.WhereNotBetween(Col, Lo, Hi)
        else
          if IsOr then Q.OrWhereBetween(Col, Lo, Hi) else Q.WhereBetween(Col, Lo, Hi);
      end;

    TCompareOp.IsNull, TCompareOp.IsNotNull:
      begin
        RequireArity(C, 0);
        if (C.Op = TCompareOp.IsNotNull) <> Negated then
          if IsOr then Q.OrWhereNotNull(Col) else Q.WhereNotNull(Col)
        else
          if IsOr then Q.OrWhereNull(Col) else Q.WhereNull(Col);
      end;
  end;
end;

// A parenthesized group. The callback is SQLCute's own nested-group verb, not a
// filter predicate — the prohibition on closures is about predicates a
// specification could smuggle past translation, and this one carries none.
procedure EmitGroup(const Node: IExpr; const Q: IQuery; const Ctx: TEmitCtx;
  Connector: TLogicalOp; Negated: Boolean);
begin
  var Build: TQueryBuilderCallback :=
    function(Sub: IQuery): IQuery
    begin
      // Recursion restarts the normalizer's First inside this group, so the head
      // element's connector is ignored here too.
      EmitTop(Node, Sub, Ctx);
      Result := Sub;
    end;

  if Negated then
    if Connector = TLogicalOp.opOr then Q.OrWhereNot(Build) else Q.WhereNot(Build)
  else
    if Connector = TLogicalOp.opOr then Q.OrWhere(Build) else Q.Where(Build);
end;

// One element of the clause list.
procedure EmitItem(const Node: IExpr; const Q: IQuery; const Ctx: TEmitCtx;
  Connector: TLogicalOp);
begin
  if Node = nil then
    raise EExprLoweringError.Create('An empty expression cannot be lowered.');

  case Node.Kind of
    TExprKind.Compare:
      EmitLeaf(Node as ICompareExpr, Q, Ctx, Connector, False);

    TExprKind.Unary:
      begin
        var Inner := (Node as IUnaryExpr).Operand;
        if (Inner <> nil) and (Inner.Kind = TExprKind.Compare) then
          // Prefer the leaf form for leaves: NOT (a = ?) reads worse than a <> ?
          // wrapped in the clause's own IsNot, and means the same thing.
          EmitLeaf(Inner as ICompareExpr, Q, Ctx, Connector, True)
        else
          // A negated subtree stays one negated group. No De Morgan pass: the
          // emitted SQL should read as what the developer wrote, not as a
          // distributed equivalent of it.
          EmitGroup(Inner, Q, Ctx, Connector, True);
      end;

    TExprKind.Logical:
      // Reaching here means a disjunction inside an AND-run, and it MUST become a
      // nested group. Inlined, the normalizer would read the list as
      // OR-of-AND-runs and regroup it into a different predicate.
      EmitGroup(Node, Q, Ctx, Connector, False);
  else
    raise EExprLoweringError.CreateFmt(
      'A %s node is not a predicate, so it cannot become a WHERE condition. '
      + 'Compare it against something.',
      [GetEnumName(TypeInfo(TExprKind), Ord(Node.Kind))]);
  end;
end;

// One operand of the OR-list: a run of AND-ed elements. The first carries the
// run's connector, the rest carry AND — which is exactly Rule 2's "an And inside
// an OR-list inlines".
procedure EmitAndRun(const Node: IExpr; const Q: IQuery; const Ctx: TEmitCtx;
  LeadConnector: TLogicalOp);
begin
  var Items := FlattenLogical(Node, TLogicalOp.opAnd);
  for var I := 0 to High(Items) do
    if I = 0 then
      EmitItem(Items[I], Q, Ctx, LeadConnector)
    else
      EmitItem(Items[I], Q, Ctx, TLogicalOp.opAnd);
end;

// The whole content of one clause list: an OR of AND-runs, which is the grammar
// the normalizer reads back.
procedure EmitTop(const Node: IExpr; const Q: IQuery; const Ctx: TEmitCtx);
begin
  if Node = nil then
    Exit;
  var Operands := FlattenLogical(Node, TLogicalOp.opOr);
  for var I := 0 to High(Operands) do
    if I = 0 then
      // The head element's connector is ignored by DoBuild. Nothing may depend on
      // it, so it is set to the neutral AND and means nothing.
      EmitAndRun(Operands[I], Q, Ctx, TLogicalOp.opAnd)
    else
      EmitAndRun(Operands[I], Q, Ctx, TLogicalOp.opOr);
end;

{ TExprLowering }

class function TExprLowering.Where(const Query: IQuery; const Expression: IExpr): IQuery;
begin
  Result := Where(Query, Expression, nil, nil);
end;

class function TExprLowering.Where(const Query: IQuery; const Expression: IExpr;
  const Names: INameResolution; EntityType: PTypeInfo): IQuery;
begin
  if Query = nil then
    raise EExprLoweringError.Create('An expression must be lowered onto a query.');
  var Ctx: TEmitCtx;
  Ctx.Names := Names;
  Ctx.EntityType := EntityType;
  EmitTop(Expression, Query, Ctx);
  Result := Query;
end;

class function TExprLowering.Apply<T>(const Query: IQuery;
  const Spec: TSpecification<T>; const Names: INameResolution): IQuery;
begin
  Result := Where(Query, Spec.Filter, Names, TypeInfo(T));

  for var Term in Spec.Order do
  begin
    var Col := ColumnFor(Names, TypeInfo(T), Term.Member);
    if Term.Descending then
      Result.OrderByDesc(Col)
    else
      Result.OrderBy(Col);
  end;

  // Zero means unset for both, so an unpaged rule adds no clause at all.
  if Spec.SkipCount > 0 then
    Result.Offset(Spec.SkipCount);
  if Spec.TakeCount > 0 then
    Result.Limit(Spec.TakeCount);
end;

end.
