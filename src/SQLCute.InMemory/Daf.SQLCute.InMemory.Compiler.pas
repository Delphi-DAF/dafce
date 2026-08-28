unit Daf.SQLCute.InMemory.Compiler;

{$SCOPEDENUMS ON}

{
  In-memory compiler (non-generic core). Consumes an IQuery AST — read-only,
  via IQuery.Clauses — and produces a TCompiledQuery: a reusable, element-type-
  agnostic execution plan for the single-source fast lane (no IRow, no catalog).

  The compiler:
    * fails fast when the query actually needs more than one source (JOIN,
      subquery, set operation, derived FROM) or a reshape the fast lane cannot
      express (SELECT projection, GROUP BY/HAVING);
    * builds the WHERE predicate from the SHARED Daf.SQLCute.WhereTree, so
      AND/OR precedence matches the SQL backend by construction;
    * evaluates comparisons with full ANSI three-valued logic:
      a comparison touching NULL is Unknown, not False; a row is kept only when
      the predicate is True.

  Values are the TValue currency: SQLCute's Variant bindings are
  converted to TValue once, here at compile time, never per row. The generic
  half (Daf.SQLCute.InMemory) supplies a TColumnGetter over T via cached RTTI,
  so this unit knows nothing about the element type.
}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.WhereTree;

type
  /// <summary>Raised when a query cannot run on the single-source fast lane.</summary>
  EInMemoryQueryError = class(Exception);

  /// <summary>SQL three-valued logic result.</summary>
  TTriState = (Unknown, No, Yes);

  /// <summary>Reads a (possibly qualified) column's value for the current element.</summary>
  TColumnGetter = reference to function(const Column: string): TValue;

  /// <summary>A tri-state predicate over a column-getter — element-agnostic.</summary>
  TValuePredicate = reference to function(const Get: TColumnGetter): TTriState;

  /// <summary>
  ///   Evaluates a subquery WHERE leaf (IN / NOT IN / EXISTS / NOT EXISTS) against
  ///   execution-time state. Supplied by the multi-source engine (which owns the
  ///   context); the compiler captures it while building the predicate.
  /// </summary>
  TSubqueryResolver = reference to function(const Clause: TWhereClause;
    const Get: TColumnGetter): TTriState;

  /// <summary>NULL-ordering policy for one ORDER BY column.</summary>
  TNullsPolicy = (Default, First, Last);

  /// <summary>One ORDER BY column: name, direction, NULL policy.</summary>
  TOrderSpec = record
    Column: string;
    Descending: Boolean;
    Nulls: TNullsPolicy;
  end;

  /// <summary>One projected (output) column: the source column and its output name.</summary>
  TProjColumn = record
    Source: string;   // underlying column read from the row
    Output: string;   // alias when present, otherwise the column name
  end;

  /// <summary>An aggregate function over a group's members.</summary>
  TAggFunc = (Count, Sum, Avg, Min, Max);

  /// <summary>One aggregate output column: function, argument column ('*' for COUNT(*)), output name.</summary>
  TAggregateSpec = record
    Func: TAggFunc;
    Column: string;
    Output: string;
  end;

  /// <summary>
  ///   Non-generic compiled plan for a single-source query. Reusable across
  ///   sources; carries no reference to the original AST (everything is
  ///   extracted at compile time).
  /// </summary>
  TCompiledQuery = class
  public
    Predicate: TValuePredicate;   // nil = no WHERE
    Orders: TArray<TOrderSpec>;
    Projection: TArray<TProjColumn>;   // nil = no reshape (all columns / preserve T)
    GroupBy: TArray<string>;           // nil = no grouping
    Aggregates: TArray<TAggregateSpec>;
    Having: TValuePredicate;           // nil = no HAVING (filters flat aggregate rows)
    Distinct: Boolean;
    HasLimit: Boolean;
    Limit: Int64;
    HasOffset: Boolean;
    Offset: Int64;
  end;

  /// <summary>Compiles an <c>IQuery</c> into a <c>TCompiledQuery</c>.</summary>
  TInMemoryCompiler = record
    /// <summary>
    ///   Builds the plan for the single-source lane. Raises
    ///   <see cref="EInMemoryQueryError"/> if the query needs the general
    ///   multi-source lane. The caller owns the result.
    /// </summary>
    class function Compile(const Query: IQuery): TCompiledQuery; static;
    /// <summary>
    ///   Same extraction without the single-source veto — for the general
    ///   multi-source lane, where FROM/JOIN resolution is handled separately.
    /// </summary>
    class function CompileMulti(const Query: IQuery): TCompiledQuery; static;
  end;

// --- Three-valued logic combinators ---------------------------
function BoolToTri(B: Boolean): TTriState;
// --- Value helpers (shared with the generic half) ---------------------------
/// <summary>Converts a SQLCute Variant binding to TValue; NULL/empty → empty TValue.</summary>
function VariantToValue(const V: Variant): TValue;
/// <summary>Non-NULL comparison of two values (both operands assumed present).</summary>
function CompareValues(const L, R: TValue): Integer;
/// <summary>Order comparison honoring direction and NULL policy.</summary>
function CompareForOrder(const L, R: TValue; const Spec: TOrderSpec): Integer;
/// <summary>
///   Canonical match key for a column vs a Delphi member: bare, lower-cased and
///   underscore-stripped, so SQL <c>user_id</c> matches member <c>UserId</c>
///   (snake_case ↔ PascalCase) — the naming bridge the symmetry goal needs.
/// </summary>
function NormalizeName(const Column: string): string;
/// <summary>Folds a value hash into an accumulator (wraparound; overflow-safe).</summary>
function CombineHash(Acc, Value: Integer): Integer;
/// <summary>3VL comparison of two values by operator (a NULL operand ⇒ Unknown).</summary>
function CompareOp(Op: TWhereOp; const L, R: TValue): TTriState;
/// <summary>
///   Builds a tri-state predicate from a flat WHERE/ON clause list via the shared
///   <c>Daf.SQLCute.WhereTree</c> (nil when there are no conditions). Reused by the
///   multi-source engine for JOIN ON callbacks and future HAVING.
/// </summary>
function WherePredicate(const Clauses: TArray<TAbstractClause>): TValuePredicate;
/// <summary>
///   Installs the subquery resolver used while building the next predicate(s);
///   pass <c>nil</c> to clear (subquery WHERE leaves then fail fast). Set by the
///   engine around <c>CompileMulti</c>; the built predicate captures it.
/// </summary>
procedure UseSubqueryResolver(const Resolver: TSubqueryResolver);

implementation

uses
  Daf.Comparers;   // IsNullValue

var
  // Subquery resolver in effect while a predicate is being built (see
  // UseSubqueryResolver). Captured by subquery leaves; not read at run time.
  FSubqueryResolver: TSubqueryResolver = nil;

procedure UseSubqueryResolver(const Resolver: TSubqueryResolver);
begin
  FSubqueryResolver := Resolver;
end;

{ ---- Three-valued logic ---- }

function BoolToTri(B: Boolean): TTriState;
begin
  if B then Result := TTriState.Yes else Result := TTriState.No;
end;

function TriNot(A: TTriState): TTriState;
begin
  case A of
    TTriState.Yes: Result := TTriState.No;
    TTriState.No:  Result := TTriState.Yes;
  else
    Result := TTriState.Unknown;
  end;
end;

function TriAnd(const A, B: TTriState): TTriState;
begin
  if (A = TTriState.No) or (B = TTriState.No) then
    Result := TTriState.No
  else if (A = TTriState.Unknown) or (B = TTriState.Unknown) then
    Result := TTriState.Unknown
  else
    Result := TTriState.Yes;
end;

function TriOr(const A, B: TTriState): TTriState;
begin
  if (A = TTriState.Yes) or (B = TTriState.Yes) then
    Result := TTriState.Yes
  else if (A = TTriState.Unknown) or (B = TTriState.Unknown) then
    Result := TTriState.Unknown
  else
    Result := TTriState.No;
end;

{ ---- Value helpers ---- }

function BareColumn(const Column: string): string;
begin
  var P := LastDelimiter('.', Column);
  if P > 0 then
    Result := Copy(Column, P + 1, MaxInt)
  else
    Result := Column;
end;

function NormalizeName(const Column: string): string;
begin
  Result := StringReplace(LowerCase(BareColumn(Column)), '_', '', [rfReplaceAll]);
end;

{$IFOPT Q+}{$DEFINE _RESTORE_Q}{$ENDIF}
{$IFOPT R+}{$DEFINE _RESTORE_R}{$ENDIF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
function CombineHash(Acc, Value: Integer): Integer;
begin
  Result := Acc * 31 + Value;   // intentional wraparound — a hash mix, not arithmetic
end;
{$IFDEF _RESTORE_R}{$RANGECHECKS ON}{$UNDEF _RESTORE_R}{$ENDIF}
{$IFDEF _RESTORE_Q}{$OVERFLOWCHECKS ON}{$UNDEF _RESTORE_Q}{$ENDIF}

function VariantToValue(const V: Variant): TValue;
begin
  if VarIsNull(V) or VarIsEmpty(V) then
    Result := TValue.Empty
  else
    Result := TValue.FromVariant(V);
end;

function CompareValues(const L, R: TValue): Integer;
var
  VL, VR: Variant;
begin
  try
    VL := L.AsVariant;
    VR := R.AsVariant;
  except
    // Not Variant-convertible (objects/records): stable textual fallback.
    Exit(CompareStr(L.ToString, R.ToString));
  end;
  case VarCompareValue(VL, VR) of
    vrLessThan:    Result := -1;
    vrGreaterThan: Result := 1;
    vrEqual:       Result := 0;
  else
    Result := CompareStr(VarToStr(VL), VarToStr(VR));
  end;
end;

function CompareForOrder(const L, R: TValue; const Spec: TOrderSpec): Integer;
var
  LNull, RNull, NullsFirst: Boolean;
begin
  LNull := IsNullValue(L);
  RNull := IsNullValue(R);
  if LNull and RNull then
    Exit(0);
  if LNull or RNull then
  begin
    // Default: NULL is the smallest value — nulls-first ASC, nulls-last DESC.
    case Spec.Nulls of
      TNullsPolicy.First: NullsFirst := True;
      TNullsPolicy.Last:  NullsFirst := False;
    else
      NullsFirst := not Spec.Descending;
    end;
    if LNull then
      if NullsFirst then Exit(-1) else Exit(1)
    else
      if NullsFirst then Exit(1) else Exit(-1);
  end;
  Result := CompareValues(L, R);
  if Spec.Descending then
    Result := -Result;
end;

function SqlLikeMatch(const S, P: string): Boolean;
var
  Si, Pi, Star, Mark: Integer;
begin
  // Classic backtracking wildcard matcher: '%' = any run, '_' = one char.
  Si := 1; Pi := 1; Star := 0; Mark := 0;
  while Si <= Length(S) do
  begin
    if (Pi <= Length(P)) and ((P[Pi] = '_') or (P[Pi] = S[Si])) then
    begin
      Inc(Si); Inc(Pi);
    end
    else if (Pi <= Length(P)) and (P[Pi] = '%') then
    begin
      Star := Pi; Mark := Si; Inc(Pi);
    end
    else if Star <> 0 then
    begin
      Pi := Star + 1; Inc(Mark); Si := Mark;
    end
    else
      Exit(False);
  end;
  while (Pi <= Length(P)) and (P[Pi] = '%') do
    Inc(Pi);
  Result := Pi > Length(P);
end;

{ ---- Leaf comparison ---- }

function CompareOp(Op: TWhereOp; const L, R: TValue): TTriState;
begin
  if IsNullValue(L) or IsNullValue(R) then
    Exit(TTriState.Unknown);   // 3VL: any NULL operand ⇒ Unknown
  var C := CompareValues(L, R);
  case Op of
    TWhereOp.Equal:          Result := BoolToTri(C = 0);
    TWhereOp.NotEqual:       Result := BoolToTri(C <> 0);
    TWhereOp.Less:           Result := BoolToTri(C < 0);
    TWhereOp.LessOrEqual:    Result := BoolToTri(C <= 0);
    TWhereOp.Greater:        Result := BoolToTri(C > 0);
    TWhereOp.GreaterOrEqual: Result := BoolToTri(C >= 0);
  else
    Result := TTriState.Unknown;
  end;
end;

{ ---- Leaf builders — extract everything at compile time into closures ---- }

function BuildComparison(W: TWhereClause): TValuePredicate;
begin
  var Col := W.Column;
  var Op := W.Op;
  if W.IsColumnValue then
  begin
    var Col2 := VarToStr(W.Value);
    Result :=
      function(const Get: TColumnGetter): TTriState
      begin
        Result := CompareOp(Op, Get(Col), Get(Col2));
      end;
  end
  else
  begin
    var Bound := VariantToValue(W.Value);
    Result :=
      function(const Get: TColumnGetter): TTriState
      begin
        Result := CompareOp(Op, Get(Col), Bound);
      end;
  end;
end;

function BuildNullCheck(W: TWhereClause): TValuePredicate;
begin
  var Col := W.Column;
  var WantNull := (W.Op = TWhereOp.IsNull);
  Result :=
    function(const Get: TColumnGetter): TTriState
    begin
      // IS NULL / IS NOT NULL always yield True/False, never Unknown.
      Result := BoolToTri(IsNullValue(Get(Col)) = WantNull);
    end;
end;

function BuildBetween(W: TWhereClause): TValuePredicate;
begin
  var Col := W.Column;
  var Lo := VariantToValue(W.Value);
  var Hi := VariantToValue(W.Value2);
  var Negate := (W.Op = TWhereOp.NotBetween);
  Result :=
    function(const Get: TColumnGetter): TTriState
    begin
      var V := Get(Col);
      if IsNullValue(V) then
        Exit(TTriState.Unknown);
      var R := TriAnd(CompareOp(TWhereOp.GreaterOrEqual, V, Lo),
                      CompareOp(TWhereOp.LessOrEqual, V, Hi));
      if Negate then
        Result := TriNot(R)
      else
        Result := R;
    end;
end;

function BuildLike(W: TWhereClause): TValuePredicate;
begin
  var Col := W.Column;
  // The builder already lower-cased the pattern when CaseSensitive = False.
  var Pattern := VarToStr(W.Value);
  var CaseSensitive := W.CaseSensitive;
  var Negate := (W.Op = TWhereOp.NotLike);
  Result :=
    function(const Get: TColumnGetter): TTriState
    begin
      var V := Get(Col);
      if IsNullValue(V) then
        Exit(TTriState.Unknown);
      var S := VarToStr(V.AsVariant);
      if not CaseSensitive then
        S := LowerCase(S);
      var M := SqlLikeMatch(S, Pattern);
      if Negate then
        Result := BoolToTri(not M)
      else
        Result := BoolToTri(M);
    end;
end;

function BuildIn(WI: TWhereInClause): TValuePredicate;
begin
  var Col := WI.Column;
  var Negate := WI.Negated;
  var Vals: TArray<TValue>;
  SetLength(Vals, Length(WI.Values));
  for var I := 0 to High(WI.Values) do
    Vals[I] := VariantToValue(WI.Values[I]);
  Result :=
    function(const Get: TColumnGetter): TTriState
    begin
      var V := Get(Col);
      if IsNullValue(V) then
        Exit(TTriState.Unknown);
      var AnyNull := False;
      for var X in Vals do
        if IsNullValue(X) then
          AnyNull := True
        else if CompareValues(V, X) = 0 then
          Exit(BoolToTri(not Negate));   // match: IN ⇒ True, NOT IN ⇒ False
      if AnyNull then
        Exit(TTriState.Unknown);
      Result := BoolToTri(Negate);       // no match: IN ⇒ False, NOT IN ⇒ True
    end;
end;

function BuildLeaf(Clause: TAbstractClause): TValuePredicate;
begin
  if Clause is TWhereInClause then
    Exit(BuildIn(TWhereInClause(Clause)));
  if Clause is TDateWhereClause then
    raise EInMemoryQueryError.Create(
      'Date/time WHERE conditions are not supported in the single-source fast path.');
  if Clause is THavingClause then
  begin
    // HAVING leaf: compares an aggregate/output column against a bound value.
    var H := THavingClause(Clause);
    if H.Op = TWhereOp.Raw then
      raise EInMemoryQueryError.Create('Raw HAVING expressions cannot be evaluated in memory.');
    var HCol := H.Column;
    var HOp := H.Op;
    var HBound := VariantToValue(H.Value);
    Exit(
      function(const Get: TColumnGetter): TTriState
      begin
        Result := CompareOp(HOp, Get(HCol), HBound);
      end);
  end;
  if Clause is TWhereClause then
  begin
    var W := TWhereClause(Clause);
    case W.Op of
      TWhereOp.IsNull, TWhereOp.IsNotNull:
        Exit(BuildNullCheck(W));
      TWhereOp.&Between, TWhereOp.NotBetween:
        Exit(BuildBetween(W));
      TWhereOp.Like, TWhereOp.NotLike:
        Exit(BuildLike(W));
      TWhereOp.Equal, TWhereOp.NotEqual, TWhereOp.Less, TWhereOp.LessOrEqual,
      TWhereOp.Greater, TWhereOp.GreaterOrEqual:
        Exit(BuildComparison(W));
      TWhereOp.Raw:
        raise EInMemoryQueryError.Create('Raw WHERE fragments cannot be evaluated in memory.');
      TWhereOp.&In, TWhereOp.NotIn, TWhereOp.&Exists, TWhereOp.NotExists:
        if (W.SubQuery <> nil) and Assigned(FSubqueryResolver) then
        begin
          var Resolver: TSubqueryResolver := FSubqueryResolver;   // explicit type: assign, don't invoke
          var WC := W;
          Exit(
            function(const Get: TColumnGetter): TTriState
            begin
              Result := Resolver(WC, Get);
            end);
        end
        else
          raise EInMemoryQueryError.Create(
            'Subquery / EXISTS WHERE conditions require a query context.');
    else
      raise EInMemoryQueryError.Create('Unsupported WHERE operator in the single-source fast path.');
    end;
  end;
  raise EInMemoryQueryError.Create('Unsupported WHERE clause in the single-source fast path.');
end;

{ ---- Tree walk (precedence from the shared WhereTree) ---- }

function BuildNode(Node: TWhereNode): TValuePredicate; forward;

function BuildChildren(const Children: TArray<TWhereNode>): TArray<TValuePredicate>;
begin
  SetLength(Result, Length(Children));
  for var I := 0 to High(Children) do
    Result[I] := BuildNode(Children[I]);
end;

function BuildNode(Node: TWhereNode): TValuePredicate;
begin
  case Node.Kind of
    TWhereNodeKind.Leaf:
      Result := BuildLeaf(Node.Clause);
    TWhereNodeKind.Neg:
      begin
        var Child := BuildNode(Node.Children[0]);
        Result :=
          function(const Get: TColumnGetter): TTriState
          begin
            Result := TriNot(Child(Get));
          end;
      end;
    TWhereNodeKind.Conjunction:
      begin
        var Kids := BuildChildren(Node.Children);
        Result :=
          function(const Get: TColumnGetter): TTriState
          begin
            Result := TTriState.Yes;
            for var K in Kids do
              Result := TriAnd(Result, K(Get));
          end;
      end;
    TWhereNodeKind.Disjunction:
      begin
        var Kids := BuildChildren(Node.Children);
        Result :=
          function(const Get: TColumnGetter): TTriState
          begin
            Result := TTriState.No;
            for var K in Kids do
              Result := TriOr(Result, K(Get));
          end;
      end;
  else
    Result := nil;
  end;
end;

function WherePredicate(const Clauses: TArray<TAbstractClause>): TValuePredicate;
begin
  var Root := TWhereTree.Build(Clauses);
  try
    if Root = nil then
      Result := nil
    else
      Result := BuildNode(Root);   // extracted data captured; the tree is freed below
  finally
    Root.Free;
  end;
end;

// HAVING predicate over the flat aggregate rows (same tree/precedence as WHERE).
function HavingPredicate(const Clauses: TArray<TAbstractClause>): TValuePredicate;
begin
  var Root := TWhereTree.BuildHaving(Clauses);
  try
    if Root = nil then
      Result := nil
    else
      Result := BuildNode(Root);
  finally
    Root.Free;
  end;
end;

// Parses a raw aggregate select expression "FUNC(col) AS alias" into a spec.
// Returns False when it is not a recognised aggregate.
function TryParseAggregate(const Raw: string; out Spec: TAggregateSpec): Boolean;
begin
  Result := False;
  var Expr := Trim(Raw);
  var Alias := '';
  var AsPos := Pos(' AS ', UpperCase(Expr));
  if AsPos > 0 then
  begin
    Alias := Trim(Copy(Expr, AsPos + 4, MaxInt));
    Expr := Trim(Copy(Expr, 1, AsPos - 1));
  end;
  var Paren := Pos('(', Expr);
  var LastParen := 0;
  for var I := Length(Expr) downto 1 do
    if Expr[I] = ')' then
    begin
      LastParen := I;
      Break;
    end;
  if (Paren = 0) or (LastParen <= Paren) then
    Exit;
  var FuncName := UpperCase(Trim(Copy(Expr, 1, Paren - 1)));
  var Col := Trim(Copy(Expr, Paren + 1, LastParen - Paren - 1));
  if FuncName = 'COUNT' then Spec.Func := TAggFunc.Count
  else if FuncName = 'SUM' then Spec.Func := TAggFunc.Sum
  else if FuncName = 'AVG' then Spec.Func := TAggFunc.Avg
  else if FuncName = 'MIN' then Spec.Func := TAggFunc.Min
  else if FuncName = 'MAX' then Spec.Func := TAggFunc.Max
  else
    Exit;
  Spec.Column := Col;
  if Alias <> '' then
    Spec.Output := Alias
  else
    Spec.Output := LowerCase(FuncName);
  Result := True;
end;

function ExtractGroupBy(const Clauses: TArray<TAbstractClause>): TArray<string>;
begin
  Result := nil;
  for var C in Clauses do
    if C is TGroupByClause then
      Result := Result + [TGroupByClause(C).Column];
end;

function ExtractAggregates(const Clauses: TArray<TAbstractClause>): TArray<TAggregateSpec>;
begin
  Result := nil;
  for var C in Clauses do
    if C is TSelectClause then
      for var Col in TSelectClause(C).Columns do
        if Col.IsRaw then
        begin
          var Spec: TAggregateSpec;
          if TryParseAggregate(Col.Column, Spec) then
            Result := Result + [Spec];
        end;
end;

{ ---- Source / shape analysis (fail-fast) ---- }

// Extracts the SELECT projection as (source -> output) columns. A lone '*'
// means "all columns" (no reshape -> nil). Raw / subquery SELECT columns need
// the general lane and fail fast here.
function ExtractProjection(const Clauses: TArray<TAbstractClause>): TArray<TProjColumn>;
begin
  Result := nil;
  // A grouped query's output is the flat aggregate row (group keys + aggregate
  // columns) built by the engine — there is no separate column projection.
  for var C in Clauses do
    if C is TGroupByClause then
      Exit(nil);
  for var C in Clauses do
    if C is TSelectClause then
      for var Col in TSelectClause(C).Columns do
      begin
        if (Col.Column = '*') and (Col.Alias = '') and (not Col.IsRaw) and (Col.SubQuery = nil) then
          Continue;   // '*' — no explicit projection entry
        if Col.IsRaw or (Col.SubQuery <> nil) then
          raise EInMemoryQueryError.Create(
            'Raw / subquery SELECT columns require the general lane (Phase 3).');
        var P: TProjColumn;
        P.Source := Col.Column;
        if Col.Alias <> '' then
          P.Output := Col.Alias
        else
          P.Output := Col.Column;
        Result := Result + [P];
      end;
end;

procedure EnsureSingleSource(const Clauses: TArray<TAbstractClause>);
begin
  for var C in Clauses do
  begin
    if (C is TJoinClause) or (C is TJoinSubqueryClause) then
      raise EInMemoryQueryError.Create(
        'This query joins multiple sources; execute it with a query context '
        + '(the general multi-source lane, Phase 3), not a single list.');
    if C is TUnionClause then
      raise EInMemoryQueryError.Create(
        'Set operations (UNION / INTERSECT / EXCEPT) require the multi-source lane (Phase 3).');
    if (C is TFromSubqueryClause) or (C is TFromRawClause) then
      raise EInMemoryQueryError.Create(
        'Derived-table / raw FROM sources require a query context (Phase 3).');
    if C is TWithClause then
      raise EInMemoryQueryError.Create('CTEs (WITH) require the multi-source lane (Phase 3).');
    if (C is TGroupByClause) or (C is THavingClause) then
      raise EInMemoryQueryError.Create(
        'GROUP BY / HAVING aggregation requires the general lane (Phase 3).');
    if (C is TInsertClause) or (C is TUpdateSetClause) or (C is TDeleteClause) then
      raise EInMemoryQueryError.Create('The in-memory backend is read-only; DML stays SQL-only.');
    if C is TWhereClause then
    begin
      var W := TWhereClause(C);
      if (W.SubQuery <> nil)
         or (W.Op = TWhereOp.&Exists) or (W.Op = TWhereOp.NotExists) then
        raise EInMemoryQueryError.Create(
          'Subquery / EXISTS WHERE conditions require a query context (Phase 3).');
    end;
    if (C is TOrderByClause) and TOrderByClause(C).IsRaw then
      raise EInMemoryQueryError.Create('Raw ORDER BY expressions cannot be evaluated in memory.');
  end;
end;

{ ---- Extractors ---- }

function ExtractOrders(const Clauses: TArray<TAbstractClause>): TArray<TOrderSpec>;
begin
  Result := nil;
  for var C in Clauses do
    if C is TOrderByClause then
    begin
      var O := TOrderByClause(C);
      var S: TOrderSpec;
      S.Column := O.Column;
      S.Descending := (O.Direction = TSortDir.Desc);
      // The AST's SQL vocabulary maps onto the plan's own.
      case O.Nulls of
        TNullsOrder.First: S.Nulls := TNullsPolicy.First;
        TNullsOrder.Last:  S.Nulls := TNullsPolicy.Last;
      else
        S.Nulls := TNullsPolicy.Default;
      end;
      Result := Result + [S];
    end;
end;

procedure ExtractLimits(const Clauses: TArray<TAbstractClause>; const Target: TCompiledQuery);
begin
  for var C in Clauses do
  begin
    if C is TLimitClause then
    begin
      Target.HasLimit := True;
      Target.Limit := TLimitClause(C).Value;
    end;
    if C is TOffsetClause then
    begin
      Target.HasOffset := True;
      Target.Offset := TOffsetClause(C).Value;
    end;
  end;
end;

function HasDistinct(const Clauses: TArray<TAbstractClause>): Boolean;
begin
  Result := False;
  for var C in Clauses do
    if C is TDistinctClause then
      Exit(True);
end;

// Shared extraction: WHERE predicate, ORDER BY, projection, DISTINCT, LIMIT/OFFSET.
// Source shape (single vs multi) is vetoed by the callers, not here.
function Populate(const Clauses: TArray<TAbstractClause>): TCompiledQuery;
begin
  Result := TCompiledQuery.Create;
  try
    Result.Predicate := WherePredicate(Clauses);
    Result.Orders := ExtractOrders(Clauses);
    Result.Projection := ExtractProjection(Clauses);
    Result.GroupBy := ExtractGroupBy(Clauses);
    Result.Aggregates := ExtractAggregates(Clauses);
    Result.Having := HavingPredicate(Clauses);
    Result.Distinct := HasDistinct(Clauses);
    ExtractLimits(Clauses, Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TInMemoryCompiler }

class function TInMemoryCompiler.Compile(const Query: IQuery): TCompiledQuery;
begin
  var Clauses := Query.Clauses;
  EnsureSingleSource(Clauses);       // fail fast before allocating the plan
  Result := Populate(Clauses);
end;

class function TInMemoryCompiler.CompileMulti(const Query: IQuery): TCompiledQuery;
begin
  // No single-source veto: FROM/JOIN resolution is the engine's job.
  Result := Populate(Query.Clauses);
end;

end.
