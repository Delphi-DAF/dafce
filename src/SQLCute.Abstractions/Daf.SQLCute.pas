unit Daf.SQLCute;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Math,
  System.Generics.Collections,
  Daf.SQLCute.Clauses;

type
  /// <summary>
  /// Result of a compiled query: the SQL string and its ordered bindings.
  /// '?' placeholders in SQL map positionally to Bindings elements.
  /// Dialect compilers may use different placeholder styles (e.g. $1, :p1).
  /// </summary>
  TSQLResult = record
    SQL: string;
    Bindings: TArray<Variant>;
    class function Empty: TSQLResult; static;
  end;

  // ---------------------------------------------------------------------------
  //  IQuery  —  fluent query builder interface
  // ---------------------------------------------------------------------------

  IQueryCompiler = interface;  // forward
  IQuery = interface;           // forward

  /// <summary>
  /// Callback type for nested WHERE groups and When clauses.
  /// The callback receives a fresh IQuery, adds conditions, and returns it.
  /// </summary>
  TQueryBuilderCallback = reference to function(Q: IQuery): IQuery;

  /// <summary>
  /// Fluent interface for building SQL queries.
  /// Every method returns Self (as IQuery) for method chaining.
  /// Obtain an instance via TQuery.New — lifetime is ref-counted, no Free needed.
  /// </summary>
  IQuery = interface
    ['{B3C4D5E6-F780-9ABC-DEF0-123456789ABC}']

    // --- SELECT ---------------------------------------------------------

    /// <summary>Adds one or more columns to the SELECT list.</summary>
    function Select(const Columns: TArray<string>): IQuery; overload;
    /// <summary>Adds a single column to the SELECT list.</summary>
    function Select(const Column: string): IQuery; overload;
    /// <summary>Adds a raw SQL expression to the SELECT list.</summary>
    function SelectRaw(const Expression: string): IQuery;

    // --- FROM -----------------------------------------------------------

    /// <summary>Sets the FROM table.</summary>
    function From(const Table: string): IQuery; overload;
    /// <summary>Sets the FROM table with an alias.</summary>
    function From(const Table, Alias: string): IQuery; overload;

    // --- WHERE ----------------------------------------------------------

    /// <summary>Adds an AND WHERE column = value condition.</summary>
    function Where(const Column: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an AND WHERE column {op} value condition.</summary>
    function Where(const Column, Op: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an OR WHERE column = value condition.</summary>
    function OrWhere(const Column: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an OR WHERE column {op} value condition.</summary>
    function OrWhere(const Column, Op: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds a WHERE column IS NULL condition.</summary>
    function WhereNull(const Column: string): IQuery;
    /// <summary>Adds a WHERE column IS NOT NULL condition.</summary>
    function WhereNotNull(const Column: string): IQuery;
    /// <summary>Adds an OR WHERE column IS NULL condition.</summary>
    function OrWhereNull(const Column: string): IQuery;
    /// <summary>Adds an OR WHERE column IS NOT NULL condition.</summary>
    function OrWhereNotNull(const Column: string): IQuery;
    /// <summary>Adds a WHERE column = True condition.</summary>
    function WhereTrue(const Column: string): IQuery;
    /// <summary>Adds a WHERE column = False condition.</summary>
    function WhereFalse(const Column: string): IQuery;
    /// <summary>Adds a WHERE NOT (column = value) condition.</summary>
    function WhereNot(const Column: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds a WHERE NOT (column {op} value) condition.</summary>
    function WhereNot(const Column, Op: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an OR WHERE NOT (column = value) condition.</summary>
    function OrWhereNot(const Column: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an OR WHERE NOT (column {op} value) condition.</summary>
    function OrWhereNot(const Column, Op: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds a WHERE column BETWEEN low AND high condition.</summary>
    function WhereBetween(const Column: string; const Low, High: Variant): IQuery;
    /// <summary>Adds a WHERE column NOT BETWEEN low AND high condition.</summary>
    function WhereNotBetween(const Column: string; const Low, High: Variant): IQuery;
    /// <summary>Adds an OR WHERE column BETWEEN low AND high condition.</summary>
    function OrWhereBetween(const Column: string; const Low, High: Variant): IQuery;
    /// <summary>Adds an OR WHERE column NOT BETWEEN low AND high condition.</summary>
    function OrWhereNotBetween(const Column: string; const Low, High: Variant): IQuery;
    /// <summary>Appends a raw SQL WHERE fragment (AND connector).</summary>
    function WhereRaw(const Sql: string): IQuery;

    // --- WHERE (grouped / column-column / conditional) ------------------

    /// <summary>
    /// Adds a parenthesized AND-connected WHERE group.
    /// The callback receives a fresh IQuery, adds conditions, and returns it.
    /// </summary>
    function Where(const Callback: TQueryBuilderCallback): IQuery; overload;
    /// <summary>Adds a parenthesized OR-connected WHERE group.</summary>
    function OrWhere(const Callback: TQueryBuilderCallback): IQuery; overload;

    /// <summary>
    /// Adds WHERE col1 op col2 (column-to-column comparison, no binding).
    /// NOTE: col1 and col2 are emitted verbatim — pass developer-controlled names only.
    /// </summary>
    function WhereColumns(const Col1, Op, Col2: string): IQuery;
    /// <summary>Adds OR WHERE col1 op col2 (column-to-column comparison).</summary>
    function OrWhereColumns(const Col1, Op, Col2: string): IQuery;

    // --- WHERE (string / LIKE operations) --------------------------------

    /// <summary>Adds WHERE col LIKE pattern. If CaseSensitive=False (default),
    /// emits LOWER(col) LIKE lower(pattern).</summary>
    function WhereLike(const Column, Pattern: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds WHERE col NOT LIKE pattern.</summary>
    function WhereNotLike(const Column, Pattern: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col LIKE pattern.</summary>
    function OrWhereLike(const Column, Pattern: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col NOT LIKE pattern.</summary>
    function OrWhereNotLike(const Column, Pattern: string; CaseSensitive: Boolean = False): IQuery;

    /// <summary>Adds WHERE col LIKE 'pattern%' (starts with).</summary>
    function WhereStarts(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds WHERE col NOT LIKE 'pattern%'.</summary>
    function WhereNotStarts(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col LIKE 'pattern%'.</summary>
    function OrWhereStarts(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col NOT LIKE 'pattern%'.</summary>
    function OrWhereNotStarts(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;

    /// <summary>Adds WHERE col LIKE '%pattern' (ends with).</summary>
    function WhereEnds(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds WHERE col NOT LIKE '%pattern'.</summary>
    function WhereNotEnds(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col LIKE '%pattern'.</summary>
    function OrWhereEnds(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col NOT LIKE '%pattern'.</summary>
    function OrWhereNotEnds(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;

    /// <summary>Adds WHERE col LIKE '%pattern%' (contains).</summary>
    function WhereContains(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds WHERE col NOT LIKE '%pattern%'.</summary>
    function WhereNotContains(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col LIKE '%pattern%'.</summary>
    function OrWhereContains(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;
    /// <summary>Adds OR WHERE col NOT LIKE '%pattern%'.</summary>
    function OrWhereNotContains(const Column, Value: string; CaseSensitive: Boolean = False): IQuery;

    // --- WHERE (date / time operations) ---------------------------------

    /// <summary>Adds WHERE DATE(col) = value (exact date match, AND connector).</summary>
    function WhereDate(const Column: string; const Value: Variant): IQuery;
    /// <summary>Adds OR WHERE DATE(col) = value.</summary>
    function OrWhereDate(const Column: string; const Value: Variant): IQuery;
    /// <summary>Adds WHERE TIME(col) {op} value (time comparison, AND connector).</summary>
    function WhereTime(const Column, Op: string; const Value: Variant): IQuery;
    /// <summary>Adds OR WHERE TIME(col) {op} value.</summary>
    function OrWhereTime(const Column, Op: string; const Value: Variant): IQuery;
    /// <summary>Adds WHERE EXTRACT(part FROM col) = value (AND connector).</summary>
    function WhereDatePart(Part: TDatePart; const Column: string; const Value: Variant): IQuery;
    /// <summary>Adds OR WHERE EXTRACT(part FROM col) = value.</summary>
    function OrWhereDatePart(Part: TDatePart; const Column: string; const Value: Variant): IQuery;

    /// <summary>
    /// If Condition is True, invokes TrueCallback and merges its WHERE clauses.
    /// If False, the query is unchanged. Returns Self for chaining.
    /// </summary>
    function When(const Condition: Boolean;
      const TrueCallback: TQueryBuilderCallback): IQuery; overload;
    /// <summary>
    /// If Condition is True, invokes TrueCallback; otherwise invokes FalseCallback.
    /// Returns Self for chaining.
    /// </summary>
    function When(const Condition: Boolean;
      const TrueCallback: TQueryBuilderCallback;
      const FalseCallback: TQueryBuilderCallback): IQuery; overload;

    // --- ORDER BY -------------------------------------------------------

    /// <summary>Adds an ORDER BY column ASC clause.</summary>
    function OrderBy(const Column: string): IQuery;
    /// <summary>Adds an ORDER BY column DESC clause.</summary>
    function OrderByDesc(const Column: string): IQuery;
    /// <summary>Adds a raw ORDER BY expression.</summary>
    function OrderByRaw(const Expression: string): IQuery;

    // --- LIMIT / OFFSET -------------------------------------------------

    /// <summary>Sets the LIMIT clause.</summary>
    function Limit(const Value: Int64): IQuery;
    /// <summary>Sets the OFFSET clause.</summary>
    function Offset(const Value: Int64): IQuery;
    /// <summary>Alias for Limit. Sets the LIMIT clause.</summary>
    function Take(const Value: Int64): IQuery;
    /// <summary>Alias for Offset. Sets the OFFSET clause.</summary>
    function Skip(const Value: Int64): IQuery;
    /// <summary>
    /// Paginates results: sets LIMIT = PerPage and OFFSET = (Page-1) * PerPage.
    /// Page is 1-based (same as SqlKata).
    /// </summary>
    function ForPage(const Page: Integer; const PerPage: Integer = 15): IQuery;

    // --- JOIN -----------------------------------------------------------

    function Join(const Table, Col1, Col2: string; const Op: string = '='): IQuery; overload;
    function Join(const Table, Condition: string): IQuery; overload;
    function Join(const Table: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function Join(const SubQuery: IQuery; const Alias: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function LeftJoin(const Table, Col1, Col2: string; const Op: string = '='): IQuery; overload;
    function LeftJoin(const Table, Condition: string): IQuery; overload;
    function LeftJoin(const Table: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function LeftJoin(const SubQuery: IQuery; const Alias: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function RightJoin(const Table, Col1, Col2: string; const Op: string = '='): IQuery; overload;
    function RightJoin(const Table, Condition: string): IQuery; overload;
    function RightJoin(const Table: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function RightJoin(const SubQuery: IQuery; const Alias: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function CrossJoin(const Table: string): IQuery;
    function FullOuterJoin(const Table, Col1, Col2: string; const Op: string = '='): IQuery; overload;
    function FullOuterJoin(const Table, Condition: string): IQuery; overload;
    function FullOuterJoin(const Table: string; const Callback: TQueryBuilderCallback): IQuery; overload;
    function FullOuterJoin(const SubQuery: IQuery; const Alias: string; const Callback: TQueryBuilderCallback): IQuery; overload;

    function Distinct: IQuery;

    // --- FROM (sub-query overload) --------------------------------------

    function From(const SubQuery: IQuery; const Alias: string): IQuery; overload;
    /// <summary>Sets the FROM clause to a raw SQL expression (e.g. table function).</summary>
    function FromRaw(const Sql: string; const Bindings: TArray<Variant>; const Alias: string = ''): IQuery;

    // --- GROUP BY / HAVING ----------------------------------------------

    function GroupBy(const Column: string): IQuery; overload;
    function GroupBy(const Columns: TArray<string>): IQuery; overload;
    function GroupByRaw(const Expression: string): IQuery;
    /// <summary>Adds a HAVING condition. Column may be an aggregate expression.</summary>
    function Having(const Column, Op: string; const Value: Variant): IQuery;
    function HavingRaw(const Sql: string): IQuery;

    // --- UNION / INTERSECT / EXCEPT -------------------------------------

    function Union(const Other: IQuery): IQuery;
    function UnionAll(const Other: IQuery): IQuery;
    function Intersect(const Other: IQuery): IQuery;
    function &Except(const Other: IQuery): IQuery;
    function IntersectAll(const Other: IQuery): IQuery;
    function ExceptAll(const Other: IQuery): IQuery;
    function CombineRaw(const Sql: string): IQuery;

    // --- WHERE (sub-query variants) -------------------------------------

    function WhereExists(const SubQuery: IQuery): IQuery;
    function WhereNotExists(const SubQuery: IQuery): IQuery;
    function OrWhereExists(const SubQuery: IQuery): IQuery;
    function OrWhereNotExists(const SubQuery: IQuery): IQuery;
    function WhereInQuery(const Column: string; const SubQuery: IQuery): IQuery;
    function WhereNotInQuery(const Column: string; const SubQuery: IQuery): IQuery;
    function OrWhereInQuery(const Column: string; const SubQuery: IQuery): IQuery;
    function OrWhereNotInQuery(const Column: string; const SubQuery: IQuery): IQuery;

    // --- WHERE IN / NOT IN (value-array variants) -----------------------

    /// <summary>Adds WHERE col IN (?, ?, ?) with AND connector.</summary>
    function WhereIn(const Column: string; const Values: TArray<Variant>): IQuery;
    /// <summary>Adds WHERE col NOT IN (?, ?, ?) with AND connector.</summary>
    function WhereNotIn(const Column: string; const Values: TArray<Variant>): IQuery;
    /// <summary>Adds WHERE col IN (...) with OR connector.</summary>
    function OrWhereIn(const Column: string; const Values: TArray<Variant>): IQuery;
    /// <summary>Adds WHERE col NOT IN (...) with OR connector.</summary>
    function OrWhereNotIn(const Column: string; const Values: TArray<Variant>): IQuery;

    // --- SELECT (aliases + aggregates) ----------------------------------

    /// <summary>Adds a column with an explicit alias to the SELECT list.</summary>
    function SelectAs(const Column, Alias: string): IQuery;
    function SelectCount(const Column: string = '*'; const Alias: string = 'count'): IQuery;
    function SelectSum(const Column: string; const Alias: string = 'sum'): IQuery;
    function SelectAvg(const Column: string; const Alias: string = 'avg'): IQuery;
    function SelectMin(const Column: string; const Alias: string = 'min'): IQuery;
    function SelectMax(const Column: string; const Alias: string = 'max'): IQuery;
    /// <summary>Adds a subquery as a derived column: (SELECT …) AS alias.</summary>
    function Select(const SubQuery: IQuery; const Alias: string): IQuery; overload;

    // --- WITH (CTE) -----------------------------------------------------

    function &With(const Name: string; const SubQuery: IQuery): IQuery;
    function WithRecursive(const Name: string; const SubQuery: IQuery): IQuery;
    /// <summary>Adds a CTE using a raw SQL string.</summary>
    function WithRaw(const Name, Sql: string; const Bindings: TArray<Variant>): IQuery;

    // --- DML ------------------------------------------------------------

    /// <summary>Prepares a single-row INSERT. Use From to set the table.</summary>
    function AsInsert(const Columns: TArray<string>; const Values: TArray<Variant>): IQuery;
    /// <summary>Prepares a multi-row INSERT.</summary>
    function AsInsertRows(const Columns: TArray<string>; const Rows: TArray<TArray<Variant>>): IQuery;
    /// <summary>Prepares an INSERT … SELECT statement.</summary>
    function AsInsertFrom(const Columns: TArray<string>; const SubQuery: IQuery): IQuery;
    /// <summary>Prepares an UPDATE SET from parallel column/value arrays.</summary>
    function AsUpdate(const Columns: TArray<string>; const Values: TArray<Variant>): IQuery;
    /// <summary>Marks this query as a DELETE statement.</summary>
    function AsDelete: IQuery;

    // --- FORK / CLONE ---------------------------------------------------

    /// <summary>
    /// Returns a deep copy of this query.
    /// Useful for building query variants from a common base.
    /// </summary>
    function Clone: IQuery;

    // --- COMPILE --------------------------------------------------------

    /// <summary>
    /// Convenience: compiles this query using the provided compiler.
    /// Equivalent to Compiler.Compile(Self).
    /// </summary>
    function Compile(const Compiler: IQueryCompiler): TSQLResult;

    // --- INTERNAL (used by compilers) -----------------------------------

    /// <summary>Returns the ordered list of AST clause nodes.</summary>
    function Clauses: TArray<TAbstractClause>;
  end;

  // ---------------------------------------------------------------------------
  //  IQueryCompiler  —  strategy for dialect-specific SQL generation
  // ---------------------------------------------------------------------------

  /// <summary>
  /// Compiles an IQuery into dialect-specific SQL text and bindings.
  /// Inject via DI; default implementation is TAnsiSqlCompiler in
  /// Daf.SQLCute.Compiler unit.
  /// </summary>
  IQueryCompiler = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Compile(const Query: IQuery): TSQLResult;
  end;

  /// <summary>
  /// A FROM clause backed by a sub-query instead of a plain table name.
  /// Declared here (not in Clauses.pas) because it holds an IQuery reference.
  /// </summary>
  TFromSubqueryClause = class(TAbstractClause)
  public
    SubQuery: IQuery;
    Alias: string;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  TQuery  —  factory
  // ---------------------------------------------------------------------------

  /// <summary>
  /// Factory class providing the TQuery.New entry point.
  /// </summary>
  TQuery = class
  public
    /// <summary>Creates a new, empty query builder (ref-counted).</summary>
    class function New: IQuery; static;
  end;

// ---------------------------------------------------------------------------
//  Query factory
// ---------------------------------------------------------------------------

  /// <summary>Factory function type used to decouple IQuery creation from Abstractions.</summary>
  TIQueryFactory = reference to function: IQuery;

/// <summary>Registers the factory function that TQuery.New delegates to.
/// Called automatically by Daf.SQLCute.Core in its initialization section.</summary>
procedure RegisterIQueryFactory(const AFactory: TIQueryFactory);


// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// <summary>Converts an operator string ('=', '>', '<', etc.) to TWhereOp.</summary>
function ParseWhereOp(const Op: string): TWhereOp;


implementation

var
  FIQueryFactory: TIQueryFactory;

{ TSQLResult }

class function TSQLResult.Empty: TSQLResult;
begin
  Result.SQL      := '';
  Result.Bindings := [];
end;

{ TFromSubqueryClause }

function TFromSubqueryClause.Clone: TAbstractClause;
var
  C: TFromSubqueryClause;
begin
  C := TFromSubqueryClause.Create;
  C.SubQuery := SubQuery;
  C.Alias    := Alias;
  Result := C;
end;

{ ParseWhereOp }

function ParseWhereOp(const Op: string): TWhereOp;
var
  S: string;
begin
  S := Trim(Op);
  if S = '='  then Exit(TWhereOp.Equal);
  if S = '<>' then Exit(TWhereOp.NotEqual);
  if S = '!=' then Exit(TWhereOp.NotEqual);
  if S = '<'  then Exit(TWhereOp.Less);
  if S = '<=' then Exit(TWhereOp.LessOrEqual);
  if S = '>'  then Exit(TWhereOp.Greater);
  if S = '>=' then Exit(TWhereOp.GreaterOrEqual);
  if SameText(S, 'like')     then Exit(TWhereOp.Like);
  if SameText(S, 'not like') then Exit(TWhereOp.NotLike);
  if SameText(S, 'in')       then Exit(TWhereOp.&In);
  if SameText(S, 'not in')   then Exit(TWhereOp.NotIn);
  raise EArgumentException.CreateFmt('Unknown SQL operator: "%s"', [Op]);
end;

{ RegisterIQueryFactory }

procedure RegisterIQueryFactory(const AFactory: TIQueryFactory);
begin
  if not Assigned(AFactory) then
    raise EInvalidOpException.Create('Argument Factory cannot be nil');
  FIQueryFactory := AFactory;
end;

{ TQuery }

class function TQuery.New: IQuery;
begin
  if not Assigned(FIQueryFactory) then
    raise EInvalidOpException.Create(
      'IQuery factory not registered. Ensure Daf.SQLCute.Core is linked.');
  Result := FIQueryFactory();
end;

end.
