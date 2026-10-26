unit Daf.SQLCute.InMemory;

{
  Daf.SQLCute.InMemory — the in-memory compiler: a second SQLCute back-end
  (analogue of IQueryCompiler) that consumes an IQuery AST and emits an
  executable plan over in-memory collections via Daf.Linq, instead of SQL text.

  Generic half of the engine:
    * TRttiAccessors<T> — cached (column -> member) RTTI accessors producing the
      TColumnGetter the non-generic predicate/comparers consume (fast lane).
    * TRttiRow<T> — a zero-copy IRow view over an object/record T, reusing the
      accessors (general lane).
    * TResultWriter<TResult> — the RTTI materializer IRow -> TResult (column/alias
      -> member, TValue coercion, TNullable via its Implicit operator).
    * TInMemoryPlan<T> / TInMemory — the reusable plan and the façade:
        Elements<T>           preserve T (fast lane, no projection)
        Rows<T>               yield IEnumerable<IRow> (dynamic / projected)
        Project<TSrc,TResult> materialize into a result class

  Depends on Daf.Linq (the sequence engine), Daf.SQLCute (the AST), the shared
  non-generic core (Daf.SQLCute.InMemory.Compiler) and the row model
  (Daf.SQLCute.InMemory.Rows), plus Commons (Daf.Comparers).
}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  Daf.Linq,
  Daf.SQLCute,
  Daf.SQLCute.InMemory.Compiler,
  Daf.SQLCute.InMemory.Rows;

type
  /// <summary>Column value access over <c>T</c> — ref-counted so it outlives the plan.</summary>
  IRttiAccessors<T> = interface
    function Get(const Column: string; const Item: T): TValue;
    function Has(const Column: string): Boolean;
    function AllColumns: TArray<string>;
  end;

  /// <summary>
  ///   RTTI column accessors for element type <c>T</c>: every member is resolved
  ///   once, when the accessors are built, and a (optionally qualified) column
  ///   name is read as a <c>TValue</c> — the fast lane's zero-wrapper value access.
  ///   Nothing changes after construction, so a plan can run on several threads.
  /// </summary>
  TRttiAccessors<T> = class(TInterfacedObject, IRttiAccessors<T>)
  private
    FCtx: TRttiContext;
    FType: TRttiType;
    FIsObject: Boolean;
    FCache: TDictionary<string, TRttiMember>;
    FAllColumns: TArray<string>;
    function Resolve(const Column: string): TRttiMember;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const Column: string; const Item: T): TValue;
    function Has(const Column: string): Boolean;
    function AllColumns: TArray<string>;
  end;

  /// <summary>A zero-copy <c>IRow</c> view over an object/record <c>T</c> via cached RTTI.</summary>
  TRttiRow<T> = class(TInterfacedObject, IRow)
  private
    FAccessors: IRttiAccessors<T>;
    FItem: T;
  public
    constructor Create(const Accessors: IRttiAccessors<T>; const Item: T);
    function Get(const Column: string): TValue;
    function Has(const Column: string): Boolean;
    function Columns: TArray<string>;
  end;

  /// <summary>Materializes an <c>IRow</c> into a fresh <c>TResult</c> (ref-counted; outlives the plan).</summary>
  IResultWriter<TResult> = interface
    function Materialize(const Row: IRow; const Proj: TArray<TProjColumn>): TResult;
  end;

  /// <summary>RTTI materializer: maps output columns to members, coercing TValue (TNullable-aware).</summary>
  TResultWriter<TResult: class, constructor> = class(TInterfacedObject, IResultWriter<TResult>)
  private
    FCtx: TRttiContext;
    FType: TRttiType;
    FCache: TDictionary<string, TRttiMember>;
    function Resolve(const Name: string): TRttiMember;
    procedure Assign(const Obj: TObject; const MemberName: string; const Source: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    function Materialize(const Row: IRow; const Proj: TArray<TProjColumn>): TResult;
  end;

  /// <summary>A reusable in-memory execution plan (preserve-T lane); run over any source of <c>T</c>.</summary>
  IInMemoryPlan<T> = interface
    function Over(const Source: TEnumerable<T>): IEnumerable<T>; overload;
    function Over(const Source: TArray<T>): IEnumerable<T>; overload;
    function Over(const Source: IEnumerable<T>): IEnumerable<T>; overload;
  end;

  /// <summary>Concrete preserve-T plan: composes Daf.Linq operators from a <c>TCompiledQuery</c>.</summary>
  TInMemoryPlan<T> = class(TInterfacedObject, IInMemoryPlan<T>)
  private
    FCompiled: TCompiledQuery;
    FAccessors: IRttiAccessors<T>;
    function Apply(const Start: TQuery<T>): IEnumerable<T>;
    function OrderComparer: IComparer<T>;
    function DistinctComparer: IEqualityComparer<T>;
  public
    /// <summary>Takes ownership of <c>Compiled</c>.</summary>
    constructor Create(const Compiled: TCompiledQuery);
    destructor Destroy; override;
    function Over(const Source: TEnumerable<T>): IEnumerable<T>; overload;
    function Over(const Source: TArray<T>): IEnumerable<T>; overload;
    function Over(const Source: IEnumerable<T>): IEnumerable<T>; overload;
  end;

  /// <summary>
  ///   Per-call catalog mapping source names to row sequences (never
  ///   global). Supplied at execution time for the general multi-source lane.
  /// </summary>
  IQueryContext = interface
    ['{B4E6F2A3-1D5C-4E7F-9A82-3C4D5E6F7081}']
    /// <summary>The rows for a named source; raises when the name is unmapped.</summary>
    function Resolve(const Name: string): TArray<IRow>;
    /// <summary>True when a source is mapped to <c>Name</c>.</summary>
    function Has(const Name: string): Boolean;
    /// <summary>
    ///   The columns of a mapped source. False when they are unknown: a source of
    ///   <c>IRow</c> mapped with no rows.
    /// </summary>
    function ColumnsOf(const Name: string; out Columns: TArray<string>): Boolean;
  end;

  /// <summary>
  ///   Builds an <c>IQueryContext</c>: <c>Map('people', list).Map('orders', list)</c>.
  ///   Not ref-counted — the caller owns it and frees it (a per-call, short-lived
  ///   catalog). Typed sources are adapted to <c>IRow</c> up front (objects via
  ///   <c>TRttiRow&lt;T&gt;</c>; an <c>IRow</c> element is used as-is).
  /// </summary>
  TQueryContext = class(TInterfacedObject, IQueryContext)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    FSources: TDictionary<string, TArray<IRow>>;
    FSchemas: TDictionary<string, TArray<string>>;
    procedure Store(const Name: string; const Rows: TArray<IRow>; const Schema: TArray<string>;
      SchemaKnown: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Map<T>(const Name: string; const Source: TEnumerable<T>): TQueryContext; overload;
    function Map<T>(const Name: string; const Source: TArray<T>): TQueryContext; overload;
    function Map(const Name: string; const Rows: TArray<IRow>): TQueryContext; overload;
    function Resolve(const Name: string): TArray<IRow>;
    function Has(const Name: string): Boolean;
    function ColumnsOf(const Name: string; out Columns: TArray<string>): Boolean;
  end;

  /// <summary>Entry point: compile a SQLCute AST onto the in-memory engine.</summary>
  TInMemory = record
  strict private
    class function MapToRows<T>(const Start: TQuery<T>): TQuery<IRow>; static;
    class function BuildRowPipeline<T>(const C: TCompiledQuery; const Start: TQuery<T>): TQuery<IRow>; static;
    class function RowsCore<T>(const Query: IQuery; const Start: TQuery<T>): IEnumerable<IRow>; static;
    class function ProjectCore<TSource; TResult: class, constructor>(
      const Query: IQuery; const Start: TQuery<TSource>): IEnumerable<TResult>; static;
  public
    /// <summary>Compiles <c>Query</c> into a reusable preserve-T plan (single-source fast lane).</summary>
    class function Compile<T>(const Query: IQuery): IInMemoryPlan<T>; static;
    /// <summary>
    ///   Fabricates the <c>IEnumerable&lt;T&gt;</c> for <c>Query</c> over <c>Source</c> preserving
    ///   <c>T</c> — sugar over <c>Compile&lt;T&gt;(Query).Over(Source)</c>. Raises if the query
    ///   projects (use <see cref="Rows"/> / <see cref="Project"/> instead).
    /// </summary>
    class function Elements<T>(const Query: IQuery; const Source: TEnumerable<T>): IEnumerable<T>; overload; static;
    class function Elements<T>(const Query: IQuery; const Source: TArray<T>): IEnumerable<T>; overload; static;
    class function Elements<T>(const Query: IQuery; const Source: IEnumerable<T>): IEnumerable<T>; overload; static;
    /// <summary>Runs <c>Query</c> over <c>Source</c> yielding dynamic <c>IRow</c>s (projected when the query has a SELECT).</summary>
    class function Rows<T>(const Query: IQuery; const Source: TEnumerable<T>): IEnumerable<IRow>; overload; static;
    class function Rows<T>(const Query: IQuery; const Source: TArray<T>): IEnumerable<IRow>; overload; static;
    class function Rows<T>(const Query: IQuery; const Source: IEnumerable<T>): IEnumerable<IRow>; overload; static;
    /// <summary>Runs <c>Query</c> over <c>Source</c> materializing each final row into <c>TResult</c>.</summary>
    class function Project<TSource; TResult: class, constructor>(
      const Query: IQuery; const Source: TEnumerable<TSource>): IEnumerable<TResult>; overload; static;
    class function Project<TSource; TResult: class, constructor>(
      const Query: IQuery; const Source: TArray<TSource>): IEnumerable<TResult>; overload; static;
    class function Project<TSource; TResult: class, constructor>(
      const Query: IQuery; const Source: IEnumerable<TSource>): IEnumerable<TResult>; overload; static;
    /// <summary>Runs a multi-source <c>Query</c> over a context, yielding dynamic <c>IRow</c>s (JOINs, projected).</summary>
    class function Rows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>; overload; static;
    /// <summary>Runs a multi-source <c>Query</c> over a context, materializing each final row into <c>TResult</c>.</summary>
    class function Project<TResult: class, constructor>(
      const Query: IQuery; const Context: IQueryContext): IEnumerable<TResult>; overload; static;
  end;

// These three stay in the interface section under protest, and it is not a style
// choice: Delphi rejects E2506 — "method of parameterized type declared in
// interface section must not use local symbol" — for any unit-level routine a
// generic method declared above references. TResultWriter reaches AssignMember,
// TInMemory.Project reaches ExecuteToRows, and both reach DistinctColumns. There
// is no visibility that expresses "public only to the generics of this unit".

/// <summary>
///   Assigns a column value to a result-class member, coercing the <c>TValue</c>
///   to the member's type. A NULL source leaves the member at its default (a
///   <c>TNullable</c> member stays <c>IsNull</c>); a <c>TNullable&lt;X&gt;</c>
///   member is built through its <c>Implicit(X)</c> operator.
/// </summary>
procedure AssignMember(const Instance: TObject; M: TRttiMember; const Source: TValue);

/// <summary>DISTINCT output columns on the pre-projection row (projection sources, or nil = all).</summary>
function DistinctColumns(const C: TCompiledQuery): TArray<string>;

/// <summary>
///   Runs a multi-source query end-to-end against the context — FROM + JOINs,
///   WHERE, GROUP BY, projection and set operations (UNION/…) — yielding the
///   final projected <c>IRow</c>s.
/// </summary>
function ExecuteToRows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>;

implementation

uses
  System.TypInfo,          // TMemberVisibility (mvPublic / mvPublished)
  System.Variants,         // VarToStr / Variant coercion (group keys, aggregates)
  Daf.SQLCute.Clauses,     // TFromClause / TJoinClause / TJoinType (multi-source engine)
  Daf.Comparers,           // TValueComparers, IsNullValue
  Daf.Rtti,                // TRttiProperty smart SetValue + CoerceForAssign (shared coercion)
  Daf.Nullable;            // TNullableRtti

// Assigns a column value to a result-class member. A NULL source leaves the
// member at its default (a TNullable member stays IsNull). Coercion (incl.
// TNullable via its Implicit operator) is delegated to the smart SetValue
// helpers in Daf.Rtti — uniform for properties and fields.
procedure AssignMember(const Instance: TObject; M: TRttiMember; const Source: TValue);
begin
  if IsNullValue(Source) then
    Exit;
  if M is TRttiProperty then
    TRttiProperty(M).SetValue(Instance, Source)
  else if M is TRttiField then
    TRttiField(M).SetValue(Instance, Source);
end;

{ TRttiAccessors<T> }

constructor TRttiAccessors<T>.Create;
begin
  inherited Create;
  FCtx := TRttiContext.Create;
  FType := FCtx.GetType(TypeInfo(T));
  FIsObject := (FType <> nil) and (FType.TypeKind = tkClass);
  FCache := TDictionary<string, TRttiMember>.Create;
  if FType = nil then
    Exit;
  // Keyed by NormalizeName (snake_case ↔ PascalCase); a property wins over a
  // field of the same key, and the first of either wins over a later one.
  for var P in FType.GetProperties do
    FCache.TryAdd(NormalizeName(P.Name), P);
  for var F in FType.GetFields do
    FCache.TryAdd(NormalizeName(F.Name), F);
  if FIsObject then
  begin
    for var P in FType.GetProperties do
      if P.IsReadable and (P.Visibility in [mvPublic, mvPublished]) then
        FAllColumns := FAllColumns + [P.Name];
  end
  else
    for var F in FType.GetFields do
      FAllColumns := FAllColumns + [F.Name];
end;

destructor TRttiAccessors<T>.Destroy;
begin
  FCache.Free;
  FCtx.Free;
  inherited;
end;

function TRttiAccessors<T>.Resolve(const Column: string): TRttiMember;
begin
  if not FCache.TryGetValue(NormalizeName(Column), Result) then
    Result := nil;
end;

function TRttiAccessors<T>.Has(const Column: string): Boolean;
begin
  Result := Resolve(Column) <> nil;
end;

function TRttiAccessors<T>.Get(const Column: string; const Item: T): TValue;
var
  Instance: Pointer;
begin
  var M := Resolve(Column);
  if M = nil then
    raise EInMemoryQueryError.CreateFmt('Column "%s" is not a member of %s.',
      [Column, FType.Name]);
  var Holder := TValue.From<T>(Item);
  if FIsObject then
  begin
    var Obj := Holder.AsObject;
    if Obj = nil then
      Exit(TValue.Empty);
    Instance := Obj;
  end
  else
    Instance := Holder.GetReferenceToRawData;
  if M is TRttiProperty then
    Result := TRttiProperty(M).GetValue(Instance)
  else if M is TRttiField then
    Result := TRttiField(M).GetValue(Instance)
  else
    Result := TValue.Empty;
  // A TNullable member is NULL when absent and its value when present.
  var Inner: TValue;
  if TNullableRtti.TryUnwrap(Result, Inner) then
    Result := Inner;
end;

function TRttiAccessors<T>.AllColumns: TArray<string>;
begin
  Result := FAllColumns;
end;

{ TRttiRow<T> }

constructor TRttiRow<T>.Create(const Accessors: IRttiAccessors<T>; const Item: T);
begin
  inherited Create;
  FAccessors := Accessors;
  FItem := Item;
end;

function TRttiRow<T>.Get(const Column: string): TValue;
begin
  Result := FAccessors.Get(Column, FItem);
end;

function TRttiRow<T>.Has(const Column: string): Boolean;
begin
  Result := FAccessors.Has(Column);
end;

function TRttiRow<T>.Columns: TArray<string>;
begin
  Result := FAccessors.AllColumns;
end;

{ TResultWriter<TResult> }

constructor TResultWriter<TResult>.Create;
begin
  inherited Create;
  FCtx := TRttiContext.Create;
  FType := FCtx.GetType(TypeInfo(TResult));
  FCache := TDictionary<string, TRttiMember>.Create;
end;

destructor TResultWriter<TResult>.Destroy;
begin
  FCache.Free;
  FCtx.Free;
  inherited;
end;

function TResultWriter<TResult>.Resolve(const Name: string): TRttiMember;
var
  M: TRttiMember;
begin
  var Key := NormalizeName(Name);   // snake_case ↔ PascalCase bridge
  if FCache.TryGetValue(Key, M) then
    Exit(M);
  M := nil;
  if FType <> nil then
  begin
    for var P in FType.GetProperties do
      if (NormalizeName(P.Name) = Key) and P.IsWritable then
      begin
        M := P;
        Break;
      end;
    if M = nil then
      for var F in FType.GetFields do
        if NormalizeName(F.Name) = Key then
        begin
          M := F;
          Break;
        end;
  end;
  FCache.AddOrSetValue(Key, M);
  Result := M;
end;

procedure TResultWriter<TResult>.Assign(const Obj: TObject; const MemberName: string; const Source: TValue);
begin
  var M := Resolve(MemberName);
  if M <> nil then
    AssignMember(Obj, M, Source);
end;

function TResultWriter<TResult>.Materialize(const Row: IRow; const Proj: TArray<TProjColumn>): TResult;
begin
  Result := TResult.Create;
  if Proj = nil then
    for var C in Row.Columns do
      Assign(Result, C, Row.Get(C))
  else
    for var P in Proj do
      Assign(Result, P.Output, ReadColumn(Row, P.Source));
end;

{ TInMemoryPlan<T> }

constructor TInMemoryPlan<T>.Create(const Compiled: TCompiledQuery);
begin
  inherited Create;
  FCompiled := Compiled;
  FAccessors := TRttiAccessors<T>.Create;
end;

destructor TInMemoryPlan<T>.Destroy;
begin
  FAccessors := nil;   // interface: released here, but survives in captured closures
  FCompiled.Free;
  inherited;
end;

function TInMemoryPlan<T>.OrderComparer: IComparer<T>;
begin
  var Acc := FAccessors;
  var Specs := FCompiled.Orders;
  Result := TComparer<T>.Construct(
    function(const A, B: T): Integer
    begin
      Result := 0;
      for var S in Specs do
      begin
        Result := CompareForOrder(Acc.Get(S.Column, A), Acc.Get(S.Column, B), S);
        if Result <> 0 then
          Exit;
      end;
    end);
end;

function TInMemoryPlan<T>.DistinctComparer: IEqualityComparer<T>;
begin
  var Acc := FAccessors;
  var Cols := Acc.AllColumns;
  var VE := TValueComparers.Equality;   // NULLs equal
  Result := TEqualityComparer<T>.Construct(
    function(const A, B: T): Boolean
    begin
      for var C in Cols do
        if not VE.Equals(Acc.Get(C, A), Acc.Get(C, B)) then
          Exit(False);
      Result := True;
    end,
    function(const X: T): Integer
    begin
      Result := 17;
      for var C in Cols do
        Result := CombineHash(Result, VE.GetHashCode(Acc.Get(C, X)));
    end);
end;

function TInMemoryPlan<T>.Apply(const Start: TQuery<T>): IEnumerable<T>;
begin
  if FCompiled.Projection <> nil then
    raise EInMemoryQueryError.Create(
      'This query has a SELECT projection; use TInMemory.Rows<T> or '
      + 'TInMemory.Project<TSource, TResult> instead of Elements<T>.');

  var Q := Start;

  if Assigned(FCompiled.Predicate) then
  begin
    var Pred := FCompiled.Predicate;
    var Acc := FAccessors;
    Q := Q.Where(
      function(X: T): Boolean
      begin
        Result := Pred(
          function(const Col: string): TValue
          begin
            Result := Acc.Get(Col, X);
          end) = TTriState.Yes;
      end);
  end;

  if FCompiled.Distinct then
    Q := Q.Distinct(DistinctComparer);

  if Length(FCompiled.Orders) > 0 then
    Q := Q.OrderBy(OrderComparer);

  if FCompiled.HasOffset and (FCompiled.Offset > 0) then
    Q := Q.Skip(Integer(FCompiled.Offset));

  if FCompiled.HasLimit then
    Q := Q.Take(Integer(FCompiled.Limit));   // Take short-circuits when unordered

  Result := Q.AsEnumerable;
end;

function TInMemoryPlan<T>.Over(const Source: TEnumerable<T>): IEnumerable<T>;
begin
  Result := Apply(Linq.From<T>(Source));
end;

function TInMemoryPlan<T>.Over(const Source: TArray<T>): IEnumerable<T>;
begin
  Result := Apply(Linq.From<T>(Source));
end;

function TInMemoryPlan<T>.Over(const Source: IEnumerable<T>): IEnumerable<T>;
begin
  Result := Apply(Linq.From<T>(Source));
end;

{ TInMemory }

// DISTINCT is over the OUTPUT columns; on the pre-projection row those are the
// projection sources (or all columns when there is no projection).
function DistinctColumns(const C: TCompiledQuery): TArray<string>;
begin
  Result := nil;
  if C.Projection <> nil then
  begin
    SetLength(Result, Length(C.Projection));
    for var I := 0 to High(C.Projection) do
      Result[I] := C.Projection[I].Source;
  end;
end;

{ TQueryContext }

constructor TQueryContext.Create;
begin
  inherited Create;
  FSources := TDictionary<string, TArray<IRow>>.Create;
  FSchemas := TDictionary<string, TArray<string>>.Create;
end;

destructor TQueryContext.Destroy;
begin
  FSchemas.Free;
  FSources.Free;
  inherited;
end;

procedure TQueryContext.Store(const Name: string; const Rows: TArray<IRow>;
  const Schema: TArray<string>; SchemaKnown: Boolean);
begin
  var Key := LowerCase(Name);
  FSources.AddOrSetValue(Key, Rows);
  if SchemaKnown then
    FSchemas.AddOrSetValue(Key, Schema)
  else if Length(Rows) > 0 then
    FSchemas.AddOrSetValue(Key, Rows[0].Columns)
  else
    FSchemas.Remove(Key);
end;

// Not ref-counted: a per-call context is owned and freed by the caller, so a
// short-lived IQueryContext parameter never disposes it.
function TQueryContext._AddRef: Integer;
begin
  Result := -1;
end;

function TQueryContext._Release: Integer;
begin
  Result := -1;
end;

// A typed source knows its columns from its accessors, also with no elements; a
// source of IRow knows them from its first row.
function TQueryContext.Map<T>(const Name: string; const Source: TEnumerable<T>): TQueryContext;
begin
  var Rows: TArray<IRow> := nil;
  if TypeInfo(T) = TypeInfo(IRow) then
  begin
    for var X in Source do
      Rows := Rows + [TValue.From<T>(X).AsType<IRow>];
    Store(Name, Rows, nil, False);
  end
  else
  begin
    var Acc: IRttiAccessors<T> := TRttiAccessors<T>.Create;
    for var X in Source do
      Rows := Rows + [TRttiRow<T>.Create(Acc, X)];
    Store(Name, Rows, Acc.AllColumns, True);
  end;
  Result := Self;
end;

function TQueryContext.Map<T>(const Name: string; const Source: TArray<T>): TQueryContext;
begin
  var Rows: TArray<IRow> := nil;
  if TypeInfo(T) = TypeInfo(IRow) then
  begin
    for var X in Source do
      Rows := Rows + [TValue.From<T>(X).AsType<IRow>];
    Store(Name, Rows, nil, False);
  end
  else
  begin
    var Acc: IRttiAccessors<T> := TRttiAccessors<T>.Create;
    for var X in Source do
      Rows := Rows + [TRttiRow<T>.Create(Acc, X)];
    Store(Name, Rows, Acc.AllColumns, True);
  end;
  Result := Self;
end;

function TQueryContext.Map(const Name: string; const Rows: TArray<IRow>): TQueryContext;
begin
  Store(Name, Rows, nil, False);
  Result := Self;
end;

function TQueryContext.Resolve(const Name: string): TArray<IRow>;
begin
  if not FSources.TryGetValue(LowerCase(Name), Result) then
    raise EInMemoryQueryError.CreateFmt('Query context has no source mapped to "%s".', [Name]);
end;

function TQueryContext.Has(const Name: string): Boolean;
begin
  Result := FSources.ContainsKey(LowerCase(Name));
end;

function TQueryContext.ColumnsOf(const Name: string; out Columns: TArray<string>): Boolean;
begin
  Result := FSchemas.TryGetValue(LowerCase(Name), Columns);
end;

{ multi-source engine: FROM + JOINs }

// Fail-fast on the multi-source features not yet handled by this lane.
procedure EnsureMultiSupported(const Query: IQuery);
begin
  // Read-only by declared kind, naming the statement — same whitelist-of-one as
  // the single-source lane, so a future statement kind is covered without a
  // clause-by-clause blacklist to remember.
  if Query.Kind <> TQueryKind.Select then
    raise EInMemoryQueryError.CreateFmt(
      'The in-memory backend is read-only; it cannot execute a %s statement.',
      [QueryKindName(Query.Kind)]);
  for var C in Query.Clauses do
  begin
    if C is TJoinSubqueryClause then
      raise EInMemoryQueryError.Create('Subquery JOINs are not supported by the in-memory backend.');
    if (C is TFromSubqueryClause) or (C is TFromRawClause) then
      raise EInMemoryQueryError.Create('Derived-table / raw FROM sources are not supported by the in-memory backend.');
    if C is TWithClause then
      raise EInMemoryQueryError.Create('CTEs (WITH) are not supported by the in-memory backend.');
  end;
end;

// The ON predicate over the combined row: column-column, callback (WHERE clauses),
// or (unsupported) raw. CROSS / empty ON keeps every pair.
function BuildJoinOn(const Jc: TJoinClause): TPredicate<IRow>;
begin
  if Length(Jc.ConditionClauses) > 0 then
  begin
    var Pred := WherePredicate(Jc.ConditionClauses);
    if Assigned(Pred) then
      Result := MakeRowPredicate(Pred)
    else
      Result := function(R: IRow): Boolean begin Result := True; end;
  end
  else if Jc.Col1 <> '' then
  begin
    var Op := Jc.Op;
    var C1 := Jc.Col1;
    var C2 := Jc.Col2;
    Result :=
      function(R: IRow): Boolean
      begin
        Result := CompareOp(Op, ReadColumn(R, C1), ReadColumn(R, C2)) = TTriState.Yes;
      end;
  end
  else if Jc.Condition <> '' then
    raise EInMemoryQueryError.Create(
      'Raw JOIN ON conditions cannot be evaluated in memory; use a column or callback ON.')
  else
    Result := function(R: IRow): Boolean begin Result := True; end;
end;

type
  /// <summary>One source already joined: the alias the row uses, and the mapped table.</summary>
  TJoinedSide = record
    Alias: string;
    Table: string;
  end;

// The absent side of an outer join: the columns of the source it stands for, or
// any column when the context does not know them.
function NullSide(const Context: IQueryContext; const Table: string): IRow;
var
  Columns: TArray<string>;
begin
  if Context.ColumnsOf(Table, Columns) then
    Result := TNullRow.Create(Columns, False)
  else
    Result := TNullRow.Create(nil, True);
end;

// An all-NULL left composite, one side per source already joined, for the
// unmatched right rows of a RIGHT / FULL OUTER join.
function NullLeft(const Sides: TArray<TJoinedSide>; const Context: IQueryContext): IRow;
var
  Children: TArray<TJoinChild>;
begin
  Children := nil;
  for var S in Sides do
  begin
    var NC: TJoinChild;
    NC.Alias := S.Alias;
    NC.Row := NullSide(Context, S.Table);
    Children := Children + [NC];
  end;
  Result := TJoinRow.Create(Children);
end;

function ApplyJoin(const Left: TArray<IRow>; const Sides: TArray<TJoinedSide>;
  const Context: IQueryContext; const Jc: TJoinClause): TArray<IRow>;
begin
  var RightAlias := Jc.Alias;
  if RightAlias = '' then
    RightAlias := Jc.Table;
  var Rights := Context.Resolve(Jc.Table);
  var OnPred := BuildJoinOn(Jc);
  Result := nil;
  case Jc.JoinType of
    TJoinType.Inner, TJoinType.Cross:
      for var L in Left do
        for var R in Rights do
        begin
          var Combined := CombineRows(L, RightAlias, R);
          if (Jc.JoinType = TJoinType.Cross) or OnPred(Combined) then
            Result := Result + [Combined];
        end;
    TJoinType.Left:
      for var L in Left do
      begin
        var Matched := False;
        for var R in Rights do
        begin
          var Combined := CombineRows(L, RightAlias, R);
          if OnPred(Combined) then
          begin
            Result := Result + [Combined];
            Matched := True;
          end;
        end;
        if not Matched then
          Result := Result + [CombineRows(L, RightAlias, NullSide(Context, Jc.Table))];
      end;
    TJoinType.Right:
      for var R in Rights do
      begin
        var Matched := False;
        for var L in Left do
        begin
          var Combined := CombineRows(L, RightAlias, R);
          if OnPred(Combined) then
          begin
            Result := Result + [Combined];
            Matched := True;
          end;
        end;
        if not Matched then
          Result := Result + [CombineRows(NullLeft(Sides, Context), RightAlias, R)];
      end;
    TJoinType.FullOuter:
      begin
        var MatchedRight := TDictionary<IRow, Byte>.Create;
        try
          for var L in Left do
          begin
            var Matched := False;
            for var R in Rights do
            begin
              var Combined := CombineRows(L, RightAlias, R);
              if OnPred(Combined) then
              begin
                Result := Result + [Combined];
                Matched := True;
                MatchedRight.AddOrSetValue(R, 0);
              end;
            end;
            if not Matched then
              Result := Result + [CombineRows(L, RightAlias, NullSide(Context, Jc.Table))];
          end;
          for var R in Rights do
            if not MatchedRight.ContainsKey(R) then
              Result := Result + [CombineRows(NullLeft(Sides, Context), RightAlias, R)];
        finally
          MatchedRight.Free;
        end;
      end;
  end;
end;

{ GROUP BY / aggregates / HAVING }

type
  TNumberFamily = (Integral, Money, Float);

  /// <summary>A non-NULL value of SUM or AVG, by the family its sum keeps.</summary>
  TNumber = record
    Family: TNumberFamily;
    Int: Int64;
    Money: Currency;
    Float: Double;
    function AsDouble: Double;
  end;

function TNumber.AsDouble: Double;
begin
  case Family of
    TNumberFamily.Integral: Result := Int;
    TNumberFamily.Money:    Result := Money;
  else
    Result := Float;
  end;
end;

// A number of SUM or AVG; anything that is not one raises, naming the column.
function AsNumber(const Column: string; const Value: TValue): TNumber;
begin
  var V := Value;
  if V.Kind = tkVariant then
    V := TValue.FromVariant(V.AsVariant);
  case V.Kind of
    tkInteger, tkInt64:
      begin
        Result.Family := TNumberFamily.Integral;
        Result.Int := V.AsInt64;
        Exit;
      end;
    tkFloat:
      if (V.TypeInfo <> TypeInfo(TDateTime)) and (V.TypeInfo <> TypeInfo(TDate))
         and (V.TypeInfo <> TypeInfo(TTime)) then
      begin
        if GetTypeData(V.TypeInfo).FloatType = ftCurr then
        begin
          Result.Family := TNumberFamily.Money;
          Result.Money := V.AsCurrency;
        end
        else
        begin
          Result.Family := TNumberFamily.Float;
          Result.Float := V.AsExtended;
        end;
        Exit;
      end;
  end;
  raise EInMemoryQueryError.CreateFmt('The aggregate of "%s" needs numbers, and got %s.', [Column, V.ToString]);
end;

function AddInt64(const A, B: Int64; const Column: string): Int64;
begin
  if ((B > 0) and (A > High(Int64) - B)) or ((B < 0) and (A < Low(Int64) - B)) then
    raise EInMemoryQueryError.CreateFmt('SUM of "%s" overflows its type.', [Column]);
  Result := A + B;
end;

// SUM keeps the family of its values: Int64 for integers, Currency when any is
// Currency and none is floating point, Double otherwise. Currency adds in its
// Int64 representation, scaled by 10000, so neither family rounds.
function SumOf(const Column: string; const Numbers: TArray<TNumber>): TValue;
const
  MoneyScale = 10000;
begin
  var AnyFloat := False;
  var AnyMoney := False;
  for var N in Numbers do
    case N.Family of
      TNumberFamily.Float: AnyFloat := True;
      TNumberFamily.Money: AnyMoney := True;
    end;
  if AnyFloat then
  begin
    var D: Double := 0;
    for var N in Numbers do
      D := D + N.AsDouble;
    Exit(D);
  end;
  if AnyMoney then
  begin
    var Scaled: Int64 := 0;
    for var N in Numbers do
      if N.Family = TNumberFamily.Money then
        Scaled := AddInt64(Scaled, PInt64(@N.Money)^, Column)
      else if Abs(N.Int) > High(Int64) div MoneyScale then
        raise EInMemoryQueryError.CreateFmt('SUM of "%s" overflows its type.', [Column])
      else
        Scaled := AddInt64(Scaled, N.Int * MoneyScale, Column);
    var Total: Currency;
    PInt64(@Total)^ := Scaled;
    Exit(TValue.From<Currency>(Total));
  end;
  var Total: Int64 := 0;
  for var N in Numbers do
    Total := AddInt64(Total, N.Int, Column);
  Result := TValue.From<Int64>(Total);
end;

// One aggregate over a group's members; NULLs are skipped (COUNT(*) counts rows).
function ComputeAggregate(const Agg: TAggregateSpec; const Members: TArray<IRow>): TValue;
begin
  case Agg.Func of
    TAggFunc.Count:
      if Agg.Column = '*' then
        Result := Length(Members)
      else
      begin
        var N := 0;
        for var R in Members do
          if not IsNullValue(ReadColumn(R, Agg.Column)) then
            Inc(N);
        Result := N;
      end;
    TAggFunc.Sum:
      begin
        var Numbers: TArray<TNumber> := nil;
        for var R in Members do
        begin
          var V := ReadColumn(R, Agg.Column);
          if not IsNullValue(V) then
            Numbers := Numbers + [AsNumber(Agg.Column, V)];
        end;
        if Numbers <> nil then Result := SumOf(Agg.Column, Numbers) else Result := TValue.Empty;
      end;
    TAggFunc.Avg:
      begin
        var S: Double := 0;
        var N := 0;
        for var R in Members do
        begin
          var V := ReadColumn(R, Agg.Column);
          if not IsNullValue(V) then
          begin
            S := S + AsNumber(Agg.Column, V).AsDouble;
            Inc(N);
          end;
        end;
        if N > 0 then Result := S / N else Result := TValue.Empty;
      end;
    TAggFunc.Min, TAggFunc.Max:
      begin
        var Best := TValue.Empty;
        var HasBest := False;
        for var R in Members do
        begin
          var V := ReadColumn(R, Agg.Column);
          if IsNullValue(V) then
            Continue;
          if not HasBest then
          begin
            Best := V;
            HasBest := True;
          end
          else
          begin
            var Cmp := CompareValues(V, Best);
            if ((Agg.Func = TAggFunc.Min) and (Cmp < 0))
               or ((Agg.Func = TAggFunc.Max) and (Cmp > 0)) then
              Best := V;
          end;
        end;
        Result := Best;
      end;
  else
    Result := TValue.Empty;
  end;
end;

// Group keys compare as tuples of values with the equality DISTINCT uses: NULLs
// are equal, so all NULL keys are one group.
function GroupKeyEquality: IEqualityComparer<TArray<TValue>>;
begin
  var VE := TValueComparers.Equality;
  Result := TEqualityComparer<TArray<TValue>>.Construct(
    function(const A, B: TArray<TValue>): Boolean
    begin
      for var I := 0 to High(A) do
        if not VE.Equals(A[I], B[I]) then
          Exit(False);
      Result := True;
    end,
    function(const K: TArray<TValue>): Integer
    begin
      Result := 17;
      for var V in K do
        Result := CombineHash(Result, VE.GetHashCode(V));
    end);
end;

// Groups rows by the GROUP BY columns, computes aggregates over each group's
// members, applies HAVING, and emits one flat row per surviving group.
// The hybrid: Daf.Linq does the grouping (key + members, its own
// general-purpose shape) and this compiler layers SQL's flat aggregate row on
// top — the SQL shape is not baked into the sequence library.
function GroupAndAggregate(const Rows: TArray<IRow>; const C: TCompiledQuery): TArray<IRow>;
begin
  var Cols := C.GroupBy;
  var Groups := Linq.From<IRow>(Rows).GroupBy<TArray<TValue>>(
    function(R: IRow): TArray<TValue>
    begin
      SetLength(Result, Length(Cols));
      for var I := 0 to High(Cols) do
        Result[I] := ReadColumn(R, Cols[I]);
    end,
    GroupKeyEquality);

  Result := nil;
  for var G in Groups do             // key order preserved by GroupBy
  begin
    var Members := G.Members;
    // Held by its interface from the start: an aggregate that raises frees it.
    var Flat := TDictRow.Create;
    var FlatRow: IRow := Flat;
    for var Gc in Cols do
      Flat.Put(Gc, ReadColumn(Members[0], Gc));   // any member carries the group's key values
    for var Agg in C.Aggregates do
      Flat.Put(Agg.Output, ComputeAggregate(Agg, Members));
    // The aggregates a HavingRaw reads are computed over the members too, and
    // stay out of the output row.
    var Hidden: TArray<TValue> := nil;
    for var Agg in C.HavingAggregates do
      Hidden := Hidden + [ComputeAggregate(Agg, Members)];
    var HiddenSpecs := C.HavingAggregates;
    var Keep := True;
    if Assigned(C.Having) then
      Keep := C.Having(
        function(const Col: string): TValue
        begin
          for var I := 0 to High(HiddenSpecs) do
            if HiddenSpecs[I].Output = Col then
              Exit(Hidden[I]);
          Result := ReadColumn(FlatRow, Col);
        end) = TTriState.Yes;
    if Keep then
      Result := Result + [FlatRow];
  end;
end;

// True when the prefix of a qualified name names one of the query's own sides.
function NamesOwnSide(const Column: string; const Aliases: TArray<string>): Boolean;
begin
  for var A in Aliases do
    if (Length(Column) > Length(A) + 1) and SameText(Copy(Column, 1, Length(A) + 1), A + '.') then
      Exit(True);
  Result := False;
end;

// The reader of the WHERE over one row. In a subquery, Outer reads the row of
// the enclosing query, and a qualified name whose prefix names none of the
// subquery's sides is read from it. Any other name is read from the row itself,
// so an unqualified name never reaches the outer row.
function WhereReader(const Row: IRow; const Aliases: TArray<string>;
  const Outer: TColumnGetter): TColumnGetter;
begin
  Result :=
    function(const Col: string): TValue
    begin
      if Assigned(Outer) and (Pos('.', Col) > 0) and not NamesOwnSide(Col, Aliases) then
        Result := Outer(Col)
      else
        Result := ReadColumn(Row, Col);
    end;
end;

// Eager WHERE filter — the grouping path must materialize before grouping.
function FilterRows(const Rows: TArray<IRow>; const Pred: TValuePredicate;
  const Aliases: TArray<string>; const Outer: TColumnGetter): TArray<IRow>;
begin
  if not Assigned(Pred) then
    Exit(Rows);
  Result := nil;
  for var R in Rows do
    if Pred(WhereReader(R, Aliases, Outer)) = TTriState.Yes then
      Result := Result + [R];
end;

// Lazy WHERE filter, reading each row as FilterRows does.
function WhereRowPredicate(const Pred: TValuePredicate; const Aliases: TArray<string>;
  const Outer: TColumnGetter): TPredicate<IRow>;
begin
  Result :=
    function(R: IRow): Boolean
    begin
      Result := Pred(WhereReader(R, Aliases, Outer)) = TTriState.Yes;
    end;
end;

function ProjectedRows(const Query: IQuery; const Context: IQueryContext;
  const Outer: TColumnGetter): IEnumerable<IRow>; forward;

type
  TSubqueryResult = record
    Clause: Pointer;
    Correlated: Boolean;
    Values: TArray<TValue>;
    NonEmpty: Boolean;
  end;

// Runs the subquery of a WHERE leaf, its WHERE reading the outer row through
// Outer, and keeps what IN and EXISTS test: whether it returns a row, and the
// values of its first output column.
function EvaluateSubquery(const Clause: TWhereClause; const Context: IQueryContext;
  const Outer: TColumnGetter): TSubqueryResult;
begin
  Result.Clause := Pointer(Clause);
  Result.Correlated := False;
  Result.Values := nil;
  var SubRows := TQuery<IRow>(ProjectedRows(Clause.SubQuery as IQuery, Context, Outer)).ToArray;
  Result.NonEmpty := Length(SubRows) > 0;
  for var R in SubRows do
  begin
    var Cols := R.Columns;
    if Length(Cols) > 0 then
      Result.Values := Result.Values + [R.Get(Cols[0])];
  end;
end;

// Resolves a subquery WHERE leaf against the context; the subquery reads the
// outer row through Get. A subquery whose first evaluation reads nothing from
// the outer row repeats the same steps for any other, so its result is cached by
// clause identity; one that reads it is evaluated again for each outer row.
// IN/NOT IN test membership in the subquery's first output column,
// EXISTS/NOT EXISTS test non-emptiness (3VL).
function MakeSubqueryResolver(const Context: IQueryContext): TSubqueryResolver;
var
  Cache: TArray<TSubqueryResult>;   // captured by the closure (managed; freed with it)
begin
  Cache := nil;
  Result :=
    function(const Clause: TWhereClause; const Get: TColumnGetter): TTriState
    var
      Sub: TSubqueryResult;
      ReadOuter: Boolean;
    begin
      var Idx := -1;
      for var I := 0 to High(Cache) do
        if Cache[I].Clause = Pointer(Clause) then
        begin
          Idx := I;
          Break;
        end;
      if Idx < 0 then
      begin
        ReadOuter := False;
        Sub := EvaluateSubquery(Clause, Context,
          function(const Column: string): TValue
          begin
            ReadOuter := True;
            Result := Get(Column);
          end);
        var Entry := Sub;
        Entry.Correlated := ReadOuter;
        if ReadOuter then
          Entry.Values := nil;
        Cache := Cache + [Entry];
      end
      else if Cache[Idx].Correlated then
        Sub := EvaluateSubquery(Clause, Context, Get)
      else
        Sub := Cache[Idx];

      if (Clause.Op = TWhereOp.&Exists) or (Clause.Op = TWhereOp.NotExists) then
        Exit(BoolToTri(Sub.NonEmpty = (Clause.Op = TWhereOp.&Exists)));

      // IN / NOT IN — against an empty subquery no comparison can be unknown,
      // so the empty set decides before the value; otherwise 3VL membership.
      if not Sub.NonEmpty then
        Exit(BoolToTri(Clause.Op = TWhereOp.NotIn));
      var ColVal := Get(Clause.Column);
      if IsNullValue(ColVal) then
        Exit(TTriState.Unknown);
      var Found := False;
      var AnyNull := False;
      for var V in Sub.Values do
        if IsNullValue(V) then
          AnyNull := True
        else if CompareValues(ColVal, V) = 0 then
          Found := True;
      if Found then
        Exit(BoolToTri(Clause.Op = TWhereOp.&In));
      if AnyNull then
        Exit(TTriState.Unknown);
      Result := BoolToTri(Clause.Op = TWhereOp.NotIn);
    end;
end;

// Resolves FROM + JOINs into composite rows, then applies WHERE / (GROUP BY +
// aggregates + HAVING) / DISTINCT / ORDER BY / paging. The SELECT projection is
// returned via Proj for the caller to apply (dynamic IRow) or materialize. Outer
// reads the row of the enclosing query in the WHERE of a subquery, and is nil
// for the main query.
function ExecuteMultiCore(const Query: IQuery; const Context: IQueryContext;
  const Outer: TColumnGetter; out Proj: TArray<TProjColumn>): IEnumerable<IRow>;
begin
  var Clauses := Query.Clauses;
  EnsureMultiSupported(Query);

  var FromCl: TFromClause := nil;
  for var C in Clauses do
    if C is TFromClause then
    begin
      FromCl := TFromClause(C);
      Break;
    end;
  if FromCl = nil then
    raise EInMemoryQueryError.Create('A multi-source query needs a FROM table mapped in the context.');

  var FromAlias := FromCl.Alias;
  if FromAlias = '' then
    FromAlias := FromCl.Table;

  var Composite: TArray<IRow> := nil;
  for var R in Context.Resolve(FromCl.Table) do
    Composite := Composite + [SingleRow(FromAlias, R)];

  var Side: TJoinedSide;
  Side.Alias := FromAlias;
  Side.Table := FromCl.Table;
  var Sides: TArray<TJoinedSide> := [Side];
  for var C in Clauses do
    if C is TJoinClause then
    begin
      var Jc := TJoinClause(C);
      Composite := ApplyJoin(Composite, Sides, Context, Jc);
      Side.Alias := Jc.Alias;
      if Side.Alias = '' then
        Side.Alias := Jc.Table;
      Side.Table := Jc.Table;
      Sides := Sides + [Side];
    end;

  var Aliases: TArray<string> := nil;
  for var S in Sides do
    Aliases := Aliases + [S.Alias];

  // The predicate captures the resolver of this context, and of no other.
  var Cq := TInMemoryCompiler.CompileMulti(Query, MakeSubqueryResolver(Context));
  try
    var Q: TQuery<IRow>;
    if Length(Cq.GroupBy) > 0 then
    begin
      // WHERE runs before grouping; the flat aggregate rows are the output.
      Q := Linq.From<IRow>(GroupAndAggregate(FilterRows(Composite, Cq.Predicate, Aliases, Outer), Cq));
      Proj := nil;
    end
    else
    begin
      Q := Linq.From<IRow>(Composite);
      if Assigned(Cq.Predicate) then
        Q := Q.Where(WhereRowPredicate(Cq.Predicate, Aliases, Outer));
      Proj := Cq.Projection;
    end;
    if Cq.Distinct then
      Q := Q.Distinct(MakeRowDistinct(DistinctColumns(Cq)));
    if Length(Cq.Orders) > 0 then
      Q := Q.OrderBy(MakeRowComparer(Cq.Orders));
    if Cq.HasOffset and (Cq.Offset > 0) then
      Q := Q.Skip(Integer(Cq.Offset));
    if Cq.HasLimit then
      Q := Q.Take(Integer(Cq.Limit));
    Result := Q.AsEnumerable;
  finally
    Cq.Free;   // predicate/orders/projection are captured / copied out above
  end;
end;

// A query's final projected rows (FROM+JOINs+WHERE+GROUP BY, then SELECT). Outer
// is the reader of the enclosing query's row for a subquery, nil for the main one.
function ProjectedRows(const Query: IQuery; const Context: IQueryContext;
  const Outer: TColumnGetter): IEnumerable<IRow>;
begin
  var Proj: TArray<TProjColumn>;
  var Q: TQuery<IRow> := ExecuteMultiCore(Query, Context, Outer, Proj);
  Result := Q.Select<IRow>(
    function(R: IRow): IRow
    begin
      Result := ProjectRow(R, Proj);
    end).AsEnumerable;
end;

{ set operations — row equality by output columns, NULLs equal }

function DedupRows(const Rows: TArray<IRow>; const Cmp: IEqualityComparer<IRow>): TArray<IRow>;
begin
  var Seen := TDictionary<IRow, Byte>.Create(Cmp);
  try
    Result := nil;
    for var R in Rows do
      if not Seen.ContainsKey(R) then
      begin
        Seen.Add(R, 0);
        Result := Result + [R];
      end;
  finally
    Seen.Free;
  end;
end;

function CountRows(const Rows: TArray<IRow>; const Cmp: IEqualityComparer<IRow>): TDictionary<IRow, Integer>;
begin
  Result := TDictionary<IRow, Integer>.Create(Cmp);
  for var R in Rows do
  begin
    var N: Integer;
    if Result.TryGetValue(R, N) then
      Result[R] := N + 1
    else
      Result.Add(R, 1);
  end;
end;

function CombineSets(Kind: TUnionKind; const A, B: TArray<IRow>): TArray<IRow>;
begin
  var Cmp := MakeRowDistinct(nil);   // compare by each row's own output columns
  case Kind of
    TUnionKind.UnionAll:
      Result := A + B;
    TUnionKind.Union:
      Result := DedupRows(A + B, Cmp);
    TUnionKind.Intersect:
      begin
        var BSet := CountRows(B, Cmp);
        try
          Result := nil;
          for var R in DedupRows(A, Cmp) do
            if BSet.ContainsKey(R) then
              Result := Result + [R];
        finally
          BSet.Free;
        end;
      end;
    TUnionKind.&Except:
      begin
        var BSet := CountRows(B, Cmp);
        try
          Result := nil;
          for var R in DedupRows(A, Cmp) do
            if not BSet.ContainsKey(R) then
              Result := Result + [R];
        finally
          BSet.Free;
        end;
      end;
    TUnionKind.IntersectAll:
      begin
        var BCount := CountRows(B, Cmp);
        var Emitted := TDictionary<IRow, Integer>.Create(Cmp);
        try
          Result := nil;
          for var R in A do
          begin
            var Avail, Used: Integer;
            if not BCount.TryGetValue(R, Avail) then
              Continue;
            if not Emitted.TryGetValue(R, Used) then
              Used := 0;
            if Used < Avail then
            begin
              Result := Result + [R];
              Emitted.AddOrSetValue(R, Used + 1);
            end;
          end;
        finally
          BCount.Free;
          Emitted.Free;
        end;
      end;
    TUnionKind.ExceptAll:
      begin
        var BCount := CountRows(B, Cmp);
        var Removed := TDictionary<IRow, Integer>.Create(Cmp);
        try
          Result := nil;
          for var R in A do
          begin
            var Rem, Used: Integer;
            if not BCount.TryGetValue(R, Rem) then
              Rem := 0;
            if not Removed.TryGetValue(R, Used) then
              Used := 0;
            if Used < Rem then
              Removed.AddOrSetValue(R, Used + 1)   // one occurrence subtracted
            else
              Result := Result + [R];
          end;
        finally
          BCount.Free;
          Removed.Free;
        end;
      end;
  else
    Result := A;
  end;
end;

// Applies each UNION/INTERSECT/EXCEPT clause of the main query against the context.
function ApplyUnions(const Main: IEnumerable<IRow>; const Query: IQuery;
  const Context: IQueryContext): IEnumerable<IRow>;
begin
  var HasUnion := False;
  for var C in Query.Clauses do
    if C is TUnionClause then
    begin
      HasUnion := True;
      Break;
    end;
  if not HasUnion then
    Exit(Main);

  var Acc := TQuery<IRow>(Main).ToArray;
  for var C in Query.Clauses do
    if C is TUnionClause then
    begin
      var U := TUnionClause(C);
      if U.SubQuery = nil then
        raise EInMemoryQueryError.Create('Raw set operations (CombineRaw) cannot be evaluated in memory.');
      var Other := TQuery<IRow>(ProjectedRows(U.SubQuery as IQuery, Context, nil)).ToArray;
      Acc := CombineSets(U.Kind, Acc, Other);
    end;
  Result := Linq.From<IRow>(Acc);
end;

function ExecuteToRows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>;
begin
  Result := ApplyUnions(ProjectedRows(Query, Context, nil), Query, Context);
end;

class function TInMemory.MapToRows<T>(const Start: TQuery<T>): TQuery<IRow>;
begin
  if TypeInfo(T) = TypeInfo(IRow) then
    Result := Start.Select<IRow>(
      function(X: T): IRow
      begin
        Result := TValue.From<T>(X).AsType<IRow>;
      end)
  else
  begin
    var Acc: IRttiAccessors<T> := TRttiAccessors<T>.Create;
    Result := Start.Select<IRow>(
      function(X: T): IRow
      begin
        Result := TRttiRow<T>.Create(Acc, X);
      end);
  end;
end;

class function TInMemory.BuildRowPipeline<T>(const C: TCompiledQuery; const Start: TQuery<T>): TQuery<IRow>;
begin
  var Rows := MapToRows<T>(Start);
  if Assigned(C.Predicate) then
    Rows := Rows.Where(MakeRowPredicate(C.Predicate));
  if C.Distinct then
    Rows := Rows.Distinct(MakeRowDistinct(DistinctColumns(C)));
  if Length(C.Orders) > 0 then
    Rows := Rows.OrderBy(MakeRowComparer(C.Orders));
  if C.HasOffset and (C.Offset > 0) then
    Rows := Rows.Skip(Integer(C.Offset));
  if C.HasLimit then
    Rows := Rows.Take(Integer(C.Limit));
  Result := Rows;
end;

class function TInMemory.RowsCore<T>(const Query: IQuery; const Start: TQuery<T>): IEnumerable<IRow>;
begin
  var C := TInMemoryCompiler.Compile(Query);
  try
    var Proj := C.Projection;
    Result := BuildRowPipeline<T>(C, Start).Select<IRow>(
      function(R: IRow): IRow
      begin
        Result := ProjectRow(R, Proj);
      end).AsEnumerable;
  finally
    C.Free;   // predicate/orders/projection are captured in the closures above
  end;
end;

class function TInMemory.ProjectCore<TSource, TResult>(
  const Query: IQuery; const Start: TQuery<TSource>): IEnumerable<TResult>;
begin
  var C := TInMemoryCompiler.Compile(Query);
  try
    var Proj := C.Projection;
    var Writer: IResultWriter<TResult> := TResultWriter<TResult>.Create;
    Result := BuildRowPipeline<TSource>(C, Start).Select<TResult>(
      function(R: IRow): TResult
      begin
        Result := Writer.Materialize(R, Proj);
      end).AsEnumerable;
  finally
    C.Free;
  end;
end;

class function TInMemory.Compile<T>(const Query: IQuery): IInMemoryPlan<T>;
begin
  Result := TInMemoryPlan<T>.Create(TInMemoryCompiler.Compile(Query));
end;

class function TInMemory.Elements<T>(const Query: IQuery; const Source: TEnumerable<T>): IEnumerable<T>;
begin
  Result := Compile<T>(Query).Over(Source);
end;

class function TInMemory.Elements<T>(const Query: IQuery; const Source: TArray<T>): IEnumerable<T>;
begin
  Result := Compile<T>(Query).Over(Source);
end;

class function TInMemory.Elements<T>(const Query: IQuery; const Source: IEnumerable<T>): IEnumerable<T>;
begin
  Result := Compile<T>(Query).Over(Source);
end;

class function TInMemory.Rows<T>(const Query: IQuery; const Source: TEnumerable<T>): IEnumerable<IRow>;
begin
  Result := RowsCore<T>(Query, Linq.From<T>(Source));
end;

class function TInMemory.Rows<T>(const Query: IQuery; const Source: TArray<T>): IEnumerable<IRow>;
begin
  Result := RowsCore<T>(Query, Linq.From<T>(Source));
end;

class function TInMemory.Rows<T>(const Query: IQuery; const Source: IEnumerable<T>): IEnumerable<IRow>;
begin
  Result := RowsCore<T>(Query, Linq.From<T>(Source));
end;

class function TInMemory.Project<TSource, TResult>(
  const Query: IQuery; const Source: TEnumerable<TSource>): IEnumerable<TResult>;
begin
  Result := ProjectCore<TSource, TResult>(Query, Linq.From<TSource>(Source));
end;

class function TInMemory.Project<TSource, TResult>(
  const Query: IQuery; const Source: TArray<TSource>): IEnumerable<TResult>;
begin
  Result := ProjectCore<TSource, TResult>(Query, Linq.From<TSource>(Source));
end;

class function TInMemory.Project<TSource, TResult>(
  const Query: IQuery; const Source: IEnumerable<TSource>): IEnumerable<TResult>;
begin
  Result := ProjectCore<TSource, TResult>(Query, Linq.From<TSource>(Source));
end;

class function TInMemory.Rows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>;
begin
  Result := ExecuteToRows(Query, Context);
end;

class function TInMemory.Project<TResult>(
  const Query: IQuery; const Context: IQueryContext): IEnumerable<TResult>;
begin
  // The rows are already projected; materialize maps each output column to a member.
  var Q: TQuery<IRow> := ExecuteToRows(Query, Context);
  var Writer: IResultWriter<TResult> := TResultWriter<TResult>.Create;
  Result := Q.Select<TResult>(
    function(R: IRow): TResult
    begin
      Result := Writer.Materialize(R, nil);
    end).AsEnumerable;
end;

end.
