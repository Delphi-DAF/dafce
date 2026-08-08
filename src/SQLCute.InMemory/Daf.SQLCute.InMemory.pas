unit Daf.SQLCute.InMemory;

{
  Daf.SQLCute.InMemory — the in-memory compiler: a second SQLCute back-end
  (analogue of IQueryCompiler) that consumes an IQuery AST and emits an
  executable plan over in-memory collections via Daf.Linq, instead of SQL text.

  Generic half of the engine:
    * TRttiAccessors<T> — cached (column -> member) RTTI accessors producing the
      TColumnGetter the non-generic predicate/comparers consume (fast lane).
    * TRttiRow<T> — a zero-copy IRow view over an object/record T, reusing the
      accessors (general lane, Decision 4).
    * TResultWriter<TResult> — the RTTI materializer IRow -> TResult (column/alias
      -> member, TValue coercion, TNullable via its Implicit operator).
    * TInMemoryPlan<T> / TInMemory — the reusable plan and the façade:
        Elements<T>           preserve T (fast lane, no projection)
        Rows<T>               yield IEnumerable<IRow> (dynamic / projected)
        Project<TSrc,TResult> materialize into a result class

  Depends on Daf.Linq (the sequence engine), Daf.SQLCute (the AST), the shared
  non-generic core (Daf.SQLCute.InMemory.Compiler) and the row model
  (Daf.SQLCute.InMemory.Rows), plus Commons (Daf.Comparers). The IQueryContext,
  TJoinRow and set/aggregate machinery arrive in Phase 3.
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
  ///   Cached RTTI column accessors for element type <c>T</c>: resolves a
  ///   (optionally qualified) column name to a readable property/field once and
  ///   reads it as a <c>TValue</c> — the fast lane's zero-wrapper value access.
  /// </summary>
  TRttiAccessors<T> = class(TInterfacedObject, IRttiAccessors<T>)
  private
    FCtx: TRttiContext;
    FType: TRttiType;
    FIsObject: Boolean;
    FCache: TDictionary<string, TRttiMember>;
    FAllColumns: TArray<string>;
    FAllResolved: Boolean;
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
  ///   Per-call catalog mapping source names to row sequences (Decision 6 — never
  ///   global). Supplied at execution time for the general multi-source lane.
  /// </summary>
  IQueryContext = interface
    ['{B4E6F2A3-1D5C-4E7F-9A82-3C4D5E6F7081}']
    /// <summary>The rows for a named source; raises when the name is unmapped.</summary>
    function Resolve(const Name: string): TArray<IRow>;
    /// <summary>True when a source is mapped to <c>Name</c>.</summary>
    function Has(const Name: string): Boolean;
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
  public
    constructor Create;
    destructor Destroy; override;
    function Map<T>(const Name: string; const Source: TEnumerable<T>): TQueryContext; overload;
    function Map<T>(const Name: string; const Source: TArray<T>): TQueryContext; overload;
    function Map(const Name: string; const Rows: TArray<IRow>): TQueryContext; overload;
    function Resolve(const Name: string): TArray<IRow>;
    function Has(const Name: string): Boolean;
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
  Daf.Rtti;                // TRttiProperty smart SetValue + CoerceForAssign (shared coercion)

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
end;

destructor TRttiAccessors<T>.Destroy;
begin
  FCache.Free;
  FCtx.Free;
  inherited;
end;

function TRttiAccessors<T>.Resolve(const Column: string): TRttiMember;
var
  M: TRttiMember;
begin
  var Key := NormalizeName(Column);   // snake_case ↔ PascalCase bridge
  if FCache.TryGetValue(Key, M) then
    Exit(M);
  M := nil;
  if FType <> nil then
  begin
    for var P in FType.GetProperties do
      if NormalizeName(P.Name) = Key then
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
end;

function TRttiAccessors<T>.AllColumns: TArray<string>;
begin
  if not FAllResolved then
  begin
    FAllColumns := nil;
    if FType <> nil then
    begin
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
    FAllResolved := True;
  end;
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
      Assign(Result, P.Output, Row.Get(P.Source));
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
  var VE := TValueComparers.Equality;   // NULLs equal (Decision 10)
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
end;

destructor TQueryContext.Destroy;
begin
  FSources.Free;
  inherited;
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

function TQueryContext.Map<T>(const Name: string; const Source: TEnumerable<T>): TQueryContext;
begin
  var Rows: TArray<IRow> := nil;
  if TypeInfo(T) = TypeInfo(IRow) then
  begin
    for var X in Source do
      Rows := Rows + [TValue.From<T>(X).AsType<IRow>];
  end
  else
  begin
    var Acc: IRttiAccessors<T> := TRttiAccessors<T>.Create;
    for var X in Source do
      Rows := Rows + [TRttiRow<T>.Create(Acc, X)];
  end;
  FSources.AddOrSetValue(LowerCase(Name), Rows);
  Result := Self;
end;

function TQueryContext.Map<T>(const Name: string; const Source: TArray<T>): TQueryContext;
begin
  var Rows: TArray<IRow> := nil;
  if TypeInfo(T) = TypeInfo(IRow) then
  begin
    for var X in Source do
      Rows := Rows + [TValue.From<T>(X).AsType<IRow>];
  end
  else
  begin
    var Acc: IRttiAccessors<T> := TRttiAccessors<T>.Create;
    for var X in Source do
      Rows := Rows + [TRttiRow<T>.Create(Acc, X)];
  end;
  FSources.AddOrSetValue(LowerCase(Name), Rows);
  Result := Self;
end;

function TQueryContext.Map(const Name: string; const Rows: TArray<IRow>): TQueryContext;
begin
  FSources.AddOrSetValue(LowerCase(Name), Rows);
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

{ multi-source engine (Phase 3A: FROM + JOINs) }

// Fail-fast on the multi-source features not yet handled by this lane.
procedure EnsureMultiSupported(const Clauses: TArray<TAbstractClause>);
begin
  for var C in Clauses do
  begin
    if C is TJoinSubqueryClause then
      raise EInMemoryQueryError.Create('Subquery JOINs are not supported by the in-memory backend.');
    if (C is TFromSubqueryClause) or (C is TFromRawClause) then
      raise EInMemoryQueryError.Create('Derived-table / raw FROM sources require Phase 3C.');
    if C is TWithClause then
      raise EInMemoryQueryError.Create('CTEs (WITH) are not supported by the in-memory backend.');
    if (C is TInsertClause) or (C is TUpdateSetClause) or (C is TDeleteClause) then
      raise EInMemoryQueryError.Create('The in-memory backend is read-only; DML stays SQL-only.');
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
    var Op := ParseWhereOp(Jc.Op);
    var C1 := Jc.Col1;
    var C2 := Jc.Col2;
    Result :=
      function(R: IRow): Boolean
      begin
        Result := CompareOp(Op, R.Get(C1), R.Get(C2)) = TTriState.Yes;
      end;
  end
  else if Jc.Condition <> '' then
    raise EInMemoryQueryError.Create(
      'Raw JOIN ON conditions cannot be evaluated in memory; use a column or callback ON.')
  else
    Result := function(R: IRow): Boolean begin Result := True; end;
end;

// An all-NULL left composite, carrying the same aliases as the accumulated left
// side, for the unmatched right rows of a RIGHT / FULL OUTER join.
function NullLeft(const Left: TArray<IRow>): IRow;
var
  Children: TArray<TJoinChild>;
  JR: IJoinRow;
begin
  Children := nil;
  if (Length(Left) > 0) and Supports(Left[0], IJoinRow, JR) then
    for var Ch in JR.Children do
    begin
      var NC: TJoinChild;
      NC.Alias := Ch.Alias;
      NC.Row := TNullRow.Create;
      Children := Children + [NC];
    end;
  Result := TJoinRow.Create(Children);
end;

function ApplyJoin(const Left: TArray<IRow>; const Context: IQueryContext;
  const Jc: TJoinClause): TArray<IRow>;
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
          Result := Result + [CombineRows(L, RightAlias, TNullRow.Create)];
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
          Result := Result + [CombineRows(NullLeft(Left), RightAlias, R)];
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
              Result := Result + [CombineRows(L, RightAlias, TNullRow.Create)];
          end;
          for var R in Rights do
            if not MatchedRight.ContainsKey(R) then
              Result := Result + [CombineRows(NullLeft(Left), RightAlias, R)];
        finally
          MatchedRight.Free;
        end;
      end;
  end;
end;

{ GROUP BY / aggregates / HAVING (Phase 3B, Decision 11) }

function AsNumber(const V: TValue): Double;
begin
  try
    Result := V.AsVariant;   // numeric / date families coerce to Double
  except
    Result := 0;
  end;
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
          if not IsNullValue(R.Get(Agg.Column)) then
            Inc(N);
        Result := N;
      end;
    TAggFunc.Sum:
      begin
        var S: Double := 0;
        var Any := False;
        for var R in Members do
        begin
          var V := R.Get(Agg.Column);
          if not IsNullValue(V) then
          begin
            S := S + AsNumber(V);
            Any := True;
          end;
        end;
        if Any then Result := S else Result := TValue.Empty;
      end;
    TAggFunc.Avg:
      begin
        var S: Double := 0;
        var N := 0;
        for var R in Members do
        begin
          var V := R.Get(Agg.Column);
          if not IsNullValue(V) then
          begin
            S := S + AsNumber(V);
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
          var V := R.Get(Agg.Column);
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

// Canonical group key: a NULL sentinel keeps all NULL keys in one group.
function GroupKey(const R: IRow; const Cols: TArray<string>): string;
begin
  Result := '';
  for var C in Cols do
  begin
    var V := R.Get(C);
    if IsNullValue(V) then
      Result := Result + #0 + '<null>' + #1
    else
      Result := Result + #0 + VarToStr(V.AsVariant) + #1;
  end;
end;

// Groups rows by the GROUP BY columns, computes aggregates over each group's
// members, applies HAVING, and emits one flat row per surviving group.
// The hybrid of Decision 11: Daf.Linq does the grouping (key + members, its own
// general-purpose shape) and this compiler layers SQL's flat aggregate row on
// top — the SQL shape is not baked into the sequence library.
function GroupAndAggregate(const Rows: TArray<IRow>; const C: TCompiledQuery): TArray<IRow>;
begin
  var Cols := C.GroupBy;
  var Groups := Linq.From<IRow>(Rows).GroupBy<string>(
    function(R: IRow): string
    begin
      Result := GroupKey(R, Cols);   // NULL sentinel: all NULL keys are one group
    end);

  Result := nil;
  for var G in Groups do             // key order preserved by GroupBy
  begin
    var Members := G.Members;
    var Flat := TDictRow.Create;
    for var Gc in Cols do
      Flat.Put(Gc, Members[0].Get(Gc));   // any member carries the group's key values
    for var Agg in C.Aggregates do
      Flat.Put(Agg.Output, ComputeAggregate(Agg, Members));
    var FlatRow: IRow := Flat;
    var Keep := True;
    if Assigned(C.Having) then
      Keep := C.Having(
        function(const Col: string): TValue
        begin
          Result := FlatRow.Get(Col);
        end) = TTriState.Yes;
    if Keep then
      Result := Result + [FlatRow];
  end;
end;

// Eager WHERE filter — the grouping path must materialize before grouping.
function FilterRows(const Rows: TArray<IRow>; const Pred: TValuePredicate): TArray<IRow>;
begin
  if not Assigned(Pred) then
    Exit(Rows);
  Result := nil;
  for var R in Rows do
    if Pred(
      function(const Col: string): TValue
      begin
        Result := R.Get(Col);
      end) = TTriState.Yes then
      Result := Result + [R];
end;

function ProjectedRows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>; forward;

type
  TSubqueryResult = record
    Clause: Pointer;
    Values: TArray<TValue>;
    NonEmpty: Boolean;
  end;

// Resolves an uncorrelated subquery WHERE leaf against the context. Each subquery
// is evaluated once and cached by clause identity; IN/NOT IN test membership in the
// subquery's first output column, EXISTS/NOT EXISTS test non-emptiness (3VL).
function MakeSubqueryResolver(const Context: IQueryContext): TSubqueryResolver;
var
  Cache: TArray<TSubqueryResult>;   // captured by the closure (managed; freed with it)
begin
  Cache := nil;
  Result :=
    function(const Clause: TWhereClause; const Get: TColumnGetter): TTriState
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
        var SR: TSubqueryResult;
        SR.Clause := Pointer(Clause);
        SR.Values := nil;
        var SubRows := TQuery<IRow>(ProjectedRows(Clause.SubQuery as IQuery, Context)).ToArray;
        SR.NonEmpty := Length(SubRows) > 0;
        for var R in SubRows do
        begin
          var Cols := R.Columns;
          if Length(Cols) > 0 then
            SR.Values := SR.Values + [R.Get(Cols[0])];
        end;
        Cache := Cache + [SR];
        Idx := High(Cache);
      end;

      if (Clause.Op = TWhereOp.&Exists) or (Clause.Op = TWhereOp.NotExists) then
        Exit(BoolToTri(Cache[Idx].NonEmpty = (Clause.Op = TWhereOp.&Exists)));

      // IN / NOT IN — 3VL membership against the subquery's values.
      var ColVal := Get(Clause.Column);
      if IsNullValue(ColVal) then
        Exit(TTriState.Unknown);
      var Found := False;
      var AnyNull := False;
      for var V in Cache[Idx].Values do
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
// returned via Proj for the caller to apply (dynamic IRow) or materialize.
function ExecuteMultiCore(const Query: IQuery; const Context: IQueryContext;
  out Proj: TArray<TProjColumn>): IEnumerable<IRow>;
begin
  var Clauses := Query.Clauses;
  EnsureMultiSupported(Clauses);

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

  for var C in Clauses do
    if C is TJoinClause then
      Composite := ApplyJoin(Composite, Context, TJoinClause(C));

  // Build the predicate with a subquery resolver in scope; the built predicate
  // captures it, so the global is cleared right after compilation.
  UseSubqueryResolver(MakeSubqueryResolver(Context));
  var Cq: TCompiledQuery;
  try
    Cq := TInMemoryCompiler.CompileMulti(Query);
  finally
    UseSubqueryResolver(nil);
  end;
  try
    var Q: TQuery<IRow>;
    if Length(Cq.GroupBy) > 0 then
    begin
      // WHERE runs before grouping; the flat aggregate rows are the output.
      Q := Linq.From<IRow>(GroupAndAggregate(FilterRows(Composite, Cq.Predicate), Cq));
      Proj := nil;
    end
    else
    begin
      Q := Linq.From<IRow>(Composite);
      if Assigned(Cq.Predicate) then
        Q := Q.Where(MakeRowPredicate(Cq.Predicate));
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

// The main query's final projected rows (FROM+JOINs+WHERE+GROUP BY, then SELECT).
function ProjectedRows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>;
begin
  var Proj: TArray<TProjColumn>;
  var Q: TQuery<IRow> := ExecuteMultiCore(Query, Context, Proj);
  Result := Q.Select<IRow>(
    function(R: IRow): IRow
    begin
      Result := ProjectRow(R, Proj);
    end).AsEnumerable;
end;

{ set operations (Phase 3C) — row equality by output columns, NULLs equal }

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
      var Other := TQuery<IRow>(ProjectedRows(U.SubQuery as IQuery, Context)).ToArray;
      Acc := CombineSets(U.Kind, Acc, Other);
    end;
  Result := Linq.From<IRow>(Acc);
end;

function ExecuteToRows(const Query: IQuery; const Context: IQueryContext): IEnumerable<IRow>;
begin
  Result := ApplyUnions(ProjectedRows(Query, Context), Query, Context);
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
