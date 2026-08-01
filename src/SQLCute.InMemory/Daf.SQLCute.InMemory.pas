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
        Queryable<T>          preserve T (fast lane, no projection)
        Rows<T>               yield IQueryable<IRow> (dynamic / projected)
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
    function Over(const Source: TEnumerable<T>): IQueryable<T>; overload;
    function Over(const Source: TArray<T>): IQueryable<T>; overload;
    function Over(const Source: IEnumerable<T>): IQueryable<T>; overload;
  end;

  /// <summary>Concrete preserve-T plan: composes Daf.Linq operators from a <c>TCompiledQuery</c>.</summary>
  TInMemoryPlan<T> = class(TInterfacedObject, IInMemoryPlan<T>)
  private
    FCompiled: TCompiledQuery;
    FAccessors: IRttiAccessors<T>;
    function Apply(const Start: TQuery<T>): IQueryable<T>;
    function OrderComparer: IComparer<T>;
    function DistinctComparer: IEqualityComparer<T>;
  public
    /// <summary>Takes ownership of <c>Compiled</c>.</summary>
    constructor Create(const Compiled: TCompiledQuery);
    destructor Destroy; override;
    function Over(const Source: TEnumerable<T>): IQueryable<T>; overload;
    function Over(const Source: TArray<T>): IQueryable<T>; overload;
    function Over(const Source: IEnumerable<T>): IQueryable<T>; overload;
  end;

  /// <summary>Entry point: compile a SQLCute AST onto the in-memory engine.</summary>
  TInMemory = record
  strict private
    class function MapToRows<T>(const Start: TQuery<T>): TQuery<IRow>; static;
    class function BuildRowPipeline<T>(const C: TCompiledQuery; const Start: TQuery<T>): TQuery<IRow>; static;
    class function RowsCore<T>(const Query: IQuery; const Start: TQuery<T>): IQueryable<IRow>; static;
    class function ProjectCore<TSource; TResult: class, constructor>(
      const Query: IQuery; const Start: TQuery<TSource>): IQueryable<TResult>; static;
  public
    /// <summary>Compiles <c>Query</c> into a reusable preserve-T plan (single-source fast lane).</summary>
    class function Compile<T>(const Query: IQuery): IInMemoryPlan<T>; static;
    /// <summary>
    ///   Fabricates the <c>IQueryable&lt;T&gt;</c> for <c>Query</c> over <c>Source</c> preserving
    ///   <c>T</c> — sugar over <c>Compile&lt;T&gt;(Query).Over(Source)</c>. Raises if the query
    ///   projects (use <see cref="Rows"/> / <see cref="Project"/> instead).
    /// </summary>
    class function Queryable<T>(const Query: IQuery; const Source: TEnumerable<T>): IQueryable<T>; overload; static;
    class function Queryable<T>(const Query: IQuery; const Source: TArray<T>): IQueryable<T>; overload; static;
    class function Queryable<T>(const Query: IQuery; const Source: IEnumerable<T>): IQueryable<T>; overload; static;
    /// <summary>Runs <c>Query</c> over <c>Source</c> yielding dynamic <c>IRow</c>s (projected when the query has a SELECT).</summary>
    class function Rows<T>(const Query: IQuery; const Source: TEnumerable<T>): IQueryable<IRow>; overload; static;
    class function Rows<T>(const Query: IQuery; const Source: TArray<T>): IQueryable<IRow>; overload; static;
    class function Rows<T>(const Query: IQuery; const Source: IEnumerable<T>): IQueryable<IRow>; overload; static;
    /// <summary>Runs <c>Query</c> over <c>Source</c> materializing each final row into <c>TResult</c>.</summary>
    class function Project<TSource; TResult: class, constructor>(
      const Query: IQuery; const Source: TEnumerable<TSource>): IQueryable<TResult>; overload; static;
    class function Project<TSource; TResult: class, constructor>(
      const Query: IQuery; const Source: TArray<TSource>): IQueryable<TResult>; overload; static;
    class function Project<TSource; TResult: class, constructor>(
      const Query: IQuery; const Source: IEnumerable<TSource>): IQueryable<TResult>; overload; static;
  end;

/// <summary>
///   Assigns a column value to a result-class member, coercing the <c>TValue</c>
///   to the member's type. A NULL source leaves the member at its default (a
///   <c>TNullable</c> member stays <c>IsNull</c>); a <c>TNullable&lt;X&gt;</c>
///   member is built through its <c>Implicit(X)</c> operator. Declared here (not
///   implementation-local) so the generic <c>TResultWriter</c> can reference it.
/// </summary>
procedure AssignMember(const Instance: TObject; M: TRttiMember; const Source: TValue);

/// <summary>DISTINCT output columns on the pre-projection row (projection sources, or nil = all).</summary>
function DistinctColumns(const C: TCompiledQuery): TArray<string>;

implementation

uses
  System.TypInfo,    // TMemberVisibility (mvPublic / mvPublished)
  Daf.Comparers,     // TValueComparers, IsNullValue
  Daf.Rtti;          // TRttiProperty smart SetValue + CoerceForAssign (shared coercion)

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
  var Key := LowerCase(BareColumn(Column));
  if FCache.TryGetValue(Key, M) then
    Exit(M);
  M := nil;
  if FType <> nil then
  begin
    for var P in FType.GetProperties do
      if SameText(P.Name, Key) then
      begin
        M := P;
        Break;
      end;
    if M = nil then
      for var F in FType.GetFields do
        if SameText(F.Name, Key) then
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
  var Key := LowerCase(Name);
  if FCache.TryGetValue(Key, M) then
    Exit(M);
  M := nil;
  if FType <> nil then
  begin
    for var P in FType.GetProperties do
      if SameText(P.Name, Name) and P.IsWritable then
      begin
        M := P;
        Break;
      end;
    if M = nil then
      for var F in FType.GetFields do
        if SameText(F.Name, Name) then
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

function TInMemoryPlan<T>.Apply(const Start: TQuery<T>): IQueryable<T>;
begin
  if FCompiled.Projection <> nil then
    raise EInMemoryQueryError.Create(
      'This query has a SELECT projection; use TInMemory.Rows<T> or '
      + 'TInMemory.Project<TSource, TResult> instead of Queryable<T>.');

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

  Result := Q.AsQueryable;
end;

function TInMemoryPlan<T>.Over(const Source: TEnumerable<T>): IQueryable<T>;
begin
  Result := Apply(Linq.From<T>(Source));
end;

function TInMemoryPlan<T>.Over(const Source: TArray<T>): IQueryable<T>;
begin
  Result := Apply(Linq.From<T>(Source));
end;

function TInMemoryPlan<T>.Over(const Source: IEnumerable<T>): IQueryable<T>;
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

class function TInMemory.RowsCore<T>(const Query: IQuery; const Start: TQuery<T>): IQueryable<IRow>;
begin
  var C := TInMemoryCompiler.Compile(Query);
  try
    var Proj := C.Projection;
    Result := BuildRowPipeline<T>(C, Start).Select<IRow>(
      function(R: IRow): IRow
      begin
        Result := ProjectRow(R, Proj);
      end).AsQueryable;
  finally
    C.Free;   // predicate/orders/projection are captured in the closures above
  end;
end;

class function TInMemory.ProjectCore<TSource, TResult>(
  const Query: IQuery; const Start: TQuery<TSource>): IQueryable<TResult>;
begin
  var C := TInMemoryCompiler.Compile(Query);
  try
    var Proj := C.Projection;
    var Writer: IResultWriter<TResult> := TResultWriter<TResult>.Create;
    Result := BuildRowPipeline<TSource>(C, Start).Select<TResult>(
      function(R: IRow): TResult
      begin
        Result := Writer.Materialize(R, Proj);
      end).AsQueryable;
  finally
    C.Free;
  end;
end;

class function TInMemory.Compile<T>(const Query: IQuery): IInMemoryPlan<T>;
begin
  Result := TInMemoryPlan<T>.Create(TInMemoryCompiler.Compile(Query));
end;

class function TInMemory.Queryable<T>(const Query: IQuery; const Source: TEnumerable<T>): IQueryable<T>;
begin
  Result := Compile<T>(Query).Over(Source);
end;

class function TInMemory.Queryable<T>(const Query: IQuery; const Source: TArray<T>): IQueryable<T>;
begin
  Result := Compile<T>(Query).Over(Source);
end;

class function TInMemory.Queryable<T>(const Query: IQuery; const Source: IEnumerable<T>): IQueryable<T>;
begin
  Result := Compile<T>(Query).Over(Source);
end;

class function TInMemory.Rows<T>(const Query: IQuery; const Source: TEnumerable<T>): IQueryable<IRow>;
begin
  Result := RowsCore<T>(Query, Linq.From<T>(Source));
end;

class function TInMemory.Rows<T>(const Query: IQuery; const Source: TArray<T>): IQueryable<IRow>;
begin
  Result := RowsCore<T>(Query, Linq.From<T>(Source));
end;

class function TInMemory.Rows<T>(const Query: IQuery; const Source: IEnumerable<T>): IQueryable<IRow>;
begin
  Result := RowsCore<T>(Query, Linq.From<T>(Source));
end;

class function TInMemory.Project<TSource, TResult>(
  const Query: IQuery; const Source: TEnumerable<TSource>): IQueryable<TResult>;
begin
  Result := ProjectCore<TSource, TResult>(Query, Linq.From<TSource>(Source));
end;

class function TInMemory.Project<TSource, TResult>(
  const Query: IQuery; const Source: TArray<TSource>): IQueryable<TResult>;
begin
  Result := ProjectCore<TSource, TResult>(Query, Linq.From<TSource>(Source));
end;

class function TInMemory.Project<TSource, TResult>(
  const Query: IQuery; const Source: IEnumerable<TSource>): IQueryable<TResult>;
begin
  Result := ProjectCore<TSource, TResult>(Query, Linq.From<TSource>(Source));
end;

end.
