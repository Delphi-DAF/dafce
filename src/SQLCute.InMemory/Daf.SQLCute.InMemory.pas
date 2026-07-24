unit Daf.SQLCute.InMemory;

{
  Daf.SQLCute.InMemory — the in-memory compiler: a second SQLCute back-end
  (analogue of IQueryCompiler) that consumes an IQuery AST and emits an
  executable plan over in-memory collections via Daf.Linq, instead of SQL text.

  This unit is the generic half of the single-source fast lane (Decision 5):
    * TRttiAccessors<T> — cached (column → member) RTTI accessors producing the
      TColumnGetter over T that the non-generic predicate/comparers consume.
    * TInMemoryPlan<T> — wraps a TCompiledQuery and drives Daf.Linq operators
      (Where / Distinct / OrderBy / Skip / Take), preserving T end-to-end and
      never allocating an IRow per element.
    * TInMemory — the façade: Compile<T> builds a reusable plan; Queryable<T> is
      sugar over Compile<T>(Q).Over(source), returning IQueryable<T> — the same
      type Linq.From yields, so the two front-ends converge.

  Depends on Daf.Linq (the sequence engine) + Daf.SQLCute (the AST) + Commons
  (Daf.Rtti / Daf.Nullable / Daf.Comparers). The AST walker, three-valued
  predicate and value currency live in Daf.SQLCute.InMemory.Compiler. The IRow
  model, IQueryContext and RTTI materializer arrive in Phases 2-3.
}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  Daf.Linq,
  Daf.SQLCute,
  Daf.SQLCute.InMemory.Compiler;

type
  /// <summary>Column value access over <c>T</c> — ref-counted so it outlives the plan.</summary>
  IRttiAccessors<T> = interface
    function Get(const Column: string; const Item: T): TValue;
    function AllColumns: TArray<string>;
  end;

  /// <summary>
  ///   Cached RTTI column accessors for element type <c>T</c>: resolves a
  ///   (optionally qualified) column name to a readable property/field once and
  ///   reads it as a <c>TValue</c> — the fast lane's zero-wrapper value access.
  ///   Interface-referenced: the deferred pipeline captures the interface, so
  ///   the accessors live as long as the query, not as long as the plan.
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
    /// <summary>The value of <c>Column</c> on <c>Item</c> (empty TValue for a nil object).</summary>
    function Get(const Column: string; const Item: T): TValue;
    /// <summary>All RTTI-visible member names — the DISTINCT key set (Decision 12).</summary>
    function AllColumns: TArray<string>;
  end;

  /// <summary>A reusable in-memory execution plan; run it over any source of <c>T</c>.</summary>
  IInMemoryPlan<T> = interface
    function Over(const Source: TEnumerable<T>): IQueryable<T>; overload;
    function Over(const Source: TArray<T>): IQueryable<T>; overload;
    function Over(const Source: IEnumerable<T>): IQueryable<T>; overload;
  end;

  /// <summary>Concrete plan: composes Daf.Linq operators from a <c>TCompiledQuery</c>.</summary>
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
    /// <summary>Compiles <c>Query</c> into a reusable plan (single-source fast lane).</summary>
    class function Compile<T>(const Query: IQuery): IInMemoryPlan<T>; static;
    /// <summary>
    ///   Fabricates the <c>IQueryable&lt;T&gt;</c> for <c>Query</c> over <c>Source</c> —
    ///   sugar over <c>Compile&lt;T&gt;(Query).Over(Source)</c>. Named after its product
    ///   (a queryable), not to be confused with building an <c>IQuery</c>.
    /// </summary>
    class function Queryable<T>(const Query: IQuery; const Source: TEnumerable<T>): IQueryable<T>; overload; static;
    class function Queryable<T>(const Query: IQuery; const Source: TArray<T>): IQueryable<T>; overload; static;
    class function Queryable<T>(const Query: IQuery; const Source: IEnumerable<T>): IQueryable<T>; overload; static;
  end;

implementation

uses
  System.TypInfo,   // TMemberVisibility (mvPublic / mvPublished)
  Daf.Comparers;   // TValueComparers

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
  FCache.AddOrSetValue(Key, M);   // cache misses too (nil)
  Result := M;
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
  var Q := Start;

  // WHERE — collapse the tri-state predicate to True-only inclusion.
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

  // DISTINCT before ORDER BY / paging (SQL logical order).
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

end.
