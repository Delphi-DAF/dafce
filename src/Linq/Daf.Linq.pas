unit Daf.Linq;

{
  Daf.Linq — a standalone, lazy, ref-counted collection-transformation library
  (LINQ-to-Objects / Kotlin sequences / Rust iterators in spirit). Knows nothing
  about SQL; built on Commons' Daf.Enumerable.

  The sequence type is System.IEnumerable<T> itself, deliberately: this is the
  closure world, the counterpart of LINQ-to-Objects, and .NET names it
  IEnumerable<T>. The name IQueryable<T> is reserved for what it means in .NET —
  a composable tree deferred to a back-end — which DAF does not have yet.

  So there is one type of our own:
    * TQuery<T> — the fluent *operator surface* (a value-type wrapper over an
      IEnumerable<T>). Delphi interfaces cannot declare generic methods, so the
      generic operators (Select/Map/Zip/GroupBy) live here rather than on the
      interface. TQuery<T> converts implicitly to/from IEnumerable<T> and is
      itself for..in-able.

  Deferred by default: Where/Select/Take/Skip run element-by-element on
  iteration (Take short-circuits); OrderBy/Distinct/GroupBy materialize once at
  the start of enumeration (they need every element), but still nothing runs
  until iteration begins.
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  Daf.Enumerable,
  Daf.Expression,
  Daf.Arrays;

type
  /// <summary>Raised by the terminals that assert a shape (see <c>Single</c>).</summary>
  ELinqError = class(Exception);

  /// <summary>A group produced by <c>GroupBy</c>: a key plus its member elements.</summary>
  IGrouping<TKey, T> = interface(IEnumerable<T>)
    function GetKey: TKey;
    /// <summary>The elements that fell into this group.</summary>
    function Members: TArray<T>;
    property Key: TKey read GetKey;
  end;

  /// <summary>
  ///   Base that solves the <c>System.IEnumerable&lt;T&gt;</c> plumbing (the
  ///   non-generic <c>GetCurrent: TObject</c> baggage) via the method-resolution
  ///   clause — the proven <c>Daf.Enumerable</c> pattern.
  /// </summary>
  TQueryableBase<T> = class(TInterfacedObject, IEnumerable, IEnumerable<T>)
  protected
    function DoGetEnumerator: IEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: IEnumerator; overload;
    function IEnumerable<T>.GetEnumerator = GetGenericEnumerator;
    function GetGenericEnumerator: IEnumerator<T>;
  end;

  /// <summary>A lazy <c>IEnumerable&lt;T&gt;</c> wrapper over another <c>IEnumerable&lt;T&gt;</c>.</summary>
  TQueryable<T> = class(TQueryableBase<T>)
  private
    FSource: IEnumerable<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(const Source: IEnumerable<T>);
  end;

  /// <summary>Concrete <c>IGrouping&lt;TKey,T&gt;</c> — a key and its captured members.</summary>
  TGrouping<TKey, T> = class(TQueryableBase<T>, IGrouping<TKey, T>)
  private
    FKey: TKey;
    FMembers: TArray<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(const Key: TKey; const Members: TArray<T>);
    function GetKey: TKey;
    function Members: TArray<T>;
  end;

  /// <summary>
  ///   The fluent operator surface over an <c>IEnumerable&lt;T&gt;</c>. A value type:
  ///   copy-cheap, ref-counted through its inner sequence. Converts implicitly to
  ///   <c>IEnumerable&lt;T&gt;</c> and is for..in-able.
  /// </summary>
  TQuery<T> = record
  private
    FSeq: IEnumerable<T>;
  public
    class operator Implicit(const Seq: IEnumerable<T>): TQuery<T>;
    class operator Implicit(const Query: TQuery<T>): IEnumerable<T>;
    /// <summary>Enables <c>for..in</c> over the query directly.</summary>
    function GetEnumerator: IEnumerator<T>;
    /// <summary>The underlying deferred sequence.</summary>
    function AsEnumerable: IEnumerable<T>;

    /// <summary>
    ///   Keeps only elements matching the filter (lazy). Takes Commons'
    ///   <c>TFilterSpec&lt;T&gt;</c> — the same type the eager
    ///   <c>TSmartArray&lt;T&gt;.Where</c> takes — so one filter value serves both
    ///   APIs and can be composed with <c>and</c> / <c>or</c> / <c>not</c>
    ///   beforehand. A bare closure still works: it converts implicitly.
    /// </summary>
    function Where(const Spec: TFilterSpec<T>): TQuery<T>;
    /// <summary>Alias of <see cref="Where"/>.</summary>
    function Filter(const Spec: TFilterSpec<T>): TQuery<T>;
    /// <summary>Projects each element through the mapper (lazy).</summary>
    function Select<TResult>(const Mapper: TFunc<T, TResult>): TQuery<TResult>;
    /// <summary>Alias of <see cref="Select"/>.</summary>
    function Map<TResult>(const Mapper: TFunc<T, TResult>): TQuery<TResult>;
    /// <summary>Yields at most <c>Count</c> elements, then stops consuming the source.</summary>
    function Take(Count: Integer): TQuery<T>;
    /// <summary>Skips the first <c>Count</c> elements (lazy).</summary>
    function Skip(Count: Integer): TQuery<T>;
    /// <summary>De-duplicates by value; pass a NULL-aware comparer for SQL semantics.</summary>
    function Distinct(const Comparer: IEqualityComparer<T> = nil): TQuery<T>;
    /// <summary>Orders elements by the comparer (materialized on iteration).</summary>
    function OrderBy(const Comparer: IComparer<T>): TQuery<T>;
    /// <summary>Pairs elements with another sequence element-wise until either ends.</summary>
    function Zip<TOther, TResult>(const Other: IEnumerable<TOther>;
      const Zipper: TFunc<T, TOther, TResult>): TQuery<TResult>;
    /// <summary>Groups elements by key (key order preserved).</summary>
    function GroupBy<TKey>(const KeySelector: TFunc<T, TKey>;
      const Comparer: IEqualityComparer<TKey> = nil): IEnumerable<IGrouping<TKey, T>>;
    // --- terminals ---------------------------------------------------------
    // All of these enumerate without consuming the query: the same TQuery<T>
    // value stays usable afterwards and a terminal may be called again. Note
    // that each call re-enumerates the source, so with an expensive or
    // single-shot source materialize once (ToArray/ToList) and query that.

    /// <summary>True when the sequence produces no element (stops at the first).</summary>
    function IsEmpty: Boolean;
    /// <summary>True when at least one element matches (stops at the first match).</summary>
    function Any(const Spec: TFilterSpec<T>): Boolean;
    /// <summary>The first element, or <c>Default(T)</c> when the sequence is empty.</summary>
    function FirstOrDefault: T; overload;
    /// <summary>The first element, or <c>Fallback</c> when the sequence is empty.</summary>
    function FirstOrDefault(const Fallback: T): T; overload;
    /// <summary>
    ///   The first element with an explicit found/not-found result — the only
    ///   form that tells an empty sequence from one whose first element equals
    ///   <c>Default(T)</c>.
    /// </summary>
    function TryFirst(out Value: T): Boolean;
    /// <summary>The last element, or <c>Default(T)</c> when the sequence is empty.</summary>
    function LastOrDefault: T; overload;
    /// <summary>The last element, or <c>Fallback</c> when the sequence is empty.</summary>
    function LastOrDefault(const Fallback: T): T; overload;
    /// <summary>
    ///   The one and only element. Raises <see cref="ELinqError"/> when the
    ///   sequence is empty or holds more than one — use <see cref="FirstOrDefault"/>
    ///   when "the first of possibly many" is what you mean.
    /// </summary>
    function Single: T;
    /// <summary>
    ///   The only element, or <c>Default(T)</c> when the sequence is empty. Still
    ///   raises when it holds more than one.
    /// </summary>
    function SingleOrDefault: T; overload;
    /// <summary>As <see cref="SingleOrDefault"/>, with an explicit empty-sequence fallback.</summary>
    function SingleOrDefault(const Fallback: T): T; overload;
    /// <summary>Non-raising <see cref="Single"/>: False unless there is exactly one element.</summary>
    function TrySingle(out Value: T): Boolean;
    /// <summary>Materializes the sequence into a new array.</summary>
    function ToArray: TArray<T>;
    /// <summary>Materializes the sequence into a new (caller-owned) list.</summary>
    function ToList: TList<T>; overload;
    /// <summary>Appends the sequence to a list the caller owns (ownership is not taken).</summary>
    procedure ToList(const List: TList<T>); overload;
    /// <summary>Materializes into Commons' eager <c>TSmartArray&lt;T&gt;</c> (the lazy→eager bridge).</summary>
    function ToSmartArray: TSmartArray<T>;
  end;

  /// <summary>
  ///   Entry point into the sequence engine — builds a <c>TQuery&lt;T&gt;</c> over
  ///   an in-memory source with typed closures. All overloads are non-owning and
  ///   lazy.
  /// </summary>
  Linq = record
  public
    class function From<T>(const Source: IEnumerable<T>): TQuery<T>; overload; static;
    class function From<T>(const Source: TEnumerable<T>): TQuery<T>; overload; static;
    class function From<T>(const Source: TArray<T>): TQuery<T>; overload; static;
    /// <summary>Enters the engine over Commons' eager <c>TSmartArray&lt;T&gt;</c> (the eager→lazy bridge).</summary>
    class function From<T>(const Source: TSmartArray<T>): TQuery<T>; overload; static;
  end;

implementation

{ TQueryableBase<T> }

function TQueryableBase<T>.GetEnumerator: IEnumerator;
begin
  Result := DoGetEnumerator;
end;

function TQueryableBase<T>.GetGenericEnumerator: IEnumerator<T>;
begin
  Result := DoGetEnumerator;
end;

{ TQueryable<T> }

constructor TQueryable<T>.Create(const Source: IEnumerable<T>);
begin
  inherited Create;
  FSource := Source;
end;

function TQueryable<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := FSource.GetEnumerator;
end;

{ TGrouping<TKey, T> }

constructor TGrouping<TKey, T>.Create(const Key: TKey; const Members: TArray<T>);
begin
  inherited Create;
  FKey := Key;
  FMembers := Members;
end;

function TGrouping<TKey, T>.GetKey: TKey;
begin
  Result := FKey;
end;

function TGrouping<TKey, T>.Members: TArray<T>;
begin
  Result := FMembers;
end;

function TGrouping<TKey, T>.DoGetEnumerator: IEnumerator<T>;
var
  Idx: Integer;
begin
  var Data := FMembers;
  Idx := -1;
  Result := TGeneratorEnumerator<T>.Create(
    function(var Current: T): Boolean
    begin
      Inc(Idx);
      Result := Idx <= High(Data);
      if Result then
        Current := Data[Idx];
    end);
end;

{ TQuery<T> }

class operator TQuery<T>.Implicit(const Seq: IEnumerable<T>): TQuery<T>;
begin
  Result.FSeq := Seq;
end;

class operator TQuery<T>.Implicit(const Query: TQuery<T>): IEnumerable<T>;
begin
  Result := Query.FSeq;
end;

function TQuery<T>.AsEnumerable: IEnumerable<T>;
begin
  Result := FSeq;
end;

function TQuery<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := FSeq.GetEnumerator;
end;

function TQuery<T>.Where(const Spec: TFilterSpec<T>): TQuery<T>;
begin
  var Src := FSeq;
  var Filter := Spec;   // local copy: a const parameter is not safely captured
  Result.FSeq := TQueryable<T>.Create(TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    var
      Inner: IEnumerator<T>;
    begin
      Inner := Src.GetEnumerator;
      Result := TGeneratorEnumerator<T>.Create(
        function(var Current: T): Boolean
        begin
          while Inner.MoveNext do
            if Filter.Eval(Inner.Current) then
            begin
              Current := Inner.Current;
              Exit(True);
            end;
          Result := False;
        end);
    end));
end;

function TQuery<T>.Filter(const Spec: TFilterSpec<T>): TQuery<T>;
begin
  Result := Where(Spec);
end;

function TQuery<T>.Select<TResult>(const Mapper: TFunc<T, TResult>): TQuery<TResult>;
begin
  var Src := FSeq;
  Result.FSeq := TQueryable<TResult>.Create(TDeferredEnumerable<TResult>.Create(
    function: IEnumerator<TResult>
    var
      Inner: IEnumerator<T>;
    begin
      Inner := Src.GetEnumerator;
      Result := TGeneratorEnumerator<TResult>.Create(
        function(var Current: TResult): Boolean
        begin
          Result := Inner.MoveNext;
          if Result then
            Current := Mapper(Inner.Current);
        end);
    end));
end;

function TQuery<T>.Map<TResult>(const Mapper: TFunc<T, TResult>): TQuery<TResult>;
begin
  Result := Select<TResult>(Mapper);
end;

function TQuery<T>.Take(Count: Integer): TQuery<T>;
begin
  var Src := FSeq;
  var Limit := Count;
  Result.FSeq := TQueryable<T>.Create(TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    var
      Inner: IEnumerator<T>;
      Taken: Integer;
    begin
      Inner := Src.GetEnumerator;
      Taken := 0;
      Result := TGeneratorEnumerator<T>.Create(
        function(var Current: T): Boolean
        begin
          if Taken >= Limit then
            Exit(False);            // short-circuit: stop consuming the source
          Result := Inner.MoveNext;
          if Result then
          begin
            Current := Inner.Current;
            Inc(Taken);
          end;
        end);
    end));
end;

function TQuery<T>.Skip(Count: Integer): TQuery<T>;
begin
  var Src := FSeq;
  var Drop := Count;
  Result.FSeq := TQueryable<T>.Create(TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    var
      Inner: IEnumerator<T>;
      Skipped: Integer;
    begin
      Inner := Src.GetEnumerator;
      Skipped := 0;
      Result := TGeneratorEnumerator<T>.Create(
        function(var Current: T): Boolean
        begin
          while (Skipped < Drop) and Inner.MoveNext do
            Inc(Skipped);
          Result := Inner.MoveNext;
          if Result then
            Current := Inner.Current;
        end);
    end));
end;

function TQuery<T>.Distinct(const Comparer: IEqualityComparer<T>): TQuery<T>;
begin
  var Src := FSeq;
  var Cmp := Comparer;
  if Cmp = nil then
    Cmp := TEqualityComparer<T>.Default;
  Result.FSeq := TQueryable<T>.Create(TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    var
      Inner: IEnumerator<T>;
      Acc: TArray<T>;
      Idx: Integer;
    begin
      var Seen := TDictionary<T, Byte>.Create(Cmp);
      var Lst := TList<T>.Create;
      try
        Inner := Src.GetEnumerator;
        while Inner.MoveNext do
          if not Seen.ContainsKey(Inner.Current) then
          begin
            Seen.Add(Inner.Current, 0);
            Lst.Add(Inner.Current);
          end;
        Acc := Lst.ToArray;
      finally
        Lst.Free;
        Seen.Free;
      end;
      Idx := -1;
      Result := TGeneratorEnumerator<T>.Create(
        function(var Current: T): Boolean
        begin
          Inc(Idx);
          Result := Idx <= High(Acc);
          if Result then
            Current := Acc[Idx];
        end);
    end));
end;

function TQuery<T>.OrderBy(const Comparer: IComparer<T>): TQuery<T>;
begin
  var Src := FSeq;
  var Cmp := Comparer;
  Result.FSeq := TQueryable<T>.Create(TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    var
      Inner: IEnumerator<T>;
      Acc: TArray<T>;
      Idx: Integer;
    begin
      var Lst := TList<T>.Create;
      try
        Inner := Src.GetEnumerator;
        while Inner.MoveNext do
          Lst.Add(Inner.Current);
        Acc := Lst.ToArray;
      finally
        Lst.Free;
      end;
      if Cmp <> nil then
        TArray.Sort<T>(Acc, Cmp)
      else
        TArray.Sort<T>(Acc);
      Idx := -1;
      Result := TGeneratorEnumerator<T>.Create(
        function(var Current: T): Boolean
        begin
          Inc(Idx);
          Result := Idx <= High(Acc);
          if Result then
            Current := Acc[Idx];
        end);
    end));
end;

function TQuery<T>.Zip<TOther, TResult>(const Other: IEnumerable<TOther>;
  const Zipper: TFunc<T, TOther, TResult>): TQuery<TResult>;
begin
  var Src := FSeq;
  var Oth := Other;
  Result.FSeq := TQueryable<TResult>.Create(TDeferredEnumerable<TResult>.Create(
    function: IEnumerator<TResult>
    var
      A: IEnumerator<T>;
      B: IEnumerator<TOther>;
    begin
      A := Src.GetEnumerator;
      B := Oth.GetEnumerator;
      Result := TGeneratorEnumerator<TResult>.Create(
        function(var Current: TResult): Boolean
        begin
          Result := A.MoveNext and B.MoveNext;
          if Result then
            Current := Zipper(A.Current, B.Current);
        end);
    end));
end;

function TQuery<T>.GroupBy<TKey>(const KeySelector: TFunc<T, TKey>;
  const Comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, T>>;
begin
  var Src := FSeq;
  var Cmp := Comparer;
  if Cmp = nil then
    Cmp := TEqualityComparer<TKey>.Default;
  Result := TQueryable<IGrouping<TKey, T>>.Create(TDeferredEnumerable<IGrouping<TKey, T>>.Create(
    function: IEnumerator<IGrouping<TKey, T>>
    var
      Groups: TArray<IGrouping<TKey, T>>;
      Idx: Integer;
    begin
      var Buckets := TDictionary<TKey, TList<T>>.Create(Cmp);
      var Order := TList<TKey>.Create;
      try
        var Inner := Src.GetEnumerator;
        while Inner.MoveNext do
        begin
          var Key := KeySelector(Inner.Current);
          var Bucket: TList<T>;
          if not Buckets.TryGetValue(Key, Bucket) then
          begin
            Bucket := TList<T>.Create;
            Buckets.Add(Key, Bucket);
            Order.Add(Key);
          end;
          Bucket.Add(Inner.Current);
        end;
        SetLength(Groups, Order.Count);
        for var I := 0 to Order.Count - 1 do
          Groups[I] := TGrouping<TKey, T>.Create(Order[I], Buckets[Order[I]].ToArray);
      finally
        for var Pair in Buckets do
          Pair.Value.Free;
        Buckets.Free;
        Order.Free;
      end;
      Idx := -1;
      Result := TGeneratorEnumerator<IGrouping<TKey, T>>.Create(
        function(var Current: IGrouping<TKey, T>): Boolean
        begin
          Inc(Idx);
          Result := Idx <= High(Groups);
          if Result then
            Current := Groups[Idx];
        end);
    end));
end;

function TQuery<T>.ToArray: TArray<T>;
begin
  var Lst := ToList;
  try
    Result := Lst.ToArray;
  finally
    Lst.Free;
  end;
end;

function TQuery<T>.ToList: TList<T>;
begin
  Result := TList<T>.Create;
  var E := FSeq.GetEnumerator;
  while E.MoveNext do
    Result.Add(E.Current);
end;

procedure TQuery<T>.ToList(const List: TList<T>);
begin
  var E := FSeq.GetEnumerator;
  while E.MoveNext do
    List.Add(E.Current);
end;

function TQuery<T>.IsEmpty: Boolean;
begin
  Result := not FSeq.GetEnumerator.MoveNext;   // stops at the first element
end;

function TQuery<T>.Any(const Spec: TFilterSpec<T>): Boolean;
begin
  Result := not Where(Spec).IsEmpty;
end;

function TQuery<T>.TryFirst(out Value: T): Boolean;
begin
  var E := FSeq.GetEnumerator;
  Result := E.MoveNext;
  if Result then
    Value := E.Current
  else
    Value := Default(T);
end;

function TQuery<T>.FirstOrDefault: T;
begin
  TryFirst(Result);
end;

function TQuery<T>.FirstOrDefault(const Fallback: T): T;
begin
  if not TryFirst(Result) then
    Result := Fallback;
end;

function TQuery<T>.LastOrDefault: T;
begin
  Result := Default(T);
  var E := FSeq.GetEnumerator;
  while E.MoveNext do
    Result := E.Current;
end;

function TQuery<T>.LastOrDefault(const Fallback: T): T;
begin
  var Found := False;
  Result := Default(T);
  var E := FSeq.GetEnumerator;
  while E.MoveNext do
  begin
    Result := E.Current;
    Found := True;
  end;
  if not Found then
    Result := Fallback;
end;

function TQuery<T>.TrySingle(out Value: T): Boolean;
begin
  var E := FSeq.GetEnumerator;
  Result := E.MoveNext;
  if not Result then
  begin
    Value := Default(T);
    Exit;
  end;
  Value := E.Current;
  if E.MoveNext then           // a second element: not "single"
  begin
    Value := Default(T);
    Result := False;
  end;
end;

function TQuery<T>.Single: T;
begin
  var E := FSeq.GetEnumerator;
  if not E.MoveNext then
    raise ELinqError.Create('Single: the sequence is empty; expected exactly one element.');
  Result := E.Current;
  if E.MoveNext then
    raise ELinqError.Create(
      'Single: the sequence holds more than one element; expected exactly one. '
      + 'Use FirstOrDefault when the first of several is what you mean.');
end;

function TQuery<T>.SingleOrDefault: T;
begin
  Result := SingleOrDefault(Default(T));
end;

function TQuery<T>.SingleOrDefault(const Fallback: T): T;
begin
  var E := FSeq.GetEnumerator;
  if not E.MoveNext then
    Exit(Fallback);            // empty is tolerated
  Result := E.Current;
  if E.MoveNext then
    raise ELinqError.Create(
      'SingleOrDefault: the sequence holds more than one element; expected at most one. '
      + 'Use FirstOrDefault when the first of several is what you mean.');
end;

function TQuery<T>.ToSmartArray: TSmartArray<T>;
begin
  Result := ToArray;   // TArray<T> -> TSmartArray<T> (implicit)
end;

{ Linq }

class function Linq.From<T>(const Source: IEnumerable<T>): TQuery<T>;
begin
  Result.FSeq := TQueryable<T>.Create(Source);
end;

class function Linq.From<T>(const Source: TEnumerable<T>): TQuery<T>;
begin
  // Non-owning: each iteration adapts a fresh enumerator from Source; the
  // TEnumerable<T> itself is never freed (the caller keeps ownership).
  var Src := Source;
  var Seq: IEnumerable<T> := TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    begin
      Result := TEnumeratorAdapter<T>.Create(Src.GetEnumerator);
    end);
  Result.FSeq := TQueryable<T>.Create(Seq);
end;

class function Linq.From<T>(const Source: TArray<T>): TQuery<T>;
begin
  var Data := Source;
  var Seq: IEnumerable<T> := TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    var
      Idx: Integer;
    begin
      Idx := -1;
      Result := TGeneratorEnumerator<T>.Create(
        function(var Current: T): Boolean
        begin
          Inc(Idx);
          Result := Idx <= High(Data);
          if Result then
            Current := Data[Idx];
        end);
    end);
  Result.FSeq := TQueryable<T>.Create(Seq);
end;

class function Linq.From<T>(const Source: TSmartArray<T>): TQuery<T>;
begin
  var Data: TArray<T> := Source;   // TSmartArray<T> -> TArray<T> (implicit)
  Result := From<T>(Data);
end;

end.
