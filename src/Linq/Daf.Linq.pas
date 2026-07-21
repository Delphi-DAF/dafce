unit Daf.Linq;

{
  Daf.Linq — a standalone, lazy, ref-counted collection-transformation library
  (LINQ-to-Objects / Kotlin sequences / Rust iterators in spirit). Core type
  IQueryable<T> : IEnumerable<T>, built on Commons' Daf.Enumerable /
  Daf.Expression. Knows nothing about SQL.

  Task 0.3: IQueryable<T> definition + the Linq.From<T> entry points.
  Operators (Where/Select/OrderBy/Take/Skip/Distinct/Zip/GroupBy) are added in
  task 0.5; eager/lazy bridges in task 0.9.
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Enumerable;

type
  /// <summary>
  ///   A deferred, ref-counted sequence. Extends <c>System.IEnumerable&lt;T&gt;</c>
  ///   (reusing Commons' <c>Daf.Enumerable</c> adapters, which resolve the
  ///   non-generic <c>GetCurrent: TObject</c> baggage), so it works directly in a
  ///   <c>for..in</c> loop and is assignable wherever <c>IEnumerable&lt;T&gt;</c>
  ///   is expected — no bridge. Execution is deferred: no work happens until
  ///   enumeration begins.
  /// </summary>
  IQueryable<T> = interface(IEnumerable<T>)
    // Operators are declared here in task 0.5.
  end;

  /// <summary>
  ///   Base that solves the <c>System.IEnumerable&lt;T&gt;</c> plumbing (the
  ///   non-generic <c>GetCurrent: TObject</c> baggage) via the method-resolution
  ///   clause — the proven <c>Daf.Enumerable</c> pattern. Subclasses supply the
  ///   enumerator through <c>DoGetEnumerator</c>.
  /// </summary>
  TQueryableBase<T> = class(TInterfacedObject, IEnumerable, IEnumerable<T>)
  protected
    function DoGetEnumerator: IEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: IEnumerator; overload;
    function IEnumerable<T>.GetEnumerator = GetGenericEnumerator;
    function GetGenericEnumerator: IEnumerator<T>;
  end;

  /// <summary>
  ///   The concrete <c>IQueryable&lt;T&gt;</c> — a thin, ref-counted wrapper over
  ///   an <c>IEnumerable&lt;T&gt;</c> source. Adds <c>IQueryable&lt;T&gt;</c> on
  ///   top of the base's <c>IEnumerable&lt;T&gt;</c> implementation (the derived
  ///   interface introduces no new methods, so inheritance satisfies it).
  /// </summary>
  TQueryable<T> = class(TQueryableBase<T>, IQueryable<T>)
  private
    FSource: IEnumerable<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(const Source: IEnumerable<T>);
  end;

  /// <summary>
  ///   Entry point into the sequence engine — builds an <c>IQueryable&lt;T&gt;</c>
  ///   over an in-memory source with typed closures. All overloads are
  ///   non-owning and lazy: the source is not scanned until iteration begins,
  ///   and passed-in <c>TEnumerable&lt;T&gt;</c> instances are never freed.
  /// </summary>
  Linq = record
  public
    class function From<T>(const Source: IEnumerable<T>): IQueryable<T>; overload; static;
    class function From<T>(const Source: TEnumerable<T>): IQueryable<T>; overload; static;
    class function From<T>(const Source: TArray<T>): IQueryable<T>; overload; static;
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

{ Linq }

class function Linq.From<T>(const Source: IEnumerable<T>): IQueryable<T>;
begin
  Result := TQueryable<T>.Create(Source);
end;

class function Linq.From<T>(const Source: TEnumerable<T>): IQueryable<T>;
begin
  // Non-owning: each iteration adapts a fresh enumerator from Source; the
  // TEnumerable<T> itself is never freed (the caller keeps ownership).
  var Src := Source;
  var Seq: IEnumerable<T> := TDeferredEnumerable<T>.Create(
    function: IEnumerator<T>
    begin
      Result := TEnumeratorAdapter<T>.Create(Src.GetEnumerator);
    end);
  Result := TQueryable<T>.Create(Seq);
end;

class function Linq.From<T>(const Source: TArray<T>): IQueryable<T>;
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
  Result := TQueryable<T>.Create(Seq);
end;

end.
