unit Daf.SQLCute.InMemory.Rows;

{
  The dynamic row model (Decision 4) and the IRow-based pipeline helpers used by
  the general lane (projection / dynamic rows). IRow is the currency only where a
  static Delphi type is absent or a reshape is needed:

    * IRow           — Get(col): TValue, Has(col), Columns.
    * TDictRow       — a generic bag backed by a name->TValue map (no RTTI).
    * TProjectedRow  — a zero-copy view exposing only the SELECT output columns.

  Plus the row-level operator builders (predicate / comparer / distinct) that mirror
  the typed fast lane but read through IRow.Get, and ProjectRow which applies a
  SELECT projection to a row. TRttiRow<T> (the zero-copy object view) is generic and
  lives with the accessors in Daf.SQLCute.InMemory.

  Values are TValue throughout (Decision 3); NULL semantics come from Daf.Comparers.
}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  Daf.SQLCute.InMemory.Compiler;

type
  /// <summary>The dynamic row: named columns carrying <c>TValue</c>.</summary>
  IRow = interface
    ['{6F2A1E4C-2B7D-4C1A-9E3F-8A5D0C6B1F42}']
    /// <summary>The value of a (optionally qualified) column; empty TValue when absent.</summary>
    function Get(const Column: string): TValue;
    /// <summary>True when the row carries the column.</summary>
    function Has(const Column: string): Boolean;
    /// <summary>The row's column names.</summary>
    function Columns: TArray<string>;
  end;

  /// <summary>A generic data bag: columns resolved against a name→<c>TValue</c> map (no RTTI).</summary>
  TDictRow = class(TInterfacedObject, IRow)
  private
    FMap: TDictionary<string, TValue>;   // keyed by lower-cased bare name
    FNames: TArray<string>;              // original-case column names
    function KeyOf(const Column: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Put(const Column: string; const Value: TValue); overload;
    procedure Put(const Column: string; const Value: Variant); overload;
    /// <summary>Builds a row from parallel name/value arrays (values as Variants; Null = SQL NULL).</summary>
    class function Make(const AColumns: array of string; const AValues: array of Variant): IRow; static;
    function Get(const Column: string): TValue;
    function Has(const Column: string): Boolean;
    function Columns: TArray<string>;
  end;

  /// <summary>A zero-copy view over a row exposing only the projected output columns.</summary>
  TProjectedRow = class(TInterfacedObject, IRow)
  private
    FRow: IRow;
    FProj: TArray<TProjColumn>;
  public
    constructor Create(const Row: IRow; const Proj: TArray<TProjColumn>);
    function Get(const Column: string): TValue;
    function Has(const Column: string): Boolean;
    function Columns: TArray<string>;
  end;

/// <summary>Wraps the tri-state predicate as a two-valued predicate over a row (keep iff True).</summary>
function MakeRowPredicate(const Pred: TValuePredicate): TPredicate<IRow>;
/// <summary>ORDER BY comparer over rows (multi-column, direction + NULL policy).</summary>
function MakeRowComparer(const Orders: TArray<TOrderSpec>): IComparer<IRow>;
/// <summary>DISTINCT equality over the given output columns (NULLs equal); nil = each row's own columns.</summary>
function MakeRowDistinct(const Columns: TArray<string>): IEqualityComparer<IRow>;
/// <summary>Applies a SELECT projection to a row (nil projection = the row unchanged).</summary>
function ProjectRow(const Row: IRow; const Proj: TArray<TProjColumn>): IRow;

implementation

uses
  Daf.Comparers;   // TValueComparers, IsNullValue

{ TDictRow }

constructor TDictRow.Create;
begin
  inherited Create;
  FMap := TDictionary<string, TValue>.Create;
end;

destructor TDictRow.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TDictRow.KeyOf(const Column: string): string;
begin
  Result := LowerCase(BareColumn(Column));
end;

procedure TDictRow.Put(const Column: string; const Value: TValue);
begin
  var Key := KeyOf(Column);
  if not FMap.ContainsKey(Key) then
    FNames := FNames + [Column];
  FMap.AddOrSetValue(Key, Value);
end;

procedure TDictRow.Put(const Column: string; const Value: Variant);
begin
  Put(Column, VariantToValue(Value));
end;

class function TDictRow.Make(const AColumns: array of string; const AValues: array of Variant): IRow;
begin
  var Row := TDictRow.Create;
  for var I := 0 to High(AColumns) do
    if I <= High(AValues) then
      Row.Put(AColumns[I], AValues[I]);
  Result := Row;
end;

function TDictRow.Get(const Column: string): TValue;
begin
  if not FMap.TryGetValue(KeyOf(Column), Result) then
    Result := TValue.Empty;
end;

function TDictRow.Has(const Column: string): Boolean;
begin
  Result := FMap.ContainsKey(KeyOf(Column));
end;

function TDictRow.Columns: TArray<string>;
begin
  Result := FNames;
end;

{ TProjectedRow }

constructor TProjectedRow.Create(const Row: IRow; const Proj: TArray<TProjColumn>);
begin
  inherited Create;
  FRow := Row;
  FProj := Proj;
end;

function TProjectedRow.Get(const Column: string): TValue;
begin
  for var P in FProj do
    if SameText(P.Output, Column) then
      Exit(FRow.Get(P.Source));
  Result := TValue.Empty;
end;

function TProjectedRow.Has(const Column: string): Boolean;
begin
  for var P in FProj do
    if SameText(P.Output, Column) then
      Exit(True);
  Result := False;
end;

function TProjectedRow.Columns: TArray<string>;
begin
  SetLength(Result, Length(FProj));
  for var I := 0 to High(FProj) do
    Result[I] := FProj[I].Output;
end;

{ pipeline helpers }

function MakeRowPredicate(const Pred: TValuePredicate): TPredicate<IRow>;
begin
  Result :=
    function(R: IRow): Boolean
    begin
      Result := Pred(
        function(const C: string): TValue
        begin
          Result := R.Get(C);
        end) = TTriState.Yes;
    end;
end;

function MakeRowComparer(const Orders: TArray<TOrderSpec>): IComparer<IRow>;
begin
  var Specs := Orders;
  Result := TComparer<IRow>.Construct(
    function(const A, B: IRow): Integer
    begin
      Result := 0;
      for var S in Specs do
      begin
        Result := CompareForOrder(A.Get(S.Column), B.Get(S.Column), S);
        if Result <> 0 then
          Exit;
      end;
    end);
end;

function MakeRowDistinct(const Columns: TArray<string>): IEqualityComparer<IRow>;
begin
  var Cols := Columns;
  var VE := TValueComparers.Equality;   // NULLs equal
  Result := TEqualityComparer<IRow>.Construct(
    function(const A, B: IRow): Boolean
    var
      Keys: TArray<string>;
    begin
      Keys := Cols;
      if Keys = nil then
        Keys := A.Columns;
      for var K in Keys do
        if not VE.Equals(A.Get(K), B.Get(K)) then
          Exit(False);
      Result := True;
    end,
    function(const X: IRow): Integer
    var
      Keys: TArray<string>;
    begin
      Keys := Cols;
      if Keys = nil then
        Keys := X.Columns;
      Result := 17;
      for var K in Keys do
        Result := CombineHash(Result, VE.GetHashCode(X.Get(K)));
    end);
end;

function ProjectRow(const Row: IRow; const Proj: TArray<TProjColumn>): IRow;
begin
  if Proj = nil then
    Result := Row
  else
    Result := TProjectedRow.Create(Row, Proj);
end;

end.
