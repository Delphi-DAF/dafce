unit Daf.Arrays;

interface
uses
  System.TypInfo,
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  Daf.FilterSpec;

type
  TFilterMap<TIn, TOut> = reference to function(Input: TIn; out Output: TOut): Boolean;
  TArrayHelper = class helper for TArray
  public
    class function Map<T, TResult>(const Value: TArray<T>; const Mapper: TFunc<T, TResult>): TArray<TResult>; overload;
    class function Map<T, TResult>(const Value: TArray<T>; const Mapper: TFilterMap<T, TResult>): TArray<TResult>; overload;
    class function Map<T>(const Value: TArray<T>; const Mapper: TFunc<T, T>): TArray<T>; overload;
    class function Map<T>(const Value: TArray<T>; const Mapper: TFilterMap<T, T>): TArray<T>; overload;
    class function Trim(const Value: TArray<string>; RemoveEmpty: Boolean = True): TArray<string>; overload;
  end;

  TMapper<TSource, TTarget> = reference to function(S: TSource): TTarget;

  TSmartArray<T> = record
  public type
    TEnumerator = class(TEnumerator<T>)
    strict private
      FValues: System.TArray<T>;
      FIndex: Integer;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
      constructor Create(Values: System.TArray<T>);
    end;
  strict private
    FValues: System.TArray<T>;
    FSorted: Boolean;
  private
    class function GetEmpty: TSmartArray<T>; static;
    function GetLength: Integer;
    procedure SetLength(const Value: Integer);
    function GetValues(idx: Integer): T;
    procedure SetValues(idx: Integer; const Value: T);
    function GetIsEmpty: Boolean;
    class function ItemTypeIsClass: Boolean; static; inline;
    function GetFirst: T;
    function GetLast: T;
  public
    class function ItemType: PTypeInfo; static; inline;
    class operator Initialize(out Dest: TSmartArray<T>);
    class operator Finalize(var Dest: TSmartArray<T>);
    class operator Assign(var Dest: TSmartArray<T>; const [ref] Src: TSmartArray<T>);
    class operator Explicit(const Value: System.TArray<T>): TSmartArray<T>;
    class operator Explicit(const Value: TSmartArray<T>): System.TArray<T>;
    class operator Implicit(Value: System.TArray<T>): TSmartArray<T>;
    class operator Implicit(Value: TSmartArray<T>): System.TArray<T>;
    class operator Add(const Left, Right: TSmartArray<T>): TSmartArray<T>; overload;
    class operator Add(const Left: TSmartArray<T>; const Right: System.TArray<T>): TSmartArray<T>; overload;
    class operator Equal(const Left: TSmartArray<T>; const Right: TSmartArray<T>): Boolean;
    class operator In (const Item: T; const Value: TSmartArray<T>): Boolean;
    class operator Implicit(CSVStr: string): TSmartArray<T>;
    class property Empty: TSmartArray<T> read GetEmpty;
    procedure FreeItems;
    /// <summary>
    ///   Projects each element. Takes a plain <c>TFunc&lt;T, TTarget&gt;</c> — the same
    ///   type the lazy <c>Daf.Linq</c> <c>Select</c> takes — so one mapper value
    ///   serves both APIs.
    /// </summary>
    function Select<TTarget>(const Map: TFunc<T, TTarget>): TSmartArray<TTarget>;
    function Any(const Filter: TFilterSpec<T>): Boolean;
    function Where(const Filter: TFilterSpec<T>): TSmartArray<T>;
    function GetEnumerator: TEnumerator;
    function Contains(const Item: T): Boolean;
    procedure Concat(const Value: T); overload;
    procedure Concat(const Value: TSmartArray<T>); overload;
    property Length: Integer read GetLength write SetLength;
    property Values[idx: Integer]: T read GetValues write SetValues; default;
    procedure Sort; overload;
    procedure Sort(const Comparer: IComparer<T>); overload;
    procedure Sort(const Comparer: IComparer<T>; Index, Count: Integer); overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>; Index, Count: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    property IsEmpty: Boolean read GetIsEmpty;
    property First: T read GetFirst;
    property Last: T read GetLast;
  end;

  TCSVArray = TSmartArray<string>;

  TCSVArrayHelper = record Helper for TCSVArray
  public
    function Join(const Separator: string = ''): string;
  end;

implementation
uses System.Rtti;

{ TArrayHelper }

class function TArrayHelper.Map<T, TResult>(const Value: TArray<T>; const Mapper: TFunc<T, TResult>): TArray<TResult>;
begin
  SetLength(Result, Length(Value));
  for var idx := Low(Value) to High(Value) do
    Result[idx] := Mapper(Value[idx]);
end;

class function TArrayHelper.Map<T>(const Value: TArray<T>; const Mapper: TFunc<T, T>): TArray<T>;
begin
  Result := Map<T, T>(Value, Mapper);
end;

class function TArrayHelper.Map<T, TResult>(const Value: TArray<T>; const Mapper: TFilterMap<T, TResult>): TArray<TResult>;
begin
  var
  idxOutput := -1;
  SetLength(Result, Length(Value));
  for var idx := 0 to High(Value) do
  begin
    var
      Output: TResult;
    if Mapper(Value[idx], Output) then
    begin
      Inc(idxOutput);
      Result[idxOutput] := Output;
    end;
  end;
  SetLength(Result, idxOutput + 1);
end;

class function TArrayHelper.Map<T>(const Value: TArray<T>; const Mapper: TFilterMap<T, T>): TArray<T>;
begin
  Result := Map<T, T>(Value, Mapper);
end;

class function TArrayHelper.Trim(const Value: TArray<string>; RemoveEmpty: Boolean): TArray<string>;
begin
  Result := TArray.Map<string>(Value,
    function(Item: string; out NewItem: string): Boolean
    begin
      NewItem := Item.Trim;
      Result := not NewItem.IsEmpty or not RemoveEmpty;
    end);
end;

{ TSmartArray<T>.TEnumerator }

constructor TSmartArray<T>.TEnumerator.Create(Values: System.TArray<T>);
begin
  inherited Create;
  FValues := Values;
  FIndex := -1;
end;

function TSmartArray<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < System.Length(FValues);
end;

function TSmartArray<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := FValues[FIndex];
end;

{ TSmartArray<T> }

class operator TSmartArray<T>.In(const Item: T; const Value: TSmartArray<T>): Boolean;
begin
  Result := Value.Contains(Item);
end;

class function TSmartArray<T>.ItemType: PTypeInfo;
begin
  Result := System.TypeInfo(T);
end;

class function TSmartArray<T>.ItemTypeIsClass: Boolean;
begin
  Result := ItemType.Kind = tkClass;
end;

class operator TSmartArray<T>.Initialize(out Dest: TSmartArray<T>);
begin
  Dest.FValues := nil;
end;

procedure TSmartArray<T>.Concat(const Value: T);
begin
  var
    V: TArray<T> := [Value];
  Concat(V);
end;

procedure TSmartArray<T>.Concat(const Value: TSmartArray<T>);
begin
  FValues := TArray.Concat<T>([FValues, Value.FValues]);
  FSorted := False;
end;

class operator TSmartArray<T>.Implicit(Value: System.TArray<T>): TSmartArray<T>;
begin
  Result.FValues := Value;
  Result.FSorted := False;
end;

class operator TSmartArray<T>.Implicit(Value: TSmartArray<T>): System.TArray<T>;
begin
  Result := Value.FValues;
end;

class operator TSmartArray<T>.Finalize(var Dest: TSmartArray<T>);
begin
  Dest.FValues := nil;
  Dest.FSorted := False;
end;

procedure TSmartArray<T>.FreeItems;
type
  TObjectDynArray = TArray<TObject>;
  PObjectDynArray = ^TObjectDynArray;
begin
  if not ItemTypeIsClass then
    Exit;
  var
    Objs: PObjectDynArray := @FValues;
  for var Obj in Objs^ do
  begin
    Obj.Free;
  end;
end;

function TSmartArray<T>.GetLength: Integer;
begin
  Result := System.Length(FValues);
end;

class operator TSmartArray<T>.Assign(var Dest: TSmartArray<T>; const [ref] Src: TSmartArray<T>);
begin
  Dest.FValues := Src.FValues;
  Dest.FSorted := Src.FSorted;
end;

class function TSmartArray<T>.GetEmpty: TSmartArray<T>;
begin
  Result := Default (TSmartArray<T>);
end;

function TSmartArray<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FValues);
end;

function TSmartArray<T>.GetFirst: T;
begin
  if IsEmpty then
    Result := Default(T)
  else
    Result := FValues[0];
end;

function TSmartArray<T>.GetLast: T;
begin
  if IsEmpty then
    Result := Default(T)
  else
    Result := FValues[Length - 1];
end;

function TSmartArray<T>.GetIsEmpty: Boolean;
begin
  Result := (Length = 0);
end;

function TSmartArray<T>.GetValues(idx: Integer): T;
begin
  Result := FValues[idx];
end;

procedure TSmartArray<T>.SetLength(const Value: Integer);
begin
  System.SetLength(FValues, Value);
  FSorted := False;
end;

procedure TSmartArray<T>.SetValues(idx: Integer; const Value: T);
begin
  FValues[idx] := Value;
  FSorted := False;
end;

procedure TSmartArray<T>.Sort;
begin
  TArray.Sort<T>(FValues);
  FSorted := True;
end;

procedure TSmartArray<T>.Sort(const Comparer: IComparer<T>);
begin
  TArray.Sort<T>(FValues, Comparer);
  FSorted := True;
end;

procedure TSmartArray<T>.Sort(const Comparer: IComparer<T>; Index, Count: Integer);
begin
  TArray.Sort<T>(FValues, Comparer, Index, Count);
  FSorted := True;
end;

function TSmartArray<T>.BinarySearch(const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>; Index, Count: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(FValues, Item, FoundIndex, Comparer, Index, Count);
end;

function TSmartArray<T>.BinarySearch(const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.BinarySearch<T>(FValues, Item, FoundIndex, Comparer);
end;

function TSmartArray<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(FValues, Item, FoundIndex);
end;

function TSmartArray<T>.Contains(const Item: T): Boolean;
begin
  var
    FoundIndex: Integer;
  if FSorted then
    Exit(BinarySearch(Item, FoundIndex));
  var
  Comparer := TEqualityComparer<T>.Default;
  for var Element: T in Self do
    if Comparer.Equals(Item, Element) then
      Exit(True);
  Result := False;
end;

class operator TSmartArray<T>.Equal(const Left, Right: TSmartArray<T>): Boolean;
begin
  if Left.Length <> Right.Length then
    Exit(False);
  var
  Comparer := TEqualityComparer<T>.Default;
  for var idx := 0 to Left.Length - 1 do
    if not Comparer.Equals(Left[idx], Right[idx]) then
      Exit(False);
  Result := True;
end;

class operator TSmartArray<T>.Explicit(const Value: TSmartArray<T>): System.TArray<T>;
begin
  Result := Value;
end;

class operator TSmartArray<T>.Explicit(const Value: System.TArray<T>): TSmartArray<T>;
begin
  Result := Value;
end;

class operator TSmartArray<T>.Add(const Left, Right: TSmartArray<T>): TSmartArray<T>;
begin
  Result.FValues := Left.FValues + Right.FValues;
end;

class operator TSmartArray<T>.Add(const Left: TSmartArray<T>; const Right: System.TArray<T>): TSmartArray<T>;
begin
  Result.FValues := Left.FValues + Right;
end;

class operator TSmartArray<T>.Implicit(CSVStr: string): TSmartArray<T>;
begin
  var
  ValuesAsStr := CSVStr.Split([',']);
  var
    AuxT: T;
  var
    AuxV: TValue;
  for var S in ValuesAsStr do
  begin
    AuxV := S;
    if AuxV.TryAsType<T>(AuxT) then
      Result.Concat(AuxT);
  end;
end;

function TSmartArray<T>.Where(const Filter: TFilterSpec<T>): TSmartArray<T>;
begin
  Result := nil;
  for var item in FValues do
    if Filter.Eval(Item) then
    Result := Result + [Item];
end;

function TSmartArray<T>.Any(const Filter: TFilterSpec<T>): Boolean;
begin
  for var item in FValues do
    if Filter.Eval(Item) then Exit(True);
  Result := False;
end;

function TSmartArray<T>.Select<TTarget>(const Map: TFunc<T, TTarget> ): TSmartArray<TTarget>;
begin
  Result := nil;
  for var item in FValues do
    Result.Concat(Map(Item));
end;


{ TCSVArrayHelper }

function TCSVArrayHelper.Join(const Separator: string = ''): string;
begin
  var
  SB := TStringBuilder.Create;
  try
    for var Item in Self do
    begin
      if not Result.IsEmpty then
        SB.Append(Separator);
      SB.Append(Item);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;

end;

end.
