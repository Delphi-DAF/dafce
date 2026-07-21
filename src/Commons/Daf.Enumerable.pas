unit Daf.Enumerable;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  ToIEnumerable<T> = record
  strict private
    FSource: IEnumerable<T>;
  public
    class operator Implicit(Source: TEnumerable<T>): ToIEnumerable<T>;
    class operator Implicit(Source: ToIEnumerable<T>): IEnumerable<T>;
    class operator Implicit(Source: IEnumerable<T>): ToIEnumerable<T>;
    function ToArray: TArray<T>;
  end;

  TEnumerableAdapter<T> = class(TInterfacedObject, IEnumerable, IEnumerable<T>)
  private
    FSource: TEnumerable<T>;
  public
    constructor Create(const Source: TEnumerable<T>);
    destructor Destroy;override;
    function GetEnumerator: IEnumerator; overload;
    function IEnumerable<T>.GetEnumerator = GetGenericEnumerator;
    function GetGenericEnumerator: IEnumerator<T>;
  end;

  TEnumeratorAdapter<T> = class(TInterfacedObject, IEnumerator, IEnumerator<T>)
  private
    FSource: TEnumerator<T>;
  public
    constructor Create(const Source: TEnumerator<T>);
    destructor Destroy; override;
    function MoveNext: Boolean;
    procedure Reset;
    function GetCurrent: TObject; overload;
    function IEnumerator<T>.GetCurrent = GetGenericCurrent;
    function GetGenericCurrent: T;
  end;

  /// <summary>
  ///   Generator closure: advances its captured state, writes the next element
  ///   into <c>Current</c>, and returns <c>False</c> once the sequence is
  ///   exhausted. The hand-written substitute for <c>yield</c> — Delphi has none.
  /// </summary>
  TGeneratorFunc<T> = reference to function(var Current: T): Boolean;

  /// <summary>
  ///   <c>IEnumerator&lt;T&gt;</c> driven by a generator closure, yielding
  ///   elements lazily one <c>MoveNext</c> at a time. Value-type <c>T</c>
  ///   (records, <c>TNullable&lt;&gt;</c>) is supported through the generic
  ///   <c>GetCurrent</c> method-resolution clause. <c>Reset</c> is unsupported.
  /// </summary>
  TGeneratorEnumerator<T> = class(TInterfacedObject, IEnumerator, IEnumerator<T>)
  private
    FCurrent: T;
    FGenerator: TGeneratorFunc<T>;
  public
    constructor Create(const Generator: TGeneratorFunc<T>);
    function MoveNext: Boolean;
    procedure Reset;
    function GetCurrent: TObject; overload;
    function IEnumerator<T>.GetCurrent = GetGenericCurrent;
    function GetGenericCurrent: T;
  end;

  /// <summary>
  ///   <c>IEnumerable&lt;T&gt;</c> built from an enumerator-factory closure — the
  ///   deferred / lazy sequence primitive every operator needs. Nothing runs
  ///   until <c>GetEnumerator</c> is called; each iteration re-invokes the
  ///   factory, so the sequence can be walked more than once.
  /// </summary>
  TDeferredEnumerable<T> = class(TInterfacedObject, IEnumerable, IEnumerable<T>)
  private
    FFactory: TFunc<IEnumerator<T>>;
  public
    constructor Create(const Factory: TFunc<IEnumerator<T>>);
    function GetEnumerator: IEnumerator; overload;
    function IEnumerable<T>.GetEnumerator = GetGenericEnumerator;
    function GetGenericEnumerator: IEnumerator<T>;
  end;

  TInterfaceList = class(System.Classes.TInterfaceList)
  public type
    TItem = IInterface;

    TEnumerator = class(System.Classes.TInterfaceListEnumerator)
    public
      function DoGetCurrent: TItem;
      property Current: TItem read DoGetCurrent;
    end;
  protected
    function GetItem(Index: Integer): TItem;
    procedure SetItem(Index: Integer; const Value: TItem);
  public
    function GetEnumerator: TEnumerator;
    function ItemType: TGUID;
    function IsEmpty: Boolean;
    function TryFind(const Predicate: TPredicate<IInterface>; out Value): Boolean;
    procedure Insert(Index: Integer; AItem: TItem);
    function Add(AItem: TItem): Integer;
    function Remove(AItem: TItem): Integer;
    function First: TItem;
    function Last: TItem;
    property Items[index: Integer]: TItem read GetItem write SetItem; default;
  end;

  IInterfaceList = System.Classes.IInterfaceList;
  TInterfaceListEnumerator<T: IInterface> = class;
  IInterfaceList<T: IInterface> = interface(IInterfaceList)
    ['{7C9100AE-4D62-48B2-9FF4-CFA3B4138072}']
    function GetEnumerator: TInterfaceListEnumerator<T>;
  end;

  TInterfaceListEnumerator<T: IInterface> = class(System.Classes.TInterfaceListEnumerator)
   public
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  TInterfaceList<T: IInterface> = class(System.Classes.TInterfaceList, IInterfaceList<T>)
  public
    function GetEnumerator: TInterfaceListEnumerator<T>;
  end;

implementation

uses
  Daf.Rtti;

{ TEnumerableConvert<T> }

class operator ToIEnumerable<T>.Implicit(Source: TEnumerable<T>): ToIEnumerable<T>;
begin
  Result.FSource := TEnumerableAdapter<T>.Create(Source);
end;

class operator ToIEnumerable<T>.Implicit(Source: ToIEnumerable<T>): IEnumerable<T>;
begin
  Result := Source.FSource;
end;

{ TEnumerableAdapter<T> }

constructor TEnumerableAdapter<T>.Create(const Source: TEnumerable<T>);
begin
  inherited Create;
  FSource := Source;
end;

destructor TEnumerableAdapter<T>.Destroy;
begin
  FSource.Free;
  inherited;
end;

function TEnumerableAdapter<T>.GetEnumerator: IEnumerator;
begin
  Result := TEnumeratorAdapter<T>.Create(FSource.GetEnumerator);
end;

function TEnumerableAdapter<T>.GetGenericEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(FSource.GetEnumerator);
end;

{ TEnumeratorAdapter<T> }

constructor TEnumeratorAdapter<T>.Create(const Source: TEnumerator<T>);
begin
  inherited Create;
  FSource := Source;
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  FSource.Free;
  inherited;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  Result := FSource.MoveNext;
end;

procedure TEnumeratorAdapter<T>.Reset;
begin
  raise ENotImplemented.Create('Reset is not supported');
end;

function TEnumeratorAdapter<T>.GetCurrent: TObject;
begin
  Result := TObject(Pointer(@FSource.Current));
end;

function TEnumeratorAdapter<T>.GetGenericCurrent: T;
begin
  Result := FSource.Current;
end;

{ TGeneratorEnumerator<T> }

constructor TGeneratorEnumerator<T>.Create(const Generator: TGeneratorFunc<T>);
begin
  inherited Create;
  FGenerator := Generator;
  FCurrent := Default(T);
end;

function TGeneratorEnumerator<T>.MoveNext: Boolean;
begin
  Result := FGenerator(FCurrent);
end;

procedure TGeneratorEnumerator<T>.Reset;
begin
  raise ENotImplemented.Create('Reset is not supported');
end;

function TGeneratorEnumerator<T>.GetCurrent: TObject;
begin
  Result := TObject(Pointer(@FCurrent));
end;

function TGeneratorEnumerator<T>.GetGenericCurrent: T;
begin
  Result := FCurrent;
end;

{ TDeferredEnumerable<T> }

constructor TDeferredEnumerable<T>.Create(const Factory: TFunc<IEnumerator<T>>);
begin
  inherited Create;
  FFactory := Factory;
end;

function TDeferredEnumerable<T>.GetEnumerator: IEnumerator;
begin
  Result := FFactory();
end;

function TDeferredEnumerable<T>.GetGenericEnumerator: IEnumerator<T>;
begin
  Result := FFactory();
end;

class operator ToIEnumerable<T>.Implicit(Source: IEnumerable<T>): ToIEnumerable<T>;
begin
  Result.FSource := Source;
end;

function ToIEnumerable<T>.ToArray: TArray<T>;
begin
  Result := nil;
  for var Item in FSource do
  begin
    SetLength(Result, 1 + Length(Result));
    Result[Length(Result) - 1] := Item;
  end;

end;

{ TInterfaceList.TEnumerator }

function TInterfaceList.TEnumerator.DoGetCurrent: TItem;
begin
  Result := inherited GetCurrent as TItem;
end;

{ TInterfaceList }

function TInterfaceList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TInterfaceList.ItemType: TGUID;
begin
  Result := TItem;
end;

function TInterfaceList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TInterfaceList.Add(AItem: TItem): Integer;
begin
  Result := inherited Add(AItem);
end;

procedure TInterfaceList.Insert(Index: Integer; AItem: TItem);
begin
  inherited Insert(index, AItem);
end;

function TInterfaceList.Remove(AItem: TItem): Integer;
begin
  Result := inherited Remove(AItem);
end;

function TInterfaceList.First: TItem;
begin
  if IsEmpty then
    Result := nil
  else
    Result := (inherited First as TItem);
end;

function TInterfaceList.Last: TItem;
begin
  if IsEmpty then
    Result := nil
  else
    Result := (inherited Last as TItem);
end;

function TInterfaceList.GetItem(Index: Integer): TItem;
begin
  Result := inherited Get(index) as TItem;
end;

procedure TInterfaceList.SetItem(Index: Integer; const Value: TItem);
begin
  inherited Put(index, Value);
end;

function TInterfaceList.TryFind(const Predicate: TPredicate<IInterface>; out Value): Boolean;
var
  Item: TItem;
begin
  for Item in Self do
  begin
    if not Predicate(Item) then
      Continue;
    IInterface(Value) := Item;
    Result := True;
    Exit;
  end;
  Result := False;
  Pointer(Value) := nil;
end;

{ TInterfaceList<T> }

function TInterfaceList<T>.GetEnumerator: TInterfaceListEnumerator<T>;
begin
  Result := TInterfaceListEnumerator<T>.Create(Self);
end;

{ TInterfaceListEnumerator<T> }

function TInterfaceListEnumerator<T>.GetCurrent: T;
begin
  Supports(inherited Current, _T.GUID<T>, Result);
end;

end.

