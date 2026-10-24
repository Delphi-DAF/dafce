unit Ownership.Doubles;

interface

uses
  System.Generics.Collections;

type
  /// <summary>
  ///   An object that signs in a registry when created and signs out when destroyed, so a
  ///   scenario asks what is still alive without touching an object that may be freed.
  /// </summary>
  TTracked = class
  private
    FName: string;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;
    property Name: string read FName;
  end;

  /// <summary>
  ///   A tracked object whose class declares two instances equal when they share a key: the
  ///   shape of a value object, so a scenario can tell identity from equality.
  /// </summary>
  TEqualTracked = class(TTracked)
  private
    FKey: Integer;
  public
    constructor Create(const Name: string; Key: Integer);
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
  end;

  /// <summary>
  ///   A list that signs in the same registry as its elements, so a scenario also sees when the
  ///   list itself is freed.
  /// </summary>
  TTrackedList = class(TObjectList<TTracked>)
  private
    FName: string;
  public
    constructor Create(const Name: string; OwnsObjects: Boolean = True);
    destructor Destroy; override;
    property Name: string read FName;
  end;

  /// <summary>
  ///   A tracked list that raises from <c>Notify</c> on receiving <c>Bad</c>, once it already
  ///   holds it: the shape of a list whose own bookkeeping rejects an element that is inside.
  /// </summary>
  TRaisingList = class(TTrackedList)
  public
    Bad: TTracked;
  protected
    procedure Notify(const Value: TTracked; Action: TCollectionNotification); override;
  end;

  ICounted = interface
    function Name: string;
  end;

  /// <summary>
  ///   A reference-counted object that signs in the same registry while alive, so a scenario sees
  ///   the references that a collection nobody freed still holds.
  /// </summary>
  TCounted = class(TInterfacedObject, ICounted)
  private
    FName: string;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;
    function Name: string;
  end;

  Tracking = record
  public
    /// <summary>The names of what is alive, in creation order and joined with commas.</summary>
    class function AliveNames: string; static;
    class function IsAlive(const Obj: TObject): Boolean; static;
    /// <summary>
    ///   Frees what is still alive and nothing twice: first the lists, with their ownership
    ///   off, then the elements. A red scenario leaves objects whose owner the code under test
    ///   got wrong, and this is what returns them without a double free. A counted object belongs
    ///   to whoever holds its references, so it only signs out.
    /// </summary>
    class procedure FreeSurvivors; static;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults;

var
  Registry: TList<TObject>;

function NameOf(const Obj: TObject): string;
begin
  if Obj is TTracked then
    Result := TTracked(Obj).Name
  else if Obj is TTrackedList then
    Result := TTrackedList(Obj).Name
  else if Obj is TCounted then
    Result := TCounted(Obj).Name
  else
    Result := Obj.ClassName;
end;

{ TTracked }

constructor TTracked.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  Registry.Add(Self);
end;

destructor TTracked.Destroy;
begin
  Registry.Remove(Self);
  inherited;
end;

{ TEqualTracked }

constructor TEqualTracked.Create(const Name: string; Key: Integer);
begin
  inherited Create(Name);
  FKey := Key;
end;

function TEqualTracked.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj is TEqualTracked) and (TEqualTracked(Obj).FKey = FKey);
end;

function TEqualTracked.GetHashCode: Integer;
begin
  Result := FKey;
end;

{ TTrackedList }

constructor TTrackedList.Create(const Name: string; OwnsObjects: Boolean);
begin
  inherited Create(OwnsObjects);
  FName := Name;
  Registry.Add(Self);
end;

destructor TTrackedList.Destroy;
begin
  Registry.Remove(Self);
  inherited;
end;

{ TRaisingList }

procedure TRaisingList.Notify(const Value: TTracked; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnAdded) and (Value = Bad) then
    raise Exception.Create('rechazo ' + Value.Name);
end;

{ TCounted }

constructor TCounted.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  Registry.Add(Self);
end;

destructor TCounted.Destroy;
begin
  Registry.Remove(Self);
  inherited;
end;

function TCounted.Name: string;
begin
  Result := FName;
end;

{ Tracking }

class function Tracking.AliveNames: string;
begin
  Result := '';
  for var Obj in Registry do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + NameOf(Obj);
  end;
end;

class function Tracking.IsAlive(const Obj: TObject): Boolean;
begin
  Result := Registry.IndexOf(Obj) >= 0;
end;

class procedure Tracking.FreeSurvivors;
begin
  for var I := Registry.Count - 1 downto 0 do
    if Registry[I] is TCounted then
      Registry.Delete(I);
  for var I := Registry.Count - 1 downto 0 do
    if Registry[I] is TTrackedList then
    begin
      var Lst := TTrackedList(Registry[I]);
      Lst.OwnsObjects := False;
      Lst.Free;
    end;
  while Registry.Count > 0 do
    Registry.Last.Free;
end;

initialization
  // By address: a registry that compared with Equals would sign out the first object equal to the
  // one being destroyed, which is not always the same object.
  Registry := TList<TObject>.Create(TComparer<TObject>.Construct(
    function(const L, R: TObject): Integer
    begin
      if Pointer(L) = Pointer(R) then
        Result := 0
      else if NativeUInt(L) < NativeUInt(R) then
        Result := -1
      else
        Result := 1;
    end));

finalization
  Registry.Free;

end.
