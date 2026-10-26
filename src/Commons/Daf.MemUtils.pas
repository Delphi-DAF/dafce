unit Daf.MemUtils;

interface

uses
  System.TypInfo,
  System.RTTI,
  System.Classes,
  System.Generics.Collections;

type

{$REGION 'from unit RTTI implem;'}
{$POINTERMATH ON}
  PVtablePtr = ^Pointer;
{$POINTERMATH OFF}
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array [0 .. MaxInt div SizeOf(Pointer) - 1] of Pointer;
{$ENDREGION}
  TFinalizeMethod = procedure of object;

  IFinalizer = interface
    ['{97917702-2562-4B25-97C5-44C62014BDE4}']
    function GetFinalizeMethod: TFinalizeMethod;
    procedure SetFinalizeMethod(const Value: TFinalizeMethod);
    property FinalizeMethod: TFinalizeMethod read GetFinalizeMethod write SetFinalizeMethod;
  end;

  TFinalizer = class(TInterfacedObject, IFinalizer)
  private
    FFinalizeMethod: TFinalizeMethod;
    function GetFinalizeMethod: TFinalizeMethod;
    procedure SetFinalizeMethod(const Value: TFinalizeMethod);
  public
    constructor Create(FinalizeMethod: TFinalizeMethod = nil);
    destructor Destroy; override;
    property FinalizeMethod: TFinalizeMethod read GetFinalizeMethod write SetFinalizeMethod;
  end;

  TPurgatory = record
  private
    FList: TObjectList<TObject>;
  public
    class operator Initialize(out Dest: TPurgatory);
    class operator Finalize(var Dest: TPurgatory);
    function Add<T: class, constructor>(): T; overload;
    function Add<T: class>(const Instance: T): T; overload;
    function Remove<T: class>(const Instance: T): T;
  end;

  // aka "SmartPointer"
  ARC<T: class> = reference to function: T;

  ARC = class
    class function From<T: class>(Value: T): ARC<T>; overload;
    class function From<T: class, constructor>: ARC<T>; overload;
  end;

  TARC<T: class> = class(TInterfacedObject, ARC<T>)
  private
    FValue: T;
  public
    constructor Create(Value: T);
    destructor Destroy; override;
    function Invoke: T; inline;
  end;

implementation

{ TARC<T> }

constructor TARC<T>.Create(Value: T);
begin
  FValue := Value;
end;

destructor TARC<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TARC<T>.Invoke: T;
begin
  Result := FValue;
end;

{ ARC }

class function ARC.From<T>: ARC<T>;
begin
  Result := From(T.Create);
end;

class function ARC.From<T>(Value: T): ARC<T>;
begin
  if Value = nil then Exit(nil);
  
  Result := TARC<T>.Create(Value);
end;


{ TFinalizer }

constructor TFinalizer.Create(FinalizeMethod: TFinalizeMethod);
begin
  FFinalizeMethod := FinalizeMethod;
end;

destructor TFinalizer.Destroy;
begin
  if Assigned(FFinalizeMethod) then
    FinalizeMethod();
  inherited;
end;

function TFinalizer.GetFinalizeMethod: TFinalizeMethod;
begin
  Result := FFinalizeMethod;
end;

procedure TFinalizer.SetFinalizeMethod(const Value: TFinalizeMethod);
begin
  FFinalizeMethod := Value;
end;

{ TPurgatory }

class operator TPurgatory.Initialize(out Dest: TPurgatory);
begin
  Dest.FList := TObjectList<TObject>.Create(True);
end;

class operator TPurgatory.Finalize(var Dest: TPurgatory);
begin
  Dest.FList.Free;
end;

function TPurgatory.Add<T>(): T;
begin
  Result := Add(T.Create);
end;

function TPurgatory.Add<T>(const Instance: T): T;
begin
  Result := Instance;
  if Result = nil then
    Exit;
  FList.Add(Instance);
end;

function TPurgatory.Remove<T>(const Instance: T): T;
begin
  FList.Extract(Instance);
  Result := Instance;
end;

end.
