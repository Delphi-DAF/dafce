unit Daf.Configuration.Binder;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  Daf.Rtti,
  Daf.Extensions.Configuration;

type
  TBindOption = (boProperties, boFields);

  TConfigurationBinder = class sealed
  public
    class function Bind<T: class, constructor>(const Config: IConfiguration; const Path: string = ''; const Option: TBindOption = boProperties): T; overload;
    class procedure Bind(const Config: IConfiguration; const Path: string; Instance: TObject; const Option: TBindOption); overload;
  end;

  function ConvertTo(const Src: Variant; const PInfo: PTypeInfo): TValue;
implementation

uses
  System.StrUtils,
  System.Variants;

type
  TGenericObjectList = TList<TObject>;
  TBaseBinder = class
  protected
    constructor Create(const AConfig: IConfiguration);
  private
    FConfig: IConfiguration;
    Ctx: TRttiContext;
    function BindDataType(const Path: string; const DataType: TRttiType; const CurrValue: TValue): TValue;
  protected
    function IsDictionary(const AClass: TClass): Boolean;overload;
    function IsDictionary(const DataType: TRttiType): Boolean;overload;
    function IsCollection(const AClass: TClass): Boolean;overload;
    function IsCollection(const DataType: TRttiType): Boolean;overload;
    function IsObject(const DataType: TRttiType): Boolean;
    function IsPlain(const DataType: TRttiType): Boolean;
    function CreateObject(const DataType: TRttiType): TObject;
    function GetBindedObject(const DataType: TRttiType; const BindedObj: TObject): TObject;
    function IsArray(const DataType: TRttiType): Boolean;
    function GetMembers(const Typ: TRttiType): TArray<TRttiDataMember>; virtual; abstract;
  public
    destructor Destroy;override;
    procedure BindMembers(const Path: string; Instance: TObject);
    function BindArray(const Path: string; const DataType: TRttiType): TValue;
    function BindObject(const Path: string; const DataType: TRttiType; const BindedObj: TObject = nil): TValue;
    function BindPlain(const Path: string; const DataType: TRttiType): TValue;
    function BindCollection(const Path: string; const DataType: TRttiType; const BindedObj: TObject = nil): TValue;
    function BindDictionary(const Path: string; const DataType: TRttiType; const BindedObj: TObject = nil): TValue;
  end;

  TByPropertyBinder = class(TBaseBinder)
  protected
    function GetMembers(const Typ: TRttiType): TArray<TRttiDataMember>; override;
  end;

  TByFieldBinder = class(TBaseBinder)
  protected
    function GetMembers(const Typ: TRttiType): TArray<TRttiDataMember>; override;
  end;

function ConvertTo(const Src: Variant; const PInfo: PTypeInfo): TValue;
begin
  TValue.Make(nil, PInfo, Result);
  if PInfo.Kind = tkEnumeration then
  begin
    var EnumValue := GetEnumValue(PInfo, Src);
    if EnumValue = -1 then //not found, use default
      EnumValue := 0;
    TValue.Make(EnumValue, PInfo, Result);
    Exit;
  end;

  var Dest := Result.AsVariant;
  var OldNullStrictConvert := NullStrictConvert;
  try
    NullStrictConvert := False;
    VarCast(Dest, Src, VarType(Dest));
    NullStrictConvert := OldNullStrictConvert;
    Result := TValue.FromVariant(Dest);
  except
    NullStrictConvert := OldNullStrictConvert;
  end;
end;


{ TConfigurationBinder }

class function TConfigurationBinder.Bind<T>(const Config: IConfiguration; const Path: string; const Option: TBindOption): T;
begin
  Result := T.Create;
  Bind(Config, Path, Result, Option);
end;

class procedure TConfigurationBinder.Bind(const Config: IConfiguration; const Path: string; Instance: TObject; const Option: TBindOption);
var
  Binder: TBaseBinder;
begin
  case Option of
    boFields: Binder := TByFieldBinder.Create(Config);
    boProperties: Binder := TByPropertyBinder.Create(Config);
  else
    raise Exception.Create('Unsupported bind option');
  end;
  try
    Binder.BindMembers(Path, Instance);
  finally
    Binder.Free;
  end;
end;

{ TBaseBinder }

constructor TBaseBinder.Create(const AConfig: IConfiguration);
begin
  inherited Create;
  FConfig := AConfig;
  Ctx := TRttiContext.Create;
end;

destructor TBaseBinder.Destroy;
begin
  Ctx.Free;
  inherited;
end;

function TBaseBinder.CreateObject(const DataType: TRttiType): TObject;
begin
  if not Assigned(DataType) then Exit(nil);
  var Ctor := DataType.GetMethod('Create');
  Result := Ctor.Invoke(DataType.AsInstance.MetaclassType, []).AsObject;
end;

function TBaseBinder.GetBindedObject(const DataType: TRttiType; const BindedObj: TObject): TObject;
begin
  if Assigned(BindedObj) then
    Result := BindedObj
  else
    Result := CreateObject(DataType);
end;

procedure TBaseBinder.BindMembers(const Path: string; Instance: TObject);
begin
  var Typ := Ctx.GetType(Instance.ClassType);
  for var Member in GetMembers(Typ) do
  begin
    var Key := IfThen(Path = '', Member.Name, TConfigurationPath.Combine([Path, Member.Name]));
    Member.SetValue(Instance, BindDataType(Key, Member.DataType, Member.GetValue(Instance)));
  end;
end;

function TBaseBinder.BindDataType(const Path: string; const DataType: TRttiType; const CurrValue: TValue): TValue;
begin
  if IsArray(DataType) then
    Result := BindArray(Path, DataType)
  else
  if IsDictionary(DataType) then
    Result := BindDictionary(Path, DataType, CurrValue. AsObject)
  else
  if IsCollection(DataType) then
    Result := BindCollection(Path, DataType, CurrValue.AsObject)
  else if IsObject(DataType) then
    Result := BindObject(Path, DataType, CurrValue.AsObject)
  else if IsPlain(DataType) then
    Result := BindPlain(Path, DataType);
end;

function TBaseBinder.BindPlain(const Path: string; const DataType: TRttiType): TValue;
begin
  Result := ConvertTo(FConfig[Path], DataType.Handle);
end;

function TBaseBinder.BindObject(const Path: string; const DataType: TRttiType; const BindedObj: TObject): TValue;
begin
  Result := GetBindedObject(DataType, BindedObj);
  BindMembers(Path, Result.AsObject);
end;

function TBaseBinder.BindArray(const Path: string; const DataType: TRttiType): TValue;
begin
  var ElementType := _T.ElementType(DataType.Handle);
  var Children := FConfig.GetSection(Path).GetChildren;
  var TempArray: TArray<TValue>;
  var Index := -1;
  for var Child in Children do
  begin
    Inc(Index);
    SetLength(TempArray, Index + 1);
    var ChildPath := TConfigurationPath.Combine([Path, Child.Key]);
    var ChildType := Ctx.GetType(ElementType);
    TempArray[Index] := BindDataType(ChildPath, ChildType, TempArray[Index]);
  end;
  Result := TValue.FromArray(DataType.Handle, TempArray);
end;

function TBaseBinder.BindCollection(const Path: string; const DataType: TRttiType; const BindedObj: TObject = nil): TValue;
begin
  var AddMethod := DataType.GetMethod('Add');
  if not Assigned(AddMethod) or (Length(AddMethod.GetParameters) <> 1) then Exit(nil);

  var ItemType := AddMethod.GetParameters[0].ParamType;

  Result := GetBindedObject(DataType, BindedObj);

  for var Child in FConfig.GetSection(Path).GetChildren do
  begin
    var ChildPath := TConfigurationPath.Combine([Path, Child.Key]);
    var Value: TValue;
    Value := BindDataType(ChildPath, ItemType, Value);
    AddMethod.Invoke(Result, [Value]);
  end;
end;

function TBaseBinder.BindDictionary(const Path: string; const DataType: TRttiType; const BindedObj: TObject = nil): TValue;
begin
  var AddMethod := DataType.GetMethod('AddOrSetValue');
  if not Assigned(AddMethod) or (Length(AddMethod.GetParameters) <> 2) then Exit(nil);

  var KeyType := AddMethod.GetParameters[0].ParamType;
  var ValueType := AddMethod.GetParameters[1].ParamType;

  Result := GetBindedObject(DataType, BindedObj);
  for var Child in FConfig.GetSection(Path).GetChildren do
  begin
    var ChildPath := TConfigurationPath.Combine([Path, Child.Key]);

    var Key := ConvertTo(Child.Key, KeyType.Handle);
    var Value: TValue;
    Value := BindDataType(ChildPath, ValueType, Value);
    AddMethod.Invoke(Result, [Key, Value]);
  end;
end;

function TBaseBinder.IsDictionary(const AClass: TClass): Boolean;
begin
  Result := False;
  if (AClass = nil) then Exit;
  if (AClass = TObject) then Exit;
  if AClass.QualifiedClassName.StartsWith('System.Generics.Collections.TDictionary<', True) then
    Exit(True)
  else
    Result := IsDictionary(AClass.ClassParent);
end;

function TBaseBinder.IsCollection(const AClass: TClass): Boolean;
begin
  Result := False;
  if (AClass = nil) then Exit;
  if (AClass = TObject) then Exit;
  if Aclass.QualifiedClassName.StartsWith('System.Generics.Collections.TList<', True) then
    Exit(True)
  else
    Result := IsCollection(AClass.ClassParent);
end;

function TBaseBinder.IsArray(const DataType: TRttiType): Boolean;
begin
  Result := (DataType.TypeKind in [tkDynArray, tkArray]);
end;

function TBaseBinder.IsCollection(const DataType: TRttiType): Boolean;
begin
  Result := (DataType.TypeKind = tkClass) and IsCollection(DataType.AsInstance.MetaclassType)
end;

function TBaseBinder.IsDictionary(const DataType: TRttiType): Boolean;
begin
  Result := (DataType.TypeKind = tkClass) and IsDictionary(DataType.AsInstance.MetaclassType)
end;

function TBaseBinder.IsObject(const DataType: TRttiType): Boolean;
begin
  Result := (DataType.TypeKind = tkClass) and not IsCollection(DataType);
end;

function TBaseBinder.IsPlain(const DataType: TRttiType): Boolean;
begin
  Result := not (IsObject(DataType) or IsCollection(DataType) or IsDictionary(DataType) or IsArray(DataType));
end;

{ TByFieldBinder }

function TByFieldBinder.GetMembers(const Typ: TRttiType): TArray<TRttiDataMember>;
begin
  Result := TArray<TRttiDataMember>(Typ.GetFields);
end;

{ TByPropertyBinder }

function TByPropertyBinder.GetMembers(const Typ: TRttiType): TArray<TRttiDataMember>;
begin
  Result := TArray<TRttiDataMember>(Typ.GetProperties);
end;

end.

