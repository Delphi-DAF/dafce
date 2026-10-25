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

  /// <summary>A configuration value that does not convert to the type of the member it binds.</summary>
  EConfigurationBindingError = class(Exception);

  TConfigurationBinder = class sealed
  public
    class function Bind<T: class, constructor>(const Config: IConfiguration; const Path: string = ''; const Option: TBindOption = boProperties): T; overload;
    class procedure Bind(const Config: IConfiguration; const Path: string; Instance: TObject; const Option: TBindOption); overload;
  end;

  /// <summary>
  ///   Converts the text of a configuration value to a type as the .NET binder does: with the invariant
  ///   culture, dates in ISO 8601 or the invariant format, booleans as true or false, and enumerations
  ///   by name, in any case, or by number. Answers whether it could.
  /// </summary>
  function TryConvert(const Text: string; const PInfo: PTypeInfo; out Value: TValue): Boolean;
  /// <summary>As TryConvert, with the type's empty value when the text does not convert.</summary>
  function ConvertTo(const Src: Variant; const PInfo: PTypeInfo): TValue;
implementation

uses
  System.StrUtils,
  System.DateUtils,
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

function TryConvert(const Text: string; const PInfo: PTypeInfo; out Value: TValue): Boolean;
var
  Int, Number: Integer;
  Int64Value: Int64;
  Float: Extended;
  Date: TDateTime;
begin
  Result := True;
  case PInfo.Kind of
    tkString, tkLString, tkWString, tkUString:
      Value := TValue.From<string>(Text).Cast(PInfo);
    tkChar, tkWChar:
      if Length(Text) = 1 then
        Value := TValue.From<Char>(Text[1]).Cast(PInfo)
      else
        Result := False;
    tkVariant:
      Value := TValue.From<Variant>(Text);
    tkInteger:
      if TryStrToInt(Text, Int) then
        Value := TValue.FromOrdinal(PInfo, Int)
      else
        Result := False;
    tkInt64:
      if TryStrToInt64(Text, Int64Value) then
        Value := TValue.From<Int64>(Int64Value).Cast(PInfo)
      else
        Result := False;
    tkFloat:
      if (PInfo = TypeInfo(TDateTime)) or (PInfo = TypeInfo(TDate)) or (PInfo = TypeInfo(TTime)) then
      begin
        Result := TryISO8601ToDate(Text, Date, False) or TryStrToDateTime(Text, Date, TFormatSettings.Invariant);
        if Result then
          Value := TValue.From<TDateTime>(Date).Cast(PInfo);
      end
      else
      begin
        Result := TryStrToFloat(Text, Float, TFormatSettings.Invariant);
        if Result then
          Value := TValue.From<Extended>(Float).Cast(PInfo);
      end;
    tkEnumeration:
      if PInfo = TypeInfo(Boolean) then
      begin
        Result := SameText(Text, 'true') or SameText(Text, 'false');
        if Result then
          Value := SameText(Text, 'true');
      end
      else
      begin
        Int := GetEnumValue(PInfo, Text);
        if (Int < 0) and TryStrToInt(Text, Number) and (Number >= GetTypeData(PInfo).MinValue) and
          (Number <= GetTypeData(PInfo).MaxValue) then
          Int := Number;
        Result := Int >= 0;
        if Result then
          Value := TValue.FromOrdinal(PInfo, Int);
      end;
  else
    Result := False;
  end;
end;

function ConvertTo(const Src: Variant; const PInfo: PTypeInfo): TValue;
begin
  if not TryConvert(VarToStr(Src), PInfo, Result) then
    TValue.Make(nil, PInfo, Result);
end;

// The message of the .NET binder: the value, where it is, and the type it does not convert to.
function BindingError(const Text, Path: string; const PInfo: PTypeInfo): EConfigurationBindingError;
begin
  Result := EConfigurationBindingError.CreateFmt('Failed to convert configuration value ''%s'' at ''%s'' to type ''%s''.',
    [Text, Path, string(PInfo.Name)]);
end;


{ TConfigurationBinder }

class function TConfigurationBinder.Bind<T>(const Config: IConfiguration; const Path: string; const Option: TBindOption): T;
begin
  Result := T.Create;
  try
    Bind(Config, Path, Result, Option);
  except
    Result.Free;
    raise;
  end;
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

// El atributo SUSTITUYE al nombre del miembro: no es un segundo sitio donde mirar, asi que un dato
// bajo el nombre del miembro no responde por el. Es la paridad con ConfigurationKeyName de .NET.
function KeyNameOf(const Member: TRttiDataMember): string;
begin
  for var Attr in Member.GetAttributes do
    if Attr is ConfigurationKeyNameAttribute then
      Exit(ConfigurationKeyNameAttribute(Attr).Name);
  Result := Member.Name;
end;

procedure TBaseBinder.BindMembers(const Path: string; Instance: TObject);
begin
  var Typ := Ctx.GetType(Instance.ClassType);
  for var Member in GetMembers(Typ) do
  begin
    var Name := KeyNameOf(Member);
    var Key := IfThen(Path = '', Name, TConfigurationPath.Combine([Path, Name]));
    // Un miembro plano consume el valor PROPIO de la clave, asi que quien responde por su ausencia
    // es Find y no Exists: una seccion que solo agrupa existe y no tiene nada que escribir en un
    // Integer. Sin dato, el miembro se queda con lo que ya tenia. Un compuesto se asigna siempre:
    // se construye aunque su seccion no aparezca, y sus miembros se rigen por esta misma regla.
    if IsPlain(Member.DataType) and not FConfig.Find(Key).HasValue then Continue;
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
  var Text := FConfig[Path];
  if not TryConvert(Text, DataType.Handle, Result) then
    raise BindingError(Text, Path, DataType.Handle);
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

    var Key: TValue;
    if not TryConvert(Child.Key, KeyType.Handle, Key) then
      raise BindingError(Child.Key, ChildPath, KeyType.Handle);
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

