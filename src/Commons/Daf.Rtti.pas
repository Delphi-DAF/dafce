unit Daf.Rtti;

interface
uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Daf.FilterSpec,
  Daf.Arrays;

type

  _T = record
  private
  public
    class function GUID<T: IInterface>: TGUID; overload; static;inline;
    class function GUID(const PInfo: PTypeInfo): TGUID; overload; static; inline;

    class function Default<T>: T; static;inline;

    class function UnitNameOf<T>: string; overload;static;inline;
    class function UnitNameOf(PInfo: PTypeInfo): string; overload;static;
    class function UnitNameOf(AClass: TClass): string; overload;static;

    class function NameOf<T>: string; overload; static;inline;
    class function NameOf(PInfo: PTypeInfo): string; overload; static;inline;

    class function IsObject(PInfo: PTypeInfo): Boolean;static;

    class function IsCollection(AClass: TClass): Boolean;static;
    class function IsDictionary(AClass: TClass): Boolean;static;

    class function ElementType<T>: PTypeInfo;overload; static;inline;
    class function ElementType(ATypeInfo: PTypeInfo): PTypeInfo;overload;static;

    /// <summary>Un tipo generico CERRADO -- el unico que existe en el binario -- se reconoce
    /// por su nombre: Delphi no emite nada mas sobre su condicion de generico.</summary>
    class function IsGeneric(PInfo: PTypeInfo): Boolean;static;
    /// <summary>El nombre hasta el primer &lt;, o cadena vacia si no es generico. Es lo que dos
    /// instanciaciones de la misma familia comparten, y por si solo NO las distingue de otra
    /// familia del mismo nombre y distinta aridad: eso lo dice GenericArgs.</summary>
    class function GenericName(PInfo: PTypeInfo): string;static;
    /// <summary>Los argumentos de una instanciacion cerrada, uno por argumento declarado.
    /// Extiende lo que da la RTL, que no expone ninguno: hay que trocear el nombre, y trocearlo
    /// RESPETANDO EL ANIDAMIENTO, porque un argumento que a su vez es generico trae sus propias
    /// comas. Una posicion queda a nil cuando el argumento no esta registrado en la RTTI; la
    /// LONGITUD es la aridad real en todo caso, que es lo que hace falta para emparejar.</summary>
    class function GenericArgs<T>: TArray<PTypeInfo>;overload; static;inline;
    class function GenericArgs(PInfo: PTypeInfo): TArray<PTypeInfo>;overload;static;

    class function PackageOf<T>: TRttiPackage;overload;static;inline;
    class function PackageOf(PInfo: PTypeInfo): TRttiPackage; overload; static;inline;
    class function PackageOf(AClass: TClass): TRttiPackage; overload; static;inline;

    class function Extends(Intf: PTypeInfo; Ancestor: PTypeInfo): Boolean; overload;static;
    class function Extends(Intf: PTypeInfo; Ancestor: PTypeInfo; ExcludeSelf: Boolean): Boolean; overload; static;

    class function HasProperty<T: class>(const PropName:string): Boolean; overload; static;
    class function HasProperty(T: TClass;const PropName:string): Boolean; overload; static;
    class function HasProperty(Instance: TObject;const PropName:string): Boolean; overload; static;

    class function GetConverFromOps(PInfo: PTypeInfo; Filter: TPredicate<TRttiMethod> = nil): TArray<TRttiMethod>;static;
  end;

  TRttiPackageHelper = class Helper for TRttiPackage
  public
    procedure DiscoverImpl<Intf: IInterface>(StrictExtensions: Boolean; Filter: TFilterSpec<TRttiType>; Action: TProc<TRttiInterfaceType, TRttiInstanceType>; ExcludeNameSpace: string = '');
    function AllTypes: TSmartArray<TRttiType>;
  end;

  TRttiPropertyHelper = class Helper for TRttiProperty
  public
    procedure SetValue(Instance: Pointer;const AValue: TValue);
  end;

  TRttiFieldHelper = class Helper for TRttiField
  public
    /// <summary>Smart set (coercion + record Implicit operator), symmetric with the property helper.</summary>
    procedure SetValue(Instance: Pointer; const AValue: TValue);
  end;

  TRttiTypeHelper = class helper for TRttiType
  private
  public
    function IsGeneric: Boolean;
    function IsInterface: Boolean;
    function IsArray: Boolean;
    function IsCollection: Boolean;
    function IsDictionary: Boolean;
    function IsObject: Boolean;
    function IsPlain: Boolean;
    function Implements<Intf: IInterface>(StrictExtensions: Boolean = False): Boolean;
    function GetExtensions<Intf: IInterface>(StrictExtensions: Boolean = False): TSmartArray<TRttiInterfaceType>;
  end;

  TRttiInstanceTypeHelper = class helper for TRttiInstanceType
  public
  end;

  /// <summary>
  ///   Marcador para un argumento generico sin fijar. <c>IX&lt;_&gt;</c> es una instanciacion
  ///   cerrada legitima con su propio tipo, y eso es lo que permite registrar un contrato
  ///   generico una sola vez para todos sus cierres.
  /// </summary>
  /// <remarks>
  ///   El constructor protesta a proposito. Dentro de una implementacion registrada sin fijar
  ///   sus argumentos, <c>T</c> ES este marcador, asi que <c>T.Create</c> construiria un <c>_</c>
  ///   y seguiria adelante: el unico fallo mudo del mecanismo. Aqui aborta explicando, y el mismo
  ///   codigo generico instanciado sobre un cierre real sigue construyendo lo suyo.
  /// </remarks>
  _ = class
  public
    constructor Create;
  end;

  TEnum = record
    class function ToString<T>(Value: T): string;static;inline;
  end;

  TPropInjector = class
  public
    class procedure Assign<TTarget: class; TSource: class>(const Target: TTarget; const Source: TSource);
    class procedure AssignTo<TTarget: class; TSource: class>(const Source: TSource; const Target: TTarget);
  end;

function TryGetMethod(const Instance: TObject; const MethodName: string; out Method: TMethod): Boolean;
function GetInterfaceIID(const I: IInterface; var IID: TGUID): Boolean;
function GetRttiFromInterface(AIntf: IInterface; out RttiType: TRttiInterfaceType): Boolean;
function HasRtti(PInfo: PTypeInfo): Boolean;
/// <summary>
///   Converts <c>Source</c> to a value assignable to <c>Target</c>, returning False when no
///   reasonable conversion exists. Handles: exact type; a record with an <c>Implicit(X)</c>
///   operator returning <c>Target</c> (the value is coerced to <c>X</c> then wrapped — e.g.
///   <c>TNullable&lt;X&gt;</c>); a direct cast; and the same cast after a round-trip through a
///   Variant, which turns an enumeration into its ordinal and a Variant into the value it holds.
///   It does not convert between families -- an integer into a Boolean, a number into a string.
///   Shared by the smart <c>SetValue</c> helpers and by callers that materialize values (e.g.
///   the in-memory query engine).
/// </summary>
function CoerceForAssign(const Target: TRttiType; const Source: TValue; out Res: TValue): Boolean;

implementation

uses
  Daf.Types;

function TryGetMethod(const Instance: TObject; const MethodName: string; out Method: TMethod): Boolean;
begin
  Method.Data := Instance;
  Method.Code := Instance.MethodAddress(MethodName);
  Result := Assigned(Method.Code);
end;

function HasRtti(PInfo: PTypeInfo): Boolean;
begin
  var
    RCtx := TRttiContext.Create;
  try
    var
    RttiType := RCtx.GetType(PInfo) as TRttiInterfaceType;
    Result := (ifHasRtti in RttiType.IntfFlags);
  finally
    RCtx.Free;
  end;
end;

function GetRttiFromInterface(AIntf: IInterface; out RttiType: TRttiInterfaceType): Boolean;
var
  obj: TObject;
  IntfType: TRttiInterfaceType;
  ctx: TRttiContext;
  tmpIntf: IInterface;
begin
  Result := False;
  ctx.Create;
  // get the implementing object...
  obj := AIntf as TObject;

  // enumerate the object's interfaces, looking for the
  // one that matches the input parameter...
  for IntfType in (ctx.GetType(obj.ClassType) as TRttiInstanceType).GetImplementedInterfaces do
  begin
    if not obj.GetInterface(IntfType.GUID, tmpIntf) then Continue;
    if AIntf = tmpIntf then
    begin
      RttiType := IntfType;
      Result := True;
      Exit;
    end;
    tmpIntf := nil;
  end;
  Ctx.Free;
end;

function GetPIMTOffset(const I: IInterface): integer;
// PIMT = Pointer to Interface Method Table
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;

  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte:
        (AdjustmentByte: shortint);
      AddLong:
        (AdjustmentLong: longint);
  end;

  PInterfaceMT = ^TInterfaceMT;

  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;

  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := -1;
  if Assigned(Pointer(I)) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte:
          Result := -QueryInterfaceThunk.AdjustmentByte;
        AddLong:
          Result := -QueryInterfaceThunk.AdjustmentLong;
      end;
    except
      // Protect against non-Delphi or invalid interface references
    end;
end;

function GetInterfaceEntry(const I: IInterface): PInterfaceEntry;
var
  Offset: integer;
  Instance: TObject;
  InterfaceTable: PInterfaceTable;
  j: integer;
  CurrentClass: TClass;
begin
  Offset := GetPIMTOffset(I);
  Instance := TObject(I);
  if (Offset >= 0) and Assigned(Instance) then
  begin
    CurrentClass := Instance.ClassType;
    while Assigned(CurrentClass) do
    begin
      InterfaceTable := CurrentClass.GetInterfaceTable;
      if Assigned(InterfaceTable) then
        for j := 0 to InterfaceTable.EntryCount - 1 do
        begin
          Result := @InterfaceTable.Entries[j];
          if Result.IOffset = Offset then
            Exit;
        end;
      CurrentClass := CurrentClass.ClassParent
    end;
  end;
  Result := nil;
end;

function GetInterfaceIID(const I: IInterface; var IID: TGUID): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  InterfaceEntry := GetInterfaceEntry(I);
  Result := Assigned(InterfaceEntry);
  if Result then
    IID := InterfaceEntry.IID;
end;

{ _T }

class function _T.Default<T>: T;
begin
  Result := System.Default(T);
end;

class function _T.Extends(Intf, Ancestor: PTypeInfo; ExcludeSelf: Boolean): Boolean;
begin
  if (Intf = Ancestor) and ExcludeSelf then Exit(False);
  Result := _T.Extends(Intf, Ancestor);
end;

class function _T.Extends(Intf, Ancestor: PTypeInfo): Boolean;
begin
  Result := False;

  if (Intf.Kind <> tkInterface) or (Ancestor.Kind <> tkInterface)then
    Exit;

  if(Intf = nil)or(Ancestor = nil)then
    Exit;

  if Intf = Ancestor then
    Exit(True);

  if Intf.TypeData.BaseType = nil then
    Exit;

  Result := Extends(Intf.TypeData.IntfParent^, Ancestor);
end;

class function _T.GUID<T>: TGUID;
begin
  Result := _T.GUID(TypeInfo(T));
end;

class function _T.GetConverFromOps(PInfo: PTypeInfo; Filter: TPredicate<TRttiMethod> = nil): TArray<TRttiMethod>;
begin
  Result := nil;
  var RC := TRttiContext.Create;
  try
    var RType := RC.GetType(PInfo);
    for var M in RType.GetMethods do
    begin
      if (M.Name = '&op_Implicit') and (M.ReturnType.Handle = PInfo) then
        if not Assigned(Filter) or Filter(M) then
          Result := Result + [M];
    end;
  finally
    RC.Free;
  end;

      // Probar si existe operador de conversion implicita

end;

class function _T.GUID(const PInfo: PTypeInfo): TGUID;
begin
  Result := PInfo.TypeData.GUID;
end;

class function _T.ElementType<T>: PTypeInfo;
begin
  Result := _T.ElementType(TypeInfo(T));
end;

class function _T.ElementType(ATypeInfo: PTypeInfo): PTypeInfo;
var
  ref: PPTypeInfo;
begin
  if ATypeInfo^.Kind = tkArray then
    ref := GetTypeData(ATypeInfo)^.ArrayData.ElType
  else if ATypeInfo^.Kind = tkDynArray then
    ref := GetTypeData(ATypeInfo).DynArrElType
  else
    ref := nil;
  if ref = nil then
    Exit(nil)
  else
    Exit(ref^);
end;

class function _T.IsGeneric(PInfo: PTypeInfo): Boolean;
begin
  Result := (PInfo <> nil) and (Pos('<', string(PInfo.Name)) > 0);
end;

class function _T.GenericName(PInfo: PTypeInfo): string;
begin
  Result := '';
  if PInfo = nil then Exit;
  var Name := string(PInfo.Name);
  var Opening := Pos('<', Name);
  if Opening = 0 then Exit;
  Result := Copy(Name, 1, Opening - 1);
end;

class function _T.GenericArgs<T>: TArray<PTypeInfo>;
begin
  Result := _T.GenericArgs(TypeInfo(T));
end;

class function _T.GenericArgs(PInfo: PTypeInfo): TArray<PTypeInfo>;
begin
  Result := nil;
  if PInfo = nil then Exit;

  // Troceo por PROFUNDIDAD, no por comas. Un argumento que a su vez es generico trae las
  // suyas -- IPair<TDictionary<string,Integer>,TAlpha> tiene dos argumentos y tres comas--,
  // y contarlas a lo bruto da una aridad equivocada y con ella un emparejamiento equivocado.
  var Name := string(PInfo.Name);
  var Depth := 0;
  var Current := '';
  var Names: TArray<string> := nil;
  var Closed := False;
  for var Idx := 1 to Length(Name) do
  begin
    var C := Name[Idx];
    if C = '<' then
    begin
      Inc(Depth);
      if Depth = 1 then Continue;   // la apertura del nivel 1 no forma parte de ningun argumento
    end
    else if C = '>' then
    begin
      Dec(Depth);
      if Depth = 0 then
      begin
        Names := Names + [Current.Trim];
        Closed := True;
        Break;
      end;
    end
    else if (C = ',') and (Depth = 1) then
    begin
      Names := Names + [Current.Trim];
      Current := '';
      Continue;
    end;
    if Depth >= 1 then
      Current := Current + C;
  end;
  if not Closed then Exit;

  // El nombre de una instanciacion cerrada YA trae sus argumentos cualificados, asi que
  // FindType basta. Se lee Name y no QualifiedName a proposito: QualifiedName lanza para un
  // tipo que no esta declarado en la interface de una unit, y aqui eso no es un error.
  SetLength(Result, Length(Names));
  var RC := TRttiContext.Create;
  try
    for var Idx := 0 to High(Names) do
    begin
      var RType := RC.FindType(Names[Idx]);
      if RType <> nil then
        Result[Idx] := RType.Handle;
    end;
  finally
    RC.Free;
  end;
end;

class function _T.HasProperty(T: TClass;const PropName:string): Boolean;
begin
  Result :=(GetPropInfo(T, PropName) <> nil);
end;

class function _T.HasProperty(Instance: TObject;const PropName:string): Boolean;
begin
  Result := Assigned(Instance) and HasProperty(Instance.ClassType, PropName);
end;

class function _T.HasProperty<T>(const PropName:string): Boolean;
begin
  Result := HasProperty(T, PropName);
end;

class function _T.IsCollection(AClass: TClass): Boolean;
begin
  if (AClass = nil) then Exit(False);
  if (AClass = TObject) then Exit(False);
  if AClass.QualifiedClassName.StartsWith('System.Generics.Collections.TList<', True) then
    Exit(True)
  else
    Result := IsCollection(AClass.ClassParent);
end;

class function _T.IsDictionary(AClass: TClass): Boolean;
begin
  if (AClass = nil) then Exit(False);
  if (AClass = TObject) then Exit(False);
  if AClass.QualifiedClassName.StartsWith('System.Generics.Collections.TDictionary<', True) then
    Exit(True)
  else
    Result := IsDictionary(AClass.ClassParent);
end;

class function _T.IsObject(PInfo: PTypeInfo): Boolean;
begin
  Result := PInfo.Kind = tkClass;
end;

class function _T.NameOf(PInfo: PTypeInfo):string;
begin
  Result := string(PInfo.Name);
end;

class function _T.NameOf<T>:string;
begin
  Result := _T.NameOf(TypeInfo(T));
end;

class function _T.UnitNameOf<T>:string;
begin
  Result :=_T.UnitNameOf(TypeInfo(T));
end;

class function _T.UnitNameOf(AClass: TClass):string;
begin
  Result := _T.UnitNameOf(AClass.ClassInfo);
end;

class function _T.UnitNameOf(PInfo: PTypeInfo):string;
begin
  Result := ChangeFileExt(TRttiContext.Create.GetType(PInfo).QualifiedName, '');
end;

class function _T.PackageOf(PInfo: PTypeInfo): TRttiPackage;
begin
  var RC := TRttiContext.Create;
  try
    Result := RC.GetType(PInfo).Package;
  finally
    RC.Free;
  end;
end;

class function _T.PackageOf(AClass: TClass): TRttiPackage;
begin
  Result := _T.PackageOf(AClass.ClassInfo);
end;

class function _T.PackageOf<T>: TRttiPackage;
begin
  Result := _T.PackageOf(TypeInfo(T));
end;

{ TRttiPackageHelper }

function TRttiPackageHelper.AllTypes: TSmartArray<TRttiType>;
begin
  Result := Self.GetTypes;
end;

procedure TRttiPackageHelper.DiscoverImpl<Intf>(StrictExtensions: Boolean; Filter: TFilterSpec<TRttiType>; Action: TProc<TRttiInterfaceType, TRttiInstanceType>; ExcludeNameSpace: string = '');
begin
  var Candidates := AllTypes.Where(
     function(T: TRttiType): Boolean
     begin
       Result := Filter.Eval(T) and T.Implements<Intf>(StrictExtensions);
     end
  ).Select<TRttiInstanceType>(
     function(T: TRttiType): TRttiInstanceType
     begin
       Result := T as TRttiInstanceType;
     end
  );

  for var RClass in Candidates do
  begin
    Action(RClass.GetExtensions<Intf>(StrictExtensions).First, RClass);
  end;
end;

{ TRttiPropertyHelper }

function CoerceForAssign(const Target: TRttiType; const Source: TValue; out Res: TValue): Boolean;
begin
  Res := TValue.Empty;
  if Target = nil then
    Exit(False);

  // 1. Exact type — nothing to convert.
  if Source.TypeInfo = Target.Handle then
  begin
    Res := Source;
    Exit(True);
  end;

  // 2. Record with an Implicit(X) operator returning Target (e.g. TNullable<X>):
  //    coerce the source to X, then wrap it through the operator.
  if Target is TRttiRecordType then
    for var Op in Target.GetMethods do
      if SameText(Op.Name, '&op_Implicit') and (Op.ReturnType <> nil)
         and (Op.ReturnType.Handle = Target.Handle) and (Length(Op.GetParameters) = 1) then
      begin
        var ParamT := Op.GetParameters[0].ParamType;
        var Inner: TValue;
        if (ParamT <> nil) and CoerceForAssign(ParamT, Source, Inner) then
        begin
          var Dummy: TValue;
          TValue.Make(nil, Target.Handle, Dummy);   // static operator: instance is a dummy
          Res := Op.Invoke(Dummy, [Inner]);
          Exit(True);
        end;
      end;

  // 3. Direct cast (compatible ordinals, class/interface hierarchy, …).
  if Source.TryCast(Target.Handle, Res) then
    Exit(True);

  // 4. Round-trip through a Variant: an enumeration comes back as its ordinal and a Variant
  //    as the value it holds, so either can take one of the casts above. It does not convert
  //    between families; the property helper leaves that to SetPropValue.
  try
    var Tmp := TValue.FromVariant(Source.AsVariant);
    if Tmp.TypeInfo = Target.Handle then
    begin
      Res := Tmp;
      Exit(True);
    end;
    Result := Tmp.TryCast(Target.Handle, Res);
  except
    Result := False;
  end;
end;

procedure TRttiPropertyHelper.SetValue(Instance: Pointer;const AValue: TValue);
var
  V: TValue;
begin
  // CoerceForAssign first: exact type, a record Implicit operator (e.g. TNullable), a
  // direct cast. What it does not convert goes to SetPropValue, which converts by the
  // kind of the property: an ordinal or a name into an enumeration or a set, a number
  // into a string, a string into a number. A record has no such conversion, so modern
  // RTTI raises EInvalidCast.
  if CoerceForAssign(PropertyType, AValue, V) then
    inherited SetValue(Instance, V)
  else if (Self is TRttiInstanceProperty) and not (PropertyType is TRttiRecordType) then
    SetPropValue(TObject(Instance), TRttiInstanceProperty(Self).PropInfo, AValue.AsVariant)
  else
    inherited SetValue(Instance, AValue);
end;

procedure TRttiFieldHelper.SetValue(Instance: Pointer; const AValue: TValue);
var
  V: TValue;
begin
  // Same smart coercion as the property helper (see CoerceForAssign), via modern RTTI.
  if CoerceForAssign(FieldType, AValue, V) then
    inherited SetValue(Instance, V)
  else
    inherited SetValue(Instance, AValue);
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.GetExtensions<Intf>(StrictExtensions: Boolean = False): TSmartArray<TRttiInterfaceType>;
begin
  Result := nil;
  if not IsInstance then Exit;
  for var RIntf in AsInstance.GetImplementedInterfaces do
    if _T.Extends(RIntf.Handle, TypeInfo(Intf), StrictExtensions) then
      Result.Concat(RIntf);
end;

function TRttiTypeHelper.Implements<Intf>(StrictExtensions: Boolean = False): Boolean;
begin
  Result := False;
  if not IsInstance then Exit;
  for var RIntf in AsInstance.GetImplementedInterfaces do
    if _T.Extends(RIntf.Handle, TypeInfo(Intf), StrictExtensions) then Exit(True)
end;

function TRttiTypeHelper.IsGeneric: Boolean;
begin
  Result := Name.Contains('<');
end;

function TRttiTypeHelper.IsInterface: Boolean;
begin
  Result := InheritsFrom(TRttiInterfaceType);
end;

function TRttiTypeHelper.IsCollection: Boolean;
begin
  Result := (TypeKind = tkClass) and _T.IsCollection(AsInstance.MetaclassType)
end;

function TRttiTypeHelper.IsDictionary: Boolean;
begin
  Result := (TypeKind = tkClass) and _T.IsDictionary(AsInstance.MetaclassType)
end;

function TRttiTypeHelper.IsObject: Boolean;
begin
  Result := (TypeKind = tkClass) and not IsCollection;
end;

function TRttiTypeHelper.IsArray: Boolean;
begin
  Result := TypeKind in [tkDynArray, tkArray];
end;

function TRttiTypeHelper.IsPlain: Boolean;
begin
  Result := not (IsObject or IsCollection or IsDictionary or IsArray);
end;

{ _ }

constructor _.Create;
begin
  inherited Create;
  raise EInvalidOpException.Create(
    '_ is the placeholder for an unbound generic argument, not a type to build. This is a ' +
    'T.Create inside an implementation registered with its type arguments left open, where T ' +
    'IS the placeholder: read the concrete class out of the _ClosedArgs field the container ' +
    'fills, and build that one.');
end;

{ TEnum }

class function TEnum.ToString<T>(Value: T): string;
begin
  Result := TRttiEnumerationType.GetName<T>(Value);
end;

class procedure TPropInjector.AssignTo<TTarget, TSource>(const Source: TSource; const Target: TTarget);
begin
  Assign(Target, Source);
end;

class procedure TPropInjector.Assign<TTarget, TSource>(const Target: TTarget; const Source: TSource);
var
  Ctx: TRttiContext;
  SrcType, DstType: TRttiType;
  SrcProp, DstProp: TRttiProperty;
begin

  Ctx := TRttiContext.Create;
  try
    SrcType := Ctx.GetType(TypeInfo(TSource));
    DstType := Ctx.GetType(TypeInfo(TTarget));

    for SrcProp in SrcType.GetProperties do
    begin
      if not SrcProp.IsReadable then
        Continue;

      DstProp := DstType.GetProperty(SrcProp.Name);
      if not Assigned(DstProp) then
        Continue;

      if not DstProp.IsWritable then
        Continue;

      if not (DstProp.PropertyType.Handle = SrcProp.PropertyType.Handle) then
        Continue;

      DstProp.SetValue(TObject(Target), SrcProp.GetValue(TObject(Source)));
    end;
  finally
    Ctx.Free;
  end;
end;

initialization
  TRttiContext.KeepContext;
finalization
  TRttiContext.DropContext;
end.
