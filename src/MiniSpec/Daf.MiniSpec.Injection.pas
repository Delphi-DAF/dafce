unit Daf.MiniSpec.Injection;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections;

type
  /// <summary>
  /// Marca una propiedad para inyección automática de contexto.
  /// El tipo de la propiedad debe coincidir con un servicio registrado.
  /// </summary>
  InjectAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  /// Error lanzado cuando la inyección falla.
  /// </summary>
  EInjectionError = class(Exception);

  /// <summary>
  /// Servicio de inyección IoC.
  /// El Runner registra/desregistra instancias de contexto.
  /// Al crear un ScenarioContext, el Runner llama InjectInto para
  /// completar las propiedades marcadas con [Inject].
  /// </summary>
  TInjectorService = class
  private
    class var FServices: TDictionary<TClass, TObject>;
    class constructor Create;
    class destructor Destroy;
  public
    /// <summary>
    /// Registra una instancia. Se indexa por su tipo real (ClassType).
    /// Si ya existe una instancia del mismo tipo, se reemplaza.
    /// </summary>
    class procedure Register(Instance: TObject);

    /// <summary>
    /// Desregistra una instancia por su tipo.
    /// </summary>
    class procedure Unregister(Instance: TObject); overload;
    class procedure Unregister(AClass: TClass); overload;

    /// <summary>
    /// Resuelve una instancia compatible con el tipo solicitado.
    /// Busca primero coincidencia exacta, luego por herencia.
    /// </summary>
    /// <returns>La instancia encontrada o nil si no hay ninguna compatible</returns>
    class function Resolve(AClass: TClass): TObject; overload;
    class function Resolve<T: class>: T; overload;

    /// <summary>
    /// Inyecta en todas las propiedades del Target marcadas con [Inject].
    /// Para cada propiedad, resuelve por tipo y asigna.
    /// </summary>
    /// <exception cref="EInjectionError">
    /// Si una propiedad [Inject] no es escribible, no es de tipo clase,
    /// o no se encuentra ningún servicio compatible registrado.
    /// </exception>
    class procedure InjectInto(Target: TObject);

    /// <summary>
    /// Limpia todos los servicios registrados.
    /// </summary>
    class procedure Clear;
  end;

implementation

{ TInjectorService }

class constructor TInjectorService.Create;
begin
  FServices := TDictionary<TClass, TObject>.Create;
end;

class destructor TInjectorService.Destroy;
begin
  FServices.Free;
end;

class procedure TInjectorService.Register(Instance: TObject);
begin
  if Instance = nil then
    Exit;
  FServices.AddOrSetValue(Instance.ClassType, Instance);
end;

class procedure TInjectorService.Unregister(Instance: TObject);
begin
  if Instance = nil then
    Exit;
  Unregister(Instance.ClassType);
end;

class procedure TInjectorService.Unregister(AClass: TClass);
begin
  if AClass = nil then
    Exit;
  FServices.Remove(AClass);
end;

class procedure TInjectorService.Clear;
begin
  FServices.Clear;
end;

class function TInjectorService.Resolve(AClass: TClass): TObject;
var
  Pair: TPair<TClass, TObject>;
begin
  Result := nil;
  if AClass = nil then
    Exit;

  // Primero buscar coincidencia exacta
  if FServices.TryGetValue(AClass, Result) then
    Exit;

  // Luego buscar por herencia (la instancia registrada hereda del tipo solicitado)
  for Pair in FServices do
  begin
    if Pair.Key.InheritsFrom(AClass) then
      Exit(Pair.Value);
  end;
end;

class function TInjectorService.Resolve<T>: T;
begin
  Result := T(Resolve(T));
end;

class procedure TInjectorService.InjectInto(Target: TObject);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  HasInjectAttr: Boolean;
  PropType: TRttiType;
  PropClass: TClass;
  ResolvedValue: TObject;
begin
  if Target = nil then
    Exit;

  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(Target.ClassType);
    if RttiType = nil then
      Exit;

    for Prop in RttiType.GetProperties do
    begin
      // Buscar atributo [Inject]
      HasInjectAttr := False;
      for Attr in Prop.GetAttributes do
      begin
        if Attr is InjectAttribute then
        begin
          HasInjectAttr := True;
          Break;
        end;
      end;

      if not HasInjectAttr then
        Continue;

      // Verificar que la propiedad es escribible
      if not Prop.IsWritable then
        raise EInjectionError.CreateFmt(
          'Property "%s" in %s is marked with [Inject] but is not writable',
          [Prop.Name, Target.ClassName]);

      // Obtener el tipo de la propiedad
      PropType := Prop.PropertyType;
      if PropType.TypeKind <> tkClass then
        raise EInjectionError.CreateFmt(
          'Property "%s" in %s is marked with [Inject] but is not a class type',
          [Prop.Name, Target.ClassName]);

      PropClass := PropType.AsInstance.MetaclassType;

      // Resolver el servicio por tipo
      ResolvedValue := Resolve(PropClass);
      if ResolvedValue = nil then
        raise EInjectionError.CreateFmt(
          'Cannot resolve service for property "%s" of type %s in %s: no compatible service registered',
          [Prop.Name, PropClass.ClassName, Target.ClassName]);

      // Inyectar
      Prop.SetValue(Target, TValue.From<TObject>(ResolvedValue));
    end;
  finally
    RttiContext.Free;
  end;
end;

end.
