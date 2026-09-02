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
  /// Los tres ambitos en los que puede vivir un contexto, del mas cercano al step al mas lejano.
  /// </summary>
  /// <remarks>
  /// El orden de la enumeracion ES la precedencia declarada: Resolve la recorre en este orden, asi
  /// que anadir un ambito en medio cambia la regla. No es una lista de nombres.
  /// </remarks>
  TContextScope = (csScenario, csFeature, csSuite);

  /// <summary>
  /// Servicio de inyección IoC.
  /// Cada ejecución posee el suyo: los contextos registrados son los de esa ejecución y no
  /// sobreviven a su entorno, de modo que una ejecución anidada no hereda los de la exterior.
  /// Al crear un ScenarioContext, el Runner llama InjectInto para
  /// completar las propiedades marcadas con [Inject].
  /// </summary>
  TInjectorService = class
  strict private
    FContexts: array[TContextScope] of TObject;
  public
    /// <summary>
    /// Registra el contexto vivo de un ambito. Un segundo registro del mismo ambito reemplaza al
    /// anterior; un nil deja el ambito vacio.
    /// </summary>
    procedure Register(const Scope: TContextScope; Instance: TObject);

    /// <summary>
    /// Vacia un ambito. Lo llama quien destruye el contexto de ese ambito.
    /// </summary>
    procedure Unregister(const Scope: TContextScope);

    /// <summary>
    /// Resuelve el contexto vivo compatible con el tipo solicitado.
    /// </summary>
    /// <remarks>
    /// Precedencia declarada: primero una coincidencia exacta en cualquier ambito, y solo si no la
    /// hay, un descendiente, en los dos casos recorriendo scenario, feature y suite en ese orden.
    /// Que el tipo exacto gane a un descendiente mas cercano es deliberado: pedir el tipo que se
    /// registro no puede contestarlo un hijo suyo por estar mas cerca del step.
    /// </remarks>
    /// <returns>La instancia encontrada o nil si no hay ninguna compatible</returns>
    function Resolve(AClass: TClass): TObject; overload;
    function Resolve<T: class>: T; overload;

    /// <summary>
    /// Inyecta en todas las propiedades del Target marcadas con [Inject].
    /// Para cada propiedad, resuelve por tipo y asigna.
    /// </summary>
    /// <exception cref="EInjectionError">
    /// Si una propiedad [Inject] no es escribible, no es de tipo clase,
    /// o no se encuentra ningún servicio compatible registrado.
    /// </exception>
    procedure InjectInto(Target: TObject);

    /// <summary>
    /// Vacia los tres ambitos.
    /// </summary>
    procedure Clear;
  end;

implementation

{ TInjectorService }

procedure TInjectorService.Register(const Scope: TContextScope; Instance: TObject);
begin
  FContexts[Scope] := Instance;
end;

procedure TInjectorService.Unregister(const Scope: TContextScope);
begin
  FContexts[Scope] := nil;
end;

procedure TInjectorService.Clear;
var
  Scope: TContextScope;
begin
  for Scope := Low(TContextScope) to High(TContextScope) do
    FContexts[Scope] := nil;
end;

function TInjectorService.Resolve(AClass: TClass): TObject;
var
  Scope: TContextScope;
begin
  Result := nil;
  if AClass = nil then
    Exit;

  for Scope := Low(TContextScope) to High(TContextScope) do
    if Assigned(FContexts[Scope]) and (FContexts[Scope].ClassType = AClass) then
      Exit(FContexts[Scope]);

  for Scope := Low(TContextScope) to High(TContextScope) do
    if Assigned(FContexts[Scope]) and FContexts[Scope].ClassType.InheritsFrom(AClass) then
      Exit(FContexts[Scope]);
end;

function TInjectorService.Resolve<T>: T;
begin
  Result := T(Resolve(T));
end;

procedure TInjectorService.InjectInto(Target: TObject);
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
