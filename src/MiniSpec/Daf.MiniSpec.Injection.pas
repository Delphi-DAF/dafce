unit Daf.MiniSpec.Injection;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections;

type
  /// <summary>
  /// Marca un campo o una propiedad para inyección automática de contexto.
  /// El tipo del miembro debe coincidir con un servicio registrado.
  /// </summary>
  /// <remarks>
  /// El prefijo no es decorativo: Inject a secas es un nombre que reclaman varias herramientas a
  /// la vez, y dos units que exporten el mismo en el mismo uses no dan error -- gana la ultima, y
  /// el barrido deja de ver los miembros marcados sin decir nada.
  /// </remarks>
  MiniInjectAttribute = class(TCustomAttribute)
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
  /// completar los miembros marcados con [MiniInject].
  /// </summary>
  TInjectorService = class
  strict private
    FContexts: array[TContextScope] of TObject;
    class function IsMarked(Member: TRttiMember): Boolean; static;
    /// <summary>
    /// La parte comun a un campo y a una propiedad: comprobar y resolver. AKind es el sustantivo
    /// que llevan los tres mensajes de error, "Field" o "Property".
    /// </summary>
    function ResolveFor(Target: TObject; const AKind, AName: string; AType: TRttiType;
      const AWritable: Boolean): TObject;
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
    /// Inyecta en todos los campos y propiedades del Target marcados con [MiniInject].
    /// Para cada uno, resuelve por tipo y asigna.
    /// </summary>
    /// <remarks>
    /// Los campos entran por la RTTI de campos, que en Delphi alcanza tambien a los privados; las
    /// propiedades, por la suya, que solo alcanza a las publicas y publicadas. Un campo strict
    /// private marcado se rellena; una propiedad private marcada no la ve nadie.
    /// </remarks>
    /// <exception cref="EInjectionError">
    /// Si un miembro [MiniInject] no es escribible, no es de tipo clase,
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

class function TInjectorService.IsMarked(Member: TRttiMember): Boolean;
var
  Attr: TCustomAttribute;
begin
  for Attr in Member.GetAttributes do
    if Attr is MiniInjectAttribute then
      Exit(True);
  Result := False;
end;

function TInjectorService.ResolveFor(Target: TObject; const AKind, AName: string;
  AType: TRttiType; const AWritable: Boolean): TObject;
var
  MemberClass: TClass;
begin
  if not AWritable then
    raise EInjectionError.CreateFmt(
      '%s "%s" in %s is marked with [MiniInject] but is not writable',
      [AKind, AName, Target.ClassName]);

  if (AType = nil) or (AType.TypeKind <> tkClass) then
    raise EInjectionError.CreateFmt(
      '%s "%s" in %s is marked with [MiniInject] but is not a class type',
      [AKind, AName, Target.ClassName]);

  MemberClass := AType.AsInstance.MetaclassType;
  Result := Resolve(MemberClass);
  if Result = nil then
    raise EInjectionError.CreateFmt(
      'Cannot resolve service for %s "%s" of type %s in %s: no compatible service registered',
      [LowerCase(AKind), AName, MemberClass.ClassName, Target.ClassName]);
end;

procedure TInjectorService.InjectInto(Target: TObject);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Prop: TRttiProperty;
begin
  if Target = nil then
    Exit;

  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(Target.ClassType);
    if RttiType = nil then
      Exit;

    for Field in RttiType.GetFields do
      if IsMarked(Field) then
        Field.SetValue(Target, TValue.From<TObject>(
          ResolveFor(Target, 'Field', Field.Name, Field.FieldType, True)));

    for Prop in RttiType.GetProperties do
      if IsMarked(Prop) then
        Prop.SetValue(Target, TValue.From<TObject>(
          ResolveFor(Target, 'Property', Prop.Name, Prop.PropertyType, Prop.IsWritable)));
  finally
    RttiContext.Free;
  end;
end;

end.
