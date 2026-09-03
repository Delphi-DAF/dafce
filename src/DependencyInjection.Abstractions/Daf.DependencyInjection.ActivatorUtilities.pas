unit Daf.DependencyInjection.ActivatorUtilities;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Classes,
  Daf.Extensions.DependencyInjection,
  Daf.Activator;

type

  ActivatorUtilitiesConstructorAttribute = class(TCustomAttribute)
  end;

  TActivatorUtilities = class
  private
    class function TryResolveArgs(Provider: IServiceProvider; Ctor: TRttiMethod; out Args: TArray<TValue>; out Failed: TRttiParameter): Boolean;
    class function GetResolvableCtors(const AClass: TClass; const Provider: IServiceProviderImpl = nil): TArray<TRttiMethod>;
    class function SelectBest(Ctors: TArray<TRttiMethod>): TRttiMethod;
  public
    class function CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod): Boolean;overload;
    class function CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;overload;
    /// <summary>
    /// Responde a la misma pregunta que CreateInstance se hace al construir: hay algun
    /// constructor que el contenedor pueda satisfacer? Cuando no lo hay, devuelve el mejor
    /// candidato y el primer parametro que nadie registra, para poder decirlo en el mensaje.
    /// </summary>
    class function TryFindConstructor(const Provider: IServiceProvider; const AClass: TClass; out Ctor: TRttiMethod;
      out Failed: TRttiParameter): Boolean;
    class function CreateInstance(const Provider: IServiceProvider; const AClass: TClass): TObject; overload;
    /// <summary>
    ///   Igual, pero con un hueco entre reservar la instancia y correr su constructor. Existe
    ///   porque hay cosas que la implementacion no puede saber por si misma y que tiene que
    ///   ver YA CONSTRUYENDOSE: en el constructor es donde se decide casi todo.
    /// </summary>
    class function CreateInstance(const Provider: IServiceProvider; const AClass: TClass;
      const Initialize: TProc<TObject>): TObject; overload;
    class function CreateInstance<T: class>(const Provider: IServiceProvider): T; overload;
    /// <summary>
    ///   Construye SIN cablear. Lo usa el contenedor, que tiene que PUBLICAR la instancia entre
    ///   construirla y cablearla: mientras no este publicada, un servicio que se pida a si mismo
    ///   --directamente o a traves de otro-- vuelve a entrar en su propia fabrica sin fondo.
    ///   Quien llame se encarga de llamar a Wire cuando toque.
    /// </summary>
    class function CreateUnwired(const Provider: IServiceProvider; const AClass: TClass;
      const Initialize: TProc<TObject> = nil): TObject;

    /// <summary>
    ///   Rellena las propiedades marcadas con DafInject de una instancia ya construida,
    ///   resolviendo cada una contra el proveedor que se pasa. Es la via para lo que el
    ///   contenedor no construye: un formulario que crea la plataforma no tiene constructor que
    ///   el contenedor pueda elegir.
    /// </summary>
    /// <remarks>
    ///   El contenedor no toma posesion de lo que cablea aqui: no lo cuenta entre sus objetos,
    ///   cerrarlo no lo destruye, y no le vacia las propiedades al cerrar.
    /// </remarks>
    class procedure InjectInto(const Provider: IServiceProvider; const Instance: TObject);
    /// <summary>
    ///   InjectInto mas los cableados por codigo que la coleccion registro para la clase. Es lo
    ///   que corre sobre lo que construye el contenedor; InjectInto a secas es lo que corre
    ///   sobre una instancia ajena, que no pasa por los cableados.
    /// </summary>
    class procedure Wire(const Provider: IServiceProvider; const Instance: TObject);
    /// <summary>
    ///   Deja en nil las propiedades marcadas de una instancia. Lo llama el desmontaje antes de
    ///   soltar nada: dos servicios que se inyectaron mutuamente se sostienen entre ellos, y sin
    ///   esto ninguno baja de dos referencias ni llega a destruirse nunca.
    /// </summary>
    class procedure ClearInjected(const Instance: TObject);

    // Si la clase declara participar en la inyeccion por propiedad.
    class function DeclaresPropertyInjection(const AClass: TClass): Boolean;
    // Las propiedades marcadas que la RTTI expone de la clase, jerarquia incluida. Vacio
    // significa dos cosas distintas segun lo anterior, y por eso se preguntan por separado.
    class function InjectableProperties(const AClass: TClass): TArray<TRttiProperty>;
    // El mensaje de "declara participar y no se ve ninguna marca", compartido por la resolucion
    // y por la validacion para que las dos digan lo mismo.
    class function DescribeMissingMarks(const AClass: TClass): string;
  end;

implementation

uses
  System.Generics.Collections,
  Daf.Types;

var RC: TRttiContext;

type
  // Lo que se sabe de una clase, calculado una vez: el barrido recorre la jerarquia entera y lee
  // los atributos de cada propiedad, y un formulario VCL tiene cientos de propiedades publicadas.
  TInjectionCatalog = record
    Declares: Boolean;
    Props: TArray<TRttiProperty>;
  end;

var
  GCatalog: TDictionary<TClass, TInjectionCatalog>;
  GCatalogLock: TObject;

function DeclaresInHierarchy(const AClass: TClass): Boolean;
begin
  // La declaracion se hereda: una clase base que participa hace participar a sus hijas, igual
  // que les hereda las propiedades marcadas.
  var Current := AClass;
  while Current <> nil do
  begin
    var RType := RC.GetType(Current);
    if (RType <> nil) and (RType.GetAttribute<DafInjectPropertiesAttribute> <> nil) then Exit(True);
    Current := Current.ClassParent;
  end;
  Result := False;
end;

function ScanCatalog(const AClass: TClass): TInjectionCatalog;
begin
  Result.Declares := DeclaresInHierarchy(AClass);
  Result.Props := [];
  var RType := RC.GetType(AClass);
  if RType = nil then Exit;
  // GetProperties recorre la jerarquia, asi que una propiedad marcada en la base entra aqui.
  for var Prop in RType.GetProperties do
    if Prop.GetAttribute<DafInjectAttribute> <> nil then
      Result.Props := Result.Props + [Prop];
end;

function CatalogOf(const AClass: TClass): TInjectionCatalog;
begin
  TMonitor.Enter(GCatalogLock);
  try
    if not GCatalog.TryGetValue(AClass, Result) then
    begin
      Result := ScanCatalog(AClass);
      GCatalog.Add(AClass, Result);
    end;
  finally
    TMonitor.Exit(GCatalogLock);
  end;
end;

{ TActivatorUtilities }

class function TActivatorUtilities.CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod): Boolean;
begin
  var Failed: TRttiParameter;
  Result := CanResolveArgs(Provider,Ctor, Failed);
end;

class function TActivatorUtilities.CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;
begin
  Failed := nil;
  Result := True;
  var Params := Ctor.GetParameters;
  for var idx := 0 to Length(Params) - 1 do
  begin
    var PInfo := Params[idx].ParamType.Handle;
    if (PInfo = TypeInfo(IServiceProviderImpl)) then Continue;
    if (PInfo = TypeInfo(IServiceProvider)) then Continue;
    if (PInfo.Kind in [tkInterface, tkClass]) and not Provider.CanResolve(PInfo) then
    begin
      Failed := Params[idx];
      Exit(False);
    end;
  end;
end;

class function TActivatorUtilities.GetResolvableCtors(const AClass: TClass; const Provider: IServiceProviderImpl): TArray<TRttiMethod>;
begin
  Result := [];
  if AClass = nil then Exit;
  var RType := RC.GetType(AClass) ;
  for var Method in RType.GetDeclaredMethods do
  begin
    if not Method.IsConstructor then Continue;
    if not (Method.Visibility in [mvPublic, mvPublished]) then Continue;
    if Assigned(Provider) and not CanResolveArgs(Provider, Method) then Continue;
    if Method.GetAttribute<ActivatorUtilitiesConstructorAttribute> <> nil then
    begin
      Result := [Method];
      Exit;
    End;
    Result := Result + [Method];
  end;
  if Length(Result) = 0 then
    Result := GetResolvableCtors(AClass.ClassParent, Provider)
end;

class function TActivatorUtilities.SelectBest(Ctors: TArray<TRttiMethod>): TRttiMethod;
begin
  Result := nil;
  if Length(Ctors) = 0 then Exit;
  var Best := Ctors[0];
  for var idx := 1 to Length(Ctors) - 1 do
    if Length(Ctors[idx].GetParameters) > Length(Best.GetParameters) then
      Best := Ctors[idx];
  Result := Best;
end;

class function TActivatorUtilities.TryFindConstructor(const Provider: IServiceProvider; const AClass: TClass;
  out Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;
begin
  Failed := nil;
  Ctor := nil;
  // Candidatos: los constructores publicos del primer nivel de la jerarquia que declara
  // alguno. Aqui NO se cae al constructor de un ancestro cuando ninguno de los propios se
  // puede satisfacer, y esa es justo la diferencia con CreateInstance: una clase que pide
  // una dependencia por constructor y acaba construida con TObject.Create queda a medias,
  // y ese es el fallo que la validacion existe para ver.
  var Declared := GetResolvableCtors(AClass);
  if Length(Declared) = 0 then Exit(True);

  // Que la clase declare mas de un constructor no es un fallo: basta con que alguno se
  // pueda satisfacer, como hace el activador al elegir.
  var Resolvable: TArray<TRttiMethod> := [];
  for var Candidate in Declared do
    if CanResolveArgs(Provider, Candidate) then
      Resolvable := Resolvable + [Candidate];

  Ctor := SelectBest(Resolvable);
  Result := Ctor <> nil;
  if Result then Exit;

  Ctor := SelectBest(Declared);
  if Ctor <> nil then
    CanResolveArgs(Provider, Ctor, Failed);
end;

class function TActivatorUtilities.TryResolveArgs(Provider: IServiceProvider; Ctor: TRttiMethod; out Args: TArray<TValue>; out Failed: TRttiParameter): Boolean;
begin
  Result := True;
  var Params := Ctor.GetParameters;
  SetLength(Args, Length(Params));
  for var idx := 0 to Length(Params) - 1 do
  begin
    var PInfo := Params[idx].ParamType.Handle;
    if not (Pinfo.Kind in [tkInterface, tkClass, tkRecord]) then Continue;

    var Aux: IInterface;
    if (PInfo = TypeInfo(IServiceProviderImpl)) or (PInfo = TypeInfo(IServiceProvider)) then
      Aux := Provider
    else
    if not Provider.TryGet(PInfo, Aux) and (Pinfo.Kind <> tkRecord) then
    begin
      Failed := Params[idx];
      Exit(False);
    end;
    TValue.Make(@Aux, PInfo, Args[idx]);
  end;
end;

class function TActivatorUtilities.CreateInstance(const Provider: IServiceProvider; const AClass: TClass): TObject;
begin
  Result := CreateInstance(Provider, AClass, nil);
end;

class function TActivatorUtilities.CreateInstance(const Provider: IServiceProvider; const AClass: TClass;
  const Initialize: TProc<TObject>): TObject;
begin
  Result := CreateUnwired(Provider, AClass, Initialize);
  try
    Wire(Provider, Result);
  except
    // Una instancia a medio cablear no se entrega, y aqui nadie mas la nombra todavia.
    Result.Free;
    raise;
  end;
end;

class function TActivatorUtilities.CreateUnwired(const Provider: IServiceProvider; const AClass: TClass;
  const Initialize: TProc<TObject>): TObject;
begin
  var Ctor := SelectBest(GetResolvableCtors(AClass, Provider));
  if Ctor = nil then
    raise EArgumentException.CreateFmt('Cannot find resolvable constructor for %s', [AClass.ClassName]);

  var Args: TArray<TValue>;
  var Failed: TRttiParameter;
  if not TryResolveArgs(Provider, Ctor, Args, Failed) then
    raise EArgumentException.CreateFmt('Cannot resolve argument %s for %s', [Failed.Name, AClass.ClassName]);

  // En dos tiempos, y no el Ctor.Invoke(AClass, Args) de una linea que habia antes. Invocar
  // sobre la METACLASE construye el objeto dentro de la llamada, asi que no hay instancia que
  // preparar antes de que corra el constructor -- y ademas reasigna. Con NewInstance la hay.
  // Invocar sobre la INSTANCIA no reasigna, y el conteo de referencias queda igual que en la
  // secuencia que genera el compilador: NewInstance deja el contador a uno y AfterConstruction
  // lo baja.
  Result := AClass.NewInstance;
  try
    if Assigned(Initialize) then
      Initialize(Result);
    Ctor.Invoke(TValue.From<TObject>(Result), Args);
    Result.AfterConstruction;
  except
    // En la forma asignadora es el compilador quien libera la instancia si el constructor
    // lanza. Aqui no hay quien lo haga.
    Result.Free;
    raise;
  end;
end;

class function TActivatorUtilities.CreateInstance<T>(const Provider: IServiceProvider): T;
begin
  Result := CreateInstance(Provider, T) as T;
end;

class function TActivatorUtilities.DeclaresPropertyInjection(const AClass: TClass): Boolean;
begin
  if AClass = nil then Exit(False);
  Result := CatalogOf(AClass).Declares;
end;

class function TActivatorUtilities.InjectableProperties(const AClass: TClass): TArray<TRttiProperty>;
begin
  if AClass = nil then Exit(nil);
  Result := CatalogOf(AClass).Props;
end;

class function TActivatorUtilities.DescribeMissingMarks(const AClass: TClass): string;
begin
  Result := Format('%s is marked with DafInjectProperties but no DafInject property of it is visible. ' +
    'Check the RTTI directives of its unit: PROPERTIES([]) leaves its properties out of the RTTI, ' +
    'and then the marks do not exist for the sweep',
    [AClass.ClassName]);
end;

class procedure TActivatorUtilities.InjectInto(const Provider: IServiceProvider; const Instance: TObject);
begin
  if Instance = nil then Exit;
  var Catalog := CatalogOf(Instance.ClassType);
  if not Catalog.Declares then Exit;
  if Length(Catalog.Props) = 0 then
    raise EPropertyInjectionError.Create(DescribeMissingMarks(Instance.ClassType));

  for var Prop in Catalog.Props do
  begin
    if not Prop.IsWritable then
      raise EPropertyInjectionError.CreateFmt('%s.%s is marked with DafInject but is not writable',
        [Instance.ClassName, Prop.Name]);

    var PType := Prop.PropertyType;
    if (PType = nil) or (PType.TypeKind <> tkInterface) then
      raise EPropertyInjectionError.CreateFmt('%s.%s is marked with DafInject but its type is not an interface',
        [Instance.ClassName, Prop.Name]);

    var PInfo := PType.Handle;
    var Aux: IInterface;
    if not Provider.TryGet(PInfo, Aux) then
      raise EPropertyInjectionError.CreateFmt('%s.%s needs %s, and nothing registers it',
        [Instance.ClassName, Prop.Name, ServiceTypeName(PInfo)]);

    var Value: TValue;
    TValue.Make(@Aux, PInfo, Value);
    Prop.SetValue(Instance, Value);
  end;
end;

class procedure TActivatorUtilities.Wire(const Provider: IServiceProvider; const Instance: TObject);
begin
  if Instance = nil then Exit;
  InjectInto(Provider, Instance);

  if Provider = nil then Exit;
  var Impl: IServiceProviderImpl := Provider;
  var Injectors: IServiceInjectors;
  if not Supports(Impl, IServiceInjectors, Injectors) then Exit;
  for var Injector in Injectors.InjectorsFor(Instance.ClassType) do
    Injector(Instance, Provider);
end;

class procedure TActivatorUtilities.ClearInjected(const Instance: TObject);
begin
  if Instance = nil then Exit;
  var Catalog := CatalogOf(Instance.ClassType);
  if not Catalog.Declares then Exit;

  var Empty: IInterface := nil;
  for var Prop in Catalog.Props do
  begin
    if not Prop.IsWritable then Continue;
    var PType := Prop.PropertyType;
    if (PType = nil) or (PType.TypeKind <> tkInterface) then Continue;
    var Value: TValue;
    TValue.Make(@Empty, PType.Handle, Value);
    Prop.SetValue(Instance, Value);
  end;
end;

initialization
  RC := TRttiContext.Create;
  GCatalog := TDictionary<TClass, TInjectionCatalog>.Create;
  GCatalogLock := TObject.Create;
finalization
  GCatalogLock.Free;
  GCatalog.Free;
  RC.Free;
end.
