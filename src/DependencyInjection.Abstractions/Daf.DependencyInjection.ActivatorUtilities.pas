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

  /// <summary>
  ///   Quien elige el constructor. El contenedor no mira la marca de constructor; el activador
  ///   toma el marcado como unico candidato.
  /// </summary>
  TConstructorChooser = (ccContainer, ccActivator);

  /// <summary>
  ///   Una propiedad marcada, con la clave que su marca declara.
  /// </summary>
  /// <remarks>
  ///   La clave viaja con la propiedad y no se vuelve a leer: el barrido de atributos es lo caro
  ///   --un formulario VCL tiene cientos de propiedades publicadas-- y por eso el catalogo se
  ///   calcula una vez.
  /// </remarks>
  TInjectableProperty = record
    Prop: TRttiProperty;
    Key: TValue;
    // Vacio salvo que la clave escrita en la marca sea imposible; el mensaje ya viene hecho.
    KeyError: string;
  end;

  TActivatorUtilities = class
  private
    /// <summary>Lo que el contenedor entrega SIN que nadie lo registre. La lista vive aqui y en
    /// ningun otro sitio: con <c>Provider</c> a nil solo reconoce, que es lo que necesita la
    /// eleccion de constructor; con proveedor, ademas lo fabrica.</summary>
    class function SupplyFromContainer(const Provider: IServiceProvider; PInfo: PTypeInfo;
      const Construir: Boolean; out Aux: IInterface): Boolean; static;
    class function TryResolveArgs(Provider: IServiceProvider; Ctor: TRttiMethod; out Args: TArray<TValue>; out Failed: TRttiParameter): Boolean;
    class function Candidates(const AClass: TClass; const Chooser: TConstructorChooser): TArray<TRttiMethod>;
    class function SelectBest(Ctors: TArray<TRttiMethod>): TRttiMethod;
    class function ChooseAmong(const AClass: TClass; const Resolvable: TArray<TRttiMethod>;
      const Chooser: TConstructorChooser): TRttiMethod;
  public
    class function CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod): Boolean;overload;
    class function CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;overload;
    /// <summary>
    ///   El constructor que Chooser usaria para construir AClass, el mismo que usa CreateUnwired.
    ///   Los candidatos son los constructores publicos del primer nivel de la jerarquia que
    ///   declara alguno. Cuando ninguno se puede satisfacer devuelve False, el mas largo y el
    ///   primer parametro que nadie registra, para poder decirlo en el mensaje. Cuando dos que se
    ///   pueden satisfacer compiten, lanza EServiceProviderError nombrando los dos.
    /// </summary>
    class function TryFindConstructor(const Provider: IServiceProvider; const AClass: TClass;
      const Chooser: TConstructorChooser; out Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;
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
    ///   Construye SIN cablear, con el constructor que elige Chooser, y lanza
    ///   EServiceProviderError nombrando el parametro cuando ninguno se puede satisfacer. Lo usa
    ///   el contenedor, que tiene que PUBLICAR la instancia entre construirla y cablearla:
    ///   mientras no este publicada, un servicio que se pida a si mismo --directamente o a traves
    ///   de otro-- vuelve a entrar en su propia fabrica sin fondo. Quien llame se encarga de
    ///   llamar a Wire cuando toque.
    /// </summary>
    class function CreateUnwired(const Provider: IServiceProvider; const AClass: TClass;
      const Chooser: TConstructorChooser; const Initialize: TProc<TObject> = nil): TObject;

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

    // Las propiedades marcadas que la RTTI expone de la clase, jerarquia incluida. Vacio significa
    // que la clase no participa: participar se deduce de marcar, no se declara aparte.
    class function InjectableProperties(const AClass: TClass): TArray<TInjectableProperty>;
    /// <summary>
    ///   La marca de inyeccion de un parametro o de una propiedad, si la lleva, con su clave.
    /// </summary>
    /// <remarks>
    ///   Reconoce el marcador POR DESCENDENCIA y no por tipo exacto, y esa palabra lo decide todo:
    ///   con el tipo exacto, el marcador tipado que una aplicacion declara sobre
    ///   DafInjectAttribute&lt;T&gt; no existe, y sus propiedades quedan sin inyectar en silencio.
    /// </remarks>
    class function TryFindMark(const Member: TRttiObject; out Key: TValue; out KeyError: string): Boolean;
    // La clave que un parametro de constructor declara, o vacia si no declara ninguna.
    class function KeyOfParameter(const Param: TRttiParameter): TValue;
    // El mensaje de un parametro que el contenedor no puede satisfacer, nombrando el servicio, la
    // clave, el parametro y la clase. Compartido por el activador y por la validacion del grafo.
    class function DescribeUnresolvedParameter(const AClass: TClass; const Param: TRttiParameter): string;
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
    Props: TArray<TInjectableProperty>;
  end;

var
  GCatalog: TDictionary<TClass, TInjectionCatalog>;
  GCatalogLock: TObject;

function ScanCatalog(const AClass: TClass): TInjectionCatalog;
begin
  Result.Props := [];
  var RType := RC.GetType(AClass);
  if RType = nil then Exit;
  // GetProperties recorre la jerarquia, asi que una propiedad marcada en la base entra aqui.
  for var Prop in RType.GetProperties do
  begin
    var Entry: TInjectableProperty;
    if not TActivatorUtilities.TryFindMark(Prop, Entry.Key, Entry.KeyError) then Continue;
    Entry.Prop := Prop;
    Result.Props := Result.Props + [Entry];
  end;
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

// Destruye una instancia cuyo constructor lanzo, como lo hace el compilador: el destructor sin
// BeforeDestruction --en un TInterfacedObject lanzaria, porque AfterConstruction no llego a bajar el
// recuento que deja NewInstance-- y despues FreeInstance. Es la llamada de TObject.DisposeOf.
procedure DestroyUnconstructed(const Instance: TObject);
type
  TDestructorProc = procedure(Instance: Pointer; OuterMost: ShortInt);
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  TDestructorProc(PPointer(PByte(PPointer(Instance)^) + vmtDestroy)^)(Instance, 0);
  {$WARN SYMBOL_DEPRECATED DEFAULT}
  Instance.FreeInstance;
end;

{ TActivatorUtilities }

class function TActivatorUtilities.TryFindMark(const Member: TRttiObject; out Key: TValue;
  out KeyError: string): Boolean;
begin
  Key := TValue.Empty;
  KeyError := '';
  Result := False;
  if Member = nil then Exit;
  for var Attr in Member.GetAttributes do
    // POR DESCENDENCIA. Un `Attr is` y no una comparacion de tipo exacto: es lo que permite que
    // una aplicacion declare su propio marcador tipado a su enumerado sin que el framework
    // conozca ese enumerado.
    if Attr is DafInjectAttribute then
    begin
      Key := DafInjectAttribute(Attr).Key;
      KeyError := DafInjectAttribute(Attr).KeyError;
      Exit(True);
    end;
end;

class function TActivatorUtilities.KeyOfParameter(const Param: TRttiParameter): TValue;
begin
  var KeyError: string;
  if not TryFindMark(Param, Result, KeyError) then
    Result := TValue.Empty;
end;

class function TActivatorUtilities.DescribeUnresolvedParameter(const AClass: TClass;
  const Param: TRttiParameter): string;
begin
  var Key: TValue;
  var KeyError: string;
  TryFindMark(Param, Key, KeyError);
  if KeyError <> '' then
    Exit(Format('%s: the key of parameter "%s" cannot be built: %s', [AClass.ClassName, Param.Name, KeyError]));
  // De un ILazy<X> falta X: el ILazy lo fabrica el contenedor.
  var ServiceType := Param.ParamType.Handle;
  if Lazy.IsLazyType(ServiceType) and (Lazy.ServiceTypeOf(ServiceType) <> nil) then
    Exit(Format('%s needs %s lazily for parameter "%s", and nothing registers it',
      [AClass.ClassName, ServiceRequestName(Lazy.ServiceTypeOf(ServiceType), Key), Param.Name]));
  Result := Format('%s needs %s for parameter "%s", and nothing registers it',
    [AClass.ClassName, ServiceRequestName(ServiceType, Key), Param.Name]);
end;

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
    var Entregado: IInterface;
    if SupplyFromContainer(Provider, PInfo, False, Entregado) then Continue;
    // La clave se lee AQUI tambien y no solo al construir: esta es la pregunta que decide QUE
    // CONSTRUCTOR se elige, asi que uno cuyo parametro con clave no se pueda satisfacer tiene que
    // quedar descartado en este punto -- si no, se elige y falla despues.
    var Key: TValue;
    var KeyError: string;
    TryFindMark(Params[idx], Key, KeyError);
    if KeyError <> '' then
    begin
      Failed := Params[idx];
      Exit(False);
    end;
    if (PInfo.Kind in [tkInterface, tkClass]) and not Provider.CanResolveKeyed(PInfo, Key) then
    begin
      Failed := Params[idx];
      Exit(False);
    end;
  end;
end;

class function TActivatorUtilities.Candidates(const AClass: TClass; const Chooser: TConstructorChooser): TArray<TRttiMethod>;
begin
  // Los constructores publicos del primer nivel que declara alguno: los constructores se heredan,
  // asi que una clase que no declara ninguno se construye con los de su ancestro, pero una que
  // declara alguno no cae al del ancestro cuando no puede satisfacer los suyos.
  Result := [];
  var Level := AClass;
  while (Level <> nil) and (Length(Result) = 0) do
  begin
    for var Method in RC.GetType(Level).GetDeclaredMethods do
      if Method.IsConstructor and (Method.Visibility in [mvPublic, mvPublished]) then
        Result := Result + [Method];
    Level := Level.ClassParent;
  end;
  if Chooser <> ccActivator then Exit;
  for var Method in Result do
    if Method.GetAttribute<ActivatorUtilitiesConstructorAttribute> <> nil then
    begin
      Result := [Method];
      Exit;
    end;
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

// La firma de un constructor para un mensaje: sus parametros con el servicio que pide cada uno.
function SignatureOf(const Ctor: TRttiMethod): string;
begin
  var Params: TArray<string> := [];
  for var Param in Ctor.GetParameters do
    if Param.ParamType = nil then
      Params := Params + [Param.Name]
    else
      Params := Params + [Format('%s: %s', [Param.Name,
        ServiceRequestName(Param.ParamType.Handle, TActivatorUtilities.KeyOfParameter(Param))])];
  Result := Format('%s(%s)', [Ctor.Name, string.Join('; ', Params)]);
end;

// Si cada servicio que pide Other --tipo y clave-- lo pide tambien Chosen.
function AsksOnlyFor(const Other, Chosen: TRttiMethod): Boolean;
begin
  var Asked: TArray<string> := [];
  for var Param in Chosen.GetParameters do
    if Param.ParamType <> nil then
      Asked := Asked + [ServiceRequestName(Param.ParamType.Handle, TActivatorUtilities.KeyOfParameter(Param))];
  for var Param in Other.GetParameters do
  begin
    if Param.ParamType = nil then Continue;
    var Wanted := ServiceRequestName(Param.ParamType.Handle, TActivatorUtilities.KeyOfParameter(Param));
    var Found := False;
    for var Name in Asked do
      if Name = Wanted then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Exit(False);
  end;
  Result := True;
end;

class function TActivatorUtilities.ChooseAmong(const AClass: TClass; const Resolvable: TArray<TRttiMethod>;
  const Chooser: TConstructorChooser): TRttiMethod;
begin
  // El mas largo, y el primero declarado si empatan. El contenedor exige ademas que cada uno de los
  // demas pida solo servicios que pide el elegido; el activador, que ningun otro tenga su longitud.
  Result := SelectBest(Resolvable);
  if Result = nil then Exit;
  var BestLength := Length(Result.GetParameters);
  for var Other in Resolvable do
  begin
    if Other = Result then Continue;
    if (Chooser = ccContainer) and not AsksOnlyFor(Other, Result) then
      raise EServiceProviderError.CreateFmt(
        '%s: the constructors %s and %s can both be satisfied, and the second one asks for a service the first one does not',
        [AClass.ClassName, SignatureOf(Result), SignatureOf(Other)]);
    if (Chooser = ccActivator) and (Length(Other.GetParameters) = BestLength) then
      raise EServiceProviderError.CreateFmt(
        '%s: the constructors %s and %s can both be satisfied and take the same number of parameters',
        [AClass.ClassName, SignatureOf(Result), SignatureOf(Other)]);
  end;
end;

class function TActivatorUtilities.TryFindConstructor(const Provider: IServiceProvider; const AClass: TClass;
  const Chooser: TConstructorChooser; out Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;
begin
  Failed := nil;
  Ctor := nil;
  var Declared := Candidates(AClass, Chooser);
  if Length(Declared) = 0 then Exit(True);

  // Que la clase declare mas de un constructor no es un fallo: basta con que alguno se
  // pueda satisfacer.
  var Resolvable: TArray<TRttiMethod> := [];
  for var Candidate in Declared do
    if CanResolveArgs(Provider, Candidate) then
      Resolvable := Resolvable + [Candidate];

  Ctor := ChooseAmong(AClass, Resolvable, Chooser);
  Result := Ctor <> nil;
  if Result then Exit;

  Ctor := SelectBest(Declared);
  if Ctor <> nil then
    CanResolveArgs(Provider, Ctor, Failed);
end;

class function TActivatorUtilities.SupplyFromContainer(const Provider: IServiceProvider;
  PInfo: PTypeInfo; const Construir: Boolean; out Aux: IInterface): Boolean;
begin
  Aux := nil;
  if (PInfo = TypeInfo(IServiceProviderImpl)) or (PInfo = TypeInfo(IServiceProvider)) then
  begin
    if Construir then Aux := Provider;
    Exit(True);
  end;
  if Lazy.IsLazyType(PInfo) then
  begin
    // Aplazar la construccion no es aplazar la pregunta: con proveedor, al elegir constructor y al
    // rellenarlo, el servicio interno tiene que poder resolverse. Si no, se devuelve False y sigue
    // el camino del parametro sin resolver. Sin proveedor solo se reconoce.
    if Provider = nil then
      Exit(True);
    var Interno := Lazy.ServiceTypeOf(PInfo);
    if (Interno = nil) or not Provider.CanResolveKeyed(Interno, TValue.Empty) then
      Exit(False);
    if Construir then
      Aux := Lazy.ForParameter(Provider, PInfo);
    Exit(True);
  end;
  Result := False;
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
    if not SupplyFromContainer(Provider, PInfo, True, Aux) then
      if not Provider.TryGetKeyed(PInfo, KeyOfParameter(Params[idx]), Aux) and (Pinfo.Kind <> tkRecord) then
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
  Result := CreateUnwired(Provider, AClass, ccActivator, Initialize);
  try
    Wire(Provider, Result);
  except
    // Una instancia a medio cablear no se entrega, y aqui nadie mas la nombra todavia.
    Result.Free;
    raise;
  end;
end;

class function TActivatorUtilities.CreateUnwired(const Provider: IServiceProvider; const AClass: TClass;
  const Chooser: TConstructorChooser; const Initialize: TProc<TObject>): TObject;
begin
  var Ctor: TRttiMethod;
  var Failed: TRttiParameter;
  if not TryFindConstructor(Provider, AClass, Chooser, Ctor, Failed) then
    raise EServiceProviderError.Create(DescribeUnresolvedParameter(AClass, Failed));

  var Args: TArray<TValue>;
  if not TryResolveArgs(Provider, Ctor, Args, Failed) then
    raise EServiceProviderError.Create(DescribeUnresolvedParameter(AClass, Failed));

  // En dos tiempos, y no el Ctor.Invoke(AClass, Args) de una linea. Invocar
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
  except
    // En la forma asignadora es el compilador quien destruye la instancia si el constructor lanza.
    // Aqui no hay quien lo haga.
    DestroyUnconstructed(Result);
    raise;
  end;
  try
    Result.AfterConstruction;
  except
    Result.Free;
    raise;
  end;
end;

class function TActivatorUtilities.CreateInstance<T>(const Provider: IServiceProvider): T;
begin
  Result := CreateInstance(Provider, T) as T;
end;

class function TActivatorUtilities.InjectableProperties(const AClass: TClass): TArray<TInjectableProperty>;
begin
  if AClass = nil then Exit(nil);
  Result := CatalogOf(AClass).Props;
end;

class procedure TActivatorUtilities.InjectInto(const Provider: IServiceProvider; const Instance: TObject);
begin
  if Instance = nil then Exit;
  // Participar es tener alguna propiedad marcada. Sin marcas no hay nada que rellenar y no hay
  // nada que decir: una clase que no participa no se distingue de una que participa sin marcas,
  // y ese estado no existe.
  var Catalog := CatalogOf(Instance.ClassType);

  for var Entry in Catalog.Props do
  begin
    var Prop := Entry.Prop;
    // Una clave imposible se dice AQUI y nombrando la propiedad: el constructor del atributo, que
    // es quien la detecto, no sabe sobre que la escribieron. Y se dice en vez de resolver sin
    // clave, porque eso se leeria como "ese servicio no esta registrado".
    if Entry.KeyError <> '' then
      raise EPropertyInjectionError.CreateFmt('%s.%s carries an unusable key: %s',
        [Instance.ClassName, Prop.Name, Entry.KeyError]);

    if not Prop.IsWritable then
      raise EPropertyInjectionError.CreateFmt('%s.%s is marked with DafInject but is not writable',
        [Instance.ClassName, Prop.Name]);

    var PType := Prop.PropertyType;
    if (PType = nil) or (PType.TypeKind <> tkInterface) then
      raise EPropertyInjectionError.CreateFmt('%s.%s is marked with DafInject but its type is not an interface',
        [Instance.ClassName, Prop.Name]);

    var PInfo := PType.Handle;
    var Aux: IInterface;
    // Una propiedad marcada SIN clave no alcanza un registro con clave: el aislamiento entre los
    // dos espacios no hace excepciones por venir de una propiedad en vez de un parametro.
    if not Provider.TryGetKeyed(PInfo, Entry.Key, Aux) then
      raise EPropertyInjectionError.CreateFmt('%s.%s needs %s, and nothing registers it',
        [Instance.ClassName, Prop.Name, ServiceRequestName(PInfo, Entry.Key)]);

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

  var Empty: IInterface := nil;
  for var Entry in Catalog.Props do
  begin
    var Prop := Entry.Prop;
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
