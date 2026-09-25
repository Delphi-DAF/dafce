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
  Result := Format('%s needs %s for parameter "%s", and nothing registers it',
    [AClass.ClassName, ServiceRequestName(Param.ParamType.Handle, Key), Param.Name]);
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
    // Al ELEGIR constructor se reconoce siempre: el contenedor sabe fabricar esto, y decir que no
    // lo dejaria fuera para que se eligiese el Create sin parametros que trae TInterfacedObject
    // --la clase se construye igual y el campo queda en nil, sin que nadie lo diga--.
    if not Construir then Exit(True);
    // Al RELLENAR si se pregunta: aplazar la construccion no es aplazar la pregunta. Si el
    // servicio interno no lo sirve nadie se devuelve False y sigue el camino normal del parametro
    // sin resolver, que falla nombrando el parametro y su clase.
    var Interno := Lazy.ServiceTypeOf(PInfo);
    if (Interno = nil) or not Provider.CanResolveKeyed(Interno, TValue.Empty) then
      Exit(False);
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
    raise EArgumentException.Create(DescribeUnresolvedParameter(AClass, Failed));

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
