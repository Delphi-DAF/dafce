unit ContainerValidation.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Hosting;

type
  // En la seccion interface: la validacion inspecciona constructores por RTTI, y un tipo declarado
  // solo en implementation no esta ahi para encontrarlo.
  IUnregistered = interface(IInvokable)
    ['{BE8E970A-510F-465A-B59D-C237AE162EB4}']
  end;

  IWired = interface(IInvokable)
    ['{57CC7600-2966-4D87-9F88-F2FFBE747128}']
  end;

  /// Pide por constructor un servicio que nadie registra.
  TWired = class(TInterfacedObject, IWired)
  public
    constructor Create(const Dependency: IUnregistered);
  end;

  /// Piden por constructor lo que registra el propio host: si alguno de esos registros no pasara la
  /// validacion, estas dos clases no se podrian construir en Development.
  TObservedService = class(TInterfacedObject, IHostedService)
  public
    constructor Create(const Factory: ILoggerFactory; const Log: ILogger<TObservedService>);
    procedure Start;
    procedure Stop;
  end;

  TEnvironmentTask = class(TInterfacedObject, IStartupTask)
  public
    constructor Create(const Environment: IHostEnvironment; const Lifetime: IHostApplicationLifetime;
      const Configuration: IConfiguration);
    procedure Run;
  end;

  TValidationWorld = class
  strict private
    // Un Build que falla lo informa por el canal de arranque, y eso sale por la salida de error del
    // arnes. Se aparta para que no se confunda con la salida de las specs.
    FSavedSinks: TArray<IBootstrapSink>;
  public
    Builder: IGenericHostBuilder;
    Host: IHost;
    Failure: string;
    Options: TServiceProviderOptions;
    constructor Create;
    destructor Destroy; override;
    /// El entorno va por argumento: gana a la variable DAF_APP_ENV del proceso.
    procedure UseEnvironment(const Name: string);
    procedure Build;
  end;

implementation

uses
  Daf.MiniSpec;

{ TWired }

constructor TWired.Create(const Dependency: IUnregistered);
begin
  inherited Create;
end;

{ TObservedService }

constructor TObservedService.Create(const Factory: ILoggerFactory; const Log: ILogger<TObservedService>);
begin
  inherited Create;
end;

procedure TObservedService.Start;
begin
end;

procedure TObservedService.Stop;
begin
end;

{ TEnvironmentTask }

constructor TEnvironmentTask.Create(const Environment: IHostEnvironment; const Lifetime: IHostApplicationLifetime;
  const Configuration: IConfiguration);
begin
  inherited Create;
end;

procedure TEnvironmentTask.Run;
begin
end;

{ TValidationWorld }

constructor TValidationWorld.Create;
begin
  inherited Create;
  FSavedSinks := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
end;

destructor TValidationWorld.Destroy;
begin
  if Host <> nil then
    Host.Stop;
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

procedure TValidationWorld.UseEnvironment(const Name: string);
begin
  Builder := THostBuilder.Create(['--DAF_APP_ENV=' + Name]);
end;

procedure TValidationWorld.Build;
begin
  Failure := '';
  try
    Host := Builder.Build;
  except
    on E: Exception do Failure := E.Message;
  end;
end;

initialization

Feature('''
Feature El host valida el contenedor en Development @hosting @validation

  Como quien compone una aplicacion DAF
  Quiero que en Development un registro que falta impida arrancar
  Para enterarme al construir, y no la primera vez que alguien resuelve ese servicio
''')

.UseWorld<TValidationWorld>

.Rule('En Development el host valida el contenedor')

  .Scenario('Una dependencia sin registrar impide construir')
    .Given('una aplicacion en Development con un servicio cuya dependencia nadie registra',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Development');
        W.Builder.Services.AddSingleton<IWired, TWired>;
      end)
    .When('se construye',
      procedure(W: TValidationWorld)
      begin
        W.Build;
      end)
    .&Then('falla nombrando la dependencia y la clase que la pide',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('IUnregistered');
        Expect(W.Failure).ToContain('TWired');
      end)

  .Scenario('Fuera de Development la misma composicion construye')
    .Given('la misma composicion en Production',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Production');
        W.Builder.Services.AddSingleton<IWired, TWired>;
      end)
    .When('se construye',
      procedure(W: TValidationWorld)
      begin
        W.Build;
      end)
    .&Then('se construye sin validar',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToEqual('');
      end)

  .Scenario('Lo que registra el propio host pasa la validacion')
    .Given('una aplicacion en Development que compone logging, un servicio alojado y una tarea de arranque que piden lo que registra el host',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Development');
        W.Builder.Logging.AddNullProvider;
        W.Builder.Services.AddHostedService<TObservedService>;
        W.Builder.Services.AddStartupTask<TEnvironmentTask>;
      end)
    .When('se construye',
      procedure(W: TValidationWorld)
      begin
        W.Build;
      end)
    .&Then('se construye sin senalar nada',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToEqual('');
      end)

.Rule('Las opciones del contenedor son una propiedad del builder')

  .Scenario('Leidas sin fijar en Development, dicen que se valida')
    .Given('un builder en Development',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Development');
      end)
    .When('se leen sus opciones del contenedor sin haberlas fijado',
      procedure(W: TValidationWorld)
      begin
        W.Options := W.Builder.ServiceProviderOptions;
      end)
    .&Then('las dos validaciones estan activas',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Options.ValidateOnBuild).ToBeTrue;
        Expect(W.Options.ValidateScopes).ToBeTrue;
      end)

  .Scenario('Leidas sin fijar fuera de Development, dicen que no se valida')
    .Given('un builder en Production',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Production');
      end)
    .When('se leen sus opciones del contenedor sin haberlas fijado',
      procedure(W: TValidationWorld)
      begin
        W.Options := W.Builder.ServiceProviderOptions;
      end)
    .&Then('las dos validaciones estan apagadas',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Options.ValidateOnBuild).ToBeFalse;
        Expect(W.Options.ValidateScopes).ToBeFalse;
      end)

  .Scenario('Fijadas sin validacion, ganan a Development')
    .Given('una aplicacion en Development con una dependencia sin registrar, y las opciones fijadas sin validacion',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Development');
        W.Builder.Services.AddSingleton<IWired, TWired>;
        W.Builder.ServiceProviderOptions := TServiceProviderOptions.None;
      end)
    .When('se construye',
      procedure(W: TValidationWorld)
      begin
        W.Build;
      end)
    .&Then('se construye sin validar',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToEqual('');
      end)

  .Scenario('Fijadas con validacion, validan fuera de Development')
    .Given('una aplicacion en Production con una dependencia sin registrar, y las opciones fijadas con validacion',
      procedure(W: TValidationWorld)
      begin
        W.UseEnvironment('Production');
        W.Builder.Services.AddSingleton<IWired, TWired>;
        W.Builder.ServiceProviderOptions := TServiceProviderOptions.Create(True, True);
      end)
    .When('se construye',
      procedure(W: TValidationWorld)
      begin
        W.Build;
      end)
    .&Then('falla nombrando la dependencia',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('IUnregistered');
      end);

end.
