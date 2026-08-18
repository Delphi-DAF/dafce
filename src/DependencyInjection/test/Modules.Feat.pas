unit Modules.Feat;

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.AppModule.DependencyInjection,
  Doubles.Feat;

type
  /// Los modulos se declaran en la interface para que la RTTI y el linker los vean: la
  /// spec de reproducibilidad usa el descubrimiento, que es lo que tenia el estado global.
  TFirstModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  TSecondModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  /// Su constructor propio deja una marca. Si se instanciara con TObject.Create —lo que
  /// hace el descubrimiento por RTTI— la marca vendria vacia y la spec lo veria.
  TOwnCtorModule = class(TAppModule)
  strict private
    FMark: string;
  public
    constructor Create;
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  /// Solo tiene constructor con parametro: es el caso que unicamente cubre la sobrecarga
  /// de instancia, porque el modulo corre antes de que exista el proveedor.
  TConfiguredModule = class(TAppModule)
  strict private
    FMark: string;
  public
    constructor Create(const AMark: string);
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  /// Ninguno de sus registros captura la instancia, asi que muere al acabar AddServices.
  TDisposableModule = class(TAppModule)
  public
    class var DestroyCount: Integer;
    destructor Destroy; override;
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  /// Se registra a si mismo: la referencia de interfaz que guarda el registro lo mantiene
  /// vivo por su cuenta, sin necesidad de que el modulo tenga una segunda fase.
  TSelfRegisteringModule = class(TAppModule, IFoo)
  public
    class var DestroyCount: Integer;
    destructor Destroy; override;
    function Name: string;
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

/// Alternativa ligera al modulo: agrupa unos pocos registros sin declarar un tipo. En
/// Delphi no encadena — se llama AddOtherFoo(Services), no Services.AddOtherFoo.
// Las specs de reproducibilidad ejercitan el descubrimiento a proposito: sigue siendo
// comportamiento publicado hasta 2.0, y es donde vivia el estado global que se arregla.
{$WARN SYMBOL_DEPRECATED OFF}

function AddOtherFoo(const Services: IServiceCollection): IServiceCollection;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.Rtti,
  Daf.DependencyInjection;

// Las specs de reproducibilidad ejercitan el descubrimiento a proposito: sigue siendo
// comportamiento publicado hasta 2.0, y es donde vivia el estado global que se arregla.
{$WARN SYMBOL_DEPRECATED OFF}

function AddOtherFoo(const Services: IServiceCollection): IServiceCollection;
begin
  Result := Services.AddSingleton<IFoo, TOtherFoo>;
end;

{ TFirstModule }

procedure TFirstModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IFoo, TAppFoo>;
end;

{ TSecondModule }

procedure TSecondModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IFoo, TLibFoo>;
end;

{ TOwnCtorModule }

constructor TOwnCtorModule.Create;
begin
  inherited Create;
  FMark := 'own-ctor';
end;

procedure TOwnCtorModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  var Mark := FMark;
  ServiceCollection.AddSingleton(TypeInfo(IFoo),
    function(Provider: IServiceProvider): IInterface
    begin
      Result := TWrappedFoo.Create(TAppFoo.Create, Mark) as IFoo;
    end);
end;

{ TConfiguredModule }

constructor TConfiguredModule.Create(const AMark: string);
begin
  inherited Create;
  FMark := AMark;
end;

procedure TConfiguredModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  var Mark := FMark;
  ServiceCollection.AddSingleton(TypeInfo(IFoo),
    function(Provider: IServiceProvider): IInterface
    begin
      Result := TWrappedFoo.Create(TAppFoo.Create, Mark) as IFoo;
    end);
end;

{ TDisposableModule }

destructor TDisposableModule.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

procedure TDisposableModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IFoo, TAppFoo>;
end;

{ TSelfRegisteringModule }

destructor TSelfRegisteringModule.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

function TSelfRegisteringModule.Name: string;
begin
  Result := 'module';
end;

procedure TSelfRegisteringModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton(TypeInfo(IFoo), Self as IFoo);
end;

initialization

Feature('''
Feature Explicit startup composition @di @composition @modules

  As an application author
  I want to apply each module by name, in the order I wrote
  So the startup can be read in one place and does not depend on what reflection finds
''')

.UseWorld<TFooWorld>

.Rule('Modules are applied per call, in the order written')

  .Scenario('Two modules are applied in the order asked for')
    .When('two modules touching the same service are registered',
      procedure(W: TFooWorld)
      begin
        W.Services
          .AddModule<TFirstModule>
          .AddModule<TSecondModule>;
      end)
    .&Then('the result is the second one, and does not depend on reflection',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('lib');
      end)

  .Scenario('Two packages contribute modules without stepping on each other')
    .Given('two independent modules, each with its own service',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TFirstModule>;
        W.Services.AddSingleton<IBar, TAppBar>;
      end)
    .When('a startup applies both',
      procedure(W: TFooWorld)
      begin
        W.Build;
      end)
    .&Then('each one registered what is its own',
      procedure(W: TFooWorld)
      begin
        Expect(W.Provider.GetRequiredService<IFoo>.Name).ToEqual('app');
        Expect(W.Provider.GetRequiredService<IBar>.Name).ToEqual('bar');
      end)

  .Scenario('Functions and modules mix into a single sequence')
    .When('an AddXxx function is called after a module, both over the same service',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TFirstModule>;
        AddOtherFoo(W.Services);
      end)
    .&Then('the last call written wins, function or module',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('other');
      end)

  .Scenario('A later TryAdd does not step on what the module registered')
    .Given('a module that registers IFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TFirstModule>;
      end)
    .When('TryAddSingleton<IFoo, TLibFoo> is called after it',
      procedure(W: TFooWorld)
      begin
        W.Services.TryAddSingleton<IFoo, TLibFoo>;
      end)
    .&Then('resolving IFoo gives what the module registered',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('app');
      end)

.Rule('The module has one phase, and its own constructor')

  .Scenario('The constructor of the module is respected')
    .When('a module with its own parameterless constructor is applied with the generic overload',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TOwnCtorModule>;
      end)
    .&Then('that constructor ran, and not the one of TObject',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('own-ctor(app)');
      end)

  .Scenario('A module with configuration is applied already built')
    .Given('a module whose behaviour depends on a value only the startup knows',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IBar, TAppBar>;
      end)
    .When('it is built with that value and passed to AddModule',
      procedure(W: TFooWorld)
      begin
        var Module: IAppModule := TConfiguredModule.Create('from-startup');
        W.Services.AddModule(Module);
      end)
    .&Then('it registers according to the value it received',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('from-startup(app)');
      end)

  .Scenario('The module does not outlive the composition')
    .Given('a module none of whose registrations capture the instance',
      procedure(W: TFooWorld)
      begin
        TDisposableModule.DestroyCount := 0;
      end)
    .When('it is applied',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TDisposableModule>;
      end)
    .&Then('the instance was released when AddServices finished',
      procedure(W: TFooWorld)
      begin
        Expect(TDisposableModule.DestroyCount).ToEqual(1);
      end)

  .Scenario('A registration that captures the module keeps it alive')
    .Given('a module that registers itself as a service',
      procedure(W: TFooWorld)
      begin
        TSelfRegisteringModule.DestroyCount := 0;
      end)
    .When('it is applied',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TSelfRegisteringModule>;
      end)
    .&Then('it is still alive, because the registration holds the reference',
      procedure(W: TFooWorld)
      begin
        Expect(TSelfRegisteringModule.DestroyCount).ToEqual(0);
        Expect(W.ResolveName).ToEqual('module');
      end)

.Rule('Composing twice in one process gives the same result')

  .Scenario('Two collections composed separately register the same thing')
    .When('the discovery is applied to two fresh collections in the same process',
      procedure(W: TFooWorld)
      begin
        var Package := _T.PackageOf<TFirstModule>;
        var First: IServiceCollection := TServiceCollection.Create;
        var Second: IServiceCollection := TServiceCollection.Create;
        AppModules.Add(First, Package);
        AppModules.Add(Second, Package);
        W.Names := [IntToStr(Length(First.Descriptors<IFoo>)), IntToStr(Length(Second.Descriptors<IFoo>))];
      end)
    .&Then('the second one has the same descriptors as the first',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[1]).ToEqual(W.Names[0]);
        Expect(StrToInt(W.Names[0])).ToBeGreaterThan(0);
      end)

  .Scenario('Two live collections do not contaminate each other')
    .When('two collections are composed interleaved in the same process',
      procedure(W: TFooWorld)
      begin
        var Package := _T.PackageOf<TFirstModule>;
        var First: IServiceCollection := TServiceCollection.Create;
        var Second: IServiceCollection := TServiceCollection.Create;
        AppModules.Add(First, Package);
        AppModules.Add(Second, Package);
        AppModules.Add(First, Package);
        AppModules.Add(Second, Package);
        W.Names := [IntToStr(Length(First.Descriptors<IFoo>)), IntToStr(Length(Second.Descriptors<IFoo>))];
      end)
    .&Then('both end up with the same content, as if each had been composed alone',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[1]).ToEqual(W.Names[0]);
      end)

  .Scenario('One spec does not contaminate the next')
    .Given('a collection composed by discovery in a previous scenario',
      procedure(W: TFooWorld)
      begin
        AppModules.Add(W.Services, _T.PackageOf<TFirstModule>);
      end)
    .When('a new collection is composed the same way',
      procedure(W: TFooWorld)
      begin
        var Other: IServiceCollection := TServiceCollection.Create;
        AppModules.Add(Other, _T.PackageOf<TFirstModule>);
        W.Names := [IntToStr(Length(W.Services.Descriptors<IFoo>)), IntToStr(Length(Other.Descriptors<IFoo>))];
      end)
    .&Then('each one sees exactly the services it registered',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[1]).ToEqual(W.Names[0]);
      end);

end.
