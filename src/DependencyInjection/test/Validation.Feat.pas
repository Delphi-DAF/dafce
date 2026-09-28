unit Validation.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  /// Nadie lo registra nunca: es la dependencia que falta.
  IMissing = interface(IInvokable)
    ['{0C6593E7-645D-4F21-B072-23D694956BBA}']
  end;

  IFirstBroken = interface(IInvokable)
    ['{3C83F676-BD4C-48A7-910A-3A0C83802735}']
  end;

  ISecondBroken = interface(IInvokable)
    ['{F0C729D6-C825-40D5-BF72-F899CDCACB72}']
  end;

  IThirdBroken = interface(IInvokable)
    ['{08647D42-29B3-4556-9F2A-0B0DAF5125C0}']
  end;

  IScopedDep = interface(IInvokable)
    ['{5445FBCA-B04F-4EB3-BA9C-C9169920D314}']
  end;

  ICaptor = interface(IInvokable)
    ['{56B11C92-8534-4E0D-AE35-2F64EE571139}']
  end;

  IHealthy = interface(IInvokable)
    ['{9C5BEBBC-9402-4F63-8893-6CA936CE94B3}']
  end;

  TFirstBroken = class(TInterfacedObject, IFirstBroken)
  public
    constructor Create(const Missing: IMissing);
  end;

  TSecondBroken = class(TInterfacedObject, ISecondBroken)
  public
    constructor Create(const Missing: IMissing);
  end;

  TThirdBroken = class(TInterfacedObject, IThirdBroken)
  public
    constructor Create(const Missing: IMissing);
  end;

  TScopedDep = class(TInterfacedObject, IScopedDep)
  end;

  TCaptor = class(TInterfacedObject, ICaptor)
  public
    constructor Create(const Dep: IScopedDep);
  end;

  TDependent = class(TInterfacedObject, IHealthy)
  public
    constructor Create(const Dep: IScopedDep);
  end;

  /// Captura el scoped por PROPIEDAD: el constructor esta limpio, asi que una validacion que
  /// solo mire constructores lo declara sano.
  IPropCaptor = interface(IInvokable)
    ['{4C7A0E51-8B32-4D69-95F7-1A6E3C8B052D}']
  end;

  TPropCaptor = class(TInterfacedObject, IPropCaptor)
  private
    FDep: IScopedDep;
  public
    [DafInject]
    property Dep: IScopedDep read FDep write FDep;
  end;

  /// Ciclo con UNA arista por constructor: inconstruible por ese lado, aunque el otro vaya por
  /// propiedad.
  IMixedLeft = interface(IInvokable)
    ['{0E3B7C24-5A19-4F86-B2D0-7C4E1A9F5386}']
  end;

  IMixedRight = interface(IInvokable)
    ['{9A5C1F70-6D48-4B23-81E9-3F0B7C2D6E51}']
  end;

  TMixedLeft = class(TInterfacedObject, IMixedLeft)
  public
    constructor Create(const Right: IMixedRight);
  end;

  TMixedRight = class(TInterfacedObject, IMixedRight)
  private
    FLeft: IMixedLeft;
  public
    [DafInject]
    property Left: IMixedLeft read FLeft write FLeft;
  end;

  TValidationWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Failure: string;
    ResolveFailure: string;
    constructor Create;
    destructor Destroy; override;
    procedure BuildWith(const Options: TServiceProviderOptions);
    procedure TryResolve(const Resolve: TProc);
  end;

implementation

uses
  System.TypInfo,
  Daf.MiniSpec,
  Daf.DependencyInjection,
  DependencyInjection.Doubles,
  PropertyInjection.Feat;

constructor TFirstBroken.Create(const Missing: IMissing);
begin
  inherited Create;
end;

constructor TSecondBroken.Create(const Missing: IMissing);
begin
  inherited Create;
end;

constructor TThirdBroken.Create(const Missing: IMissing);
begin
  inherited Create;
end;

constructor TCaptor.Create(const Dep: IScopedDep);
begin
  inherited Create;
end;

constructor TDependent.Create(const Dep: IScopedDep);
begin
  inherited Create;
end;

constructor TMixedLeft.Create(const Right: IMixedRight);
begin
  inherited Create;
end;

{ TValidationWorld }

constructor TValidationWorld.Create;
begin
  inherited;
  Services := TServiceCollection.Create;
end;

destructor TValidationWorld.Destroy;
begin
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TValidationWorld.BuildWith(const Options: TServiceProviderOptions);
begin
  Failure := '';
  try
    Provider := Services.BuildServiceProvider(Options);
  except
    on E: Exception do Failure := E.Message;
  end;
end;

procedure TValidationWorld.TryResolve(const Resolve: TProc);
begin
  ResolveFailure := '';
  try
    Resolve;
  except
    on E: Exception do ResolveFailure := E.Message;
  end;
end;

initialization

Feature('''
Feature Dependency graph validation @di @validation

  As an application author
  I want a miswired graph to fail when the provider is built
  So I get a message at startup instead of an access violation halfway through an operation
''')

.UseWorld<TValidationWorld>

.Rule('The graph can be validated at build time')

  .Scenario('An unregistered dependency is caught at build time')
    .Given('a registered class whose constructor asks for a service nobody registered',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IFirstBroken, TFirstBroken>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('it fails naming the missing service, the class asking for it and the parameter',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('IMissing');
        Expect(W.Failure).ToContain('TFirstBroken');
        Expect(W.Failure).ToContain('Missing');
      end)

  .Scenario('All the failures are reported, not the first one')
    .Given('three registrations with unresolved dependencies',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IFirstBroken, TFirstBroken>;
        W.Services.AddSingleton<ISecondBroken, TSecondBroken>;
        W.Services.AddSingleton<IThirdBroken, TThirdBroken>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('the message lists the three of them',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('TFirstBroken');
        Expect(W.Failure).ToContain('TSecondBroken');
        Expect(W.Failure).ToContain('TThirdBroken');
      end)

  .Scenario('A singleton that captures a scoped service is rejected')
    .Given('a Singleton whose constructor asks for a Scoped',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.Services.AddSingleton<ICaptor, TCaptor>;
      end)
    .When('the provider is built with ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(False, True));
      end)
    .&Then('it fails naming both services and their lifetimes',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('singleton');
        Expect(W.Failure).ToContain('scoped');
        Expect(W.Failure).ToContain('ICaptor');
        Expect(W.Failure).ToContain('IScopedDep');
      end)

.Rule('Validation is opt-in and declares its reach')

  .Scenario('A registration by factory is neither validated nor blocked')
    .Given('a service registered with a factory that resolves its dependencies on its own',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IFoo),
          function(Provider: IServiceProvider): IInterface
          begin
            Result := TAppFoo.Create;
          end);
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, True));
      end)
    .&Then('that descriptor produces neither an error nor a false positive',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(W.Provider.GetRequiredService<IFoo>.Name).ToEqual('app');
      end)

  .Scenario('A healthy graph is not blocked')
    .Given('a composition whose dependencies are all registered, with coherent lifetimes',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.Services.AddScoped<IHealthy, TDependent>;
        W.Services.AddSingleton<IFoo, TAppFoo>;
      end)
    .When('the provider is built with ValidateOnBuild and ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, True));
      end)
    .&Then('the provider is built without flagging anything, and a scope resolves from it',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        // From a child scope: with ValidateScopes the root provider refuses a scoped service.
        var Scope := W.Provider.CreateScope;
        Expect(Scope.ServiceProvider.GetRequiredService<IHealthy> <> nil).ToBeTrue;
      end)

  .Scenario('A healthy graph with marked properties is not blocked either')
    .Given('a composition with classes taking part in property injection, all their dependencies registered',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IByField, TByField>;
      end)
    .When('the provider is built with ValidateOnBuild and ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, True));
      end)
    .&Then('the provider is built without flagging anything',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(W.Provider.GetRequiredService<IByField>.StorageName).ToEqual('storage');
      end)

  .Scenario('Without options nothing is validated')
    .Given('a registration with an unresolved dependency',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IFirstBroken, TFirstBroken>;
      end)
    .When('the provider is built the usual way',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.None);
      end)
    .&Then('building succeeds: validation is opt-in',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
      end)

.Rule('Validation reaches the marked properties, not only the constructors')

  .Scenario('An unregistered dependency asked for by property is caught the same way')
    .Given('a registered class with a satisfiable constructor and a marked property nobody registers',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<INeedsUnregistered, TNeedsUnregistered>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('it fails naming the missing service, the class asking for it and the property',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('IUnregistered');
        Expect(W.Failure).ToContain('TNeedsUnregistered');
        Expect(W.Failure).ToContain('Missing');
      end)

  .Scenario('A singleton capturing a scoped by property is rejected the same way')
    .Given('a Singleton with a marked property of a Scoped service',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.Services.AddSingleton<IPropCaptor, TPropCaptor>;
      end)
    .When('the provider is built with ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(False, True));
      end)
    .&Then('it fails naming both services, their lifetimes and the property',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('singleton');
        Expect(W.Failure).ToContain('scoped');
        Expect(W.Failure).ToContain('IPropCaptor');
        Expect(W.Failure).ToContain('IScopedDep');
        Expect(W.Failure).ToContain('Dep');
      end)

.Rule('A cycle the container cannot resolve is rejected at build time')

  .Scenario('A cycle with an edge by constructor')
    .Given('two services where one asks the other by constructor and the other asks back by marked property',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IMixedLeft, TMixedLeft>;
        W.Services.AddSingleton<IMixedRight, TMixedRight>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('it fails listing the path of the cycle and naming the constructor parameter that makes it unbreakable',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('cycle');
        Expect(W.Failure).ToContain('IMixedLeft');
        Expect(W.Failure).ToContain('IMixedRight');
        Expect(W.Failure).ToContain('constructor parameter "Right"');
      end)

  .Scenario('A cycle going through a transient')
    .Given('a cycle of two services by marked properties where one is registered as Transient',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<ISingletonPeer, TSingletonPeer>;
        W.Services.AddTransient<ITransientPeer, TTransientPeer>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('it fails listing the path and naming the Transient service as the unbreakable edge',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('cycle');
        Expect(W.Failure).ToContain('ITransientPeer');
        Expect(W.Failure).ToContain('transient');
      end)

  .Scenario('A cycle that can be broken is not rejected')
    .Given('a cycle of two Singleton services where every edge goes by marked property',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IAlphaSvc, TAlphaSvc>;
        W.Services.AddSingleton<IBetaSvc, TBetaSvc>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('the provider is built without flagging anything',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
      end)

  .Scenario('A service that asks for itself by property')
    .Given('a Singleton with a marked property of its own service type',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<ITwin, TTwin>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('the provider is built, and resolving it the property points at the same instance',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        var Twin := W.Provider.GetRequiredService<ITwin>;
        Expect(Twin.Peer.Serial).ToEqual(Twin.Serial);
      end)

  .Scenario('Several failures are still all listed, cycles included')
    .Given('three registrations with unresolved dependencies and one unbreakable cycle',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddSingleton<IFirstBroken, TFirstBroken>;
        W.Services.AddSingleton<ISecondBroken, TSecondBroken>;
        W.Services.AddSingleton<IThirdBroken, TThirdBroken>;
        W.Services.AddSingleton<IMixedLeft, TMixedLeft>;
        W.Services.AddSingleton<IMixedRight, TMixedRight>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TValidationWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('the message lists the four of them',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToContain('TFirstBroken');
        Expect(W.Failure).ToContain('TSecondBroken');
        Expect(W.Failure).ToContain('TThirdBroken');
        Expect(W.Failure).ToContain('cycle');
      end)

.Rule('with ValidateScopes, a scoped service is not resolved from the root provider')

  .Scenario('A scoped service asked of the root provider is rejected')
    .Given('a scoped service, and a provider built with ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.BuildWith(TServiceProviderOptions.Create(False, True));
      end)
    .When('it is resolved from the root provider',
      procedure(W: TValidationWorld)
      begin
        W.TryResolve(
          procedure
          begin
            W.Provider.GetRequiredService<IScopedDep>;
          end);
      end)
    .&Then('the resolution fails naming the service and saying it was asked of the root',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToEqual('');
        Expect(W.ResolveFailure).ToContain('Cannot resolve scoped service');
        Expect(W.ResolveFailure).ToContain('IScopedDep');
        Expect(W.ResolveFailure).ToContain('from root provider');
      end)

  .Scenario('A service that requires a scoped one, asked of the root provider, is rejected')
    .Given('a transient service that asks for a scoped one, and a provider built with ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.Services.AddTransient<IHealthy, TDependent>;
        W.BuildWith(TServiceProviderOptions.Create(False, True));
      end)
    .When('the transient is resolved from the root provider',
      procedure(W: TValidationWorld)
      begin
        W.TryResolve(
          procedure
          begin
            W.Provider.GetRequiredService<IHealthy>;
          end);
      end)
    .&Then('the resolution fails naming what was asked and the scoped service it requires',
      procedure(W: TValidationWorld)
      begin
        Expect(W.ResolveFailure).ToContain('IHealthy');
        Expect(W.ResolveFailure).ToContain('requires scoped service');
        Expect(W.ResolveFailure).ToContain('IScopedDep');
      end)

  .Scenario('The same scoped service asked of a child scope is built')
    .Given('a scoped service, and a provider built with ValidateScopes',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.BuildWith(TServiceProviderOptions.Create(False, True));
      end)
    .When('it is resolved from a child scope',
      procedure(W: TValidationWorld)
      begin
        W.TryResolve(
          procedure
          begin
            var Scope := W.Provider.CreateScope;
            Scope.ServiceProvider.GetRequiredService<IScopedDep>;
          end);
      end)
    .&Then('it is built',
      procedure(W: TValidationWorld)
      begin
        Expect(W.ResolveFailure).ToEqual('');
      end)

  .Scenario('Without ValidateScopes the root provider still resolves it')
    .Given('a scoped service, and a provider built without options',
      procedure(W: TValidationWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.BuildWith(TServiceProviderOptions.None);
      end)
    .When('it is resolved from the root provider',
      procedure(W: TValidationWorld)
      begin
        W.TryResolve(
          procedure
          begin
            W.Provider.GetRequiredService<IScopedDep>;
          end);
      end)
    .&Then('it is built',
      procedure(W: TValidationWorld)
      begin
        Expect(W.ResolveFailure).ToEqual('');
      end);

end.
