unit Validation.Feat;

interface

uses
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

  TValidationWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Failure: string;
    constructor Create;
    destructor Destroy; override;
    procedure BuildWith(const Options: TServiceProviderOptions);
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.DependencyInjection,
  Doubles.Feat;

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
    .&Then('the provider is built without flagging anything',
      procedure(W: TValidationWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(W.Provider.GetRequiredService<IHealthy> <> nil).ToBeTrue;
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
      end);

end.
