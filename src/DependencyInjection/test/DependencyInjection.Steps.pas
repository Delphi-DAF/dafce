unit DependencyInjection.Steps;

interface

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  DependencyInjection.Feat;

type
  /// <summary>
  /// Step bindings for the DependencyInjection feature.
  /// Regex captures are auto-converted to method parameters.
  /// </summary>
  TDISteps = class
  public
    // === Given ===

    [Given('a (transient|singleton|scoped) ITestableService registered')]
    procedure RegisterByLifetime(W: TDIWorld; Lifetime: string);

    // === When ===

    [When('I register ITestableService with TTestableService as transient')]
    procedure RegisterTransient(W: TDIWorld);

    [When('I register ITestableService with a factory function')]
    procedure RegisterFactory(W: TDIWorld);

    [When('I add a transient service')]
    procedure AddTransient(W: TDIWorld);

    [When('I add a singleton service')]
    procedure AddSingleton(W: TDIWorld);

    [When('I resolve the service')]
    procedure ResolveService(W: TDIWorld);

    [When('I resolve two instances')]
    procedure ResolveTwoTransients(W: TDIWorld);

    [When('I resolve two references')]
    procedure ResolveTwoSingletons(W: TDIWorld);

    [When('I resolve twice from root provider')]
    procedure ResolveTwoScoped(W: TDIWorld);

    [When('I resolve from root and two child scopes')]
    procedure ResolveScopedInChildScopes(W: TDIWorld);

    // === Then ===

    [ThenAttribute('I can resolve the service')]
    procedure CanResolveService(W: TDIWorld);

    [ThenAttribute('the implementor class is TTestableService')]
    procedure AssertImplementorClass(W: TDIWorld);

    [ThenAttribute('after releasing it the instance count is 0')]
    procedure AssertZeroAfterOne(W: TDIWorld);

    [ThenAttribute('after releasing them the instance count is 0')]
    procedure AssertZeroAfterTwo(W: TDIWorld);

    [ThenAttribute('only one instance exists')]
    procedure AssertOneInstance(W: TDIWorld);

    [Given('a service collection')]
    procedure GivenServiceCollection(W: TDIWorld);

    [ThenAttribute('the service collection count increases')]
    procedure AssertCountIncreased(W: TDIWorld);

    [ThenAttribute('only one instance was created')]
    procedure AssertOnlyOneInstance(W: TDIWorld);

    [ThenAttribute('each child scope has a different instance')]
    procedure AssertEachScopeDifferent(W: TDIWorld);
  end;

function DIFactoryFunc(ServiceProvider: IServiceProvider): IInterface;
begin
  Result := TTestableService.Create;
end;

{ TDISteps }

procedure TDISteps.RegisterByLifetime(W: TDIWorld; Lifetime: string);
begin
  if Lifetime = 'transient' then
    W.ServiceCollection.AddTransient<ITestableService, TTestableService>
  else if Lifetime = 'singleton' then
    W.ServiceCollection.AddSingleton<ITestableService, TTestableService>
  else if Lifetime = 'scoped' then
    W.ServiceCollection.AddScoped<ITestableService, TTestableService>;
end;

procedure TDISteps.RegisterTransient(W: TDIWorld);
begin
  W.ServiceCollection.AddTransient<ITestableService, TTestableService>();
end;

procedure TDISteps.RegisterFactory(W: TDIWorld);
begin
  W.ServiceCollection.AddTransient(TypeInfo(ITestableService), DIFactoryFunc);
end;

procedure TDISteps.AddTransient(W: TDIWorld);
begin
  W.ServiceCollection.AddTransient<ITestableService, TTestableService>();
end;

procedure TDISteps.AddSingleton(W: TDIWorld);
begin
  W.ServiceCollection.AddSingleton<ITestableService, TTestableService>();
end;

procedure TDISteps.ResolveService(W: TDIWorld);
begin
  W.BuildProvider;
  W.ObtainedService := W.ServiceProvider.GetService<ITestableService>;
end;

procedure TDISteps.ResolveTwoTransients(W: TDIWorld);
var
  Svc1, Svc2: ITestableService;
begin
  W.BuildProvider;
  Svc1 := W.ServiceProvider.GetService<ITestableService>;
  Svc2 := W.ServiceProvider.GetService<ITestableService>;
  Expect(TTestableService.InstanceCount).ToEqual(2);
  Svc1 := nil;
  Svc2 := nil;
end;

procedure TDISteps.ResolveTwoSingletons(W: TDIWorld);
var
  Svc1, Svc2: ITestableService;
begin
  W.BuildProvider;
  Svc1 := W.ServiceProvider.GetService<ITestableService>;
  Svc2 := W.ServiceProvider.GetService<ITestableService>;
  Expect(TTestableService.InstanceCount).ToEqual(1);
end;

procedure TDISteps.ResolveTwoScoped(W: TDIWorld);
var
  Svc1, Svc2: ITestableService;
begin
  W.BuildProvider;
  Svc1 := W.ServiceProvider.GetRequiredService<ITestableService>;
  Svc2 := W.ServiceProvider.GetRequiredService<ITestableService>;
  Expect(Svc1 = Svc2).ToBeTrue;
end;

procedure TDISteps.ResolveScopedInChildScopes(W: TDIWorld);
var
  Svc1_a, Svc1_b: ITestableService;
begin
  W.BuildProvider;
  W.ChildScope1 := W.ServiceProvider.CreateScope;
  W.ChildScope2 := W.ServiceProvider.CreateScope;
  Svc1_a := W.ChildScope1.ServiceProvider.GetRequiredService<ITestableService>;
  Svc1_b := W.ChildScope1.ServiceProvider.GetRequiredService<ITestableService>;
  // Within the same scope the cached instance is returned
  Expect(Svc1_a = Svc1_b).ToBeTrue;
end;

procedure TDISteps.CanResolveService(W: TDIWorld);
begin
  W.BuildProvider;
  W.ObtainedService := W.ServiceProvider.GetService<ITestableService>;
  Expect(Assigned(W.ObtainedService)).ToBeTrue;
end;

procedure TDISteps.AssertImplementorClass(W: TDIWorld);
begin
  Expect(W.ObtainedService.GetImplementorClass.ClassName).ToEqual('TTestableService');
end;

procedure TDISteps.AssertZeroAfterOne(W: TDIWorld);
begin
  W.ObtainedService := nil;
  Expect(TTestableService.InstanceCount).ToEqual(0);
end;

procedure TDISteps.AssertZeroAfterTwo(W: TDIWorld);
begin
  Expect(TTestableService.InstanceCount).ToEqual(0);
end;

procedure TDISteps.AssertOneInstance(W: TDIWorld);
begin
  Expect(TTestableService.InstanceCount).ToEqual(1);
end;

procedure TDISteps.GivenServiceCollection(W: TDIWorld);
begin
  W.CountBeforeAdd := W.ServiceCollection.Count;
end;

procedure TDISteps.AssertCountIncreased(W: TDIWorld);
begin
  Expect(W.ServiceCollection.Count).ToEqual(W.CountBeforeAdd + 1);
end;

procedure TDISteps.AssertOnlyOneInstance(W: TDIWorld);
begin
  Expect(TTestableService.InstanceCount).ToEqual(1);
end;

procedure TDISteps.AssertEachScopeDifferent(W: TDIWorld);
var
  Svc1, Svc2: ITestableService;
begin
  Svc1 := W.ChildScope1.ServiceProvider.GetRequiredService<ITestableService>;
  Svc2 := W.ChildScope2.ServiceProvider.GetRequiredService<ITestableService>;
  Expect(Svc1 <> Svc2).ToBeTrue;
  Expect(TTestableService.InstanceCount).ToEqual(2);
end;

initialization
  Bindings.RegisterSteps<TDISteps>;

end.
