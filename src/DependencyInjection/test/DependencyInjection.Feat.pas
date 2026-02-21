unit DependencyInjection.Feat;

interface

type
  ITestableService = interface(IInterface)
    ['{F88F262A-6591-452C-BC93-E5D2DAA48875}']
    function GetRefCount: Integer;
    function GetImplementorClass: TClass;
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Value: string read GetValue write SetValue;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  Daf.MemUtils;

type
  TTestableService = class(TDIObject<TTestableService>, ITestableService)
  private
    FValue: string;
  public
    procedure AfterConstruction; override;
    function GetImplementorClass: TClass;
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Value: string read GetValue write SetValue;
  end;

  TDIWorld = class
  public
    ServiceCollection: IServiceCollection;
    ServiceProvider: IServiceProvider;
    ObtainedService: ITestableService;
    constructor Create;
    destructor Destroy; override;
    procedure BuildProvider;
  end;

{ TTestableService }

procedure TTestableService.AfterConstruction;
begin
  inherited;
  FValue := 'value is not assigned';
end;

function TTestableService.GetImplementorClass: TClass;
begin
  Result := Self.ClassType;
end;

function TTestableService.GetValue: string;
begin
  Result := FValue;
end;

procedure TTestableService.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ TDIWorld }

constructor TDIWorld.Create;
begin
  inherited;
  ServiceCollection := TServiceCollection.Create;
end;

destructor TDIWorld.Destroy;
begin
  ObtainedService := nil;
  if ServiceProvider <> nil then
    ServiceProvider.ShutDown;
  inherited;
end;

procedure TDIWorld.BuildProvider;
begin
  if ServiceProvider = nil then
    ServiceProvider := ServiceCollection.BuildServiceProvider;
end;

function FactoryFunc(ServiceProvider: IServiceProvider): IInterface;
begin
  Result := TTestableService.Create;
end;

// --- Steps ---

procedure Step_RegisterTransient(W: TDIWorld);
begin
  W.ServiceCollection.AddTransient<ITestableService, TTestableService>();
end;

procedure Step_ResolveFromTypeInfo(W: TDIWorld);
begin
  W.BuildProvider;
  W.ObtainedService := W.ServiceProvider.GetService<ITestableService>;
end;

procedure Step_AssertObtainedAssigned(W: TDIWorld);
begin
  Expect(Assigned(W.ObtainedService)).ToBeTrue;
end;

procedure Step_AssertImplementorClass(W: TDIWorld);
begin
  Expect(W.ObtainedService.GetImplementorClass.ClassName).ToEqual('TTestableService');
end;

procedure Step_RegisterFactory(W: TDIWorld);
begin
  W.ServiceCollection.AddTransient(TypeInfo(ITestableService), FactoryFunc);
end;

procedure Step_AddTransientCheckCount(W: TDIWorld);
var SavedCount: Integer;
begin
  SavedCount := W.ServiceCollection.Count;
  W.ServiceCollection.AddTransient<ITestableService, TTestableService>();
  Expect(W.ServiceCollection.Count).ToEqual(SavedCount + 1);
  W.BuildProvider;
end;

procedure Step_ResolveAndReleaseTransient(W: TDIWorld);
var
  Svc: ITestableService;
begin
  W.BuildProvider;
  Svc := W.ServiceProvider.GetService<ITestableService>;
  Expect(TTestableService.InstanceCount).ToEqual(1);
  Svc := nil;
end;

procedure Step_AssertZeroInstances(W: TDIWorld);
begin
  Expect(TTestableService.InstanceCount).ToEqual(0);
end;

procedure Step_ResolveTwoTransients(W: TDIWorld);
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

procedure Step_AddSingleton(W: TDIWorld);
var SavedCount: Integer;
begin
  SavedCount := W.ServiceCollection.Count;
  W.ServiceCollection.AddSingleton<ITestableService, TTestableService>();
  Expect(W.ServiceCollection.Count).ToEqual(SavedCount + 1);
  W.BuildProvider;
end;

procedure Step_RegisterSingleton(W: TDIWorld);
begin
  W.ServiceCollection.AddSingleton<ITestableService, TTestableService>();
end;

procedure Step_ResolveTwoSingletons(W: TDIWorld);
var
  Svc1, Svc2: ITestableService;
begin
  W.BuildProvider;
  Svc1 := W.ServiceProvider.GetService<ITestableService>;
  Svc2 := W.ServiceProvider.GetService<ITestableService>;
  Expect(TTestableService.InstanceCount).ToEqual(1);
end;

procedure Step_AssertOneInstance(W: TDIWorld);
begin
  Expect(TTestableService.InstanceCount).ToEqual(1);
end;

procedure Step_RegisterScoped(W: TDIWorld);
begin
  W.ServiceCollection.AddScoped<ITestableService, TTestableService>;
end;

procedure Step_ResolveTwoScoped(W: TDIWorld);
var
  Svc1, Svc2: ITestableService;
begin
  W.BuildProvider;
  Svc1 := W.ServiceProvider.GetRequiredService<ITestableService>;
  Svc2 := W.ServiceProvider.GetRequiredService<ITestableService>;
  Expect(Svc1 = Svc2).ToBeTrue;
end;

procedure Step_ResolveScopedInChildScopes(W: TDIWorld);
var
  Scope1, Scope2: IServiceScope;
  Svc1_a, Svc1_b, Svc2_a: ITestableService;
begin
  W.BuildProvider;

  Scope1 := W.ServiceProvider.CreateScope;
  Scope2 := W.ServiceProvider.CreateScope;

  Svc1_a := Scope1.ServiceProvider.GetRequiredService<ITestableService>;
  Svc1_b := Scope1.ServiceProvider.GetRequiredService<ITestableService>;
  // Same scope returns same instance
  Expect(Svc1_a = Svc1_b).ToBeTrue;

  Svc2_a := Scope2.ServiceProvider.GetRequiredService<ITestableService>;
  // Different scope returns different instance
  Expect(Svc1_a = Svc2_a).ToBeFalse;
end;

initialization

Feature('''
Feature DependencyInjection @di

  As a framework developer
  I want to register and resolve services via dependency injection
  So I can build loosely coupled applications
''')

.UseWorld<TDIWorld>

// --- Service Registration ---

.Rule('Service Registration')

  .Scenario('Register with implementor class')
    .Given('a service collection').NoAction
    .When('I register ITestableService with TTestableService as transient', Step_RegisterTransient)
    .&Then('I can resolve the service', Step_ResolveFromTypeInfo)
    .&And('the implementor class is TTestableService', Step_AssertImplementorClass)

  .Scenario('Register with factory function')
    .Given('a service collection').NoAction
    .When('I register ITestableService with a factory function', Step_RegisterFactory)
    .&Then('I can resolve the service', Step_ResolveFromTypeInfo)
    .&And('the implementor class is TTestableService', Step_AssertImplementorClass)

// --- Transient Lifetime ---

.Rule('Transient Lifetime')

  .Scenario('Can add a transient service')
    .Given('a service collection').NoAction
    .When('I add a transient service', Step_AddTransientCheckCount)
    .&Then('the service collection count increases').NoAction

  .Scenario('Can resolve a transient service')
    .Given('a transient ITestableService registered', Step_RegisterTransient)
    .When('I resolve the service', Step_ResolveAndReleaseTransient)
    .&Then('after releasing it the instance count is 0', Step_AssertZeroInstances)

  .Scenario('Transient returns new instance each time')
    .Given('a transient ITestableService registered', Step_RegisterTransient)
    .When('I resolve two instances', Step_ResolveTwoTransients)
    .&Then('after releasing them the instance count is 0', Step_AssertZeroInstances)

// --- Singleton Lifetime ---

.Rule('Singleton Lifetime')

  .Scenario('Can add a singleton service')
    .Given('a service collection').NoAction
    .When('I add a singleton service', Step_AddSingleton)
    .&Then('the service collection count increases').NoAction

  .Scenario('Singleton returns same instance')
    .Given('a singleton ITestableService registered', Step_RegisterSingleton)
    .When('I resolve two references', Step_ResolveTwoSingletons)
    .&Then('only one instance exists', Step_AssertOneInstance)

// --- Scoped Lifetime ---

.Rule('Scoped Lifetime')

  .Scenario('Scoped returns same instance within scope')
    .Given('a scoped ITestableService registered', Step_RegisterScoped)
    .When('I resolve twice from root provider', Step_ResolveTwoScoped)
    .&Then('only one instance was created').NoAction

  .Scenario('Different scopes get different instances')
    .Given('a scoped ITestableService registered', Step_RegisterScoped)
    .When('I resolve from root and two child scopes', Step_ResolveScopedInChildScopes)
    .&Then('three separate instances exist').NoAction;

end.
