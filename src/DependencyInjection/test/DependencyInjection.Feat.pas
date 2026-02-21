unit DependencyInjection.Feat;

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection;

type
  ITestableService = interface(IInterface)
    ['{F88F262A-6591-452C-BC93-E5D2DAA48875}']
    function GetRefCount: Integer;
    function GetImplementorClass: TClass;
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Value: string read GetValue write SetValue;
  end;

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

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

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

// --- Feature definition ---

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
    .When('I register ITestableService with TTestableService as transient')
    .&Then('I can resolve the service')
    .&And('the implementor class is TTestableService')

  .Scenario('Register with factory function')
    .Given('a service collection').NoAction
    .When('I register ITestableService with a factory function')
    .&Then('I can resolve the service')
    .&And('the implementor class is TTestableService')

// --- Transient Lifetime ---

.Rule('Transient Lifetime')

  .Scenario('Can add a transient service')
    .Given('a service collection').NoAction
    .When('I add a transient service')
    .&Then('the service collection count increases').NoAction

  .Scenario('Can resolve a transient service')
    .Given('a transient ITestableService registered')
    .When('I resolve the service')
    .&Then('after releasing it the instance count is 0')

  .Scenario('Transient returns new instance each time')
    .Given('a transient ITestableService registered')
    .When('I resolve two instances')
    .&Then('after releasing them the instance count is 0')

// --- Singleton Lifetime ---

.Rule('Singleton Lifetime')

  .Scenario('Can add a singleton service')
    .Given('a service collection').NoAction
    .When('I add a singleton service')
    .&Then('the service collection count increases').NoAction

  .Scenario('Singleton returns same instance')
    .Given('a singleton ITestableService registered')
    .When('I resolve two references')
    .&Then('only one instance exists')

// --- Scoped Lifetime ---

.Rule('Scoped Lifetime')

  .Scenario('Scoped returns same instance within scope')
    .Given('a scoped ITestableService registered')
    .When('I resolve twice from root provider')
    .&Then('only one instance was created').NoAction

  .Scenario('Different scopes get different instances')
    .Given('a scoped ITestableService registered')
    .When('I resolve from root and two child scopes')
    .&Then('three separate instances exist').NoAction;

end.
