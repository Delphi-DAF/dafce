unit Activator.Feat;

interface

uses
  Daf.Activator;

type
  {$TYPEINFO ON}
  TActivatable = class
  private
    FValue: Integer;
  public
    constructor Create; overload;
    constructor Create(const AValue: Integer); overload;
    property Value: Integer read FValue write FValue;
  end;
  {$TYPEINFO OFF}

  TActivatorWorld = class
  public
    Activated: TActivatable;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TActivatable }

constructor TActivatable.Create;
begin
  inherited;
  Create(1);
end;

constructor TActivatable.Create(const AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

{ TActivatorWorld }

destructor TActivatorWorld.Destroy;
begin
  Activated.Free;
  inherited;
end;

// --- Feature definition ---

initialization

Feature('''
Feature Activator @activator

  As a framework developer
  I want to create instances dynamically via TActivator
  So I can support dependency injection and plugin architectures
''')

.UseWorld<TActivatorWorld>

.Scenario('Activate with default constructor from TypeInfo')
  .Given('a type with a default constructor').NoAction
  .When('I activate it from TypeInfo')
  .&Then('the instance should be created')
  .&And('the default value should be set')

.Scenario('Activate with default constructor from Class')
  .Given('a type with a default constructor').NoAction
  .When('I activate it from Class')
  .&Then('the instance should be created')
  .&And('the default value should be set')

.Scenario('Activate with default constructor from Generic')
  .Given('a type with a default constructor').NoAction
  .When('I activate it from Generic')
  .&Then('the instance should be created')
  .&And('the default value should be set')

.Scenario('Activate with Integer argument from Class')
  .Given('a type with a parameterized constructor').NoAction
  .When('I activate it from Class with argument 2')
  .&Then('the instance should be created')
  .&And('the value should be 2')

.Scenario('Activate with Integer argument from Generic')
  .Given('a type with a parameterized constructor').NoAction
  .When('I activate it from Generic with argument 2')
  .&Then('the instance should be created')
  .&And('the value should be 2');

end.
