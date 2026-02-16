unit Activator.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
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
  .When('I activate it from TypeInfo',
    procedure(W: TActivatorWorld)
    begin
      W.Activated := TActivator.CreateInstance(TypeInfo(TActivatable)) as TActivatable;
    end)
  .&Then('the instance should be created',
    procedure(W: TActivatorWorld)
    begin
      Expect(Assigned(W.Activated)).ToBeTrue;
    end)
  .&And('the default value should be set',
    procedure(W: TActivatorWorld)
    begin
      Expect(W.Activated.Value).ToEqual(1);
    end)

.Scenario('Activate with default constructor from Class')
  .Given('a type with a default constructor').NoAction
  .When('I activate it from Class',
    procedure(W: TActivatorWorld)
    begin
      W.Activated := TActivator.CreateInstance(TActivatable) as TActivatable;
    end)
  .&Then('the instance should be created',
    procedure(W: TActivatorWorld)
    begin
      Expect(Assigned(W.Activated)).ToBeTrue;
    end)
  .&And('the default value should be set',
    procedure(W: TActivatorWorld)
    begin
      Expect(W.Activated.Value).ToEqual(1);
    end)

.Scenario('Activate with default constructor from Generic')
  .Given('a type with a default constructor').NoAction
  .When('I activate it from Generic',
    procedure(W: TActivatorWorld)
    begin
      W.Activated := TActivator.CreateInstance<TActivatable>;
    end)
  .&Then('the instance should be created',
    procedure(W: TActivatorWorld)
    begin
      Expect(Assigned(W.Activated)).ToBeTrue;
    end)
  .&And('the default value should be set',
    procedure(W: TActivatorWorld)
    begin
      Expect(W.Activated.Value).ToEqual(1);
    end)

.Scenario('Activate with Integer argument from Class')
  .Given('a type with a parameterized constructor').NoAction
  .When('I activate it from Class with argument 2',
    procedure(W: TActivatorWorld)
    begin
      W.Activated := TActivator.CreateInstance(TActivatable, [2]) as TActivatable;
    end)
  .&Then('the instance should be created',
    procedure(W: TActivatorWorld)
    begin
      Expect(Assigned(W.Activated)).ToBeTrue;
    end)
  .&And('the value should be 2',
    procedure(W: TActivatorWorld)
    begin
      Expect(W.Activated.Value).ToEqual(2);
    end)

.Scenario('Activate with Integer argument from Generic')
  .Given('a type with a parameterized constructor').NoAction
  .When('I activate it from Generic with argument 2',
    procedure(W: TActivatorWorld)
    begin
      W.Activated := TActivator.CreateInstance<TActivatable>([2]) as TActivatable;
    end)
  .&Then('the instance should be created',
    procedure(W: TActivatorWorld)
    begin
      Expect(Assigned(W.Activated)).ToBeTrue;
    end)
  .&And('the value should be 2',
    procedure(W: TActivatorWorld)
    begin
      Expect(W.Activated.Value).ToEqual(2);
    end);

end.
