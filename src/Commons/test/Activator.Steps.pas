unit Activator.Steps;

interface

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.Activator,
  Activator.Feat;

type
  /// <summary>
  /// Step bindings for the Activator feature.
  /// </summary>
  TActivatorSteps = class
  public
    // === When ===

    [When('I activate it from TypeInfo')]
    procedure ActivateFromTypeInfo(W: TActivatorWorld);

    [When('I activate it from Class$')]
    procedure ActivateFromClass(W: TActivatorWorld);

    [When('I activate it from Generic$')]
    procedure ActivateFromGeneric(W: TActivatorWorld);

    [When('I activate it from Class with argument (\d+)')]
    procedure ActivateFromClassWithArg(W: TActivatorWorld; AValue: Integer);

    [When('I activate it from Generic with argument (\d+)')]
    procedure ActivateFromGenericWithArg(W: TActivatorWorld; AValue: Integer);

    // === Then ===

    [ThenAttribute('the instance should be created')]
    procedure AssertInstanceCreated(W: TActivatorWorld);

    [ThenAttribute('the default value should be set')]
    procedure AssertDefaultValue(W: TActivatorWorld);

    [ThenAttribute('the value should be (\d+)')]
    procedure AssertValue(W: TActivatorWorld; Expected: Integer);
  end;

{ TActivatorSteps }

procedure TActivatorSteps.ActivateFromTypeInfo(W: TActivatorWorld);
begin
  W.Activated := TActivator.CreateInstance(TypeInfo(TActivatable)) as TActivatable;
end;

procedure TActivatorSteps.ActivateFromClass(W: TActivatorWorld);
begin
  W.Activated := TActivator.CreateInstance(TActivatable) as TActivatable;
end;

procedure TActivatorSteps.ActivateFromGeneric(W: TActivatorWorld);
begin
  W.Activated := TActivator.CreateInstance<TActivatable>;
end;

procedure TActivatorSteps.ActivateFromClassWithArg(W: TActivatorWorld; AValue: Integer);
begin
  W.Activated := TActivator.CreateInstance(TActivatable, [TValue.From<Integer>(AValue)]) as TActivatable;
end;

procedure TActivatorSteps.ActivateFromGenericWithArg(W: TActivatorWorld; AValue: Integer);
begin
  W.Activated := TActivator.CreateInstance<TActivatable>([TValue.From<Integer>(AValue)]);
end;

procedure TActivatorSteps.AssertInstanceCreated(W: TActivatorWorld);
begin
  Expect(Assigned(W.Activated)).ToBeTrue;
end;

procedure TActivatorSteps.AssertDefaultValue(W: TActivatorWorld);
begin
  Expect(W.Activated.Value).ToEqual(1);
end;

procedure TActivatorSteps.AssertValue(W: TActivatorWorld; Expected: Integer);
begin
  Expect(W.Activated.Value).ToEqual(Expected);
end;

initialization
  Bindings.RegisterSteps<TActivatorSteps>;

end.
