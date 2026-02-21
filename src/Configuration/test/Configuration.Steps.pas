unit Configuration.Steps;

interface

implementation

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.Extensions.Configuration,
  Configuration.Feat;

type
  /// <summary>
  /// Step bindings for the Configuration feature.
  /// </summary>
  TConfigurationSteps = class
  public
    // === When ===

    [When('I build the configuration')]
    procedure BuildConfiguration(W: TConfigWorld);

    // === Then ===

    [ThenAttribute('the configuration should be created')]
    procedure AssertConfigCreated(W: TConfigWorld);
  end;

{ TConfigurationSteps }

procedure TConfigurationSteps.BuildConfiguration(W: TConfigWorld);
begin
  W.Config := W.Builder.Build;
end;

procedure TConfigurationSteps.AssertConfigCreated(W: TConfigWorld);
begin
  Expect(Assigned(W.Config)).ToBeTrue;
end;

initialization
  Bindings.RegisterSteps<TConfigurationSteps>;

end.
