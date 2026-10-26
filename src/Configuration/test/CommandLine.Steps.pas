unit CommandLine.Steps;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.Extensions.Configuration,
  Daf.Configuration.CommandLine,
  CommandLine.Feat;

type
  /// <summary>
  ///   Step bindings for the command-line configuration feature.
  /// </summary>
  TCommandLineSteps = class
  private
    FRaised: Exception;
  public
    destructor Destroy; override;

    [When('the source is assembled')]
    procedure AssembleSource(W: TCmdLineWorld);

    [ThenAttribute('it fails while composing, naming the alias')]
    procedure AssertComposeFailed(W: TCmdLineWorld);
  end;

{ TCommandLineSteps }

destructor TCommandLineSteps.Destroy;
begin
  FRaised.Free;
  inherited;
end;

procedure TCommandLineSteps.AssembleSource(W: TCmdLineWorld);
begin
  FreeAndNil(FRaised);
  try
    CommandLineConfig.Source([], [TSwitchMapping.Create('-v', 'One'),
                                  TSwitchMapping.Create('-v', 'Two')]);
  except
    on E: Exception do
      FRaised := Exception(AcquireExceptionObject);
  end;
end;

procedure TCommandLineSteps.AssertComposeFailed(W: TCmdLineWorld);
begin
  // ToEqual imprime el valor real; ToBeEmpty no, y aqui el fallo tiene que NOMBRAR el alias.
  if not Assigned(FRaised) then
    Expect('no exception was raised').ToEqual('a CommandLineConfigError naming "-v"');
  Expect(FRaised.Message).ToContain('-v');
end;

initialization
  Bindings.RegisterSteps<TCommandLineSteps>;

end.
