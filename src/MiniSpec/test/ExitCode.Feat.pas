unit ExitCode.Feat;

// El codigo de salida solo se puede observar desde fuera del proceso, y ese arnes no existe. Por eso
// la regla vive en TMiniSpecOptions.ExitCodeFor, como funcion de los tres recuentos: asi la tabla de
// verdad entera se ejercita aqui dentro.

interface

implementation

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Runner;

type
  TExitCodeWorld = class
  strict private
    FOptions: TMiniSpecOptions;
  public
    // El campo va antes que la propiedad: Delphi no admite una definicion de campo detras de un
    // metodo o una propiedad, y el E2169 arrastra un E2250 por cada step del fichero.
    Code: Integer;
    constructor Create;
    destructor Destroy; override;
    property Options: TMiniSpecOptions read FOptions;
  end;

constructor TExitCodeWorld.Create;
begin
  inherited;
  FOptions := TMiniSpecOptions.Create;
end;

destructor TExitCodeWorld.Destroy;
begin
  FOptions.Free;
  inherited;
end;

initialization

Feature('''
Exit code @exit-code

  As whoever runs a suite in CI
  I want the exit code to separate what is missing from what I declared pending
  So a green build means the specs ran
''')

.UseWorld<TExitCodeWorld>

.Scenario('A failed spec sets the exit code')
  .When('the suite finishes with one failure', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(1, 0, 0);
    end)
  .&Then('the exit code is 1', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(1);
    end)

.Scenario('A step with no binding sets the exit code')
  .When('the suite finishes with one undefined and nothing else', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(0, 1, 0);
    end)
  .&Then('the exit code is 1', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(1);
    end)

.Scenario('Pending specs alone do not set the exit code')
  .When('the suite finishes with two pending and nothing else', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(0, 0, 2);
    end)
  .&Then('the exit code is 0', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(0);
    end)

.Scenario('With allow-undefined an undefined releases the exit code')
  .Given('the runner is told to allow undefined steps', procedure(W: TExitCodeWorld)
    begin
      W.Options.AllowUndefined := True;
    end)
  .When('the suite finishes with one undefined and nothing else', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(0, 1, 0);
    end)
  .&Then('the exit code is 0', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(0);
    end)

// El control negativo del escenario anterior: el mismo recuento sin la bandera. Sin el, un
// ExitCodeFor que devolviera siempre cero pasaria aquel y este no.
.Scenario('Without the flag that same count still sets the exit code')
  .When('the suite finishes with one undefined and no flag', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(0, 1, 0);
    end)
  .&Then('the exit code is 1', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(1);
    end)

.Scenario('A failure sets the exit code even when undefined are allowed')
  .Given('the runner is told to allow undefined steps', procedure(W: TExitCodeWorld)
    begin
      W.Options.AllowUndefined := True;
    end)
  .When('the suite finishes with one failure and one undefined', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(1, 1, 0);
    end)
  .&Then('the exit code is 1', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(1);
    end)

.Scenario('Each flag moves its own state and only its own')
  .Given('both flags are set', procedure(W: TExitCodeWorld)
    begin
      W.Options.AllowUndefined := True;
      W.Options.StrictPending := True;
    end)
  .When('the suite finishes with one undefined and one pending', procedure(W: TExitCodeWorld)
    begin
      W.Code := W.Options.ExitCodeFor(0, 1, 1);
    end)
  .&Then('the exit code is 1, because the pending one breaks', procedure(W: TExitCodeWorld)
    begin
      Expect(W.Code).ToEqual(1);
    end);

end.
