unit SuiteContext.Feat;

// El tipo del contexto va en la interface por dos razones: el .dpr lo nombra en
// MiniSpec.UseSuiteContext<T>, y un tipo que solo aparece en implementation lo descarta el linker.

interface

type
  /// Recurso compartido por toda la suite. El contador y la instancia se guardan como class var
  /// porque la pregunta que hay que contestar --cuantas veces se construyo-- no la puede contestar
  /// el propio objeto: haria falta tener los dos para compararlos.
  TSuiteResources = class
  public
    class var CreateCount: Integer;
    class var Construido: TObject;
    constructor Create;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  TSuiteCtxWorld = class
  public
    Leido: TObject;
    Veces: Integer;
  end;

constructor TSuiteResources.Create;
begin
  inherited;
  Inc(CreateCount);
  Construido := Self;
end;

initialization

Feature('''
Suite context @context

  As a test author
  I want state shared by every feature of a suite
  So an expensive resource is prepared once and not once per scenario
''')

.UseWorld<TSuiteCtxWorld>

.Scenario('The suite context a step reads is the one that was constructed')
  .When('a step asks the runner for the suite context', procedure(W: TSuiteCtxWorld)
    begin
      W.Leido := SpecContext.SuiteContext;
    end)
  .&Then('it is there, and it is that instance', procedure(W: TSuiteCtxWorld)
    begin
      Expect(W.Leido <> nil).ToBeTrue;
      Expect(W.Leido = TSuiteResources.Construido).ToBeTrue;
    end)

// El contador es lo que separa «hay un contexto» de «hay UNO». Con uno por feature saldria 21, y
// sin contexto ninguno saldria 0: los dos casos rompen este escenario y no el anterior.
.Scenario('It is constructed exactly once for the whole suite')
  .When('the suite has been running', procedure(W: TSuiteCtxWorld)
    begin
      W.Veces := TSuiteResources.CreateCount;
    end)
  .&Then('the context was constructed once', procedure(W: TSuiteCtxWorld)
    begin
      Expect(W.Veces).ToEqual(1);
    end);

end.
