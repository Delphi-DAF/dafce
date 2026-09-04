unit TrimmedRtti.Feat;

// La RTTI de esta unit va recortada a proposito: METHODS, PROPERTIES y FIELDS vacios. Bajo ese
// recorte las propiedades marcadas de sus clases NO EXISTEN para el barrido, asi que la inyeccion
// por atributo queda apagada -- sin error, porque no hay nada que el contenedor pueda ver para
// sospecharlo. Es un limite consciente, y esta aqui medido para que este escrito.
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

interface

uses
  Daf.Extensions.DependencyInjection,
  PropertyInjection.Feat;

type
  /// Marca una propiedad. Bajo el recorte esa marca no se ve, asi que para el contenedor esta
  /// clase no participa -- es indistinguible de la de abajo.
  TTrimmedParticipant = class(TInterfacedObject, ITrimmedParticipant)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  /// No marca nada. Lo suyo lo escribe un cableado por codigo, que no usa RTTI y por eso es la
  /// unica via inmune al recorte.
  TTrimmedOutsider = class(TInterfacedObject, ITrimmedOutsider)
  private
    FStorage: IStorage;
  public
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.DependencyInjection;

{ TTrimmedParticipant }

function TTrimmedParticipant.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TTrimmedOutsider }

function TTrimmedOutsider.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

initialization

Feature('''
Feature Property injection under a trimmed RTTI @di @injection @rtti

  As an application author
  I want the limit written down where I can find it
  So a unit whose RTTI directives hide the marks is a documented dead end and not a mystery
''')

.UseWorld<TInjectionWorld>

.Rule('The trimmed RTTI of this unit is real, not just declared')

  // Sin esto los escenarios de abajo podrian estar en verde con la RTTI completa y sus titulos
  // mentirian. Se compara contra una clase de una unit sin recortar: si el compilador ignorase la
  // directiva, los dos conteos saldrian iguales y esto lo dice.
  .Scenario('The RTTI exposes no property of a type declared here, and does of one declared elsewhere')
    .When('the properties of both are counted',
      procedure(W: TInjectionWorld)
      begin
        var Ctx := TRttiContext.Create;
        try
          // Declaradas, no todas: TInterfacedObject aporta RefCount a las dos, asi que un
          // conteo de GetProperties da uno donde el recorte funciona y dice lo contrario.
          W.Numbers := [Length(Ctx.GetType(TTrimmedParticipant).GetDeclaredProperties),
                        Length(Ctx.GetType(TByField).GetDeclaredProperties)];
        finally
          Ctx.Free;
        end;
      end)
    .&Then('this unit contributes none',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Numbers[0]).ToEqual(0);
      end)
    .&And('and the unit without the directive does contribute',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Numbers[1]).ToBeGreaterThan(0);
      end)

.Rule('A trimmed property RTTI switches the attribute injection off, and nothing says so')

  .Scenario('A class with a marked property in a trimmed unit')
    .Given('a class with a marked property, in a unit compiled with the property RTTI trimmed',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ITrimmedParticipant, TTrimmedParticipant>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ITrimmedParticipant>.StorageName; end);
      end)
    .&Then('it resolves with no error and the marked property is left in nil',
      procedure(W: TInjectionWorld)
      begin
        // El limite, escrito: la marca esta en el fuente y no llega a la RTTI, asi que para el
        // contenedor esta clase no participa. El fallo aparece en el primer uso, como un AV.
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / <nil>');
      end)

  .Scenario('A class that marks nothing behaves identically, which is the point')
    .Given('a class that marks nothing, in the same trimmed unit',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ITrimmedOutsider, TTrimmedOutsider>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ITrimmedOutsider>.StorageName; end);
      end)
    .&Then('the outcome is the same as the one above, so the container cannot tell them apart',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / <nil>');
      end)

  .Scenario('The build time validation does not flag it either')
    .Given('the same registered class with a marked property in the trimmed unit',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ITrimmedParticipant, TTrimmedParticipant>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TInjectionWorld)
      begin
        W.Attempt(procedure begin W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False)); end);
      end)
    .&Then('nothing is flagged: the property it would check is not in the RTTI to be checked',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
      end)

.Rule('A code wiring works where the attribute does not reach')

  .Scenario('A class in a trimmed unit is wired by code')
    .Given('a class in a unit compiled with the property RTTI trimmed, with a code wiring registered for its type',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ITrimmedOutsider, TTrimmedOutsider>;
        W.Services.AddInjector<TTrimmedOutsider>(
          procedure(const Instance: TTrimmedOutsider; const Provider: IServiceProvider)
          begin
            Instance.Storage := Provider.GetRequiredService<IStorage>;
          end);
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ITrimmedOutsider>.StorageName; end);
      end)
    .&Then('its property ends up filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)
    .&And('and it is the only way in: this one does not use RTTI at all',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
      end)

;

end.
