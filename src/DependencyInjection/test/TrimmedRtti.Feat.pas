unit TrimmedRtti.Feat;

// La RTTI de esta unit va recortada a proposito: METHODS, PROPERTIES y FIELDS vacios. Bajo ese
// recorte las propiedades marcadas de sus clases NO EXISTEN para el barrido, que es el unico modo
// de fallo del mecanismo indistinguible del acierto sin un aviso.
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

interface

uses
  Daf.Extensions.DependencyInjection,
  PropertyInjection.Feat;

type
  /// Declara participar y marca una propiedad. Bajo el recorte no se ve ninguna de las dos cosas
  /// salvo la declaracion, que es de clase.
  [DafInjectProperties]
  TTrimmedParticipant = class(TInterfacedObject, ITrimmedParticipant)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  /// No declara participar, asi que el barrido no tiene nada que decir de ella. Lo suyo lo
  /// escribe un cableado por codigo, que no usa RTTI y por eso es la unica via inmune al recorte.
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
  I want to be told when the RTTI directives of a unit hide the marks
  So the injection cannot be switched off without a single symptom
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

.Rule('A trimmed RTTI does not switch the injection off in silence')

  .Scenario('A class that participates and exposes no marked property')
    .Given('a class that declares participation, with a marked property, in a unit compiled with the property RTTI trimmed',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ITrimmedParticipant, TTrimmedParticipant>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<ITrimmedParticipant>; end);
      end)
    .&Then('it fails naming the class and pointing at the RTTI directives of its unit',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('TTrimmedParticipant');
        Expect(W.Outcome).ToContain('RTTI');
      end)
    .&And('and it does not hand out an instance with the property nil',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Held = nil).ToBeTrue;
      end)

  .Scenario('A class that does not participate says nothing')
    .Given('a class that does not declare participation, in the same trimmed unit',
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
    .&Then('it resolves as always, with no error and nothing injected',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
        Expect(W.Report).ToEqual('<nil>');
      end)

.Rule('The build time validation warns about it too, which is where it arrives first')

  .Scenario('A class that participates under a trimmed RTTI')
    .Given('a registered class that declares participation, with a marked property, in a unit compiled with the property RTTI trimmed',
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
    .&Then('it fails naming the class and pointing at the RTTI directives of its unit',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('TTrimmedParticipant');
        Expect(W.Outcome).ToContain('RTTI');
      end)

  .Scenario('A class that does not participate does not show up in the report')
    .Given('a registered class that does not declare participation, in the same trimmed unit',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ITrimmedOutsider, TTrimmedOutsider>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TInjectionWorld)
      begin
        W.Attempt(procedure begin W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False)); end);
      end)
    .&Then('nothing is flagged about it',
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
    .&And('and the trimmed RTTI warning does not fire, because the class does not declare participation',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
      end)

;

end.
