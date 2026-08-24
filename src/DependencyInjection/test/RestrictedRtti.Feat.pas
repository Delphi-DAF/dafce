unit RestrictedRtti.Feat;

// La RTTI de esta unit va recortada a proposito: METHODS, PROPERTIES y FIELDS vacios. Es
// el caso que el descubrimiento dejaba ciego en silencio -- y la razon por la que se
// retiro. Aqui se afirma lo contrario: recortar la RTTI no cambia lo que se registra,
// porque el registro de modulos ya no la consulta.
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.AppModule.DependencyInjection,
  Doubles.Feat;

type
  /// Un modulo como cualquier otro, declarado bajo la RTTI recortada de esta unit.
  TRestrictedModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  /// Declarado, compilado y enlazado -- el runner lleva STRONGLINKTYPES ON, asi que el
  /// linker no lo descarta y la RTTI lo tiene delante. Lo unico que le falta es que
  /// alguien lo llame. Con el descubrimiento eso no importaba: aparecer en el paquete
  /// bastaba para entrar en el arranque.
  TNeverRegisteredModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.DependencyInjection,
  Modules.Feat;

{ TRestrictedModule }

procedure TRestrictedModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IFoo, TAppFoo>;
end;

{ TNeverRegisteredModule }

procedure TNeverRegisteredModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IBar, TAppBar>;
end;

// --- Feature definition ---

initialization

Feature('''
Feature Startup composition does not depend on reflection @di @composition

  As a developer
  I want the startup to register exactly what is written
  So restricting the RTTI cannot change what my application resolves
''')

.UseWorld<TFooWorld>

.Rule('A restricted RTTI does not change what gets registered')

  .Scenario('A module declared under a trimmed RTTI registers the same')
    .Given('a module whose unit compiles with METHODS, PROPERTIES and FIELDS emptied').NoAction
    .When('it is applied by name',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TRestrictedModule>;
      end)
    .&Then('its registration is there, and resolves',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('app');
      end)

.Rule('Nothing enters the startup without being written')

  .Scenario('A module nobody registers contributes nothing')
    .Given('a module that is declared and linked, but never named',
      procedure(W: TFooWorld)
      begin
        W.Services.AddModule<TRestrictedModule>;
      end)
    .When('the collection is composed',
      procedure(W: TFooWorld)
      begin
        W.Names := [IntToStr(Length(W.Services.Descriptors<IBar>))];
      end)
    .&Then('the unnamed module left no descriptor behind',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[0]).ToEqual('0');
      end)
    .&And('what was named is registered, so the collection did compose',
      procedure(W: TFooWorld)
      begin
        Expect(Length(W.Services.Descriptors<IFoo>)).ToEqual(1);
      end)

.Rule('The trimmed RTTI of this unit is real, not just declared')

  // Sin esto los escenarios de arriba podrian estar en verde con la RTTI completa y sus
  // titulos mentirian. Se compara contra un modulo de una unit sin recortar: si el
  // compilador ignorase la directiva, los dos conteos saldrian iguales y esto lo dice.
  .Scenario('The RTTI exposes no method of a type declared here, and does of one declared elsewhere')
    .When('the declared methods of both are counted',
      procedure(W: TFooWorld)
      begin
        var Ctx := TRttiContext.Create;
        try
          W.Names := [IntToStr(Length(Ctx.GetType(TRestrictedModule).GetDeclaredMethods)),
                      IntToStr(Length(Ctx.GetType(TFirstModule).GetDeclaredMethods))];
        finally
          Ctx.Free;
        end;
      end)
    .&Then('this unit contributes none',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[0]).ToEqual('0');
      end)
    .&And('and the unit without the directive does contribute',
      procedure(W: TFooWorld)
      begin
        Expect(StrToInt(W.Names[1])).ToBeGreaterThan(0);
      end);

end.
