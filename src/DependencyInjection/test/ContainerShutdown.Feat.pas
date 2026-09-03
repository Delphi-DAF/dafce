unit ContainerShutdown.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  PropertyInjection.Feat;

type
  /// <summary>
  /// Los dos ciclos de vida que el cierre trata distinto hoy: el Singleton vive en el ambito de
  /// singletons —el que el cierre anula— y el Scoped no. Por eso el defecto se manifiesta en uno
  /// como Access Violation y en el otro como silencio.
  /// </summary>
  IAlpha = interface(IInvokable)
    ['{00E8BF16-AE8C-48F3-A156-976B11B296C4}']
    function Name: string;
  end;

  IBeta = interface(IInvokable)
    ['{10EF0E06-899D-4459-A2AE-ED55ACF728CE}']
    function Name: string;
  end;

  TAlpha = class(TInterfacedObject, IAlpha)
  public
    // Publicas a proposito: son el instrumento de medida del spec, no estado del servicio.
    class var Created: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    function Name: string;
  end;

  TBeta = class(TInterfacedObject, IBeta)
  public
    class var Created: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    function Name: string;
  end;

  /// <summary>
  /// Captura el resultado de cada intento en una cadena —clase de la excepcion y mensaje, o el
  /// centinela de que no hubo error— en vez de dejarlo escapar. Asi el spec puede afirmar CUAL fue
  /// el fallo: mientras el defecto este presente lo que sale es un EAccessViolation, y un spec que
  /// solo dijera "fallo" lo dara por bueno.
  /// </summary>
  TShutdownWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Child: IServiceScope;
    HeldBeta: IBeta;
    LastName: string;
    Outcome: string;
    AliveAfterFirstClose, AliveAfterSecondClose: Integer;
    BetaCreatedBefore, BetaCreatedAfter: Integer;
    OwnedBefore, OwnedAfter: Integer;
    // El objeto que el contenedor cablea sin construir: no es suyo, asi que el cierre no le
    // vacia las propiedades ni lo destruye.
    Guest: TGuest;
    Closed: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure BuildAndResolve;
    // Resuelve sin quedarse con nada: el contador de vivos solo dice algo del desmontaje si
    // nadie de fuera sostiene lo que se cuenta.
    procedure BuildAndForget;
    procedure OpenChildScope;
    procedure Close;
    procedure Attempt(const Action: TProc);
    function Owned: Integer;
  end;

const
  // Lo que Attempt escribe cuando la accion NO lanzo. Un centinela con texto, y no la cadena vacia,
  // porque la cadena vacia es tambien lo que se lee cuando nadie escribio nada.
  SinError = '<sin error>';

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities;

{ TAlpha }

constructor TAlpha.Create;
begin
  inherited Create;
  Inc(Created);
  Inc(Alive);
end;

destructor TAlpha.Destroy;
begin
  Dec(Alive);
  inherited;
end;

function TAlpha.Name: string;
begin
  Result := 'alpha';
end;

{ TBeta }

constructor TBeta.Create;
begin
  inherited Create;
  Inc(Created);
  Inc(Alive);
end;

destructor TBeta.Destroy;
begin
  Dec(Alive);
  inherited;
end;

function TBeta.Name: string;
begin
  Result := 'beta';
end;

{ TShutdownWorld }

constructor TShutdownWorld.Create;
begin
  inherited;
  TAlpha.Created := 0;
  TAlpha.Alive := 0;
  TBeta.Created := 0;
  TBeta.Alive := 0;
  TAlphaSvc.Destroyed := 0;
  TBetaSvc.Destroyed := 0;
  TRed.Alive := 0;
  TGreen.Alive := 0;
  TBlue.Alive := 0;
  TCounted.Destroyed := 0;
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TShutdownWorld.Destroy;
begin
  Child := nil;
  HeldBeta := nil;
  Guest.Free;
  // No se cierra otra vez desde aqui: mientras el defecto este presente, el segundo cierre revienta,
  // y un runner que muere en el teardown no dice que asercion fallo.
  if not Closed then
    Provider.ShutDown;
  inherited;
end;

procedure TShutdownWorld.BuildAndResolve;
begin
  Services.AddSingleton<IAlpha, TAlpha>;
  Services.AddScoped<IBeta, TBeta>;
  Provider := Services.BuildServiceProvider;
  LastName := Provider.GetRequiredService<IAlpha>.Name;
  HeldBeta := Provider.GetRequiredService<IBeta>;
end;

procedure TShutdownWorld.OpenChildScope;
begin
  Child := Provider.CreateScope;
  Child.ServiceProvider.GetRequiredService<IBeta>;
end;

procedure TShutdownWorld.Close;
begin
  Provider.ShutDown;
  Closed := True;
end;

procedure TShutdownWorld.BuildAndForget;
begin
  Services.AddSingleton<IAlpha, TAlpha>;
  Services.AddScoped<IBeta, TBeta>;
  Provider := Services.BuildServiceProvider;
  Provider.GetRequiredService<IAlpha>;
  Provider.GetRequiredService<IBeta>;
end;

procedure TShutdownWorld.Attempt(const Action: TProc);
begin
  Outcome := SinError;
  try
    Action();
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function TShutdownWorld.Owned: Integer;
begin
  var Access: IContainerAccess;
  if not Supports(Provider, IContainerAccess, Access) then
    raise Exception.Create('the provider does not expose IContainerAccess');
  Result := Access.OwnedInstanceCount;
end;

initialization

Feature('''
Feature Container shutdown leaves a defined state @di @lifetime @shutdown

  As an application author
  I want a closed container to say so instead of crashing or half-answering
  So that shutting down is a state I can reason about, not a race I discover in production
''')

.UseWorld<TShutdownWorld>

.Rule('A closed container does not resolve')

  .Scenario('Resolving a singleton after shutdown')
    .Given('a container with a singleton already resolved once',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
      end)
    .When('I close it and ask for that singleton again',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
        W.Attempt(procedure begin W.LastName := W.Provider.GetRequiredService<IAlpha>.Name; end);
      end)
    .&Then('it fails saying the container is shut down',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToContain('EContainerShutDownError');
      end)
    .&And('the message names the service that was asked for',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToContain('IAlpha');
      end)

  .Scenario('Resolving a scoped service after shutdown')
    .Given('a container with a scoped service already resolved once',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
        W.BetaCreatedBefore := TBeta.Created;
      end)
    .When('I close it and ask for that scoped service again',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
        W.Attempt(procedure begin W.LastName := W.Provider.GetRequiredService<IBeta>.Name; end);
        W.BetaCreatedAfter := TBeta.Created;
      end)
    .&Then('it fails with the same error as the singleton case',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToContain('EContainerShutDownError');
      end)
    .&And('no new instance was built',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.BetaCreatedAfter).ToEqual(W.BetaCreatedBefore);
      end)

  .Scenario('Before shutdown nothing changes')
    .Given('a container that has not been closed',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
      end)
    .When('I resolve the registered services',
      procedure(W: TShutdownWorld)
      begin
        W.Attempt(procedure begin W.LastName := W.Provider.GetRequiredService<IAlpha>.Name; end);
      end)
    .&Then('they are served as before, with no error',
      procedure(W: TShutdownWorld)
      begin
        // vigila que la guarda no se lleve por delante la resolucion normal.
        Expect(W.Outcome).ToEqual(SinError);
      end)
    .&And('the singleton answers',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.LastName).ToEqual('alpha');
      end)

.Rule('Closing an already closed container does nothing')

  .Scenario('Closing twice in a row')
    .Given('a container with services resolved and held by the caller',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
      end)
    .When('I close it and close it again',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
        W.AliveAfterFirstClose := TBeta.Alive;
        W.Attempt(procedure begin W.Provider.ShutDown; end);
        W.AliveAfterSecondClose := TBeta.Alive;
      end)
    .&Then('the second call ends without error',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToEqual(SinError);
      end)
    .&And('it does not destroy again what the first one left alone',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.AliveAfterFirstClose).ToEqual(1);
        Expect(W.AliveAfterSecondClose).ToEqual(1);
      end)

  .Scenario('Closing through a route other than the first one')
    .Given('a container closed through its root provider, with a child scope open',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
        W.OpenChildScope;
        W.Close;
      end)
    .When('I close again through the child scope provider',
      procedure(W: TShutdownWorld)
      begin
        W.Attempt(procedure begin W.Child.ServiceProvider.ShutDown; end);
      end)
    .&Then('whoever arrives second finds nothing left to do',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToEqual(SinError);
      end)

.Rule('Closing the container closes the scopes it created')

  .Scenario('A child scope is closed along with its container')
    .Given('a container and a child scope with a scoped service resolved',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
        W.OpenChildScope;
      end)
    .When('I close the container and resolve from the child scope',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
        W.Attempt(procedure begin W.LastName := W.Child.ServiceProvider.GetRequiredService<IBeta>.Name; end);
      end)
    .&Then('it fails with the shut down error',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToContain('EContainerShutDownError');
      end)

  .Scenario('Creating a scope from a closed container')
    .Given('a container that is already closed',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
        W.Close;
      end)
    .When('I ask it for a new scope',
      procedure(W: TShutdownWorld)
      begin
        W.Attempt(procedure begin W.Child := W.Provider.CreateScope; end);
      end)
    .&Then('it fails with the shut down error instead of handing out a scope that cannot resolve',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Outcome).ToContain('EContainerShutDownError');
      end)

.Rule('A closed container owns nothing')

  // Lo que este escenario mide no era observable antes: solo se podia contar instancias de los
  // tipos que heredaban de una clase base del framework, es decir de un doble de test. El
  // contenedor cuenta lo que posee, sea de quien sea. Las dos mitades se afirman a proposito: un
  // contador que devolviese siempre cero pasaria la segunda y fallaria la primera.
  .Scenario('A container that has resolved services owns them until it is closed')
    .Given('a container with a singleton and a scoped service already resolved',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndResolve;
      end)
    .When('I read what it owns before and after closing it',
      procedure(W: TShutdownWorld)
      begin
        var Access: IContainerAccess;
        if not Supports(W.Provider, IContainerAccess, Access) then
          raise Exception.Create('the provider does not expose IContainerAccess');
        W.OwnedBefore := Access.OwnedInstanceCount;
        W.Close;
        W.OwnedAfter := Access.OwnedInstanceCount;
      end)
    .&Then('it owned something before',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.OwnedBefore > 0).ToBeTrue;
      end)
    .&And('and it owns nothing after',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.OwnedAfter).ToEqual(0);
      end)

  // La cuenta sola no dice nada del desmontaje: el cierre termina vaciando el registro, asi que
  // un objeto que el cierre no consiguiera destruir desapareceria de la cuenta sin dejar de
  // existir. Las dos mitades juntas, o ninguna.
  .Scenario('The objects it counted have really been destroyed')
    .Given('a container with resolved services that record their destruction',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndForget;
      end)
    .When('it is closed',
      procedure(W: TShutdownWorld)
      begin
        W.OwnedBefore := W.Owned;
        W.Close;
        W.OwnedAfter := W.Owned;
      end)
    .&Then('it owns none',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.OwnedBefore).ToBeGreaterThan(0);
        Expect(W.OwnedAfter).ToEqual(0);
      end)
    .&And('and one destruction happened for each object it owned',
      procedure(W: TShutdownWorld)
      begin
        Expect(TAlpha.Alive).ToEqual(0);
        Expect(TBeta.Alive).ToEqual(0);
      end)

.Rule('Closing the container breaks the references it injected')

  .Scenario('A cycle of two singletons is destroyed on close')
    .Given('a container with two Singleton services that injected each other by property',
      procedure(W: TShutdownWorld)
      begin
        W.Services.AddSingleton<IAlphaSvc, TAlphaSvc>;
        W.Services.AddSingleton<IBetaSvc, TBetaSvc>;
        W.Provider := W.Services.BuildServiceProvider;
        W.Provider.GetRequiredService<IAlphaSvc>;
      end)
    .When('the container is closed',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
      end)
    .&Then('both of them are destroyed',
      procedure(W: TShutdownWorld)
      begin
        Expect(TAlphaSvc.Destroyed).ToEqual(1);
        Expect(TBetaSvc.Destroyed).ToEqual(1);
      end)
    .&And('and the container reports it owns none',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Owned).ToEqual(0);
      end)

  .Scenario('A longer cycle is taken apart too')
    .Given('three Scoped services chained in a cycle by injected properties',
      procedure(W: TShutdownWorld)
      begin
        W.Services.AddScoped<IRed, TRed>;
        W.Services.AddScoped<IGreen, TGreen>;
        W.Services.AddScoped<IBlue, TBlue>;
        W.Provider := W.Services.BuildServiceProvider;
        W.Child := W.Provider.CreateScope;
        W.Child.ServiceProvider.GetRequiredService<IRed>;
      end)
    .When('the scope that owns them is closed',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
        W.Child := nil;
      end)
    .&Then('the three of them are destroyed',
      procedure(W: TShutdownWorld)
      begin
        Expect(TRed.Alive).ToEqual(0);
        Expect(TGreen.Alive).ToEqual(0);
        Expect(TBlue.Alive).ToEqual(0);
      end)

  // El control del escenario de arriba: si esta cadena tampoco se desmontase, lo que falla no es
  // el ciclo sino el desmontaje entero, y no habria nada que arreglar todavia.
  .Scenario('A chain without a cycle is still taken apart as before')
    .Given('a container with services depending on each other without forming a cycle',
      procedure(W: TShutdownWorld)
      begin
        W.BuildAndForget;
      end)
    .When('it is closed',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
      end)
    .&Then('all of them are destroyed, just as before this pass existed',
      procedure(W: TShutdownWorld)
      begin
        Expect(TAlpha.Alive).ToEqual(0);
        Expect(TBeta.Alive).ToEqual(0);
      end)

  .Scenario('A wired foreign object is left alone')
    .Given('an object built outside the container and wired against it, with its property filled',
      procedure(W: TShutdownWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Provider := W.Services.BuildServiceProvider;
        W.Guest := TGuest.Create;
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
      end)
    .When('the container is closed',
      procedure(W: TShutdownWorld)
      begin
        W.Close;
      end)
    .&Then('the object is still alive and its property still points where it pointed',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Guest.Storage <> nil).ToBeTrue;
        Expect(W.Guest.Storage.Name).ToEqual('storage');
        Expect(TCounted.Destroyed).ToEqual(0);
      end)

;

end.
