unit ScopeLifetime.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  /// Servicio Scoped de juguete. Lo unico que se le pregunta es cuantos hay vivos, que es lo
  /// que distingue "el ambito solto lo que resolvio" de "el ambito lo sigue poseyendo".
  /// </summary>
  IPerScope = interface(IInvokable)
    ['{5C1B7A20-3E64-4A9D-8F41-9D2C6E0B7A31}']
    function Name: string;
  end;

  /// <summary>
  /// Singleton, para que el ambito raiz tampoco este vacio: un recuento que solo mirase los
  /// Scoped no veria lo que sobreviva en la raiz.
  /// </summary>
  IShared = interface(IInvokable)
    ['{5C1B7A20-3E64-4A9D-8F41-9D2C6E0B7A32}']
    function Name: string;
  end;

  /// <summary>
  /// Las dos cuentas viven en la base y las comparten los dos servicios: lo que se afirma es
  /// "no queda vivo NINGUNO de los que se construyeron", sin distinguir de que tipo era.
  /// Publicas a proposito: son el instrumento de medida del spec, no estado del servicio.
  /// </summary>
  TCountedBase = class(TInterfacedObject)
  public
    class var Created: Integer;
    class var Alive: Integer;
    class procedure ResetCounters;
    constructor Create;
    destructor Destroy; override;
  end;

  TPerScope = class(TCountedBase, IPerScope)
  public
    function Name: string;
  end;

  TShared = class(TCountedBase, IShared)
  public
    function Name: string;
  end;

  ITransientThing = interface(IInvokable)
    ['{389A4218-0901-471C-89C8-0A5EBA9D597E}']
    function Name: string;
  end;

  TTransientThing = class(TInterfacedObject, ITransientThing)
  public
    function Name: string;
  end;

  /// <summary>
  /// Guarda el proveedor que le dio el contenedor y resuelve por el despues: la forma de un
  /// servicio que pide cosas cuando las necesita, y no al construirse.
  /// </summary>
  IProviderHolder = interface(IInvokable)
    ['{71922D2B-94D6-4A62-8AE3-3F0359F0C6EA}']
    function ResolveTransient: string;
  end;

  TProviderHolder = class(TCountedBase, IProviderHolder)
  private
    FProvider: IServiceProvider;
  public
    constructor Create(const Provider: IServiceProvider);
    function ResolveTransient: string;
  end;

  /// <summary>Un Scoped que guarda un ILazy de otro servicio, sin pedir nunca su valor.</summary>
  TLazyHolder = class(TCountedBase, IPerScope)
  private
    FShared: ILazy<IShared>;
  public
    constructor Create(const Shared: ILazy<IShared>);
    function Name: string;
  end;

  /// <summary>
  /// ⚠️ Las cuentas de vivos se leen desde el paso THEN, nunca se guardan durante el WHEN.
  /// El compilador finaliza el temporal del resultado de una funcion que devuelve interfaz al
  /// salir de la RUTINA, no al acabar la sentencia: dentro del paso que abrio los ambitos
  /// siguen vivos el ultimo ambito y el ultimo servicio devueltos, hagas lo que hagas con las
  /// variables. Un "vivos = 1" medido ahi es del instrumento, no del contenedor.
  /// </summary>
  TScopeLifetimeWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    // El ambito que el llamante sostiene, en los escenarios que lo piden. Sostenerlo desde el
    // World y no desde el paso es lo que lo hace sobrevivir de un paso al siguiente.
    Held: IServiceScope;
    // Un servicio que el llamante sigue sosteniendo despues de soltar su ambito.
    Holder: IProviderHolder;
    Resolved: string;
    Outcome: string;
    OwnedBefore, OwnedAfter: Integer;
    Closed: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    procedure ResolveInRoot;
    procedure WorkUnits(const Count: Integer);
    procedure WorkUnitsOf<T: IInterface>(const Count: Integer);
    procedure KeepServiceReleaseScope;
    procedure HoldScopeWithService;
    procedure Close;
    procedure ReleaseEverything;
    procedure Attempt(const Action: TProc);
    function LiveNow: Integer;
    function BuiltNow: Integer;
    function Owned: Integer;
  end;

const
  // Suficientes para que "no acumula" y "acumula" no se puedan confundir: con el defecto
  // presente la cuenta final es este numero, y sin el es cero.
  UnitsOfWork = 100;
  // Lo que Attempt escribe cuando la accion NO lanzo. Un centinela con texto, y no la cadena
  // vacia, porque la cadena vacia es tambien lo que se lee cuando nadie escribio nada.
  SinError = '<sin error>';

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection;

{ TCountedBase }

class procedure TCountedBase.ResetCounters;
begin
  Created := 0;
  Alive := 0;
end;

constructor TCountedBase.Create;
begin
  inherited Create;
  Inc(Created);
  Inc(Alive);
end;

destructor TCountedBase.Destroy;
begin
  Dec(Alive);
  inherited;
end;

{ TPerScope }

function TPerScope.Name: string;
begin
  Result := 'per-scope';
end;

{ TShared }

function TShared.Name: string;
begin
  Result := 'shared';
end;

{ TTransientThing }

function TTransientThing.Name: string;
begin
  Result := 'transient';
end;

{ TProviderHolder }

constructor TProviderHolder.Create(const Provider: IServiceProvider);
begin
  inherited Create;
  FProvider := Provider;
end;

function TProviderHolder.ResolveTransient: string;
begin
  Result := FProvider.GetRequiredService<ITransientThing>.Name;
end;

{ TLazyHolder }

constructor TLazyHolder.Create(const Shared: ILazy<IShared>);
begin
  inherited Create;
  FShared := Shared;
end;

function TLazyHolder.Name: string;
begin
  Result := 'lazy-holder';
end;

{ TScopeLifetimeWorld }

constructor TScopeLifetimeWorld.Create;
begin
  inherited;
  TCountedBase.ResetCounters;
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TScopeLifetimeWorld.Destroy;
begin
  Holder := nil;
  Held := nil;
  if (not Closed) and (Provider <> nil) then
    Provider.ShutDown;
  inherited;
end;

procedure TScopeLifetimeWorld.Build;
begin
  Services.AddScoped<IPerScope, TPerScope>;
  Services.AddSingleton<IShared, TShared>;
  Provider := Services.BuildServiceProvider;
end;

procedure TScopeLifetimeWorld.ResolveInRoot;
begin
  Provider.GetRequiredService<IShared>;
  Provider.GetRequiredService<IPerScope>;
end;

procedure TScopeLifetimeWorld.WorkUnits(const Count: Integer);
begin
  for var Idx := 1 to Count do
  begin
    var Scope: IServiceScope := Provider.CreateScope;
    Scope.ServiceProvider.GetRequiredService<IPerScope>;
    Scope := nil;
  end;
end;

procedure TScopeLifetimeWorld.WorkUnitsOf<T>(const Count: Integer);
begin
  for var Idx := 1 to Count do
  begin
    var Scope: IServiceScope := Provider.CreateScope;
    Scope.ServiceProvider.GetRequiredService<T>;
    Scope := nil;
  end;
end;

procedure TScopeLifetimeWorld.KeepServiceReleaseScope;
begin
  // Un ambito hijo resuelve el servicio que se queda el llamante y un Scoped que no se queda
  // nadie: si el ambito se cierra al soltarlo, ese Scoped muere, y la cuenta de vivos lo ve.
  var Scope: IServiceScope := Provider.CreateScope;
  Holder := Scope.ServiceProvider.GetRequiredService<IProviderHolder>;
  Scope.ServiceProvider.GetRequiredService<IPerScope>;
  Scope := nil;
end;

procedure TScopeLifetimeWorld.HoldScopeWithService;
begin
  Held := Provider.CreateScope;
  // Resuelto y soltado: lo unico que lo sostiene es el diccionario de su ambito, que es la
  // condicion del escenario. Sostenerlo tambien aqui lo mantendria vivo por otro motivo.
  Held.ServiceProvider.GetRequiredService<IPerScope>;
end;

procedure TScopeLifetimeWorld.Close;
begin
  Provider.ShutDown;
  Closed := True;
end;

procedure TScopeLifetimeWorld.ReleaseEverything;
begin
  Held := nil;
  Provider := nil;
end;

procedure TScopeLifetimeWorld.Attempt(const Action: TProc);
begin
  Outcome := SinError;
  try
    Action();
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function TScopeLifetimeWorld.LiveNow: Integer;
begin
  Result := TCountedBase.Alive;
end;

function TScopeLifetimeWorld.BuiltNow: Integer;
begin
  Result := TCountedBase.Created;
end;

function TScopeLifetimeWorld.Owned: Integer;
begin
  var Access: IContainerAccess;
  if not Supports(Provider, IContainerAccess, Access) then
    raise Exception.Create('the provider does not expose IContainerAccess');
  Result := Access.OwnedInstanceCount;
end;

initialization

Feature('''
Feature Releasing a scope releases what it resolved @di @lifetime @scope

  As an application author serving one unit of work per scope
  I want releasing the scope to destroy the services it created
  So a long lived process does not accumulate one of everything per request
''')

.UseWorld<TScopeLifetimeWorld>

.Rule('The life of what a scope resolved ends with the life of the scope')

  .Scenario('A released scope destroys what it resolved')
    .Given('a container with a scoped service registered',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Build;
      end)
    .When('I open a scope, resolve the service and release the scope',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.WorkUnits(1);
      end)
    .&Then('the service has been destroyed',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(0);
      end)
    .&And('one was built, so the count above is not zero for having done nothing',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.BuiltNow).ToEqual(1);
      end)


  .Scenario('Many units of work in a row accumulate nothing')
    .Given('a container with a scoped service registered',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Build;
      end)
    .When('I serve many units of work, each one opening and releasing its own scope',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.WorkUnits(UnitsOfWork);
      end)
    .&Then('none of the services that were built is still alive',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(0);
      end)
    .&And('one was built per round, so the count above is not zero for having done nothing',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.BuiltNow).ToEqual(UnitsOfWork);
      end)

  // ⚠️ Pasa ya contra el codigo de hoy, y es correcto: hoy no se cierra NINGUN ambito, asi que
  // este no puede ser una excepcion. Es control y no demostracion — lo que vigila es que el
  // arreglo no se pase de largo y destruya lo que el llamante sigue sosteniendo.
  .Scenario('A scope someone is still holding is not closed')
    .Given('a scope with a scoped service resolved, and the caller holding the scope',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Build;
        W.HoldScopeWithService;
      end)
    .When('I look at what is alive',
      procedure(W: TScopeLifetimeWorld)
      begin
        // Nada que hacer: el escenario es sobre lo que NO ocurre. El paso existe para que la
        // lectura del Then caiga fuera de la rutina que creo el ambito.
      end)
    .&Then('the service is still alive, because its scope is still open',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(1);
      end)

  // ⚠️ Nace en verde: guardia de regresion, no demostracion. Lo que anade sobre el escenario de
  // ContainerShutdown.Feat.pas que mide OwnedInstanceCount es la otra mitad — que el contador
  // llegue a cero no dice que los objetos se hayan destruido, solo que dejaron de contarse.
  .Scenario('After closing the container and releasing everything nothing is alive')
    .Given('a container that has resolved services in its root scope and in child scopes',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Build;
        W.ResolveInRoot;
        W.WorkUnits(3);
        W.HoldScopeWithService;
      end)
    .When('I close it and release everything the caller was holding',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Close;
        W.ReleaseEverything;
      end)
    .&Then('none of the objects it built is still alive',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(0);
      end)
    .&And('at least one was built, so the claim above is not true by vacuity',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.BuiltNow > 0).ToBeTrue;
      end)

.Rule('The container does not retain the scopes it hands out')

  .Scenario('What the container owns does not grow with the requests it serves')
    .Given('a container with a scoped service registered',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Build;
        W.OwnedBefore := W.Owned;
      end)
    .When('I serve many units of work, each one with its own scope, releasing it at the end',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.WorkUnits(UnitsOfWork);
      end)
    .&Then('it owns the same number of objects at the end as at the start',
      procedure(W: TScopeLifetimeWorld)
      begin
        // Leido aqui y no en el When por la misma razon que las cuentas de vivos: el ultimo
        // ambito de aquel paso sigue vivo mientras el paso no ha vuelto, y el contador lo ve.
        W.OwnedAfter := W.Owned;
        Expect(W.OwnedAfter).ToEqual(W.OwnedBefore);
      end)
    .&And('one service was built per round, so nothing above is true for having done nothing',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.BuiltNow).ToEqual(UnitsOfWork);
      end)

  // ⚠️ Verde hoy, y que siga verde ES el punto: la fabrica deja de sostener los ambitos que
  // reparte, asi que tiene que seguir alcanzandolos para desmontarlos. La misma garantia se mide
  // desde el lado del contenedor en ContainerShutdown.Feat.pas.
  .Scenario('Closing the container still closes the scopes that are alive')
    .Given('a container and an open scope its creator is still holding, with a scoped service resolved',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Build;
        W.HoldScopeWithService;
      end)
    .When('I close the container and resolve from that scope',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Close;
        W.Attempt(procedure begin W.Held.ServiceProvider.GetRequiredService<IPerScope>; end);
      end)
    .&Then('it fails with the shut down error',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.Outcome).ToContain('EContainerShutDownError');
      end)

.Rule('What the container builds does not hold its scope')

  .Scenario('A scoped service that keeps its provider does not retain its scope')
    .Given('a scoped service whose constructor receives and keeps an IServiceProvider',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Services.AddScoped<IProviderHolder, TProviderHolder>;
        W.Services.AddTransient<ITransientThing, TTransientThing>;
        W.Provider := W.Services.BuildServiceProvider;
      end)
    .When('I serve many units of work, each one opening and releasing its own scope',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.WorkUnitsOf<IProviderHolder>(UnitsOfWork);
      end)
    .&Then('none of the services that were built is still alive',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(0);
      end)
    .&And('one was built per round, so the count above is not zero for having done nothing',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.BuiltNow).ToEqual(UnitsOfWork);
      end)

  .Scenario('A scoped service that keeps an ILazy does not retain its scope')
    .Given('a scoped service whose constructor receives and keeps an ILazy of another service',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Services.AddScoped<IPerScope, TLazyHolder>;
        W.Services.AddSingleton<IShared, TShared>;
        W.Provider := W.Services.BuildServiceProvider;
      end)
    .When('I serve many units of work, each one opening and releasing its own scope',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.WorkUnitsOf<IPerScope>(UnitsOfWork);
      end)
    .&Then('none of the services that were built is still alive',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(0);
      end)
    .&And('one was built per round, so the count above is not zero for having done nothing',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.BuiltNow).ToEqual(UnitsOfWork);
      end)

  .Scenario('A service that outlives its scope does not bring it back')
    .Given('a scoped service that keeps its provider, held by the caller after its scope was released',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Services.AddScoped<IProviderHolder, TProviderHolder>;
        W.Services.AddScoped<IPerScope, TPerScope>;
        W.Services.AddTransient<ITransientThing, TTransientThing>;
        W.Provider := W.Services.BuildServiceProvider;
        W.KeepServiceReleaseScope;
      end)
    .When('the service resolves something through that provider',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Attempt(procedure begin W.Holder.ResolveTransient; end);
      end)
    .&Then('the resolution fails with the shut down error',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.Outcome).ToContain('EContainerShutDownError');
      end)
    .&And('the scope closed when it was released, though the service is still alive',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(1);
      end)

.Rule('A singleton receives the provider of the container')

  .Scenario('A singleton outlives the scope that asked for it first')
    .Given('a singleton that keeps its provider, resolved first from a child scope that was then released',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Services.AddSingleton<IProviderHolder, TProviderHolder>;
        W.Services.AddScoped<IPerScope, TPerScope>;
        W.Services.AddTransient<ITransientThing, TTransientThing>;
        W.Provider := W.Services.BuildServiceProvider;
        W.KeepServiceReleaseScope;
      end)
    .When('the singleton resolves a transient service through its provider',
      procedure(W: TScopeLifetimeWorld)
      begin
        W.Attempt(procedure begin W.Resolved := W.Holder.ResolveTransient; end);
      end)
    .&Then('the resolution works, because its provider is the container''s',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.Outcome).ToEqual(SinError);
        Expect(W.Resolved).ToEqual('transient');
      end)
    .&And('the child scope closed when it was released',
      procedure(W: TScopeLifetimeWorld)
      begin
        Expect(W.LiveNow).ToEqual(1);
      end)

;

end.
