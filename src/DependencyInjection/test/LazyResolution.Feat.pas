unit LazyResolution.Feat;

{
  Una dependencia que el CONSUMIDOR pide perezosa, sin que el registro del servicio cambie.

  Dos modos de fallo de aqui se leen como acierto, y por eso ningun escenario se conforma con
  "hay objeto":

    - un proxy que resuelve al construirse en vez de en el primer uso: todo funciona, y lo unico
      que delata la diferencia es CUANTAS veces se construyo el servicio y CUANDO
    - un puntero devuelto sin QueryInterface sobre el tipo declarado: la referencia parece
      asignada y revienta en la primera llamada, no al recibirla

  De ahi que se afirme sobre el contador y sobre el resultado de llamar, nunca sobre Assigned.
}

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  IRepo = interface(IInvokable)
    ['{2C9F4A31-7D58-4E6B-9A02-3F81C5D7E4B6}']
    function Name: string;
  end;

  TRepo = class(TInterfacedObject, IRepo)
  public
    class var Built: Integer;
    constructor Create;
    function Name: string;
  end;

  IClient = interface(IInvokable)
    ['{B71E3D48-6C05-4F92-A83B-2D4E7C1F9560}']
    function Ask: string;
    function AskTwice: string;
  end;

  /// Pide el servicio PEREZOSO. Nada en el registro de IRepo dice nada de esto.
  TLazyClient = class(TInterfacedObject, IClient)
  strict private
    FRepo: ILazy<IRepo>;
  public
    constructor Create(const ARepo: ILazy<IRepo>);
    function AskTwice: string;
    function Ask: string;
  end;

  IEager = interface(IInvokable)
    ['{3F8A6B25-1E47-4D03-95C6-7A2B8F41D0E9}']
    function Ask: string;
  end;

  /// Pide el MISMO servicio construido, contra el MISMO registro.
  TEagerClient = class(TInterfacedObject, IEager)
  strict private
    FRepo: IRepo;
  public
    constructor Create(const ARepo: IRepo);
    function Ask: string;
  end;

  // --- el ciclo: cada uno necesita al otro, y solo uno lo pide perezoso ---
  IAlpha = interface(IInvokable)
    ['{5E1B8C74-0A39-4D25-B6F7-8C93A1E5D027}']
    function Chain: string;
  end;

  IBeta = interface(IInvokable)
    ['{9A4D2F06-3B81-4C57-8E0A-1D6B7F92C3E8}']
    function Who: string;
  end;

  TAlpha = class(TInterfacedObject, IAlpha)
  strict private
    FBeta: ILazy<IBeta>;
  public
    constructor Create(const ABeta: ILazy<IBeta>);
    function Chain: string;
  end;

  TBeta = class(TInterfacedObject, IBeta)
  strict private
    FAlpha: IAlpha;
  public
    constructor Create(const AAlpha: IAlpha);
    function Who: string;
  end;

  TLazyWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Told: string;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection;

constructor TRepo.Create;
begin
  inherited;
  Inc(Built);
end;

function TRepo.Name: string;
begin
  Result := 'repo';
end;

constructor TLazyClient.Create(const ARepo: ILazy<IRepo>);
begin
  inherited Create;
  FRepo := ARepo;
end;

function TLazyClient.Ask: string;
begin
  Result := FRepo.Value.Name;
end;

function TLazyClient.AskTwice: string;
begin
  Result := FRepo.Value.Name + ',' + FRepo.Value.Name;
end;

constructor TEagerClient.Create(const ARepo: IRepo);
begin
  inherited Create;
  FRepo := ARepo;
end;

function TEagerClient.Ask: string;
begin
  Result := FRepo.Name;
end;

constructor TAlpha.Create(const ABeta: ILazy<IBeta>);
begin
  inherited Create;
  FBeta := ABeta;
end;

function TAlpha.Chain: string;
begin
  Result := 'alpha->' + FBeta.Value.Who;
end;

constructor TBeta.Create(const AAlpha: IAlpha);
begin
  inherited Create;
  FAlpha := AAlpha;
end;

function TBeta.Who: string;
begin
  Result := 'beta';
end;

{ TLazyWorld }

constructor TLazyWorld.Create;
begin
  inherited;
  TRepo.Built := 0;
  Told := '';
  Services := TServiceCollection.Create;
end;

destructor TLazyWorld.Destroy;
begin
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TLazyWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

initialization

Feature('''
Feature A dependency the consumer asks for lazily @di @lazy

  As an application author
  I want a consumer to defer one of its dependencies by changing its own constructor
  So that an expensive service, or one that would close a cycle, is not built to build its holder
''')

.UseWorld<TLazyWorld>

.Rule('The consumer decides, and the registration does not change')

  .Scenario('Building the consumer does not build what it asks for lazily')
    .Given('a repo registered as any other service, and a client that asks for it lazily',
      procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IRepo, TRepo>;
        W.Services.AddSingleton<IClient, TLazyClient>;
      end)
    .When('the client is resolved', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Provider.GetRequiredService<IClient>;
      end)
    .&Then('the repo has not been built', procedure(W: TLazyWorld)
      begin
        // El contador es la unica diferencia observable entre esto y un proxy que resuelve
        // al construirse: las dos formas entregan un cliente que funciona.
        Expect(TRepo.Built).ToEqual(0);
      end)

  .Scenario('The first use builds it, and the second does not build it again')
    .Given('a repo and a client that asks for it lazily', procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IRepo, TRepo>;
        W.Services.AddSingleton<IClient, TLazyClient>;
      end)
    .When('the client reads it twice', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IClient>.AskTwice;
      end)
    .&Then('both reads answer, and the repo was built once', procedure(W: TLazyWorld)
      begin
        Expect(W.Told).ToEqual('repo,repo');
        Expect(TRepo.Built).ToEqual(1);
      end)

  .Scenario('The same registration still serves a consumer that asks for the service itself')
    .Given('one registration of the repo, and two clients that ask differently',
      procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IRepo, TRepo>;
        W.Services.AddSingleton<IClient, TLazyClient>;
        W.Services.AddSingleton<IEager, TEagerClient>;
      end)
    .When('the eager one is resolved', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IEager>.Ask;
      end)
    .&Then('it got the repo built, from the very same registration', procedure(W: TLazyWorld)
      begin
        Expect(W.Told).ToEqual('repo');
        Expect(TRepo.Built).ToEqual(1);
      end)

.Rule('A cycle resolves when one of the two sides asks lazily')

  .Scenario('Two services that need each other')
    .Given('alpha needs beta lazily, and beta needs alpha built', procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IAlpha, TAlpha>;
        W.Services.AddSingleton<IBeta, TBeta>;
      end)
    .When('alpha is resolved and asked to walk the chain', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IAlpha>.Chain;
      end)
    .&Then('the chain closes', procedure(W: TLazyWorld)
      begin
        // Que llegue a 'beta' prueba las dos mitades: el ciclo se rompio al construir, y lo que
        // Value devolvio es un IBeta de verdad y no un puntero que solo lo parece.
        Expect(W.Told).ToEqual('alpha->beta');
      end)

.Rule('Deferring the construction does not defer the question')

  .Scenario('A lazy dependency on a service nobody registered is refused')
    .Given('a client that asks lazily for a repo that is not registered', procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IClient, TLazyClient>;
      end)
    .When('the client is resolved', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Provider.GetRequiredService<IClient>;
      end)
    .&Then('it fails when the client is built, naming what is missing', procedure(W: TLazyWorld)
      begin
        // Si se eligiera el constructor y se fallara en el primer Value, el mensaje llegaria
        // lejos de la causa y desde una llamada que el autor no escribio.
        ExpectException(Raised).ToHaveMessage('IRepo');
      end);

end.
