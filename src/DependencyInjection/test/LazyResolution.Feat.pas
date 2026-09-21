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
  System.Classes,
  System.SyncObjs,
  Daf.Extensions.DependencyInjection;

type
  IArchive = interface(IInvokable)
    ['{2C9F4A31-7D58-4E6B-9A02-3F81C5D7E4B6}']
    function Name: string;
    /// Distingue una instancia de otra: el recuento dice cuantas se construyeron, esto dice
    /// si las dos lecturas trajeron la MISMA.
    function Id: Integer;
  end;

  TArchive = class(TInterfacedObject, IArchive)
  strict private
    FId: Integer;
  public
    class var Built: Integer;
    constructor Create;
    function Name: string;
    function Id: Integer;
  end;

  IClient = interface(IInvokable)
    ['{B71E3D48-6C05-4F92-A83B-2D4E7C1F9560}']
    function Ask: string;
    function AskTwice: string;
    /// Los identificadores de dos lecturas seguidas, para poder afirmar sobre la identidad.
    function TwoIds: string;
  end;

  /// Pide el servicio PEREZOSO. Nada en el registro de IArchive dice nada de esto.
  TLazyClient = class(TInterfacedObject, IClient)
  strict private
    FRepo: ILazy<IArchive>;
  public
    constructor Create(const ARepo: ILazy<IArchive>);
    function AskTwice: string;
    function Ask: string;
    function TwoIds: string;
  end;

  IEager = interface(IInvokable)
    ['{3F8A6B25-1E47-4D03-95C6-7A2B8F41D0E9}']
    function Ask: string;
  end;

  /// Pide el MISMO servicio construido, contra el MISMO registro.
  TEagerClient = class(TInterfacedObject, IEager)
  strict private
    FRepo: IArchive;
  public
    constructor Create(const ARepo: IArchive);
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
    Other: string;
    Manual: IArchive;
    BuiltBefore: Integer;
    TimedOut: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    /// Suelta N hilos a la vez sobre el mismo cuerpo. Con compuerta y con timeout: un cerrojo mal
    /// puesto se cuelga en vez de fallar, y un runner colgado no dice qué aserción se rompió.
    procedure Race(const Count: Integer; const Body: TProc);
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection;

constructor TArchive.Create;
begin
  inherited;
  Inc(Built);
  FId := Built;
end;

function TArchive.Id: Integer;
begin
  Result := FId;
end;

function TArchive.Name: string;
begin
  Result := 'archive';
end;

constructor TLazyClient.Create(const ARepo: ILazy<IArchive>);
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

function TLazyClient.TwoIds: string;
begin
  Result := Format('%d,%d', [FRepo.Value.Id, FRepo.Value.Id]);
end;

constructor TEagerClient.Create(const ARepo: IArchive);
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
  TArchive.Built := 0;
  Told := '';
  Services := TServiceCollection.Create;
end;

destructor TLazyWorld.Destroy;
begin
  Manual := nil;
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TLazyWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TLazyWorld.Race(const Count: Integer; const Body: TProc);
begin
  var Gate := TCountdownEvent.Create(1);
  var Done := TCountdownEvent.Create(Count);
  try
    for var I := 0 to Count - 1 do
      TThread.CreateAnonymousThread(
        procedure
        begin
          try
            Gate.WaitFor;
            Body;
          finally
            Done.Signal;
          end;
        end).Start;
    Gate.Signal;
    TimedOut := Done.WaitFor(30000) <> wrSignaled;
  finally
    Gate.Free;
    Done.Free;
  end;
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
    .Given('an archive registered as any other service, and a client that asks for it lazily',
      procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IArchive, TArchive>;
        W.Services.AddSingleton<IClient, TLazyClient>;
      end)
    .When('the client is resolved', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Provider.GetRequiredService<IClient>;
      end)
    .&Then('the archive has not been built', procedure(W: TLazyWorld)
      begin
        // El contador es la unica diferencia observable entre esto y un proxy que resuelve
        // al construirse: las dos formas entregan un cliente que funciona.
        Expect(TArchive.Built).ToEqual(0);
      end)

  .Scenario('The same registration still serves a consumer that asks for the service itself')
    .Given('one registration of the archive, and two clients that ask differently',
      procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IArchive, TArchive>;
        W.Services.AddSingleton<IClient, TLazyClient>;
        W.Services.AddSingleton<IEager, TEagerClient>;
      end)
    .When('the eager one is resolved', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IEager>.Ask;
      end)
    .&Then('it got the archive built, from the very same registration', procedure(W: TLazyWorld)
      begin
        Expect(W.Told).ToEqual('archive');
        Expect(TArchive.Built).ToEqual(1);
      end)

.Rule('Value asks the container, so the lifetime is the registration''s')

  .Scenario('A singleton asked lazily is the same instance every time')
    .Given('the archive registered as a singleton', procedure(W: TLazyWorld)
      begin
        W.Services.AddSingleton<IArchive, TArchive>;
        W.Services.AddSingleton<IClient, TLazyClient>;
      end)
    .When('the client reads it twice', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IClient>.TwoIds;
      end)
    .&Then('both reads bring the same instance, built once', procedure(W: TLazyWorld)
      begin
        Expect(W.Told).ToEqual('1,1');
        Expect(TArchive.Built).ToEqual(1);
      end)

  .Scenario('A scoped asked lazily is the one of its scope')
    .Given('the very same archive registered as scoped, and a scoped client too',
      procedure(W: TLazyWorld)
      begin
        W.Services.AddScoped<IArchive, TArchive>;
        W.Services.AddScoped<IClient, TLazyClient>;
      end)
    .When('two scopes each read it twice', procedure(W: TLazyWorld)
      begin
        W.Build;
        var First: IServiceScope := W.Provider.CreateScope;
        W.Told := First.ServiceProvider.GetRequiredService<IClient>.TwoIds;
        var Second: IServiceScope := W.Provider.CreateScope;
        W.Other := Second.ServiceProvider.GetRequiredService<IClient>.TwoIds;
      end)
    .&Then('each scope keeps its own, and inside one the two reads agree',
      procedure(W: TLazyWorld)
      begin
        // Las dos mitades: dentro de un ambito Value no construye dos veces, y entre ambitos no
        // comparte. Un proxy que cachease pasaria la primera y fallaria la segunda; uno que no
        // respetase el ambito pasaria la segunda y fallaria la primera.
        Expect(W.Told).ToEqual('1,1');
        Expect(W.Other).ToEqual('2,2');
      end)

  .Scenario('A transient asked lazily is a new instance every time')
    .Given('the very same archive registered as transient instead', procedure(W: TLazyWorld)
      begin
        W.Services.AddTransient<IArchive, TArchive>;
        W.Services.AddSingleton<IClient, TLazyClient>;
      end)
    .When('the client reads it twice', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IClient>.TwoIds;
      end)
    .&Then('each read brings its own, exactly as asking the container twice would',
      procedure(W: TLazyWorld)
      begin
        // Las dos afirmaciones juntas son la regla: Value pregunta, no guarda. Un proxy que
        // cachease daria '1,1' aqui y pasaria el escenario de arriba igual de bien.
        Expect(W.Told).ToEqual('1,2');
        Expect(TArchive.Built).ToEqual(2);
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

.Rule('The manual form, Lazy.From<T>')

  .Scenario('It defers until the first call, and then keeps what it built')
    .Given('a lazy built from a factory of its own', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Manual := Lazy.From<IArchive>(W.Provider,
          function(P: IServiceProvider): IArchive
          begin
            Result := TArchive.Create;
          end);
        W.BuiltBefore := TArchive.Built;
      end)
    .When('it is read twice', procedure(W: TLazyWorld)
      begin
        W.Told := W.Manual.Name + ',' + W.Manual.Name;
      end)
    .&Then('nothing was built until the first read, and the second reused it',
      procedure(W: TLazyWorld)
      begin
        // Aqui SI guarda, al reves que el ILazy<> del contenedor: la factoria la escribe quien
        // llama y puede costar lo que sea, asi que repetirla en cada metodo seria otra cosa.
        Expect(W.BuiltBefore).ToEqual(0);
        Expect(W.Told).ToEqual('archive,archive');
        Expect(TArchive.Built).ToEqual(1);
      end)

  .Scenario('Several threads reading at once build it once')
    .Given('a lazy whose factory takes its time', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Manual := Lazy.From<IArchive>(W.Provider,
          function(P: IServiceProvider): IArchive
          begin
            // El retardo ensancha la ventana a proposito: sin el, dos hilos casi nunca se cruzan
            // dentro del comprobar-y-construir y el escenario pasaria con el cerrojo quitado.
            TThread.Sleep(50);
            Result := TArchive.Create;
          end);
      end)
    .When('eight threads read it at the same time', procedure(W: TLazyWorld)
      begin
        W.Race(8,
          procedure
          begin
            W.Manual.Name;
          end);
      end)
    .&Then('the factory ran once, and nobody was left waiting', procedure(W: TLazyWorld)
      begin
        Expect(W.TimedOut).ToBeFalse;
        Expect(TArchive.Built).ToEqual(1);
      end)

  .Scenario('A factory that returns nothing fails, naming the contract')
    .Given('a lazy whose factory gives back nil', procedure(W: TLazyWorld)
      begin
        W.Build;
        W.Manual := Lazy.From<IArchive>(W.Provider,
          function(P: IServiceProvider): IArchive
          begin
            Result := nil;
          end);
      end)
    .When('it is read', procedure(W: TLazyWorld)
      begin
        W.Manual.Name;
      end)
    .&Then('it says so instead of answering the zero of the return type', procedure(W: TLazyWorld)
      begin
        // Devolver '' se lee como un resultado, y quien lo recibe esta lejos de la factoria que lo
        // causo: la escribio al registrar, no en esta llamada.
        ExpectException(Raised).ToHaveMessage('IArchive');
      end)

.Rule('Deferring the construction does not defer the question')

  .Scenario('A lazy dependency on a service nobody registered is refused')
    .Given('a client that asks lazily for an archive that is not registered', procedure(W: TLazyWorld)
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
        ExpectException(Raised).ToHaveMessage('IArchive');
      end);

end.
