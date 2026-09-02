unit Concurrency.Feat;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  ///   El servicio que se disputa. Su constructor lleva una espera configurable: eso ENSANCHA la
  ///   ventana de la carrera, no la crea. Sin ella el resultado depende del planificador y un
  ///   escenario que a veces pasa no mide nada.
  /// </summary>
  IRacer = interface(IInvokable)
    ['{4C0A9E11-0001-4B77-9E4E-2F6D1B0A7C01}']
    function Serial: Integer;
  end;

  // Un segundo registro, para el escenario que resuelve varias cosas a la vez y para el que
  // comprueba que dos hilos construyendo servicios DISTINTOS no se acusan de ciclo.
  IOther = interface(IInvokable)
    ['{4C0A9E11-0002-4B77-9E4E-2F6D1B0A7C02}']
    function Serial: Integer;
  end;

  TRacer = class(TInterfacedObject, IRacer)
  private
    FSerial: Integer;
  public
    // Publicas a proposito: son el instrumento de medida del spec, no estado del servicio.
    class var Created: Integer;
    class var Destroyed: Integer;
    class var Alive: Integer;
    class var CtorDelayMs: Integer;
    // Si esta puesta, el constructor no vuelve hasta que alguien la abra. Es lo que hace que
    // "construir y perder la carrera con el cierre" sea una CITA y no una apuesta entre dos
    // esperas de reloj: con relojes el escenario mide una cosa u otra segun como vaya la
    // maquina, y ya dio verde con el resultado que el requisito prohibe.
    class var CtorGate: TEvent;
    // La otra mitad de la cita: el constructor avisa de que ya esta dentro. Sin ella, el que
    // cierra puede llegar primero y entonces no se construye nada -- el escenario da verde
    // habiendo medido el caso de al lado, no el suyo.
    class var InCtorGate: TEvent;
    class procedure Reset;
    constructor Create;
    destructor Destroy; override;
    function Serial: Integer;
  end;

  // Un contrato sin fijar y dos cierres. Resolver un cierre POR PRIMERA VEZ escribe la cache de
  // emparejamientos de la coleccion, que es el unico estado suyo que se toca al resolver.
  IBag<T: class> = interface(IInvokable)
    ['{4C0A9E11-0003-4B77-9E4E-2F6D1B0A7C03}']
    function Tag: string;
  end;

  // Ocho marcadores, uno por hilo: lo que se disputa es la ESCRITURA de la cache de
  // emparejamientos, y con dos claves la ventana es demasiado estrecha para verla. Con ocho
  // claves entrando a la vez, el diccionario redimensiona mientras lo escriben.
  TKind1 = class end;
  TKind2 = class end;
  TKind3 = class end;
  TKind4 = class end;
  TKind5 = class end;
  TKind6 = class end;
  TKind7 = class end;
  TKind8 = class end;

  // Ojo: dentro de TBag<_> el T ES el marcador, asi que T.ClassName no nombra el cierre pedido
  // y no sirve para distinguirlos. Lo que este doble aporta es una resolucion que pasa por la
  // cache de emparejamientos; quien la pidio, lo sabe el llamante.
  TBag<T: class> = class(TInterfacedObject, IBag<T>)
  public
    function Tag: string;
  end;

  TOther = class(TInterfacedObject, IOther)
  private
    FSerial: Integer;
  public
    class var Created: Integer;
    class var CtorDelayMs: Integer;
    class procedure Reset;
    constructor Create;
    function Serial: Integer;
  end;

  /// <summary>
  ///   Captura lo que pasa en cada hilo en vez de dejarlo escapar, y lo devuelve en informes que
  ///   NOMBRAN los numeros. Un `ToBeEmpty` o un `ToBeTrue` no imprimen el valor, asi que el fallo
  ///   diria "no coincide" sin decir cuantos constructores corrieron -- que es el dato entero.
  /// </summary>
  TConcurrencyWorld = class
  private
    FSerials: TArray<Integer>;
    FFailures: TArray<string>;
    FScopes: TArray<IServiceScope>;
    FOtherSerial: Integer;
    FOutcomes: TArray<string>;
    FCtorGate: TEvent;
    FInCtorGate: TEvent;
    FSameFromCreator: Integer;
    FTimedOut: Boolean;
    procedure Race(const Count: Integer; const Body: TProc<Integer>);
    function ThreadBody(const Idx: Integer; const Body: TProc<Integer>;
      const Gate, Done: TCountdownEvent): TProc;
    function DistinctSerials: Integer;
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    SharedScope: IServiceScope;
    OwnedBefore, OwnedAfter: Integer;
    Closed: Boolean;
    constructor Create;
    destructor Destroy; override;

    procedure Register(const LifeTime: TServiceLifeTime);
    procedure NoCtorDelay;
    procedure HoldCtorUntilClosed;
    procedure RegisterOther(const LifeTime: TServiceLifeTime);
    procedure Build;
    procedure OpenScopePerThread;
    procedure OpenSharedScope;
    procedure RaceFromOwnScope;
    procedure RaceFromSharedScope;
    procedure RaceFromRootProvider;
    procedure RaceResolvingTwoServices;
    procedure RaceTwoDifferentServicesFromSharedScope;
    procedure RaceResolveWhileClosing;
    procedure RaceReleaseScopesWhileClosing;
    procedure RaceConstructionAgainstClose;
    procedure ResolveSharedScopeFromOtherThread;
    procedure CloseFromOtherThread;
    procedure RaceOnOpenGenericClosures;
    procedure ReadOwnership;
    procedure Close;

    // Informes. Cada uno imprime lo que midio, para que el fallo lo diga.
    function ConstructorsReport: string;
    function DistinctReport: string;
    function FailureReport: string;
    function OwnershipReport: string;
    function BothServedReport: string;
    function UnexpectedOutcomesReport: string;
    function ClosedSeenCount: Integer;
    function SameInstanceReport: string;
    function LoserOutcomeReport: string;
    function TeardownBalanceReport: string;
    function ClosureOutcomesReport: string;
    function TeardownReport: string;
    function LiveAfterRaceReport: string;
  end;

const
  ThreadCount = 8;
  // Lo que UnexpectedOutcomesReport escribe cuando ninguna resolucion acabo de una forma que el
  // requisito no admita.
  SinInesperados = '<nada inesperado>';
  // Lo que FailureReport escribe cuando nadie fallo. Un centinela con texto, no la cadena vacia:
  // la cadena vacia es tambien lo que se lee cuando nadie escribio nada.
  SinErrores = '<sin errores>';

implementation

uses
  System.Classes,
  System.Diagnostics,
  Daf.MiniSpec,
  Daf.Rtti,
  Daf.DependencyInjection;

{ TRacer }

class procedure TRacer.Reset;
begin
  Created := 0;
  Destroyed := 0;
  Alive := 0;
  CtorDelayMs := 20;
  CtorGate := nil;
  InCtorGate := nil;
end;

constructor TRacer.Create;
begin
  inherited Create;
  FSerial := TInterlocked.Increment(Created);
  TInterlocked.Increment(Alive);
  if CtorDelayMs > 0 then
    TThread.Sleep(CtorDelayMs);
  if Assigned(InCtorGate) then
    InCtorGate.SetEvent;
  if Assigned(CtorGate) then
    CtorGate.WaitFor(10000);
end;

destructor TRacer.Destroy;
begin
  TInterlocked.Increment(Destroyed);
  TInterlocked.Decrement(Alive);
  inherited;
end;

function TRacer.Serial: Integer;
begin
  Result := FSerial;
end;

{ TOther }

class procedure TOther.Reset;
begin
  Created := 0;
  CtorDelayMs := 20;
end;

constructor TOther.Create;
begin
  inherited Create;
  FSerial := TInterlocked.Increment(Created);
  if CtorDelayMs > 0 then
    TThread.Sleep(CtorDelayMs);
end;

function TOther.Serial: Integer;
begin
  Result := FSerial;
end;

{ TBag<T> }

function TBag<T>.Tag: string;
begin
  Result := 'bag';
end;

{ TConcurrencyWorld }

constructor TConcurrencyWorld.Create;
begin
  inherited;
  TRacer.Reset;
  TOther.Reset;
  FCtorGate := TEvent.Create(nil, True, False, '');
  FInCtorGate := TEvent.Create(nil, True, False, '');
  Services := TServiceCollection.Create;
end;

destructor TConcurrencyWorld.Destroy;
begin
  // Se descuelga antes de liberarla: el constructor de otro escenario no puede quedarse con un
  // puntero a una cita ya muerta.
  TRacer.CtorGate := nil;
  TRacer.InCtorGate := nil;
  FScopes := nil;
  SharedScope := nil;
  // Si un escenario dejo hilos colgados, cerrar aqui desmonta lo que siguen usando. Se cierra
  // igual: el informe ya recogio el planton y lo que hace falta es que el spec falle, no que el
  // runner se muera en el teardown sin decir que asercion fallo.
  if not (Closed or FTimedOut) then
    Provider.ShutDown;
  FCtorGate.Free;
  FInCtorGate.Free;
  inherited;
end;

procedure TConcurrencyWorld.Register(const LifeTime: TServiceLifeTime);
begin
  case LifeTime of
    Singleton: Services.AddSingleton(TypeInfo(IRacer), TRacer);
    Scoped: Services.AddScoped(TypeInfo(IRacer), TRacer);
    Transient: Services.AddTransient(TypeInfo(IRacer), TRacer);
  end;
end;

procedure TConcurrencyWorld.RegisterOther(const LifeTime: TServiceLifeTime);
begin
  case LifeTime of
    Singleton: Services.AddSingleton(TypeInfo(IOther), TOther);
    Scoped: Services.AddScoped(TypeInfo(IOther), TOther);
    Transient: Services.AddTransient(TypeInfo(IOther), TOther);
  end;
end;

procedure TConcurrencyWorld.NoCtorDelay;
begin
  TRacer.CtorDelayMs := 0;
  TOther.CtorDelayMs := 0;
end;

procedure TConcurrencyWorld.HoldCtorUntilClosed;
begin
  TRacer.CtorDelayMs := 0;
  TRacer.CtorGate := FCtorGate;
  TRacer.InCtorGate := FInCtorGate;
end;

procedure TConcurrencyWorld.Build;
begin
  Provider := Services.BuildServiceProvider;
end;

procedure TConcurrencyWorld.OpenScopePerThread;
begin
  // Los ambitos se abren ANTES de la carrera: lo unico que queda detras de la puerta es la
  // resolucion, que es lo que se quiere medir.
  SetLength(FScopes, ThreadCount);
  for var I := 0 to ThreadCount - 1 do
    FScopes[I] := Provider.CreateScope;
end;

procedure TConcurrencyWorld.OpenSharedScope;
begin
  SharedScope := Provider.CreateScope;
end;

procedure TConcurrencyWorld.Race(const Count: Integer; const Body: TProc<Integer>);
begin
  SetLength(FSerials, Count);
  SetLength(FFailures, Count);
  SetLength(FOutcomes, Count);
  var Gate := TCountdownEvent.Create(1);
  var Done := TCountdownEvent.Create(Count);
  try
    for var I := 0 to Count - 1 do
    begin
      FSerials[I] := -1;
      TThread.CreateAnonymousThread(ThreadBody(I, Body, Gate, Done)).Start;
    end;
    Gate.Signal;
    // Con timeout, y el planton entra en el informe: un cerrojo mal puesto se cuelga en vez de
    // fallar, y un runner colgado no dice que asercion se rompio.
    FTimedOut := Done.WaitFor(30000) <> wrSignaled;
  finally
    Gate.Free;
    Done.Free;
  end;
end;

function TConcurrencyWorld.ThreadBody(const Idx: Integer; const Body: TProc<Integer>;
  const Gate, Done: TCountdownEvent): TProc;
begin
  // El indice entra como PARAMETRO y no como variable del bucle. Delphi captura las variables
  // POR REFERENCIA y hay una sola por rutina, asi que ocho closures creados dentro de un bucle
  // comparten el indice y ven el ultimo valor: los ocho hilos escribian en la casilla 7 y el
  // recuento de instancias distintas daba 1 fuese cierto o no. Un parametro trae su marco.
  Result :=
    procedure
    begin
      try
        Gate.WaitFor;
        Body(Idx);
      except
        on E: Exception do FFailures[Idx] := E.ClassName + ': ' + E.Message;
      end;
      Done.Signal;
    end;
end;

procedure TConcurrencyWorld.RaceFromOwnScope;
begin
  Race(ThreadCount,
    procedure(Idx: Integer)
    begin
      FSerials[Idx] := FScopes[Idx].ServiceProvider.GetRequiredService<IRacer>.Serial;
    end);
end;

procedure TConcurrencyWorld.RaceFromSharedScope;
begin
  Race(ThreadCount,
    procedure(Idx: Integer)
    begin
      FSerials[Idx] := SharedScope.ServiceProvider.GetRequiredService<IRacer>.Serial;
    end);
end;

procedure TConcurrencyWorld.RaceFromRootProvider;
begin
  Race(ThreadCount,
    procedure(Idx: Integer)
    begin
      FSerials[Idx] := Provider.GetRequiredService<IRacer>.Serial;
    end);
end;

procedure TConcurrencyWorld.RaceResolvingTwoServices;
begin
  Race(ThreadCount,
    procedure(Idx: Integer)
    begin
      // Repetidamente, que es lo que pide el requisito: una sola resolucion por hilo no cruza
      // la ventana mas que una vez.
      for var Rep := 1 to 50 do
      begin
        FSerials[Idx] := FScopes[Idx].ServiceProvider.GetRequiredService<IRacer>.Serial;
        if FScopes[Idx].ServiceProvider.GetRequiredService<IOther>.Serial <= 0 then
          raise Exception.Create('IOther devolvio un serial invalido');
      end;
    end);
end;

procedure TConcurrencyWorld.RaceTwoDifferentServicesFromSharedScope;
begin
  // Dos hilos y DOS servicios distintos, del mismo ambito. No hay ciclo por ningun lado: lo
  // unico que comparten es el ambito.
  FOtherSerial := -1;
  Race(2,
    procedure(Idx: Integer)
    begin
      if Idx = 0 then
        FSerials[Idx] := SharedScope.ServiceProvider.GetRequiredService<IRacer>.Serial
      else
        FOtherSerial := SharedScope.ServiceProvider.GetRequiredService<IOther>.Serial;
    end);
end;

procedure TConcurrencyWorld.RaceResolveWhileClosing;
begin
  // Un hilo mas que los que resuelven, y es el que cierra. Los que resuelven van por tiempo y no
  // por vueltas: con un numero fijo de vueltas terminarian antes del cierre y el escenario
  // pasaria sin haber cruzado la ventana ni una vez.
  Race(ThreadCount + 1,
    procedure(Idx: Integer)
    begin
      if Idx = ThreadCount then
      begin
        TThread.Sleep(25);
        Close;
        FOutcomes[Idx] := 'closer';
        Exit;
      end;

      var Clock := TStopwatch.StartNew;
      while Clock.ElapsedMilliseconds < 500 do
      begin
        try
          if FScopes[Idx].ServiceProvider.GetRequiredService<IRacer>.Serial <= 0 then
          begin
            FOutcomes[Idx] := 'serial invalido';
            Break;
          end;
          FOutcomes[Idx] := 'ok';
        except
          on E: EContainerShutDownError do
          begin
            FOutcomes[Idx] := 'closed';
            Break;
          end;
          on E: Exception do
          begin
            FOutcomes[Idx] := E.ClassName + ': ' + E.Message;
            Break;
          end;
        end;
      end;
    end);
end;

procedure TConcurrencyWorld.RaceReleaseScopesWhileClosing;
begin
  // La carrera que antes no podia existir: hasta ahora ningun ambito moria antes del cierre,
  // porque la fabrica los sostenia todos. Ahora el contador de un ambito puede llegar a cero
  // mientras otro hilo recorre la lista para desmontarlo.
  // Un hilo mas que los que trabajan, y es el que cierra. Los demas van por tiempo y no por
  // vueltas: con un numero fijo terminarian antes del cierre y la ventana no se cruzaria.
  Race(ThreadCount + 1,
    procedure(Idx: Integer)
    begin
      if Idx = ThreadCount then
      begin
        TThread.Sleep(25);
        Close;
        FOutcomes[Idx] := 'closer';
        Exit;
      end;

      var Clock := TStopwatch.StartNew;
      while Clock.ElapsedMilliseconds < 500 do
      begin
        try
          var Scope: IServiceScope := Provider.CreateScope;
          Scope.ServiceProvider.GetRequiredService<IRacer>;
          Scope := nil;
          FOutcomes[Idx] := 'ok';
        except
          on E: EContainerShutDownError do
          begin
            FOutcomes[Idx] := 'closed';
            Break;
          end;
          on E: Exception do
          begin
            FOutcomes[Idx] := E.ClassName + ': ' + E.Message;
            Break;
          end;
        end;
      end;
    end);
end;

procedure TConcurrencyWorld.RaceConstructionAgainstClose;
begin
  // El constructor no vuelve hasta que el cierre haya terminado, asi que quien construye pierde
  // la carrera SIEMPRE, y el escenario mide lo mismo en cualquier maquina.
  Race(2,
    procedure(Idx: Integer)
    begin
      if Idx = 1 then
      begin
        try
          // Espera a que el otro este DENTRO del constructor, cierra, y solo entonces lo suelta.
          if FInCtorGate.WaitFor(10000) <> wrSignaled then
            FOutcomes[Idx] := 'el otro no llego al constructor'
          else
          begin
            Close;
            FOutcomes[Idx] := 'closer';
          end;
        finally
          FCtorGate.SetEvent;
        end;
        Exit;
      end;
      try
        FSerials[Idx] := Provider.GetRequiredService<IRacer>.Serial;
        FOutcomes[Idx] := 'ok';
      except
        on E: Exception do FOutcomes[Idx] := E.ClassName;
      end;
    end);
end;

procedure TConcurrencyWorld.ResolveSharedScopeFromOtherThread;
begin
  Race(1,
    procedure(Idx: Integer)
    begin
      FSerials[Idx] := SharedScope.ServiceProvider.GetRequiredService<IRacer>.Serial;
    end);
  // Y otra vez desde el hilo que lo creo, que es este.
  FSameFromCreator := SharedScope.ServiceProvider.GetRequiredService<IRacer>.Serial;
end;

procedure TConcurrencyWorld.CloseFromOtherThread;
begin
  Race(1,
    procedure(Idx: Integer)
    begin
      Close;
      FOutcomes[Idx] := 'closer';
    end);
end;

procedure TConcurrencyWorld.RaceOnOpenGenericClosures;
begin
  Race(ThreadCount,
    procedure(Idx: Integer)
    begin
      // Los pares piden un cierre y los impares el otro: dos claves distintas entrando a la vez
      // en el mismo diccionario es lo que revienta una tabla hash sin cerrojo, no una sola.
      for var Rep := 1 to 200 do
        case Idx of
          0: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind1>>.Tag;
          1: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind2>>.Tag;
          2: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind3>>.Tag;
          3: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind4>>.Tag;
          4: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind5>>.Tag;
          5: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind6>>.Tag;
          6: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind7>>.Tag;
          7: FOutcomes[Idx] := Provider.GetRequiredService<IBag<TKind8>>.Tag;
        end;
      if FOutcomes[Idx] <> 'bag' then
        raise Exception.Create('el cierre no devolvio un servicio utilizable: ' + FOutcomes[Idx]);
    end);
end;

procedure TConcurrencyWorld.ReadOwnership;
begin
  var Access: IContainerAccess;
  if not Supports(Provider, IContainerAccess, Access) then
    raise Exception.Create('el proveedor no expone IContainerAccess');
  OwnedAfter := Access.OwnedInstanceCount;
end;

procedure TConcurrencyWorld.Close;
begin
  Provider.ShutDown;
  Closed := True;
end;

function TConcurrencyWorld.DistinctSerials: Integer;
begin
  Result := 0;
  for var I := 0 to High(FSerials) do
  begin
    if FSerials[I] <= 0 then Continue;
    var Seen := False;
    for var J := 0 to I - 1 do
      if FSerials[J] = FSerials[I] then Seen := True;
    if not Seen then Inc(Result);
  end;
end;

function TConcurrencyWorld.ConstructorsReport: string;
begin
  Result := Format('constructores=%d', [TRacer.Created]);
end;

function TConcurrencyWorld.DistinctReport: string;
begin
  Result := Format('instancias distintas=%d', [DistinctSerials]);
end;

function TConcurrencyWorld.FailureReport: string;
begin
  var Lines: TArray<string> := [];
  if FTimedOut then
    Lines := Lines + ['TIMEOUT: algun hilo no volvio en 30 s'];
  for var I := 0 to High(FFailures) do
    if FFailures[I] <> '' then
      Lines := Lines + [Format('hilo %d: %s', [I, FFailures[I]])];
  if Length(Lines) = 0 then
    Exit(SinErrores);
  Result := string.Join(' | ', Lines);
end;

function TConcurrencyWorld.OwnershipReport: string;
begin
  // Diferencia y no total: el contenedor ya posee el IServiceScopeFactory que se registra a si
  // mismo, asi que el total no es el numero de instancias que la carrera construyo.
  Result := Format('constructores=%d, poseidos=%d', [TRacer.Created, OwnedAfter - OwnedBefore]);
end;

function TConcurrencyWorld.BothServedReport: string;
begin
  Result := Format('racer=%d, other=%d', [FSerials[0], FOtherSerial]);
end;

function TConcurrencyWorld.UnexpectedOutcomesReport: string;
begin
  var Lines: TArray<string> := [];
  if FTimedOut then
    Lines := Lines + ['TIMEOUT: algun hilo no volvio en 30 s'];
  for var I := 0 to High(FOutcomes) do
    // Los dos que el requisito admite, y el hilo que cierra. Cualquier otra cosa --incluido el
    // Access Violation, y incluido el hilo que no dejo nada escrito-- sale nombrada.
    if not ((FOutcomes[I] = 'ok') or (FOutcomes[I] = 'closed') or (FOutcomes[I] = 'closer')) then
      Lines := Lines + [Format('hilo %d: %s', [I, FOutcomes[I]])];
  if Length(Lines) = 0 then
    Exit(SinInesperados);
  Result := string.Join(' | ', Lines);
end;

function TConcurrencyWorld.ClosedSeenCount: Integer;
begin
  Result := 0;
  for var I := 0 to High(FOutcomes) do
    if FOutcomes[I] = 'closed' then Inc(Result);
end;

function TConcurrencyWorld.SameInstanceReport: string;
begin
  Result := Format('otro hilo=%d, su creador=%d', [FSerials[0], FSameFromCreator]);
end;

function TConcurrencyWorld.LoserOutcomeReport: string;
begin
  // Los cinco datos en una linea: el fallo tiene que decir QUE paso, y "esperaba X y llego Y"
  // sobre un solo campo no distingue "la guarda no salto" de "el cierre no llego a correr".
  Result := Format('constructor=%s, cierre=%s, construidos=%d, destruidos=%d, vivos=%d',
    [FOutcomes[0], FOutcomes[1], TRacer.Created, TRacer.Destroyed, TRacer.Alive]);
end;

function TConcurrencyWorld.ClosureOutcomesReport: string;
begin
  var Served := 0;
  for var I := 0 to High(FOutcomes) do
    if FOutcomes[I] = 'bag' then Inc(Served);
  Result := Format('hilos servidos=%d de %d', [Served, Length(FOutcomes)]);
end;

function TConcurrencyWorld.TeardownBalanceReport: string;
begin
  // El requisito dice "se destruyo una vez cada uno de los que llego a construir", no cuantos
  // fueron. Bajo un cierre concurrente son uno o dos: quien pierde la carrera despues de
  // construir tambien construyo, y su instancia se destruye. Afirmar 1 hacia el escenario
  // intermitente por medir algo que el requisito no promete.
  Result := Format('sin destruir=%d, vivos=%d, construidos>0=%s',
    [TRacer.Created - TRacer.Destroyed, TRacer.Alive, BoolToStr(TRacer.Created > 0, True)]);
end;

function TConcurrencyWorld.LiveAfterRaceReport: string;
begin
  // Imprime la cifra, no un booleano: si sobra alguno, el fallo tiene que decir cuantos.
  // Se lee desde el paso Then, que es una rutina distinta de la que abrio los ambitos --dentro
  // de aquella, los temporales del compilador cuentan como vivos.
  Result := Format('vivos=%d, construidos>0=%s',
    [TRacer.Alive, BoolToStr(TRacer.Created > 0, True)]);
end;

function TConcurrencyWorld.TeardownReport: string;
begin
  Result := Format('construidos=%d, destruidos=%d, vivos=%d',
    [TRacer.Created, TRacer.Destroyed, TRacer.Alive]);
end;

initialization

Feature('''
Feature Resolving from several threads at once @di @concurrency

  As the author of a server that opens a scope per request
  I want the container to build each registration once and hand the same instance to every thread
  So that a constructor with side effects runs once, and not once per thread
''')

.UseWorld<TConcurrencyWorld>

.Rule('Several threads asking for the same registration get a single instance')

  .Scenario('Several threads ask for the same singleton')
    .Given('a singleton registered and not resolved yet, with one scope per thread',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.Build;
        W.OpenScopePerThread;
      end)
    .When('they all resolve it at once',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceFromOwnScope;
      end)
    .&Then('its constructor ran exactly once',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.ConstructorsReport).ToEqual('constructores=1');
      end)
    .&And('every thread got the same instance',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.DistinctReport).ToEqual('instancias distintas=1');
      end)

  .Scenario('Several threads ask for the same scoped service in a shared scope')
    .Given('a scoped service registered and a single scope shared by every thread',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Scoped);
        W.Build;
        W.OpenSharedScope;
      end)
    .When('they all resolve it from that same scope',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceFromSharedScope;
      end)
    .&Then('its constructor ran exactly once',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.ConstructorsReport).ToEqual('constructores=1');
      end)
    .&And('every thread got the same instance',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.DistinctReport).ToEqual('instancias distintas=1');
      end)

  // El control de la regla: un Transient no pasa por el diccionario, asi que ya esta bien hoy.
  // Esta para que el cerrojo no lo convierta en un singleton por accidente.
  .Scenario('Several threads ask for the same transient')
    .Given('a transient registered',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Transient);
        W.Build;
      end)
    .When('they all resolve it at once',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceFromRootProvider;
      end)
    .&Then('there are as many instances as requests',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.ConstructorsReport).ToEqual('constructores=8');
      end)
    .&And('each thread got its own',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.DistinctReport).ToEqual('instancias distintas=8');
      end)

.Rule('Resolving from several threads does not throw nor leave the container inconsistent')

  .Scenario('Nobody gets an error for resolving at the same time')
    .Given('a container with two services registered and one scope per thread',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Scoped);
        W.Services.AddSingleton(TypeInfo(IOther), TOther);
        W.Build;
        W.OpenScopePerThread;
      end)
    .When('several threads resolve at once, repeatedly',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceResolvingTwoServices;
      end)
    .&Then('no resolution failed',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.FailureReport).ToEqual(SinErrores);
      end)
    .&And('each service was built once and handed to everybody',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.DistinctReport).ToEqual('instancias distintas=8');
      end)

  // El emparejamiento de un contrato sin fijar se guarda en una cache de la coleccion, y esa
  // cache se escribe al RESOLVER -- no al componer. Es el unico estado de la coleccion que dos
  // hilos pueden tocar a la vez.
  .Scenario('Several threads resolve open generic closures for the first time')
    .Given('an unfixed registration and nothing resolved yet',
      procedure(W: TConcurrencyWorld)
      begin
        W.Services.AddTransient<IBag<_>, TBag<_>>;
        W.Build;
      end)
    .When('several threads each resolve a different closure for the first time, at once',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceOnOpenGenericClosures;
      end)
    .&Then('no resolution failed',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.FailureReport).ToEqual(SinErrores);
      end)
    .&And('and every thread got the closure it asked for',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.ClosureOutcomesReport).ToEqual('hilos servidos=8 de 8');
      end)

  .Scenario('Ownership accounting survives the concurrency')
    .Given('a freshly built container with a singleton registered',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.Build;
        W.OpenScopePerThread;
        var Access: IContainerAccess;
        if not Supports(W.Provider, IContainerAccess, Access) then
          raise Exception.Create('el proveedor no expone IContainerAccess');
        W.OwnedBefore := Access.OwnedInstanceCount;
      end)
    .When('several threads resolve that singleton at once',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceFromOwnScope;
        W.ReadOwnership;
      end)
    .&Then('it owns as many objects as instances were built',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.OwnershipReport).ToEqual('constructores=1, poseidos=1');
      end)

  .Scenario('And closing destroys everything that was built')
    .Given('the same container after that concurrent resolution',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.Build;
        W.OpenScopePerThread;
        W.RaceFromOwnScope;
      end)
    .When('it is closed',
      procedure(W: TConcurrencyWorld)
      begin
        W.Close;
      end)
    .&Then('each object it owned was destroyed once, and none is left alive',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.TeardownReport).ToEqual('construidos=1, destruidos=1, vivos=0');
      end)

.Rule('A scope is a lifetime boundary, not a thread boundary')

  .Scenario('Two threads resolve different services from the same scope')
    .Given('a shared scope with a singleton and a scoped service registered, neither resolved',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.RegisterOther(Scoped);
        W.Build;
        W.OpenSharedScope;
      end)
    .When('one thread resolves the singleton and another the scoped service, at once',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceTwoDifferentServicesFromSharedScope;
      end)
    .&Then('neither is rejected for looking like a cycle to the other',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.FailureReport).ToEqual(SinErrores);
      end)
    .&And('both got their service',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.BothServedReport).ToEqual('racer=1, other=1');
      end)

  .Scenario('A thread resolves from a scope another thread created')
    .Given('a scope created in one thread, with a scoped service registered',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Scoped);
        W.Build;
        W.OpenSharedScope;
      end)
    .When('another thread resolves from it, and then its creator resolves again',
      procedure(W: TConcurrencyWorld)
      begin
        W.ResolveSharedScopeFromOtherThread;
      end)
    .&Then('the other thread got it with no error',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.FailureReport).ToEqual(SinErrores);
      end)
    .&And('and both threads see the same instance',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.SameInstanceReport).ToEqual('otro hilo=1, su creador=1');
      end)

  // Un ambito por hilo es lo que haria quien crea que asi aisla: no aisla. El ambito de
  // singletons es uno para todos, y este escenario es el que lo dice.
  .Scenario('One scope per thread does not isolate the singletons')
    .Given('several threads, each with its own scope',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.Build;
        W.OpenScopePerThread;
      end)
    .When('they all resolve the same singleton',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceFromOwnScope;
      end)
    .&Then('they all get the same instance, because the singletons scope is shared',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.DistinctReport).ToEqual('instancias distintas=1');
      end)

  .Scenario('Closing from a thread other than the one that opened the scope')
    .Given('a scope opened in one thread with a scoped service resolved',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Scoped);
        W.NoCtorDelay;
        W.Build;
        W.OpenSharedScope;
        W.SharedScope.ServiceProvider.GetRequiredService<IRacer>;
      end)
    .When('another thread closes the container',
      procedure(W: TConcurrencyWorld)
      begin
        W.CloseFromOtherThread;
      end)
    .&Then('what the scope owned was destroyed, just as if its creator had closed it',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.TeardownReport).ToEqual('construidos=1, destruidos=1, vivos=0');
      end)

.Rule('Closing while another thread resolves ends in a defined state')

  .Scenario('A close concurrent with resolutions')
    .Given('a container with services registered and several threads about to resolve in a loop',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.NoCtorDelay;
        W.Build;
        W.OpenScopePerThread;
      end)
    .When('another thread closes it while they resolve',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceResolveWhileClosing;
      end)
    .&Then('every resolution either delivered or failed with the shut down error, and nothing else',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.UnexpectedOutcomesReport).ToEqual(SinInesperados);
      end)
    .&And('and the race really happened: somebody met the close',
      procedure(W: TConcurrencyWorld)
      begin
        // Sin esto el escenario pasa cuando el cierre llega despues de que todos terminen, que
        // es no haber medido nada.
        Expect(W.ClosedSeenCount).ToBeGreaterThan(0);
      end)

  .Scenario('After that race nothing is left alive')
    .Given('the same container after closing under load',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.NoCtorDelay;
        W.Build;
        W.OpenScopePerThread;
        W.RaceResolveWhileClosing;
      end)
    .When('what is left is checked',
      procedure(W: TConcurrencyWorld)
      begin
        W.ReadOwnership;
      end)
    .&Then('it owns nothing',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.OwnedAfter).ToEqual(0);
      end)
    .&And('and each object it had built was destroyed exactly once',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.TeardownBalanceReport).ToEqual('sin destruir=0, vivos=0, construidos>0=True');
      end)

  .Scenario('A close concurrent with scopes being released')
    .Given('a container with a scoped service and several threads about to open and release scopes',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Scoped);
        W.NoCtorDelay;
        W.Build;
      end)
    .When('another thread closes it while they open, resolve and release scopes in a loop',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceReleaseScopesWhileClosing;
      end)
    .&Then('every unit of work either finished or failed with the shut down error, and nothing else',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.UnexpectedOutcomesReport).ToEqual(SinInesperados);
      end)
    .&And('and the race really happened: somebody met the close',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.ClosedSeenCount).ToBeGreaterThan(0);
      end)
    .&And('and nothing was left alive by the scopes that died during the close',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.LiveAfterRaceReport).ToEqual('vivos=0, construidos>0=True');
      end)

  .Scenario('Whoever loses the race does not leave its instance behind')
    .Given('a container whose singleton constructor does not return until the close is done',
      procedure(W: TConcurrencyWorld)
      begin
        W.Register(Singleton);
        W.HoldCtorUntilClosed;
        W.Build;
      end)
    .When('one thread builds it while another closes the container',
      procedure(W: TConcurrencyWorld)
      begin
        W.RaceConstructionAgainstClose;
      end)
    .&Then('it gets the shut down error, and the instance it had built was destroyed',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.LoserOutcomeReport).ToEqual(
          'constructor=EContainerShutDownError, cierre=closer, construidos=1, destruidos=1, vivos=0');
      end)

;

end.
