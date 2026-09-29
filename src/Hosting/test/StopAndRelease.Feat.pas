unit StopAndRelease.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Hosting;

type
  EHostedStopFailed = class(Exception);

  // En la seccion interface: el contenedor construye los servicios alojados por RTTI, y un tipo
  // declarado solo en implementation no esta ahi para encontrarlo.
  TFailingStopService = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  Daf.Hosting;

const
  NoError = '<nada>';

{ TFailingStopService }

procedure TFailingStopService.Start;
begin
end;

procedure TFailingStopService.Stop;
begin
  raise EHostedStopFailed.Create('el servicio no pudo pararse');
end;

function Describe(const E: Exception): string;
begin
  Result := E.ClassName + ': ' + E.Message;
end;

function Holds(const Ref: IHost): string;
begin
  if Assigned(Ref) then
    Result := 'apunta al host'
  else
    Result := 'vacia';
end;

type
  TStopAndNilWorld = class
  strict private
    // Los fallos de la parada se informan por el canal de arranque, y eso sale por la salida de
    // error del arnes. Aqui solo se aparta para que no se confunda con la salida de las specs.
    FSavedSinks: TArray<IBootstrapSink>;
    FStopped: Boolean;
    // Del tamaño de un THost y en un array fijo, para no reservar nada entre la parada y la
    // salida del ambito: el gestor de memoria entrega primero el bloque que el host acaba de
    // devolver, y una referencia que siga apuntando alli encuentra basura y no un host.
    FBlocks: array[0..63] of Pointer;
    procedure ReuseFreedBlock;
    procedure ReleaseBlocks;
  public
    Host: IHost;
    HostCopy: IHost;
    StopOutcome: string;
    HostAfterStop: string;
    CopyAfterStop: string;
    LeaveOutcome: string;
    constructor Create;
    destructor Destroy; override;
    procedure StartHost(WithFailingStop: Boolean);
    procedure KeepCopyReleasedOnStopped;
    procedure StopHost;
    procedure Leave(var Ref: IHost);
  end;

constructor TStopAndNilWorld.Create;
begin
  inherited Create;
  FSavedSinks := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
end;

destructor TStopAndNilWorld.Destroy;
begin
  // Solo queda un host sin parar aqui si el escenario no llego a pararlo.
  if not FStopped and Assigned(Host) then
    try
      StopAndNil(Host);
    except
      on E: Exception do
        ;
    end;
  // Parado, el host esta liberado: lo que todavia apunte a el se suelta sin pasar por _Release.
  Pointer(Host) := nil;
  Pointer(HostCopy) := nil;
  ReleaseBlocks;
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

procedure TStopAndNilWorld.StartHost(WithFailingStop: Boolean);
begin
  var Builder := THost.CreateBuilder;
  if WithFailingStop then
    Hosting.AddHostedService(Builder.Services, TFailingStopService);
  Host := Builder.Build;
  Host.Start;
end;

procedure TStopAndNilWorld.KeepCopyReleasedOnStopped;
begin
  HostCopy := Host;
  Host.Services.GetRequiredService<IHostApplicationLifetime>.ApplicationStopped.Register(
    procedure
    begin
      HostCopy := nil;
    end);
end;

procedure TStopAndNilWorld.StopHost;
begin
  StopOutcome := NoError;
  FStopped := True;
  try
    StopAndNil(Host);
  except
    on E: Exception do
      StopOutcome := Describe(E);
  end;
  ReuseFreedBlock;
  HostAfterStop := Holds(Host);
  CopyAfterStop := Holds(HostCopy);
end;

procedure TStopAndNilWorld.Leave(var Ref: IHost);
begin
  LeaveOutcome := NoError;
  try
    // Es lo que hace la finalizacion de la variable al salir de su ambito: la vacia y llama a
    // _Release sobre lo que tenia.
    Ref := nil;
  except
    on E: Exception do
      LeaveOutcome := Describe(E);
  end;
end;

procedure TStopAndNilWorld.ReuseFreedBlock;
begin
  for var Idx := Low(FBlocks) to High(FBlocks) do
  begin
    FBlocks[Idx] := GetMemory(THost.InstanceSize);
    FillChar(FBlocks[Idx]^, THost.InstanceSize, $FF);
  end;
end;

procedure TStopAndNilWorld.ReleaseBlocks;
begin
  for var Idx := Low(FBlocks) to High(FBlocks) do
  begin
    FreeMemory(FBlocks[Idx]);
    FBlocks[Idx] := nil;
  end;
end;

initialization

Feature('''
Feature Parar el host y soltar su referencia en un solo paso @hosting @lifetime @shutdown

  Como quien escribe el .dpr que arranca y para un host
  Quiero que la referencia con que lo paro quede vacia al pararlo
  Para no tocar un host liberado cuando esa referencia sale de su ambito
''')

.UseWorld<TStopAndNilWorld>

.Rule('Parar y soltar en un solo paso deja la referencia vacia')

  .Scenario('Parar y soltar deja la referencia vacia')
    .Given('un host arrancado, sostenido por una sola referencia',
      procedure(W: TStopAndNilWorld)
      begin
        W.StartHost(False);
      end)
    .When('se para y se suelta en un solo paso',
      procedure(W: TStopAndNilWorld)
      begin
        W.StopHost;
        W.Leave(W.Host);
      end)
    .&Then('la referencia queda vacia',
      procedure(W: TStopAndNilWorld)
      begin
        Expect(W.HostAfterStop).ToEqual('vacia');
      end)
    .&And('salir de su ambito no falla, aunque la memoria del host ya se haya reutilizado',
      procedure(W: TStopAndNilWorld)
      begin
        Expect(W.LeaveOutcome).ToEqual(NoError);
      end)

  .Scenario('Si la parada falla, la referencia queda vacia igual')
    .Given('un host arrancado con un servicio alojado que falla al parar',
      procedure(W: TStopAndNilWorld)
      begin
        W.StartHost(True);
      end)
    .When('se para y se suelta en un solo paso',
      procedure(W: TStopAndNilWorld)
      begin
        W.StopHost;
      end)
    .&Then('sale la excepcion de ese servicio',
      procedure(W: TStopAndNilWorld)
      begin
        Expect(W.StopOutcome).ToEqual('EHostedStopFailed: el servicio no pudo pararse');
      end)
    .&And('la referencia queda vacia',
      procedure(W: TStopAndNilWorld)
      begin
        Expect(W.HostAfterStop).ToEqual('vacia');
      end)

.Rule('La notificacion de parada llega con el host todavia vivo')

  .Scenario('Una copia se suelta al notificarse la parada')
    .Given('un host arrancado, y una copia de su referencia que se suelta al notificarse la parada',
      procedure(W: TStopAndNilWorld)
      begin
        W.StartHost(False);
        W.KeepCopyReleasedOnStopped;
      end)
    .When('se para',
      procedure(W: TStopAndNilWorld)
      begin
        W.StopHost;
        W.Leave(W.HostCopy);
      end)
    .&Then('la copia queda vacia',
      procedure(W: TStopAndNilWorld)
      begin
        Expect(W.CopyAfterStop).ToEqual('vacia');
      end)
    .&And('salir de su ambito no falla, aunque la memoria del host ya se haya reutilizado',
      procedure(W: TStopAndNilWorld)
      begin
        Expect(W.LeaveOutcome).ToEqual(NoError);
      end);

end.
