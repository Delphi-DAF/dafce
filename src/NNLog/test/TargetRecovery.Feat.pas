unit TargetRecovery.Feat;

// El destino de fichero tiene un estado del que hoy no se sale: sus caminos de fallo dejan la ruta
// actual fijada con el stream a nil, y la guarda "si la ruta es la misma, no hay nada que hacer"
// se cumple entonces, asi que ya no se reabre nada. A partir de ahi TODO el log del proceso falla.
//
// Que el bloqueador es la guarda y no otra cosa lo dice el CONTRA-CONTROL: desde el MISMO objeto
// que acaba de fallar, escribir en OTRA ruta funciona a la primera. Sin el, un "el Write falla"
// podria venir de un handle perdido o de una fixture rota, y el escenario seguiria en verde
// mintiendo. El CONTROL es la otra mitad: sin el, "no llego la linea" lo cumple tambien un destino
// que nunca escribio nada.
//
// El "otro proceso que tiene el log abierto" se simula reteniendo el fichero con fmShareExclusive
// desde el propio escenario: sin base de datos, sin hilos y sin DI.
//
// El layout se fija a la linea literal que se quiere escribir. No es un atajo: el motor de layout
// resuelve a vacio cualquier token mientras nadie haya registrado los renderers, y eso es un
// defecto aparte --el de LayoutTokens.Feat--. Mezclarlo aqui haria que un fallo suyo se leyera
// como un fallo de la maquina de estados.

interface

uses
  System.SysUtils,
  System.Classes,
  Daf.Extensions.Logging,
  Daf.NNLog,
  Daf.NNLog.Targets.File_;

type
  // Un destino del canal de arranque que se queda con lo que le llega: es por donde tiene que salir
  // el fallo cuando el Write deje de propagarlo.
  IChannelProbe = interface(IBootstrapSink)
    ['{D56567F1-0A94-453F-8259-B9988EF467CA}']
    function Text: string;
    function Count: Integer;
  end;

  TRecoveryWorld = class
  strict private
    FDir: string;
    FTarget: TFileTarget;
    FHold: TFileStream;
    FSavedSinks: TArray<IBootstrapSink>;
    FChannel: IChannelProbe;
    FSpy: TObject;
  public
    Failure: string;
    FailureClass: string;
    constructor Create;
    destructor Destroy; override;
    function PathOf(const Name: string): string;
    /// Deja el canal de arranque con un solo destino, el que graba, para poder afirmar que llego
    /// --o que no llego-- sin que lo tape ninguno de los de serie.
    procedure WatchStartupChannel;
    procedure WriteTo(const Name, Line: string);
    procedure HoldExclusively(const Name: string);
    procedure Release;
    /// Lo que una regla hace de verdad: recorrer sus destinos y despachar a cada uno.
    procedure RuleWritesToBoth(const FailingName, GoodName, Line: string);
    function TextIn(const Name: string): string;
    function ChannelText: string;
    function ProbeGot: string;
  end;

implementation

uses
  System.IOUtils,
  Daf.MiniSpec;

type
  TChannelProbe = class(TInterfacedObject, IBootstrapSink, IChannelProbe)
  strict private
    FLines: TArray<string>;
  public
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
    function Text: string;
    function Count: Integer;
  end;

  // Un segundo destino que no escribe a ningun sitio: solo anota si le llego la linea. Es lo que
  // permite afirmar que un destino que falla no calla al que va con el.
  TSpyTarget = class(TTarget)
  strict private
    FGot: string;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    property Got: string read FGot;
  end;

{ TChannelProbe }

function TChannelProbe.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TChannelProbe.Write(const Line: string);
begin
  FLines := FLines + [Line];
end;

function TChannelProbe.Text: string;
begin
  Result := string.Join(sLineBreak, FLines);
end;

function TChannelProbe.Count: Integer;
begin
  Result := Length(FLines);
end;

{ TSpyTarget }

procedure TSpyTarget.DoWrite(const Entry: TLogEntry);
begin
  FGot := FGot + RenderEvent(Entry);
end;

{ TRecoveryWorld }

function ReadAllTextShared(const Path: string): string;
begin
  if not TFile.Exists(Path) then
    Exit(EmptyStr);
  // Compartido a proposito: el destino tiene el fichero abierto mientras se lee.
  var FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    var SR := TStreamReader.Create(FS);
    try
      Result := SR.ReadToEnd;
    finally
      SR.Free;
    end;
  finally
    FS.Free;
  end;
end;

function EntryFor(const Category: string): TLogEntry;
begin
  Result := Default(TLogEntry);
  Result.Level := TLogLevel.Information;
  Result.Category := Category;
  Result.State := TLogState.Create('linea');
end;

constructor TRecoveryWorld.Create;
begin
  inherited Create;
  FDir := TPath.Combine(TPath.GetTempPath, 'nnlog-rec-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(FDir);
  FTarget := TFileTarget.Create;
  FTarget.Name := 'fichero';
  FSavedSinks := BootstrapLog.Sinks;
end;

destructor TRecoveryWorld.Destroy;
begin
  FTarget.Free;
  FSpy.Free;
  Release;
  // El canal es global: cada escenario lo deja como lo encontro.
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  FChannel := nil;
  if TDirectory.Exists(FDir) then
    TDirectory.Delete(FDir, True);
  inherited;
end;

function TRecoveryWorld.PathOf(const Name: string): string;
begin
  Result := TPath.Combine(FDir, Name);
end;

procedure TRecoveryWorld.WatchStartupChannel;
begin
  FChannel := TChannelProbe.Create;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(FChannel);
end;

procedure TRecoveryWorld.WriteTo(const Name, Line: string);
begin
  Failure := EmptyStr;
  FailureClass := EmptyStr;
  FTarget.FileName := PathOf(Name);
  FTarget.Layout := Line;
  try
    FTarget.Write(EntryFor('Prueba'));
  except
    on E: Exception do
    begin
      Failure := E.Message;
      FailureClass := E.ClassName;
    end;
  end;
end;

procedure TRecoveryWorld.HoldExclusively(const Name: string);
begin
  Release;
  FHold := TFileStream.Create(PathOf(Name), fmCreate or fmShareExclusive);
end;

procedure TRecoveryWorld.Release;
begin
  FreeAndNil(FHold);
end;

procedure TRecoveryWorld.RuleWritesToBoth(const FailingName, GoodName, Line: string);
begin
  Failure := EmptyStr;
  FailureClass := EmptyStr;
  HoldExclusively(FailingName);
  FTarget.FileName := PathOf(FailingName);
  FTarget.Layout := Line;

  FreeAndNil(FSpy);
  var Spy := TSpyTarget.Create;
  Spy.Layout := Line;
  FSpy := Spy;

  var Rule := TLogRule.Create;
  try
    // Los cuatro a mano porque TLogRule no declara constructor: arranca con el cero de su clase
    // base --desactivada y con el nivel maximo en el mas bajo--. Es el defecto que arregla la
    // tarea 5, y aqui se ve en que consiste tener que escribirlos.
    Rule.Name := '*';
    Rule.Enabled := True;
    Rule.MinLevel := TLogLevel.Trace;
    Rule.MaxLevel := TLogLevel.Critical;
    Rule.WriteTo := [FTarget, TSpyTarget(FSpy)];
    try
      Rule.Log(EntryFor('Prueba'));
    except
      on E: Exception do
      begin
        Failure := E.Message;
        FailureClass := E.ClassName;
      end;
    end;
  finally
    Rule.Free;
  end;
end;

function TRecoveryWorld.TextIn(const Name: string): string;
begin
  Result := ReadAllTextShared(PathOf(Name));
end;

function TRecoveryWorld.ChannelText: string;
begin
  if FChannel = nil then
    Exit(EmptyStr);
  Result := FChannel.Text;
end;

function TRecoveryWorld.ProbeGot: string;
begin
  if FSpy = nil then
    Exit(EmptyStr);
  Result := TSpyTarget(FSpy).Got;
end;

initialization

Feature('''
Feature Un destino de NNLog que no pudo escribir vuelve a intentarlo @nnlog @targets @recovery

  Como quien registra desde una aplicacion que se queda levantada
  Quiero que un fallo de escritura del log no sea definitivo ni salga hacia mi codigo
  Para que un fichero retenido un momento no deje al proceso sin registros para siempre ni lo tumbe
''')

.UseWorld<TRecoveryWorld>

.Rule('Un destino que fallo vuelve a intentarlo')

  .Scenario('CONTROL: una escritura normal deja la linea en el fichero')
    .When('se registra una linea en un destino que puede escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('a.log', 'PRIMERA');
      end)
    .&Then('el fichero la contiene',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.TextIn('a.log')).ToContain('PRIMERA');
      end)

  .Scenario('El destino se recupera cuando el recurso vuelve')
    .Given('un destino cuyo fichero esta retenido por otro que impide abrirlo',
      procedure(W: TRecoveryWorld)
      begin
        W.HoldExclusively('b.log');
      end)
    .When('se registra una linea, que por eso no llega a escribirse',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('b.log', 'PERDIDA');
      end)
    .&And('deja de estar retenido y se escribe otra vez',
      procedure(W: TRecoveryWorld)
      begin
        W.Release;
        W.WriteTo('b.log', 'RECUPERADA');
      end)
    .&Then('la segunda linea llega al fichero',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.TextIn('b.log')).ToContain('RECUPERADA');
      end)

  .Scenario('CONTRA-CONTROL: con otra ruta el mismo objeto si se recupera')
    .Given('un destino que acaba de fallar al abrir un fichero retenido',
      procedure(W: TRecoveryWorld)
      begin
        W.HoldExclusively('b.log');
        W.WriteTo('b.log', 'PERDIDA');
      end)
    .When('se le pide escribir en otra ruta, sin tocar nada mas',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('c.log', 'OTRA-RUTA');
      end)
    .&Then('escribe a la primera, asi que el objeto sabe recuperarse',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.TextIn('c.log')).ToContain('OTRA-RUTA');
      end)

  .Scenario('Un fallo de apertura no borra el estado que servia')
    .Given('un destino que esta escribiendo correctamente en un fichero',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('a.log', 'ANTES');
        Expect(W.TextIn('a.log')).ToContain('ANTES');
      end)
    .When('una escritura hacia otra ruta falla al abrirla',
      procedure(W: TRecoveryWorld)
      begin
        W.HoldExclusively('b.log');
        W.WriteTo('b.log', 'PERDIDA');
      end)
    .&Then('las escrituras hacia la ruta que funcionaba siguen llegando',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('a.log', 'DESPUES');
        Expect(W.TextIn('a.log')).ToContain('DESPUES');
      end)

.Rule('Un fallo del log no sale como excepcion hacia quien registra')

  .Scenario('El llamante no recibe la excepcion')
    .Given('un destino que no puede escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.HoldExclusively('b.log');
      end)
    .When('se registra una linea por el',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('b.log', 'PERDIDA');
      end)
    .&Then('quien registro continua sin recibir ninguna excepcion',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.FailureClass).ToEqual(EmptyStr);
      end)

  .Scenario('El fallo queda dicho por el canal de arranque')
    .Given('un canal de arranque que graba lo que le llega, y un destino que no puede escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.WatchStartupChannel;
        W.HoldExclusively('b.log');
      end)
    .When('se registra una linea por el',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('b.log', 'PERDIDA');
      end)
    .&Then('el canal recibe el fallo, nombrando el destino',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelText).ToContain('fichero');
      end)

  .Scenario('Un destino que falla no calla a los demas')
    .When('una regla con dos destinos despacha una linea, y el primero no puede escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.RuleWritesToBoth('b.log', 'd.log', 'COMPARTIDA');
      end)
    .&Then('el destino que si puede escribir la recibe',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ProbeGot).ToContain('COMPARTIDA');
      end);

end.
