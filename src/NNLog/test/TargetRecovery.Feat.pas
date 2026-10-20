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
    FFlaky: TObject;
    function Flaky: TObject;
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
    /// Varias escrituras seguidas; Failure se queda con la primera excepcion que saliera.
    procedure WriteRepeatedly(const Name, Line: string; const Times: Integer);
    procedure HoldExclusively(const Name: string);
    /// Retiene el fichero como lo retiene otro proceso que escribe en el con NNLog.
    procedure HoldForWriting(const Name: string);
    procedure Release;
    procedure FlakyFails;
    procedure FlakyRecovers;
    procedure WriteFlaky(const Times: Integer);
    function FlakyWritten: Integer;
    function ChannelLinesWith(const Text: string): Integer;
    function ChannelLineWith(const Text: string): string;
    procedure Unname;
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

  // Un destino que falla a voluntad, sin fichero de por medio: la racha de fallos es de TTarget, asi
  // que se mide igual en cualquier destino.
  TFlakyTarget = class(TTarget)
  strict private
    FWritten: Integer;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    Failing: Boolean;
    property Written: Integer read FWritten;
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

{ TFlakyTarget }

procedure TFlakyTarget.DoWrite(const Entry: TLogEntry);
begin
  if Failing then
    raise EInOutError.Create('recurso no disponible');
  Inc(FWritten);
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
  FFlaky.Free;
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

procedure TRecoveryWorld.WriteRepeatedly(const Name, Line: string; const Times: Integer);
begin
  var FirstFailure := EmptyStr;
  var FirstFailureClass := EmptyStr;
  for var I := 1 to Times do
  begin
    WriteTo(Name, Line);
    if FirstFailureClass.IsEmpty then
    begin
      FirstFailure := Failure;
      FirstFailureClass := FailureClass;
    end;
  end;
  Failure := FirstFailure;
  FailureClass := FirstFailureClass;
end;

procedure TRecoveryWorld.HoldExclusively(const Name: string);
begin
  Release;
  FHold := TFileStream.Create(PathOf(Name), fmCreate or fmShareExclusive);
end;

procedure TRecoveryWorld.HoldForWriting(const Name: string);
begin
  Release;
  FHold := TFileStream.Create(PathOf(Name), fmCreate or fmShareDenyWrite);
end;

function TRecoveryWorld.Flaky: TObject;
begin
  if FFlaky = nil then
  begin
    var Target := TFlakyTarget.Create;
    Target.Name := 'inestable';
    Target.Layout := 'linea';
    FFlaky := Target;
  end;
  Result := FFlaky;
end;

procedure TRecoveryWorld.FlakyFails;
begin
  TFlakyTarget(Flaky).Failing := True;
end;

procedure TRecoveryWorld.FlakyRecovers;
begin
  TFlakyTarget(Flaky).Failing := False;
end;

procedure TRecoveryWorld.WriteFlaky(const Times: Integer);
begin
  Failure := EmptyStr;
  FailureClass := EmptyStr;
  for var I := 1 to Times do
    try
      TFlakyTarget(Flaky).Write(EntryFor('Prueba'));
    except
      on E: Exception do
        if FailureClass.IsEmpty then
        begin
          Failure := E.Message;
          FailureClass := E.ClassName;
        end;
    end;
end;

function TRecoveryWorld.FlakyWritten: Integer;
begin
  Result := TFlakyTarget(Flaky).Written;
end;

function TRecoveryWorld.ChannelLinesWith(const Text: string): Integer;
begin
  Result := 0;
  if FChannel = nil then
    Exit;
  for var Line in FChannel.Text.Split([sLineBreak]) do
    if Line.Contains(Text) then
      Inc(Result);
end;

function TRecoveryWorld.ChannelLineWith(const Text: string): string;
begin
  Result := EmptyStr;
  if FChannel = nil then
    Exit;
  for var Line in FChannel.Text.Split([sLineBreak]) do
    if Line.Contains(Text) then
      Exit(Line);
end;

procedure TRecoveryWorld.Release;
begin
  FreeAndNil(FHold);
end;

procedure TRecoveryWorld.Unname;
begin
  FTarget.Name := EmptyStr;
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

  .Scenario('Un destino sin nombre se nombra por su clase')
    .Given('un canal de arranque que graba lo que le llega, y un destino sin nombre que no puede escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.WatchStartupChannel;
        W.Unname;
        W.HoldExclusively('b.log');
      end)
    .When('se registra una linea por el',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteTo('b.log', 'PERDIDA');
      end)
    .&Then('el canal nombra el destino por su clase, y no con una cadena vacia',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelText).ToContain('el destino "TFileTarget"');
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
      end)

.Rule('Un destino que sigue fallando no repite el informe')

  .Scenario('Muchas escrituras fallidas, un informe')
    .Given('un canal de arranque que graba lo que le llega, y un destino que no puede escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.WatchStartupChannel;
        W.FlakyFails;
      end)
    .When('se registran diez lineas por el',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteFlaky(10);
      end)
    .&Then('el canal recibe un solo informe de fallo',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelLinesWith('no pudo escribir')).ToEqual(1);
      end)
    .&And('nombrando el destino',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelLineWith('no pudo escribir')).ToContain('"inestable"');
      end)

  .Scenario('La recuperacion se informa con lo perdido')
    .Given('un canal de arranque que graba lo que le llega, y un destino que fallo en tres escrituras seguidas',
      procedure(W: TRecoveryWorld)
      begin
        W.WatchStartupChannel;
        W.FlakyFails;
        W.WriteFlaky(3);
      end)
    .When('su recurso vuelve y se registra otra linea',
      procedure(W: TRecoveryWorld)
      begin
        W.FlakyRecovers;
        W.WriteFlaky(1);
      end)
    .&Then('la linea se escribe',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.FlakyWritten).ToEqual(1);
      end)
    .&And('el canal recibe un informe de recuperacion que nombra el destino',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelLineWith('vuelve a escribir')).ToContain('"inestable"');
      end)
    .&And('y dice que se perdieron tres lineas',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelLineWith('vuelve a escribir')).ToContain('perdio 3 lineas');
      end)

  .Scenario('Un fallo despues de recuperarse se vuelve a informar')
    .Given('un destino que fallo, se recupero y lo informo',
      procedure(W: TRecoveryWorld)
      begin
        W.WatchStartupChannel;
        W.FlakyFails;
        W.WriteFlaky(1);
        W.FlakyRecovers;
        W.WriteFlaky(1);
        Expect(W.ChannelLinesWith('vuelve a escribir')).ToEqual(1);
      end)
    .When('vuelve a fallar',
      procedure(W: TRecoveryWorld)
      begin
        W.FlakyFails;
        W.WriteFlaky(1);
      end)
    .&Then('el canal recibe un informe de fallo nuevo',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelLinesWith('no pudo escribir')).ToEqual(2);
      end)

  .Scenario('Dos procesos sobre el mismo fichero')
    .Given('un canal de arranque que graba lo que le llega, y un fichero de log que otro tiene abierto para escribir',
      procedure(W: TRecoveryWorld)
      begin
        W.WatchStartupChannel;
        W.HoldForWriting('m.log');
      end)
    .When('se registran varias lineas por un destino de ese fichero',
      procedure(W: TRecoveryWorld)
      begin
        W.WriteRepeatedly('m.log', 'PERDIDA', 5);
      end)
    .&Then('el canal recibe un solo informe de fallo',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.ChannelLinesWith('no pudo escribir')).ToEqual(1);
      end)
    .&And('quien registra no recibe ninguna excepcion',
      procedure(W: TRecoveryWorld)
      begin
        Expect(W.FailureClass).ToEqual(EmptyStr);
      end);

end.
