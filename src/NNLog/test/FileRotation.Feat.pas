unit FileRotation.Feat;

// La rotacion esta documentada entera --seis propiedades en una tabla del GUIDE-- y no la tocaba
// ningun escenario. Estos afirman lo que un lector VE: como se llama el archivo y que pasa con la
// linea que provoco la rotacion.
//
// La politica Size es la unica que se dispara sin esperar a que cambie el reloj, asi que es la que
// usan los escenarios que solo necesitan que rote. El de la fecha usa Daily y fija la fecha de
// creacion del fichero a mano, porque es justo la fecha que la funcion de nombrado recibe y que
// hoy ignora: sin fijarla, "ayer" y "ahora" caen el mismo dia y el defecto no se ve.
//
// El layout se fija a la linea literal por la misma razon que en TargetRecovery: los renderers no
// estan registrados fuera de un host, y mezclar ese defecto con este haria ilegibles los dos.

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.NNLog,
  Daf.NNLog.Targets.File_;

type
  TRotationWorld = class
  strict private
    FDir: string;
    FTarget: TFileTarget;
    FHolds: TArray<TObject>;
    FSavedSinks: TArray<IBootstrapSink>;
    FChannel: IBootstrapSink;
  public
    Failure: string;
    constructor Create;
    destructor Destroy; override;
    function LogPath: string;
    function HistoryDir: string;
    procedure WriteLine(const Line: string);
    /// Deja un fichero con esa antiguedad como log actual, sin pasar por el destino: es la unica
    /// forma de que la fecha del contenido y la del momento de rotar no coincidan.
    procedure ExistingLogCreatedDaysAgo(const Days: Integer; const Line: string);
    procedure RotateOnEverySize;
    procedure RotateDaily;
    procedure KeepAtMostOneArchive;
    /// TODOS los archivos del historico retenidos: la limpieza no puede borrar ninguno, asi que
    /// lanza con seguridad. Retener solo uno dependia de en que orden cayera al ordenar por fecha.
    procedure HoldEveryArchive;
    /// Que sigan ahi ES la prueba de que la limpieza fallo. Sin ella, "la linea se escribio" lo
    /// cumple tambien una limpieza que nunca llego a intentarlo.
    function HeldArchivesStillThere: Boolean;
    function ArchivedNames: TArray<string>;
    function OneArchivedName: string;
    function CurrentText: string;
    /// Un fichero por proceso: app-${pid}.log, con el renderer de pid registrado como lo registra
    /// el host.
    procedure OneFilePerProcess;
    /// Lo mismo, con la carpeta escrita como un marcador que resuelve a la de siempre, como ${binPath}.
    procedure OneFilePerProcessInMarkedFolder;
    procedure RotateBySize(const MaxBytes: Int64);
    procedure NeverRotate;
    /// Un fichero de la carpeta que no escribe este destino, cerrado y con esa antiguedad.
    procedure ForeignFileCreatedDaysAgo(const Name: string; const Days: Integer);
    procedure ForeignFileOfSize(const Name: string; const Bytes: Integer);
    /// Retenido como lo retiene otro proceso que escribe en el con NNLog.
    procedure HoldForWriting(const Name: string);
    procedure WatchStartupChannel;
    function ChannelText: string;
    function InFolder(const Name: string): Boolean;
    /// El nombre en history/ del archivo que procede de ese fichero, o vacio si no hay.
    function ArchivedNameOf(const Name: string): string;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  Daf.Types,
  Daf.NNLog.Layout,
  Daf.MiniSpec;

type
  TChannelRecorder = class(TInterfacedObject, IBootstrapSink)
  strict private
    FText: string;
  public
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
    property Text: string read FText;
  end;

function TChannelRecorder.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TChannelRecorder.Write(const Line: string);
begin
  FText := FText + Line + sLineBreak;
end;

function ReadAllTextShared(const Path: string): string;
begin
  if not TFile.Exists(Path) then
    Exit(EmptyStr);
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

function EntryFor: TLogEntry;
begin
  Result := Default(TLogEntry);
  Result.Level := TLogLevel.Information;
  Result.Category := 'Prueba';
  Result.State := TLogState.Create('linea');
end;

{ TRotationWorld }

constructor TRotationWorld.Create;
begin
  inherited Create;
  FDir := TPath.Combine(TPath.GetTempPath, 'nnlog-rot-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(FDir);
  FTarget := TFileTarget.Create;
  FTarget.Name := 'fichero';
  FTarget.FileName := LogPath;
  FSavedSinks := BootstrapLog.Sinks;
end;

destructor TRotationWorld.Destroy;
begin
  FTarget.Free;
  for var H in FHolds do
    H.Free;
  // El canal es global: cada escenario lo deja como lo encontro.
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  FChannel := nil;
  if TDirectory.Exists(FDir) then
    TDirectory.Delete(FDir, True);
  inherited;
end;

function TRotationWorld.LogPath: string;
begin
  Result := TPath.Combine(FDir, 'app.log');
end;

function TRotationWorld.HistoryDir: string;
begin
  Result := TPath.Combine(FDir, 'history');
end;

procedure TRotationWorld.WriteLine(const Line: string);
begin
  Failure := EmptyStr;
  FTarget.Layout := Line;
  try
    FTarget.Write(EntryFor);
  except
    on E: Exception do
      Failure := E.ClassName;
  end;
end;

procedure TRotationWorld.ExistingLogCreatedDaysAgo(const Days: Integer; const Line: string);
begin
  TFile.WriteAllText(LogPath, Line + sLineBreak);
  TFile.SetCreationTime(LogPath, IncDay(Now, -Days));
end;

procedure TRotationWorld.RotateOnEverySize;
begin
  FTarget.RotationPolicy := TRotationPolicy.Size;
  FTarget.MaxFileSize := 1;
end;

procedure TRotationWorld.RotateDaily;
begin
  FTarget.RotationPolicy := TRotationPolicy.Daily;
end;

procedure TRotationWorld.KeepAtMostOneArchive;
begin
  FTarget.MaxArchivedFiles := 1;
end;

procedure TRotationWorld.HoldEveryArchive;
begin
  TDirectory.CreateDirectory(HistoryDir);
  for var I := 1 to 3 do
  begin
    var P := TPath.Combine(HistoryDir, Format('viejo-%d.log', [I]));
    TFile.WriteAllText(P, 'x');
    FHolds := FHolds + [TFileStream.Create(P, fmOpenRead or fmShareExclusive)];
  end;
end;

function TRotationWorld.HeldArchivesStillThere: Boolean;
begin
  Result := True;
  for var I := 1 to 3 do
    if not TFile.Exists(TPath.Combine(HistoryDir, Format('viejo-%d.log', [I]))) then
      Exit(False);
end;

function TRotationWorld.ArchivedNames: TArray<string>;
begin
  SetLength(Result, 0);
  if not TDirectory.Exists(HistoryDir) then
    Exit;
  for var F in TDirectory.GetFiles(HistoryDir) do
    Result := Result + [TPath.GetFileName(F)];
end;

function TRotationWorld.OneArchivedName: string;
begin
  Result := EmptyStr;
  for var N in ArchivedNames do
    if N.StartsWith('app_') then
      Exit(N);
end;

function TRotationWorld.CurrentText: string;
begin
  Result := ReadAllTextShared(LogPath);
end;

procedure TRotationWorld.OneFilePerProcess;
begin
  TLogLayoutEngine.RegisterRenderer('pid',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := IntToStr(Debugger.CurrentProcessId);
    end);
  FTarget.FileName := TPath.Combine(FDir, 'app-${pid}.log');
end;

procedure TRotationWorld.OneFilePerProcessInMarkedFolder;
begin
  OneFilePerProcess;
  var Dir := FDir;
  TLogLayoutEngine.RegisterRenderer('rotationSpecDir',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := Dir;
    end);
  FTarget.FileName := '${rotationSpecDir}/app-${pid}.log';
end;

procedure TRotationWorld.RotateBySize(const MaxBytes: Int64);
begin
  FTarget.RotationPolicy := TRotationPolicy.Size;
  FTarget.MaxFileSize := MaxBytes;
end;

procedure TRotationWorld.NeverRotate;
begin
  FTarget.RotationPolicy := TRotationPolicy.None;
end;

procedure TRotationWorld.ForeignFileCreatedDaysAgo(const Name: string; const Days: Integer);
begin
  var P := TPath.Combine(FDir, Name);
  TFile.WriteAllText(P, 'DE-OTRA-EJECUCION' + sLineBreak);
  TFile.SetCreationTime(P, IncDay(Now, -Days));
end;

procedure TRotationWorld.ForeignFileOfSize(const Name: string; const Bytes: Integer);
begin
  TFile.WriteAllText(TPath.Combine(FDir, Name), StringOfChar('x', Bytes));
end;

procedure TRotationWorld.HoldForWriting(const Name: string);
begin
  FHolds := FHolds + [TFileStream.Create(TPath.Combine(FDir, Name), fmOpenWrite or fmShareDenyWrite)];
end;

procedure TRotationWorld.WatchStartupChannel;
begin
  FChannel := TChannelRecorder.Create;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(FChannel);
end;

function TRotationWorld.ChannelText: string;
begin
  if FChannel = nil then
    Exit(EmptyStr);
  Result := (FChannel as TChannelRecorder).Text;
end;

function TRotationWorld.InFolder(const Name: string): Boolean;
begin
  Result := TFile.Exists(TPath.Combine(FDir, Name));
end;

function TRotationWorld.ArchivedNameOf(const Name: string): string;
begin
  Result := EmptyStr;
  var Stem := TPath.GetFileNameWithoutExtension(Name) + '_';
  for var N in ArchivedNames do
    if N.StartsWith(Stem) then
      Exit(N);
end;

initialization

Feature('''
Feature La rotacion de NNLog archiva con un nombre que se puede leer @nnlog @targets @rotation

  Como quien deja un servicio escribiendo su log durante dias
  Quiero que el archivo rotado se llame como el fichero que archiva y por el periodo que contiene
  Para poder encontrar el log de un dia por su nombre, y para que archivar no me cueste una linea
''')

.UseWorld<TRotationWorld>

.Rule('El archivo rotado conserva la extension del fichero que archiva')

  .Scenario('El nombre rotado tiene un solo punto antes de la extension')
    .Given('un destino que escribe en un fichero con extension, y que rota por tamano',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
        W.RotateOnEverySize;
      end)
    .When('la escritura siguiente dispara la rotacion',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('SEGUNDA');
      end)
    .&Then('el archivo aparece en el historico',
      procedure(W: TRotationWorld)
      begin
        Expect(W.OneArchivedName).ToContain('app_');
      end)
    .&And('y lleva la extension del original una sola vez',
      procedure(W: TRotationWorld)
      begin
        Expect(W.OneArchivedName).ToEndWith('.log');
      end)
    .&And('sin el punto de mas que delata la concatenacion',
      procedure(W: TRotationWorld)
      begin
        Expect(W.OneArchivedName.Contains('..')).ToBeFalse;
      end)

.Rule('El archivo rotado se nombra por el periodo que contiene')

  .Scenario('El log de ayer se archiva con la fecha de ayer')
    .Given('un log actual cuyo contenido es de hace dos dias, y rotacion diaria',
      procedure(W: TRotationWorld)
      begin
        W.ExistingLogCreatedDaysAgo(2, 'DE-HACE-DOS-DIAS');
        W.RotateDaily;
      end)
    .When('se escribe ya entrado otro dia, y la rotacion se dispara',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('DE-HOY');
      end)
    .&Then('el archivo lleva la fecha del contenido que archiva',
      procedure(W: TRotationWorld)
      begin
        Expect(W.OneArchivedName).ToContain(FormatDateTime('yyyy-mm-dd', IncDay(Now, -2)));
      end)

.Rule('La rotacion no deja el destino sin recurso mientras trabaja')

  .Scenario('CONTROL: tras rotar, el fichero actual tiene la linea nueva y el archivo la vieja')
    .Given('un destino que ya escribio una linea, y que rota por tamano',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('VIEJA');
        W.RotateOnEverySize;
      end)
    .When('la escritura siguiente dispara la rotacion',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('NUEVA');
      end)
    .&Then('el fichero actual tiene la linea nueva',
      procedure(W: TRotationWorld)
      begin
        Expect(W.CurrentText).ToContain('NUEVA');
      end)
    .&And('y ya no tiene la vieja, que esta archivada',
      procedure(W: TRotationWorld)
      begin
        Expect(W.CurrentText.Contains('VIEJA')).ToBeFalse;
      end)

  .Scenario('Una rotacion cuya limpieza falla no cuesta la linea')
    .Given('un historico con un archivo retenido, y un destino que solo guarda uno',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('VIEJA');
        W.HoldEveryArchive;
        W.KeepAtMostOneArchive;
        W.RotateOnEverySize;
      end)
    .When('la escritura siguiente dispara una rotacion cuya limpieza no puede borrar',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('SOBREVIVE');
      end)
    .&Then('la linea se escribe igual',
      procedure(W: TRotationWorld)
      begin
        Expect(W.CurrentText).ToContain('SOBREVIVE');
      end)
    .&And('y los archivos que la limpieza no pudo borrar siguen ahi, que es lo que prueba que fallo',
      procedure(W: TRotationWorld)
      begin
        Expect(W.HeldArchivesStillThere).ToBeTrue;
      end)

.Rule('Un fichero de otra ejecucion del destino rota cuando vence su periodo')

  .Scenario('El fichero de ayer de otra ejecucion se archiva')
    .Given('un destino con rotacion diaria y un fichero por proceso, y en su carpeta el de otra ejecucion, cerrado y creado ayer',
      procedure(W: TRotationWorld)
      begin
        W.OneFilePerProcess;
        W.RotateDaily;
        W.ForeignFileCreatedDaysAgo('app-4242.log', 1);
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('ese fichero esta en el historico, nombrado con la fecha de ayer',
      procedure(W: TRotationWorld)
      begin
        Expect(W.ArchivedNameOf('app-4242.log')).ToContain(FormatDateTime('yyyy-mm-dd', IncDay(Now, -1)));
      end)
    .&And('ya no esta en la carpeta',
      procedure(W: TRotationWorld)
      begin
        Expect(W.InFolder('app-4242.log')).ToBeFalse;
      end)

  .Scenario('Un marcador en la carpeta no impide archivar')
    .Given('un destino diario con la carpeta escrita como marcador y un fichero por proceso, y el de otra ejecucion creado ayer',
      procedure(W: TRotationWorld)
      begin
        W.OneFilePerProcessInMarkedFolder;
        W.RotateDaily;
        W.ForeignFileCreatedDaysAgo('app-4242.log', 1);
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('ese fichero esta en el historico',
      procedure(W: TRotationWorld)
      begin
        Expect(W.ArchivedNameOf('app-4242.log')).ToStartWith('app-4242_');
      end)

  .Scenario('El fichero de hoy de otra ejecucion se queda')
    .Given('un destino con rotacion diaria y un fichero por proceso, y en su carpeta el de otra ejecucion, cerrado y creado hoy',
      procedure(W: TRotationWorld)
      begin
        W.OneFilePerProcess;
        W.RotateDaily;
        W.ForeignFileCreatedDaysAgo('app-4242.log', 0);
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('ese fichero sigue en la carpeta',
      procedure(W: TRotationWorld)
      begin
        Expect(W.InFolder('app-4242.log')).ToBeTrue;
      end)

  .Scenario('Un fichero que otro proceso tiene abierto se queda, sin informe')
    .Given('un destino diario con un fichero por proceso, y el de otra ejecucion creado ayer y abierto para escribir',
      procedure(W: TRotationWorld)
      begin
        W.WatchStartupChannel;
        W.OneFilePerProcess;
        W.RotateDaily;
        W.ForeignFileCreatedDaysAgo('app-4242.log', 1);
        W.HoldForWriting('app-4242.log');
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('ese fichero sigue en la carpeta',
      procedure(W: TRotationWorld)
      begin
        Expect(W.InFolder('app-4242.log')).ToBeTrue;
      end)
    .&And('el canal de arranque no recibe nada',
      procedure(W: TRotationWorld)
      begin
        Expect(W.ChannelText).ToEqual(EmptyStr);
      end)

  .Scenario('Un fichero que no es del destino no se toca')
    .Given('un destino con rotacion diaria y un fichero por proceso, y en su carpeta uno creado ayer que su FileName no produce',
      procedure(W: TRotationWorld)
      begin
        W.OneFilePerProcess;
        W.RotateDaily;
        W.ForeignFileCreatedDaysAgo('app-viejo.log', 1);
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('ese fichero sigue en la carpeta',
      procedure(W: TRotationWorld)
      begin
        Expect(W.InFolder('app-viejo.log')).ToBeTrue;
      end)

  .Scenario('Sin rotacion no se archiva nada')
    .Given('un destino sin rotacion y con un fichero por proceso, y en su carpeta el de otra ejecucion, creado ayer',
      procedure(W: TRotationWorld)
      begin
        W.OneFilePerProcess;
        W.NeverRotate;
        W.ForeignFileCreatedDaysAgo('app-4242.log', 1);
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('ese fichero sigue en la carpeta',
      procedure(W: TRotationWorld)
      begin
        Expect(W.InFolder('app-4242.log')).ToBeTrue;
      end)

  .Scenario('Por tamano, rota el que se paso')
    .Given('un destino por tamano con un fichero por proceso, y dos de otras ejecuciones: uno pasado del maximo y otro no',
      procedure(W: TRotationWorld)
      begin
        W.OneFilePerProcess;
        W.RotateBySize(10);
        W.ForeignFileOfSize('app-4242.log', 20);
        W.ForeignFileOfSize('app-4343.log', 2);
      end)
    .When('este proceso registra su primera linea',
      procedure(W: TRotationWorld)
      begin
        W.WriteLine('PRIMERA');
      end)
    .&Then('el que se paso esta en el historico',
      procedure(W: TRotationWorld)
      begin
        Expect(W.ArchivedNameOf('app-4242.log')).ToStartWith('app-4242_');
      end)
    .&And('y el otro sigue en la carpeta',
      procedure(W: TRotationWorld)
      begin
        Expect(W.InFolder('app-4343.log')).ToBeTrue;
      end);

end.
