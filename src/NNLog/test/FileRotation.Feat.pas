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
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  Daf.MiniSpec;

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
end;

destructor TRotationWorld.Destroy;
begin
  FTarget.Free;
  for var H in FHolds do
    H.Free;
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
      end);

end.
