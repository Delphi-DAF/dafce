unit Daf.NNLog.Targets.File_;

interface

uses
  System.Classes,
  System.DateUtils,
  System.RegularExpressions,
  Daf.Extensions.Logging,
  Daf.NNLog,
  Daf.NNLog.Layout;

type
  TRotationPolicy = (None, hourly, Daily, Days, Monthly, Size);

  TFileTarget = class(TTarget)
  private
    FStream: TStreamWriter;
    FCurrentPath: string;
    FFileName: string;
    FRotationPolicy: TRotationPolicy;
    FMaxFileAge: Byte;
    FMaxArchivedFiles: Byte;
    FMaxFileSize: Int64;
    FCompressOnRotate: Boolean;
    FAutoFlush: Boolean;
    // Los ficheros que FileName produce con otros valores en sus marcadores. Sin marcadores en el
    // nombre, o sin texto literal fuera de la extension, no se buscan.
    FOtherRunPattern: TRegEx;
    FOtherRunsFindable: Boolean;
    FOtherRunsSwept: Boolean;
    // Los archivos de history/ que proceden de FileName, de cualquier ejecucion y comprimidos o no.
    FArchivePattern: TRegEx;
    // Cierto mientras dura una racha de archivados fallidos: el primero se informa, los demas no.
    FArchiveFailing: Boolean;
    // La fecha de comienzo de FStartedPath, fijada al abrirlo: las estrategias la preguntan en cada
    // linea y no se vuelve a leer del disco.
    FStartedPath: string;
    FStartedAt: TDateTime;
    function StartedAt(const Path: string): TDateTime;
    procedure EnsureStreamFor(const Path: string);
    function ResolveFileName(const Entry: TLogEntry): string;
    procedure SetFileName(const Value: string);
    procedure BuildOwnPatterns;
    procedure CleanupHistory(const HistoryDir: string);
    procedure CompressIfAsked(const RotatedPath: string);
    function MoveToHistory(const Path, RotatedPath: string): Boolean;
    function RotateCurrentFileIfNeeded(const Path: string): Boolean;
    procedure ArchiveOtherRuns(const Path: string);
    function MoveOtherRun(const Candidate, HistoryDir: string; out RotatedPath: string): Boolean;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property FileName: string read FFileName write SetFileName;
    property RotationPolicy: TRotationPolicy read FRotationPolicy write FRotationPolicy;
    property MaxArchivedFiles: Byte read FMaxArchivedFiles write FMaxArchivedFiles;
    property MaxFileSize: Int64 read FMaxFileSize write FMaxFileSize;
    property MaxFileAge: Byte read FMaxFileAge write FMaxFileAge;
    property CompressOnRotate: Boolean read FCompressOnRotate write FCompressOnRotate;
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush;
  end;


  TRotationStrategyClass = class of TRotationStrategy;
  TRotationStrategy = class
  public
    class function BuildRotatePath(const Path: string; const FileDate: TDateTime): string;
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; virtual; abstract;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdio,
  {$ENDIF}
  System.Zip,
  System.IOUtils,
  System.Rtti,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Types,
  System.SysUtils;

type
  TRotationNone = class(TRotationStrategy)
  private
  public
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; override;
  end;

  TRotationDaily = class(TRotationStrategy)
  private
  public
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; override;
  end;

  TRotationMonthly = class(TRotationStrategy)
  public
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; override;
  end;

  TRotationDays = class(TRotationStrategy)
  public
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; override;
  end;

  TRotationHourly = class(TRotationStrategy)
  public
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; override;
  end;

  TRotationSize = class(TRotationStrategy)
  public
    class function NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean; override;
  end;

const
  StrategyOf: array[TRotationPolicy] of TRotationStrategyClass = (
    TRotationNone,
    TRotationHourly,
    TRotationDaily,
    TRotationDays,
    TRotationMonthly,
    TRotationSize
  );

// Un destino de fichero abre el suyo con fmShareDenyWrite, y mientras lo tenga abierto nadie mas puede
// abrirlo en exclusiva: en Windows lo impide el modo de comparticion, y fuera el cerrojo con que el
// RTL lo emula. Es la senal de que el proceso que lo escribe sigue vivo.
function OpenedByAnother(const Path: string): Boolean;
begin
  try
    TFileStream.Create(Path, fmOpenReadWrite or fmShareExclusive).Free;
    Result := False;
  except
    on EFOpenError do
      Result := True;
  end;
end;

// Crea el fichero vacio y, en Windows, le fija la fecha de creacion de ahora. NTFS da a un fichero que
// se crea con el nombre de otro borrado o renombrado hace unos segundos la fecha de creacion de aquel
// (tunneling), y una rotacion es justo eso: sin fijarla, el fichero nuevo seria tan viejo como el que se
// acaba de archivar. Se fija con el fichero cerrado, porque SetCreationTime necesita abrirlo para
// escribir. Si no se puede fijar, el fichero sirve igual.
procedure CreateStartingNow(const Path: string);
begin
  TFileStream.Create(Path, fmCreate or fmShareDenyWrite).Free;
  {$IFDEF MSWINDOWS}
  try
    TFile.SetCreationTime(Path, Now);
  except
    on Exception do ;
  end;
  {$ENDIF}
end;

// La fecha de comienzo de un fichero que ya tenia contenido: la de creacion, y donde el sistema no la
// conserva, la de ultima escritura. Fuera de Windows, macOS e iOS, GetCreationTime devuelve st_ctime,
// la del ultimo cambio de estado, que se mueve con cada escritura.
function StartDateOnDisk(const Path: string): TDateTime;
begin
  {$IF defined(MSWINDOWS) or defined(MACOS)}
  Result := TFile.GetCreationTime(Path);
  {$ELSE}
  Result := TFile.GetLastWriteTime(Path);
  {$ENDIF}
end;

procedure ReplaceFile(const Source, Dest: string);
begin
  {$IFDEF MSWINDOWS}
  if not MoveFileEx(PChar(Source), PChar(Dest), MOVEFILE_REPLACE_EXISTING) then
    RaiseLastOSError;
  {$ELSE}
  if not RenameFile(Source, Dest) then
    RaiseLastOSError;
  {$ENDIF}
end;

// Mueve el fichero a su archivo, o, si el archivo de ese periodo ya existe, lo copia al final de este y
// lo borra: el nombre ocupado es el mismo periodo, como en el ArchiveFileAppendExisting de NLog, y
// moverlo encima fallaria.
procedure ArchiveFile(const Path, ArchivePath: string);
begin
  if not TFile.Exists(ArchivePath) then
  begin
    TFile.Move(Path, ArchivePath);
    Exit;
  end;
  var Source := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    var Dest := TFileStream.Create(ArchivePath, fmOpenWrite or fmShareDenyWrite);
    try
      Dest.Seek(0, soEnd);
      Dest.CopyFrom(Source, 0);
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
  TFile.Delete(Path);
end;

// Lo mismo dentro de un .zip que ya existe. Una entrada no se amplia en sitio y zmWrite sobrescribe el
// comprimido, asi que se escribe entero en un temporal, con la entrada seguida del fichero, y solo
// entonces sustituye al anterior.
procedure AppendIntoZip(const ZipPath, Entry, SourcePath: string);
begin
  var Tmp := ZipPath + '.tmp';
  var Source := TFile.ReadAllBytes(SourcePath);
  var Old := TZipFile.Create;
  var New := TZipFile.Create;
  try
    Old.Open(ZipPath, zmRead);
    New.Open(Tmp, zmWrite);
    var Appended := False;
    for var I := 0 to Old.FileCount - 1 do
    begin
      var Bytes: TBytes;
      Old.Read(I, Bytes);
      var Name := Old.FileName[I];
      if SameText(Name, Entry) then
      begin
        Bytes := Bytes + Source;
        Appended := True;
      end;
      New.Add(Bytes, Name);
    end;
    if not Appended then
      New.Add(Source, Entry);
    New.Close;
    Old.Close;
  finally
    New.Free;
    Old.Free;
  end;
  ReplaceFile(Tmp, ZipPath);
end;

{ TRotationStrategy }

class function TRotationStrategy.BuildRotatePath(const Path: string; const FileDate: TDateTime): string;
begin
  // GetExtension devuelve la extension CON su punto.
  var Ext := TPath.GetExtension(Path);
  var Dir := TPath.GetDirectoryName(Path);
  // La marca es la del fichero que se archiva y no la del instante de rotar: una rotacion diaria
  // que salta pasada la medianoche archiva el log del dia anterior, y quien lo busque por su nombre
  // busca ese dia. Las cinco estrategias ya calculan esa fecha y la pasan aqui.
  var FName := TPath.GetFileNameWithoutExtension(Path) + '_' + FormatDateTime('yyyy-mm-dd_hhnnsszzz', FileDate);
  Result := TPath.Combine(Dir, FName) + Ext;
end;

{ TRotationNone }

class function TRotationNone.NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean;
begin
  Result := False;
end;

{ TRotationHourly }

class function TRotationHourly.NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean;
begin
  var FileDate := Target.StartedAt(Path);
  if (Trunc(FileDate) <> Trunc(Now)) or (HourOf(FileDate) <> HourOf(Now)) then
  begin
    RotatedPath := BuildRotatePath(Path, FileDate);
    Exit(True);
  end;
  Result := False;
end;

{ TRotationDaily }

class function TRotationDaily.NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean;
begin
  var FileDate := Target.StartedAt(Path);
  if Trunc(Now) > Trunc(FileDate) then
  begin
    RotatedPath := BuildRotatePath(Path, FileDate);
    Exit(True);
  end;
  Result := False;
end;

{ TRotationDays }

class function TRotationDays.NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean;
begin
  var FileDate := Target.StartedAt(Path);
  if Trunc(Now - FileDate) >= Target.MaxFileAge then
  begin
    RotatedPath := BuildRotatePath(Path, FileDate);
    Exit(True);
  end;
  Result := False;
end;

{ TRotationMonthly }

class function TRotationMonthly.NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean;
begin
  var FileDate := Target.StartedAt(Path);
  if (MonthOf(FileDate) <> MonthOf(Now)) or (YearOf(FileDate) <> YearOf(Now)) then
  begin
    RotatedPath := BuildRotatePath(Path, FileDate);
    Exit(True);
  end;
  Result := False;
end;

 { TRotationSize }

class function TRotationSize.NeedRotate(const Target: TFileTarget; const Path: string; out RotatedPath: string): Boolean;
begin
  var FileDate := Target.StartedAt(Path);
  if TFile.GetSize(Path) >= Target.MaxFileSize then
  begin
    RotatedPath := BuildRotatePath(Path, FileDate);
    Exit(True);
  end;
  Result := False;
end;

{ TFileTarget }

constructor TFileTarget.Create;
begin
  inherited Create;
  FAutoFlush := True;
  BuildOwnPatterns;
end;

destructor TFileTarget.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TFileTarget.DoWrite(const Entry: TLogEntry);
begin
  var Path := ResolveFileName(Entry);
  EnsureStreamFor(Path);
  var Line := RenderEvent(Entry);
  FStream.WriteLine(Line);
end;

procedure TFileTarget.EnsureStreamFor(const Path: string);
begin
  var Rotated := RotateCurrentFileIfNeeded(Path);
  if Rotated then
    FCurrentPath := EmptyStr; // hace falta un stream nuevo

  // Las DOS cosas. Con solo la ruta, un destino que quedo sin stream --por una apertura que fallo o
  // por una rotacion que lanzo-- sale por aqui y deriva nil en la linea siguiente, y ya nunca vuelve
  // a intentar abrir: el estado es absorbente y se lleva por delante todo el log del proceso.
  if (FCurrentPath = Path) and Assigned(FStream) then
    Exit;

  ForceDirectories(TPath.GetDirectoryName(Path));
  // Una vez por proceso y otra por rotacion propia; no en cada reintento de una apertura que falla.
  if Rotated or not FOtherRunsSwept then
  begin
    FOtherRunsSwept := True;
    try
      ArchiveOtherRuns(Path);
    except
      on E: Exception do
        BootstrapLog.LoggerFor(ClassName).LogError(E,
          Format('el destino "%s" no pudo revisar los ficheros de otras ejecuciones', [DisplayName]));
    end;
  end;

  var Created := not FileExists(Path);
  if Created then
    CreateStartingNow(Path);
  var FileStream := TFileStream.Create(Path, fmOpenWrite or fmShareDenyWrite);

  var NewWriter: TStreamWriter;
  var Started: TDateTime;
  try
    // Reabrir el mismo fichero tras un archivado que fallo conserva la fecha que ya tenia.
    if Created or (FileStream.Size = 0) then
      Started := Now
    else if SameFileName(Path, FStartedPath) then
      Started := FStartedAt
    else
      Started := StartDateOnDisk(Path);
    FileStream.Seek(0, soEnd);
    NewWriter := TStreamWriter.Create(FileStream);
  except
    FileStream.Free;
    raise;
  end;
  NewWriter.OwnStream;
  NewWriter.AutoFlush := FAutoFlush;

  // Se publica al final, cuando ya no puede fallar: hasta aqui el objeto conserva el stream que
  // tenia, asi que una apertura fallida no se lleva por delante el destino que estaba sirviendo.
  FreeAndNil(FStream);
  FStream := NewWriter;
  FCurrentPath := Path;
  FStartedPath := Path;
  FStartedAt := Started;
end;

function TFileTarget.StartedAt(const Path: string): TDateTime;
begin
  if SameFileName(Path, FStartedPath) then
    Result := FStartedAt
  else
    Result := StartDateOnDisk(Path);
end;

function TFileTarget.RotateCurrentFileIfNeeded(const Path: string): Boolean;
var
  RotatedPath: string;
begin
  Result := False;
  if not TFile.Exists(Path) then
    Exit;

  if not StrategyOf[RotationPolicy].NeedRotate(Self, Path, RotatedPath) then Exit;
  var HistoryDir := TPath.Combine(TPath.GetDirectoryName(Path), 'history');
  ForceDirectories(HistoryDir);
  RotatedPath := TPath.Combine(HistoryDir, TPath.GetFileName(RotatedPath));
  Result := MoveToHistory(Path, RotatedPath);
end;

function TFileTarget.MoveToHistory(const Path: string; const RotatedPath: string): Boolean;
begin
  Result := False;
  // Soltar el fichero es obligado para moverlo: el archivo conserva el nombre del original, asi que
  // no hay un segundo fichero que abrir antes. Lo que NO es obligado es que archivar pueda costar la
  // linea que provoco la rotacion -- comprimir un log de varios megas tarda, y limpiar el historico
  // falla si otro tiene un archivo abierto. Ninguna de las dos es parte de escribir esa linea.
  FreeAndNil(FStream);
  try
    ArchiveFile(Path, RotatedPath);
  except
    on E: Exception do
    begin
      // Sin rotar. El llamante reabre el mismo fichero y la linea se escribe igual, al final del
      // que ya habia, y la escritura siguiente lo vuelve a intentar.
      if not FArchiveFailing then
        BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" no pudo archivar su fichero', [DisplayName]));
      FArchiveFailing := True;
      Exit;
    end;
  end;
  FArchiveFailing := False;
  Result := True;

  try
    CompressIfAsked(RotatedPath);
    CleanupHistory(TPath.GetDirectoryName(RotatedPath));
  except
    on E: Exception do
      BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" no pudo comprimir o limpiar el historico tras rotar su fichero', [DisplayName]));
  end;
end;

procedure TFileTarget.CompressIfAsked(const RotatedPath: string);
begin
  if not CompressOnRotate then
    Exit;
  var ZipPath := RotatedPath + '.zip';
  var Entry := TPath.GetFileName(RotatedPath);
  if TFile.Exists(ZipPath) then
    AppendIntoZip(ZipPath, Entry, RotatedPath)
  else
  begin
    var Zip := TZipFile.Create;
    try
      Zip.Open(ZipPath, zmWrite);
      Zip.Add(RotatedPath, Entry);
      Zip.Close;
    finally
      Zip.Free;
    end;
  end;
  TFile.Delete(RotatedPath);
end;

procedure TFileTarget.ArchiveOtherRuns(const Path: string);
begin
  if not FOtherRunsFindable or (RotationPolicy = TRotationPolicy.None) then
    Exit;
  var Dir := TPath.GetDirectoryName(TPath.GetFullPath(Path));
  var OwnName := TPath.GetFileName(Path);
  var HistoryDir := TPath.Combine(Dir, 'history');
  var Archived := False;
  for var Candidate in TDirectory.GetFiles(Dir) do
  begin
    var Name := TPath.GetFileName(Candidate);
    if SameText(Name, OwnName) or not FOtherRunPattern.IsMatch(Name) then
      Continue;
    var RotatedPath: string;
    var Moved := False;
    try
      Moved := MoveOtherRun(Candidate, HistoryDir, RotatedPath);
    except
      on E: Exception do
        BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" no pudo archivar "%s"', [DisplayName, Name]));
    end;
    if not Moved then
      Continue;
    Archived := True;
    try
      CompressIfAsked(RotatedPath);
    except
      on E: Exception do
        BootstrapLog.LoggerFor(ClassName).LogError(E,
          Format('el destino "%s" no pudo comprimir "%s"', [DisplayName, TPath.GetFileName(RotatedPath)]));
    end;
  end;
  if Archived then
    try
      CleanupHistory(HistoryDir);
    except
      on E: Exception do
        BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" no pudo limpiar el historico', [DisplayName]));
    end;
end;

function TFileTarget.MoveOtherRun(const Candidate, HistoryDir: string; out RotatedPath: string): Boolean;
begin
  Result := False;
  try
    if not StrategyOf[RotationPolicy].NeedRotate(Self, Candidate, RotatedPath) then
      Exit;
  except
    // Otro proceso lo archivo entre el listado y la pregunta.
    if not TFile.Exists(Candidate) then
      Exit;
    raise;
  end;
  if OpenedByAnother(Candidate) then
    Exit;
  ForceDirectories(HistoryDir);
  // El mismo nombre que le da la rotacion de su propio proceso.
  RotatedPath := TPath.Combine(HistoryDir, TPath.GetFileName(RotatedPath));
  try
    ArchiveFile(Candidate, RotatedPath);
  except
    // Otro proceso lo archivo, o lo abrio, entre la comprobacion y el movimiento.
    if not TFile.Exists(Candidate) or OpenedByAnother(Candidate) then
      Exit;
    raise;
  end;
  Result := True;
end;

procedure TFileTarget.CleanupHistory(const HistoryDir: string);
type
  TFileInfo = record
    Path: string;
    Date: TDateTime;
  end;
begin
  if MaxArchivedFiles <= 0 then Exit;
  if not TDirectory.Exists(HistoryDir) then Exit;

  // Otro proceso puede archivar o borrar mientras tanto: un fichero que desaparece no es un fallo.
  var FileInfos: TArray<TFileInfo>;
  for var F in TDirectory.GetFiles(HistoryDir) do
  begin
    if not FArchivePattern.IsMatch(TPath.GetFileName(F)) then
      Continue;
    var Info: TFileInfo;
    Info.Path := F;
    var Dated := False;
    try
      Info.Date := TFile.GetLastWriteTime(F);
      Dated := True;
    except
      if TFile.Exists(F) then
        raise;
    end;
    if Dated then
      FileInfos := FileInfos + [Info];
  end;
  if Length(FileInfos) <= MaxArchivedFiles then Exit;

  TArray.Sort<TFileInfo>(FileInfos,
    TComparer<TFileInfo>.Construct(
      function(const L, R: TFileInfo): Integer
      begin
        Result := CompareDateTime(L.Date, R.Date);
      end));

  for var I := 0 to Length(FileInfos) - MaxArchivedFiles - 1 do
    try
      TFile.Delete(FileInfos[I].Path);
    except
      if TFile.Exists(FileInfos[I].Path) then
        raise;
    end;
end;

procedure TFileTarget.SetFileName(const Value: string);
begin
  FFileName := Trim(Value);
  BuildOwnPatterns;
end;

const
  MarkerExpr = '\$\{(.*?)\}';
  // La que pone TRotationStrategy.BuildRotatePath entre el nombre y la extension.
  RotationMarkExpr = '_\d{4}-\d{2}-\d{2}_\d{9}';

// El texto literal, escapado; ${pid}, digitos; cualquier otro marcador, lo que sea.
function MarkersToRegex(const Text: string): string;
begin
  Result := EmptyStr;
  var Done := 0;
  for var Match in TRegEx.Matches(Text, MarkerExpr) do
  begin
    var Start := Match.Index - 1;
    Result := Result + TRegEx.Escape(Text.Substring(Done, Start - Done));
    var Marker := Match.Groups[1].Value;
    if Marker.Contains(':') then
      Marker := Marker.Substring(0, Marker.IndexOf(':'));
    if SameText(Marker, 'pid') then
      Result := Result + '\d+'
    else
      Result := Result + '.+?';
    Done := Start + Match.Length;
  end;
  Result := Result + TRegEx.Escape(Text.Substring(Done));
end;

// El punto de la extension es el ultimo que queda fuera de los marcadores: ${date:yyyy.MM} no tiene.
function ExtensionDot(const Name: string): Integer;
begin
  var Masked := Name;
  for var Match in TRegEx.Matches(Name, MarkerExpr) do
    Masked := Masked.Remove(Match.Index - 1, Match.Length).Insert(Match.Index - 1, StringOfChar('_', Match.Length));
  Result := Masked.LastIndexOf('.');
end;

procedure TFileTarget.BuildOwnPatterns;
begin
  FOtherRunsFindable := False;
  var Normalized := FFileName.Replace('/', PathDelim);
  // Solo el nombre: los ficheros se buscan en la carpeta ya resuelta del fichero propio.
  var Name := Normalized.Substring(Normalized.LastDelimiter(PathDelim) + 1);
  var Stem := Name;
  var Ext := EmptyStr;
  var Dot := ExtensionDot(Name);
  if Dot >= 0 then
  begin
    Stem := Name.Substring(0, Dot);
    Ext := Name.Substring(Dot);
  end;
  var StemExpr := MarkersToRegex(Stem);
  var ExtExpr := MarkersToRegex(Ext);
  FArchivePattern := TRegEx.Create('\A' + StemExpr + RotationMarkExpr + ExtExpr + '(\.zip)?\z', [roIgnoreCase]);

  if not Name.Contains('${') then
    Exit;
  // Sin texto literal, el patron cubriria ficheros que no son del destino: ${date}.log, cualquier .log.
  if TRegEx.Replace(Stem, MarkerExpr, EmptyStr).IsEmpty then
    Exit;
  FOtherRunPattern := TRegEx.Create('\A' + StemExpr + ExtExpr + '\z', [roIgnoreCase]);
  FOtherRunsFindable := True;
end;

function TFileTarget.ResolveFileName(const Entry: TLogEntry): string;
begin
  Result := TLogLayoutEngine.ResolveLayout(FileName, Entry).Replace('/', PathDelim);
end;

end.

