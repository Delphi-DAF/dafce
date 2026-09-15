unit Daf.NNLog.Targets.File_;

interface

uses
  System.Classes,
  System.DateUtils,
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
    procedure EnsureStreamFor(const Path: string);
    function ResolveFileName(const Entry: TLogEntry): string;
    procedure SetFileName(const Value: string);
    procedure CleanupHistory(const HistoryDir: string);
    procedure MoveToHistory(const Path, RotatedPath: string);
    function RotateCurrentFileIfNeeded(const Path: string): Boolean;
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

{ TRotationStrategy }

class function TRotationStrategy.BuildRotatePath(const Path: string; const FileDate: TDateTime): string;
begin
  // GetExtension devuelve la extension CON su punto, asi que concatenar otro daba "app_....log".
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
  var FileDate := TFile.GetCreationTime(Path);
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
  var FileDate := TFile.GetCreationTime(Path);
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
  var FileDate := TFile.GetCreationTime(Path);
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
  var FileDate := TFile.GetCreationTime(Path);
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
  var FileDate := TFile.GetCreationTime(Path);
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
  if RotateCurrentFileIfNeeded(Path) then
    FCurrentPath := EmptyStr; // hace falta un stream nuevo

  // Las DOS cosas. Con solo la ruta, un destino que quedo sin stream --por una apertura que fallo o
  // por una rotacion que lanzo-- sale por aqui y deriva nil en la linea siguiente, y ya nunca vuelve
  // a intentar abrir: el estado es absorbente y se lleva por delante todo el log del proceso.
  if (FCurrentPath = Path) and Assigned(FStream) then
    Exit;

  ForceDirectories(TPath.GetDirectoryName(Path));
  var FileStream: TFileStream;
  if FileExists(Path) then
    FileStream := TFileStream.Create(Path, fmOpenWrite or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(Path, fmCreate or fmShareDenyWrite);

  var NewWriter: TStreamWriter;
  try
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
end;

function TFileTarget.RotateCurrentFileIfNeeded(const Path: string): Boolean;
var
  RotatedPath: string;
begin
  Result := False;
  if not TFile.Exists(Path) then
    Exit;

  if not StrategyOf[RotationPolicy].NeedRotate(Self, Path, RotatedPath) then Exit;
  Result := True;
  // Ajustar ruta de histórico
  var HistoryDir := TPath.Combine(TPath.GetDirectoryName(Path), 'history');
  ForceDirectories(HistoryDir);
  RotatedPath := TPath.Combine(HistoryDir, TPath.GetFileName(RotatedPath));
  MoveToHistory(Path, RotatedPath);
end;

procedure TFileTarget.MoveToHistory(const Path: string; const RotatedPath: string);
begin
  // Soltar el fichero es obligado para moverlo: el archivo conserva el nombre del original, asi que
  // no hay un segundo fichero que abrir antes. Lo que NO es obligado es que archivar pueda costar la
  // linea que provoco la rotacion -- comprimir un log de varios megas tarda, y limpiar el historico
  // falla si otro tiene un archivo abierto. Ninguna de las dos es parte de escribir esa linea.
  FreeAndNil(FStream);
  try
    TFile.Move(Path, RotatedPath);
  except
    on E: Exception do
    begin
      // Sin rotar. El llamante reabre el mismo fichero y la linea se escribe igual, al final del
      // que ya habia: peor que rotar, mejor que perderla y quedarse sin destino.
      BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" no pudo archivar su fichero', [Name]));
      Exit;
    end;
  end;

  try
    if CompressOnRotate then
    begin
      var Zip := TZipFile.Create;
      try
        Zip.Open(RotatedPath + '.zip', zmWrite);
        Zip.Add(RotatedPath, TPath.GetFileName(RotatedPath));
        Zip.Close;
      finally
        Zip.Free;
      end;
      TFile.Delete(RotatedPath);
    end;
    CleanupHistory(TPath.GetDirectoryName(RotatedPath));
  except
    on E: Exception do
      BootstrapLog.LoggerFor(ClassName).LogError(E, Format('el destino "%s" roto, pero no pudo comprimir o limpiar el historico', [Name]));
  end;
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

  var Files := TDirectory.GetFiles(HistoryDir);
  if Length(Files) <= MaxArchivedFiles then Exit;

  var FileInfos: TArray<TFileInfo>;
  SetLength(FileInfos, Length(Files));
for var I := 0 to High(Files) do
  begin
    FileInfos[I].Path := Files[I];
    FileInfos[I].Date := TFile.GetLastWriteTime(Files[I]);
  end;

  TArray.Sort<TFileInfo>(FileInfos,
    TComparer<TFileInfo>.Construct(
      function(const L, R: TFileInfo): Integer
      begin
        Result := CompareDateTime(L.Date, R.Date);
      end));

  for var I := 0 to Length(FileInfos) - MaxArchivedFiles - 1 do
    TFile.Delete(FileInfos[I].Path);
end;

procedure TFileTarget.SetFileName(const Value: string);
begin
  FFileName := Trim(Value);
end;

function TFileTarget.ResolveFileName(const Entry: TLogEntry): string;
begin
  Result := TLogLayoutEngine.ResolveLayout(FileName, Entry).Replace('/', PathDelim);
end;

end.

