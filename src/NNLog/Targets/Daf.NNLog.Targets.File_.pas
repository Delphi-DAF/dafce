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
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Write(const Entry: TLogEntry); override;
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
  var Ext := TPath.GetExtension(Path);
  var Dir := TPath.GetDirectoryName(Path);
  var FName := TPath.GetFileNameWithoutExtension(Path) + '_' + FormatDateTime('yyyy-mm-dd_hhnnsszzz', Now);
  Result := TPath.Combine(Dir, FName) + '.' + Ext;
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

procedure TFileTarget.Write(const Entry: TLogEntry);
begin
  var Path := ResolveFileName(Entry);
  EnsureStreamFor(Path);
  var Line := RenderEvent(Entry);
  FStream.WriteLine(Line);
end;

procedure TFileTarget.EnsureStreamFor(const Path: string);
begin
  if  RotateCurrentFileIfNeeded(Path) then
    FCurrentPath := ''; // we  need a new stream

  if (FCurrentPath = Path) then Exit;
  var FileStream: TFileStream := nil;
  try
    FCurrentPath := Path;
    FreeAndNil(FStream);

    ForceDirectories(TPath.GetDirectoryName(FCurrentPath));
    if FileExists(FCurrentPath) then
      FileStream := TFileStream.Create(FCurrentPath, fmOpenWrite or fmShareDenyWrite)
    else
      FileStream := TFileStream.Create(FCurrentPath, fmCreate or fmShareDenyWrite);
    FileStream.Seek(0, soEnd);

    FStream := TStreamWriter.Create(FileStream);
    FStream.OwnStream;
    FStream.AutoFlush := FAutoFlush;
  except
    on E: Exception do
    begin
      FileStream.Free;
      raise;
    end;
  end;
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
  FreeAndNil(FStream);
  TFile.Move(Path, RotatedPath);
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

