unit DAF.Logging.Provider;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.TypInfo,
  System.Rtti,
  Daf.Extensions.Logging,
  Daf.Logging;

type
  TCompositeLogger = class(TLogger)
  private
    FLoggers: TArray<ILogger>;
    FLock: TObject;
  public
    constructor Create(const Category: string; const Loggers: TArray<ILogger>);
    destructor Destroy;override;
    procedure Log(const Entry: TLogEntry);override;
  end;

  TMultiProviderLoggerFactory = class(TInterfacedObject, ILoggerFactory)
  private
    FProviders: TList<ILoggerProvider>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddProvider(const Provider: ILoggerProvider);
    function HasProviders: Boolean;
    function CreateLogger(const Category: string): ILogger; overload;
    function CreateLogger(const AClass: TClass): ILogger; overload;
    function CreateLogger(const AType: PTypeInfo): ILogger; overload;
    function CreateLogger<T>: ILogger; overload;
  end;

implementation

{ TCompositeLogger }

constructor TCompositeLogger.Create(const Category: string; const Loggers: TArray<ILogger>);
begin
  inherited Create(Category);
  FLock := TObject.Create;
  FLoggers := Loggers;
end;

destructor TCompositeLogger.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TCompositeLogger.Log(const Entry: TLogEntry);
begin
  TMonitor.Enter(FLock);
  try
    try
      for var Logger in FLoggers do
        Logger.Log(Entry);
    except
      on E: EXception do
        BootstrapLog.LoggerFor(ClassName).LogError(E, 'a logger provider raised while writing an entry');
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

{ TMultiProviderLoggerFactory }

constructor TMultiProviderLoggerFactory.Create;
begin
  inherited Create;
  FProviders := TList<ILoggerProvider>.Create;
end;

destructor TMultiProviderLoggerFactory.Destroy;
begin
  FProviders.Free;
  inherited;
end;

procedure TMultiProviderLoggerFactory.AddProvider(const Provider: ILoggerProvider);
begin
  FProviders.Add(Provider);
end;

function TMultiProviderLoggerFactory.HasProviders: Boolean;
begin
  Result := FProviders.Count > 0;
end;

function TMultiProviderLoggerFactory.CreateLogger(const Category: string): ILogger;
var
  Loggers: TArray<ILogger>;
  I: Integer;
begin
  SetLength(Loggers, FProviders.Count);
  for I := 0 to FProviders.Count - 1 do
    Loggers[I] := FProviders[I].CreateLogger(Category);

  Result := TCompositeLogger.Create(Category, Loggers);
end;

function TMultiProviderLoggerFactory.CreateLogger(const AClass: TClass): ILogger;
begin
  Result := CreateLogger(AClass.ClassInfo);
end;

function TMultiProviderLoggerFactory.CreateLogger(const AType: PTypeInfo): ILogger;
begin
  Result := CreateLogger(LogCategoryOf(AType));
end;

function TMultiProviderLoggerFactory.CreateLogger<T>: ILogger;
begin
  Result := CreateLogger(TypeInfo(T));
end;

end.
