unit HostedWorker;

interface

uses
  System.SysUtils,
  System.DateUtils,
  System.Threading,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Threading;

type
  THostedWorker = class(TInterfacedObject, IHostedService)
  private
    FLifetime: IHostApplicationLifetime;
    FLogger: ILogger;
    FTask: ITask;
    procedure DoWork(const Token: ICancellationToken);
    procedure HandleException(E: Exception);
  public
    constructor Create(const LoggerFactory: ILoggerFactory; const Lifetime: IHostApplicationLifetime);
    procedure Start;
    procedure Stop;
    property Logger: ILogger read FLogger;
    property Lifetime: IHostApplicationLifetime read FLifetime;
  end;

implementation

uses
  System.SyncObjs,
  System.Classes;

{ THostedWorker }

constructor THostedWorker.Create(const LoggerFactory: ILoggerFactory; const Lifetime: IHostApplicationLifetime);
begin
  inherited Create;
  FLogger := LoggerFactory.CreateLogger(Self.ClassInfo);
  FLifetime := Lifetime;

  FLifetime.ApplicationStopping.Register(
    procedure
    begin
      FLogger.LogWarning('🚪 El host se está apagando, THostedWorker preparando su salida...');
    end);
end;

procedure THostedWorker.DoWork(const Token: ICancellationToken);
var
  Idx: Integer;
begin
  Idx := 0;
  with FLogger.BeginScope('working with guid {guid}', [TGUID.NewGuid.ToString]) do
  begin
    while not Token.IsCancellationRequested and (Token.WaitFor(1000) = wrTimeout) do
    begin
      Inc(Idx);
      FLogger.BeginScope('scope {number}', [Idx],
        procedure
        begin
          FLogger.LogInformation('I''m working at [%s] ...', [FormatDateTime('hh:nn:ss', Now)]);
          case Random(20) of
            0: raise Exception.Create('Simulated Error in ' + ClassName);
          end;
        end);
    end;
  end;
end;

procedure THostedWorker.HandleException(E: Exception);
begin
  FLogger.LogError(E, 'TTimeWorker raised!');
end;

procedure THostedWorker.Start;
begin
  FLogger.LogInformation('🟢 THostedWorker.Start called');

  FTask := TTask.Run(
    procedure
    begin
      try
        DoWork(FLifetime.ApplicationStopping);
        FLogger.LogInformation('✅ Servicio finalizó correctamente');
      except
        on E: Exception do
          HandleException(E);
      end;
    end);
end;

procedure THostedWorker.Stop;
begin
  FLogger.LogInformation('🔴 THostedWorker.Stop called');

  if Assigned(FTask) and not(FTask.Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception]) then
  begin
    try
      FLogger.LogInformation('✅ Esperando terminación del servicio');
      FTask.Wait;
    except
      on E: Exception do
        FLogger.LogError(E, '❗ Error esperando al servicio');
    end;
  end;
end;

end.

