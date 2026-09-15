unit BootstrapSinks.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging;

type
  /// Los destinos de verdad escriben fuera del proceso, asi que aqui hay que ir a mirar donde
  /// escriben: un fichero temporal para el destino duradero, y la salida de error redirigida para
  /// el de consola.
  TSinksWorld = class
  strict private
    FSaved: TArray<IBootstrapSink>;
    FFolder: string;
  public
    LogFile: string;
    Captured: string;
    PlatformSink: IBootstrapSink;
    PlatformAvailable: Boolean;
    /// Lo que cada escenario pregunta, guardado entre el When que lo pregunta y el Then que lo
    /// describe.
    Found: Boolean;
    MissingPath: string;
    FileSink: IBootstrapSink;
    /// Texto y no Boolean a proposito donde lo esperado es False: un campo sin asignar YA vale
    /// False, asi que un ToBeFalse pasaria igual si el When no llegara a correr.
    ChannelAvailable: string;
    FileReport: string;
    constructor Create;
    destructor Destroy; override;
    function DefaultSinks: TArray<IBootstrapSink>;
    procedure OnlySinks(const Sinks: array of IBootstrapSink);
    /// Una carpeta que solo conoce este escenario: es el papel que en una aplicacion de verdad
    /// hace la carpeta de datos, la que el framework no puede adivinar.
    function Folder: string;
    function LogFileText: string;
    procedure CaptureStdErr(const Proc: TProc);
  end;

implementation

uses
  System.IOUtils,
  Daf.FMX.BootstrapSink,
  Daf.MiniSpec;

{ TSinksWorld }

constructor TSinksWorld.Create;
begin
  inherited Create;
  FSaved := BootstrapLog.Sinks;
end;

destructor TSinksWorld.Destroy;
begin
  BootstrapLog.ClearSinks;
  for var Sink in FSaved do
    BootstrapLog.AddSink(Sink);
  if (FFolder <> '') and TDirectory.Exists(FFolder) then
    TDirectory.Delete(FFolder, True);
  inherited;
end;

function TSinksWorld.DefaultSinks: TArray<IBootstrapSink>;
begin
  Result := FSaved;
end;

procedure TSinksWorld.OnlySinks(const Sinks: array of IBootstrapSink);
begin
  BootstrapLog.ClearSinks;
  for var Sink in Sinks do
    BootstrapLog.AddSink(Sink);
end;

function TSinksWorld.Folder: string;
begin
  if FFolder = '' then
  begin
    FFolder := TPath.Combine(TPath.GetTempPath, 'daf-bootstrap-' + TGuid.NewGuid.ToString);
    TDirectory.CreateDirectory(FFolder);
    LogFile := TPath.Combine(FFolder, 'startup.log');
  end;
  Result := FFolder;
end;

function TSinksWorld.LogFileText: string;
begin
  if (LogFile = '') or not TFile.Exists(LogFile) then
    Exit('');
  Result := TFile.ReadAllText(LogFile, TEncoding.UTF8);
end;

procedure TSinksWorld.CaptureStdErr(const Proc: TProc);
begin
  var Target := TPath.Combine(TPath.GetTempPath, 'daf-stderr-' + TGuid.NewGuid.ToString + '.txt');
  var Saved: TTextRec;
  Move(TTextRec(ErrOutput), Saved, SizeOf(TTextRec));
  try
    AssignFile(ErrOutput, Target);
    Rewrite(ErrOutput);
    try
      Proc;
    finally
      CloseFile(ErrOutput);
    end;
    Captured := TFile.ReadAllText(Target);
  finally
    Move(Saved, TTextRec(ErrOutput), SizeOf(TTextRec));
    if TFile.Exists(Target) then
      TFile.Delete(Target);
  end;
end;

initialization

Feature('''
Feature Startup destinations do not have to be guessed @logging @bootstrap @sinks

  As someone deploying an application
  I want the startup report to reach a place that does not need a debugger attached
  So that an unattended process that dies at startup still says why
''')

.UseWorld<TSinksWorld>

.Rule('DAF only brings destinations whose place it does not have to guess')

  .Scenario('On a console the reason comes out through the error output')
    .Given('a process with an error output available',
      procedure(W: TSinksWorld)
      begin
        var StdErr: IBootstrapSink := TStdErrBootstrapSink.Create;
        Expect(StdErr.IsAvailable).ToBeTrue;
        W.OnlySinks([StdErr]);
      end)
    .When('its startup fails, with no debugger in the picture',
      procedure(W: TSinksWorld)
      begin
        W.CaptureStdErr(
          procedure
          begin
            BootstrapLog.LoggerFor('DAF').LogError('cannot parse json in file "appsettings.json"');
          end);
      end)
    .&Then('the reason came out through there',
      procedure(W: TSinksWorld)
      begin
        Expect(W.Captured).ToContain('cannot parse json in file "appsettings.json"');
        Expect(W.Captured).ToContain('] DAF |');
      end)

  .Scenario('The debug output destination is kept')
    // Nada que montar: lo que el escenario mide es el juego de destinos con el que arranco el
    // proceso, que el World guardo antes de que ningun escenario tocara nada.
    .Given('los destinos con los que arranco el proceso').NoAction
    .When('se buscan entre ellos la salida de depuracion del sistema',
      procedure(W: TSinksWorld)
      begin
        W.Found := False;
        for var Sink in W.DefaultSinks do
          W.Found := W.Found or ((Sink as TObject) is TDebugOutputBootstrapSink);
      end)
    .&Then('the process starts with the system debug output among its destinations',
      procedure(W: TSinksWorld)
      begin
        Expect(W.Found).ToBeTrue;
      end)

  .Scenario('The platform is the one that answers whether a destination can write')
    .Given('the destination that leans on the platform log service',
      procedure(W: TSinksWorld)
      begin
        // Preguntar no puede lanzar en ninguna plataforma, tenga o no servicio que responder.
        W.PlatformSink := TPlatformLogBootstrapSink.Create;
        W.PlatformAvailable := W.PlatformSink.IsAvailable;
        W.OnlySinks([W.PlatformSink]);
      end)
    .When('se le pregunta al canal si tiene algun destino disponible',
      procedure(W: TSinksWorld)
      begin
        W.ChannelAvailable := BoolToStr(BootstrapLog.HasAvailableSink, True);
      end)
    .&Then('what the channel reports as available is what the platform answered',
      procedure(W: TSinksWorld)
      begin
        Expect(W.ChannelAvailable).ToEqual(BoolToStr(W.PlatformAvailable, True));
      end)
    .&And('where it has no service to answer, it does not take the report and lose it',
      procedure(W: TSinksWorld)
      begin
        ExpectException(
          procedure
          begin
            BootstrapLog.LoggerFor('DAF').LogError('host could not be built');
          end).ToBeNone;
      end)

  .Scenario('The framework does not choose where to write')
    .Given('no application has registered a durable destination',
      procedure(W: TSinksWorld)
      begin
        W.OnlySinks(W.DefaultSinks);
      end)
    .When('a startup failure is reported',
      procedure(W: TSinksWorld)
      begin
        W.CaptureStdErr(
          procedure
          begin
            BootstrapLog.LoggerFor('DAF').LogError('host could not be built');
          end);
      end)
    .&Then('no file destination came with the framework',
      procedure(W: TSinksWorld)
      begin
        var Found := False;
        for var Sink in W.DefaultSinks do
          Found := Found or ((Sink as TObject) is TFileBootstrapSink);
        Expect(Found).ToBeFalse;
      end)
    .&And('no file was written next to the executable',
      procedure(W: TSinksWorld)
      begin
        Expect(TFile.Exists(TPath.ChangeExtension(ParamStr(0), '.log'))).ToBeFalse;
        Expect(TFile.Exists(TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'startup.log'))).ToBeFalse;
      end)

.Rule('Whoever deploys adds their own durable destination')

  .Scenario('The reports, the ones from DAF and the ones of its own, arrive there too')
    .Given('an application that registers a file destination with a path it knows',
      procedure(W: TSinksWorld)
      begin
        var FileSink: IBootstrapSink := TFileBootstrapSink.Create(TPath.Combine(W.Folder, 'startup.log'));
        Expect(FileSink.IsAvailable).ToBeTrue;
        W.OnlySinks([FileSink]);
      end)
    .When('both DAF and the application report during the startup',
      procedure(W: TSinksWorld)
      begin
        BootstrapLog.LoggerFor('DAF').LogError('host could not be built');
        BootstrapLog.LoggerFor('Acme.Payroll').LogError('data folder is not writable');
      end)
    .&Then('the file holds both reports',
      procedure(W: TSinksWorld)
      begin
        Expect(W.LogFileText).ToContain('host could not be built');
        Expect(W.LogFileText).ToContain('data folder is not writable');
      end)

  .Scenario('A destination pointing nowhere declares itself unavailable')
    .Given('un destino de fichero que apunta bajo una carpeta que no existe',
      procedure(W: TSinksWorld)
      begin
        W.MissingPath := TPath.Combine(TPath.Combine(W.Folder, 'not-there'), 'startup.log');
        W.FileSink := TFileBootstrapSink.Create(W.MissingPath);
      end)
    .When('se le pregunta si puede escribir',
      procedure(W: TSinksWorld)
      begin
        W.FileReport := Format('available=%s folder=%s',
          [BoolToStr(W.FileSink.IsAvailable, True),
           BoolToStr(TDirectory.Exists(TPath.GetDirectoryName(W.MissingPath)), True)]);
      end)
    .&Then('a file destination under a folder that does not exist says so, instead of creating it',
      procedure(W: TSinksWorld)
      begin
        Expect(W.FileReport).ToEqual('available=False folder=False');
      end);

end.
