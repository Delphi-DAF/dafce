unit StandardStreams.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Threading,
  Winapi.Windows,
  Daf.MiniSpec,
  Daf.SystemProcess;

const
  // Muchas veces los 4 KB que admite una tuberia creada con el tamano por defecto.
  BigSize = 1024 * 1024;
  // Ejecuciones con la salida a un fichero mientras otro hilo lanza hijos.
  RaceRounds = 20;
  Enye = #$00F1;

function GetConsoleProcessList(lpdwProcessList: PDWORD; dwProcessCount: DWORD): DWORD; stdcall;
  external kernel32 name 'GetConsoleProcessList';

// Cuantos procesos hay conectados a la consola del runner.
function ConsoleProcesses: Integer;
var
  List: array [0 .. 63] of DWORD;
begin
  Result := GetConsoleProcessList(@List[0], Length(List));
end;

type
  TStreamsWorld = class
  private
    FDir: string;
    FRedirected: DWORD;
    FSaved: THandle;
    FParentFile: THandle;
    FSavedDir: string;
  public
    Builder: TBuilder;
    Proc: ISystemProcess;
    Result: TProcessResult;
    Captured: string;
    Report: string;
    ConsoleBefore: Integer;
    ConsoleMost: Integer;
    /// El hilo que lanza hijos sin pausa, y la orden de parar; espera a sus hijos antes de acabar.
    Launcher: TThread;
    [volatile] StopLaunching: Boolean;
    constructor Create;
    destructor Destroy; override;
    function PathOf(const Name: string): string;
    function WriteBig: string;
    function ReadText(const FileName: string): string;
    function Capture: TProcessOutputProc;
    /// El handle estandar del propio runner pasa a ser un fichero hasta RestoreParent. La RTL abrio
    /// Output al arrancar, asi que el informe del runner no se desvia; un hijo lanzado entre
    /// medias, si.
    procedure RedirectParent(StdHandle: DWORD; const FileName: string; ForReading: Boolean = False);
    procedure RestoreParent;
    /// El directorio actual del runner pasa a ser Dir hasta LeaveDir.
    procedure EnterDir(const Dir: string);
    procedure LeaveDir;
    function Status: string;
  end;

function StatusText(Status: TProcessResult.TStatus): string;
begin
  case Status of
    TProcessResult.TStatus.Failed:    Result := 'Failed';
    TProcessResult.TStatus.Timeout:   Result := 'Timeout';
    TProcessResult.TStatus.Running:   Result := 'Running';
    TProcessResult.TStatus.Completed: Result := 'Completed';
    TProcessResult.TStatus.Canceled:  Result := 'Canceled';
    TProcessResult.TStatus.Killed:    Result := 'Killed';
  else
    Result := '?';
  end;
end;

// Los caracteres como puntos de codigo: un fallo se lee igual en cualquier consola.
function CodePoints(const S: string): string;
begin
  Result := '';
  for var C in S do
    Result := Result + Format('U+%.4X ', [Ord(C)]);
  Result := Result.TrimRight;
end;

constructor TStreamsWorld.Create;
begin
  inherited Create;
  FDir := TPath.Combine(TPath.GetTempPath, 'daf-streams-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  TDirectory.CreateDirectory(FDir);
end;

destructor TStreamsWorld.Destroy;
begin
  if Assigned(Launcher) then
  begin
    StopLaunching := True;
    Launcher.WaitFor;
    Launcher.Free;
  end;
  RestoreParent;
  LeaveDir;
  if Assigned(Builder) then
  begin
    Proc := Builder.Build;
    Builder := nil;
  end;
  Proc := nil;
  try
    TDirectory.Delete(FDir, True);
  except
    on Exception do;
  end;
  inherited;
end;

function TStreamsWorld.PathOf(const Name: string): string;
begin
  Result := TPath.Combine(FDir, Name);
end;

function TStreamsWorld.WriteBig: string;
begin
  Result := PathOf('big.txt');
  TFile.WriteAllText(Result, StringOfChar('x', BigSize), TEncoding.ANSI);
end;

function TStreamsWorld.ReadText(const FileName: string): string;
begin
  Result := TFile.ReadAllText(FileName, TEncoding.ANSI).Trim;
end;

function TStreamsWorld.Capture: TProcessOutputProc;
begin
  Result :=
    procedure(Text: string)
    begin
      TMonitor.Enter(Self);
      try
        Captured := Captured + Text;
      finally
        TMonitor.Exit(Self);
      end;
    end;
end;

procedure TStreamsWorld.RedirectParent(StdHandle: DWORD; const FileName: string; ForReading: Boolean);
begin
  if ForReading then
    FParentFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0)
  else
    FParentFile := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL, 0);
  if FParentFile = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  FRedirected := StdHandle;
  FSaved := GetStdHandle(StdHandle);
  SetStdHandle(StdHandle, FParentFile);
end;

procedure TStreamsWorld.RestoreParent;
begin
  if FRedirected = 0 then
    Exit;
  SetStdHandle(FRedirected, FSaved);
  CloseHandle(FParentFile);
  FRedirected := 0;
end;

procedure TStreamsWorld.EnterDir(const Dir: string);
begin
  FSavedDir := GetCurrentDir;
  SetCurrentDir(Dir);
end;

procedure TStreamsWorld.LeaveDir;
begin
  if FSavedDir = '' then
    Exit;
  SetCurrentDir(FSavedDir);
  FSavedDir := '';
end;

function TStreamsWorld.Status: string;
begin
  Result := StatusText(Self.Result.Status);
end;

initialization

Feature('''
Feature Cada flujo del hijo va adonde se declara @systemprocess @process @streams

  Como quien lanza un proceso hijo desde una aplicacion DAF
  Quiero decidir adonde va cada uno de sus flujos
  Para que el hijo no se bloquee y lo que escribe llegue donde lo necesito
''')

.UseWorld<TStreamsWorld>

.Rule('Cada flujo de salida tiene exactamente un destino')

  .Scenario('Una salida capturada llega al manejador')
    .Given('un hijo que escribe hola, con la salida estandar capturada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .OnStdOut(W.Capture())
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe hola',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Captured.Trim).ToEqual('hola');
      end)

  .Scenario('Una salida descartada no llega a ninguna parte')
    .Given('la salida estandar del padre va a un fichero, y un hijo que escribe hola en la suya, descartada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .StandardOutput(TProcessOutput.Discard)
          .Timeout(10000)
          .Build;
        W.RedirectParent(STD_OUTPUT_HANDLE, W.PathOf('padre.txt'));
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Result := W.Proc.Execute;
        finally
          W.RestoreParent;
        end;
      end)
    .&Then('termina como Completed, y el fichero del padre sigue vacio',
      procedure(W: TStreamsWorld)
      begin
        Expect(Format('%s "%s"', [W.Status, W.ReadText(W.PathOf('padre.txt'))])).ToEqual('Completed ""');
      end)

  .Scenario('Una salida a fichero sustituye su contenido')
    .Given('un fichero que contiene antes, y un hijo que escribe hola con la salida estandar a ese fichero',
      procedure(W: TStreamsWorld)
      begin
        TFile.WriteAllText(W.PathOf('salida.txt'), 'antes', TEncoding.ANSI);
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .StandardOutput(TProcessOutput.ToFile(W.PathOf('salida.txt')))
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el fichero contiene hola y no antes',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.ReadText(W.PathOf('salida.txt'))).ToEqual('hola');
      end)

  .Scenario('Una salida a fichero puede anadirse al final')
    .Given('un fichero que contiene antes, y un hijo que escribe hola con la salida estandar anadida a ese fichero',
      procedure(W: TStreamsWorld)
      begin
        TFile.WriteAllText(W.PathOf('salida.txt'), 'antes' + sLineBreak, TEncoding.ANSI);
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .StandardOutput(TProcessOutput.ToFile(W.PathOf('salida.txt'), True))
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el fichero contiene antes seguido de hola',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.ReadText(W.PathOf('salida.txt'))).ToEqual('antes' + sLineBreak + 'hola');
      end)

  .Scenario('Las dos salidas pueden tener destinos distintos')
    .Given('un hijo que escribe en las dos salidas, con la estandar capturada y la de error descartada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo salida& echo error 1>&2')
          .OnStdOut(W.Capture())
          .StandardError(TProcessOutput.Discard)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe solo lo de la salida estandar',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Captured.Trim).ToEqual('salida');
      end)

.Rule('Un fichero de destino se abre antes de lanzar al hijo')

  .Scenario('Un fichero que no se puede abrir no lanza al hijo')
    .Given('un hijo que crearia una marca, con la salida estandar a un fichero de un directorio que no existe',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command(Format('cmd /c echo x> "%s"', [W.PathOf('marca.txt')]))
          .StandardOutput(TProcessOutput.ToFile(W.PathOf('no-existe\salida.txt')))
          .OnFailed(
            procedure(R: TProcessResult)
            begin
              W.Report := 'OnFailed';
            end)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('termina como Failed nombrando el fichero, llama a OnFailed y el hijo no se lanza',
      procedure(W: TStreamsWorld)
      begin
        Expect(Format('%s %s nombra=%s marca=%s', [W.Status, W.Report,
          BoolToStr(W.Result.LastError.Contains(W.PathOf('no-existe\salida.txt')), True),
          BoolToStr(TFile.Exists(W.PathOf('marca.txt')), True)]))
          .ToEqual('Failed OnFailed nombra=True marca=False');
      end)

  .Scenario('La ruta relativa es la del padre')
    .Given('el directorio actual del padre y el de trabajo del hijo son distintos, y la salida va a una ruta relativa',
      procedure(W: TStreamsWorld)
      begin
        TDirectory.CreateDirectory(W.PathOf('padre'));
        TDirectory.CreateDirectory(W.PathOf('hijo'));
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .WorkingDir(W.PathOf('hijo'))
          .StandardOutput(TProcessOutput.ToFile('relativa.txt'))
          .Timeout(10000)
          .Build;
        W.EnterDir(W.PathOf('padre'));
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Result := W.Proc.Execute;
        finally
          W.LeaveDir;
        end;
      end)
    .&Then('el fichero se crea en el directorio del padre',
      procedure(W: TStreamsWorld)
      begin
        Expect(Format('padre=%s hijo=%s', [
          BoolToStr(TFile.Exists(TPath.Combine(W.PathOf('padre'), 'relativa.txt')), True),
          BoolToStr(TFile.Exists(TPath.Combine(W.PathOf('hijo'), 'relativa.txt')), True)]))
          .ToEqual('padre=True hijo=False');
      end)

.Rule('Ningun volumen de salida impide que el hijo termine')

  .Scenario('Una salida grande descartada no bloquea al hijo')
    .Given('un hijo que escribe 1 MB en cada flujo, los dos descartados, con un plazo de 10 s',
      procedure(W: TStreamsWorld)
      begin
        var Big := W.WriteBig;
        W.Proc := TSystemProcess.Builder
          .Command(Format('cmd /c type "%s" & type "%s" 1>&2', [Big, Big]))
          .StandardOutput(TProcessOutput.Discard)
          .StandardError(TProcessOutput.Discard)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('termina como Completed, y no como Timeout',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Status).ToEqual('Completed');
      end)

  .Scenario('Capturar un flujo no bloquea al hijo en el otro')
    .Given('un hijo que escribe 1 MB en la salida de error, con la estandar capturada y la de error descartada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command(Format('cmd /c type "%s" 1>&2', [W.WriteBig]))
          .OnStdOut(W.Capture())
          .StandardError(TProcessOutput.Discard)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('termina como Completed',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Status).ToEqual('Completed');
      end)

  .Scenario('Una salida grande capturada llega entera')
    .Given('un hijo que escribe 1 MB en la salida estandar, capturada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command(Format('cmd /c type "%s"', [W.WriteBig]))
          .OnStdOut(W.Capture())
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe todo lo que escribio el hijo',
      procedure(W: TStreamsWorld)
      begin
        Expect(Format('%s %d', [W.Status, Length(W.Captured)])).ToEqual(Format('Completed %d', [BigSize]));
      end)

.Rule('Un flujo admite una sola declaracion de destino')

  .Scenario('Capturar y descartar el mismo flujo falla')
    .Given('un builder que ya captura la salida estandar',
      procedure(W: TStreamsWorld)
      begin
        W.Builder := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .OnStdOut(W.Capture());
      end)
    .When('se declara ademas que la salida estandar se descarta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Builder.StandardOutput(TProcessOutput.Discard);
        except
          on E: Exception do
          begin
            W.Builder := nil;
            W.Report := E.ClassName + ': ' + E.Message;
          end;
        end;
      end)
    .&Then('falla con EInvalidOpException, nombrando la salida estandar',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Report).ToEqual('EInvalidOpException: StandardOutput already has a destination: ' +
          'a stream takes exactly one');
      end)

  .Scenario('Declarar la salida de error dos veces falla')
    .Given('un builder que ya descarta la salida de error',
      procedure(W: TStreamsWorld)
      begin
        W.Builder := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .StandardError(TProcessOutput.Discard);
      end)
    .When('se declara otra vez la salida de error',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Builder.StandardError(TProcessOutput.Inherit);
        except
          on E: Exception do
          begin
            W.Builder := nil;
            W.Report := E.ClassName + ': ' + E.Message;
          end;
        end;
      end)
    .&Then('falla con EInvalidOpException, nombrando la salida de error',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Report).ToEqual('EInvalidOpException: StandardError already has a destination: ' +
          'a stream takes exactly one');
      end)

.Rule('Un flujo sin destino declarado hereda el del padre')

  .Scenario('Sin declarar nada, la salida del hijo va adonde va la del padre')
    .Given('la salida estandar del padre va a un fichero, y un hijo que escribe hola sin destino declarado',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .Timeout(10000)
          .Build;
        W.RedirectParent(STD_OUTPUT_HANDLE, W.PathOf('padre.txt'));
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Result := W.Proc.Execute;
        finally
          W.RestoreParent;
        end;
      end)
    .&Then('el fichero del padre tiene hola',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.ReadText(W.PathOf('padre.txt'))).ToEqual('hola');
      end)

  .Scenario('Heredar se puede declarar')
    .Given('la salida de error del padre va a un fichero, y un hijo que escribe fallo en la suya, declarada heredada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo fallo 1>&2')
          .StandardError(TProcessOutput.Inherit)
          .Timeout(10000)
          .Build;
        W.RedirectParent(STD_ERROR_HANDLE, W.PathOf('padre.txt'));
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Result := W.Proc.Execute;
        finally
          W.RestoreParent;
        end;
      end)
    .&Then('el fichero del padre tiene fallo',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.ReadText(W.PathOf('padre.txt'))).ToEqual('fallo');
      end)

.Rule('El texto capturado conserva los caracteres que escribio el hijo')

  .Scenario('Un caracter no ASCII llega con la pagina de la consola')
    .Given('un hijo que escribe una enye con la pagina de salida de la consola, con la salida estandar capturada',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo ' + Enye)
          .OnStdOut(W.Capture())
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe la enye',
      procedure(W: TStreamsWorld)
      begin
        Expect(CodePoints(W.Captured.Trim)).ToEqual('U+00F1');
      end)

  .Scenario('Un hijo oculto se lee con la pagina de su propia consola')
    .Given('la consola del padre en UTF-8, y un hijo oculto que escribe una enye con la pagina de su consola',
      procedure(W: TStreamsWorld)
      begin
        // El runner de MiniSpec deja su consola en UTF-8; un hijo oculto escribe con la OEM, que es otra.
        Expect(Format('consola=%d oem-distinta=%s', [GetConsoleOutputCP, BoolToStr(GetOEMCP <> CP_UTF8, True)]))
          .ToEqual('consola=65001 oem-distinta=True');
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo ' + Enye)
          .OnStdOut(W.Capture())
          .HideWindow(True)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe la enye',
      procedure(W: TStreamsWorld)
      begin
        Expect(CodePoints(W.Captured.Trim)).ToEqual('U+00F1');
      end)

  .Scenario('Una codificacion declarada manda')
    .Given('un hijo que escribe una enye en UTF-8, con la salida estandar capturada y UTF-8 declarado',
      procedure(W: TStreamsWorld)
      begin
        TFile.WriteAllBytes(W.PathOf('utf8.txt'), TEncoding.UTF8.GetBytes(Enye));
        W.Proc := TSystemProcess.Builder
          .Command(Format('cmd /c type "%s"', [W.PathOf('utf8.txt')]))
          .OnStdOut(W.Capture())
          .StandardOutputEncoding(TEncoding.UTF8)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe la enye',
      procedure(W: TStreamsWorld)
      begin
        Expect(CodePoints(W.Captured)).ToEqual('U+00F1');
      end)

  .Scenario('Una salida larga de caracteres multibyte llega entera')
    .Given('un hijo que escribe una x y diez mil enyes en UTF-8, con la salida estandar capturada y UTF-8 declarado',
      procedure(W: TStreamsWorld)
      begin
        // La x desplaza un byte: con lecturas de un numero par de bytes, cada una acaba a mitad de una enye.
        TFile.WriteAllBytes(W.PathOf('utf8.txt'), TEncoding.UTF8.GetBytes('x' + StringOfChar(Enye, 10000)));
        W.Proc := TSystemProcess.Builder
          .Command(Format('cmd /c type "%s"', [W.PathOf('utf8.txt')]))
          .OnStdOut(W.Capture())
          .StandardOutputEncoding(TEncoding.UTF8)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el manejador recibe la x, diez mil enyes y ningun otro caracter',
      procedure(W: TStreamsWorld)
      begin
        var Others := 0;
        for var C in W.Captured do
          if C <> Enye then
            Inc(Others);
        Expect(Format('enyes=%d otros=%d', [Length(W.Captured) - Others, Others])).ToEqual('enyes=10000 otros=1');
      end)

.Rule('HideWindow decide si el hijo tiene ventana, no si tiene otra consola')

  .Scenario('Por defecto el hijo comparte la consola del padre')
    .Given('un hijo de consola que dura un par de segundos, lanzado sin llamar a HideWindow',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('ping -n 3 127.0.0.1')
          .StandardOutput(TProcessOutput.Discard)
          .StandardError(TProcessOutput.Discard)
          .OnIdle(
            procedure(R: TProcessResult)
            begin
              if ConsoleProcesses > W.ConsoleMost then
                W.ConsoleMost := ConsoleProcesses;
            end)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.ConsoleBefore := ConsoleProcesses;
        W.ConsoleMost := W.ConsoleBefore;
        W.Result := W.Proc.Execute;
      end)
    .&Then('mientras corre, esta entre los procesos de la consola del padre',
      procedure(W: TStreamsWorld)
      begin
        Expect(Format('%s comparte=%s', [W.Status, BoolToStr(W.ConsoleMost > W.ConsoleBefore, True)]))
          .ToEqual('Completed comparte=True');
      end)

  .Scenario('Oculto, el hijo no usa la consola del padre')
    .Given('un hijo de consola que dura un par de segundos, lanzado con HideWindow',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('ping -n 3 127.0.0.1')
          .StandardOutput(TProcessOutput.Discard)
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .OnIdle(
            procedure(R: TProcessResult)
            begin
              if ConsoleProcesses > W.ConsoleMost then
                W.ConsoleMost := ConsoleProcesses;
            end)
          .Timeout(10000)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        W.ConsoleBefore := ConsoleProcesses;
        W.ConsoleMost := W.ConsoleBefore;
        W.Result := W.Proc.Execute;
      end)
    .&Then('mientras corre, no esta entre los procesos de la consola del padre',
      procedure(W: TStreamsWorld)
      begin
        Expect(Format('%s comparte=%s', [W.Status, BoolToStr(W.ConsoleMost > W.ConsoleBefore, True)]))
          .ToEqual('Completed comparte=False');
      end)

  .Scenario('Oculto, la salida heredada sigue llegando al padre')
    .Given('la salida estandar del padre va a un fichero, y un hijo oculto que escribe hola sin destino declarado',
      procedure(W: TStreamsWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .HideWindow(True)
          .Timeout(10000)
          .Build;
        W.RedirectParent(STD_OUTPUT_HANDLE, W.PathOf('padre.txt'));
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Result := W.Proc.Execute;
        finally
          W.RestoreParent;
        end;
      end)
    .&Then('el fichero del padre tiene hola',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.ReadText(W.PathOf('padre.txt'))).ToEqual('hola');
      end)

.Rule('Un hijo no hereda los flujos de otro lanzamiento')

  .Scenario('Un lanzamiento concurrente no retiene el fichero de otro')
    .Given('un hilo que lanza sin pausa hijos que viven unos segundos',
      procedure(W: TStreamsWorld)
      begin
        W.Launcher := TThread.CreateAnonymousThread(
          procedure
          begin
            var Live := TList<IFuture<TProcessResult>>.Create;
            try
              while not W.StopLaunching do
              begin
                Live.Add(TSystemProcess.Builder
                  .Command('ping')
                  .CmdArgs(['-n', '3', '127.0.0.1'])
                  .StandardOutput(TProcessOutput.Discard)
                  .StandardError(TProcessOutput.Discard)
                  .HideWindow(True)
                  .Timeout(30000)
                  .Build
                  .ExecuteAsync);
                if Live.Count >= 8 then
                begin
                  Live[0].Wait;
                  Live.Delete(0);
                end;
              end;
              for var F in Live do
                F.Wait;
            finally
              Live.Free;
            end;
          end);
        W.Launcher.FreeOnTerminate := False;
        W.Launcher.Start;
      end)
    .When('otro hilo ejecuta muchas veces un hijo corto con la salida a un fichero, y lo borra al volver',
      procedure(W: TStreamsWorld)
      begin
        var Failed := 0;
        var Target := W.PathOf('carrera.txt');
        for var Round := 1 to RaceRounds do
        begin
          TSystemProcess.Builder
            .Command('cmd /c echo hola')
            .StandardOutput(TProcessOutput.ToFile(Target))
            .StandardError(TProcessOutput.Discard)
            .HideWindow(True)
            .Timeout(10000)
            .Build
            .Execute;
          if not DeleteFile(PChar(Target)) then
            Inc(Failed);
        end;
        W.Report := Format('%d de %d borrados fallaron', [Failed, RaceRounds]);
      end)
    .&Then('todos los borrados funcionan',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Report).ToEqual(Format('0 de %d borrados fallaron', [RaceRounds]));
      end)

.Rule('La entrada estandar del hijo es la del padre')

  .Scenario('El hijo lee lo que lee el padre')
    .Given('la entrada estandar del padre es un fichero con hola, y un hijo que copia su entrada a su salida capturada',
      procedure(W: TStreamsWorld)
      begin
        TFile.WriteAllText(W.PathOf('entrada.txt'), 'hola' + sLineBreak, TEncoding.ANSI);
        W.Proc := TSystemProcess.Builder
          .Command('findstr "^"')
          .OnStdOut(W.Capture())
          .Timeout(10000)
          .Build;
        W.RedirectParent(STD_INPUT_HANDLE, W.PathOf('entrada.txt'), True);
      end)
    .When('se ejecuta',
      procedure(W: TStreamsWorld)
      begin
        try
          W.Result := W.Proc.Execute;
        finally
          W.RestoreParent;
        end;
      end)
    .&Then('el manejador recibe hola',
      procedure(W: TStreamsWorld)
      begin
        Expect(W.Captured.Trim).ToEqual('hola');
      end);

end.
