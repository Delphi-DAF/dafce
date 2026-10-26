unit Daf.NNLog.Targets.Console;

interface

uses
  Daf.Extensions.Logging,
  Daf.NNLog;

type
  TConsoleTarget = class(TTarget)
  private
    FUseColors: Boolean;
    procedure ColoredWrite(const Entry: TLogEntry);
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    constructor Create; override;
    property UseColors: Boolean read FUseColors write FUseColors;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils;

{$IFNDEF MSWINDOWS}
const
  // Vive aqui y no dentro del metodo porque un const EN LINEA solo admite constantes simples: una
  // constante tipada como este array da E2003 en su propia linea de declaracion.
  AnsiReset = #27'[0m';
  AnsiColors: array[TLogLevel] of string = (
    #27'[90m',    // Trace = gris
    #27'[36m',    // Debug = cyan
    #27'[32m',    // Information = verde
    #27'[33m',    // Warning = amarillo
    #27'[31m',    // Error = rojo
    #27'[41;97m', // Critical = blanco sobre rojo
    ''            // None: el nivel que apaga, asi que no se compone ninguna linea con el
  );
{$ENDIF}

{ TConsoleTarget }

constructor TConsoleTarget.Create;
begin
  inherited Create;
  UseColors := False;
end;

procedure TConsoleTarget.DoWrite(const Entry: TLogEntry);
begin
  if UseColors then
    ColoredWrite(Entry)
  else
    Writeln(RenderEvent(Entry));
end;

procedure TConsoleTarget.ColoredWrite(const Entry: TLogEntry);
var
  Msg: string;
  Color: Word;
begin
  Msg := RenderEvent(Entry);

  {$IFDEF MSWINDOWS}
  case Entry.Level of
    TLogLevel.Trace:       Color := FOREGROUND_INTENSITY;
    TLogLevel.Debug:       Color := FOREGROUND_BLUE or FOREGROUND_GREEN;
    TLogLevel.Information: Color := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    TLogLevel.Warning:     Color := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    TLogLevel.Error:       Color := FOREGROUND_RED or FOREGROUND_INTENSITY;
    TLogLevel.Critical:    Color := FOREGROUND_RED or FOREGROUND_INTENSITY or BACKGROUND_RED;
  else
    Color := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  end;

  var StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  var OriginalAttrs: CONSOLE_SCREEN_BUFFER_INFO;
  GetConsoleScreenBufferInfo(StdOut, OriginalAttrs);

  SetConsoleTextAttribute(StdOut, Color);
  Writeln(Msg);
  SetConsoleTextAttribute(StdOut, OriginalAttrs.wAttributes);

  {$ELSE}
  // En plataformas no Windows: colores ANSI (sólo en consolas compatibles)
  WriteLn(AnsiColors[Entry.Level] + Msg + AnsiReset);
  {$ENDIF}
end;

end.

