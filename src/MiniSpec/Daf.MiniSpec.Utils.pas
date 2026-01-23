unit Daf.MiniSpec.Utils;

interface

type
  OSShell = class
  public
    class procedure UseUTF8;
    class procedure Open(FileName: string);
    class procedure WaitForShutdown;
  end;

implementation
uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
  WinApi.ShellAPI,
{$ENDIF}
  System.SysUtils,
  System.Types;

class procedure OSShell.Open(FileName: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

class procedure OSShell.UseUTF8;
begin
{$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
  // Reopen stdout with UTF-8 encoding
  SetTextCodePage(Output, CP_UTF8);
{$ENDIF}
end;

class procedure OSShell.WaitForShutdown;
  begin
    WriteLn('Press ctl-c to exit');
    ReadLn;
  end;
end.
