program PingProcess;

{ $APPTYPE CONSOLE }

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  WinApi.Windows,
  Daf.Rtti,
  Daf.Threading,
  Daf.SystemProcess;

begin
  ReportMemoryLeaksOnShutdown := True;

  var Timeout_secs := 18;

  var Report :=
    function(Title: string=''): TProcessNotifyProc
    begin
        Result := procedure(Result: TProcessResult)
                  begin
                    TThread.Synchronize(nil,
                    procedure
                    begin
                      WriteLn(Title);
                      Writeln('    Status: ', TEnum.ToString(Result.Status));
                      Writeln('    ExitCode: ', Result.ExitCode);
                      Writeln('    LastError: ', Result.LastError);
                      Writeln('    Duration: ', Result.Duration.Seconds);
                    end);
                  end;
    end;

  var
  Process := TSystemProcess.Builder
    .Command('cmd.exe').CmdArgs(['/k', 'ping', '127.0.0.1'])
    .Timeout(Timeout_secs*1000)
    .OnStdOut(procedure(Text: string)
              begin
                Writeln(Text);
              end)
    .OnStdErr(procedure(Text: string)
              begin
                Writeln('Error: ', Text);
              end)
    .OnIdle(Report('On Idle:'))
    .OnFailed(Report('On Failed:'))
    .OnCancelled(Report('On Cancelled:'))
    .OnKilled(Report('On Killed:'))
    .OnCompleted(Report('On Completed:'))
    //.HideWindow
    .Build;

  TShutdownHook.OnShutdownRequested := procedure
  begin
    Process.Kill;
  end;

  var token := CreateCancellationTokenSource;
  Process.ExecuteAsync(token.token);

  Writeln('Process launched');
  Writeln(' it will be killed by Timeout:');
  for var sec := Timeout_secs downto 0 do
  begin
    Sleep(1000);
    CheckSynchronize(10);
    WriteLn('...', sec);
    if sec = 5 then
        token.Cancel;  // also you can use Process.Kill without use any token

  end;
  Writeln;
  TShutdownHook.WaitForShutdown;
  Process := nil;
  end.
