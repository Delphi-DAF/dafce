
program ConfigExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  InMemoryConfig in 'InMemoryConfig.pas',
  FileConfig in 'FileConfig.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    TInMemoryConfigSample.Run;
    Readln;
    TFileConfigSample.Run;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
