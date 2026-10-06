program PackageInit;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  LegacyEngine in 'LegacyEngine.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

    // El arranque no nombra la tarea de inicializacion del paquete, ni sabe que existe.
    // Llamar a esta funcion, ademas, enlaza su unit: no hace falta un uses de force-link.
    AddLegacyEngine(B.Services);

    B.SetAction(
      function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
      begin
        // Si la tarea de arranque no hubiera corrido, esto lanzaria.
        Writeln(TLegacyEngine.Evaluate('2+2'));
        Result := TCliExit.Ok;
      end);

    ExitCode := B.Build.Run;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
