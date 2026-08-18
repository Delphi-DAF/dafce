program PackageInit;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  LegacyEngine in 'LegacyEngine.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    var Builder: IHostBuilder := THostBuilder.Create;
    Builder.ConfigureServices(
      procedure(Context: IHostBuilderContext; Services: IServiceCollection)
      begin
        // El arranque no nombra la tarea de inicializacion del paquete, ni sabe que existe.
        // Llamar a esta funcion, ademas, enlaza su unit: no hace falta un uses de force-link.
        AddLegacyEngine(Services);
      end);

    var Host := Builder.Build;
    Host.Start;
    try
      // Si la tarea de arranque no hubiera corrido, esto lanzaria.
      Writeln(TLegacyEngine.Evaluate('2+2'));
    finally
      Host.Stop;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
