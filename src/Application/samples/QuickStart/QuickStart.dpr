program QuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

// Lo mas corto que se puede escribir con la familia: una variante, sus registros donde estan
// escritos, y el codigo de salida que devuelve el trabajo.
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
