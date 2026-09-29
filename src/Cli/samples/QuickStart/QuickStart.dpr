program QuickStart;

{$APPTYPE CONSOLE}

uses
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli;

// El ejemplo de entrada de una herramienta de linea de comandos: el builder, la accion de la raiz
// escrita en el sitio, y el codigo de salida que devuelve esa accion.
//
//   QuickStart  -> 0, saluda
//
// Como se anade un servicio esta en el sample ServiceWiring, que es el nivel siguiente.
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  B.SetAction(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Writeln('Hola desde DAF');
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
