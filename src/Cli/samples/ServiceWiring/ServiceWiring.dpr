program ServiceWiring;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  ServiceWiring.Greeter in 'ServiceWiring.Greeter.pas';

// Como entra un servicio en una herramienta de linea de comandos: se registra en el builder, que
// ya esta vivo, y la accion lo resuelve del proveedor que recibe.
//
//   ServiceWiring  -> 0, saluda a traves del servicio
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.SetAction(
    function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
      Result := TCliExit.Ok;
    end);

  B.ParseArgs(CommandLineConfig.ProcessArgs);
  ExitCode := B.Build.Run;
end.
