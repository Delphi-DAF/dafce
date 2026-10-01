program GreetTool;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli;

// Una herramienta CLI de la familia, con gramatica de verdad. Lo que enseña es el BORDE del Run:
// --version y --help los contesta el parse, una invocacion que no encaja sale con su propio
// codigo, y lo que se escape aplica la politica declarada. Ninguna de esas cuatro lineas se
// escribe en el .dpr: se declaran.
//
//   GreetTool --name Jorge      -> 0, saluda
//   GreetTool --name Jorge --shout -> 0, saluda gritando
//   GreetTool --version         -> 0, la version
//   GreetTool --help            -> 0, la ayuda
//   GreetTool                   -> 2, falta un argumento obligatorio
//   GreetTool --name boom       -> 7, la politica traduce la excepcion
begin
  var NameOpt := CommandLine.Option<string>('--name', 'A quien saludar').Required;
  var ShoutOpt := CommandLine.Option<Boolean>('--shout', 'En mayusculas').WithAlias('-s');

  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
  B.RootCommand.WithVersion('2.0.0').Add([NameOpt, ShoutOpt]);

  // Solo una instancia: la segunda no llega a construir el contenedor.
  B.SingleInstance('daf-greet-tool');

  // La politica es un callback, asi que una suite puede compartir la suya sin heredar nada mas.
  B.OnException(
    function(const Failure: TCliFailure): Integer
    begin
      Writeln(ErrOutput, 'greet: ', Failure.Error.Message);
      Result := 7;
    end);

  B.SetAction(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      var Who := NameOpt.ValueIn(Parsed);
      if Who = 'boom' then
        raise Exception.Create('ese nombre no me gusta');

      var Greeting := 'Hola, ' + Who;
      if ShoutOpt.ValueIn(Parsed) then
        Greeting := Greeting.ToUpper;

      Writeln(Greeting);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
