program QuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Application.Builder,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

begin
  var Builder := TDafApplication.CreateHostBuilder;

  Builder.ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<IGreeter, TGreeter>;
    end);

  Builder.Build.Run(
    procedure(Services: IServiceProvider)
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
    end);
end.
