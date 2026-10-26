program FmxQuickStart;

uses
  FMX.Forms,
  Daf.Extensions.DependencyInjection,
  Daf.FMX.Application,
  FmxQuickStart.Services in 'FmxQuickStart.Services.pas',
  FmxQuickStart.MainForm in 'FmxQuickStart.MainForm.pas' {MainForm};

{$R *.res}

// La variante FMX de la familia. El arranque es el mismo que el de la CLI y el de la Web
// --contenedor, configuracion, entorno y logging-- hasta el punto en que la plataforma se
// queda con el bucle: aqui Run crea el formulario principal y delega en
// FMX.Forms.Application, que es quien lo tiene. En la CLI y en la Web, Run ES el proceso y
// el .dpr acaba cuando Run acaba; son dos contratos de ciclo de vida, no dos variaciones
// de uno.
begin
  ReportMemoryLeaksOnShutdown := True;

  var B := TFmxApplication.CreateBuilder;

  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.Build.Run(TMainForm);
end.
