program TicTacToeApp;

uses
  Vcl.Forms,
  Daf.Extensions.DependencyInjection,
  Daf.VCL.Application,
  TicTacToe.Game in 'Domain\TicTacToe.Game.pas',
  TicTacToe.ViewPort in 'Presentation\TicTacToe.ViewPort.pas',
  TicTacToe.ViewModel in 'Presentation\TicTacToe.ViewModel.pas',
  TicTacToe.MainForm in 'UI\TicTacToe.MainForm.pas' {MainForm};

{$R *.res}

// La variante VCL de la familia. Lo que antes eran cuatro lineas de Vcl.Forms.Application ahora
// pasa por el arranque de DAF --contenedor, configuracion, entorno y logging-- y acaba
// entregandole el bucle a la misma Vcl.Forms.Application de siempre, que es quien lo tiene.
//
// El formulario lo construye la plataforma, no el contenedor, asi que es justo el caso para el
// que DafApp existe: desde FormCreate se llega al proveedor y el ViewModel se resuelve por el.
begin
  ReportMemoryLeaksOnShutdown := True;

  var B := TVclApplication.CreateBuilder;

  // Aqui `Application` sin cualificar es el de la VCL: los dos globales conviven en este mismo
  // fichero, y por eso el de DAF se llama DafApp y no Application.
  Vcl.Forms.Application.MainFormOnTaskbar := True;

  B.Build.Run(TMainForm);
end.
