unit FmxQuickStart.MainForm;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Edit,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.FMX.Application,
  FmxQuickStart.Services;

type
  TMainForm = class(TForm)
    NameLabel: TLabel;
    NameCtl: TEdit;
    GreetCtl: TButton;
    AnswerLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GreetCtlClick(Sender: TObject);
  private
    FGreeter: IGreeter;
  public
    /// La dependencia del formulario, y su unica via: el formulario lo construye
    /// FireMonkey, asi que no hay constructor que el contenedor pueda elegir.
    [DafInject]
    property Greeter: IGreeter read FGreeter write FGreeter;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // InjectInto rellena las propiedades marcadas contra el proveedor. DafApp es como se
  // llega al proveedor desde codigo que construye la plataforma -- y se llama DafApp y
  // no Application porque el global de FireMonkey ya ocupa ese nombre.
  TActivatorUtilities.InjectInto(DafApp.Services, Self);
  AnswerLabel.Text := 'Escribe un nombre y pulsa el boton.';
end;

procedure TMainForm.GreetCtlClick(Sender: TObject);
begin
  AnswerLabel.Text := Format('%s  (saludo numero %d)',
    [FGreeter.Greet(NameCtl.Text), FGreeter.Count]);
end;

end.
