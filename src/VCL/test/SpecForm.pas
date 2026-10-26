unit SpecForm;

// Un formulario de verdad, con su .dfm: Application.CreateForm construye leyendo el recurso, y
// sin el levanta EResNotFound. Sin un formulario real, el spec no llegaria a donde la variante
// entrega el mando a la plataforma.

interface

uses
  System.Classes,
  Vcl.Forms;

type
  TSpecForm = class(TForm)
  end;

implementation

{$R *.dfm}

end.
