unit VclVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  IDesktopService = interface(IInvokable)
    ['{7AF488F7-2EDB-4388-BD90-00A9D91A6F63}']
    function Name: string;
  end;

  TDesktopService = class(TInterfacedObject, IDesktopService)
  public
    function Name: string;
  end;

  TVclWorld = class
  public
    Report: string;
  end;

implementation

uses
  Vcl.Forms,
  Daf.MiniSpec,
  Daf.Extensions.Hosting,
  Daf.VCL.Application,
  SpecForm;

function TDesktopService.Name: string;
begin
  Result := 'desktop';
end;

initialization

Feature('''
Feature La variante VCL de la familia @vcl @application-family

  Como quien escribe una aplicacion de escritorio
  Quiero que el arranque sea el mismo de la familia
  Para que solo cambie quien es dueno del bucle
''')

.UseWorld<TVclWorld>

.Rule('El arranque llega hasta donde manda la plataforma')

  // Hasta el BORDE del bucle y no mas alla: llamar a Run entraria en el message loop de la VCL
  // y la suite se quedaria ahi. Una spec colgada no es una spec roja.
  .Scenario('Construye su builder y llega a entregar el mando')
    .Given('una aplicacion VCL compuesta',
      procedure(W: TVclWorld)
      begin
        var B := TVclApplication.CreateBuilder;
        B.Services.AddSingleton<IDesktopService, TDesktopService>;
        var App := B.Build;
        try
          App.StartUpTo(TSpecForm);
          W.Report := Format('servicio=%s formulario=%s',
            [App.Services.GetRequiredService<IDesktopService>.Name,
             App.MainForm.ClassName]);
        finally
          App.Stop;
        end;
      end)
    .When('se arranca hasta crear el formulario principal').NoAction
    .&Then('el contenedor responde y el formulario es el que se pidio',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual('servicio=desktop formulario=TSpecForm');
      end)

.Rule('El acceso global')

  // Existe SOLO porque la plataforma ya tiene uno y el usuario lo espera. En CLI y en Web no lo
  // hay, y alli no falta.
  .Scenario('Convive con el global de su plataforma, y se distingue cual es cual')
    .Given('una aplicacion VCL construida',
      procedure(W: TVclWorld)
      begin
        var B := TVclApplication.CreateBuilder;
        B.Services.AddSingleton<IDesktopService, TDesktopService>;
        var App := B.Build;
        try
          // Los dos en la misma expresion: el de DAF y el de la plataforma, y el nombre no los
          // confunde.
          W.Report := Format('daf=%s vcl=%s',
            [BoolToStr(DafApp = App, True), Vcl.Forms.Application.ClassName]);
        finally
          App.Stop;
        end;
      end)
    .When('se pide el global de la variante').NoAction
    .&Then('devuelve la aplicacion DAF, y el de la VCL sigue siendo el suyo',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual('daf=True vcl=TApplication');
      end)

  .Scenario('Cuando la aplicacion se para, el global deja de apuntar a ella')
    .Given('una aplicacion que ya se ha parado',
      procedure(W: TVclWorld)
      begin
        var B := TVclApplication.CreateBuilder;
        var App := B.Build;
        App.Stop;
        W.Report := BoolToStr(DafApp = nil, True);
      end)
    .When('se pide el global').NoAction
    .&Then('no queda apuntando a una aplicacion muerta',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end);

end.
