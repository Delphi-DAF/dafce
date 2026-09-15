unit VclVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.VCL.Application;

type
  IDesktopService = interface(IInvokable)
    ['{7AF488F7-2EDB-4388-BD90-00A9D91A6F63}']
    function Name: string;
  end;

  TDesktopService = class(TInterfacedObject, IDesktopService)
  public
    function Name: string;
  end;

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given a
  /// ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara es la
  /// cadena que el propio montaje acaba de componer, no el comportamiento.
  TVclWorld = class
  public
    Builder: IVclApplicationBuilder;
    App: IVclApplication;
    Report: string;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Forms,
  Daf.MiniSpec,
  Daf.Extensions.Hosting,
  SpecForm;

destructor TVclWorld.Destroy;
begin
  // Lo que el World sostiene lo cierra el World: una aplicacion construida y abandonada retiene
  // su host y deja el global apuntandola. Stop es idempotente, asi que el escenario que ya la
  // paro no paga por esto.
  if App <> nil then
    App.Stop;
  inherited;
end;

function TDesktopService.Name: string;
begin
  Result := 'desktop';
end;

initialization

Feature('''
Feature La variante VCL de la familia @vcl @application-family

  Como quien escribe una aplicacion de escritorio
  Quiero que el arranque sea el mismo de la familia
  Para que solo cambie quien es dueño del bucle
''')

.UseWorld<TVclWorld>

.Rule('El arranque llega hasta donde manda la plataforma')

  // Hasta el BORDE del bucle y no mas alla: llamar a Run entraria en el message loop de la VCL
  // y la suite se quedaria ahi. Una spec colgada no es una spec roja.
  .Scenario('Construye su builder y llega a entregar el mando')
    .Given('una aplicacion VCL compuesta y construida, sin formulario todavia',
      procedure(W: TVclWorld)
      begin
        W.Builder := TVclApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IDesktopService, TDesktopService>;
        W.App := W.Builder.Build;
        // Lo que el escenario supone: si el formulario ya estuviera aqui, el Then no diria que
        // lo creo el arranque.
        Expect(W.App.MainForm = nil).ToBeTrue;
      end)
    .When('se arranca hasta crear el formulario principal',
      procedure(W: TVclWorld)
      begin
        W.App.StartUpTo(TSpecForm);
      end)
    .&Then('el contenedor responde y el formulario es el que se pidio',
      procedure(W: TVclWorld)
      begin
        Expect(Format('servicio=%s formulario=%s',
          [W.App.Services.GetRequiredService<IDesktopService>.Name,
           W.App.MainForm.ClassName])).ToEqual('servicio=desktop formulario=TSpecForm');
      end)

.Rule('El acceso global')

  // Existe SOLO porque la plataforma ya tiene uno y el usuario lo espera. En CLI y en Web no lo
  // hay, y alli no falta.
  .Scenario('Convive con el global de su plataforma, y se distingue cual es cual')
    .Given('una aplicacion VCL construida',
      procedure(W: TVclWorld)
      begin
        W.Builder := TVclApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IDesktopService, TDesktopService>;
        W.App := W.Builder.Build;
      end)
    .When('se pide el global de la variante',
      procedure(W: TVclWorld)
      begin
        // Los dos en la misma expresion: el de DAF y el de la plataforma, y el nombre no los
        // confunde.
        W.Report := Format('daf=%s vcl=%s',
          [BoolToStr(DafApp = W.App, True), Vcl.Forms.Application.ClassName]);
      end)
    .&Then('devuelve la aplicacion DAF, y el de la VCL sigue siendo el suyo',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual('daf=True vcl=TApplication');
      end)

  .Scenario('Cuando la aplicacion se para, el global deja de apuntar a ella')
    .Given('una aplicacion VCL construida, con el global apuntandola',
      procedure(W: TVclWorld)
      begin
        W.Builder := TVclApplication.CreateBuilder;
        W.App := W.Builder.Build;
        // Lo que el escenario supone: sin esto, un global que no hubiera apuntado nunca a nadie
        // daria el mismo verde que el que deja de apuntar al pararse.
        Expect(DafApp = W.App).ToBeTrue;
      end)
    .When('se para la aplicacion',
      procedure(W: TVclWorld)
      begin
        W.App.Stop;
      end)
    .&Then('el global no queda apuntando a una aplicacion muerta',
      procedure(W: TVclWorld)
      begin
        Expect(DafApp = nil).ToBeTrue;
      end);

end.
