unit VclVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
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

  /// Nadie lo registra: es lo que hace fallar la construccion con la validacion fijada.
  IUnregistered = interface(IInvokable)
    ['{3F0C8E5A-1B7D-4E2A-9C64-8D2F5A7B1E09}']
  end;

  INeedsUnregistered = interface(IInvokable)
    ['{C47A1D93-6E2B-4F58-A0D1-5B9E3C7F2A64}']
  end;

  TNeedsUnregistered = class(TInterfacedObject, INeedsUnregistered)
  private
    FDependency: IUnregistered;
  public
    constructor Create(const Dependency: IUnregistered);
  end;

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given a
  /// ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara es la
  /// cadena que el propio montaje acaba de componer, no el comportamiento.
  TVclWorld = class
  public
    Builder: IVclApplicationBuilder;
    App: IVclApplication;
    /// La primera aplicacion construida; la actual es App.
    Earlier: IVclApplication;
    /// La instancia que ya corre, y que sostiene el cerrojo mientras viva.
    Holder: IVclApplication;
    Report: string;
    /// El codigo de salida del proceso al empezar: una aplicacion resuelta lo cambia, y el runner
    /// sale con el.
    SavedExitCode: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  /// Falla al pararse: es lo que pone a prueba la liberacion de una aplicacion sin parar.
  TRefusingStop = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  System.StrUtils,
  Vcl.Forms,
  Daf.MiniSpec,
  Daf.Extensions.Logging,
  SpecForm;

/// Lo que se ve de una aplicacion de escritorio despues de arrancarla hasta el bucle.
function Outcome(const App: IVclApplication): string;
begin
  // La expresion condicional y no IfThen, que evalua sus dos argumentos: el ClassName de un
  // formulario nil.
  Result := Format('contenedor=%s formulario=%s', [if App.Services = nil then 'no' else 'si',
    if App.MainForm = nil then 'no' else App.MainForm.ClassName]);
end;

constructor TVclWorld.Create;
begin
  inherited;
  SavedExitCode := System.ExitCode;
end;

constructor TNeedsUnregistered.Create(const Dependency: IUnregistered);
begin
  inherited Create;
  FDependency := Dependency;
end;

destructor TVclWorld.Destroy;
begin
  System.ExitCode := SavedExitCode;
  if Holder <> nil then
    Holder.Stop;
  // Lo que el World sostiene lo cierra el World: una aplicacion construida y abandonada retiene
  // su host y deja el global apuntandola. Stop es idempotente, asi que el escenario que ya la
  // paro no paga por esto.
  if App <> nil then
    App.Stop;
  inherited;
end;

procedure TRefusingStop.Start;
begin
end;

procedure TRefusingStop.Stop;
begin
  raise Exception.Create('la parada fallo');
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

.Rule('Las variantes de escritorio pueden declarar instancia unica')

  .Scenario('Con instancia unica declarada, la segunda no arranca')
    .Given('una aplicacion VCL con un identificador de instancia unica, y una segunda declarada igual',
      procedure(W: TVclWorld)
      begin
        var First := TVclApplication.CreateBuilder;
        First.SingleInstance('daf-vcl-spec');
        W.Holder := First.Build;
        W.Builder := TVclApplication.CreateBuilder;
        W.Builder.SingleInstance('daf-vcl-spec');
        W.App := W.Builder.Build;
      end)
    .When('se arranca la segunda hasta el borde del bucle',
      procedure(W: TVclWorld)
      begin
        W.App.StartUpTo(TSpecForm);
        W.Report := Format('%s exit=%d global=%s', [Outcome(W.App), System.ExitCode,
          IfThen(DafApp = W.Holder, 'la primera', 'otra')]);
      end)
    .&Then('vuelve sin construir el contenedor ni crear el formulario, y el proceso termina con el codigo de otra instancia en marcha',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual(Format('contenedor=no formulario=no exit=%d global=la primera',
          [TAppExit.AlreadyRunning]));
      end)

  .Scenario('Sin instancia unica declarada, corren varias')
    .Given('una aplicacion VCL que no declara instancia unica, y una segunda igual',
      procedure(W: TVclWorld)
      begin
        W.Holder := TVclApplication.CreateBuilder.Build;
        W.Builder := TVclApplication.CreateBuilder;
        W.App := W.Builder.Build;
      end)
    .When('se arranca la segunda hasta el borde del bucle',
      procedure(W: TVclWorld)
      begin
        W.App.StartUpTo(TSpecForm);
        W.Report := Outcome(W.App);
      end)
    .&Then('la segunda construye su contenedor y crea su formulario',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual('contenedor=si formulario=TSpecForm');
      end)

  .Scenario('Una construccion que falla suelta el cerrojo')
    .Given('una aplicacion VCL con instancia unica cuya construccion fallo',
      procedure(W: TVclWorld)
      begin
        var First := TVclApplication.CreateBuilder;
        First.SingleInstance('daf-vcl-spec-failed');
        First.Services.AddSingleton<INeedsUnregistered, TNeedsUnregistered>;
        First.ServiceProviderOptions := TServiceProviderOptions.Create(True, True);
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            First.Build;
            W.Report := '<construyo sin protestar>';
          except
            on E: Exception do W.Report := '';
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .When('arranca una segunda con el mismo identificador',
      procedure(W: TVclWorld)
      begin
        W.Builder := TVclApplication.CreateBuilder;
        W.Builder.SingleInstance('daf-vcl-spec-failed');
        W.App := W.Builder.Build;
        W.App.StartUpTo(TSpecForm);
        W.Report := W.Report + Outcome(W.App);
      end)
    .&Then('la segunda construye su contenedor y crea su formulario',
      procedure(W: TVclWorld)
      begin
        // El vacio delante es el control de que la primera construccion fallo de verdad.
        Expect(W.Report).ToEqual('contenedor=si formulario=TSpecForm');
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
      end)

.Rule('Lo que falla al parar no sale del destructor')

  .Scenario('Soltar una aplicacion que ya no es la actual, cuyo servicio alojado falla al parar')
    .Given('una aplicacion VCL con un servicio alojado que falla al parar, y otra construida despues',
      procedure(W: TVclWorld)
      begin
        var Earlier := TVclApplication.CreateBuilder;
        Hosting.AddHostedService(Earlier.Services, TRefusingStop);
        W.Earlier := Earlier.Build;
        W.Builder := TVclApplication.CreateBuilder;
        W.App := W.Builder.Build;
        // Lo que el escenario supone: la actual es la segunda, asi que a la primera solo la sostiene
        // el World, y soltarla la destruye sin haberla parado.
        Expect(DafApp = W.App).ToBeTrue;
      end)
    .When('se suelta la primera',
      procedure(W: TVclWorld)
      begin
        // La parada informa su fallo por el canal de arranque; se aparta para que no se confunda
        // con la salida de las specs.
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            W.Earlier := nil;
            W.Report := '<sin excepcion>';
          except
            on E: Exception do
              W.Report := E.ClassName + ': ' + E.Message;
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('la liberacion termina sin error',
      procedure(W: TVclWorld)
      begin
        Expect(W.Report).ToEqual('<sin excepcion>');
      end);

end.
