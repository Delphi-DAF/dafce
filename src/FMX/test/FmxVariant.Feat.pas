unit FmxVariant.Feat;

interface

uses
  System.SysUtils,
  System.TypInfo,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.FMX.Application;

type
  IDesktopService = interface(IInvokable)
    ['{9AC6D78E-DC71-4DF6-93F3-FB5F53131C9B}']
    function Name: string;
  end;

  TDesktopService = class(TInterfacedObject, IDesktopService)
  public
    function Name: string;
  end;

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given a
  /// ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara es la
  /// cadena que el propio montaje acaba de componer, no el comportamiento.
  TFmxWorld = class
  public
    Builder: IFmxApplicationBuilder;
    App: IFmxApplication;
    /// La primera aplicacion construida; la actual es App.
    Earlier: IFmxApplication;
    Info: PTypeInfo;
    Report: string;
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
  System.Rtti,
  Daf.MiniSpec,
  Daf.Extensions.Logging;

destructor TFmxWorld.Destroy;
begin
  // Lo que el World sostiene lo cierra el World: una aplicacion construida y abandonada retiene
  // su host y deja el global apuntandola. Stop es idempotente, asi que el escenario que ya la
  // paro no paga por esto.
  if App <> nil then
    App.Stop;
  inherited;
end;

/// Los metodos que la interfaz DECLARA, en orden. Es RTTI y no lectura del codigo porque una
/// comprobacion escrita a mano no se pone roja el dia que las dos variantes divergen.
function MembersOf(const Info: PTypeInfo): string;
begin
  Result := '';
  var Ctx := TRttiContext.Create;
  try
    for var M in Ctx.GetType(Info).GetDeclaredMethods do
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + M.Name;
    end;
  finally
    Ctx.Free;
  end;
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
Feature La variante FMX de la familia @fmx @application-family

  Como quien escribe una aplicacion de escritorio
  Quiero que el arranque sea el mismo de la familia
  Para que solo cambie quien es dueño del bucle
''')

.UseWorld<TFmxWorld>

.Rule('El arranque llega hasta donde manda la plataforma')

  // Hasta el BORDE del bucle y no mas alla: llamar a Run entraria en el message loop de FireMonkey
  // y la suite se quedaria ahi. Una spec colgada no es una spec roja.
  // ⚠️ ESTA VARIANTE NO SE EJERCITA HASTA CREAR EL FORMULARIO, y conviene que se sepa: FMX no
  // crea su Application global en un proceso sin interfaz grafica, asi que StartUpTo muere ahi
  // en una violacion de acceso -- medido, tanto con {$APPTYPE CONSOLE} como sin el. Lo que
  // queda cubierto es todo lo anterior, mas que el contrato de esta variante no se separe del
  // de la VCL, que si llega hasta el formulario. Si las dos divergen, esto se pone rojo.
  .Scenario('Construye su builder y el contenedor responde')
    .Given('un componedor FMX con un servicio registrado',
      procedure(W: TFmxWorld)
      begin
        W.Builder := TFmxApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IDesktopService, TDesktopService>;
      end)
    .When('se construye',
      procedure(W: TFmxWorld)
      begin
        W.App := W.Builder.Build;
      end)
    .&Then('el contenedor responde, y el formulario aun no existe',
      procedure(W: TFmxWorld)
      begin
        Expect(Format('servicio=%s formulario=%s',
          [W.App.Services.GetRequiredService<IDesktopService>.Name,
           BoolToStr(W.App.MainForm = nil, True)])).ToEqual('servicio=desktop formulario=True');
      end)

  .Scenario('El contrato de la variante FMX es el mismo que el de la VCL')
    .Given('la interfaz de variante de escritorio de FMX, con su RTTI en el binario',
      procedure(W: TFmxWorld)
      begin
        W.Info := TypeInfo(IFmxApplication);
        // Lo que el escenario supone del entorno: sin RTTI del tipo, MembersOf devolveria vacio,
        // y eso no seria "las dos variantes divergen" sino que no hay nada que comparar.
        Expect(Assigned(W.Info)).ToBeTrue;
      end)
    .When('se enumeran sus miembros',
      procedure(W: TFmxWorld)
      begin
        W.Report := MembersOf(W.Info);
      end)
    .&Then('declara los mismos que la VCL, en el mismo orden',
      procedure(W: TFmxWorld)
      begin
        // Escrito literal a proposito: si alguien anade un miembro a una variante y no a la
        // otra, esto lo dice. Es lo unico que queda cuando el formulario no se puede crear.
        Expect(W.Report).ToEqual('GetServices,Run,StartUpTo,Stop,GetMainForm');
      end)

.Rule('El acceso global')

  // Existe SOLO porque la plataforma ya tiene uno y el usuario lo espera. En CLI y en Web no lo
  // hay, y alli no falta.
  .Scenario('Convive con el global de su plataforma, y se distingue cual es cual')
    .Given('una aplicacion FMX construida',
      procedure(W: TFmxWorld)
      begin
        W.Builder := TFmxApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IDesktopService, TDesktopService>;
        W.App := W.Builder.Build;
      end)
    .When('se pide el global de la variante',
      procedure(W: TFmxWorld)
      begin
        // Los dos en la misma expresion: el de DAF y el de la plataforma, y el nombre no los
        // confunde.
        W.Report := Format('daf=%s fmx=%s',
          [BoolToStr(DafApp = W.App, True), 'TApplication']);
      end)
    .&Then('devuelve la aplicacion DAF, y el de FireMonkey sigue siendo el suyo',
      procedure(W: TFmxWorld)
      begin
        Expect(W.Report).ToEqual('daf=True fmx=TApplication');
      end)

  .Scenario('Cuando la aplicacion se para, el global deja de apuntar a ella')
    .Given('una aplicacion FMX construida, con el global apuntandola',
      procedure(W: TFmxWorld)
      begin
        W.Builder := TFmxApplication.CreateBuilder;
        W.App := W.Builder.Build;
        // Lo que el escenario supone: sin esto, un global que no hubiera apuntado nunca a nadie
        // daria el mismo verde que el que deja de apuntar al pararse.
        Expect(DafApp = W.App).ToBeTrue;
      end)
    .When('se para la aplicacion',
      procedure(W: TFmxWorld)
      begin
        W.App.Stop;
      end)
    .&Then('el global no queda apuntando a una aplicacion muerta',
      procedure(W: TFmxWorld)
      begin
        Expect(DafApp = nil).ToBeTrue;
      end)

.Rule('Lo que falla al parar no sale del destructor')

  .Scenario('Soltar una aplicacion que ya no es la actual, cuyo servicio alojado falla al parar')
    .Given('una aplicacion FMX con un servicio alojado que falla al parar, y otra construida despues',
      procedure(W: TFmxWorld)
      begin
        var Earlier := TFmxApplication.CreateBuilder;
        Hosting.AddHostedService(Earlier.Services, TRefusingStop);
        W.Earlier := Earlier.Build;
        W.Builder := TFmxApplication.CreateBuilder;
        W.App := W.Builder.Build;
        // Lo que el escenario supone: la actual es la segunda, asi que a la primera solo la sostiene
        // el World, y soltarla la destruye sin haberla parado.
        Expect(DafApp = W.App).ToBeTrue;
      end)
    .When('se suelta la primera',
      procedure(W: TFmxWorld)
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
      procedure(W: TFmxWorld)
      begin
        Expect(W.Report).ToEqual('<sin excepcion>');
      end);

end.
