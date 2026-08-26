unit FmxVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  IDesktopService = interface(IInvokable)
    ['{9AC6D78E-DC71-4DF6-93F3-FB5F53131C9B}']
    function Name: string;
  end;

  TDesktopService = class(TInterfacedObject, IDesktopService)
  public
    function Name: string;
  end;

  TFmxWorld = class
  public
    Report: string;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.Extensions.Hosting,
  Daf.FMX.Application;

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

function TDesktopService.Name: string;
begin
  Result := 'desktop';
end;

initialization

Feature('''
Feature La variante FMX de la familia @fmx @application-family

  Como quien escribe una aplicacion de escritorio
  Quiero que el arranque sea el mismo de la familia
  Para que solo cambie quien es dueno del bucle
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
    .Given('una aplicacion FMX compuesta',
      procedure(W: TFmxWorld)
      begin
        var B := TFmxApplication.CreateBuilder;
        B.Services.AddSingleton<IDesktopService, TDesktopService>;
        var App := B.Build;
        try
          W.Report := Format('servicio=%s formulario=%s',
            [App.Services.GetRequiredService<IDesktopService>.Name,
             BoolToStr(App.MainForm = nil, True)]);
        finally
          App.Stop;
        end;
      end)
    .When('se construye').NoAction
    .&Then('el contenedor responde, y el formulario aun no existe',
      procedure(W: TFmxWorld)
      begin
        Expect(W.Report).ToEqual('servicio=desktop formulario=True');
      end)

  .Scenario('El contrato de la variante FMX es el mismo que el de la VCL')
    .Given('las dos interfaces de variante de escritorio',
      procedure(W: TFmxWorld)
      begin
        W.Report := MembersOf(TypeInfo(IFmxApplication));
      end)
    .When('se enumeran sus miembros').NoAction
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
        var B := TFmxApplication.CreateBuilder;
        B.Services.AddSingleton<IDesktopService, TDesktopService>;
        var App := B.Build;
        try
          // Los dos en la misma expresion: el de DAF y el de la plataforma, y el nombre no los
          // confunde.
          W.Report := Format('daf=%s fmx=%s',
            [BoolToStr(DafApp = App, True), 'TApplication']);
        finally
          App.Stop;
        end;
      end)
    .When('se pide el global de la variante').NoAction
    .&Then('devuelve la aplicacion DAF, y el de FireMonkey sigue siendo el suyo',
      procedure(W: TFmxWorld)
      begin
        Expect(W.Report).ToEqual('daf=True fmx=TApplication');
      end)

  .Scenario('Cuando la aplicacion se para, el global deja de apuntar a ella')
    .Given('una aplicacion que ya se ha parado',
      procedure(W: TFmxWorld)
      begin
        var B := TFmxApplication.CreateBuilder;
        var App := B.Build;
        App.Stop;
        W.Report := BoolToStr(DafApp = nil, True);
      end)
    .When('se pide el global').NoAction
    .&Then('no queda apuntando a una aplicacion muerta',
      procedure(W: TFmxWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end);

end.
