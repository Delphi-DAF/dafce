unit LayoutTokens.Feat;

// El pipeline rellena TLogEntry.Scope en CADA entrada que compone, y hoy no lo lee nadie: hay
// veinte renderers registrados y ninguno es el del ambito. Asi que "Scopes" --que el README de
// Logging anuncia y el sample HostedService ejercita dos veces-- no llega a ninguna linea.
//
// El segundo defecto es el modo de fallo del propio motor: un token para el que no hay renderer se
// resuelve a cadena VACIA, sin error ni aviso. Un destino puede escribir, rotar y no lanzar mientras
// llena el fichero de lineas sin contenido, y todo lo observable dice que funciona. Lo mismo le pasa
// a un token mal escrito en una configuracion de produccion: se pierde una columna y nada lo dice.
//
// Los renderers se registran aqui a proposito --el motor los guarda en un mapa global que llena el
// host-- porque estos escenarios son justo los que preguntan que se ve en la salida.

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting,
  Daf.NNLog;

type
  TLayoutWorld = class
  strict private
    FEnv: IHostEnvironment;
    FTarget: TTarget;
    FRule: TLogRule;
    FLogger: ILogger;
    procedure Compose(const Layout: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure TargetRenders(const Layout: string);
    procedure WriteInsideScope(const ScopeMsg, Line: string);
    procedure WriteWithoutScope(const Line: string);
    function Written: string;
    function BinPath: string;
  end;

implementation

uses
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder,
  Daf.Hosting,
  Daf.Logging,
  Daf.NNLog.Layout,
  Daf.MiniSpec;

type
  // Se queda con la linea YA RENDERIZADA, que es lo unico que contesta a "que se ve en la salida".
  TRenderTarget = class(TTarget)
  strict private
    FText: string;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    property Text: string read FText;
  end;

{ TRenderTarget }

procedure TRenderTarget.DoWrite(const Entry: TLogEntry);
begin
  FText := FText + RenderEvent(Entry);
end;

{ TLayoutWorld }

constructor TLayoutWorld.Create;
begin
  inherited Create;
  var CB: IConfigurationBuilder := TConfigurationBuilder.Create;
  var EmptyConfig: IConfiguration := CB.Build as IConfiguration;
  FEnv := THostEnvironment.Create(EmptyConfig) as IHostEnvironment;
  TLogLayoutEngine.RegisterLayoutRenderers(FEnv);
end;

destructor TLayoutWorld.Destroy;
begin
  FLogger := nil;
  FRule.Free;
  FTarget.Free;
  FEnv := nil;
  inherited;
end;

procedure TLayoutWorld.Compose(const Layout: string);
begin
  FTarget := TRenderTarget.Create;
  FTarget.Name := 'render';
  FTarget.Layout := Layout;
  FRule := TLogRule.Create;
  // A mano porque TLogRule no declara constructor: el cero de su clase base la deja desactivada y
  // con el nivel maximo en el mas bajo de todos.
  FRule.Name := '*';
  FRule.Enabled := True;
  FRule.MinLevel := TLogLevel.Trace;
  FRule.MaxLevel := TLogLevel.Critical;
  FRule.WriteTo := [FTarget];
  FLogger := TNNLogLogger.Create('Prueba', [FRule]);
end;

procedure TLayoutWorld.TargetRenders(const Layout: string);
begin
  Compose(Layout);
end;

procedure TLayoutWorld.WriteInsideScope(const ScopeMsg, Line: string);
begin
  FLogger.BeginScope(ScopeMsg,
    procedure
    begin
      FLogger.LogInformation(Line);
    end);
end;

procedure TLayoutWorld.WriteWithoutScope(const Line: string);
begin
  FLogger.LogInformation(Line);
end;

function TLayoutWorld.Written: string;
begin
  Result := TRenderTarget(FTarget).Text;
end;

function TLayoutWorld.BinPath: string;
begin
  Result := FEnv.BinPath;
end;

initialization

Feature('''
Feature El layout de NNLog escribe el ambito, y no se calla un token que nadie atiende @nnlog @layout

  Como quien adjunta contexto a un bloque de lineas con BeginScope
  Quiero que ese contexto llegue a la salida, y que un token sin renderer se note
  Para no descubrir que la feature no se ve, ni perder una columna entera por una letra
''')

.UseWorld<TLayoutWorld>

.Rule('El layout resuelve el ambito de la entrada')

  .Scenario('Una linea escrita dentro de un ambito lo lleva')
    .Given('un destino cuyo layout incluye el token del ambito',
      procedure(W: TLayoutWorld)
      begin
        W.TargetRenders('<${scope}>');
      end)
    .When('se registra una linea dentro de un ambito abierto',
      procedure(W: TLayoutWorld)
      begin
        W.WriteInsideScope('procesando pedido', 'dentro');
      end)
    .&Then('la linea escrita contiene el texto del ambito',
      procedure(W: TLayoutWorld)
      begin
        Expect(W.Written).ToContain('procesando pedido');
      end)

  .Scenario('Una linea fuera de todo ambito no inventa nada')
    .Given('un destino cuyo layout incluye el token del ambito',
      procedure(W: TLayoutWorld)
      begin
        W.TargetRenders('<${scope}>');
      end)
    .When('se registra una linea sin ningun ambito abierto',
      procedure(W: TLayoutWorld)
      begin
        W.WriteWithoutScope('suelta');
      end)
    .&Then('el token se resuelve a vacio, sin fallar',
      procedure(W: TLayoutWorld)
      begin
        Expect(W.Written).ToEqual('<>');
      end)

.Rule('Un token que nadie atiende se nota')

  .Scenario('Un token mal escrito no desaparece sin mas')
    .Given('un layout que nombra un token para el que no hay renderer',
      procedure(W: TLayoutWorld)
      begin
        W.TargetRenders('[${mesage}]');
      end)
    .When('se compone una linea',
      procedure(W: TLayoutWorld)
      begin
        W.WriteWithoutScope('da igual que diga');
      end)
    .&Then('la linea deja constancia de ese token en vez de dejar un hueco vacio',
      procedure(W: TLayoutWorld)
      begin
        Expect(W.Written).ToContain('mesage');
      end)

  .Scenario('CONTROL: un token que SI tiene renderer sigue resolviendose')
    .Given('un layout con un token atendido',
      procedure(W: TLayoutWorld)
      begin
        W.TargetRenders('[${category}]');
      end)
    .When('se compone una linea',
      procedure(W: TLayoutWorld)
      begin
        W.WriteWithoutScope('da igual que diga');
      end)
    .&Then('sale su valor, no su nombre',
      procedure(W: TLayoutWorld)
      begin
        Expect(W.Written).ToEqual('[Prueba]');
      end)

.Rule('Un token se reconoce escrito con cualquier caja')

  .Scenario('BinPath y binPath son el mismo token')
    .Given('un layout que nombra la carpeta del ejecutable con las dos cajas',
      procedure(W: TLayoutWorld)
      begin
        W.TargetRenders('[${BinPath}|${binPath}]');
      end)
    .When('se compone una linea',
      procedure(W: TLayoutWorld)
      begin
        W.WriteWithoutScope('da igual que diga');
      end)
    .&Then('las dos dan la carpeta del ejecutable',
      procedure(W: TLayoutWorld)
      begin
        Expect(W.Written).ToEqual('[' + W.BinPath + '|' + W.BinPath + ']');
      end);

end.
