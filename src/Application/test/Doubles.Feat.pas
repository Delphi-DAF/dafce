unit Doubles.Feat;

interface

uses
  Daf.Extensions.Hosting,
  Daf.Extensions.DependencyInjection;

type
  IGreeter = interface(IInvokable)
    ['{6E2C7B41-9A55-4E3D-8C10-5F7B2A9D4C63}']
    function Greet: string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  public
    function Greet: string;
  end;

  /// Un IHost que anota lo que le piden y nunca bloquea. Existe por una razon concreta y no
  /// por gusto de doblar: THost.WaitForShutdown espera un Ctrl+C que nada dentro del proceso
  /// puede señalar, asi que una spec que llamase al de verdad se colgaria en vez de ponerse
  /// roja. Con el espia, "Run espero el apagado" es una traza que se compara.
  TSpyHost = class(TInterfacedObject, IHost)
  strict private
    FServices: IServiceProvider;
    FTrace: string;
    procedure Note(const Step: string);
  public
    constructor Create(const Services: IServiceProvider);
    function GetServices: IServiceProvider;
    procedure Start;
    procedure Stop;
    procedure WaitForShutdown;
    property Trace: string read FTrace;
  end;

/// Lo minimo que TDafApplication exige de su host: su constructor resuelve IHostEnvironment e
/// IConfiguration antes de devolver, asi que un host espia sin ellos no llega a construirse.
function SpecServiceProvider: IServiceProvider;

implementation

uses
  System.SysUtils,
  Daf.Hosting,
  Daf.DependencyInjection,
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder;

{ TGreeter }

function TGreeter.Greet: string;
begin
  Result := 'hola';
end;

{ TSpyHost }

constructor TSpyHost.Create(const Services: IServiceProvider);
begin
  inherited Create;
  FServices := Services;
end;

procedure TSpyHost.Note(const Step: string);
begin
  if FTrace <> '' then
    FTrace := FTrace + ',';
  FTrace := FTrace + Step;
end;

function TSpyHost.GetServices: IServiceProvider;
begin
  Result := FServices;
end;

procedure TSpyHost.Start;
begin
  Note('start');
end;

procedure TSpyHost.Stop;
begin
  Note('stop');
end;

procedure TSpyHost.WaitForShutdown;
begin
  Note('waitforshutdown');
end;

function SpecServiceProvider: IServiceProvider;
begin
  var Services: IServiceCollection := TServiceCollection.Create;
  // TConfigurationBuilder es contado por referencia: encadenar sobre el Create deja el objeto
  // sin dueño y se pierde. Se sostiene en una variable de interfaz a proposito.
  var ConfigBuilder: IConfigurationBuilder := TConfigurationBuilder.Create;
  var Config: IConfiguration := ConfigBuilder.Build as IConfiguration;
  // El entorno SALE de la configuracion de host, asi que hay que darsela.
  Services.AddSingleton<IHostEnvironment>(THostEnvironment.Create(Config) as IHostEnvironment);
  Services.AddSingleton<IConfiguration>(Config);
  Result := Services.BuildServiceProvider;
end;

end.
