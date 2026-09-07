unit LegacyEngine;

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting;

type
  IClock = interface(IInvokable)
    ['{71660C4D-B2C8-45F2-9D3F-22CBB0D1FEF3}']
    function Stamp: string;
  end;

  /// <summary>
  /// El motor que no se puede cambiar: una clase estatica que resuelve por su cuenta a
  /// traves de un IServiceProvider global, y a la que hay que sembrarle ese proveedor
  /// cuando ya esta construido.
  /// </summary>
  TLegacyEngine = class
  strict private
    class var FProvider: IServiceProvider;
  public
    class procedure Seed(const Provider: IServiceProvider);
    class function Evaluate(const Input: string): string;
  end;

/// <summary>
/// Lo unico que el ejecutable escribe. Que ahi dentro haya una tarea de arranque, y como
/// se llame, es asunto del paquete: quien lo usa no tiene por que saberlo.
/// </summary>
function AddLegacyEngine(const Services: IServiceCollection): IServiceCollection;

implementation

uses
  System.SysUtils;

type
  TClock = class(TInterfacedObject, IClock)
  public
    function Stamp: string;
  end;

  /// Fase 2 del arranque: corre una vez, con el proveedor ya construido, y no tiene nada
  /// que parar. Si esto fuera un IHostedService haria falta un Stop vacio que mentiria.
  TSeedEngineTask = class(TInterfacedObject, IStartupTask)
  strict private
    FProvider: IServiceProvider;
  public
    constructor Create(const Provider: IServiceProvider);
    procedure Run;
  end;

function AddLegacyEngine(const Services: IServiceCollection): IServiceCollection;
begin
  Result := Services;
  // El default del paquete: si el arranque ya registro un IClock, TryAdd no lo pisa.
  Services.TryAddSingleton<IClock, TClock>;
  Services.AddStartupTask<TSeedEngineTask>;
end;

{ TClock }

function TClock.Stamp: string;
begin
  Result := 'T0';
end;

{ TSeedEngineTask }

constructor TSeedEngineTask.Create(const Provider: IServiceProvider);
begin
  inherited Create;
  FProvider := Provider;
end;

procedure TSeedEngineTask.Run;
begin
  TLegacyEngine.Seed(FProvider);
end;

{ TLegacyEngine }

class procedure TLegacyEngine.Seed(const Provider: IServiceProvider);
begin
  FProvider := Provider;
end;

class function TLegacyEngine.Evaluate(const Input: string): string;
begin
  if FProvider = nil then
    raise Exception.Create('TLegacyEngine was never seeded with a service provider');

  Result := Format('[%s] %s', [FProvider.GetRequiredService<IClock>.Stamp, Input]);
end;

end.
