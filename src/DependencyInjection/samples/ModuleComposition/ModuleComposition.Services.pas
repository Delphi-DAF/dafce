unit ModuleComposition.Services;

// Los tipos van en una unit y no en el .dpr por obligacion del contenedor: un tipo declarado en el
// fichero de programa no esta en la seccion interface de ninguna unit, y resolverlo aborta en
// tiempo de ejecucion con ENonPublicType.

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.AppModule.DependencyInjection;

type
  IClock = interface(IInvokable)
    ['{0ED801D1-8529-4C2F-860F-472419E770B3}']
    function Today: string;
  end;

  TSystemClock = class(TInterfacedObject, IClock)
  public
    function Today: string;
  end;

  IGreeter = interface(IInvokable)
    ['{6803CD07-1D7D-4FA0-A890-65ED845860B1}']
    function Greet(const Name: string): string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  strict private
    FClock: IClock;
  public
    constructor Create(const AClock: IClock);
    function Greet(const Name: string): string;
  end;

  IBanner = interface(IInvokable)
    ['{E9A88893-3081-4181-8279-B189E6CF33DB}']
    function Text: string;
  end;

  TBanner = class(TInterfacedObject, IBanner)
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    function Text: string;
  end;

  // Los modulos se declaran en la interface para que el linker no los descarte: un tipo que solo
  // aparece en implementation y no se referencia se elimina como codigo muerto.
  TInfrastructureModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  // Su registro resuelve IClock desde el proveedor, asi que necesita que el modulo que lo aporta
  // se haya aplicado antes.
  TDomainModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  // Solo tiene constructor con parametro: es el caso que unicamente cubre la sobrecarga de
  // instancia, porque el modulo corre antes de que exista el proveedor.
  TBannerModule = class(TAppModule)
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

implementation

uses
  System.SysUtils;

{ TSystemClock }

function TSystemClock.Today: string;
begin
  Result := FormatDateTime('yyyy-mm-dd', Now);
end;

{ TGreeter }

constructor TGreeter.Create(const AClock: IClock);
begin
  inherited Create;
  FClock := AClock;
end;

function TGreeter.Greet(const Name: string): string;
begin
  Result := Format('Hola, %s. Hoy es %s', [Name, FClock.Today]);
end;

{ TBanner }

constructor TBanner.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TBanner.Text: string;
begin
  Result := FText;
end;

{ TInfrastructureModule }

procedure TInfrastructureModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IClock, TSystemClock>;
end;

{ TDomainModule }

procedure TDomainModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IGreeter>(
    function(Provider: IServiceProvider): IGreeter
    begin
      Result := TGreeter.Create(Provider.GetRequiredService<IClock>);
    end);
end;

{ TBannerModule }

constructor TBannerModule.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

procedure TBannerModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IBanner>(TBanner.Create(FText) as IBanner);
end;

end.
