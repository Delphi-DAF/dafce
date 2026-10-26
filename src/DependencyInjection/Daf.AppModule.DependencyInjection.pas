unit Daf.AppModule.DependencyInjection;

interface
uses
  Daf.Extensions.DependencyInjection,
  Daf.AppModule;

type
  TAppModule = class(TInterfacedObject, IAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection);virtual;
  end;

implementation

{ TAppModule }

procedure TAppModule.AddServices(const ServiceCollection: IServiceCollection);
begin

end;

end.
