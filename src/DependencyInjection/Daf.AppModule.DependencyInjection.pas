unit Daf.AppModule.DependencyInjection;

interface
uses
  System.TypInfo,
  System.Rtti,
  Daf.Extensions.DependencyInjection,
  Daf.AppModule;

type
  TAppModule = class(TInterfacedObject, IAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection);virtual;
  end;

  TAppModuleServiceCollection = record helper for IServiceCollection
  public
    procedure AddModules(Package: TRttiPackage);
      deprecated 'Aplica cada modulo por su nombre: Services.AddModule<TMiModulo>';
  end;

    // In case of previous helper goes shadowed
  AppModules = class
  public
    class procedure Add(ServiceCollection: IServiceCollection; Package: TRttiPackage);
      deprecated 'Aplica cada modulo por su nombre: Services.AddModule<TMiModulo>';
  end;

implementation
uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Rtti;


{ TAppModule }

procedure TAppModule.AddServices(const ServiceCollection: IServiceCollection);
begin

end;

{ TAppModuleServiceCollection }

procedure TAppModuleServiceCollection.AddModules(Package: TRttiPackage);
begin
  var ServiceCollection := Self;
  // La contabilidad de paquetes ya recorridos vive en la coleccion, no en una global de
  // unit: dos hosts construidos en el mismo proceso deben registrar lo mismo.
  if not ServiceCollection.TryMarkVisited('AppModules', Package.Name) then Exit;
  Package.DiscoverImpl<IAppModule>(False,True,
    procedure(RIntf: TRttiInterfaceType; AClass: TRttiInstanceType)
    begin
      var Instance := AClass.MetaclassType.Create;
      var Module: IAppModule;
      Supports(Instance, IAppModule, Module);
      Module.AddServices(ServiceCollection);
  end);
end;

{ AppModules }

class procedure AppModules.Add(ServiceCollection: IServiceCollection; Package: TRttiPackage);
begin
{$WARN SYMBOL_DEPRECATED OFF}
  ServiceCollection.AddModules(Package);
{$WARN SYMBOL_DEPRECATED ON}
end;

end.
