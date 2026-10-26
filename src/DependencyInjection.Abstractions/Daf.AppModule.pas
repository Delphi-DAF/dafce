unit Daf.AppModule;

interface
uses
  Daf.Extensions.DependencyInjection;

type
  // IAppModule se declara en Daf.Extensions.DependencyInjection, porque AddModule<T> es un
  // metodo del record IServiceCollection y no puede depender de esta unit. El alias mantiene
  // valido el `uses Daf.AppModule` que ya escribieron los consumidores: es el mismo tipo.
  IAppModule = Daf.Extensions.DependencyInjection.IAppModule;

implementation

end.
