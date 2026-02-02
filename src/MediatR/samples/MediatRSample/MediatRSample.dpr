program MediatRSample;

{$STRONGLINKTYPES ON}
uses
  Vcl.Forms,
  System.SysUtils,
  Daf.Rtti,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  Daf.MediatR.DependencyInjection,
  Daf.MediatR.Contracts,
  MediatRSample.MainForm in 'MediatRSample.MainForm.pas' {MainForm},
  MediatRSample.Requests in 'MediatRSample.Requests.pas',
  MediatRSample.Handlers in 'MediatRSample.Handlers.pas',
  MediatRSample.Customer in 'MediatRSample.Customer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  // Configurar el contenedor de servicios
  var ServiceCollection: IServiceCollection := TServiceCollection.Create;
  MediatR.AddTo(ServiceCollection);
  MediatR.AddTo(ServiceCollection, _T.PackageOf<TMainForm>);
  ServiceCollection.AddSingleton<ICustomerStore, TCustomerStore>;

  // Construir el proveedor de servicios
  var ServiceProvider := ServiceCollection.BuildServiceProvider;

  // Listos para obtener servicios
  var Mediator := ServiceProvider.GetRequiredService<IMediatorImpl>;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.Initialize(Mediator);
  Application.Run;
  // Liberar servicios gestionados por el contenedor
  ServiceProvider.Shutdown;
end.