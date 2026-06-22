program MediatRSample;

{$STRONGLINKTYPES ON}
uses
  Vcl.Forms,
  Daf.Rtti,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  Daf.MediatR.DependencyInjection,
  Daf.MediatR.Contracts,
  MediatRSample.AppServices in 'MediatRSample.AppServices.pas',
  MediatRSample.MainForm in 'MediatRSample.MainForm.pas' {MainForm},
  MediatRSample.Requests in 'MediatRSample.Requests.pas',
  MediatRSample.Handlers in 'MediatRSample.Handlers.pas',
  MediatRSample.Notifications in 'MediatRSample.Notifications.pas',
  MediatRSample.Customer in 'MediatRSample.Customer.pas',
  MediatRSample.Behaviors in 'MediatRSample.Behaviors.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  var ServiceCollection: IServiceCollection := TServiceCollection.Create;

  MediatR.AddTo(ServiceCollection);
  MediatR.AddTo(ServiceCollection, _T.PackageOf<TMainForm>);

  // Behaviors run outermost-first.
  // TLoggingBehavior   — global (all requests)
  // TValidationBehavior — typed void (TAddCustomerCommand only)
  // TQueryResultBehavior — typed response (TCustomerQuery only)
  MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);
  MediatR.AddBehavior(ServiceCollection, TValidationBehavior);
  MediatR.AddBehavior(ServiceCollection, TQueryResultBehavior);

  ServiceCollection.AddSingleton<IAppLog, TAppLog>;
  ServiceCollection.AddSingleton<ICustomerStore, TCustomerStore>;

  var ServiceProvider := ServiceCollection.BuildServiceProvider;
  var Mediator := ServiceProvider.GetRequiredService<IMediatorImpl>;
  var AppLog   := ServiceProvider.GetRequiredService<IAppLog>;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.Initialize(Mediator, AppLog);
  Application.Run;

  ServiceProvider.Shutdown;
end.
