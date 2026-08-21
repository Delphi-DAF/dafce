unit DAF.Logging.Builder;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  DAF.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  DAF.Logging.Provider;

type
  // Componer el pipeline sin un solo destino. No es un fallo al escribir --el pipeline nunca falla
  // al escribir-- sino un error de composicion, y se levanta donde se comete.
  ELoggingCompositionError = class(Exception);

  ILoggingBuilder = interface(IInvokable)
    ['{F989B61A-8A02-4C1C-AF3D-DA94CF097B1C}']
    procedure AddProvider(const Provider: ILoggerProvider);
    // Descartar a proposito. Sin esto, componer sin proveedores es indistinguible de olvidarse el
    // AddProvider, y lo segundo se paga meses despues buscando unos registros que nunca existieron.
    procedure AddNullProvider;
    function Build: ILoggerFactory;
  end;

  TLoggingBuilder = class(TInterfacedObject, ILoggingBuilder)
  private
    FFactory: TMultiProviderLoggerFactory;
    // La factoria vive por cuenta de esta referencia mientras se compone. 
    FOwned: ILoggerFactory;
  public
    constructor Create;
    procedure AddProvider(const Provider: ILoggerProvider);
    procedure AddNullProvider;
    // No esta en ILoggingBuilder: lo pregunta AddLogging al terminar de componer, no el consumidor.
    function HasProviders: Boolean;
    function Build: ILoggerFactory;
  end;

procedure AddLogging(const Services: IServiceCollection; const Add: TProc<ILoggingBuilder>);

implementation

{ TLoggingBuilder }

constructor TLoggingBuilder.Create;
begin
  inherited Create;
  FFactory := TMultiProviderLoggerFactory.Create;
  FOwned := FFactory;
end;

procedure TLoggingBuilder.AddProvider(const Provider: ILoggerProvider);
begin
  FFactory.AddProvider(Provider);
end;

procedure TLoggingBuilder.AddNullProvider;
begin
  AddProvider(TNullLoggerProvider.Create);
end;

function TLoggingBuilder.HasProviders: Boolean;
begin
  Result := FFactory.HasProviders;
end;

function TLoggingBuilder.Build: ILoggerFactory;
begin
  Result := FOwned;
end;

procedure AddLogging(const  Services: IServiceCollection; const Add: TProc<ILoggingBuilder>);
var
  Builder: TLoggingBuilder;
  Configure: ILoggingBuilder;
begin
  Builder := TLoggingBuilder.Create;
  Configure := Builder;
  Add(Configure);
  // Se comprueba una sola vez, aqui, al acabar de componer. Ni al escribir --lo enseñaria lejos del
  // descuido y una vez por entrada, que es como un aviso se convierte en decorado-- ni por cada
  // logger creado. Y al componer todavia hay a quien decirselo: esto corre dentro de
  // IHostBuilder.Build, que informa por el canal de arranque y relanza.
  if not Builder.HasProviders then
    raise ELoggingCompositionError.Create('AddLogging was given no providers: the pipeline would ' +
      'accept every entry and discard it. Register a provider, or if the silence is deliberate say ' +
      'so with Builder.AddNullProvider.');
  Services.AddSingleton<ILoggerFactory>(Configure.Build);
end;

end.
