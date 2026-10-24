unit Daf.Logging.Builder;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  Daf.Logging.Provider;

type
  // Componer el pipeline sin un solo destino. No es un fallo al escribir --el pipeline nunca falla
  // al escribir-- sino un error de composicion, y se levanta donde se comete.
  ELoggingCompositionError = class(Exception);

  // ILoggingBuilder vive en Daf.Logging.Abstractions, que es donde .NET lo tiene y lo que permite
  // que el contrato de la familia de aplicaciones lo exponga sin que un .Abstractions requiera
  // una implementacion. Aqui se queda quien lo implementa y quien lo consume.

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

procedure AddLogging(const Services: IServiceCollection; const Add: TProc<ILoggingBuilder>); overload;
// Para quien ya sostiene el componedor y lo ha ido llenando a lo largo del arranque --el estilo
// inmediato-- en vez de llenarlo dentro de un callback. La comprobacion de "hubo algun
// proveedor" y su mensaje viven aqui una sola vez, que es lo que evita tener dos redacciones
// del mismo error segun por donde se entre.
procedure AddLogging(const Services: IServiceCollection; const Builder: TLoggingBuilder); overload;

implementation

uses
  Daf.Rtti,
  Daf.Logging;

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
  AddLogging(Services, Builder);
end;

procedure AddLogging(const Services: IServiceCollection; const Builder: TLoggingBuilder);
begin
  // Se comprueba una sola vez, aqui, al acabar de componer. Ni al escribir --lo enseñaria lejos del
  // descuido y una vez por entrada, que es como un aviso se convierte en decorado-- ni por cada
  // logger creado. Y al componer todavia hay a quien decirselo: esto corre dentro de Build,
  // que informa por el canal de arranque y relanza.
  if not Builder.HasProviders then
    raise ELoggingCompositionError.Create('AddLogging was given no providers: the pipeline would ' +
      'accept every entry and discard it. Register a provider, or if the silence is deliberate say ' +
      'so with Builder.AddNullProvider.');
  Services.AddSingleton<ILoggerFactory>((Builder as ILoggingBuilder).Build);
  // UN registro para todos los servicios, con los argumentos de tipo sin fijar. Es lo que hace
  // util a ILogger<T>: sin esto haria falta una linea de composicion por cada clase que quisiera
  // un registrador con su nombre.
  // TryAdd y no Add: quien componga la aplicacion manda sobre el default de la libreria.
  Services.TryAddSingleton<ILogger<_>, TTypedLogger<_>>;
end;

end.
