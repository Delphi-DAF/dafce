unit Daf.Configuration.Manager;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  Daf.Nullable,
  Daf.Extensions.Configuration,
  Daf.Configuration.ConfigurationRoot;

type
  /// <summary>
  /// Configuration that builds and reads over a single object: what Add brings in is
  /// readable by the very next line, with nothing built in between.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///   It descends from <see cref="TConfigurationRoot"/> rather than reimplementing the
  ///   read side, and that is load-bearing twice over: the traversal of the providers
  ///   stays in one place, and <c>TConfigurationSection</c> reaches its root through a
  ///   hard cast to <c>TConfigurationRoot</c> — a section handed out by a manager that
  ///   were not one would fail there and nowhere near here.
  ///   </para>
  ///   <para>
  ///   The inherited provider enumeration wraps the very list this class appends to, and
  ///   enumerating it is live, so a provider added at any moment is seen by every read
  ///   that follows without anything being rebuilt.
  ///   </para>
  /// </remarks>
  TConfigurationManager = class(TConfigurationRoot, IConfigurationManager, IConfigurationBuilder)
  private
    FProviderList: TList<IConfigurationProvider>;
    FSources: TList<IConfigurationSource>;
    FSourcesEnum: IEnumerable<IConfigurationSource>;
    FProperties: TDictionary<string, TValue>;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Source: IConfigurationSource): IConfigurationBuilder;
    function Build: IConfigurationRoot;
    function GetSources: IEnumerable<IConfigurationSource>;
    function GetProperties: TDictionary<string, TValue>;

    property Sources: IEnumerable<IConfigurationSource> read GetSources;
    property Properties: TDictionary<string, TValue> read GetProperties;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.Extensions.Logging,
  Daf.Enumerable;

{ TConfigurationManager }

constructor TConfigurationManager.Create;
begin
  FProviderList := TList<IConfigurationProvider>.Create;
  // The inherited enumeration takes ownership of this list, so it is not freed here.
  // It starts empty, which is also why the inherited constructor loads nothing.
  inherited Create(FProviderList);

  FSources := TList<IConfigurationSource>.Create;
  FSourcesEnum := ToIEnumerable<IConfigurationSource>(FSources);  // owns FSources from here on
  FProperties := TDictionary<string, TValue>.Create(TIStringComparer.Ordinal);
end;

destructor TConfigurationManager.Destroy;
begin
  FProperties.Free;
  // FSources belongs to FSourcesEnum and FProviderList to the inherited enumeration:
  // freeing either here would free it twice.
  inherited;
end;

function TConfigurationManager.Add(const Source: IConfigurationSource): IConfigurationBuilder;
begin
  Result := Self;
  if not Assigned(Source) then Exit;

  FSources.Add(Source);

  // Only the NEW provider is built and loaded. Rebuilding the root on every Add would
  // re-open every file already added -- and, worse, throw away everything written through
  // SetItem, because those values live in the providers that the rebuild replaces. No
  // error, no exception, and a read afterwards returns what an absent key would.
  try
    var Provider := Source.Build(Self);
    Provider.Load;
    FProviderList.Add(Provider);
  except
    on E: Exception do
    begin
      // Este es el punto donde se ABRE el fichero, y con el estilo inmediato eso pasa aqui y no
      // dentro de Build: la fuente se carga cuando se anade. El informe tiene que estar donde
      // ocurre el fallo -- en una aplicacion de escritorio, sin consola
      // y sin depurador, el canal de arranque es lo unico que queda entre el fallo y el silencio.
      //
      // Y despues se relanza: la propagacion es la otra mitad del informe.
      BootstrapLog.LoggerFor(ClassName).LogError(E, 'a configuration source could not be added');
      raise;
    end;
  end;
end;

function TConfigurationManager.Build: IConfigurationRoot;
begin
  // Itself -- there is nothing left to build, every source was built as it arrived. And
  // NOT kept in a field: an object holding a reference to itself never reaches refcount
  // zero.
  Result := Self;
end;

function TConfigurationManager.GetSources: IEnumerable<IConfigurationSource>;
begin
  Result := FSourcesEnum;
end;

function TConfigurationManager.GetProperties: TDictionary<string, TValue>;
begin
  Result := FProperties;
end;

end.
