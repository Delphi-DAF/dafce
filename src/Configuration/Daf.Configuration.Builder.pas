unit Daf.Configuration.Builder;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Rtti,
  Daf.Extensions.Configuration;

type
  TConfigurationBuilder = class(TInterfacedObject, IConfigurationBuilder)
  private
    FProperties: TDictionary<string, TValue>;
    FSources: TList<IConfigurationSource>;
  public
    constructor Create;
    destructor Destroy;override;

    function Add(const Source: IConfigurationSource): IConfigurationBuilder;
    function Build: IConfigurationRoot;

    function GetSources: IEnumerable<IConfigurationSource>;
    function GetProperties: TDictionary<string, TValue>;

    property Sources: IEnumerable<IConfigurationSource> read GetSources;
    property Properties: TDictionary<string, TValue> read GetProperties;
  end;

implementation

uses
  Daf.Enumerable,
  Daf.Configuration.ConfigurationRoot;

{ TConfigurationBuilder }

constructor TConfigurationBuilder.Create;
begin
  inherited Create;
  FProperties := TDictionary<string, TValue>.Create(TIStringComparer.Ordinal);
  FSources := TList<IConfigurationSource>.Create;
end;

destructor TConfigurationBuilder.Destroy;
begin
  FSources.Free;
  FProperties.Free;
  inherited;
end;

function TConfigurationBuilder.Add(const Source: IConfigurationSource): IConfigurationBuilder;
begin
  if Assigned(Source) then
    FSources.Add(Source);
  Result := Self;
end;

function TConfigurationBuilder.Build: IConfigurationRoot;
var
  Providers: TList<IConfigurationProvider>;
  Source: IConfigurationSource;
begin
  Providers := TList<IConfigurationProvider>.Create;
  for Source in FSources do
    Providers.Add(Source.Build(Self));

  Result := TConfigurationRoot.Create(Providers);
end;

function TConfigurationBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FProperties;
end;

function TConfigurationBuilder.GetSources: IEnumerable<IConfigurationSource>;
begin
  Result := ToIEnumerable<IConfigurationSource>(FSources);
end;

end.

