unit Daf.Configuration.Chained;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration;

type
  /// <summary>
  /// Fuente de configuración que encadena otra IConfiguration ya existente.
  /// </summary>
  TChainedConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    FConfiguration: IConfiguration;
  public
    constructor Create(const Configuration: IConfiguration);
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
    property Configuration: IConfiguration read FConfiguration;
  end;

  /// <summary>
  /// Provider que simplemente reexpone otra IConfiguration.
  /// </summary>
  TChainedConfigurationProvider = class(TInterfacedObject, IConfigurationProvider)
  strict private
    FConfiguration: IConfiguration;
  public
    constructor Create(const Source: TChainedConfigurationSource);
    function TryGet(const Key: string; out Value: string): Boolean;
    function GetChildKeys(const EarlierKeys: IEnumerable<string>; const ParentPath: string): IEnumerable<string>;
    procedure Load;
    procedure &Set(const Key, Value: string);
    property Configuration: IConfiguration read FConfiguration;

  end;

/// <summary>
/// Extension method para añadir un IConfiguration existente al builder.
/// </summary>
procedure AddChainedConfiguration(const Builder: IConfigurationBuilder; const Config: IConfiguration);

implementation

uses
  System.Generics.Defaults,
  Daf.Enumerable,
  Daf.Configuration.ConfigurationRoot;

procedure AddChainedConfiguration(const Builder: IConfigurationBuilder; const Config: IConfiguration);
begin
 if not Assigned(Config) then
    raise Exception.Create('Cannot chain a nil IConfiguration');
  Builder.Add(TChainedConfigurationSource.Create(Config));
end;

{ TChainedConfigurationSource }

constructor TChainedConfigurationSource.Create(const Configuration: IConfiguration);
begin
  inherited Create;
  FConfiguration := Configuration;
end;

function TChainedConfigurationSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TChainedConfigurationProvider.Create(Self);
end;

{ TChainedConfigurationProvider }

constructor TChainedConfigurationProvider.Create(const Source: TChainedConfigurationSource);
begin
  inherited Create;
  FConfiguration := Source.Configuration;
  Assert(FConfiguration <> nil);
end;

function TChainedConfigurationProvider.TryGet(const Key: string; out Value: string): Boolean;
begin
  // Asked, not inferred: a chained key whose value IS the empty string is present, and
  // inferring presence from the value would make it disappear on the way through. Both
  // read as the empty string afterwards, so the loss would not show from outside.
  var Found := FConfiguration.Find(Key);
  Value := Found.GetValueOrDefault('');
  Result := Found.HasValue;
end;

function TChainedConfigurationProvider.GetChildKeys(const EarlierKeys: IEnumerable<string>; const ParentPath: string): IEnumerable<string>;
begin
  var Section := FConfiguration.GetSection(ParentPath);

  var Keys := TList<string>.Create(TIStringComparer.Ordinal);
  try
    if Assigned(Section) and ((not Section.GetValue.IsEmpty) or Section.GetChildren.GetEnumerator.MoveNext) then
    begin
      for var Child in Section.GetChildren do
        if not Keys.Contains(Child.Key) then
          Keys.Add(Child.Key);
    end;

    if Assigned(EarlierKeys) then
      for var Key in EarlierKeys do
        if not Keys.Contains(Key) then
          Keys.Add(Key);

    Keys.Sort(TConfigurationKeyComparer.Instance);
    Result := ToIEnumerable<string>(Keys);
  except
    Keys.Free;
    raise;
  end;
end;

procedure TChainedConfigurationProvider.Load;
begin
  // No-op
end;

procedure TChainedConfigurationProvider.&Set(const Key, Value: string);
begin
  FConfiguration[Key] := Value
end;

end.

