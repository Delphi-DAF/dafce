unit Configuration.Feat;

interface

uses
  Daf.Extensions.Configuration;

type
  TMyConfig = class
  private
    FNamedProperty: string;
    procedure SetNamedProperty(const Value: string);
  public
    property NamedProperty: string read FNamedProperty write SetNamedProperty;
  end;

  TNumbersConfig = class
  private
    FNumbers: TArray<Integer>;
  public
    property Numbers: TArray<Integer> read FNumbers write FNumbers;
  end;

  TConfigWorld = class
  public
    Builder: IConfigurationBuilder;
    Config: IConfigurationRoot;
    BoundObj: TMyConfig;
    Path: string;
    SectionKey: string;
    ParentPath: string;
    Report: string;
    JsonFile: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Configuration.Json,
  Daf.Configuration.Binder;

/// The keys of a section's children, in the order the section gives them.
function ChildKeys(const Config: IConfiguration; const Path: string): string;
begin
  Result := '';
  for var Child in Config.GetSection(Path).GetChildren do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + Child.Key;
  end;
end;

function Collection(const Pairs: array of string): TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;
  var I := 0;
  while I < High(Pairs) do
  begin
    Result.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
end;

{ TMyConfig }

procedure TMyConfig.SetNamedProperty(const Value: string);
begin
  FNamedProperty := Value;
end;

{ TConfigWorld }

constructor TConfigWorld.Create;
begin
  inherited;
  Builder := TConfigurationBuilder.Create;
end;

destructor TConfigWorld.Destroy;
begin
  BoundObj.Free;
  if (JsonFile <> '') and TFile.Exists(JsonFile) then
    TFile.Delete(JsonFile);
  inherited;
end;

// --- Feature definition ---

initialization

Feature('''
Feature Configuration @configuration

  As a developer
  I want to build and query configuration values
  So I can configure application behavior from multiple sources
''')

.UseWorld<TConfigWorld>

.Rule('ConfigurationBuilder')

  .Scenario('Build empty configuration')
    .Given('a configuration builder').NoAction
    .When('I build the configuration')
    .&Then('the configuration should be created')

  .Scenario('Add in-memory collection and retrieve value')
    .Given('a configuration builder with memory data',
      procedure(W: TConfigWorld)
      var
        Dic: TDictionary<string, string>;
      begin
        Dic := TDictionary<string, string>.Create;
        Dic.Add('key1', 'value1');
        Dic.Add('section:key2', 'value2');
        MemoryConfig.AddCollection(W.Builder, Dic);
      end)
    .When('I build the configuration')
    .&Then('I can retrieve a simple key',
      procedure(W: TConfigWorld)
      begin
        Expect(W.Config['key1']).ToEqual('value1');
      end)
    .&And('I can retrieve a section key',
      procedure(W: TConfigWorld)
      begin
        Expect(W.Config['section:key2']).ToEqual('value2');
      end)

  .Scenario('Get section from configuration')
    .Given('a configuration with section data',
      procedure(W: TConfigWorld)
      var
        Dic: TDictionary<string, string>;
      begin
        Dic := TDictionary<string, string>.Create;
        Dic.Add('app:name', 'MyApp');
        Dic.Add('app:version', '1.0');
        MemoryConfig.AddCollection(W.Builder, Dic);
      end)
    .When('I build the configuration')
    .&Then('the section should have the expected key',
      procedure(W: TConfigWorld)
      var
        Section: IConfigurationSection;
      begin
        Section := W.Config.GetSection('app');
        Expect(Section.Key).ToEqual('app');
      end)
    .&And('the section should have children',
      procedure(W: TConfigWorld)
      var
        Section: IConfigurationSection;
      begin
        Section := W.Config.GetSection('app');
        Expect(Section.HasChildren).ToBeTrue;
      end)

.Rule('TConfigurationPath')

  // Not one ':' written by hand in this scenario: the key is composed with Combine and
  // taken apart with GetSectionKey/GetParentPath. The day the declared delimiter and the
  // one those functions use stop being the same, this is what turns red -- the alternative
  // is producing keys that do not match, in silence.
  .Scenario('Compose and decompose a key with the declared delimiter')
    .Given('a path composed from two segments',
      procedure(W: TConfigWorld)
      begin
        W.Path := TConfigurationPath.Combine(['parent', 'child']);
      end)
    .When('I take the path apart',
      procedure(W: TConfigWorld)
      begin
        W.SectionKey := TConfigurationPath.GetSectionKey(W.Path);
        W.ParentPath := TConfigurationPath.GetParentPath(W.Path);
      end)
    .&Then('the section key is the last segment',
      procedure(W: TConfigWorld)
      begin
        Expect(W.SectionKey).ToEqual('child');
      end)
    .&And('the parent path is everything before it',
      procedure(W: TConfigWorld)
      begin
        Expect(W.ParentPath).ToEqual('parent');
      end)

.Rule('ConfigurationBinder')

  .Scenario('Bind configuration to object properties')
    .Given('a configuration with matching property names',
      procedure(W: TConfigWorld)
      var
        Dic: TDictionary<string, string>;
      begin
        Dic := TDictionary<string, string>.Create;
        Dic.Add('namedproperty', 'bound value');
        MemoryConfig.AddCollection(W.Builder, Dic);
        W.Config := W.Builder.Build;
      end)
    .When('I bind the configuration to an object',
      procedure(W: TConfigWorld)
      begin
        W.BoundObj := TConfigurationBinder.Bind<TMyConfig>(W.Config);
      end)
    .&Then('the object property should have the configured value',
      procedure(W: TConfigWorld)
      begin
        Expect(W.BoundObj.NamedProperty).ToEqual('bound value');
      end)

.Rule('A section''s children follow the order of .NET')

  .Scenario('An array of twelve elements')
    .Given('a configuration with a JSON array of twelve integers',
      procedure(W: TConfigWorld)
      begin
        W.JsonFile := TPath.Combine(TPath.GetTempPath, 'daf-config-order-' + TGuid.NewGuid.ToString + '.json');
        TFile.WriteAllText(W.JsonFile, '{ "Numbers": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] }');
        JsonConfig.AddFile(W.Builder, W.JsonFile);
        W.Config := W.Builder.Build;
      end)
    .When('it is bound to a TArray<Integer> member',
      procedure(W: TConfigWorld)
      begin
        var Bound := TConfigurationBinder.Bind<TNumbersConfig>(W.Config);
        try
          for var N in Bound.Numbers do
            W.Report := W.Report + IntToStr(N) + ',';
        finally
          Bound.Free;
        end;
      end)
    .&Then('the array holds the twelve in the order of the file',
      procedure(W: TConfigWorld)
      begin
        Expect(W.Report).ToEqual('1,2,3,4,5,6,7,8,9,10,11,12,');
      end)

  .Scenario('Numbers go by their value and before names')
    .Given('a section with the keys 10, b, 2 and A',
      procedure(W: TConfigWorld)
      begin
        MemoryConfig.AddCollection(W.Builder, Collection(['S:10', 'ten', 'S:b', 'bee', 'S:2', 'two', 'S:A', 'ay']));
        W.Config := W.Builder.Build;
      end)
    .When('its children are read',
      procedure(W: TConfigWorld)
      begin
        W.Report := ChildKeys(W.Config, 'S');
      end)
    .&Then('they come in the order 2, 10, A, b',
      procedure(W: TConfigWorld)
      begin
        Expect(W.Report).ToEqual('2,10,A,b');
      end)

.Rule('The order of the children does not depend on which source brought each key')

  .Scenario('An index the second source brings')
    .Given('a first source with the elements 0 and 2 of an array, and a second one with the 1',
      procedure(W: TConfigWorld)
      begin
        MemoryConfig.AddCollection(W.Builder, Collection(['Arr:0', 'zero', 'Arr:2', 'two']));
        MemoryConfig.AddCollection(W.Builder, Collection(['Arr:1', 'one']));
        W.Config := W.Builder.Build;
      end)
    .When('the array''s children are read',
      procedure(W: TConfigWorld)
      begin
        W.Report := ChildKeys(W.Config, 'Arr');
      end)
    .&Then('they come in the order 0, 1, 2',
      procedure(W: TConfigWorld)
      begin
        Expect(W.Report).ToEqual('0,1,2');
      end);

end.
