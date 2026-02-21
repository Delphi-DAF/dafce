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

  TConfigWorld = class
  public
    Builder: IConfigurationBuilder;
    Config: IConfigurationRoot;
    BoundObj: TMyConfig;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Configuration.Binder;

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
      end);

end.
