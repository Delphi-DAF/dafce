unit ChainedConfig.Feat;

interface

uses
  Daf.Extensions.Configuration;

type
  TChainedWorld = class
  public
    Origin: IConfigurationRoot;
    Builder: IConfigurationBuilder;
    Config: IConfigurationRoot;
    Report: string;
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Configuration.Chained;

{ TChainedWorld }

constructor TChainedWorld.Create;
begin
  inherited;
  Builder := TConfigurationBuilder.Create;
end;

function BuiltFrom(const Pairs: array of string): IConfigurationRoot;
begin
  var Dic := TDictionary<string, string>.Create;
  var I := 0;
  while I < Length(Pairs) do
  begin
    Dic.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
  var B: IConfigurationBuilder := TConfigurationBuilder.Create;
  MemoryConfig.AddCollection(B, Dic);
  Result := B.Build;
end;

procedure Seed(const Builder: IConfigurationBuilder; const Pairs: array of string);
begin
  var Dic := TDictionary<string, string>.Create;
  var I := 0;
  while I < Length(Pairs) do
  begin
    Dic.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
  MemoryConfig.AddCollection(Builder, Dic);
end;

// --- Feature definition ---

initialization

Feature('''
Feature Chained configuration source @configuration @chained

  As a developer composing configuration
  I want to seed a builder from an IConfiguration that is already resolved
  So that a configuration I was handed can take part in a new one without enumerating it by hand
''')

.UseWorld<TChainedWorld>

.Rule('Chaining an existing configuration')

  .Scenario('A resolved configuration is taken into another builder')
    .Given('an IConfiguration already built, with keys in it',
      procedure(W: TChainedWorld)
      begin
        W.Origin := BuiltFrom(['Db:Server', 'localhost', 'Db:Port', '5432']);
      end)
    .When('I add it as a source to a new builder and build it',
      procedure(W: TChainedWorld)
      begin
        AddChainedConfiguration(W.Builder, W.Origin);
        W.Config := W.Builder.Build;
      end)
    .&Then('its keys are read from the resulting configuration',
      procedure(W: TChainedWorld)
      begin
        Expect(W.Config['Db:Server'] + ':' + W.Config['Db:Port']).ToEqual('localhost:5432');
      end)

  .Scenario('Precedence is decided by whoever composes')
    .Given('the same key in a chained configuration and in a source added after it',
      procedure(W: TChainedWorld)
      begin
        W.Origin := BuiltFrom(['Db:Server', 'from-chain']);
        AddChainedConfiguration(W.Builder, W.Origin);
        Seed(W.Builder, ['Db:Server', 'added-after']);
      end)
    .When('I build and read the key',
      procedure(W: TChainedWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('the one added after wins',
      procedure(W: TChainedWorld)
      begin
        Expect(W.Config['Db:Server']).ToEqual('added-after');
      end)
    .&And('the chained source claims no precedence of its own',
      procedure(W: TChainedWorld)
      begin
        var Other: IConfigurationBuilder := TConfigurationBuilder.Create;
        Seed(Other, ['Db:Server', 'added-before']);
        AddChainedConfiguration(Other, W.Origin);
        Expect(Other.Build['Db:Server']).ToEqual('from-chain');
      end)

  .Scenario('Chaining nothing is refused when composing')
    .Given('a builder with nothing chained into it yet',
      procedure(W: TChainedWorld)
      begin
        Expect(Assigned(W.Builder)).ToBeTrue;
      end)
    .When('I try to chain an unassigned configuration',
      procedure(W: TChainedWorld)
      begin
        try
          AddChainedConfiguration(W.Builder, nil);
          W.Report := '<the builder accepted nil>';
        except
          on E: Exception do
            W.Report := E.Message;
        end;
      end)
    .&Then('it fails on the spot, naming the problem',
      procedure(W: TChainedWorld)
      begin
        Expect(W.Report).ToContain('Cannot chain a nil IConfiguration');
      end)

.Rule('The chained source tells an absent key from an empty one')

  .Scenario('An empty key survives the chain')
    .Given('an origin configuration with a key whose value is the empty string',
      procedure(W: TChainedWorld)
      begin
        W.Origin := BuiltFrom(['Db:Password', '']);
      end)
    .When('I chain it into another builder and ask for that key',
      procedure(W: TChainedWorld)
      begin
        AddChainedConfiguration(W.Builder, W.Origin);
        W.Config := W.Builder.Build;
      end)
    .&Then('it answers that the key is there',
      procedure(W: TChainedWorld)
      begin
        Expect(W.Config.Exists('Db:Password')).ToBeTrue;
      end)

  .Scenario('A key absent in the origin stays absent')
    .Given('an origin configuration that does not have the key',
      procedure(W: TChainedWorld)
      begin
        W.Origin := BuiltFrom(['Db:Server', 'localhost']);
      end)
    .When('I chain it and ask for the key',
      procedure(W: TChainedWorld)
      begin
        AddChainedConfiguration(W.Builder, W.Origin);
        W.Config := W.Builder.Build;
      end)
    .&Then('it answers that it is not there',
      procedure(W: TChainedWorld)
      begin
        Expect(W.Config.Exists('Db:Password')).ToBeFalse;
      end)

  // This is the one that goes red on the old provider: inferring presence from the value
  // not being empty made the chained source say it did not have the key, and the value
  // from the source added BEFORE it won. Both read as the empty string, so nothing showed.
  .Scenario('A source added earlier does not cover an empty chained key')
    .Given('an empty chained key, and the same key with a value in a source added before',
      procedure(W: TChainedWorld)
      begin
        W.Origin := BuiltFrom(['Db:Password', '']);
        Seed(W.Builder, ['Db:Password', 'from-earlier']);
        AddChainedConfiguration(W.Builder, W.Origin);
      end)
    .When('I build and read the key',
      procedure(W: TChainedWorld)
      begin
        W.Config := W.Builder.Build;
        W.Report := Format('value=[%s] exists=%s',
          [W.Config['Db:Password'], BoolToStr(W.Config.Exists('Db:Password'), True)]);
      end)
    .&Then('the empty value wins, because the chained source was added later and does have it',
      procedure(W: TChainedWorld)
      begin
        Expect(W.Report).ToEqual('value=[] exists=True');
      end);

end.
