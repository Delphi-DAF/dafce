unit ConfigManager.Feat;

interface

uses
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Configuration.ConfigurationProvider;

type
  /// <summary>
  /// A source whose loading is observable: it is the only way to tell "the manager added
  /// the new provider" from "the manager rebuilt everything and it happened to look the
  /// same", which reads identically from the values alone.
  /// </summary>
  TCountingProvider = class(TConfigurationProvider)
  private
    FPairs: TArray<string>;
  public
    class var LoadCount: Integer;
    constructor Create(const Pairs: TArray<string>);
    procedure Load; override;
  end;

  TCountingSource = class(TInterfacedObject, IConfigurationSource)
  private
    FPairs: TArray<string>;
  public
    constructor Create(const Pairs: TArray<string>);
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  TManagerWorld = class
  public
    Manager: IConfigurationManager;
    Report: string;
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Configuration.Manager,
  Daf.Configuration.Memory;

{ TCountingProvider }

constructor TCountingProvider.Create(const Pairs: TArray<string>);
begin
  inherited Create;
  FPairs := Pairs;
end;

procedure TCountingProvider.Load;
begin
  Inc(LoadCount);
  var I := 0;
  while I < Length(FPairs) do
  begin
    Data.AddOrSetValue(FPairs[I], FPairs[I + 1]);
    Inc(I, 2);
  end;
end;

{ TCountingSource }

constructor TCountingSource.Create(const Pairs: TArray<string>);
begin
  inherited Create;
  FPairs := Pairs;
end;

function TCountingSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TCountingProvider.Create(FPairs);
end;

{ TManagerWorld }

constructor TManagerWorld.Create;
begin
  inherited;
  Manager := TConfigurationManager.Create;
end;

procedure Seed(const Manager: IConfigurationManager; const Pairs: array of string);
begin
  var Dic := TDictionary<string, string>.Create;
  var I := 0;
  while I < Length(Pairs) do
  begin
    Dic.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
  MemoryConfig.AddCollection(Manager, Dic);
end;

// --- Feature definition ---

initialization

Feature('''
Feature Configuration manager @configuration @manager

  As a developer composing startup
  I want the configuration I am building to be readable while I build it
  So that I can pick a source from what is already configured, without a deferred callback
''')

.UseWorld<TManagerWorld>

.Rule('Reading while building')

  .Scenario('What is added is read straight afterwards')
    .Given('a configuration manager').NoAction
    .When('I add a source',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Features:Beta', 'on']);
      end)
    .&Then('the very next read already sees its keys',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Manager['Features:Beta']).ToEqual('on');
      end)
    .&And('nothing had to be built in between',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Manager.Exists('Features:Beta')).ToBeTrue;
      end)

  // The one case the deferred style paid a callback for. If this could not be written,
  // dropping that style would have taken capability away without putting it back.
  .Scenario('A source is chosen from what is already configured')
    .Given('a manager that already knows the environment',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['DAF_APP_ENV', 'Staging']);
      end)
    .When('I read it to decide which settings to add',
      procedure(W: TManagerWorld)
      begin
        var Env := W.Manager['DAF_APP_ENV'];
        Seed(W.Manager, ['Db:Server', 'db-' + Env.ToLower]);
      end)
    .&Then('the chosen source is in and readable',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Manager['Db:Server']).ToEqual('db-staging');
      end)

  .Scenario('Holding it by both of its contracts at once is safe')
    .Given('a manager with a key in it',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Kept', 'yes']);
      end)
    .When('I hold it as a builder and as a configuration at the same time',
      procedure(W: TManagerWorld)
      begin
        var AsBuilder: IConfigurationBuilder := W.Manager;
        var AsConfig: IConfiguration := W.Manager as IConfiguration;
        var AsRoot: IConfigurationRoot := AsBuilder.Build;
        W.Report := Format('%s/%s/%s', [AsConfig['Kept'], AsRoot['Kept'], W.Manager['Kept']]);
      end)
    .&Then('all three see the same object, alive',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Report).ToEqual('yes/yes/yes');
      end)
    .&And('Build hands back the manager itself rather than a second object',
      procedure(W: TManagerWorld)
      begin
        // Written through the values, because that is what a caller can observe: a value
        // set on the manager after Build has to be visible through what Build returned.
        var Root := W.Manager.Build;
        W.Manager['Late'] := 'written';
        Expect(Root['Late']).ToEqual('written');
      end)

.Rule('Adding a source neither reloads nor discards')

  // The failure mode a naive rebuild causes without raising anything: values written
  // through the configuration live in the providers, and a rebuild replaces them.
  .Scenario('What was written survives a later add')
    .Given('a manager with a value written into it',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Db:Server', 'localhost']);
        W.Manager['Db:Server'] := 'written-by-hand';
      end)
    .When('another source is added that does not mention that key',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Unrelated', 'value']);
      end)
    .&Then('the key still holds what was written',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Manager['Db:Server']).ToEqual('written-by-hand');
      end)

  .Scenario('A source already added is not loaded again')
    .Given('a source whose loading is observable',
      procedure(W: TManagerWorld)
      begin
        TCountingProvider.LoadCount := 0;
        W.Manager.Add(TCountingSource.Create(['First', '1']));
      end)
    .When('two more sources are added after it',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Second', '2']);
        Seed(W.Manager, ['Third', '3']);
      end)
    .&Then('it was loaded exactly once',
      procedure(W: TManagerWorld)
      begin
        Expect(TCountingProvider.LoadCount).ToEqual(1);
      end)
    .&And('all three sources are readable',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Manager['First'] + W.Manager['Second'] + W.Manager['Third']).ToEqual('123');
      end)

  .Scenario('Precedence is the order the sources were added in')
    .Given('two sources bringing the same key with different values',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Db:Server', 'first']);
      end)
    .When('the second one is added after the first',
      procedure(W: TManagerWorld)
      begin
        Seed(W.Manager, ['Db:Server', 'second']);
      end)
    .&Then('the last one added wins',
      procedure(W: TManagerWorld)
      begin
        Expect(W.Manager['Db:Server']).ToEqual('second');
      end);

end.
