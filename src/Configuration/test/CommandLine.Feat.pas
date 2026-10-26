unit CommandLine.Feat;

interface

uses
  Daf.Extensions.Configuration;

type
  TCmdLineWorld = class
  public
    Builder: IConfigurationBuilder;
    Config: IConfigurationRoot;
    Other: IConfigurationRoot;
    JsonFile: string;
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   El unico proveedor de Config. Hace falta porque IConfiguration devuelve cadena
    ///   vacia tanto para una clave ausente como para una cuyo valor ES la cadena vacia:
    ///   solo TryGet distingue las dos cosas, y hay escenarios que van justo de eso.
    /// </summary>
    function Provider: IConfigurationProvider;
    function Has(const Key: string): Boolean;
    function KeyCount: Integer;
    /// <summary>Escribe un appsettings temporal y devuelve su ruta.</summary>
    function WriteJson(const Body: string): string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.CommandLine;

{ TCmdLineWorld }

constructor TCmdLineWorld.Create;
begin
  inherited;
  Builder := TConfigurationBuilder.Create;
end;

destructor TCmdLineWorld.Destroy;
begin
  if (JsonFile <> '') and TFile.Exists(JsonFile) then
    TFile.Delete(JsonFile);
  inherited;
end;

function TCmdLineWorld.WriteJson(const Body: string): string;
begin
  JsonFile := TPath.ChangeExtension(TPath.GetTempFileName, '.json');
  TFile.WriteAllText(JsonFile, Body, TEncoding.UTF8);
  Result := JsonFile;
end;

function TCmdLineWorld.Provider: IConfigurationProvider;
begin
  Result := nil;
  for var P in Config.Providers do
    Result := P;
end;

function TCmdLineWorld.Has(const Key: string): Boolean;
begin
  var Dummy: string;
  Result := Provider.TryGet(Key, Dummy);
end;

function TCmdLineWorld.KeyCount: Integer;
begin
  Result := 0;
  for var Child in Config.GetChildren do
    Inc(Result);
end;

// --- Feature definition ---

initialization

Feature('''
Feature Configuration from the command line @configuration @commandline

  As a developer
  I want the process arguments read as a configuration source
  So I can override any setting when invoking the executable, without touching a file
''')

.UseWorld<TCmdLineWorld>

.Rule('The command line is one more configuration source')

  .Scenario('A command-line value arrives as configuration')
    .Given('the process is invoked naming a key and its value',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['--Greeting=hello']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('the key reads with that value',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config['Greeting']).ToEqual('hello');
      end)

  .Scenario('Precedence is decided by whoever composes, not by the provider')
    .Given('the same key in a file and on the command line',
      procedure(W: TCmdLineWorld)
      begin
        W.WriteJson('{ "Greeting": "from file" }');
      end)
    .When('the file is added first and the command line second, and then the other way round',
      procedure(W: TCmdLineWorld)
      begin
        JsonConfig.AddFile(W.Builder, W.JsonFile);
        CommandLineConfig.AddArgs(W.Builder, ['--Greeting=from args']);
        W.Config := W.Builder.Build;

        var Reversed: IConfigurationBuilder := TConfigurationBuilder.Create;
        CommandLineConfig.AddArgs(Reversed, ['--Greeting=from args']);
        JsonConfig.AddFile(Reversed, W.JsonFile);
        W.Other := Reversed.Build;
      end)
    .&Then('the source added last wins',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config['Greeting']).ToEqual('from args');
      end)
    .&And('reversing the order reverses the result',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Other['Greeting']).ToEqual('from file');
      end)

  .Scenario('An argument that names no key does not invent one')
    .Given('arguments that follow no key-and-value shape',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['bare', 'also-bare', '--']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('no key appears at all',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.KeyCount).ToEqual(0);
      end)
    .&And('reading them does not fail the startup',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Has('bare')).ToBeFalse;
      end)

  .Scenario('The same key repeated keeps the last one')
    .Given('the command line names one key twice with different values',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['--Port=80', '--Port=8080']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('the last appearance wins',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config['Port']).ToEqual('8080');
      end)

.Rule('The accepted syntax is the one a .NET user expects')

  .Scenario('Equivalent forms give the same result')
    .Given('the same key and value written in every accepted form',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder,
          ['A=1', '--B=1', '--C', '1', '/D=1', '/E', '1']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('all five forms produce the same value',
      procedure(W: TCmdLineWorld)
      begin
        for var Key in ['A', 'B', 'C', 'D', 'E'] do
          Expect(W.Config[Key]).ToEqual('1');
      end)

  .Scenario('A short alias lands on its long key')
    .Given('an alias declared for a key, and the process invoked with the alias',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['-v', 'Debug'],
          [TSwitchMapping.Create('-v', 'Logging:LogLevel')]);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('the value appears under the long key',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config[TConfigurationPath.Combine(['Logging', 'LogLevel'])]).ToEqual('Debug');
      end)
    .&And('it does not appear under the alias',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Has('-v')).ToBeFalse;
      end)

  .Scenario('A short switch with no alias declared is ignored')
    .Given('single-dash arguments with no alias, one of them carrying an equals sign',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['-x', 'value', '-y=value']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('neither appears as configuration',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.KeyCount).ToEqual(0);
      end)
    .&And('the startup does not fail even for the one with the equals sign',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Has('y')).ToBeFalse;
      end)

  .Scenario('A key with nothing after the equals sign is an empty string')
    .Given('an argument ending in the equals sign',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['--ConnectionStrings:Default=']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('the key exists',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Has(TConfigurationPath.Combine(['ConnectionStrings', 'Default']))).ToBeTrue;
      end)
    .&And('its value is the empty string',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config[TConfigurationPath.Combine(['ConnectionStrings', 'Default'])]).ToEqual('');
      end)

  .Scenario('A duplicated alias is a composition error, not an invocation one')
    .Given('the same alias declared twice').NoAction
    .When('the source is assembled')
    .&Then('it fails while composing, naming the alias')

.Rule('A nested key uses the separator the rest of DAF uses')

  .Scenario('A nested key from the command line and from a file are the same key')
    .Given('a nested value declared in a file',
      procedure(W: TCmdLineWorld)
      begin
        W.WriteJson('{ "Logging": { "LogLevel": { "Default": "Warning" } } }');
        JsonConfig.AddFile(W.Builder, W.JsonFile);
      end)
    .When('the same value is passed on the command line',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder,
          ['--' + TConfigurationPath.Combine(['Logging', 'LogLevel', 'Default']) + '=Debug']);
        W.Config := W.Builder.Build;
      end)
    .&Then('both sources write the same key',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config[TConfigurationPath.Combine(['Logging', 'LogLevel', 'Default'])]).ToEqual('Debug');
      end)
    .&And('the section reads the same either way',
      procedure(W: TCmdLineWorld)
      begin
        var Section := W.Config.GetSection('Logging').GetSection('LogLevel');
        Expect(Section['Default']).ToEqual('Debug');
      end)

  .Scenario('The key does not distinguish case')
    .Given('a key written with one capitalisation',
      procedure(W: TCmdLineWorld)
      begin
        CommandLineConfig.AddArgs(W.Builder, ['--greeting=hello']);
      end)
    .When('I build the configuration',
      procedure(W: TCmdLineWorld)
      begin
        W.Config := W.Builder.Build;
      end)
    .&Then('it is the same key read with another capitalisation',
      procedure(W: TCmdLineWorld)
      begin
        Expect(W.Config['GREETING']).ToEqual('hello');
      end);

end.
