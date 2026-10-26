unit BinderKeyName.Feat;

{
  Que clave lee el enlazador para cada miembro.

  Los defectos de partida --'sin-tocar', 30-- no son el cero de su tipo, y eso es lo que hace que
  los escenarios midan algo: sobre un defecto de '' o 0, "se leyo la clave que el atributo nombra"
  y "no se leyo nada" dan el mismo verde y el mismo fallo.
}

interface

uses
  Daf.Extensions.Configuration;

type
  {$M+}
  /// <summary>
  ///   La seccion anidada. El atributo va sobre el miembro que la sostiene, asi que lo que se
  ///   renombra es la seccion entera y no sus hijos.
  /// </summary>
  TCredentialsSettings = class
  private
    FUser: string;
  public
    constructor Create;
    property User: string read FUser write FUser;
  end;

  /// <summary>
  ///   Un miembro con atributo, uno sin el y uno compuesto con atributo. El sin atributo es el
  ///   control de que el renombrado no alcanza a quien no lo pide.
  /// </summary>
  TConnectionSettings = class
  private
    FConnectionString: string;
    FTimeout: Integer;
    FCredentials: TCredentialsSettings;
  public
    constructor Create;
    destructor Destroy; override;
    [ConfigurationKeyName('connection-string')]
    property ConnectionString: string read FConnectionString write FConnectionString;
    property Timeout: Integer read FTimeout write FTimeout;
    [ConfigurationKeyName('Auth')]
    property Credentials: TCredentialsSettings read FCredentials write FCredentials;
  end;

  /// <summary>
  ///   Enlazado por campos. .NET declara el atributo solo para propiedades porque alli no hay
  ///   enlace por campos; aqui los dos modos comparten TRttiDataMember, asi que el atributo vale
  ///   para los dos.
  /// </summary>
  TFieldSettings = class
  public
    [ConfigurationKeyName('log-level')]
    Level: string;
    Plain: string;
    constructor Create;
  end;
  {$M-}

  TKeyNameWorld = class
  public
    Config: IConfigurationRoot;
    Settings: TConnectionSettings;
    Fields: TFieldSettings;
    Report: string;
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

{ TCredentialsSettings }

constructor TCredentialsSettings.Create;
begin
  inherited Create;
  FUser := 'sin-tocar';
end;

{ TConnectionSettings }

constructor TConnectionSettings.Create;
begin
  inherited Create;
  FConnectionString := 'sin-tocar';
  FTimeout := 30;
end;

destructor TConnectionSettings.Destroy;
begin
  FCredentials.Free;
  inherited;
end;

{ TFieldSettings }

constructor TFieldSettings.Create;
begin
  inherited Create;
  Level := 'sin-tocar';
  Plain := 'sin-tocar';
end;

{ TKeyNameWorld }

destructor TKeyNameWorld.Destroy;
begin
  Settings.Free;
  Fields.Free;
  inherited;
end;

function ConfigOf(const Pairs: array of string): IConfigurationRoot;
begin
  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  var Dic := TDictionary<string, string>.Create;
  var I := 0;
  while I < Length(Pairs) do
  begin
    Dic.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
  MemoryConfig.AddCollection(Builder, Dic);
  Result := Builder.Build;
end;

// --- Feature definition ---

initialization

Feature('''
Feature Configuration binder key name @configuration @binder @keyname

  As a developer binding settings
  I want a member to declare the configuration key it is read from
  So that a key the file writes as 'connection-string' reaches a property Delphi cannot name that way
''')

.UseWorld<TKeyNameWorld>

.Rule('A member with the attribute is read by the key the attribute names')

  .Scenario('The attribute names the key')
    .Given('a configuration that carries the key the attribute names',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['connection-string', 'Server=db']);
      end)
    .When('the configuration is bound',
      procedure(W: TKeyNameWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TConnectionSettings>(W.Config);
        W.Report := Format('ConnectionString=%s Timeout=%d', [W.Settings.ConnectionString, W.Settings.Timeout]);
      end)
    .&Then('the renamed member carries the value and the other one keeps its default',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('ConnectionString=Server=db Timeout=30');
      end)

  // El escenario que separa REEMPLAZAR de PROBAR-Y-CAER: una implementacion que usara el nombre
  // del miembro cuando el atributo no encuentra dato daria verde en todos los demas y rojo aqui.
  .Scenario('The member name no longer answers for it')
    .Given('a configuration that carries the member name instead of the key the attribute names',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['ConnectionString', 'por-el-nombre-del-miembro']);
      end)
    .When('the configuration is bound',
      procedure(W: TKeyNameWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TConnectionSettings>(W.Config);
        W.Report := W.Settings.ConnectionString;
      end)
    .&Then('the member keeps what its constructor put there',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('sin-tocar');
      end)

  .Scenario('Both keys present, and the attribute is the one that answers')
    .Given('a configuration carrying the key the attribute names and the member name too',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['connection-string', 'por-el-atributo',
                              'ConnectionString', 'por-el-nombre-del-miembro']);
      end)
    .When('the configuration is bound',
      procedure(W: TKeyNameWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TConnectionSettings>(W.Config);
        W.Report := W.Settings.ConnectionString;
      end)
    .&Then('the value that arrives is the one under the key the attribute names',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('por-el-atributo');
      end)

  .Scenario('A member without the attribute is still read by its own name')
    .Given('a configuration that carries the name of the member that declares no attribute',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['Timeout', '90']);
      end)
    .When('the configuration is bound',
      procedure(W: TKeyNameWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TConnectionSettings>(W.Config);
        W.Report := Format('ConnectionString=%s Timeout=%d', [W.Settings.ConnectionString, W.Settings.Timeout]);
      end)
    .&Then('it carries the value, and the renamed one is untouched',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('ConnectionString=sin-tocar Timeout=90');
      end)

.Rule('The attribute on a composite member renames its whole section')

  .Scenario('The children are read under the renamed section')
    .Given('a configuration whose section is the one the attribute names',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['Auth:User', 'admin']);
      end)
    .When('the configuration is bound',
      procedure(W: TKeyNameWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TConnectionSettings>(W.Config);
        W.Report := W.Settings.Credentials.User;
      end)
    .&Then('the child of the renamed section is written',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('admin');
      end)

  .Scenario('The member name does not name the section either')
    .Given('a configuration whose section is named after the member',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['Credentials:User', 'admin']);
      end)
    .When('the configuration is bound',
      procedure(W: TKeyNameWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TConnectionSettings>(W.Config);
        W.Report := W.Settings.Credentials.User;
      end)
    .&Then('the child object is built and keeps its own default',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('sin-tocar');
      end)

.Rule('Binding by fields reads the attribute the same way')

  .Scenario('A field declares the key it is read from')
    .Given('a configuration carrying the key the attribute names and the name of the plain field',
      procedure(W: TKeyNameWorld)
      begin
        W.Config := ConfigOf(['log-level', 'Debug', 'Plain', 'por-su-nombre']);
      end)
    .When('the configuration is bound by fields',
      procedure(W: TKeyNameWorld)
      begin
        W.Fields := TConfigurationBinder.Bind<TFieldSettings>(W.Config, '', boFields);
        W.Report := Format('Level=%s Plain=%s', [W.Fields.Level, W.Fields.Plain]);
      end)
    .&Then('the renamed field and the plain one both carry their value',
      procedure(W: TKeyNameWorld)
      begin
        Expect(W.Report).ToEqual('Level=Debug Plain=por-su-nombre');
      end);

end.
