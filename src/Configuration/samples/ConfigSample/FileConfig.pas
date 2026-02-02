unit FileConfig;

interface
uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder,
  Daf.Configuration.Binder,
  Daf.Configuration.Json,
  Daf.Configuration.Ini;

type
  TFileConfigSample = class
  private
  public
    class procedure Run;
  end;

  {$M+}
  TUser = class
  public
    Name: string;
    Roles: TList<string>;
    constructor Create;
    destructor Destroy; override;
  end;

  TAppSettings = class
  public
    Host: string;
    Port: Integer;
    UseSsl: Boolean;
    Users: TObjectList<TUser>;
    constructor Create;
    destructor Destroy; override;
  end;
  {$M-}

implementation
uses
  System.IOUtils;

constructor TUser.Create;
begin
  inherited Create;
  Roles := TList<string>.Create;
end;

destructor TUser.Destroy;
begin
  Roles.Free;
  inherited Destroy;
end;

constructor TAppSettings.Create;
begin
  inherited Create;
  Users := TObjectList<TUser>.Create;
end;

destructor TAppSettings.Destroy;
begin
  Users.Free;
  inherited Destroy;
end;

procedure DumpAppSettings(const Settings: TAppSettings);
begin
  Writeln('Host: ', Settings.Host);
  Writeln('Port: ', Settings.Port);
  Writeln('UseSsl: ', BoolToStr(Settings.UseSsl, True));
  for var user in Settings.Users do
  begin
    Writeln('User: ', user.Name);
    for var role in user.Roles do
      Writeln('  Role: ', role);
  end;
end;

{ TFileConfigSample }

procedure Dump(Title: string; const Config: IConfiguration; const Indent: string = '');
begin
  if not Title.IsEmpty then
  begin
    WriteLn('-----------------');
    WriteLn(Title);
    WriteLn('-----------------');
  end;

  for var section in Config.GetChildren do
  begin
    if Section.HasChildren then
    begin
      Writeln(Indent + section.Key + ':');
      Dump('', section, Indent + '  ');
    end
    else
      Writeln(Indent + section.Key + ' = ' + section.Value);
  end;
end;

class procedure TFileConfigSample.Run;
var
  Config: IConfigurationRoot;
  ConfigBuilder: IConfigurationBuilder;
begin
  var SampleFilesPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), TPath.GetFileNameWithoutExtension(ParamStr(0)));

  ConfigBuilder := TConfigurationBuilder.Create;

  Config := JsonConfig.AddFile(ConfigBuilder, TPath.Combine(SampleFilesPath, 'appsettings.json')).Build;

  Dump('JSON', Config);

//  ConfigBuilder := TConfigurationBuilder.Create;

  Config := IniConfig.AddFile(ConfigBuilder, TPath.Combine(SampleFilesPath, 'appsettings.ini')).Build;

  Dump('INI', Config);

 Writeln; Writeln('--- Binding to object ---');
  var Settings := TConfigurationBinder.Bind<TAppSettings>(Config, 'AppSettings', boFields);
  try
    DumpAppSettings(Settings);
  finally
    Settings.Free;
  end;
end;

end.
