unit BinderConversion.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Configuration;

type
  TSpecMode = (smSlow, smFast);

  TConversionSettings = class
  private
    FRatio: Double;
    FPort: Integer;
    FMode: TSpecMode;
  public
    property Ratio: Double read FRatio write FRatio;
    property Port: Integer read FPort write FPort;
    property Mode: TSpecMode read FMode write FMode;
  end;

  TConversionWorld = class
  strict private
    FSavedDecimal: Char;
    FSavedThousand: Char;
  public
    Builder: IConfigurationBuilder;
    /// What the binding left, or the exception it raised.
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
    procedure Configure(const Key, Value: string);
    procedure BindSettings(const Report: TFunc<TConversionSettings, string>);
  end;

implementation

uses
  System.TypInfo,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Configuration.Binder;

{ TConversionWorld }

constructor TConversionWorld.Create;
begin
  inherited;
  FSavedDecimal := FormatSettings.DecimalSeparator;
  FSavedThousand := FormatSettings.ThousandSeparator;
  Builder := TConfigurationBuilder.Create;
end;

destructor TConversionWorld.Destroy;
begin
  FormatSettings.DecimalSeparator := FSavedDecimal;
  FormatSettings.ThousandSeparator := FSavedThousand;
  inherited;
end;

procedure TConversionWorld.Configure(const Key, Value: string);
begin
  var Data := TDictionary<string, string>.Create;
  Data.Add(Key, Value);
  MemoryConfig.AddCollection(Builder, Data);
end;

procedure TConversionWorld.BindSettings(const Report: TFunc<TConversionSettings, string>);
begin
  var Config: IConfiguration := Builder.Build;
  try
    var Settings := TConfigurationBinder.Bind<TConversionSettings>(Config);
    try
      Outcome := 'bound ' + Report(Settings);
    finally
      Settings.Free;
    end;
  except
    on E: Exception do
      Outcome := 'raised ' + E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature Converting a configuration value to its member's type @configuration @binder

  As a developer binding settings from configuration
  I want a value converted the way .NET does it, and a value that does not convert to fail by name
  So a setting never ends up silently different from what the file says
''')

.UseWorld<TConversionWorld>

.Rule('Binding converts with the invariant culture')

  .Scenario('A decimal in a process with a decimal comma')
    .Given('a process whose format uses a comma as the decimal separator, and the key Ratio with the value 1.5',
      procedure(W: TConversionWorld)
      begin
        FormatSettings.DecimalSeparator := ',';
        FormatSettings.ThousandSeparator := '.';
        W.Configure('Ratio', '1.5');
      end)
    .When('it is bound to settings with a member Ratio: Double',
      procedure(W: TConversionWorld)
      begin
        W.BindSettings(
          function(S: TConversionSettings): string
          begin
            Result := 'Ratio=' + FloatToStr(S.Ratio, TFormatSettings.Invariant);
          end);
      end)
    .&Then('the member is 1.5',
      procedure(W: TConversionWorld)
      begin
        Expect(W.Outcome).ToEqual('bound Ratio=1.5');
      end)

.Rule('A value that does not convert fails the binding naming the key')

  .Scenario('An integer that is not a number')
    .Given('the key Port with the value abc',
      procedure(W: TConversionWorld)
      begin
        W.Configure('Port', 'abc');
      end)
    .When('it is bound to settings with a member Port: Integer',
      procedure(W: TConversionWorld)
      begin
        W.BindSettings(
          function(S: TConversionSettings): string
          begin
            Result := 'Port=' + IntToStr(S.Port);
          end);
      end)
    .&Then('the binding fails with an error that names Port, abc and the type',
      procedure(W: TConversionWorld)
      begin
        Expect(W.Outcome).ToContain('raised ');
        Expect(W.Outcome).ToContain('''Port''');
        Expect(W.Outcome).ToContain('''abc''');
        Expect(W.Outcome).ToContain('Integer');
      end)

.Rule('An enumeration binds by its name or its number')

  .Scenario('A name that does not exist')
    .Given('the key Mode with a value that is none of the enumeration''s names',
      procedure(W: TConversionWorld)
      begin
        W.Configure('Mode', 'smTurbo');
      end)
    .When('it is bound to settings with a member of that enumeration',
      procedure(W: TConversionWorld)
      begin
        W.BindSettings(
          function(S: TConversionSettings): string
          begin
            Result := 'Mode=' + GetEnumName(TypeInfo(TSpecMode), Ord(S.Mode));
          end);
      end)
    .&Then('the binding fails with an error that names Mode',
      procedure(W: TConversionWorld)
      begin
        Expect(W.Outcome).ToContain('raised ');
        Expect(W.Outcome).ToContain('''Mode''');
      end)

  .Scenario('A name in another case')
    .Given('the key Mode with the name of the enumeration''s second value, in lower case',
      procedure(W: TConversionWorld)
      begin
        W.Configure('Mode', 'smfast');
      end)
    .When('it is bound to settings with a member of that enumeration',
      procedure(W: TConversionWorld)
      begin
        W.BindSettings(
          function(S: TConversionSettings): string
          begin
            Result := 'Mode=' + GetEnumName(TypeInfo(TSpecMode), Ord(S.Mode));
          end);
      end)
    .&Then('the member is the second value',
      procedure(W: TConversionWorld)
      begin
        Expect(W.Outcome).ToEqual('bound Mode=smFast');
      end);

end.
