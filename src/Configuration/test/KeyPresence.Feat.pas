unit KeyPresence.Feat;

interface

uses
  Daf.Nullable,
  Daf.Extensions.Configuration;

type
  TPresenceWorld = class
  public
    Builder: IConfigurationBuilder;
    Config: IConfigurationRoot;
    Found: TNullable<string>;
    Report: string;
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory;

{ TPresenceWorld }

constructor TPresenceWorld.Create;
begin
  inherited;
  Builder := TConfigurationBuilder.Create;
end;

// A consumer expression written against the indexer: it takes a plain string, and it
// has to keep compiling. That is the point of the last scenario -- if the indexer ever
// went nullable, this signature is what would stop accepting it.
function Shout(const S: string): string;
begin
  Result := S.ToUpper;
end;

procedure Seed(W: TPresenceWorld; const Pairs: array of string);
begin
  var Dic := TDictionary<string, string>.Create;
  var I := 0;
  while I < Length(Pairs) do
  begin
    Dic.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
  MemoryConfig.AddCollection(W.Builder, Dic);
end;

// --- Feature definition ---

initialization

Feature('''
Feature Configuration key presence @configuration @presence

  As a developer composing configuration
  I want to tell an absent key from one whose value is empty
  So that reading a key before its source was added stops looking like an empty value
''')

.UseWorld<TPresenceWorld>

.Rule('Presence at the root')

  .Scenario('A key that is not there is told apart from one that is empty')
    .Given('a configuration where Absent does not appear and Blank is the empty string',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Blank', '', 'Other', 'value']);
        W.Config := W.Builder.Build;
      end)
    .When('I ask each one for its presence',
      procedure(W: TPresenceWorld)
      begin
        W.Report := Format('Absent=%s Blank=%s',
          [BoolToStr(W.Config.Exists('Absent'), True), BoolToStr(W.Config.Exists('Blank'), True)]);
      end)
    .&Then('Absent answers no and Blank answers yes',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('Absent=False Blank=True');
      end)
    .&And('reading both through the indexer still gives the empty string',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Config['Absent'] + '|' + W.Config['Blank']).ToEqual('|');
      end)

  .Scenario('One lookup answers presence and value')
    .Given('a configuration with a key that has a value',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Features:Beta', 'on']);
        W.Config := W.Builder.Build;
      end)
    .When('I read it once',
      procedure(W: TPresenceWorld)
      begin
        W.Found := W.Config.Find('Features:Beta');
      end)
    .&Then('the single read carries both the presence and the value',
      procedure(W: TPresenceWorld)
      begin
        Expect(Format('%s/%s', [BoolToStr(W.Found.HasValue, True), W.Found.Value])).ToEqual('True/on');
      end)
    .&And('the same read reports absence for a key nobody added',
      procedure(W: TPresenceWorld)
      begin
        var Missing := W.Config.Find('Features:Alpha');
        Expect(Format('%s/%s', [BoolToStr(Missing.HasValue, True), Missing.GetValueOrDefault('<none>')]))
          .ToEqual('False/<none>');
      end)

  .Scenario('Telling them apart does not require knowing the provider')
    .Given('a configuration held only as IConfiguration, with no idea of its sources',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Blank', '']);
        W.Config := W.Builder.Build;
      end)
    .When('I ask it through that contract alone',
      procedure(W: TPresenceWorld)
      begin
        var Plain: IConfiguration := W.Config;
        W.Report := Format('Blank=%s Absent=%s',
          [BoolToStr(Plain.Exists('Blank'), True), BoolToStr(Plain.Exists('Absent'), True)]);
      end)
    .&Then('it still tells absence from empty',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('Blank=True Absent=False');
      end)

  // This is the scenario that makes the other ones worth writing: the SAME assertion
  // has to flip when the key turns up. Written through the indexer it could not --
  // absent and empty read alike, so "it is not there" would have the same green as
  // its opposite, and the suite would prove nothing.
  .Scenario('A spec can claim a key does not appear')
    .Given('a configuration that does not mention the key',
      procedure(W: TPresenceWorld)
      begin
        W.Config := W.Builder.Build;
        W.Report := 'before=' + BoolToStr(W.Config.Exists('Features:Beta'), True);
      end)
    .When('a later source brings the key in, with the empty string for a value',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Features:Beta', '']);
        W.Config := W.Builder.Build;
        W.Report := W.Report + ' after=' + BoolToStr(W.Config.Exists('Features:Beta'), True);
      end)
    .&Then('the claim held before and stops holding after',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('before=False after=True');
      end)

.Rule('Presence through a section')

  .Scenario('A section answers presence against its own path')
    .Given('a section with one empty child and one absent',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['App:Blank', '', 'App:Name', 'MyApp']);
        W.Config := W.Builder.Build;
      end)
    .When('I ask the section, not the root',
      procedure(W: TPresenceWorld)
      begin
        var Section := W.Config.GetSection('App');
        W.Report := Format('Blank=%s Absent=%s Name=%s',
          [BoolToStr(Section.Exists('Blank'), True), BoolToStr(Section.Exists('Absent'), True),
           Section.Find('Name').GetValueOrDefault('<none>')]);
      end)
    .&Then('it answers about its own children',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('Blank=True Absent=False Name=MyApp');
      end)

  .Scenario('A section that only groups exists')
    .Given('a section with no value of its own but keys underneath',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Logging:Level', 'Info', 'Logging:Path', 'app.log']);
        W.Config := W.Builder.Build;
      end)
    .When('I ask whether it exists',
      procedure(W: TPresenceWorld)
      begin
        W.Report := Format('Exists=%s OwnValue=%s',
          [BoolToStr(W.Config.Exists('Logging'), True),
           BoolToStr(W.Config.Find('Logging').HasValue, True)]);
      end)
    .&Then('it answers yes even though it has no value of its own',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('Exists=True OwnValue=False');
      end)

  .Scenario('A section no source mentions does not exist')
    .Given('a configuration with an unrelated section',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Logging:Level', 'Info']);
        W.Config := W.Builder.Build;
      end)
    .When('I ask for a section name nobody wrote').NoAction
    .&Then('it answers no',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Config.Exists('Database')).ToBeFalse;
      end)

  .Scenario('A leaf with a value exists as a section')
    .Given('a leaf key with a value and no children',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Timeout', '30']);
        W.Config := W.Builder.Build;
      end)
    .When('I ask whether it exists as a section').NoAction
    .&Then('it answers yes',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Config.Exists('Timeout')).ToBeTrue;
      end)

.Rule('The indexer stays a string')

  .Scenario('Comparing the indexer with the empty string does not pretend to tell them apart')
    .Given('an absent key and one present with an empty value',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Blank', '']);
        W.Config := W.Builder.Build;
      end)
    .When('I compare each one with the empty string',
      procedure(W: TPresenceWorld)
      begin
        W.Report := Format('Absent=%s Blank=%s',
          [BoolToStr(W.Config['Absent'] = '', True), BoolToStr(W.Config['Blank'] = '', True)]);
      end)
    .&Then('both comparisons give the same answer',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('Absent=True Blank=True');
      end)
    .&And('whoever needs to tell them apart asks for the presence instead',
      procedure(W: TPresenceWorld)
      begin
        Expect(Format('%s/%s', [BoolToStr(W.Config.Exists('Absent'), True),
          BoolToStr(W.Config.Exists('Blank'), True)])).ToEqual('False/True');
      end)

  // Assignment, comparison, concatenation, passing as const string and the setter: the
  // five shapes a consumer writes. This scenario is a compile-time check first and a
  // runtime one second -- if the indexer stopped being a string, it would not build.
  .Scenario('Existing expressions through the indexer still compile')
    .Given('a configuration with a plain value',
      procedure(W: TPresenceWorld)
      begin
        Seed(W, ['Name', 'daf']);
        W.Config := W.Builder.Build;
      end)
    .When('I use it in the shapes consumer code already uses',
      procedure(W: TPresenceWorld)
      begin
        var Copied: string := W.Config['Name'];
        var Compared := W.Config['Name'] = 'daf';
        var Joined := '<' + W.Config['Name'] + '>';
        var Passed := Shout(W.Config['Name']);
        W.Config['Name'] := W.Config['Name'] + '!';
        W.Report := Format('%s %s %s %s %s',
          [Copied, BoolToStr(Compared, True), Joined, Passed, W.Config['Name']]);
      end)
    .&Then('every one of them behaves as a string',
      procedure(W: TPresenceWorld)
      begin
        Expect(W.Report).ToEqual('daf True <daf> DAF daf!');
      end);

end.
