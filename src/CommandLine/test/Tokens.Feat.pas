unit Tokens.Feat;

interface

uses
  Daf.CommandLine.Tokens;

type
  /// <summary>A grammar reduced to the three questions the rewriting is allowed to ask.</summary>
  /// <remarks>
  ///   That the whole of it fits in twenty lines IS the point of design D1: the
  ///   rewriting can be told what it needs to know without a command, an option or a
  ///   parse anywhere near it, so what these specs measure is the rewriting and not the
  ///   grammar walking on top of it.
  /// </remarks>
  TDeclaredTokens = class(TInterfacedObject, IKnownTokens)
  private
    FFlags: TArray<string>;
    FValued: TArray<string>;
    function Holds(const List: TArray<string>; const Token: string): Boolean;
  public
    constructor Create(const Flags, Valued: array of string);
    function Knows(const Token: string): Boolean;
    function KnowsShort(const Letter: Char): Boolean;
    function ShortTakesValue(const Letter: Char): Boolean;
  end;

  TTokensWorld = class
  public
    Known: IKnownTokens;
    Output: TArray<string>;
    Errors: TArray<string>;
    procedure Declare(const Flags, Valued: array of string);
    procedure Normalize(const Tokens: array of string);
    /// <summary>
    ///   The tokens as one line. Every assertion below goes through it on purpose:
    ///   ToBeEmpty does not print what it found, so a spec that failed on it could not
    ///   name what came out instead.
    /// </summary>
    function Report: string;
    function ErrorReport: string;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TDeclaredTokens }

constructor TDeclaredTokens.Create(const Flags, Valued: array of string);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FFlags, Length(Flags));
  for I := Low(Flags) to High(Flags) do
    FFlags[I - Low(Flags)] := Flags[I];
  SetLength(FValued, Length(Valued));
  for I := Low(Valued) to High(Valued) do
    FValued[I - Low(Valued)] := Valued[I];
end;

function TDeclaredTokens.Holds(const List: TArray<string>; const Token: string): Boolean;
var
  Candidate: string;
begin
  for Candidate in List do
    if SameText(Candidate, Token) then
      Exit(True);
  Result := False;
end;

function TDeclaredTokens.Knows(const Token: string): Boolean;
begin
  Result := Holds(FFlags, Token) or Holds(FValued, Token);
end;

function TDeclaredTokens.KnowsShort(const Letter: Char): Boolean;
begin
  Result := Knows(ShortPrefix + Letter);
end;

function TDeclaredTokens.ShortTakesValue(const Letter: Char): Boolean;
begin
  Result := Holds(FValued, ShortPrefix + Letter);
end;

{ TTokensWorld }

procedure TTokensWorld.Declare(const Flags, Valued: array of string);
begin
  Known := TDeclaredTokens.Create(Flags, Valued);
end;

procedure TTokensWorld.Normalize(const Tokens: array of string);
var
  Given: TArray<string>;
  I: Integer;
begin
  SetLength(Given, Length(Tokens));
  for I := Low(Tokens) to High(Tokens) do
    Given[I - Low(Tokens)] := Tokens[I];
  Output := TTokenNormalizer.Normalize(Given, Known, Errors);
end;

function TTokensWorld.Report: string;
begin
  Result := '[' + string.Join(' ', Output) + ']';
end;

function TTokensWorld.ErrorReport: string;
begin
  Result := '[' + string.Join(' | ', Errors) + ']';
end;

initialization

Feature('''
Feature Command line tokenizing @commandline @tokens

  As someone reviewing a change that makes the parser more permissive
  I want the rewriting of tokens specified apart from the grammar
  So I can tell what a token becomes without a parse in the way
''')

.UseWorld<TTokensWorld>

.Rule('a list of tokens goes in and a list of tokens comes out')

  .Scenario('A token that already names something declared is left alone')
    .Given('a declared option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--file']);
      end)
    .When('the invocation writes it in the form the parser already reads',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['--file', 'main.pas']);
      end)
    .&Then('nothing is rewritten',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[--file main.pas]');
        Expect(W.ErrorReport).ToEqual('[]');
      end)

  .Scenario('What is not an option is left alone')
    .Given('a declared option',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--amount']);
      end)
    .When('the invocation carries a negative number and a plain value',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['--amount', '-5', 'app.dpr']);
      end)
    .&Then('they come out as they went in, dashes and all',
      procedure(W: TTokensWorld)
      begin
        // -5 is a value, not an unknown option. It is the first thing any rewriting
        // could take away, so it is the first thing asserted.
        Expect(W.Report).ToEqual('[--amount -5 app.dpr]');
      end)

  .Scenario('An empty invocation')
    .Given('any grammar',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-v'], []);
      end)
    .When('there are no tokens at all',
      procedure(W: TTokensWorld)
      begin
        W.Normalize([]);
      end)
    .&Then('nothing comes out and nothing is refused',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[]');
        Expect(W.ErrorReport).ToEqual('[]');
      end)

  .Scenario('Behind the end of options nothing is rewritten')
    .Given('short options declared, so a bundle would otherwise be split',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f', '-d', '-x'], []);
      end)
    .When('the invocation writes the bundle behind the end of options',
      procedure(W: TTokensWorld)
      begin
        W.Normalize([EndOfOptions, '-fdx']);
      end)
    .&Then('it stays the single token it was written as',
      procedure(W: TTokensWorld)
      begin
        // This is the escape hatch: whatever a
        // new reading takes away, `--` gives back.
        Expect(W.Report).ToEqual('[-- -fdx]');
      end)

.Rule('a value can be delimited by a colon, or glued to a one-character alias')

  .Scenario('The colon becomes the delimiter the parser already reads')
    .Given('a declared option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--verbosity']);
      end)
    .When('the token writes the value after a colon',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['--verbosity:quiet']);
      end)
    .&Then('it comes out in the canonical form, as one token',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[--verbosity=quiet]');
      end)

  .Scenario('Only the first delimiter delimits')
    .Given('a declared option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--url']);
      end)
    .When('the value itself carries colons',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['--url:http://example.org:8080']);
      end)
    .&Then('the value arrives whole',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[--url=http://example.org:8080]');
      end)

  .Scenario('A colon in a token nobody declared is not a delimiter')
    .Given('a grammar that declares no such option',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--file']);
      end)
    .When('the token looks like a configuration key',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['--Logging:LogLevel=Debug']);
      end)
    .&Then('it is left exactly as written, to fail as the unknown option it is',
      procedure(W: TTokensWorld)
      begin
        // Reading a colon as a delimiter here would turn an unknown option into a
        // half-understood one, and `--Logging` is not what anybody declared.
        Expect(W.Report).ToEqual('[--Logging:LogLevel=Debug]');
      end)

  .Scenario('A one-character alias glued to its value')
    .Given('a one-character option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['-v']);
      end)
    .When('the token glues the value to it',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-vquiet']);
      end)
    .&Then('it comes out as the alias and its value, in canonical form',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[-v=quiet]');
      end)

  .Scenario('A one-character flag takes nothing glued to it')
    .Given('a one-character option that takes no value',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f'], []);
      end)
    .When('a token starts with it and carries letters nobody declared',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-fzz']);
      end)
    .&Then('nothing is glued to a flag, and the token stays whole',
      procedure(W: TTokensWorld)
      begin
        // What happens to it belongs to the bundling rule, not to this one: a flag has
        // no value to swallow.
        Expect(W.Report).ToNotEqual('[-f=zz]');
      end)

.Rule('one-character options can be written as a group')

  .Scenario('Three flags in one token')
    .Given('three declared one-character flags',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f', '-d', '-x'], []);
      end)
    .When('the token writes them together behind one dash',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-fdx']);
      end)
    .&Then('three tokens come out, in the order they were written',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[-f -d -x]');
      end)

  .Scenario('A group whose last option takes a value')
    .Given('two flags and a one-character option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f', '-d'], ['-o']);
      end)
    .When('the group ends with the one that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-fdo', 'out.exe']);
      end)
    .&Then('the group is split and the value stays the next token, for the last of them',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[-f -d -o out.exe]');
      end)

  .Scenario('A letter of the group that nobody declared')
    .Given('a group in which one letter is not a declared option',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f', '-d'], []);
      end)
    .When('the token is rewritten',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-fdz']);
      end)
    .&Then('it is refused naming that letter',
      procedure(W: TTokensWorld)
      begin
        Expect(W.ErrorReport).ToContain('-z');
        Expect(W.ErrorReport).ToContain('-fdz');
      end)
    .&And('none of the other letters comes out taken',
      procedure(W: TTokensWorld)
      begin
        // Accepting the letters that exist and keeping quiet about the one that does
        // not would leave the caller believing they asked for something they did not
        // (design D2).
        Expect(W.Report).ToEqual('[]');
      end)

  .Scenario('A token whose letters are none of them declared')
    .Given('a grammar declaring none of those letters',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f'], []);
      end)
    .When('the token is rewritten',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-abc']);
      end)
    .&Then('it is not a group at all and stays whole',
      procedure(W: TTokensWorld)
      begin
        // With nothing of it declared there is no group to refuse: it is the unknown
        // option it has always been, and the parser gets to say so by its own name
        // rather than blame one of its letters.
        Expect(W.Report).ToEqual('[-abc]');
        Expect(W.ErrorReport).ToEqual('[]');
      end)

.Rule('the Windows prefix names an option too')

  .Scenario('A long name written with a slash')
    .Given('a declared option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--verbosity']);
      end)
    .When('the token names it with a slash and a colon',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['/verbosity:quiet']);
      end)
    .&Then('it comes out as the declared name in canonical form',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[--verbosity=quiet]');
      end)

  .Scenario('A one-character alias written with a slash')
    .Given('a declared one-character flag',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f'], []);
      end)
    .When('the token names it with a slash',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['/f']);
      end)
    .&Then('it comes out as the alias that was declared',
      procedure(W: TTokensWorld)
      begin
        Expect(W.Report).ToEqual('[-f]');
      end)

  .Scenario('A value that starts with a slash and is nobody option')
    .Given('a grammar declaring no option by that name',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['--out']);
      end)
    .When('the token is a POSIX absolute path',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['/tmp/app.dpr']);
      end)
    .&Then('it stays exactly what it was',
      procedure(W: TTokensWorld)
      begin
        // This is the cost of admitting the slash, and the whole of what bounds it: a
        // slash names an option only when what follows it was declared (design D3).
        Expect(W.Report).ToEqual('[/tmp/app.dpr]');
        Expect(W.ErrorReport).ToEqual('[]');
      end)

.Rule('where two readings of a token compete, which one wins is written down here')

  .Scenario('A one-character alias with an explicit delimiter')
    .Given('a one-character option that takes a value',
      procedure(W: TTokensWorld)
      begin
        W.Declare([], ['-v']);
      end)
    .When('the token writes the delimiter as well as gluing nothing',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-v=quiet']);
      end)
    .&Then('the delimiter wins and the value does not keep it',
      procedure(W: TTokensWorld)
      begin
        // Read as a glued value instead, this would bind `=quiet`. Both readings fit
        // the token; only one of them is right, so it is asserted rather than left to
        // whichever if happens to come first.
        Expect(W.Report).ToEqual('[-v=quiet]');
      end)

  .Scenario('A glued value beats a bundle that would also fit')
    .Given('an option that takes a value, and every letter of its value declared as a flag too',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-q', '-u', '-i', '-e', '-t'], ['-v']);
      end)
    .When('the token could be read either way',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-vquiet']);
      end)
    .&Then('it is the value of the option that takes one, not five flags',
      procedure(W: TTokensWorld)
      begin
        // getopt reads `-ovalue` this way, and the alternative is worse: a grammar
        // would change what an invocation MEANS by declaring an unrelated flag.
        Expect(W.Report).ToEqual('[-v=quiet]');
      end)

  .Scenario('A token that is itself a declared option is never split')
    .Given('an option named with two characters, whose letters are also declared apart',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-f', '-d', '-fd'], []);
      end)
    .When('the token is written',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-fd']);
      end)
    .&Then('it stays the option it names',
      procedure(W: TTokensWorld)
      begin
        // This is the guard the whole change rests on: a form added here can only ever
        // claim a token that meant nothing before.
        Expect(W.Report).ToEqual('[-fd]');
      end)

  .Scenario('A token already carrying the canonical delimiter is not read again')
    .Given('two declared flags whose letters spell the name part',
      procedure(W: TTokensWorld)
      begin
        W.Declare(['-a', '-b'], []);
      end)
    .When('the token already has a value on it',
      procedure(W: TTokensWorld)
      begin
        W.Normalize(['-ab=1']);
      end)
    .&Then('it is left alone rather than split into flags',
      procedure(W: TTokensWorld)
      begin
        // It is also what stops the rewriting from re-reading its own output: every
        // rewriting above produces this form, so this form is where it stops.
        Expect(W.Report).ToEqual('[-ab=1]');
      end);

end.
