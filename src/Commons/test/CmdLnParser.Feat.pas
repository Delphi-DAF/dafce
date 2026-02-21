unit CmdLnParser.Feat;

interface

uses
  Daf.CmdLn.Parser;

type
  TCmdLnWorld = class
  public
    Builder: TCmdLnParserBuilder;
    Parser: ICmdLnParser;
    CmdLnParams: ICmdLnParams;
    LastArgName: string;
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec;

{ TCmdLnWorld }

constructor TCmdLnWorld.Create;
begin
  inherited;
  Builder := TCmdLnParserBuilder.Create;
end;

// --- Feature definition ---

initialization

Feature('''
Feature CmdLnParser @cmdln

  As a developer
  I want to parse command line arguments
  So I can configure application behavior from the CLI
''')

.UseWorld<TCmdLnWorld>

// --- Builder ---

.Rule('Builder')

  .Scenario('Can add an argument')
    .Given('a parser builder').NoAction
    .When('I add a Boolean argument "verbose"',
      procedure(W: TCmdLnWorld)
      begin
        W.LastArgName := 'verbose';
        W.Builder.Arg<Boolean>('verbose');
        W.Parser := W.Builder.Build;
      end)
    .&Then('the argument should exist in the parser')

  .Scenario('Can add a command with arguments')
    .Given('a parser builder').NoAction
    .When('I add a command "mycmd" with arg "cmd_arg" and a root arg "root_arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder
          .Command('mycmd')
            .Arg<Boolean>('cmd_arg')
          .EndCommand
          .Arg<string>('root_arg');
        W.Parser := W.Builder.Build;
      end)
    .&Then('root_arg should be at root level',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.Parser.Root['Args']['root_arg'])).ToBeTrue;
      end)
    .&And('cmd_arg should NOT be at root level',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.Parser.Root['Args']['cmd_arg'])).ToBeFalse;
      end)
    .&And('cmd_arg should be under mycmd',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.Parser.Root['Cmds']['mycmd']['Args']['cmd_arg'])).ToBeTrue;
      end)
    .&And('root_arg should NOT be under mycmd',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.Parser.Root['Cmds']['mycmd']['Args']['root_arg'])).ToBeFalse;
      end)

// --- Flags ---

.Rule('Parsing flags')

  .Scenario('Flag --verbose sets value to True')
    .Given('a parser with a Boolean flag "verbose"')
    .When('I parse "--verbose"')
    .&Then('the flag value should be True')

  .Scenario('Flag --verbose false sets value to False')
    .Given('a parser with a Boolean flag "verbose"')
    .When('I parse "--verbose false"')
    .&Then('the flag value should be False')

  .Scenario('Flag --verbose true sets value to True')
    .Given('a parser with a Boolean flag "verbose"')
    .When('I parse "--verbose true"')
    .&Then('the flag value should be True')

// --- Required arguments ---

.Rule('Required arguments')

  .Scenario('Missing required argument fails parsing')
    .Given('a parser with a required String argument "arg"')
    .When('I parse "--other"')
    .&Then('parsing should fail')

  .Scenario('Required argument without value fails parsing')
    .Given('a parser with a required String argument "arg"')
    .When('I parse "--arg"')
    .&Then('parsing should fail')

  .Scenario('Required argument with value succeeds parsing')
    .Given('a parser with a required String argument "arg"')
    .When('I parse "--arg value"')
    .&Then('parsing should succeed')

// --- Typed arguments ---

.Rule('Typed argument parsing')

  .Scenario('Can parse an Integer argument')
    .Given('a parser with an Integer argument "number"')
    .When('I parse "--number 42"')
    .&Then('the value should be "42"')

  .Scenario('Can parse a Double argument')
    .Given('a parser with a Double argument "arg"')
    .When('I parse "--arg 42.24"')
    .&Then('the value should be "42.24"')

  .Scenario('Can parse a Boolean argument')
    .Given('a parser with a Boolean argument "arg"')
    .When('I parse "--arg true"')
    .&Then('the value should be "True"')

  .Scenario('Can parse a String argument')
    .Given('a parser with a String argument "arg"')
    .When('I parse "--arg Hola"')
    .&Then('the value should be "Hola"')

  // Lambda kept: parse string contains embedded quotes incompatible with the
  // 'I parse "(...)"' binding pattern used in other scenarios
  .Scenario('Can parse a quoted String argument')
    .Given('a parser with a String argument "arg"')
    .When('I parse --arg "Hola, mundo"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg "Hola, mundo"');
      end)
    .&Then('the value should be "Hola, mundo"')

// --- Complex command line ---

.Rule('Complex command lines')

  .Scenario('Can parse a complex command line with mixed arguments')
    .Given('a parser with flag, command, and multiple typed arguments',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder
          .Arg<Boolean>('flag|f')
          .Command('build')
            .Arg<string>('template|t')
            .Arg<TArray<Integer>>('codes')
            .Arg<TArray<string>>('intervals|i',
              procedure(Arg: TArgNode)
              begin
                Arg.RegEx('\d+(m|h)');
              end)
          .EndCommand;
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--flag build -t "template.txt" --codes 1,3,5 -i 3m,5h"')
    .&Then('parsing should succeed')
    .&And('flag should be true',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['flag'].AsBoolean).ToEqual(True);
      end)
    .&And('command should be "build"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams.Command).ToEqual('build');
      end)
    .&And('template should be "template.txt"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['build.template'].AsString).ToEqual('template.txt');
      end);

end.
