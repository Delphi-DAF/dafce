unit CmdLnParser.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.CmdLn.Parser;

type
  TCmdLnWorld = class
  public
    Builder: TCmdLnParserBuilder;
    Parser: ICmdLnParser;
    CmdLnParams: ICmdLParams;
    constructor Create;
  end;

{ TCmdLnWorld }

constructor TCmdLnWorld.Create;
begin
  inherited;
  Builder := TCmdLnParserBuilder.Create;
end;

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
        W.Builder.Arg<Boolean>('verbose');
        W.Parser := W.Builder.Build;
      end)
    .&Then('the argument should exist in the parser',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.Parser.Root['Args']['verbose'])).ToBeTrue;
      end)

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
    .Given('a parser with a Boolean flag "verbose"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<Boolean>('verbose');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--verbose"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--verbose');
      end)
    .&Then('the flag value should be True',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['verbose'].ToString).ToEqual('True');
      end)

  .Scenario('Flag --verbose false sets value to False')
    .Given('a parser with a Boolean flag "verbose"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<Boolean>('verbose');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--verbose false"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--verbose false');
      end)
    .&Then('the flag value should be False',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['verbose'].ToString).ToEqual('False');
      end)

  .Scenario('Flag --verbose true sets value to True')
    .Given('a parser with a Boolean flag "verbose"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<Boolean>('verbose');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--verbose true"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--verbose true');
      end)
    .&Then('the flag value should be True',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['verbose'].ToString).ToEqual('True');
      end)

// --- Required arguments ---

.Rule('Required arguments')

  .Scenario('Missing required argument fails parsing')
    .Given('a parser with a required String argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<string>('arg',
          procedure(Arg: TArgNode)
          begin
            Arg.Required(True);
          end);
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--other"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--other');
      end)
    .&Then('parsing should fail',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.CmdLnParams)).ToBeFalse;
      end)

  .Scenario('Required argument without value fails parsing')
    .Given('a parser with a required String argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<string>('arg',
          procedure(Arg: TArgNode)
          begin
            Arg.Required(True);
          end);
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg');
      end)
    .&Then('parsing should fail',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.CmdLnParams)).ToBeFalse;
      end)

  .Scenario('Required argument with value succeeds parsing')
    .Given('a parser with a required String argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<string>('arg',
          procedure(Arg: TArgNode)
          begin
            Arg.Required(True);
          end);
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--arg value"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg value');
      end)
    .&Then('parsing should succeed',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.CmdLnParams)).ToBeTrue;
      end)

// --- Typed arguments ---

.Rule('Typed argument parsing')

  .Scenario('Can parse an Integer argument')
    .Given('a parser with an Integer argument "number"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<Integer>('number');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--number 42"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--number 42');
      end)
    .&Then('the value should be "42"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['number'].ToString).ToEqual('42');
      end)

  .Scenario('Can parse a Double argument')
    .Given('a parser with a Double argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        FormatSettings.DecimalSeparator := '.';
        W.Builder.Arg<Double>('arg');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--arg 42.24"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg 42.24');
      end)
    .&Then('the value should be "42.24"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['arg'].ToString).ToEqual('42.24');
      end)

  .Scenario('Can parse a Boolean argument')
    .Given('a parser with a Boolean argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<Boolean>('arg');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--arg true"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg true');
      end)
    .&Then('the value should be "True"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['arg'].ToString).ToEqual('True');
      end)

  .Scenario('Can parse a String argument')
    .Given('a parser with a String argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<string>('arg');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse "--arg Hola"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg Hola');
      end)
    .&Then('the value should be "Hola"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['arg'].ToString).ToEqual('Hola');
      end)

  .Scenario('Can parse a quoted String argument')
    .Given('a parser with a String argument "arg"',
      procedure(W: TCmdLnWorld)
      begin
        W.Builder.Arg<string>('arg');
        W.Parser := W.Builder.Build;
      end)
    .When('I parse --arg "Hola, mundo"',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--arg "Hola, mundo"');
      end)
    .&Then('the value should be "Hola, mundo"',
      procedure(W: TCmdLnWorld)
      begin
        Expect(W.CmdLnParams['arg'].ToString).ToEqual('Hola, mundo');
      end)

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
    .When('I parse the full command line',
      procedure(W: TCmdLnWorld)
      begin
        W.CmdLnParams := W.Parser.Parse('--flag build -t "template.txt" --codes 1,3,5 -i 3m,5h');
      end)
    .&Then('parsing should succeed',
      procedure(W: TCmdLnWorld)
      begin
        Expect(Assigned(W.CmdLnParams)).ToBeTrue;
      end)
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
