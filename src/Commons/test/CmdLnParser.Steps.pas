unit CmdLnParser.Steps;

interface

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.CmdLn.Parser,
  CmdLnParser.Feat;

type
  /// <summary>
  /// Step bindings for the CmdLnParser feature.
  /// Regex captures are auto-converted to method parameters.
  /// </summary>
  TCmdLnSteps = class
  public
    // === Given: typed argument setup ===

    [Given('a parser with a Boolean (flag|argument) "([^"]+)"')]
    procedure GivenBooleanArg(W: TCmdLnWorld; Kind, Name: string);

    [Given('a parser with an Integer (flag|argument) "([^"]+)"')]
    procedure GivenIntegerArg(W: TCmdLnWorld; Kind, Name: string);

    [Given('a parser with a Double (flag|argument) "([^"]+)"')]
    procedure GivenDoubleArg(W: TCmdLnWorld; Kind, Name: string);

    [Given('a parser with a String (flag|argument) "([^"]+)"')]
    procedure GivenStringArg(W: TCmdLnWorld; Kind, Name: string);

    [Given('a parser with a required String argument "([^"]+)"')]
    procedure GivenRequiredStringArg(W: TCmdLnWorld; Name: string);

    // === When: parsing ===

    [When('I parse "(.*)"')]
    procedure ParseCmdLine(W: TCmdLnWorld; CmdLine: string);

    // === Then: assertions ===

    [ThenAttribute('the argument should exist in the parser')]
    procedure AssertArgExists(W: TCmdLnWorld);

    [ThenAttribute('parsing should (succeed|fail)')]
    procedure AssertParsingResult(W: TCmdLnWorld; Outcome: string);

    [ThenAttribute('the flag value should be (\w+)')]
    procedure AssertFlagValue(W: TCmdLnWorld; Expected: string);

    [ThenAttribute('the value should be "([^"]*)"')]
    procedure AssertArgValue(W: TCmdLnWorld; Expected: string);
  end;

{ TCmdLnSteps }

procedure TCmdLnSteps.GivenBooleanArg(W: TCmdLnWorld; Kind, Name: string);
begin
  W.LastArgName := Name;
  W.Builder.Arg<Boolean>(Name);
  W.Parser := W.Builder.Build;
end;

procedure TCmdLnSteps.GivenIntegerArg(W: TCmdLnWorld; Kind, Name: string);
begin
  W.LastArgName := Name;
  W.Builder.Arg<Integer>(Name);
  W.Parser := W.Builder.Build;
end;

procedure TCmdLnSteps.GivenDoubleArg(W: TCmdLnWorld; Kind, Name: string);
begin
  FormatSettings.DecimalSeparator := '.';
  W.LastArgName := Name;
  W.Builder.Arg<Double>(Name);
  W.Parser := W.Builder.Build;
end;

procedure TCmdLnSteps.GivenStringArg(W: TCmdLnWorld; Kind, Name: string);
begin
  W.LastArgName := Name;
  W.Builder.Arg<string>(Name);
  W.Parser := W.Builder.Build;
end;

procedure TCmdLnSteps.GivenRequiredStringArg(W: TCmdLnWorld; Name: string);
begin
  W.LastArgName := Name;
  W.Builder.Arg<string>(Name,
    procedure(Arg: TArgNode)
    begin
      Arg.Required(True);
    end);
  W.Parser := W.Builder.Build;
end;

procedure TCmdLnSteps.ParseCmdLine(W: TCmdLnWorld; CmdLine: string);
begin
  W.CmdLnParams := W.Parser.Parse(CmdLine);
end;

procedure TCmdLnSteps.AssertArgExists(W: TCmdLnWorld);
begin
  Expect(Assigned(W.Parser.Root['Args'][W.LastArgName])).ToBeTrue;
end;

procedure TCmdLnSteps.AssertParsingResult(W: TCmdLnWorld; Outcome: string);
begin
  if Outcome = 'succeed' then
    Expect(Assigned(W.CmdLnParams)).ToBeTrue
  else
    Expect(Assigned(W.CmdLnParams)).ToBeFalse;
end;

procedure TCmdLnSteps.AssertFlagValue(W: TCmdLnWorld; Expected: string);
begin
  Expect(W.CmdLnParams[W.LastArgName].ToString).ToEqual(Expected);
end;

procedure TCmdLnSteps.AssertArgValue(W: TCmdLnWorld; Expected: string);
begin
  Expect(W.CmdLnParams[W.LastArgName].ToString).ToEqual(Expected);
end;

initialization
  Bindings.RegisterSteps<TCmdLnSteps>;

end.
