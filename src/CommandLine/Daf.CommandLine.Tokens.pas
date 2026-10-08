unit Daf.CommandLine.Tokens;

{
  The tokenizer, kept apart from the grammar on purpose.

  Deciding WHAT A TOKEN IS and deciding WHICH COMMAND OR OPTION IT REACHES are two
  jobs. Every extra way of writing an
  invocation -- the : delimiter, an alias glued to its value, a bundle of short
  options -- takes a reading away from another, and doing that
  inside the parse loop would make the order of its ifs the semantics without anybody
  having written the semantics down.

  So this unit answers only the first question. It turns a token into the canonical
  form the parser already understands -- long name, = for an inline value -- and it
  can be exercised with no grammar at all: a list of tokens in, a list of tokens out.
}

interface

uses
  System.SysUtils;

const
  /// <summary>Everything after this token is a value, dashes and all. POSIX.</summary>
  EndOfOptions = '--';
  /// <summary>The long prefix, POSIX style.</summary>
  LongPrefix = '--';
  /// <summary>The short prefix, one dash and one letter.</summary>
  ShortPrefix = '-';
  /// <summary>The delimiter every rewriting produces, because the parser already reads it.</summary>
  CanonicalDelimiter = '=';
  /// <summary>The other delimiter an invocation may be written with.</summary>
  ColonDelimiter = ':';
  /// <summary>What a directive opens with.</summary>
  DirectiveOpen = '[';
  /// <summary>What a directive closes with.</summary>
  DirectiveClose = ']';

type
  /// <summary>Replaces a token with other tokens before the parse reads any of them, as .NET's <c>TryReplaceToken</c>.</summary>
  /// <remarks>
  ///   It receives the token without its <c>@</c>. True replaces it with <c>ReplacementTokens</c>, which are read
  ///   in its place, a token starting with <c>@</c> among them included. False with an <c>ErrorMessage</c> drops
  ///   it and fails the parse with that message; False with none leaves it as it was written.
  /// </remarks>
  TTryReplaceToken = reference to function(const TokenToReplace: string; out ReplacementTokens: TArray<string>;
    out ErrorMessage: string): Boolean;

  /// <summary>
  ///   What the rewriting is allowed to ask about the grammar in scope, and nothing
  ///   more.
  /// </summary>
  /// <remarks>
  ///   Bundling is NOT purely lexical: <c>-fdx</c> can only be split knowing that f, d
  ///   and x are declared, and <c>-fobin</c> knowing that o takes a value. This is the
  ///   whole of what that costs -- two questions, answerable by a test double, so the
  ///   rewriting can be specified without a command in sight.
  /// </remarks>
  IKnownTokens = interface
    ['{7C4B1A20-0008-4F31-9C10-4B0E2D5A0008}']
    /// <summary>True when the token, exactly as written, names something declared here.</summary>
    function Knows(const Token: string): Boolean;
    /// <summary>True when the one-character option by that letter takes a value, i.e. is not a flag.</summary>
    function ShortTakesValue(const Letter: Char): Boolean;
  end;

  /// <summary>What one token became: itself, other tokens, or a refusal.</summary>
  TTokenRewrite = record
    /// <summary>False when the token is already canonical and nothing was done to it.</summary>
    Changed: Boolean;
    /// <summary>The tokens that replace it. Meaningless when <c>Error</c> is set.</summary>
    Tokens: TArray<string>;
    /// <summary>Set when the token cannot be read at all, e.g. a bundle with a letter nobody declared.</summary>
    Error: string;
  end;

  /// <summary>Everything that decides the shape of a token, in one place.</summary>
  TTokenNormalizer = record
  public
    /// <summary>
    ///   A token starting with a dash is an option -- unless it is a negative number,
    ///   and then it is a value. Without this an Integer argument could never be given
    ///   -5, which is one of the two defects this module exists not to inherit.
    /// </summary>
    class function LooksLikeOption(const Token: string): Boolean; static;
    /// <summary>Splits <c>--name=value</c>, the only inline form the parser reads.</summary>
    class procedure SplitToken(const Token: string; out OptionName, InlineValue: string;
      out HasInline: Boolean); static;
    /// <summary>
    ///   True for a token with the shape of a directive: <c>[name]</c> or <c>[name:value]</c>, with a name that is
    ///   not empty. Whether it IS one depends on where it was written and on what the root declares.
    /// </summary>
    class function LooksLikeDirective(const Token: string): Boolean; static;
    /// <summary>Splits a token with the shape of a directive into its name and what follows its first <c>:</c>.</summary>
    class procedure SplitDirective(const Token: string; out Name, Value: string; out HasValue: Boolean); static;
    /// <summary>Turns one token into the canonical form, or leaves it exactly as written.</summary>
    /// <remarks>
    ///   With <c>EnablePosixBundling</c> off, a single-dash token is neither a group nor an alias with a value
    ///   glued to it, as in .NET; a value after a delimiter is still read.
    /// </remarks>
    class function Rewrite(const Token: string; const Known: IKnownTokens;
      EnablePosixBundling: Boolean = True): TTokenRewrite; static;
    /// <summary>
    ///   The whole invocation rewritten in one scope. This is the shape the rewriting
    ///   is specified in -- tokens in, tokens out -- and the parser applies the same
    ///   Rewrite token by token, because which options are declared depends on the
    ///   command reached, and that is only known while walking.
    /// </summary>
    class function Normalize(const Tokens: TArray<string>; const Known: IKnownTokens;
      out Errors: TArray<string>; EnablePosixBundling: Boolean = True): TArray<string>; static;
    /// <summary>Puts <c>Replacement</c> where <c>At</c> was, which is how a rewriting lands.</summary>
    class function Splice(const Tokens: TArray<string>; At: Integer;
      const Replacement: TArray<string>): TArray<string>; static;
  end;

  /// <summary>A response file is a source of tokens, and is read with distrust.</summary>
  /// <remarks>
  ///   It exists for two concrete reasons -- a command line that does not fit in the
  ///   console's limit, and a long invocation that repeats -- and it does nothing beyond
  ///   that: no variables are expanded and no path is resolved against anything. A line
  ///   is split as a command line is, a <c>#</c> starts a comment, and the less it does
  ///   the less there is to explain about where a token came from.
  /// </remarks>
  TResponseFiles = record
  public
    const
      /// <summary>The token that names one.</summary>
      Marker = '@';
      /// <summary>A line starting with this contributes nothing.</summary>
      CommentMarker = '#';
      /// <summary>
      ///   How deep one may name another. Fixed rather than declared, and the number is
      ///   not the point: it is there because a file that names itself would loop
      ///   forever, and a process that hangs does not read as a failure -- it reads as a
      ///   suite still running. Far above any real nesting, so nobody has cause to tune it.
      ///   A declared divergence from .NET, which has no limit.
      /// </summary>
      MaxDepth = 10;
    /// <summary>
    ///   Replaces every <c>@token</c> before <c>--</c> through <c>Replacer</c>. With no replacer, the tokens
    ///   are left as they were written.
    /// </summary>
    class function Expand(const Tokens: TArray<string>; const Replacer: TTryReplaceToken;
      out Errors: TArray<string>): TArray<string>; static;
  end;

/// <summary>
///   Splits a line into tokens as the command line does: white space separates, and double quotes group
///   and are not part of the token. A port of .NET's <c>CommandLineParser.SplitCommandLine</c>.
/// </summary>
/// <remarks>What follows a double quote that is never closed is dropped, as in .NET.</remarks>
function SplitCommandLine(const CommandLine: string): TArray<string>;

/// <summary>Reads the response file at <c>FilePath</c>: the replacer the parse uses unless it is told otherwise.</summary>
/// <remarks>
///   Each line is split with <c>SplitCommandLine</c>, and a token of it starting with <c>@</c> names another
///   file, read in its place up to <c>TResponseFiles.MaxDepth</c> deep.
/// </remarks>
function TryReadResponseFile(const FilePath: string; out Tokens: TArray<string>; out Error: string): Boolean;

implementation

uses
  System.Character,
  System.IOUtils,
  Daf.CommandLine.Resources;

// The first `=` or `:`, whichever comes first -- and only that first one delimits, so a
// value is free to contain either character: `--url:http://host:8080` keeps its value
// whole, which is the difference between a delimiter and a separator.
function DelimiterAt(const Token: string): Integer;
var
  Equals, Colon: Integer;
begin
  Equals := Token.IndexOf(CanonicalDelimiter);
  Colon := Token.IndexOf(ColonDelimiter);
  if Equals < 0 then
    Exit(Colon);
  if Colon < 0 then
    Exit(Equals);
  if Equals < Colon then
    Result := Equals
  else
    Result := Colon;
end;

{ TTokenNormalizer }

class function TTokenNormalizer.LooksLikeOption(const Token: string): Boolean;
begin
  Result := (Length(Token) >= 2) and (Token[1] = '-') and not CharInSet(Token[2], ['0' .. '9', '.']);
end;

class procedure TTokenNormalizer.SplitToken(const Token: string; out OptionName, InlineValue: string;
  out HasInline: Boolean);
var
  P: Integer;
begin
  P := Token.IndexOf(CanonicalDelimiter);
  HasInline := P > 0;
  if HasInline then
  begin
    OptionName := Token.Substring(0, P);
    InlineValue := Token.Substring(P + 1);
  end
  else
  begin
    OptionName := Token;
    InlineValue := '';
  end;
end;

class function TTokenNormalizer.LooksLikeDirective(const Token: string): Boolean;
begin
  Result := (Length(Token) > 2) and (Token[1] = DirectiveOpen) and (Token[Length(Token)] = DirectiveClose) and
    (Token[2] <> DirectiveClose) and (Token[2] <> ColonDelimiter);
end;

class procedure TTokenNormalizer.SplitDirective(const Token: string; out Name, Value: string; out HasValue: Boolean);
var
  Inner: string;
  Colon: Integer;
begin
  Inner := Token.Substring(1, Token.Length - 2);
  Colon := Inner.IndexOf(ColonDelimiter);
  HasValue := Colon > 0;
  if HasValue then
  begin
    Name := Inner.Substring(0, Colon);
    Value := Inner.Substring(Colon + 1);
  end
  else
  begin
    Name := Inner;
    Value := '';
  end;
end;

// The POSIX short forms of one token: a group of one-character options, and a value glued to the one that
// takes it. As .NET's TryUnbundle: the token is a group only when its FIRST letter is known, and an option
// that takes a value keeps the rest of the token -- `-fdobin` is `-f -d -o bin`, and `-vquiet` is one value
// and not five flags on a grammar that happens to declare q, u, i, e and t.
function Unbundle(const Token: string; const Known: IKnownTokens): TTokenRewrite;
var
  Body: string;
  Index, Rest: Integer;
  Letter: Char;
  Parts: TArray<string>;
begin
  Result.Changed := False;
  Result.Tokens := [Token];
  Result.Error := '';
  Body := Token.Substring(1);
  Parts := nil;
  for Index := 0 to Body.Length - 1 do
  begin
    Letter := Body.Chars[Index];
    // A delimiter inside the group gives the rest to the option before it.
    if CharInSet(Letter, [CanonicalDelimiter, ColonDelimiter]) then
    begin
      if Parts = nil then
        Exit;
      Parts[High(Parts)] := Parts[High(Parts)] + CanonicalDelimiter + Body.Substring(Index + 1);
      Break;
    end;
    // The help letter is known, so `-fh` asks for help.
    if not Known.Knows(ShortPrefix + Letter) then
    begin
      // An unknown first letter is no group: the token stays the unknown one it is, and the parser
      // names it whole.
      if Parts = nil then
        Exit;
      // Refused WHOLE, naming the letter. Taking the letters that exist and keeping
      // quiet about the one that does not would leave whoever invoked believing they
      // asked for something they did not. A declared divergence from .NET, which takes
      // the rest as a value.
      Result.Tokens := nil;
      Result.Changed := True;
      Result.Error := Format(SUnknownOptionInGroup, [ShortPrefix + Letter, Token]);
      Exit;
    end;
    Parts := Parts + [ShortPrefix + Letter];
    if (Index < Body.Length - 1) and Known.ShortTakesValue(Letter) then
    begin
      Rest := Index + 1;
      if CharInSet(Body.Chars[Rest], [CanonicalDelimiter, ColonDelimiter]) then
        Inc(Rest);
      Parts[High(Parts)] := ShortPrefix + Letter + CanonicalDelimiter + Body.Substring(Rest);
      Break;
    end;
  end;
  Result.Tokens := Parts;
  Result.Changed := True;
end;

class function TTokenNormalizer.Rewrite(const Token: string; const Known: IKnownTokens;
  EnablePosixBundling: Boolean): TTokenRewrite;
var
  Cut: Integer;
  Name, Value: string;
begin
  Result.Changed := False;
  Result.Tokens := [Token];
  Result.Error := '';

  if (Token = '') or (Token = EndOfOptions) or (Known = nil) then
    Exit;

  // A token that already names something declared is left exactly as written. Every
  // rewriting below fires only for one that does not.
  if Known.Knows(Token) then
    Exit;

  // A delimiter after a declared name delimits whatever the name looks like: an alias declared as `/p`
  // takes `/p:value` too.
  Cut := DelimiterAt(Token);
  if (Cut > 0) and Known.Knows(Token.Substring(0, Cut)) then
  begin
    Name := Token.Substring(0, Cut);
    Value := Token.Substring(Cut + 1);
    // Already the form the parser reads. Saying so here is also what stops the loop
    // from rewriting the result of its own rewriting, since every rewriting
    // produces exactly this form.
    if Token.Substring(Cut, 1) = CanonicalDelimiter then
      Exit;
    Result.Tokens := [Name + CanonicalDelimiter + Value];
    Result.Changed := True;
    Exit;
  end;

  if not LooksLikeOption(Token) then
    Exit;

  // A long name nobody declared keeps its colon: there is nothing on the left of it for a
  // delimiter to delimit, and `--Logging:LogLevel=Debug` has to stay the one unknown
  // option it is rather than become a half-understood one.
  if Token.StartsWith(LongPrefix) then
    Exit;

  if (not EnablePosixBundling) or (Length(Token) < 3) then
    Exit;
  Result := Unbundle(Token, Known);
end;

class function TTokenNormalizer.Normalize(const Tokens: TArray<string>; const Known: IKnownTokens;
  out Errors: TArray<string>; EnablePosixBundling: Boolean): TArray<string>;
var
  Index: Integer;
  Ended: Boolean;
  Step: TTokenRewrite;
  Pending: TArray<string>;
begin
  Result := nil;
  Errors := nil;
  Pending := Tokens;
  Index := 0;
  Ended := False;
  while Index < Length(Pending) do
  begin
    if Ended or (Pending[Index] = EndOfOptions) then
    begin
      // Past `--` nothing is an option any more, so nothing is rewritten either: that
      // token is the escape hatch for every value a new form would otherwise claim.
      Ended := True;
      Result := Result + [Pending[Index]];
      Inc(Index);
      Continue;
    end;
    Step := Rewrite(Pending[Index], Known, EnablePosixBundling);
    if Step.Error <> '' then
    begin
      Errors := Errors + [Step.Error];
      Inc(Index);
      Continue;
    end;
    if Step.Changed then
    begin
      Pending := Splice(Pending, Index, Step.Tokens);
      Continue;
    end;
    Result := Result + [Pending[Index]];
    Inc(Index);
  end;
end;

class function TTokenNormalizer.Splice(const Tokens: TArray<string>; At: Integer;
  const Replacement: TArray<string>): TArray<string>;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to At - 1 do
    Result := Result + [Tokens[I]];
  Result := Result + Replacement;
  for I := At + 1 to High(Tokens) do
    Result := Result + [Tokens[I]];
end;

{ SplitCommandLine }

type
  {$SCOPEDENUMS ON}
  TBoundary = (TokenStart, WordEnd, QuoteStart, QuoteEnd);
  {$SCOPEDENUMS OFF}

// The state machine of .NET's SplitCommandLine, transition for transition: a quote that opens a token
// starts a token of its own, and one inside a word only switches whether white space separates.
function SplitCommandLine(const CommandLine: string): TArray<string>;
var
  StartTokenIndex, Position: Integer;
  Seeking, SeekingQuote: TBoundary;
  C: Char;

  function CurrentToken: string;
  begin
    Result := CommandLine.Substring(StartTokenIndex, Position - StartTokenIndex).Replace('"', '');
  end;

begin
  Result := nil;
  StartTokenIndex := 0;
  Position := 0;
  Seeking := TBoundary.TokenStart;
  SeekingQuote := TBoundary.QuoteStart;
  while Position < CommandLine.Length do
  begin
    C := CommandLine.Chars[Position];
    if C.IsWhiteSpace then
    begin
      if SeekingQuote = TBoundary.QuoteStart then
        case Seeking of
          TBoundary.WordEnd:
            begin
              Result := Result + [CurrentToken];
              StartTokenIndex := Position;
              Seeking := TBoundary.TokenStart;
            end;
          TBoundary.TokenStart:
            StartTokenIndex := Position;
        end;
    end
    else if C = '"' then
    begin
      if Seeking = TBoundary.TokenStart then
        case SeekingQuote of
          TBoundary.QuoteEnd:
            begin
              Result := Result + [CurrentToken];
              StartTokenIndex := Position;
              SeekingQuote := TBoundary.QuoteStart;
            end;
          TBoundary.QuoteStart:
            begin
              StartTokenIndex := Position + 1;
              SeekingQuote := TBoundary.QuoteEnd;
            end;
        end
      else if SeekingQuote = TBoundary.QuoteEnd then
        SeekingQuote := TBoundary.QuoteStart
      else
        SeekingQuote := TBoundary.QuoteEnd;
    end
    else if (Seeking = TBoundary.TokenStart) and (SeekingQuote = TBoundary.QuoteStart) then
    begin
      Seeking := TBoundary.WordEnd;
      StartTokenIndex := Position;
    end;

    Inc(Position);
    if (Position = CommandLine.Length) and (Seeking <> TBoundary.TokenStart) then
      Result := Result + [CurrentToken];
  end;
end;

{ TResponseFiles }

function ReadResponseFile(const Path: string; Depth: Integer; var Tokens: TArray<string>; out Error: string): Boolean;
var
  Lines: TArray<string>;
  Line, Trimmed, Word: string;
begin
  if Depth > TResponseFiles.MaxDepth then
  begin
    Error := Format(SResponseFileTooDeep, [Path, TResponseFiles.MaxDepth]);
    Exit(False);
  end;
  if not TFile.Exists(Path) then
  begin
    // A file that is not there is an invalid invocation and not a literal token:
    // letting it through turns a typo into an argument with an at sign in front of it,
    // travelling all the way to wherever it ends up used.
    Error := Format(SResponseFileMissing, [Path]);
    Exit(False);
  end;
  try
    Lines := TFile.ReadAllLines(Path);
  except
    on E: Exception do
    begin
      Error := Format(SResponseFileUnreadable, [Path, E.Message]);
      Exit(False);
    end;
  end;
  for Line in Lines do
  begin
    Trimmed := Line.Trim;
    if (Trimmed = '') or Trimmed.StartsWith(TResponseFiles.CommentMarker) then
      Continue;
    for Word in SplitCommandLine(Trimmed) do
      if Word.StartsWith(TResponseFiles.Marker) and (Length(Word) > 1) then
      begin
        if not ReadResponseFile(Word.Substring(1), Depth + 1, Tokens, Error) then
          Exit(False);
      end
      else
        Tokens := Tokens + [Word];
  end;
  Result := True;
end;

function TryReadResponseFile(const FilePath: string; out Tokens: TArray<string>; out Error: string): Boolean;
begin
  Tokens := nil;
  Error := '';
  Result := ReadResponseFile(FilePath, 1, Tokens, Error);
  if not Result then
    Tokens := nil;
end;

class function TResponseFiles.Expand(const Tokens: TArray<string>; const Replacer: TTryReplaceToken;
  out Errors: TArray<string>): TArray<string>;
var
  Pending, Replacement: TArray<string>;
  Index: Integer;
  Token, Error: string;
  Ended: Boolean;
begin
  Errors := nil;
  if not Assigned(Replacer) then
    Exit(Tokens);
  Result := nil;
  Pending := Tokens;
  Index := 0;
  Ended := False;
  while Index < Length(Pending) do
  begin
    Token := Pending[Index];
    Inc(Index);
    // `--` protects a value here exactly as it protects one everywhere else. Without
    // that, a positional whose value happens to start with an at sign could not be
    // passed at all.
    if Token = EndOfOptions then
      Ended := True;
    if Ended or not Token.StartsWith(Marker) or (Length(Token) < 2) then
    begin
      Result := Result + [Token];
      Continue;
    end;
    if Replacer(Token.Substring(1), Replacement, Error) then
    begin
      Pending := Replacement + Copy(Pending, Index, MaxInt);
      Index := 0;
    end
    else if Error.Trim <> '' then
      Errors := Errors + [Error]
    else
      Result := Result + [Token];
  end;
end;

end.
