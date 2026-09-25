unit Daf.CommandLine.Tokens;

{
  The tokenizer, kept apart from the grammar on purpose.

  Deciding WHAT A TOKEN IS and deciding WHICH COMMAND OR OPTION IT REACHES are two
  jobs. Every extra way of writing an
  invocation -- the : delimiter, an alias glued to its value, a bundle of short
  options, the Windows / prefix -- takes a reading away from another, and doing that
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
  /// <summary>The Windows prefix, the other convention .NET admits.</summary>
  WindowsPrefix = '/';
  /// <summary>The delimiter every rewriting produces, because the parser already reads it.</summary>
  CanonicalDelimiter = '=';
  /// <summary>The other delimiter an invocation may be written with.</summary>
  ColonDelimiter = ':';

type
  /// <summary>
  ///   What the rewriting is allowed to ask about the grammar in scope, and nothing
  ///   more.
  /// </summary>
  /// <remarks>
  ///   Bundling and the <c>/</c> prefix are NOT purely lexical:
  ///   <c>-fdx</c> can only be split knowing that f, d and x are declared, and
  ///   <c>/x</c> is an option only when x is. This is the whole of what that costs --
  ///   three questions, answerable by a test double, so the rewriting can be specified
  ///   without a command in sight.
  /// </remarks>
  IKnownTokens = interface
    ['{7C4B1A20-0008-4F31-9C10-4B0E2D5A0008}']
    /// <summary>True when the token, exactly as written, names something declared here.</summary>
    function Knows(const Token: string): Boolean;
    /// <summary>True when a one-character option by that letter is declared here.</summary>
    function KnowsShort(const Letter: Char): Boolean;
    /// <summary>True when that one-character option takes a value, i.e. is not a flag.</summary>
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
    /// <summary>Turns one token into the canonical form, or leaves it exactly as written.</summary>
    class function Rewrite(const Token: string; const Known: IKnownTokens): TTokenRewrite; static;
    /// <summary>
    ///   The whole invocation rewritten in one scope. This is the shape the rewriting
    ///   is specified in -- tokens in, tokens out -- and the parser applies the same
    ///   Rewrite token by token, because which options are declared depends on the
    ///   command reached, and that is only known while walking.
    /// </summary>
    class function Normalize(const Tokens: TArray<string>; const Known: IKnownTokens;
      out Errors: TArray<string>): TArray<string>; static;
    /// <summary>Puts <c>Replacement</c> where <c>At</c> was, which is how a rewriting lands.</summary>
    class function Splice(const Tokens: TArray<string>; At: Integer;
      const Replacement: TArray<string>): TArray<string>; static;
  end;

  /// <summary>A response file is a source of tokens, and is read with distrust.</summary>
  /// <remarks>
  ///   It exists for two concrete reasons -- a command line that does not fit in the
  ///   console's limit, and a long invocation that repeats -- and it does nothing beyond
  ///   that: no variables are expanded and no path is resolved against anything. A line
  ///   is a token, a <c>#</c> starts a comment, and the less it does the less there is to
  ///   explain about where a token came from.
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
      /// </summary>
      MaxDepth = 10;
    /// <summary>Replaces every <c>@file</c> with the tokens its lines carry.</summary>
    class function Expand(const Tokens: TArray<string>; out Errors: TArray<string>): TArray<string>; static;
  end;

implementation

uses
  System.IOUtils;

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

class function TTokenNormalizer.Rewrite(const Token: string; const Known: IKnownTokens): TTokenRewrite;
var
  Cut: Integer;
  Name, Value, Body: string;
  Letter: Char;
  Parts: TArray<string>;
  AnyKnown, HasValue: Boolean;
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

  // The Windows prefix, the other convention .NET admits on purpose. It is bounded
  // exactly like bundling: `/x` names an option only when x was declared, so a
  // positional whose value starts with a slash -- which on Windows is a POSIX absolute
  // path, or somebody else's argument -- stays the value it was.
  if Token.StartsWith(WindowsPrefix) then
  begin
    Cut := DelimiterAt(Token);
    HasValue := Cut > 0;
    if HasValue then
    begin
      Name := Token.Substring(1, Cut - 1);
      Value := Token.Substring(Cut + 1);
    end
    else
    begin
      Name := Token.Substring(1);
      Value := '';
    end;
    if Name = '' then
      Exit;
    if Known.Knows(LongPrefix + Name) then
      Name := LongPrefix + Name
    else if Known.Knows(ShortPrefix + Name) then
      Name := ShortPrefix + Name
    else
      Exit;
    if HasValue then
      Result.Tokens := [Name + CanonicalDelimiter + Value]
    else
      Result.Tokens := [Name];
    Result.Changed := True;
    Exit;
  end;

  if not LooksLikeOption(Token) then
    Exit;

  Cut := DelimiterAt(Token);
  if Cut > 0 then
  begin
    Name := Token.Substring(0, Cut);
    Value := Token.Substring(Cut + 1);
    // Already the form the parser reads. Saying so here is also what stops the loop
    // from rewriting the result of its own rewriting, since every rewriting below
    // produces exactly this form.
    if Token.Substring(Cut, 1) = CanonicalDelimiter then
      Exit;
    if Known.Knows(Name) then
    begin
      Result.Tokens := [Name + CanonicalDelimiter + Value];
      Result.Changed := True;
    end;
    // A name nobody declared keeps its colon: there is nothing on the left of it for a
    // delimiter to delimit, and `--Logging:LogLevel=Debug` has to stay the one unknown
    // option it is rather than become a half-understood one.
    Exit;
  end;

  // What follows are the POSIX SHORT forms: one dash and then letters. `--something` is
  // neither of them, whatever its letters happen to be.
  if (Length(Token) < 3) or Token.StartsWith(LongPrefix) then
    Exit;

  Body := Token.Substring(1);
  Letter := Body.Chars[0];

  // The glued value is tried BEFORE bundling, and that order is fixed here by a spec
  // rather than by the order of these ifs. An option that
  // takes a value swallows the rest of its own token, exactly as getopt does with
  // `-ovalue`; trying to bundle first would read `-vquiet` as five flags on any grammar
  // that happened to declare q, u, i, e and t.
  if Known.KnowsShort(Letter) and Known.ShortTakesValue(Letter) then
  begin
    Result.Tokens := [ShortPrefix + Letter + CanonicalDelimiter + Body.Substring(1)];
    Result.Changed := True;
    Exit;
  end;

  // Bundling, which is where the grammar is really needed: `-fdx` can only be split
  // knowing that f, d and x were declared. It is a group only when at least ONE of its
  // letters is -- with none of them declared there is nothing to split, and `-abc`
  // stays the unknown option it has always been so that the parser can name the token
  // rather than blame one of its letters.
  AnyKnown := False;
  for Letter in Body do
    if Known.KnowsShort(Letter) then
    begin
      AnyKnown := True;
      Break;
    end;
  if not AnyKnown then
    Exit;

  Parts := nil;
  for Letter in Body do
  begin
    if not Known.KnowsShort(Letter) then
    begin
      // Refused WHOLE, naming the letter. Taking the letters that exist and keeping
      // quiet about the one that does not would leave whoever invoked believing they
      // asked for something they did not -- the silent failure this module has spent
      // three changes removing.
      Result.Tokens := nil;
      Result.Changed := True;
      Result.Error := Format('Unknown option "%s" inside the group "%s", so nothing in the group is taken',
        [ShortPrefix + Letter, Token]);
      Exit;
    end;
    Parts := Parts + [ShortPrefix + Letter];
  end;
  Result.Tokens := Parts;
  Result.Changed := True;
end;

class function TTokenNormalizer.Normalize(const Tokens: TArray<string>; const Known: IKnownTokens;
  out Errors: TArray<string>): TArray<string>;
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
    Step := Rewrite(Pending[Index], Known);
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

{ TResponseFiles }

function ExpandResponseFile(const Path: string; Depth: Integer; var Errors: TArray<string>): TArray<string>;
var
  Lines: TArray<string>;
  Line, Trimmed: string;
begin
  Result := nil;
  if Depth > TResponseFiles.MaxDepth then
  begin
    Errors := Errors + [Format('Response file "%s" is nested deeper than %d, which is what a file naming itself looks like',
      [Path, TResponseFiles.MaxDepth])];
    Exit;
  end;
  if not TFile.Exists(Path) then
  begin
    // A file that is not there is an invalid invocation and not a literal token:
    // letting it through turns a typo into an argument with an at sign in front of it,
    // travelling all the way to wherever it ends up used.
    Errors := Errors + [Format('Response file "%s" does not exist', [Path])];
    Exit;
  end;
  try
    Lines := TFile.ReadAllLines(Path);
  except
    on E: Exception do
    begin
      Errors := Errors + [Format('Response file "%s" could not be read: %s', [Path, E.Message])];
      Exit;
    end;
  end;
  for Line in Lines do
  begin
    Trimmed := Line.Trim;
    if (Trimmed = '') or Trimmed.StartsWith(TResponseFiles.CommentMarker) then
      Continue;
    if Trimmed.StartsWith(TResponseFiles.Marker) and (Length(Trimmed) > 1) then
      Result := Result + ExpandResponseFile(Trimmed.Substring(1), Depth + 1, Errors)
    else
      Result := Result + [Trimmed];
  end;
end;

class function TResponseFiles.Expand(const Tokens: TArray<string>; out Errors: TArray<string>): TArray<string>;
var
  Token: string;
  Ended: Boolean;
begin
  Result := nil;
  Errors := nil;
  Ended := False;
  for Token in Tokens do
  begin
    // `--` protects a value here exactly as it protects one everywhere else. Without
    // that, a positional whose value happens to start with an at sign could not be
    // passed at all -- and the published requirement already says everything behind
    // `--` is a value.
    if Token = EndOfOptions then
      Ended := True;
    if (not Ended) and Token.StartsWith(Marker) and (Length(Token) > 1) then
      Result := Result + ExpandResponseFile(Token.Substring(1), 1, Errors)
    else
      Result := Result + [Token];
  end;
end;

end.
