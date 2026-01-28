unit Daf.MiniSpec.Filter;

interface

uses
  System.SysUtils,
  System.StrUtils,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Contexto para evaluar filtros de specs.
  /// Contiene toda la información necesaria para matching.
  /// </summary>
  TSpecFilterContext = record
    Tags: TSpecTags;
    FeatureTitle: string;
    ScenarioDescription: string;
    RuleDescription: string;
    SourceUnit: string;
    class function FromScenario(const Scenario: IScenario): TSpecFilterContext; static;
  end;

  /// <summary>
  /// Parsea y evalúa expresiones de filtro de specs.
  /// Sintaxis soportada:
  ///   @tag           - tiene el tag
  ///   ~@tag          - NO tiene el tag (negación)
  ///   not @tag       - NO tiene el tag (alternativa)
  ///   F:texto        - Feature title contiene texto (case-insensitive)
  ///   S:texto        - Scenario description contiene texto
  ///   R:texto        - Rule description contiene texto
  ///   U:texto        - SourceUnit contiene texto
  ///   @a and @b      - tiene ambos
  ///   @a or @b       - tiene alguno
  ///   @a,@b          - atajo para OR
  ///   (expr) and ~@c - expresiones con paréntesis
  /// </summary>
  TSpecFilter = record
  private type
    TTokenKind = (tkTag, tkFeature, tkScenario, tkRule, tkUnit,
                  tkNot, tkAnd, tkOr, tkLParen, tkRParen, tkEOF);
    TToken = record
      Kind: TTokenKind;
      Value: string;
    end;
    TNodeKind = (nkTag, nkFeature, nkScenario, nkRule, nkUnit, nkNot, nkAnd, nkOr);
    PNode = ^TNode;
    TNode = record
      Kind: TNodeKind;
      Value: string;
      Left, Right: PNode;
    end;
  private
    FExpression: string;
    FRoot: PNode;
    FTokens: TArray<TToken>;
    FTokenIndex: Integer;
    class function Tokenize(const Expr: string): TArray<TToken>; static;
    function CurrentToken: TToken;
    procedure Advance;
    function ParseOr: PNode;
    function ParseAnd: PNode;
    function ParseUnary: PNode;
    function ParsePrimary: PNode;
    function NewNode(Kind: TNodeKind; const Value: string = ''; Left: PNode = nil; Right: PNode = nil): PNode;
    function EvalNode(Node: PNode; const Context: TSpecFilterContext): Boolean;
    class procedure FreeNode(Node: PNode); static;
  public
    class function Parse(const Expression: string): TSpecFilter; static;
    function IsEmpty: Boolean;
    function Matches(const Context: TSpecFilterContext): Boolean;
    function MatchesTags(const Tags: TSpecTags): Boolean;
    procedure Free;
  end;

implementation

{ TSpecFilterContext }

class function TSpecFilterContext.FromScenario(const Scenario: IScenario): TSpecFilterContext;
var
  Parent: ISpecItem;
  Feature: IFeature;
  Rule: IRule;
begin
  Result.Tags := Scenario.EffectiveTags;
  Result.ScenarioDescription := Scenario.Description;
  Result.RuleDescription := '';
  Result.FeatureTitle := '';
  Result.SourceUnit := '';

  // Navegar hacia arriba para obtener Rule y Feature
  Parent := Scenario.Parent;
  while Assigned(Parent) do
  begin
    if Supports(Parent, IRule, Rule) then
    begin
      if Rule.Kind = sikRule then
        Result.RuleDescription := Rule.Description;
      Parent := Rule.Feature as ISpecItem;
    end
    else if Supports(Parent, IFeature, Feature) then
    begin
      Result.FeatureTitle := Feature.Title;
      Result.SourceUnit := Feature.SourceUnit;
      Break;
    end
    else
      Parent := Parent.Parent;
  end;
end;

{ TSpecFilter }

class function TSpecFilter.Parse(const Expression: string): TSpecFilter;
begin
  Result.FExpression := Trim(Expression);
  Result.FRoot := nil;
  Result.FTokenIndex := 0;

  if Result.FExpression.IsEmpty then
    Exit;

  // Reemplazar comas por OR para sintaxis abreviada
  var NormalizedExpr := StringReplace(Result.FExpression, ',', ' or ', [rfReplaceAll]);
  Result.FTokens := Tokenize(NormalizedExpr);
  Result.FTokenIndex := 0;
  Result.FRoot := Result.ParseOr;
end;

function TSpecFilter.IsEmpty: Boolean;
begin
  Result := FExpression.IsEmpty;
end;

function TSpecFilter.Matches(const Context: TSpecFilterContext): Boolean;
begin
  if FRoot = nil then
    Result := True
  else
    Result := EvalNode(FRoot, Context);
end;

function TSpecFilter.MatchesTags(const Tags: TSpecTags): Boolean;
var
  Ctx: TSpecFilterContext;
begin
  Ctx := Default(TSpecFilterContext);
  Ctx.Tags := Tags;
  Result := Matches(Ctx);
end;

procedure TSpecFilter.Free;
begin
  FreeNode(FRoot);
  FRoot := nil;
  FTokens := nil;
end;

class function TSpecFilter.Tokenize(const Expr: string): TArray<TToken>;
var
  Tokens: TArray<TToken>;
  i, Len: Integer;

  procedure AddToken(Kind: TTokenKind; const Value: string = '');
  begin
    SetLength(Tokens, Length(Tokens) + 1);
    Tokens[High(Tokens)].Kind := Kind;
    Tokens[High(Tokens)].Value := Value;
  end;

  procedure SkipWhitespace;
  begin
    while (i <= Len) and (Expr[i] = ' ') do
      Inc(i);
  end;

  function ReadUntilSpace: string;
  var
    StartPos: Integer;
  begin
    StartPos := i;
    // Leer hasta espacio, paréntesis o fin
    while (i <= Len) and not CharInSet(Expr[i], [' ', '(', ')']) do
      Inc(i);
    Result := Copy(Expr, StartPos, i - StartPos);
  end;

begin
  Tokens := nil;
  i := 1;
  Len := Length(Expr);

  while i <= Len do
  begin
    SkipWhitespace;
    if i > Len then Break;

    // Paréntesis
    if Expr[i] = '(' then
    begin
      AddToken(tkLParen);
      Inc(i);
    end
    else if Expr[i] = ')' then
    begin
      AddToken(tkRParen);
      Inc(i);
    end
    // Negación con ~
    else if Expr[i] = '~' then
    begin
      AddToken(tkNot);
      Inc(i);
    end
    // Tag @xxx
    else if Expr[i] = '@' then
    begin
      Inc(i); // Saltar el @
      var StartPos := i;
      while (i <= Len) and CharInSet(Expr[i], ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) do
        Inc(i);
      AddToken(tkTag, Copy(Expr, StartPos, i - StartPos));
    end
    // Filtros F:, S:, R:, U:
    else if (i + 1 <= Len) and CharInSet(Expr[i], ['F', 'f', 'S', 's', 'R', 'r', 'U', 'u']) and (Expr[i + 1] = ':') then
    begin
      var FilterType := UpCase(Expr[i]);
      Inc(i, 2); // Saltar X:
      var FilterValue := ReadUntilSpace;
      case FilterType of
        'F': AddToken(tkFeature, FilterValue);
        'S': AddToken(tkScenario, FilterValue);
        'R': AddToken(tkRule, FilterValue);
        'U': AddToken(tkUnit, FilterValue);
      end;
    end
    // Palabras clave: and, or, not
    else if CharInSet(Expr[i], ['a'..'z', 'A'..'Z']) then
    begin
      var StartPos := i;
      while (i <= Len) and CharInSet(Expr[i], ['a'..'z', 'A'..'Z']) do
        Inc(i);
      var Word := LowerCase(Copy(Expr, StartPos, i - StartPos));
      if Word = 'and' then
        AddToken(tkAnd)
      else if Word = 'or' then
        AddToken(tkOr)
      else if Word = 'not' then
        AddToken(tkNot)
      else
        raise Exception.CreateFmt('Token inesperado en expresión de filtro: %s', [Word]);
    end
    else
      raise Exception.CreateFmt('Carácter inesperado en expresión de filtro: %s', [Expr[i]]);
  end;

  AddToken(tkEOF);
  Result := Tokens;
end;

function TSpecFilter.CurrentToken: TToken;
begin
  if FTokenIndex < Length(FTokens) then
    Result := FTokens[FTokenIndex]
  else
  begin
    Result.Kind := tkEOF;
    Result.Value := '';
  end;
end;

procedure TSpecFilter.Advance;
begin
  if FTokenIndex < Length(FTokens) then
    Inc(FTokenIndex);
end;

function TSpecFilter.NewNode(Kind: TNodeKind; const Value: string; Left, Right: PNode): PNode;
begin
  New(Result);
  Result.Kind := Kind;
  Result.Value := Value;
  Result.Left := Left;
  Result.Right := Right;
end;

function TSpecFilter.ParseOr: PNode;
begin
  Result := ParseAnd;
  while CurrentToken.Kind = tkOr do
  begin
    Advance;
    Result := NewNode(nkOr, '', Result, ParseAnd);
  end;
end;

function TSpecFilter.ParseAnd: PNode;
begin
  Result := ParseUnary;
  while CurrentToken.Kind = tkAnd do
  begin
    Advance;
    Result := NewNode(nkAnd, '', Result, ParseUnary);
  end;
end;

function TSpecFilter.ParseUnary: PNode;
begin
  if CurrentToken.Kind = tkNot then
  begin
    Advance;
    Result := NewNode(nkNot, '', ParseUnary, nil);
  end
  else
    Result := ParsePrimary;
end;

function TSpecFilter.ParsePrimary: PNode;
begin
  case CurrentToken.Kind of
    tkTag:
      begin
        Result := NewNode(nkTag, CurrentToken.Value);
        Advance;
      end;
    tkFeature:
      begin
        Result := NewNode(nkFeature, CurrentToken.Value);
        Advance;
      end;
    tkScenario:
      begin
        Result := NewNode(nkScenario, CurrentToken.Value);
        Advance;
      end;
    tkRule:
      begin
        Result := NewNode(nkRule, CurrentToken.Value);
        Advance;
      end;
    tkUnit:
      begin
        Result := NewNode(nkUnit, CurrentToken.Value);
        Advance;
      end;
    tkLParen:
      begin
        Advance;
        Result := ParseOr;
        if CurrentToken.Kind <> tkRParen then
          raise Exception.Create('Se esperaba ) en expresión de filtro');
        Advance;
      end;
  else
    raise Exception.CreateFmt('Token inesperado en expresión de filtro: %d', [Ord(CurrentToken.Kind)]);
  end;
end;

function TSpecFilter.EvalNode(Node: PNode; const Context: TSpecFilterContext): Boolean;
begin
  if Node = nil then
    Exit(True);

  case Node.Kind of
    nkTag:
      Result := Context.Tags.Contains(Node.Value);
    nkFeature:
      Result := ContainsText(Context.FeatureTitle, Node.Value);
    nkScenario:
      Result := ContainsText(Context.ScenarioDescription, Node.Value);
    nkRule:
      Result := ContainsText(Context.RuleDescription, Node.Value);
    nkUnit:
      Result := ContainsText(Context.SourceUnit, Node.Value);
    nkNot:
      Result := not EvalNode(Node.Left, Context);
    nkAnd:
      Result := EvalNode(Node.Left, Context) and EvalNode(Node.Right, Context);
    nkOr:
      Result := EvalNode(Node.Left, Context) or EvalNode(Node.Right, Context);
  else
    Result := False;
  end;
end;

class procedure TSpecFilter.FreeNode(Node: PNode);
begin
  if Node = nil then Exit;
  FreeNode(Node.Left);
  FreeNode(Node.Right);
  Dispose(Node);
end;

end.
