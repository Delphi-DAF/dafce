unit Daf.MiniSpec.TagFilter;

interface

uses
  System.SysUtils,
  System.RegularExpressions,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Parsea y evalúa expresiones de filtro de tags.
  /// Sintaxis soportada:
  ///   @tag           - tiene el tag
  ///   ~@tag          - NO tiene el tag (negación)
  ///   not @tag       - NO tiene el tag (alternativa)
  ///   @a and @b      - tiene ambos tags
  ///   @a or @b       - tiene alguno de los tags
  ///   @a,@b          - atajo para OR
  ///   (@a or @b) and ~@c - expresiones con paréntesis
  /// </summary>
  TTagExpression = record
  private type
    TTokenKind = (tkTag, tkNot, tkAnd, tkOr, tkLParen, tkRParen, tkEOF);
    TToken = record
      Kind: TTokenKind;
      Value: string;
    end;
    TNodeKind = (nkTag, nkNot, nkAnd, nkOr);
    PNode = ^TNode;
    TNode = record
      Kind: TNodeKind;
      Tag: string;
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
    function NewNode(Kind: TNodeKind; const Tag: string = ''; Left: PNode = nil; Right: PNode = nil): PNode;
    function EvalNode(Node: PNode; const Tags: TSpecTags): Boolean;
    class procedure FreeNode(Node: PNode); static;
  public
    class function Parse(const Expression: string): TTagExpression; static;
    function IsEmpty: Boolean;
    function Matches(const Tags: TSpecTags): Boolean;
    procedure Free;
  end;

implementation

{ TTagExpression }

class function TTagExpression.Parse(const Expression: string): TTagExpression;
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

function TTagExpression.IsEmpty: Boolean;
begin
  Result := FExpression.IsEmpty;
end;

function TTagExpression.Matches(const Tags: TSpecTags): Boolean;
begin
  if FRoot = nil then
    Result := True
  else
    Result := EvalNode(FRoot, Tags);
end;

procedure TTagExpression.Free;
begin
  FreeNode(FRoot);
  FRoot := nil;
  FTokens := nil;
end;

class function TTagExpression.Tokenize(const Expr: string): TArray<TToken>;
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
    // Tag @xxx - almacenar sin el @
    else if Expr[i] = '@' then
    begin
      Inc(i); // Saltar el @
      var StartPos := i;
      while (i <= Len) and CharInSet(Expr[i], ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) do
        Inc(i);
      AddToken(tkTag, Copy(Expr, StartPos, i - StartPos));
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
        raise Exception.CreateFmt('Token inesperado en expresión de tags: %s', [Word]);
    end
    else
      raise Exception.CreateFmt('Carácter inesperado en expresión de tags: %s', [Expr[i]]);
  end;

  AddToken(tkEOF);
  Result := Tokens;
end;

function TTagExpression.CurrentToken: TToken;
begin
  if FTokenIndex < Length(FTokens) then
    Result := FTokens[FTokenIndex]
  else
  begin
    Result.Kind := tkEOF;
    Result.Value := '';
  end;
end;

procedure TTagExpression.Advance;
begin
  if FTokenIndex < Length(FTokens) then
    Inc(FTokenIndex);
end;

function TTagExpression.NewNode(Kind: TNodeKind; const Tag: string; Left, Right: PNode): PNode;
begin
  New(Result);
  Result.Kind := Kind;
  Result.Tag := Tag;
  Result.Left := Left;
  Result.Right := Right;
end;

function TTagExpression.ParseOr: PNode;
begin
  Result := ParseAnd;
  while CurrentToken.Kind = tkOr do
  begin
    Advance;
    Result := NewNode(nkOr, '', Result, ParseAnd);
  end;
end;

function TTagExpression.ParseAnd: PNode;
begin
  Result := ParseUnary;
  while CurrentToken.Kind = tkAnd do
  begin
    Advance;
    Result := NewNode(nkAnd, '', Result, ParseUnary);
  end;
end;

function TTagExpression.ParseUnary: PNode;
begin
  if CurrentToken.Kind = tkNot then
  begin
    Advance;
    Result := NewNode(nkNot, '', ParseUnary, nil);
  end
  else
    Result := ParsePrimary;
end;

function TTagExpression.ParsePrimary: PNode;
begin
  case CurrentToken.Kind of
    tkTag:
      begin
        Result := NewNode(nkTag, CurrentToken.Value);
        Advance;
      end;
    tkLParen:
      begin
        Advance;
        Result := ParseOr;
        if CurrentToken.Kind <> tkRParen then
          raise Exception.Create('Se esperaba ) en expresión de tags');
        Advance;
      end;
  else
    raise Exception.CreateFmt('Token inesperado en expresión de tags: %d', [Ord(CurrentToken.Kind)]);
  end;
end;

function TTagExpression.EvalNode(Node: PNode; const Tags: TSpecTags): Boolean;
begin
  if Node = nil then
    Exit(True);

  case Node.Kind of
    nkTag:
      Result := Tags.Contains(Node.Tag);
    nkNot:
      Result := not EvalNode(Node.Left, Tags);
    nkAnd:
      Result := EvalNode(Node.Left, Tags) and EvalNode(Node.Right, Tags);
    nkOr:
      Result := EvalNode(Node.Left, Tags) or EvalNode(Node.Right, Tags);
  else
    Result := False;
  end;
end;

class procedure TTagExpression.FreeNode(Node: PNode);
begin
  if Node = nil then Exit;
  FreeNode(Node.Left);
  FreeNode(Node.Right);
  Dispose(Node);
end;

end.
