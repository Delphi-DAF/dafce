unit Lowering.SpecHelpers;

{
  Shared harness for the emitter specs: lower an expression, read the clause list
  back, normalize it the way both back-ends will, and evaluate both the authored
  tree and the normalized one under SQL's three truth values.

  The evaluators are deliberately narrow. A leaf is an equality on a member named
  'a'..'d', and anything else raises rather than being interpreted generously —
  a harness that guesses would be able to agree with itself while the emitter was
  wrong, which is the one outcome that would make these specs worthless.
}

interface

uses
  System.SysUtils,
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.WhereTree,
  Daf.Query.Expressions;

type
  /// <summary>SQL's three truth values, so NULL behaviour is pinned by construction.</summary>
  TTri = (triFalse, triTrue, triUnknown);

  /// <summary>One truth value per leaf; leaf 'a' is index 0, 'b' is 1, and so on.</summary>
  TAssignment = TArray<TTri>;

  /// <summary>The harness was handed something it refuses to interpret.</summary>
  EEvalHarness = class(Exception);

function Not3(V: TTri): TTri;
function And3(A, B: TTri): TTri;
function Or3(A, B: TTri): TTri;
function TriName(V: TTri): string;

/// <summary>A leaf: an equality on the member named <c>Name</c>.</summary>
function Leaf(const Name: string): TExpr;

/// <summary>
///   Lowers <c>E</c> onto a fresh query. No FROM clause: the in-memory lane takes
///   its source as a parameter, so nothing here needs to invent a table name.
/// </summary>
function Lower(const E: IExpr): IQuery;

/// <summary>Lowers <c>E</c> onto a query with a FROM, for the scenarios that read SQL.</summary>
function LowerFrom(const Table: string; const E: IExpr): IQuery;

/// <summary>The WHERE-family clauses of <c>Q</c>, in order.</summary>
function WhereClausesOf(const Q: IQuery): TArray<TAbstractClause>;

/// <summary>How many clauses of <c>Q</c> are nested groups.</summary>
function NestedGroupCount(const Q: IQuery): Integer;

/// <summary><c>Q</c>'s clause list as the normalizer reconstructs it. The caller frees it.</summary>
function Normalized(const Q: IQuery): TWhereNode;

/// <summary>The authored tree's value under <c>A</c>.</summary>
function EvalExpr(const Node: IExpr; const A: TAssignment): TTri;

/// <summary>The normalized tree's value under <c>A</c>.</summary>
function EvalTree(N: TWhereNode; const A: TAssignment): TTri;

/// <summary>The ANSI SQL <c>Q</c> compiles to.</summary>
function SqlOf(const Q: IQuery): string;

/// <summary>
///   Every assignment of the three truth values to <c>LeafCount</c> leaves, as
///   base-3 counting. Four leaves is 81 cases: exhaustive and cheap.
/// </summary>
function AllAssignments(LeafCount: Integer): TArray<TAssignment>;

implementation

uses
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Expressions;

function Not3(V: TTri): TTri;
begin
  case V of
    triFalse: Result := triTrue;
    triTrue: Result := triFalse;
  else
    Result := triUnknown;   // NOT unknown stays unknown — the 3VL discriminator
  end;
end;

function And3(A, B: TTri): TTri;
begin
  if (A = triFalse) or (B = triFalse) then
    Result := triFalse
  else if (A = triUnknown) or (B = triUnknown) then
    Result := triUnknown
  else
    Result := triTrue;
end;

function Or3(A, B: TTri): TTri;
begin
  if (A = triTrue) or (B = triTrue) then
    Result := triTrue
  else if (A = triUnknown) or (B = triUnknown) then
    Result := triUnknown
  else
    Result := triFalse;
end;

function TriName(V: TTri): string;
begin
  case V of
    triFalse: Result := 'False';
    triTrue: Result := 'True';
  else
    Result := 'Unknown';
  end;
end;

function Leaf(const Name: string): TExpr;
begin
  Result := Prop(Name) = 1;
end;

function TriOf(const A: TAssignment; const Name: string): TTri;
begin
  if Length(Name) <> 1 then
    raise EEvalHarness.CreateFmt('Leaf "%s" is not one of a..d.', [Name]);
  var I := Ord(Name[1]) - Ord('a');
  if (I < 0) or (I > High(A)) then
    raise EEvalHarness.CreateFmt('Leaf "%s" is not part of this assignment.', [Name]);
  Result := A[I];
end;

function Lower(const E: IExpr): IQuery;
begin
  Result := TExprLowering.Where(TQuery.New, E);
end;

function LowerFrom(const Table: string; const E: IExpr): IQuery;
begin
  Result := TExprLowering.Where(TQuery.New.From(Table), E);
end;

function IsWhereFamily(C: TAbstractClause): Boolean;
begin
  // The four types TWhereTree.IsWhereClause accepts. Emitting anything else would
  // make the clause vanish from the normalized tree.
  Result := (C is TWhereClause) or (C is TNestedWhereClause)
         or (C is TWhereInClause) or (C is TDateWhereClause);
end;

function WhereClausesOf(const Q: IQuery): TArray<TAbstractClause>;
begin
  Result := nil;
  for var C in Q.Clauses do
    if IsWhereFamily(C) then
      Result := Result + [C];
end;

function NestedGroupCount(const Q: IQuery): Integer;
begin
  Result := 0;
  for var C in Q.Clauses do
    if C is TNestedWhereClause then
      Inc(Result);
end;

function Normalized(const Q: IQuery): TWhereNode;
begin
  Result := TWhereTree.Build(Q.Clauses);
end;

function EvalExpr(const Node: IExpr; const A: TAssignment): TTri;
begin
  if Node = nil then
    raise EEvalHarness.Create('Nothing to evaluate.');
  case Node.Kind of
    TExprKind.Compare:
      begin
        var C := Node as ICompareExpr;
        if C.Op <> TCompareOp.Equal then
          raise EEvalHarness.Create('The harness only interprets equality leaves.');
        Result := TriOf(A, (C.Target as IMemberExpr).Name);
      end;
    TExprKind.Logical:
      begin
        var L := Node as ILogicalExpr;
        if L.Op = TLogicalOp.opAnd then
          Result := And3(EvalExpr(L.Left, A), EvalExpr(L.Right, A))
        else
          Result := Or3(EvalExpr(L.Left, A), EvalExpr(L.Right, A));
      end;
    TExprKind.Unary:
      Result := Not3(EvalExpr((Node as IUnaryExpr).Operand, A));
  else
    raise EEvalHarness.Create('Not a predicate node.');
  end;
end;

function EvalTree(N: TWhereNode; const A: TAssignment): TTri;
begin
  if N = nil then
    raise EEvalHarness.Create('Nothing to evaluate.');
  case N.Kind of
    TWhereNodeKind.Leaf:
      begin
        if not (N.Clause is TWhereClause) then
          raise EEvalHarness.Create('The harness only interprets plain WHERE leaves.');
        var W := TWhereClause(N.Clause);
        if W.Op <> TWhereOp.Equal then
          raise EEvalHarness.Create('The harness only interprets equality leaves.');
        // W.IsNot is not read here on purpose: the normalizer already wrapped a
        // negated clause in a Neg node, and reading it twice would cancel out.
        Result := TriOf(A, W.Column);
      end;
    TWhereNodeKind.Neg:
      Result := Not3(EvalTree(N.Children[0], A));
    TWhereNodeKind.Conjunction:
      begin
        Result := triTrue;
        for var Child in N.Children do
          Result := And3(Result, EvalTree(Child, A));
      end;
    TWhereNodeKind.Disjunction:
      begin
        Result := triFalse;
        for var Child in N.Children do
          Result := Or3(Result, EvalTree(Child, A));
      end;
  else
    raise EEvalHarness.Create('Unknown node kind.');
  end;
end;

function SqlOf(const Q: IQuery): string;
begin
  var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
  Result := Compiler.Compile(Q).SQL;
end;

function AllAssignments(LeafCount: Integer): TArray<TAssignment>;
begin
  var Total := 1;
  for var I := 1 to LeafCount do
    Total := Total * 3;
  SetLength(Result, Total);
  for var N := 0 to Total - 1 do
  begin
    SetLength(Result[N], LeafCount);
    var Rest := N;
    for var I := 0 to LeafCount - 1 do
    begin
      Result[N][I] := TTri(Rest mod 3);
      Rest := Rest div 3;
    end;
  end;
end;

end.
