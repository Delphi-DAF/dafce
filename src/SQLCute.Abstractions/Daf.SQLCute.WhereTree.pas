unit Daf.SQLCute.WhereTree;

{$SCOPEDENUMS ON}

{
  Shared WHERE-tree normalizer (Decision 13). Turns a flat WHERE clause list —
  where each clause carries a Connector (AND/OR) joining it with the previous —
  into a precedence-correct boolean tree, applying SQL operator precedence:
  consecutive AND runs are grouped, then those groups are OR-ed. Nested groups
  (TNestedWhereClause) recurse; a clause's IsNot wraps its node in NOT(...).

  Both back-ends consume the SAME tree:
    * TAnsiSqlCompiler.CompileWhere walks it and emits explicit parentheses, so
      precedence no longer relies on the database parser.
    * The in-memory compiler builds its (tri-state) predicate from it, so the two
      back-ends agree on precedence by construction.

  Depth-first left-to-right traversal preserves the original clause order, so a
  consumer that emits positional bindings while walking keeps the same order the
  flat list produced.

  The tree owns its interior nodes (freed recursively); a leaf's Clause is NOT
  owned — it points back into the query's AST.
}

interface

uses
  Daf.SQLCute.Clauses;

type
  TWhereNodeKind = (Leaf, Neg, Conjunction, Disjunction);

  /// <summary>A node of the normalized boolean WHERE tree.</summary>
  TWhereNode = class
  public
    Kind: TWhereNodeKind;
    /// <summary>The condition, for a <c>Leaf</c> node. Not owned.</summary>
    Clause: TAbstractClause;
    /// <summary>Operands for <c>Conjunction</c>/<c>Disjunction</c>, or the single child of <c>Neg</c>. Owned.</summary>
    Children: TArray<TWhereNode>;
    destructor Destroy; override;
    class function NewLeaf(AClause: TAbstractClause): TWhereNode; static;
    class function NewNeg(Child: TWhereNode): TWhereNode; static;
    class function NewNode(AKind: TWhereNodeKind; const AChildren: TArray<TWhereNode>): TWhereNode; static;
  end;

  /// <summary>Builds precedence-correct boolean trees from flat WHERE clause lists.</summary>
  TWhereTree = record
    /// <summary>
    ///   Normalizes the WHERE-relevant clauses in <c>Clauses</c> into a boolean
    ///   tree (AND grouped before OR; nested groups recursed). Returns <c>nil</c>
    ///   when there are no WHERE clauses. The caller owns and frees the result.
    /// </summary>
    class function Build(const Clauses: TArray<TAbstractClause>): TWhereNode; static;
  end;

implementation

uses
  System.SysUtils;

function IsWhereClause(C: TAbstractClause): Boolean;
begin
  Result := (C is TWhereClause) or (C is TNestedWhereClause)
         or (C is TWhereInClause) or (C is TDateWhereClause);
end;

function ConnectorIsOr(C: TAbstractClause): Boolean;
begin
  if C is TWhereClause then
    Result := TWhereClause(C).Connector = TBoolOp.opOr
  else if C is TNestedWhereClause then
    Result := TNestedWhereClause(C).Connector = TBoolOp.opOr
  else if C is TDateWhereClause then
    Result := TDateWhereClause(C).Connector = TBoolOp.opOr
  else if C is TWhereInClause then
    Result := SameText(TWhereInClause(C).Connector, 'OR')
  else
    Result := False;
end;

function ClauseIsNot(C: TAbstractClause): Boolean;
begin
  if C is TWhereClause then
    Result := TWhereClause(C).IsNot
  else if C is TNestedWhereClause then
    Result := TNestedWhereClause(C).IsNot
  else
    Result := False;
end;

// Turns one clause into a node: nested groups recurse into a subtree; every
// other clause becomes a leaf. IsNot wraps the node in Neg.
function NodeForClause(C: TAbstractClause): TWhereNode;
begin
  if C is TNestedWhereClause then
    Result := TWhereTree.Build(TNestedWhereClause(C).SubClauses)
  else
    Result := TWhereNode.NewLeaf(C);
  if (Result <> nil) and ClauseIsNot(C) then
    Result := TWhereNode.NewNeg(Result);
end;

{ TWhereNode }

destructor TWhereNode.Destroy;
begin
  for var Child in Children do
    Child.Free;
  inherited;
end;

class function TWhereNode.NewLeaf(AClause: TAbstractClause): TWhereNode;
begin
  Result := TWhereNode.Create;
  Result.Kind := TWhereNodeKind.Leaf;
  Result.Clause := AClause;
end;

class function TWhereNode.NewNeg(Child: TWhereNode): TWhereNode;
begin
  Result := TWhereNode.Create;
  Result.Kind := TWhereNodeKind.Neg;
  Result.Children := [Child];
end;

class function TWhereNode.NewNode(AKind: TWhereNodeKind; const AChildren: TArray<TWhereNode>): TWhereNode;
begin
  Result := TWhereNode.Create;
  Result.Kind := AKind;
  Result.Children := AChildren;
end;

{ TWhereTree }

class function TWhereTree.Build(const Clauses: TArray<TAbstractClause>): TWhereNode;
var
  AndGroups: TArray<TWhereNode>;
  CurrentAnd: TArray<TWhereNode>;
  First: Boolean;

  // Close the AND run being accumulated and add it as one OR-group operand.
  procedure FlushAndGroup;
  begin
    if Length(CurrentAnd) = 0 then
      Exit;
    if Length(CurrentAnd) = 1 then
      AndGroups := AndGroups + [CurrentAnd[0]]
    else
      AndGroups := AndGroups + [TWhereNode.NewNode(TWhereNodeKind.Conjunction, CurrentAnd)];
    CurrentAnd := nil;
  end;

begin
  AndGroups := nil;
  CurrentAnd := nil;
  First := True;
  for var C in Clauses do
  begin
    if not IsWhereClause(C) then
      Continue;
    var Node := NodeForClause(C);
    if Node = nil then
      Continue;                       // empty nested group — nothing to add
    if (not First) and ConnectorIsOr(C) then
      FlushAndGroup;                  // OR boundary: close the current AND run
    CurrentAnd := CurrentAnd + [Node];
    First := False;
  end;
  FlushAndGroup;

  if Length(AndGroups) = 0 then
    Result := nil
  else if Length(AndGroups) = 1 then
    Result := AndGroups[0]            // no spurious OR wrapper for a single run
  else
    Result := TWhereNode.NewNode(TWhereNodeKind.Disjunction, AndGroups);
end;

end.
