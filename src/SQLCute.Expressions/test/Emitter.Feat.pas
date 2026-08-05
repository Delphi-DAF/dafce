unit Emitter.Feat;

{
  The emitter, which is where the one real correctness risk of this design lives.

  TWhereTree.Build reads a flat clause list as OR-of-AND-runs, so a disjunction
  inlined into an enclosing conjunction comes back as a different predicate. Both
  back-ends consume that same normalized tree, which means the parity harness
  structurally cannot catch it: SQL and in-memory agree, on the wrong answer. So
  it is asserted here, at the emitter, and the headline assertion is semantic —
  every three-valued assignment evaluates the same on the authored tree and on
  the normalized one — because inlining associative runs legitimately changes the
  tree's shape and a structural comparison would reject correct output.
}

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.WhereTree,
  Daf.Query.Expressions;

type
  TEmitterWorld = class
  public
    Authored: IExpr;
    Query: IQuery;
    Tree: TWhereNode;
    Sql: string;
    /// Assignments where the two evaluations disagreed. Must stay empty.
    Divergences: TArray<string>;
    Checked: Integer;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute.Clauses,
  Lowering.SpecHelpers;

destructor TEmitterWorld.Destroy;
begin
  Tree.Free;   // the normalizer hands ownership to its caller
  inherited;
end;

// The three shapes the exhaustive check runs over. One would satisfy the
// requirement; three cost the same and cover nesting, both negation forms and
// associative runs.
function ShapesUnderTest: TArray<IExpr>;
begin
  var A := Leaf('a');
  var B := Leaf('b');
  var C := Leaf('c');
  var D := Leaf('d');
  Result := [
    // a negated subtree, and an OR nested under an AND
    IExpr(not (A and B) or ((C or D) and A)),
    // conjunction of two disjunctions: nesting on both sides
    IExpr((A or B) and (C or D)),
    // associative runs the emitter is allowed to inline, plus a negated leaf
    IExpr((A and B and C) or (not D))
  ];
end;

initialization

Feature('''
Feature Lowering an expression to WHERE clauses @expressions @emitter

  As the author of the one lowering in this layer
  I want the emitted clause list to normalize back to what was written
  So both back-ends inherit a correct predicate instead of agreeing on a wrong one
''')

.UseWorld<TEmitterWorld>

.Rule('A disjunction under a conjunction keeps its grouping')

  .Scenario('OrUnderAndKeepsItsGroup')
    .When('(a or b) and c is lowered',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := (Leaf('a') or Leaf('b')) and Leaf('c');
        W.Query := Lower(W.Authored);
        W.Tree := Normalized(W.Query);
      end)
    .&Then('the disjunction is emitted as a nested group, not inlined',
      procedure(W: TEmitterWorld)
      begin
        var Clauses := WhereClausesOf(W.Query);
        Expect(Length(Clauses)).ToEqual(2);
        Expect(Clauses[0] is TNestedWhereClause).ToBeTrue;
        Expect(Clauses[1] is TWhereClause).ToBeTrue;
      end)
    .&Then('normalizing yields (a or b) AND c, not a OR (b and c)',
      procedure(W: TEmitterWorld)
      begin
        // The flat-list bug would make this a Disjunction. That is the whole point
        // of the scenario, so it is asserted first and explicitly.
        Expect(W.Tree.Kind = TWhereNodeKind.Disjunction).ToBeFalse;
        Expect(W.Tree.Kind = TWhereNodeKind.Conjunction).ToBeTrue;
        Expect(Length(W.Tree.Children)).ToEqual(2);

        var Group := W.Tree.Children[0];
        Expect(Group.Kind = TWhereNodeKind.Disjunction).ToBeTrue;
        Expect(Length(Group.Children)).ToEqual(2);
        Expect(TWhereClause(Group.Children[0].Clause).Column).ToEqual('a');
        Expect(TWhereClause(Group.Children[1].Clause).Column).ToEqual('b');

        var Rest := W.Tree.Children[1];
        Expect(Rest.Kind = TWhereNodeKind.Leaf).ToBeTrue;
        Expect(TWhereClause(Rest.Clause).Column).ToEqual('c');
      end)

.Rule('Negation is emitted, never rewritten')

  .Scenario('NegatedGroupSurvivesAsNegatedGroup')
    .When('not ((a and b) or (c and d)) is lowered',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := not ((Leaf('a') and Leaf('b')) or (Leaf('c') and Leaf('d')));
        W.Query := LowerFrom('t', W.Authored);
        W.Tree := Normalized(W.Query);
        W.Sql := SqlOf(W.Query);
      end)
    .&Then('it is one negated nested group holding the disjunction',
      procedure(W: TEmitterWorld)
      begin
        var Clauses := WhereClausesOf(W.Query);
        Expect(Length(Clauses)).ToEqual(1);
        Expect(Clauses[0] is TNestedWhereClause).ToBeTrue;
        Expect(TNestedWhereClause(Clauses[0]).IsNot).ToBeTrue;

        Expect(W.Tree.Kind = TWhereNodeKind.Neg).ToBeTrue;
        var Inner := W.Tree.Children[0];
        Expect(Inner.Kind = TWhereNodeKind.Disjunction).ToBeTrue;
        Expect(Length(Inner.Children)).ToEqual(2);
        Expect(Inner.Children[0].Kind = TWhereNodeKind.Conjunction).ToBeTrue;
        Expect(Inner.Children[1].Kind = TWhereNodeKind.Conjunction).ToBeTrue;
      end)
    .&Then('the SQL reads as NOT around the authored shape, not a distributed equivalent',
      procedure(W: TEmitterWorld)
      begin
        Expect(W.Sql).ToContain('NOT (');
        // De Morgan would have produced four negated comparisons and no NOT group.
        Expect(W.Sql.CountChar('(')).ToBeGreaterThan(1);
      end)

  .Scenario('NegatedLeafNeedsNoGroup')
    .When('a negated equality and a negated LIKE are lowered',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := not Leaf('a');
        W.Query := Lower(W.Authored);

        // The fluent .Like arrives with the breadth pass-throughs; the node it
        // will build is already expressible, so the emitter is exercised now.
        var Like := TNode.Compare(TCompareOp.Like, TNode.Member('Name'),
                                  [TNode.Value('%son%')]);
        W.Sql := SqlOf(LowerFrom('t', TNode.Unary(TUnaryOp.opNot, Like)));
      end)
    .&Then('each is a single clause carrying its own negation, with no group',
      procedure(W: TEmitterWorld)
      begin
        var Clauses := WhereClausesOf(W.Query);
        Expect(Length(Clauses)).ToEqual(1);
        Expect(Clauses[0] is TWhereClause).ToBeTrue;
        Expect(TWhereClause(Clauses[0]).IsNot).ToBeTrue;
        Expect(NestedGroupCount(W.Query)).ToEqual(0);

        // The negated LIKE took the paired operator, so no NOT wrapper appears.
        Expect(W.Sql).ToContain('NOT LIKE');
        Expect(W.Sql).ToNotEqual('');
      end)

.Rule('The emitted list is logically equivalent to what was authored')

  .Scenario('EquivalenceHoldsForEveryThreeValuedAssignment')
    .When('every assignment of True, False and Unknown to four leaves is evaluated',
      procedure(W: TEmitterWorld)
      begin
        for var Shape in ShapesUnderTest do
        begin
          var Q := Lower(Shape);
          var Tree := Normalized(Q);
          try
            for var A in AllAssignments(4) do
            begin
              Inc(W.Checked);
              var Wanted := EvalExpr(Shape, A);
              var Got := EvalTree(Tree, A);
              if Wanted <> Got then
                W.Divergences := W.Divergences + [Format(
                  'a=%s b=%s c=%s d=%s: authored %s, emitted %s',
                  [TriName(A[0]), TriName(A[1]), TriName(A[2]), TriName(A[3]),
                   TriName(Wanted), TriName(Got)])];
            end;
          finally
            Tree.Free;
          end;
        end;
      end)
    .&Then('the normalized tree agrees with the authored one everywhere',
      procedure(W: TEmitterWorld)
      begin
        Expect(W.Checked).ToEqual(3 * 81);
        Expect(string.Join(' | ', W.Divergences)).ToEqual('');
      end)

.Rule('Precedence is inherited from the normalizer, not implemented here')

  .Scenario('PrecedenceIsNotImplementedHere')
    .When('a and b or c and d is lowered with no explicit grouping',
      procedure(W: TEmitterWorld)
      begin
        // Delphi's own precedence already binds `and` tighter than `or`, exactly as
        // SQL does, so no parentheses are written here either.
        W.Authored := Leaf('a') and Leaf('b') or Leaf('c') and Leaf('d');
        W.Query := Lower(W.Authored);
        W.Tree := Normalized(W.Query);
      end)
    .&Then('the emitter produced a flat list with no group of its own',
      procedure(W: TEmitterWorld)
      begin
        // Nothing about the grouping came from this layer: it emitted four plain
        // clauses and let the shared normalizer decide.
        Expect(Length(WhereClausesOf(W.Query))).ToEqual(4);
        Expect(NestedGroupCount(W.Query)).ToEqual(0);
      end)
    .&Then('and the normalizer grouped it as (a and b) or (c and d)',
      procedure(W: TEmitterWorld)
      begin
        Expect(W.Tree.Kind = TWhereNodeKind.Disjunction).ToBeTrue;
        Expect(Length(W.Tree.Children)).ToEqual(2);
        for var Child in W.Tree.Children do
        begin
          Expect(Child.Kind = TWhereNodeKind.Conjunction).ToBeTrue;
          Expect(Length(Child.Children)).ToEqual(2);
        end;
      end);

end.
