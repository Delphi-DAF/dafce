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
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.WhereTree,
  Daf.Specification,
  Emission.SpecHelpers;

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
    /// For the scenarios that run the emitted query rather than only read it.
    Products: TObjectList<TProduct>;
    Names: TArray<string>;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Specification;

destructor TEmitterWorld.Destroy;
begin
  Tree.Free;   // the normalizer hands ownership to its caller
  Products.Free;
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
Feature Emitting an expression to WHERE clauses @expressions @emitter

  As the author of the one emission in this layer
  I want the emitted clause list to normalize back to what was written
  So both back-ends inherit a correct predicate instead of agreeing on a wrong one
''')

.UseWorld<TEmitterWorld>

.Rule('A disjunction under a conjunction keeps its grouping')

  .Scenario('OrUnderAndKeepsItsGroup')
    .When('(a or b) and c is emitted',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := (Leaf('a') or Leaf('b')) and Leaf('c');
        W.Query := Emit(W.Authored);
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
    .When('not ((a and b) or (c and d)) is emitted',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := not ((Leaf('a') and Leaf('b')) or (Leaf('c') and Leaf('d')));
        W.Query := EmitFrom('t', W.Authored);
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
    .When('a negated equality and a negated LIKE are emitted',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := not Leaf('a');
        W.Query := Emit(W.Authored);

        // The fluent .Like arrives with the breadth pass-throughs; the node it
        // will build is already expressible, so the emitter is exercised now.
        var Like := TNode.Compare(TCompareOp.Like, TNode.Member('Name'),
                                  [TNode.Value('%son%')]);
        W.Sql := SqlOf(EmitFrom('t', TNode.Unary(TUnaryOp.opNot, Like)));
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
          var Q := Emit(Shape);
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
    .When('a and b or c and d is emitted with no explicit grouping',
      procedure(W: TEmitterWorld)
      begin
        // Delphi's own precedence already binds `and` tighter than `or`, exactly as
        // SQL does, so no parentheses are written here either.
        W.Authored := Leaf('a') and Leaf('b') or Leaf('c') and Leaf('d');
        W.Query := Emit(W.Authored);
        W.Tree := Normalized(W.Query);
      end)
    .&Then('the emitter transcribed the tree it was given, deciding nothing',
      procedure(W: TEmitterWorld)
      begin
        // Delphi's parser already grouped this as Or(And(a,b), And(c,d)) — `and`
        // binds tighter than `or` in Delphi exactly as in SQL. The emitter walked
        // that tree and wrote one group per operator change: two conjunctions
        // under a disjunction, so two groups and nothing else at this level.
        //
        // The evidence is the SHAPE MATCHING THE TREE, not the list being flat. A
        // flat list was evidence for an earlier emission rule; it said nothing
        // about whether this layer computes precedence — and it does not, because
        // there is no precedence table, no operator ranking and no reordering
        // anywhere in the emission.
        var Root := W.Authored as ILogicalExpr;
        Expect(Root.Op = TLogicalOp.opOr).ToBeTrue;
        Expect(Root.Left.Kind = TExprKind.Logical).ToBeTrue;
        Expect(Root.Right.Kind = TExprKind.Logical).ToBeTrue;

        Expect(Length(WhereClausesOf(W.Query))).ToEqual(2);
        Expect(NestedGroupCount(W.Query)).ToEqual(2);
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
      end)

.Rule('Breadth is inherited by emission, not by new node types')

  .Scenario('LikeEmitsTheExistingClause')
    .When('LIKE, BETWEEN and IN expressions are emitted',
      procedure(W: TEmitterWorld)
      begin
        W.Authored := Prop('Name').Like('%son%');
        W.Query := Emit(W.Authored);
        W.Sql := SqlOf(EmitFrom('Product', W.Authored));
      end)
    .&Then('LIKE becomes SQLCute''s own LIKE clause, built by the same node type',
      procedure(W: TEmitterWorld)
      begin
        var Clauses := WhereClausesOf(W.Query);
        Expect(Length(Clauses)).ToEqual(1);
        Expect(Clauses[0] is TWhereClause).ToBeTrue;
        Expect(TWhereClause(Clauses[0]).Op = TWhereOp.Like).ToBeTrue;
        Expect(W.Sql).ToContain('LIKE');

        // No dedicated node type was introduced for it: the authored node is the
        // same class an equality produces, carrying a different operator value.
        Expect((W.Authored as TExprNode).ClassName)
          .ToEqual(((Prop('Price') > 1).Node as TExprNode).ClassName);
      end)
    .&Then('BETWEEN and IN likewise reach the clauses SQLCute already has',
      procedure(W: TEmitterWorld)
      begin
        var Between := WhereClausesOf(Emit(Prop('Price').Between(10, 200)));
        Expect(Length(Between)).ToEqual(1);
        Expect(TWhereClause(Between[0]).Op = TWhereOp.&Between).ToBeTrue;

        // IN has its own clause type in the AST, and the fluent verb sets its
        // Connector as the string 'AND'/'OR' the normalizer compares it as.
        var InSet := WhereClausesOf(Emit(Prop('Price').IsIn([5, 120])));
        Expect(Length(InSet)).ToEqual(1);
        Expect(InSet[0] is TWhereInClause).ToBeTrue;
        Expect(TWhereInClause(InSet[0]).Negated).ToBeFalse;
        Expect(TWhereInClause(InSet[0]).Connector).ToEqual('AND');
      end)

  .Scenario('OrderingAndPagingLowerWithTheFilter')
    .When('a specification with a filter, an ordering and a page is applied',
      procedure(W: TEmitterWorld)
      begin
        var Spec := TSpecification<TProduct>.New
          .Where(Prop('Active') = True)
          .OrderByDesc('Price')
          .Skip(1)
          .Take(2);
        W.Query := TExprEmitter.Apply<TProduct>(TQuery.New.From('Product'), Spec);
        W.Sql := SqlOf(W.Query);
      end)
    .&Then('one query carries all three, so a caller never applies them separately',
      procedure(W: TEmitterWorld)
      begin
        var Wheres := 0;
        var Orders := 0;
        var Limits := 0;
        var Offsets := 0;
        for var C in W.Query.Clauses do
        begin
          if C is TWhereClause then Inc(Wheres);
          if C is TOrderByClause then Inc(Orders);
          if C is TLimitClause then Inc(Limits);
          if C is TOffsetClause then Inc(Offsets);
        end;
        Expect(Wheres).ToEqual(1);
        Expect(Orders).ToEqual(1);
        Expect(Limits).ToEqual(1);
        Expect(Offsets).ToEqual(1);

        Expect(W.Sql).ToContain('WHERE');
        Expect(W.Sql).ToContain('ORDER BY');
        Expect(W.Sql).ToContain('DESC');
      end)

  .Scenario('EmissionNeedsNoTableName')
    .When('an expression is emitted with no FROM and run over a plain list',
      procedure(W: TEmitterWorld)
      begin
        W.Products := SampleProducts;
        W.Query := Emit((Prop('Price') > 100) and (Prop('Active') = True));
        W.Names := NamesOf(W.Products, W.Query);
      end)
    .&Then('no table was invented, and the in-memory lane ran it anyway',
      procedure(W: TEmitterWorld)
      begin
        for var C in W.Query.Clauses do
          Expect(C is TFromClause).ToBeFalse;
        // The source arrives as a parameter, so there is nothing for a FROM to name.
        Expect(Length(W.Names)).ToEqual(2);
        Expect(W.Names[0]).ToEqual('Anvil');
        Expect(W.Names[1]).ToEqual('Engine');
      end)

  // The emitter used to refuse an empty set rather than hand SQLCute a clause
  // that compiled to `IN ()`. With the builder resolving the empty case itself,
  // there is nothing here to refuse: IN goes through the same call every other
  // operator does.
  .Scenario('EmptySetEmitsWithoutRaising')
    .When('a membership expression over an empty value set is emitted and run',
      procedure(W: TEmitterWorld)
      begin
        W.Products := SampleProducts;
        W.Query := Emit(Prop('Price').IsIn([]));
        W.Names := NamesOf(W.Products, W.Query);
      end)
    .&Then('it emitted without raising and the query selects nothing',
      procedure(W: TEmitterWorld)
      begin
        // Drill's Price is NULL, and it is excluded like every other row: the
        // empty disjunction is False without consulting the column at all.
        Expect(Length(W.Names)).ToEqual(0);
      end)
    .&Then('and it emitted no IN clause for the emitter or a compiler to puzzle over',
      procedure(W: TEmitterWorld)
      begin
        for var C in WhereClausesOf(W.Query) do
          Expect(C is TWhereInClause).ToBeFalse;
      end)

// ---------------------------------------------------------------------------
// Every way emission refuses, one scenario per raise site.
//
// The stake is not tidiness. Emitting fewer clauses than the expression carries
// *widens* the resulting query — the query returns rows it was asked to exclude
// — and it does so silently, on both back-ends at once, since both read the
// clause list that came out. So each of these has to raise, and each has to say
// what was wrong: a caller who lands here is looking at an expression that
// cannot mean what they wrote.

.Rule('Emission refuses what it cannot express')

  .Scenario('NonMemberComparisonSidesAreRefused')
    .&Then('a comparison whose left-hand side is not a member reference raises',
      procedure(W: TEmitterWorld)
      begin
        // Value = Value: nothing here names a column, so there is no condition
        // to emit and no defensible clause to emit instead.
        var Bad := TNode.Compare(TCompareOp.Equal, TNode.Value(1), [TNode.Value(1)]);
        ExpectException(
          procedure
          begin
            Emit(Bad);
          end).ToBe(EExprEmissionError);
      end)
    .&Then('and a comparison whose right-hand side is neither value nor parameter raises',
      procedure(W: TEmitterWorld)
      begin
        var Bad := TNode.Compare(TCompareOp.Equal, TNode.Member('Price'),
                                 [TNode.Member('Name')]);
        ExpectException(
          procedure
          begin
            Emit(Bad);
          end).ToBe(EExprEmissionError);
      end)

  .Scenario('WrongArityIsRefused')
    .&Then('BETWEEN given one operand raises, naming operator and arity',
      procedure(W: TEmitterWorld)
      begin
        var Bad := TNode.Compare(TCompareOp.&Between, TNode.Member('Price'),
                                 [TNode.Value(10)]);
        ExpectException(
          procedure
          begin
            Emit(Bad);
          end).ToMatchMessage('Between.*2.*1');
      end)
    .&Then('and IS NULL given an operand raises too — arity is checked in both directions',
      procedure(W: TEmitterWorld)
      begin
        var Bad := TNode.Compare(TCompareOp.IsNull, TNode.Member('Price'),
                                 [TNode.Value(10)]);
        ExpectException(
          procedure
          begin
            Emit(Bad);
          end).ToBe(EExprEmissionError);
      end)

  .Scenario('ANodeThatIsNotAPredicateIsRefused')
    .&Then('a bare member where a condition belongs raises, naming the node kind',
      procedure(W: TEmitterWorld)
      begin
        // `Prop('Active')` on its own is a column reference, not a condition.
        // Emitting it as `Active = True` would be the emitter guessing.
        ExpectException(
          procedure
          begin
            Emit(TNode.Member('Active'));
          end).ToMatchMessage('Member');
      end)

  // Two things look alike here and must not be confused, because the difference
  // is between an unfiltered rule and a widened one.
  .Scenario('NoFilterIsNotTheSameAsABrokenOne')
    .&Then('a whole expression that is absent leaves the query unfiltered, deliberately',
      procedure(W: TEmitterWorld)
      begin
        // Apply<T> hands Spec.Filter straight through, and that is nil for a rule
        // built with only an ordering or a page. Emitting nothing is what such a
        // rule means, so this is a contract and not an oversight.
        var Q := Emit(nil);
        Expect(Length(WhereClausesOf(Q))).ToEqual(0);
      end)
    .&Then('but an expression with a hole in it raises rather than emitting the rest',
      procedure(W: TEmitterWorld)
      begin
        // Here the caller did ask for a condition. Skipping the missing operand
        // would emit `b` alone in place of `a AND b` — strictly more rows than
        // were asked for, on both back-ends, with nothing said.
        var Holed := TNode.Logical(TLogicalOp.opAnd, nil, Leaf('b'));
        ExpectException(
          procedure
          begin
            Emit(Holed);
          end).ToBe(EExprEmissionError);
      end)

  .Scenario('EmissionNeedsAQueryToEmitOnto')
    .&Then('emitting onto no query raises rather than inventing one',
      procedure(W: TEmitterWorld)
      begin
        ExpectException(
          procedure
          begin
            TExprEmitter.Where(nil, Prop('Price') > 100);
          end).ToBe(EExprEmissionError);
      end);

end.
