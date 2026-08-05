unit Model.Feat;

{
  The node model itself. Two properties are asserted here because both are
  claims about the shape of the model rather than about any behaviour, and both
  are the kind of claim that erodes silently:

    * the node set is bounded — operators are data, so adding one adds no class;
    * the model's vocabulary is free of SQL-only concepts.

  Both assert over RTTI, so a later addition to an enum or a new node class
  breaks the spec instead of slipping past it.
}

interface

uses
  Daf.Query.Expressions;

type
  TModelWorld = class
  public
    /// The class name behind each node built, one per comparison operator.
    ClassesUsed: TArray<string>;
    /// Every node kind observed while building those comparisons.
    KindsSeen: TArray<TExprKind>;
    /// The concrete node classes the model declares.
    NodeClasses: TArray<string>;
    /// Every operator name across the three enums.
    OperatorNames: TArray<string>;
    /// The operator names that read as SQL-only concepts. Must stay empty.
    SqlOnlyNames: TArray<string>;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Rtti,
  System.TypInfo,
  Daf.MiniSpec;

const
  /// Three interior nodes (compare / logical / unary) plus three leaves
  /// (member / value / param). A change to this number means a node class was
  /// added — which is exactly what the spec below exists to notice.
  BoundedNodeClassCount = 6;

// The concrete descendants of TExprNode the process knows about. The runner sets
// {$STRONGLINKTYPES ON}, so nothing is quietly dropped from RTTI.
function ConcreteNodeClasses: TArray<string>;
begin
  Result := nil;
  var Ctx := TRttiContext.Create;
  try
    for var T in Ctx.GetTypes do
      if T is TRttiInstanceType then
      begin
        var C := TRttiInstanceType(T).MetaclassType;
        if (C <> TExprNode) and C.InheritsFrom(TExprNode) then
          Result := Result + [C.ClassName];
      end;
  finally
    Ctx.Free;
  end;
end;

function EnumMemberNames(Info: PTypeInfo): TArray<string>;
begin
  Result := nil;
  var Ctx := TRttiContext.Create;
  try
    var T := Ctx.GetType(Info) as TRttiEnumerationType;
    for var I := T.MinValue to T.MaxValue do
      Result := Result + [GetEnumName(Info, I)];
  finally
    Ctx.Free;
  end;
end;

function IsSqlOnlyConcept(const OperatorName: string): Boolean;
begin
  // 'Raw' and 'Exists' are the two members of SQLCute's TWhereOp that must have
  // no counterpart here: raw SQL and a subquery predicate are not things a
  // domain expression should be able to say.
  Result := ContainsText(OperatorName, 'Raw') or ContainsText(OperatorName, 'Exists');
end;

initialization

Feature('''
Feature Expression node model @expressions @model

  As the author of a query layer that must stay small
  I want operators to be data and the node set to be bounded
  So breadth grows without the model growing, and no SQL concept leaks in
''')

.UseWorld<TModelWorld>

.Rule('Operators are enum values, never node classes')

  .Scenario('NewOperatorNeedsNoClass')
    .When('a comparison is built for every member of the comparison enum',
      procedure(W: TModelWorld)
      begin
        for var Op := Low(TCompareOp) to High(TCompareOp) do
        begin
          var N := TNode.Compare(Op, TNode.Member('Price'), [TNode.Value(100)]);
          W.ClassesUsed := W.ClassesUsed + [(N as TExprNode).ClassName];
          W.KindsSeen := W.KindsSeen + [N.Kind];
        end;
        W.NodeClasses := ConcreteNodeClasses;
      end)
    .&Then('every one of them is the one binary node type',
      procedure(W: TModelWorld)
      begin
        Expect(Length(W.ClassesUsed)).ToEqual(Ord(High(TCompareOp)) + 1);
        for var I := 0 to High(W.ClassesUsed) do
        begin
          Expect(W.ClassesUsed[I]).ToEqual(TCompareExpr.ClassName);
          Expect(W.KindsSeen[I] = TExprKind.Compare).ToBeTrue;
        end;
      end)
    .&Then('and the model still declares exactly six node classes',
      procedure(W: TModelWorld)
      begin
        // Not "at most": adding a class is as much a regression as removing one,
        // because it means an operator was modelled as a type again.
        Expect(Length(W.NodeClasses)).ToEqual(BoundedNodeClassCount);
      end)

.Rule('The model cannot express SQL-only concepts')

  .Scenario('ModelHasNoSqlVocabulary')
    .When('the three operator enums are read back through RTTI',
      procedure(W: TModelWorld)
      begin
        W.OperatorNames := EnumMemberNames(TypeInfo(TCompareOp))
                         + EnumMemberNames(TypeInfo(TLogicalOp))
                         + EnumMemberNames(TypeInfo(TUnaryOp));
        for var N in W.OperatorNames do
          if IsSqlOnlyConcept(N) then
            W.SqlOnlyNames := W.SqlOnlyNames + [N];
      end)
    .&Then('no member names a raw-SQL or EXISTS-style concept',
      procedure(W: TModelWorld)
      begin
        Expect(Length(W.OperatorNames)).ToBeGreaterThan(0);   // RTTI really answered
        Expect(string.Join(', ', W.SqlOnlyNames)).ToEqual('');
      end);

end.
