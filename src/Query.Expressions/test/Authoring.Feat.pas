unit Authoring.Feat;

{
  The authoring surface: what an operator expression is once written, and where
  a value with no Variant form gets narrowed. Both assertions read the tree back
  rather than a rendering of it, because the tree is what the emitter will see.
}

interface

uses
  Daf.Query.Expressions;

type
  TAuthoringWorld = class
  public
    /// The expression under test, as authored.
    Authored: IExpr;
    /// The GUID a scenario compares against, and what the leaf ended up holding.
    Id: TGUID;
    LeafValue: Variant;
    LeafVarType: Word;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec;

initialization

Feature('''
Feature Expression authoring @expressions @authoring

  As a developer writing a query condition
  I want operators to build the tree for me
  So the condition is ordinary Delphi code and still inspectable
''')

.UseWorld<TAuthoringWorld>

.Rule('Overloaded operators build the tree')

  .Scenario('OperatorBuildsBinaryNode')
    .When('Prop(''Price'') > 100 is written',
      procedure(W: TAuthoringWorld)
      begin
        W.Authored := Prop('Price') > 100;
      end)
    .&Then('it is a comparison node carrying greater-than, member Price and value 100',
      procedure(W: TAuthoringWorld)
      begin
        var C := W.Authored as ICompareExpr;
        Expect(C.Op = TCompareOp.Greater).ToBeTrue;
        Expect((C.Target as IMemberExpr).Name).ToEqual('Price');
        Expect(Length(C.Operands)).ToEqual(1);
        Expect((C.Operands[0] as IValueExpr).Value).ToEqual(100);
      end)

.Rule('A value with no Variant form is narrowed where it is written')

  .Scenario('GuidNarrowsAtAuthoring')
    .When('a TGUID is compared through the typed operator overload',
      procedure(W: TAuthoringWorld)
      begin
        W.Id := StringToGUID('{2B7F0C1A-5D34-4E86-9A02-6C1D8B45F730}');
        W.Authored := Prop('Id') = W.Id;
        var C := W.Authored as ICompareExpr;
        W.LeafValue := (C.Operands[0] as IValueExpr).Value;
        W.LeafVarType := VarType(W.LeafValue);
      end)
    .&Then('the leaf holds the canonical string form, so both back-ends see one value',
      procedure(W: TAuthoringWorld)
      begin
        Expect(string(W.LeafValue)).ToEqual(GUIDToString(W.Id));
        // A string, not a record smuggled through a Variant: the in-memory lane
        // would otherwise compare a TGUID while SQL compares its text.
        Expect((W.LeafVarType = varUString) or (W.LeafVarType = varOleStr)
            or (W.LeafVarType = varString)).ToBeTrue;
      end);

end.
