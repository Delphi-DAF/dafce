unit Metamodel.Feat;

{
  The metamodel is optional, and these specs are what keep it optional. If a
  metamodel leaf ever became distinguishable from a runtime leaf — a different
  node, an extra field, a capability only it reaches — the option would quietly
  become a tax, and adopting or dropping one later would stop being a local edit.
}

interface

uses
  Daf.Specification,
  Authoring.Feat;   // TProduct — one entity is enough for the whole suite

type
  /// <summary>
  ///   The intended shape: <c>class var</c> plus a <c>class constructor</c>, so
  ///   the call site is a field read and initialization runs once per class.
  ///   That its names are valid is why the runner starts at all — the class
  ///   constructor validates them during unit initialization.
  /// </summary>
  ProductMeta = class
  public
    class var Price: TPropExpr<TProduct>;
    class var Active: TPropExpr<TProduct>;
    class constructor Create;
  end;

  /// <summary>
  ///   A metamodel whose member no longer resolves. Its initializer is a class
  ///   <i>procedure</i> for one reason only: a class constructor runs during
  ///   unit initialization, so a raising one would take the runner down before a
  ///   single scenario ran. The failure it demonstrates is the same one, at the
  ///   same point — when the metamodel initializes, not when a query runs.
  /// </summary>
  StaleProductMeta = class
  public
    class var Price: TPropExpr<TProduct>;
    class procedure Initialize;
  end;

  TMetamodelWorld = class
  public
    ViaRuntime: string;
    ViaMetamodel: string;
    RuntimeAccessorIsNil: Boolean;
    MetamodelAccessorIsNil: Boolean;
    Mixed: IExpr;
    /// What initializing the stale metamodel answered, and the names the valid one holds.
    Outcome: string;
    PriceName: string;
    ActiveName: string;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ ProductMeta }

class constructor ProductMeta.Create;
begin
  Price := TPropExpr<TProduct>.Create('Price');     // the PROPERTY name
  Active := TPropExpr<TProduct>.Create('Active');
end;

{ StaleProductMeta }

class procedure StaleProductMeta.Initialize;
begin
  Price := TPropExpr<TProduct>.Create('Pricee');
end;

initialization

Feature('''
Feature Optional typed metamodel @expressions @metamodel

  As a developer who wants completion and typo-safety on a hot entity
  I want to hand-write a metamodel for it
  So authoring is checked at compile time without any of it becoming mandatory
''')

.UseWorld<TMetamodelWorld>

.Rule('Both authoring paths produce the same node')

  .Scenario('BothPathsYieldEquivalentNodes')
    .When('the same condition is authored through the proxy and through the metamodel',
      procedure(W: TMetamodelWorld)
      begin
        var Runtime := TEntityExpr<TProduct>.Create(nil);
        var A: IExpr := Runtime.Prop('Price') > 100;
        var B: IExpr := ProductMeta.Price > 100;

        W.ViaRuntime := ExprToText(A);
        W.ViaMetamodel := ExprToText(B);

        // The reserved accessor field is nil on both, so it cannot become the
        // thing that tells them apart either.
        W.RuntimeAccessorIsNil :=
          ((A as ICompareExpr).Target as IMemberExpr).Accessor = nil;
        W.MetamodelAccessorIsNil :=
          ((B as ICompareExpr).Target as IMemberExpr).Accessor = nil;
      end)
    .&Then('the trees are identical, so nothing downstream can distinguish them',
      procedure(W: TMetamodelWorld)
      begin
        Expect(W.ViaMetamodel).ToEqual(W.ViaRuntime);
        // Not vacuously equal: the rendering really carries the condition.
        Expect(W.ViaRuntime).ToContain('m:Price');
        Expect(W.ViaRuntime).ToContain('Greater');
        Expect(W.RuntimeAccessorIsNil).ToBeTrue;
        Expect(W.MetamodelAccessorIsNil).ToBeTrue;
      end)

  .Scenario('TwoFormsMixInOneExpression')
    .When('one expression combines a metamodel leaf and a runtime leaf',
      procedure(W: TMetamodelWorld)
      begin
        var Runtime := TEntityExpr<TProduct>.Create(nil);
        W.Mixed := (ProductMeta.Price > 100) and (Runtime.Prop('Active') = True);
      end)
    .&Then('it composes normally, and reads back as one ordinary conjunction',
      procedure(W: TMetamodelWorld)
      begin
        var L := W.Mixed as ILogicalExpr;
        Expect(L.Op = TLogicalOp.opAnd).ToBeTrue;

        var FromMeta := L.Left as ICompareExpr;
        Expect(FromMeta.Op = TCompareOp.Greater).ToBeTrue;
        Expect((FromMeta.Target as IMemberExpr).Name).ToEqual('Price');

        var FromProxy := L.Right as ICompareExpr;
        Expect(FromProxy.Op = TCompareOp.Equal).ToBeTrue;
        Expect((FromProxy.Target as IMemberExpr).Name).ToEqual('Active');

        // Same node class on both sides — the origin left no trace.
        Expect((FromMeta.Target as TExprNode).ClassName)
          .ToEqual((FromProxy.Target as TExprNode).ClassName);
      end)

.Rule('A metamodel validates itself')

  .Scenario('StaleMetamodelMemberRaisesOnInitialize')
    // Nothing to build: the stale metamodel is a declaration, and its initializer is a class
    // procedure precisely so that nothing runs it before this scenario does.
    .Given('a metamodel declaring a member the type no longer has').NoAction
    .When('it is initialized',
      procedure(W: TMetamodelWorld)
      begin
        try
          StaleProductMeta.Initialize;
          W.Outcome := '<it initialized without complaining>';
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('initializing a metamodel whose member no longer resolves raises there',
      procedure(W: TMetamodelWorld)
      begin
        Expect(W.Outcome).ToStartWith('EExprMemberError');
      end)

  .Scenario('A valid metamodel initialized fine, which is why this suite runs')
    // Nothing to build: its class constructor ran during unit initialization, which is the
    // event this scenario looks back at -- the suite starting at all is half the assertion.
    .Given('a metamodel whose class constructor already ran').NoAction
    .When('its members are asked for the names they carry',
      procedure(W: TMetamodelWorld)
      begin
        W.PriceName := (ProductMeta.Price.Node as IMemberExpr).Name;
        W.ActiveName := (ProductMeta.Active.Node as IMemberExpr).Name;
      end)
    .&Then('its members hold the property names RTTI spells',
      procedure(W: TMetamodelWorld)
      begin
        Expect(W.PriceName).ToEqual('Price');
        Expect(W.ActiveName).ToEqual('Active');
      end);

end.
