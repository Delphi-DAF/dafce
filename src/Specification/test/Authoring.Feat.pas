unit Authoring.Feat;

{
  The authoring surface: what an operator expression is once written, and where
  a value with no Variant form gets narrowed. Both assertions read the tree back
  rather than a rendering of it, because the tree is what the emitter will see.
}

interface

uses
  System.TypInfo,
  Daf.Specification;

type
  /// A plain class: public properties, a parameterless constructor, no base class.
  TProduct = class
  private
    FPrice: Currency;
    FActive: Boolean;
    FName: string;
  public
    property Price: Currency read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
    property Name: string read FName write FName;
  end;

  /// A type whose name does not follow the T-prefix convention, so the default
  /// table rule has something to leave alone.
  Thing = class
  private
    FTag: string;
  public
    property Tag: string read FTag write FTag;
  end;

  /// <summary>
  ///   What an ORM's strategy would look like from here: richer names for both
  ///   questions, plugged into the one seam.
  /// </summary>
  TMappedResolver = class(TInterfacedObject, INameResolver)
  public
    function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
    function TableFor(EntityType: PTypeInfo): string;
  end;

  TAuthoringWorld = class
  public
    /// The expression under test, as authored.
    Authored: IExpr;
    /// The GUID a scenario compares against, and what the leaf ended up holding.
    Id: TGUID;
    LeafValue: Variant;
    LeafVarType: Word;
    /// Resolution results.
    MappedColumn: string;
    DefaultColumn: string;
    DefaultTable: string;
    UnprefixedTable: string;
    MappedTable: string;
    /// What authoring an unresolvable leaf answered: the exception class with its message, or
    /// the mark that none was raised.
    Outcome: string;
    /// The shared default resolver, read twice, and the answers it and a no-strategy proxy give.
    Shared: INameResolver;
    SecondRead: INameResolver;
    ProxyTable: string;
    SharedTable: string;
    SharedColumn: string;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec;

{ TMappedResolver }

function TMappedResolver.ColumnFor(EntityType: PTypeInfo; const Member: string): string;
begin
  if SameText(Member, 'Price') then
    Result := 'price_eur'
  else
    Result := Member;
end;

function TMappedResolver.TableFor(EntityType: PTypeInfo): string;
begin
  Result := 'products';
end;

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
      end)

.Rule('One strategy answers both naming questions')

  .Scenario('DefaultStrategyIsIdentity and CustomStrategyMapsColumn')
    .When('a member is resolved with no strategy, then with one that renames it',
      procedure(W: TAuthoringWorld)
      begin
        var Plain := TEntityExpr<TProduct>.Create(nil);
        W.DefaultColumn := Plain.Column('Price');

        var Mapped := TEntityExpr<TProduct>.Create(TMappedResolver.Create);
        W.MappedColumn := Mapped.Column('Price');
      end)
    .&Then('the default is the property name unchanged, and the strategy overrides it',
      procedure(W: TAuthoringWorld)
      begin
        Expect(W.DefaultColumn).ToEqual('Price');
        Expect(W.MappedColumn).ToEqual('price_eur');
      end)

  .Scenario('TypeResolvesToTableByStrippingT and SuppliedStrategyOverridesTable')
    .When('the table is resolved for TProduct, for Thing, and with a strategy',
      procedure(W: TAuthoringWorld)
      begin
        W.DefaultTable := TEntityExpr<TProduct>.Create(nil).Table;
        W.UnprefixedTable := TEntityExpr<Thing>.Create(nil).Table;
        W.MappedTable := TEntityExpr<TProduct>.Create(TMappedResolver.Create).Table;
      end)
    .&Then('the leading T goes, nothing else is touched, and a strategy wins',
      procedure(W: TAuthoringWorld)
      begin
        // Verbatim from there on: no pluralization, no case conversion.
        Expect(W.DefaultTable).ToEqual('Product');
        Expect(W.UnprefixedTable).ToEqual('Thing');
        Expect(W.MappedTable).ToEqual('products');
      end)

.Rule('A member that resolves to nothing fails where it is written')

  .Scenario('UnresolvableMemberRaisesAtAuthoring')
    // Nothing to build: the member name that does not exist is the argument, and the type it
    // does not exist on is compiled in.
    .Given('a member name TProduct does not have').NoAction
    .When('a leaf is authored with it',
      procedure(W: TAuthoringWorld)
      begin
        try
          TEntityExpr<TProduct>.Create(nil).Prop('Pricee');
          W.Outcome := '<the leaf was authored>';
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('authoring the leaf raises, naming both the member and the type',
      procedure(W: TAuthoringWorld)
      begin
        Expect(W.Outcome).ToStartWith('EExprMemberError');
        Expect(W.Outcome).ToContain('Pricee');
        Expect(W.Outcome).ToContain('TProduct');
      end)

// The default strategy carries no state, so duplicating it would be waste — but
// "shared" only stays true if nothing hands out a fresh one. That is the property
// a refactor of the accessor could break silently, since a per-call instance
// resolves names identically and every other spec would still pass.
.Rule('The default resolver is owned by its own type')

  .Scenario('DefaultResolverIsOneSharedInstance')
    .Given('the default resolver, read once',
      procedure(W: TAuthoringWorld)
      begin
        W.Shared := TDefaultNameResolver.Instance;
      end)
    .When('it is read again, and a proxy built with no strategy is asked the same questions',
      procedure(W: TAuthoringWorld)
      begin
        W.SecondRead := TDefaultNameResolver.Instance;
        W.ProxyTable := TEntityExpr<TProduct>.Create(nil).Table;
        W.SharedTable := W.Shared.TableFor(TypeInfo(TProduct));
        W.SharedColumn := W.Shared.ColumnFor(TypeInfo(TProduct), 'Price');
      end)
    .&Then('reading Instance twice yields the same object',
      procedure(W: TAuthoringWorld)
      begin
        Expect(W.SecondRead = W.Shared).ToBeTrue;
      end)
    .&Then('and a proxy built with no strategy resolves through that same instance',
      procedure(W: TAuthoringWorld)
      begin
        // Same answers as the shared instance, on both questions it owns.
        Expect(W.ProxyTable).ToEqual(W.SharedTable);
        Expect(W.SharedColumn).ToEqual('Price');
      end);

end.
