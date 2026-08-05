unit Authoring.Feat;

{
  The authoring surface: what an operator expression is once written, and where
  a value with no Variant form gets narrowed. Both assertions read the tree back
  rather than a rendering of it, because the tree is what the emitter will see.
}

interface

uses
  System.TypInfo,
  Daf.Query.Expressions;

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
  TMappedNames = class(TInterfacedObject, INameResolution)
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
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec;

{ TMappedNames }

function TMappedNames.ColumnFor(EntityType: PTypeInfo; const Member: string): string;
begin
  if SameText(Member, 'Price') then
    Result := 'price_eur'
  else
    Result := Member;
end;

function TMappedNames.TableFor(EntityType: PTypeInfo): string;
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

        var Mapped := TEntityExpr<TProduct>.Create(TMappedNames.Create);
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
        W.MappedTable := TEntityExpr<TProduct>.Create(TMappedNames.Create).Table;
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
    .&Then('authoring the leaf raises, naming both the member and the type',
      procedure(W: TAuthoringWorld)
      begin
        ExpectException(
          procedure
          begin
            TEntityExpr<TProduct>.Create(nil).Prop('Pricee');
          end).ToBe(EExprMemberError);
        ExpectException(
          procedure
          begin
            TEntityExpr<TProduct>.Create(nil).Prop('Pricee');
          end).ToHaveMessage('Pricee');
        ExpectException(
          procedure
          begin
            TEntityExpr<TProduct>.Create(nil).Prop('Pricee');
          end).ToHaveMessage('TProduct');
      end);

end.
