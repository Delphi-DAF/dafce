unit Specification.Feat;

{
  Specifications and the port. Everything here runs with nothing linked but
  Daf.Specification and MiniSpec — which is the claim, not a convenience:
  a domain service depending on IRepository<T> must not acquire a dependency on
  SQL, or the port fails its own purpose.

  The stub repository below is a test double, not the in-memory adapter (that one
  lives in Daf.SQLCute.Specification and is tested there). Its job is to show that
  implementing the port needs no base class on T and nothing from persistence.
}

interface

uses
  Daf.Specification,
  Authoring.Feat;   // TProduct — a plain class with public properties

type
  /// <summary>
  ///   A record entity. Not a class at all, so it could not inherit from a
  ///   framework base even if one existed.
  /// </summary>
  TProductRec = record
    Price: Currency;
    Active: Boolean;
  end;

  /// <summary>Implementing the port takes nothing but the port.</summary>
  TStubRepository<T> = class(TInterfacedObject, IRepository<T>)
  strict private
    FSeen: string;
  public
    function List(const Spec: TSpecification<T>): TArray<T>;
    /// The key of the last specification handed over — enough to show the rule
    /// crossed the boundary intact.
    property Seen: string read FSeen;
  end;

  TSpecificationWorld = class
  public
    Spec: TSpecification<TProduct>;
    RecordSpec: TSpecification<TProductRec>;
    RecordLeaf: string;
    Repo: IRepository<TProduct>;
    RecordRepo: IRepository<TProductRec>;
    ServiceResult: Integer;
    KeySeenByService: string;
    /// Two rules written the same way and a third with a different page, and their keys.
    SpecA: TSpecification<TProduct>;
    SpecB: TSpecification<TProduct>;
    SpecC: TSpecification<TProduct>;
    KeyA: string;
    KeyB: string;
    KeyC: string;
  end;

/// <summary>The Double whose IEEE 754 bits are <c>Bits</c>.</summary>
function DoubleFromBits(const Bits: Int64): Double;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.Variants,
  Daf.MiniSpec;

function DoubleFromBits(const Bits: Int64): Double;
begin
  PInt64(@Result)^ := Bits;
end;

{ TStubRepository<T> }

function TStubRepository<T>.List(const Spec: TSpecification<T>): TArray<T>;
begin
  FSeen := Spec.Key;
  Result := nil;
end;

/// A domain service: it knows the port and the rule, and nothing else. If this
/// compiled only because a SQL unit happened to be linked, the runner would say so.
function CountExpensive(const Repo: IRepository<TProduct>): Integer;
begin
  Result := Length(Repo.List(
    TSpecification<TProduct>.New.Where(Prop('Price') > 100)));
end;

initialization

Feature('''
Feature Specifications and the repository port @expressions @specification

  As a domain author composing hexagonally
  I want a query rule I can assert without a database and a port with no SQL in it
  So the domain half is testable and independent by construction
''')

.UseWorld<TSpecificationWorld>

.Rule('A specification is a domain object, assertable on its own')

  .Scenario('SpecificationIsAssertableWithoutDatabase')
    .When('a rule is declared with a filter, an ordering and a page',
      procedure(W: TSpecificationWorld)
      begin
        W.Spec := TSpecification<TProduct>.New
          .Where((Prop('Price') > 100) and (Prop('Active') = True))
          .OrderByDesc('Price')
          .Skip(10)
          .Take(5);
      end)
    .&Then('its tree, ordering and page read back exactly as written',
      procedure(W: TSpecificationWorld)
      begin
        var Root := W.Spec.Filter as ILogicalExpr;
        Expect(Root.Op = TLogicalOp.opAnd).ToBeTrue;

        var Left := Root.Left as ICompareExpr;
        Expect(Left.Op = TCompareOp.Greater).ToBeTrue;
        Expect((Left.Target as IMemberExpr).Name).ToEqual('Price');
        Expect((Left.Operands[0] as IValueExpr).Value).ToEqual(100);

        var Right := Root.Right as ICompareExpr;
        Expect(Right.Op = TCompareOp.Equal).ToBeTrue;
        Expect((Right.Target as IMemberExpr).Name).ToEqual('Active');

        Expect(Length(W.Spec.Order)).ToEqual(1);
        Expect(W.Spec.Order[0].Member).ToEqual('Price');
        Expect(W.Spec.Order[0].Descending).ToBeTrue;
        Expect(W.Spec.SkipCount).ToEqual(10);
        Expect(W.Spec.TakeCount).ToEqual(5);
      end)

  .Scenario('The whole rule has one canonical key')
    .Given('two rules written the same way, and a third that differs only in its page',
      procedure(W: TSpecificationWorld)
      begin
        W.SpecA := TSpecification<TProduct>.New.Where(Prop('Price') > 100).Take(5);
        W.SpecB := TSpecification<TProduct>.New.Where(Prop('Price') > 100).Take(5);
        W.SpecC := TSpecification<TProduct>.New.Where(Prop('Price') > 100).Take(6);
      end)
    .When('each one is asked for its key',
      procedure(W: TSpecificationWorld)
      begin
        W.KeyA := W.SpecA.Key;
        W.KeyB := W.SpecB.Key;
        W.KeyC := W.SpecC.Key;
      end)
    .&Then('two rules written the same way agree, and a different page does not',
      procedure(W: TSpecificationWorld)
      begin
        Expect(W.KeyA).ToEqual(W.KeyB);
        Expect(W.KeyA).ToNotEqual(W.KeyC);
      end)

.Rule('The key tells apart values that differ')

  .Scenario('A millisecond changes the key')
    .Given('two specifications filtering CreatedAt by two dates a millisecond apart',
      procedure(W: TSpecificationWorld)
      begin
        W.SpecA := TSpecification<TProduct>.New.Where(
          Prop('CreatedAt') > VarFromDateTime(EncodeDateTime(2026, 1, 1, 10, 0, 0, 0)));
        W.SpecB := TSpecification<TProduct>.New.Where(
          Prop('CreatedAt') > VarFromDateTime(EncodeDateTime(2026, 1, 1, 10, 0, 0, 1)));
      end)
    .When('each one is asked for its key',
      procedure(W: TSpecificationWorld)
      begin
        W.KeyA := W.SpecA.Key;
        W.KeyB := W.SpecB.Key;
      end)
    .&Then('the two keys differ',
      procedure(W: TSpecificationWorld)
      begin
        Expect(W.KeyA).ToNotEqual(W.KeyB);
      end)

  .Scenario('The last digits of a Double change the key')
    .Given('two specifications filtering Price < 0.30000000000000004 and Price < 0.3, two distinct Doubles',
      procedure(W: TSpecificationWorld)
      begin
        W.SpecA := TSpecification<TProduct>.New.Where(Prop('Price') < DoubleFromBits($3FD3333333333334));
        W.SpecB := TSpecification<TProduct>.New.Where(Prop('Price') < DoubleFromBits($3FD3333333333333));
      end)
    .When('each one is asked for its key',
      procedure(W: TSpecificationWorld)
      begin
        W.KeyA := W.SpecA.Key;
        W.KeyB := W.SpecB.Key;
      end)
    .&Then('the two keys differ',
      procedure(W: TSpecificationWorld)
      begin
        Expect(W.KeyA).ToNotEqual(W.KeyB);
      end)

.Rule('The port imposes no base class, and no SQL')

  .Scenario('PlainClassIsAValidEntity')
    .When('the port and a rule are instantiated for a plain class and for a record',
      procedure(W: TSpecificationWorld)
      begin
        W.Repo := TStubRepository<TProduct>.Create;
        W.RecordRepo := TStubRepository<TProductRec>.Create;

        W.RecordSpec := TSpecification<TProductRec>.New
          .Where(TEntityExpr<TProductRec>.Create(nil).Prop('Price') > 1);
        W.RecordLeaf := ExprToText(W.RecordSpec.Filter);

        W.RecordRepo.List(W.RecordSpec);
      end)
    .&Then('both work with no framework base class anywhere',
      procedure(W: TSpecificationWorld)
      begin
        Expect(Assigned(W.Repo)).ToBeTrue;
        Expect(Assigned(W.RecordRepo)).ToBeTrue;
        // A record's field resolved as a member: the entity needs neither a base
        // class nor even to be a class.
        Expect(W.RecordLeaf).ToContain('m:Price');
      end)

  .Scenario('A domain service depends only on the port')
    .When('a service is handed a repository and asks for its rule',
      procedure(W: TSpecificationWorld)
      begin
        var Stub := TStubRepository<TProduct>.Create;
        W.Repo := Stub;
        W.ServiceResult := CountExpensive(W.Repo);
        W.KeySeenByService := Stub.Seen;
      end)
    .&Then('the rule crossed the boundary intact, with no database in the picture',
      procedure(W: TSpecificationWorld)
      begin
        Expect(W.ServiceResult).ToEqual(0);
        Expect(W.KeySeenByService).ToContain('m:Price');
        Expect(W.KeySeenByService).ToContain('Greater');
      end);

end.
