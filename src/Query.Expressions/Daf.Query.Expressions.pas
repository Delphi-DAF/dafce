unit Daf.Query.Expressions;

{
  Daf.Query.Expressions — the domain-side half of the expression layer: a
  data-driven expression tree, the operator-overloaded authoring surface, the
  name-resolution seam, TSpecification<T> and IRepository<T>.

  It depends on Commons and nothing else. In particular it does NOT reference
  SQLCute, so a domain unit can author expressions, declare specifications and
  depend on the repository port without acquiring a SQL dependency. The
  operator enums below are this model's own for the same reason: SQLCute's
  TWhereOp carries Raw / Exists / NotExists, concepts a domain expression model
  should not be able to express. Mapping onto SQLCute's vocabulary happens in
  the emitter (Daf.SQLCute.Expressions), which is a table, not a traversal.
}

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  Daf.Nullable,
  Daf.Rtti;

type
  /// <summary>
  ///   How two operands are compared. The model's own vocabulary, deliberately
  ///   NOT SQLCute's <c>TWhereOp</c>: that one carries <c>Raw</c>,
  ///   <c>Exists</c> and <c>NotExists</c>, so reusing it would import SQL
  ///   concepts a domain expression must not be able to express — and would
  ///   drag <c>Daf.SQLCute.Clauses</c> into this unit besides.
  /// </summary>
  /// <remarks>
  ///   Adding a comparison is a new value here and a new row in the emitter's
  ///   lookup table. It is never a new node class: the same binary node carries
  ///   every member of this enum.
  /// </remarks>
  TCompareOp = (
    Equal, NotEqual, Less, LessOrEqual, Greater, GreaterOrEqual,
    Like, NotLike, StartsWith, EndsWith, Contains,
    &In, NotIn,
    &Between, NotBetween,
    IsNull, IsNotNull);

  /// <summary>How two predicates are joined. Named after SQLCute's <c>TBoolOp</c>.</summary>
  TLogicalOp = (opAnd, opOr);

  /// <summary>
  ///   The one-operand predicate operators. <c>opNot</c> is emitted as the
  ///   operand's own negation or as a negated group — never rewritten through
  ///   De Morgan.
  /// </summary>
  TUnaryOp = (opNot);

  // ---------------------------------------------------------------------------
  //  The node set
  // ---------------------------------------------------------------------------

  /// <summary>
  ///   The complete set of node kinds. It is bounded on purpose: three interior
  ///   nodes parameterized by the enums above, three leaves. New operators
  ///   arrive as enum values, so this list does not grow when the expressible
  ///   vocabulary does.
  /// </summary>
  TExprKind = (Compare, Logical, Unary, Member, Value, Param);

  /// <summary>
  ///   A node of the expression tree. Lifetime is interface ref-counting and
  ///   nothing else: no <c>Acquire</c>/<c>Release</c>, no manual <c>Free</c>,
  ///   one idiom throughout — the same one <c>IQuery</c> and
  ///   <c>Daf.Enumerable</c> already use.
  /// </summary>
  IExpr = interface
    ['{6C2D0A57-9F41-4B8E-9C3D-1E5A7B04D2F1}']
    /// <summary>Which kind of node this is — the discriminator the emitter switches on.</summary>
    function Kind: TExprKind;
  end;

  /// <summary>
  ///   A comparison: one target, zero or more operands, one operator.
  ///   <c>IsNull</c> takes no operand, <c>Between</c> takes two, <c>In</c> takes
  ///   as many as the set holds — the arity travels with the operator, so the
  ///   node stays one type.
  /// </summary>
  ICompareExpr = interface(IExpr)
    ['{0B9E3C14-5A76-4D28-8F0B-2C6D9E71A483}']
    function Op: TCompareOp;
    /// <summary>The left-hand side — normally a member leaf.</summary>
    function Target: IExpr;
    /// <summary>The right-hand side(s), in order. Empty for <c>IsNull</c>/<c>IsNotNull</c>.</summary>
    function Operands: TArray<IExpr>;
    /// <summary>Whether a text match is case-sensitive. Only the LIKE family reads it.</summary>
    function CaseSensitive: Boolean;
  end;

  /// <summary>A conjunction or disjunction of two predicates.</summary>
  ILogicalExpr = interface(IExpr)
    ['{9D4A6E82-3B15-4C70-A69F-7E0B248CD35A}']
    function Op: TLogicalOp;
    function Left: IExpr;
    function Right: IExpr;
  end;

  /// <summary>A one-operand predicate — today, negation.</summary>
  IUnaryExpr = interface(IExpr)
    ['{4F17B0D6-8C29-4A53-B1E8-06D3A95F7C24}']
    function Op: TUnaryOp;
    function Operand: IExpr;
  end;

  /// <summary>
  ///   A reference to a member of the entity. It carries the <b>property</b>
  ///   name, never a column name: a column is persistence vocabulary and would
  ///   make this unit persistence-aware, would declare the mapping a second
  ///   time with no compiler link to the first, and is not what the in-memory
  ///   lane needs anyway. The emitter resolves the column once, at emit time.
  /// </summary>
  IMemberExpr = interface(IExpr)
    ['{A3E5C271-6D48-4B9A-830F-5C1E7B62D04F}']
    function Name: string;
    /// <summary>
    ///   A pre-resolved accessor for the member, or <c>nil</c> — which is what
    ///   it always is today. The room is reserved from the first commit because
    ///   the real payoff of generated metamodels is not the names but attaching
    ///   an accessor here, letting the in-memory lane skip the name
    ///   normalization, dictionary lookup and boxing it does per row and per
    ///   column. Reserving a field now means that arrives without reshaping the
    ///   node; it is a field, not an architecture.
    /// </summary>
    function Accessor: TRttiMember;
  end;

  /// <summary>
  ///   A literal. The payload is a single <c>Variant</c> — the representation
  ///   SQLCute clauses already hold, so nothing is reimplemented and nothing is
  ///   lost: the DAL's 42 typed accessors had the very same ceiling. Typing is
  ///   the binding layer's business; NULL is Variant's <c>Null</c>.
  /// </summary>
  IValueExpr = interface(IExpr)
    ['{7B0D4A93-2E61-4F58-9C27-8A5B3D06E1C4}']
    function Value: Variant;
  end;

  /// <summary>A named placeholder carrying its bound value. A value leaf with a name.</summary>
  IParamExpr = interface(IValueExpr)
    ['{2C8F5B41-7A03-4D96-B25E-9F1C6A047D3B}']
    function Name: string;
  end;

  // ---------------------------------------------------------------------------
  //  Implementations
  // ---------------------------------------------------------------------------

  TExprNode = class abstract(TInterfacedObject, IExpr)
  strict private
    FKind: TExprKind;
  public
    constructor Create(AKind: TExprKind);
    function Kind: TExprKind;
  end;

  TCompareExpr = class(TExprNode, ICompareExpr)
  strict private
    FOp: TCompareOp;
    FTarget: IExpr;
    FOperands: TArray<IExpr>;
    FCaseSensitive: Boolean;
  public
    constructor Create(AOp: TCompareOp; const ATarget: IExpr;
      const AOperands: TArray<IExpr>; ACaseSensitive: Boolean = False);
    function Op: TCompareOp;
    function Target: IExpr;
    function Operands: TArray<IExpr>;
    function CaseSensitive: Boolean;
  end;

  TLogicalExpr = class(TExprNode, ILogicalExpr)
  strict private
    FOp: TLogicalOp;
    FLeft: IExpr;
    FRight: IExpr;
  public
    constructor Create(AOp: TLogicalOp; const ALeft, ARight: IExpr);
    function Op: TLogicalOp;
    function Left: IExpr;
    function Right: IExpr;
  end;

  TUnaryExpr = class(TExprNode, IUnaryExpr)
  strict private
    FOp: TUnaryOp;
    FOperand: IExpr;
  public
    constructor Create(AOp: TUnaryOp; const AOperand: IExpr);
    function Op: TUnaryOp;
    function Operand: IExpr;
  end;

  TMemberExpr = class(TExprNode, IMemberExpr)
  strict private
    FName: string;
    FAccessor: TRttiMember;
  public
    constructor Create(const AName: string; const AAccessor: TRttiMember = nil);
    function Name: string;
    function Accessor: TRttiMember;
  end;

  TValueExpr = class(TExprNode, IValueExpr)
  strict private
    FValue: Variant;
  public
    constructor Create(const AValue: Variant);
    function Value: Variant;
  end;

  TParamExpr = class(TExprNode, IParamExpr)
  strict private
    FName: string;
    FValue: Variant;
  public
    constructor Create(const AName: string; const AValue: Variant);
    function Name: string;
    function Value: Variant;
  end;

  // ---------------------------------------------------------------------------
  //  Node construction
  // ---------------------------------------------------------------------------

  /// <summary>
  ///   The one place nodes are built. Both authoring paths — the runtime proxy
  ///   and a hand-written metamodel — come through here, which is what makes
  ///   their output indistinguishable rather than merely similar.
  /// </summary>
  TNode = record
    class function Compare(Op: TCompareOp; const Target: IExpr;
      const Operands: TArray<IExpr>; CaseSensitive: Boolean = False): IExpr; static;
    class function Logical(Op: TLogicalOp; const Left, Right: IExpr): IExpr; static;
    class function Unary(Op: TUnaryOp; const Operand: IExpr): IExpr; static;
    class function Member(const Name: string; const Accessor: TRttiMember = nil): IExpr; static;
    class function Value(const V: Variant): IExpr; static;
    class function Param(const Name: string; const V: Variant): IExpr; static;
  end;

  // ---------------------------------------------------------------------------
  //  Narrowing at the authoring edge
  // ---------------------------------------------------------------------------

  /// <summary>
  ///   Converts a value with no direct <c>Variant</c> form into the one the
  ///   leaves carry. The caller invokes it, visibly, where the expression is
  ///   written — the lowering never narrows behind anyone's back, because a leaf
  ///   holding a richer value would leave the in-memory lane comparing one thing
  ///   while SQL compares another, in the one place this design promises parity.
  /// </summary>
  TExprValue = record
    /// <summary>
    ///   The nullable's value, or Variant <c>Null</c> when it is absent. NULL has
    ///   exactly one representation here — <c>TNullable</c> on the way in,
    ///   Variant's <c>Null</c> from the leaf onward — and no node carries a null
    ///   flag of its own.
    /// </summary>
    class function From<T>(const V: TNullable<T>): Variant; overload; static;
    /// <summary>
    ///   A GUID as its canonical string form — the same thing the DAL's
    ///   dedicated <c>SetAsGUID</c> accessor did, minus the boilerplate around
    ///   it. Narrowing here rather than in the lowering is what keeps the two
    ///   lanes honest: a leaf holding a real <c>TGUID</c> would leave the
    ///   in-memory side comparing a record while SQL compares a string.
    /// </summary>
    class function From(const V: TGUID): Variant; overload; static;
  end;

  // ---------------------------------------------------------------------------
  //  Authoring
  // ---------------------------------------------------------------------------

  /// <summary>
  ///   The authoring surface: one node, wrapped in the record Delphi lets us
  ///   overload operators on, so a predicate is written as Delphi code and read
  ///   back as a tree.
  /// </summary>
  /// <remarks>
  ///   What is borrowed from Commons' <c>Daf.Expression</c> is the
  ///   operator-overloading <i>shape</i>, never the type. <c>TFilterSpec&lt;T&gt;</c>
  ///   composes closures, and a closure is opaque to SQL by construction — you
  ///   cannot translate it, only run it. This layer needs a tree it can inspect,
  ///   so the two are siblings serving different worlds rather than rivals:
  ///   there is no <c>TFilterSpec</c>, and no closure of any kind, anywhere in
  ///   this unit.
  /// </remarks>
  TExpr = record
  strict private
    FNode: IExpr;
  public
    class operator Implicit(const Node: IExpr): TExpr;
    class operator Implicit(const E: TExpr): IExpr;

    class operator Equal(const L: TExpr; const R: Variant): TExpr; overload;
    class operator NotEqual(const L: TExpr; const R: Variant): TExpr; overload;
    /// <summary>
    ///   Comparison against a value with no direct <c>Variant</c> form. The
    ///   narrowing is visible in the call it belongs to, not hidden in the
    ///   lowering — one typed overload per such type, added as the types show up.
    /// </summary>
    class operator Equal(const L: TExpr; const R: TGUID): TExpr; overload;
    class operator NotEqual(const L: TExpr; const R: TGUID): TExpr; overload;
    class operator LessThan(const L: TExpr; const R: Variant): TExpr;
    class operator LessThanOrEqual(const L: TExpr; const R: Variant): TExpr;
    class operator GreaterThan(const L: TExpr; const R: Variant): TExpr;
    class operator GreaterThanOrEqual(const L: TExpr; const R: Variant): TExpr;

    class operator LogicalAnd(const L, R: TExpr): TExpr;
    class operator LogicalOr(const L, R: TExpr): TExpr;
    class operator LogicalNot(const E: TExpr): TExpr;

    /// <summary>The node this record wraps.</summary>
    function Node: IExpr;
  end;

/// <summary>
///   A reference to a member by name — the always-available authoring path. The
///   name is a property name; nothing here knows about columns.
/// </summary>
function Prop(const Name: string): TExpr;

/// <summary>A named placeholder carrying its bound value.</summary>
function Prm(const Name: string; const Value: Variant): TExpr;

type
  // ---------------------------------------------------------------------------
  //  Name resolution — one seam, two questions
  // ---------------------------------------------------------------------------

  /// <summary>
  ///   Answers the layer's only two naming questions: which column backs this
  ///   member, and which table backs this type. They are the same kind of
  ///   question, so they belong to one strategy — the ORM then plugs into one
  ///   seam rather than two.
  /// </summary>
  /// <remarks>
  ///   What is deliberately absent is everything richer: keys, lock columns,
  ///   attributes, inheritance mapping. That is ORM metadata, and the ORM is not
  ///   designed yet; baking a mapping model in here would prejudge that design
  ///   from the wrong layer. This layer commits to the seam, not the mapping —
  ///   whatever the ORM turns out to be, it supplies its own implementation and
  ///   nothing here changes.
  /// </remarks>
  INameResolution = interface
    ['{E8A1C704-3D62-4F95-B08A-71C3D65E24BF}']
    /// <summary>The column backing <c>Member</c> of <c>EntityType</c>.</summary>
    function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
    /// <summary>The table backing <c>EntityType</c>.</summary>
    function TableFor(EntityType: PTypeInfo): string;
  end;

  /// <summary>
  ///   The defaults, both deliberately minimal: a member's column is its own
  ///   name, and a type's table is the type name with a leading <c>T</c>
  ///   removed. No pluralization and no case conversion — those are naming
  ///   <i>policy</i>, and guessing them would collide with whatever the ORM
  ///   decides while looking like a feature.
  /// </summary>
  /// <remarks>
  ///   A documented convention applied uniformly is a default, not the hazard a
  ///   pass-through is: it is visible, replaceable, and wrong in the same way
  ///   for everybody rather than on one back-end only.
  /// </remarks>
  TDefaultNameResolution = class(TInterfacedObject, INameResolution)
  public
    function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
    function TableFor(EntityType: PTypeInfo): string;
  end;

/// <summary>
///   The shared default strategy. Stateless, so one instance serves every
///   caller that supplies none of its own.
/// </summary>
function DefaultNameResolution: INameResolution;

type
  // ---------------------------------------------------------------------------
  //  The entity proxy
  // ---------------------------------------------------------------------------

  /// <summary>
  ///   A member name that resolves to no property or field of the entity.
  /// </summary>
  /// <remarks>
  ///   Raised where the expression is authored, never carried forward. The
  ///   hazard being avoided is a pass-through default: it makes a wrong name
  ///   look right on whichever path tolerates it — SQL will happily send an
  ///   unmapped column to the server — so the divergence surfaces far from its
  ///   cause and on only one back-end. The house precedent is already set;
  ///   <c>TRttiAccessors&lt;T&gt;.Get</c> raises naming both the column and the
  ///   type rather than returning an empty value.
  /// </remarks>
  EExprMemberError = class(Exception);

  /// <summary>
  ///   An entity's members as expression leaves, resolved through RTTI. This is
  ///   the always-available authoring path: nothing in this layer is reachable
  ///   only through the optional metamodel, so adopting or dropping one later is
  ///   a local edit rather than a migration.
  /// </summary>
  TEntityExpr<T> = record
  strict private
    FNames: INameResolution;
    function Names: INameResolution;
  public
    /// <summary>Binds the proxy to a strategy; <c>nil</c> means the defaults.</summary>
    constructor Create(const ANames: INameResolution);
    /// <summary>
    ///   The leaf for <c>Member</c>, carrying the property name as RTTI spells
    ///   it. Raises when the member resolves to nothing.
    /// </summary>
    function Prop(const Member: string): TExpr;
    /// <summary>The column <c>Member</c> resolves to. Also validates the member.</summary>
    function Column(const Member: string): string;
    /// <summary>The table <c>T</c> resolves to, through the same strategy.</summary>
    function Table: string;
  end;

/// <summary>
///   The property or field of <c>EntityType</c> named <c>Member</c>, or
///   <c>nil</c>. Properties win over fields, matching the in-memory lane's own
///   resolution order.
/// </summary>
function FindEntityMember(EntityType: PTypeInfo; const Member: string): TRttiMember;

/// <summary>
///   The property name as RTTI spells it, raising <c>EExprMemberError</c> when
///   the member resolves to nothing. Both authoring paths — the runtime proxy and
///   a metamodel leaf — go through here, so neither can disagree with the other
///   about casing, and both fail at the same point for the same reason.
/// </summary>
function ResolvedMemberName(EntityType: PTypeInfo; const Member: string): string;

implementation

{ TExprNode }

constructor TExprNode.Create(AKind: TExprKind);
begin
  inherited Create;
  FKind := AKind;
end;

function TExprNode.Kind: TExprKind;
begin
  Result := FKind;
end;

{ TCompareExpr }

constructor TCompareExpr.Create(AOp: TCompareOp; const ATarget: IExpr;
  const AOperands: TArray<IExpr>; ACaseSensitive: Boolean);
begin
  inherited Create(TExprKind.Compare);
  FOp := AOp;
  FTarget := ATarget;
  FOperands := AOperands;
  FCaseSensitive := ACaseSensitive;
end;

function TCompareExpr.Op: TCompareOp;
begin
  Result := FOp;
end;

function TCompareExpr.Target: IExpr;
begin
  Result := FTarget;
end;

function TCompareExpr.Operands: TArray<IExpr>;
begin
  Result := FOperands;
end;

function TCompareExpr.CaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

{ TLogicalExpr }

constructor TLogicalExpr.Create(AOp: TLogicalOp; const ALeft, ARight: IExpr);
begin
  inherited Create(TExprKind.Logical);
  FOp := AOp;
  FLeft := ALeft;
  FRight := ARight;
end;

function TLogicalExpr.Op: TLogicalOp;
begin
  Result := FOp;
end;

function TLogicalExpr.Left: IExpr;
begin
  Result := FLeft;
end;

function TLogicalExpr.Right: IExpr;
begin
  Result := FRight;
end;

{ TUnaryExpr }

constructor TUnaryExpr.Create(AOp: TUnaryOp; const AOperand: IExpr);
begin
  inherited Create(TExprKind.Unary);
  FOp := AOp;
  FOperand := AOperand;
end;

function TUnaryExpr.Op: TUnaryOp;
begin
  Result := FOp;
end;

function TUnaryExpr.Operand: IExpr;
begin
  Result := FOperand;
end;

{ TMemberExpr }

constructor TMemberExpr.Create(const AName: string; const AAccessor: TRttiMember);
begin
  inherited Create(TExprKind.Member);
  FName := AName;
  FAccessor := AAccessor;
end;

function TMemberExpr.Name: string;
begin
  Result := FName;
end;

function TMemberExpr.Accessor: TRttiMember;
begin
  Result := FAccessor;
end;

{ TValueExpr }

constructor TValueExpr.Create(const AValue: Variant);
begin
  inherited Create(TExprKind.Value);
  FValue := AValue;
end;

function TValueExpr.Value: Variant;
begin
  Result := FValue;
end;

{ TParamExpr }

constructor TParamExpr.Create(const AName: string; const AValue: Variant);
begin
  inherited Create(TExprKind.Param);
  FName := AName;
  FValue := AValue;
end;

function TParamExpr.Name: string;
begin
  Result := FName;
end;

function TParamExpr.Value: Variant;
begin
  Result := FValue;
end;

{ TNode }

class function TNode.Compare(Op: TCompareOp; const Target: IExpr;
  const Operands: TArray<IExpr>; CaseSensitive: Boolean): IExpr;
begin
  Result := TCompareExpr.Create(Op, Target, Operands, CaseSensitive);
end;

class function TNode.Logical(Op: TLogicalOp; const Left, Right: IExpr): IExpr;
begin
  Result := TLogicalExpr.Create(Op, Left, Right);
end;

class function TNode.Unary(Op: TUnaryOp; const Operand: IExpr): IExpr;
begin
  Result := TUnaryExpr.Create(Op, Operand);
end;

class function TNode.Member(const Name: string; const Accessor: TRttiMember): IExpr;
begin
  Result := TMemberExpr.Create(Name, Accessor);
end;

class function TNode.Value(const V: Variant): IExpr;
begin
  Result := TValueExpr.Create(V);
end;

class function TNode.Param(const Name: string; const V: Variant): IExpr;
begin
  Result := TParamExpr.Create(Name, V);
end;

{ TExprValue }

class function TExprValue.From<T>(const V: TNullable<T>): Variant;
begin
  if V.IsNull then
    Result := Null
  else
    Result := TValue.From<T>(V.Value).AsVariant;
end;

class function TExprValue.From(const V: TGUID): Variant;
begin
  Result := GUIDToString(V);
end;

{ TExpr }

class operator TExpr.Implicit(const Node: IExpr): TExpr;
begin
  Result.FNode := Node;
end;

class operator TExpr.Implicit(const E: TExpr): IExpr;
begin
  Result := E.FNode;
end;

function TExpr.Node: IExpr;
begin
  Result := FNode;
end;

class operator TExpr.Equal(const L: TExpr; const R: Variant): TExpr;
begin
  Result := TNode.Compare(TCompareOp.Equal, L.FNode, [TNode.Value(R)]);
end;

class operator TExpr.NotEqual(const L: TExpr; const R: Variant): TExpr;
begin
  Result := TNode.Compare(TCompareOp.NotEqual, L.FNode, [TNode.Value(R)]);
end;

class operator TExpr.Equal(const L: TExpr; const R: TGUID): TExpr;
begin
  Result := TNode.Compare(TCompareOp.Equal, L.FNode, [TNode.Value(TExprValue.From(R))]);
end;

class operator TExpr.NotEqual(const L: TExpr; const R: TGUID): TExpr;
begin
  Result := TNode.Compare(TCompareOp.NotEqual, L.FNode, [TNode.Value(TExprValue.From(R))]);
end;

class operator TExpr.LessThan(const L: TExpr; const R: Variant): TExpr;
begin
  Result := TNode.Compare(TCompareOp.Less, L.FNode, [TNode.Value(R)]);
end;

class operator TExpr.LessThanOrEqual(const L: TExpr; const R: Variant): TExpr;
begin
  Result := TNode.Compare(TCompareOp.LessOrEqual, L.FNode, [TNode.Value(R)]);
end;

class operator TExpr.GreaterThan(const L: TExpr; const R: Variant): TExpr;
begin
  Result := TNode.Compare(TCompareOp.Greater, L.FNode, [TNode.Value(R)]);
end;

class operator TExpr.GreaterThanOrEqual(const L: TExpr; const R: Variant): TExpr;
begin
  Result := TNode.Compare(TCompareOp.GreaterOrEqual, L.FNode, [TNode.Value(R)]);
end;

class operator TExpr.LogicalAnd(const L, R: TExpr): TExpr;
begin
  Result := TNode.Logical(TLogicalOp.opAnd, L.FNode, R.FNode);
end;

class operator TExpr.LogicalOr(const L, R: TExpr): TExpr;
begin
  Result := TNode.Logical(TLogicalOp.opOr, L.FNode, R.FNode);
end;

class operator TExpr.LogicalNot(const E: TExpr): TExpr;
begin
  Result := TNode.Unary(TUnaryOp.opNot, E.FNode);
end;

{ Authoring entry points }

function Prop(const Name: string): TExpr;
begin
  Result := TNode.Member(Name);
end;

function Prm(const Name: string; const Value: Variant): TExpr;
begin
  Result := TNode.Param(Name, Value);
end;

{ TDefaultNameResolution }

function TDefaultNameResolution.ColumnFor(EntityType: PTypeInfo; const Member: string): string;
begin
  // Identity, which is already the shipped convention: the in-memory lane's
  // TRttiAccessors resolves a column name against a property of the same name.
  Result := Member;
end;

function TDefaultNameResolution.TableFor(EntityType: PTypeInfo): string;
begin
  Result := _T.NameOf(EntityType);
  // The leading T is stripped only when what follows is upper case, i.e. when
  // the name really follows the Delphi T-prefix convention. Without that guard
  // a type named `Thing` would silently become `hing` — a wrong answer dressed
  // as a default, which is the one thing a default must not be.
  if (Length(Result) > 1) and (Result[1] = 'T') and CharInSet(Result[2], ['A'..'Z']) then
    Result := Copy(Result, 2, MaxInt);
end;

var
  GDefaultNames: INameResolution;

function DefaultNameResolution: INameResolution;
begin
  if GDefaultNames = nil then
    GDefaultNames := TDefaultNameResolution.Create;
  Result := GDefaultNames;
end;

{ Member resolution }

function FindEntityMember(EntityType: PTypeInfo; const Member: string): TRttiMember;
begin
  Result := nil;
  if EntityType = nil then
    Exit;
  var Ctx := TRttiContext.Create;
  try
    var RType := Ctx.GetType(EntityType);
    if RType = nil then
      Exit;
    for var P in RType.GetProperties do
      if SameText(P.Name, Member) then
        Exit(P);
    for var F in RType.GetFields do
      if SameText(F.Name, Member) then
        Exit(F);
  finally
    Ctx.Free;
  end;
end;

function ResolvedMemberName(EntityType: PTypeInfo; const Member: string): string;
begin
  var M := FindEntityMember(EntityType, Member);
  if M = nil then
    raise EExprMemberError.CreateFmt(
      'Member "%s" resolves to no property or field of %s.',
      [Member, _T.NameOf(EntityType)]);
  Result := M.Name;
end;

{ TEntityExpr<T> }

constructor TEntityExpr<T>.Create(const ANames: INameResolution);
begin
  FNames := ANames;
end;

function TEntityExpr<T>.Names: INameResolution;
begin
  if FNames <> nil then
    Result := FNames
  else
    Result := DefaultNameResolution;
end;

function TEntityExpr<T>.Prop(const Member: string): TExpr;
begin
  // The leaf carries the PROPERTY name. The column is the emitter's business and
  // it asks the strategy for it once, at emit time — declaring it here would put
  // persistence vocabulary in a unit that must stay free of it.
  Result := TNode.Member(ResolvedMemberName(TypeInfo(T), Member));
end;

function TEntityExpr<T>.Column(const Member: string): string;
begin
  Result := Names.ColumnFor(TypeInfo(T), ResolvedMemberName(TypeInfo(T), Member));
end;

function TEntityExpr<T>.Table: string;
begin
  Result := Names.TableFor(TypeInfo(T));
end;

end.
