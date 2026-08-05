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
  System.Rtti,
  System.Variants,
  Daf.Nullable;

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
    class function From<T>(const V: TNullable<T>): Variant; static;
  end;

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

end.
