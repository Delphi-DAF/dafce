unit SQLCute.WhereTree.Feat;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.WhereTree;

type
  TWhereTreeWorld = class
  public
    Query: IQuery;
    Root: TWhereNode;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Daf.MiniSpec;

constructor TWhereTreeWorld.Create;
begin
  inherited;
  Query := TQuery.New;
end;

destructor TWhereTreeWorld.Destroy;
begin
  Root.Free;
  inherited;
end;

initialization

Feature('''
Feature WhereTree @wheretree @sqlcute

  As the shared WHERE normalizer
  I want a flat clause list turned into a precedence-correct boolean tree
  So both back-ends apply SQL precedence (AND before OR) by construction
''')

.UseWorld<TWhereTreeWorld>

.Rule('Precedence grouping')

  .Scenario('A single condition becomes a leaf')
    .When('a single equality condition is built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').Where('a', '=', 1);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the tree is a single leaf',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(W.Root <> nil).ToBeTrue;
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end)

  .Scenario('A run of ANDs becomes one conjunction with no OR wrapper')
    .When('two AND-joined conditions are built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').Where('a', '=', 1).Where('b', '=', 2);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the root is a conjunction of two leaves',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Conjunction));
        Expect(Length(W.Root.Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end)

  .Scenario('Mixed AND/OR groups AND before OR')
    .When('a AND b OR c AND d is built with no explicit group',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t')
          .Where('a', '=', 1)
          .Where('b', '=', 2)
          .OrWhere('c', '=', 3)
          .Where('d', '=', 4);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the root is (a AND b) OR (c AND d)',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Disjunction));
        Expect(Length(W.Root.Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Conjunction));
        Expect(Length(W.Root.Children[0].Children)).ToEqual(2);
        Expect(Ord(W.Root.Children[1].Kind)).ToEqual(Ord(TWhereNodeKind.Conjunction));
        Expect(Length(W.Root.Children[1].Children)).ToEqual(2);
      end)

  .Scenario('WhereNot wraps the condition in a negation node')
    .When('a negated condition is built',
      procedure(W: TWhereTreeWorld)
      begin
        W.Query.From('t').WhereNot('a', '>', 18);
        W.Root := TWhereTree.Build(W.Query.Clauses);
      end)
    .&Then('the tree is a negation of a leaf',
      procedure(W: TWhereTreeWorld)
      begin
        Expect(Ord(W.Root.Kind)).ToEqual(Ord(TWhereNodeKind.Neg));
        Expect(Length(W.Root.Children)).ToEqual(1);
        Expect(Ord(W.Root.Children[0].Kind)).ToEqual(Ord(TWhereNodeKind.Leaf));
      end);

end.
