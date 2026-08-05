unit Daf.SQLCute.Expressions;

{
  Daf.SQLCute.Expressions — the SQL-side half of the expression layer: the one
  lowering (expression tree -> WHERE clauses on an IQuery) and the in-memory
  IRepository<T> adapter.

  There is exactly one lowering and one target. Both back-ends are already
  downstream of the SQLCute AST — the dialect compilers emit SQL from it and
  TInMemory compiles an executable plan from it — so "one query, two back-ends"
  is a property this unit inherits rather than one it implements. There is no
  expression -> Daf.Linq translator here, and there never should be.

  The name is honest precisely because of that: the only thing this unit
  produces is an IQuery.
}

interface

implementation

end.
