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

interface

implementation

end.
