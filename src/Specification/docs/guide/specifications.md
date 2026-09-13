# Specifications and the repository port

**🌍 Language: English | [Español](specifications.es.md)**

← [Back to Guide](../GUIDE.md)

---

## A rule as a value

`TSpecification<T>` bundles a filter expression with optional ordering and paging. It is a value: build it, pass it, store it, assert it.

```pascal
var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price')
  .Skip(10)
  .Take(5);
```

Each builder returns a new specification, so a base rule can be refined without being mutated:

```pascal
var Active := TSpecification<TProduct>.New.Where(Prop('Active') = True);
var Page1  := Active.OrderBy('Name').Take(20);
var Page2  := Active.OrderBy('Name').Skip(20).Take(20);
```

Ordering members are validated against `T` when declared, for the same reason member leaves are: a name that resolves to nothing should fail where the rule is written.

Reading it back needs no database:

```pascal
var Root := Spec.Filter as ILogicalExpr;
Root.Op                                        // TLogicalOp.opAnd
Spec.Order[0].Member                           // 'Price'
Spec.Order[0].Descending                       // True
Spec.SkipCount                                 // 10
Spec.TakeCount                                 // 5
Spec.Key                                       // canonical, whole-rule
```

`Skip(0)` and `Take(0)` mean *unset*, so an unpaged rule adds no clause at all when emitted.

---

## No closures, on purpose

There is no overload of `Where` that accepts a predicate. A specification carries only the expression tree, which makes it translatable by construction — and leaves no silent client-side fallback to regret later. EF Core shipped one and removed it in 3.0 for exactly this reason.

Nothing is lost. When you genuinely want arbitrary Delphi code over a materialized sequence, that is what [`Daf.Linq`](../../../Linq/README.md) is for:

```pascal
for var P in Linq.From<TProduct>(Loaded).Where(
    function(P: TProduct): Boolean
    begin
      Result := SomeRuleNoSqlCanExpress(P);
    end) do ...
```

The two are siblings. One is translatable; the other is not, and says so by being a different type.

---

## No includes, either

Eager-loading directives are not part of a specification. Beyond having no clause to lower into, they are the wrong owner: a repository returns aggregates, and how an aggregate gets hydrated is the adapter's business. A specification says *what* to select.

---

## The port

```pascal
IRepository<T> = interface
  function List(const Spec: TSpecification<T>): TArray<T>;
end;
```

No SQL, no connection, no row, no query in the signature — and no `T: TEntity` or any other base-class constraint. A plain class with public properties works; so does a record.

A domain service depends on the port and on nothing else:

```pascal
function TCatalogService.Bargains: TArray<TProduct>;
begin
  Result := FProducts.List(
    TSpecification<TProduct>.New
      .Where((Prop('Price') < 50) and (Prop('Active') = True))
      .OrderBy('Price'));
end;
```

That unit links neither SQLCute nor any adapter. It is testable with a five-line stub, and the composition root is the only place that names a concrete repository.

---

## Getting an adapter

This package defines the port; [`Daf.SQLCute.Specification`](../../../SQLCute.Specification/README.md) ships the in-memory one:

```pascal
uses Daf.SQLCute.Specification;

var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded);
var Service := TCatalogService.Create(Repo);
```

The in-memory adapter is not a test stub. Running several rules over one already-loaded collection is a production path — load once, then resolve N specifications against the loaded set, with the same specification type that would go to a database. That is why the "one rule, two back-ends" symmetry has a concrete motive rather than an aesthetic one.

There is no database-backed adapter: SQL execution, driver and row materialization are not part of this package or of `Daf.SQLCute.Specification`.
