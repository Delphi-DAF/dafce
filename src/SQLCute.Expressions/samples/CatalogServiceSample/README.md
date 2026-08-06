# CatalogServiceSample — A Domain Service Over a Port

A domain service written against `IRepository<T>`, resolved over a plain `TList<T>`, with the composition root the only place that names an adapter.

The sample exists to make the hexagonal claim **checkable rather than stated**, so the interesting part is the file layout:

| File | Uses clause | Meaning |
|------|-------------|---------|
| `Catalog.Domain.pas` | `Daf.Query.Expressions` | The entity, its rules and the service. No SQLCute, no adapter, no driver. |
| `CatalogServiceSample.dpr` | `Daf.SQLCute.Expressions`, `Daf.SQLCute.Core`, … | The composition root — the only place SQL is linked at all. |

Swapping the in-memory adapter for a database-backed one is an edit to the second file. The domain does not change, and neither do its rules.

## Running it

```bash
BDS make --project:src/SQLCute.Expressions/samples/CatalogServiceSample/CatalogServiceSample.dproj
out\samples\bin\CatalogServiceSample.exe
```

## What it shows

- **Rules as values.** `TCatalogRules` returns `TSpecification<TProduct>` — inspectable, assertable, and translatable by construction, because there is no overload of `Where` that takes a closure.
- **A service that knows nothing about persistence.** `TCatalogService` holds an `IRepository<TProduct>` and states rules. It would compile and run against any adapter, and is testable with a five-line stub.
- **NULL handled as SQL handles it.** `Drill` has no price, so it is absent from every price comparison and present in `Price IS NULL` — three-valued logic, not a special case.
- **One loaded set, five rules.** The list is loaded once and referenced, never copied; the plan count printed at the end shows what was compiled.
- **One emission, two destinations.** The last section takes the same `Bargains` rule, emits it once, and compiles it for a database — the SQL a driver would receive for the rule the service just ran over a list.

## Ownership

The repository *references* the collection; it never owns it. The sample frees the list after the service, which is the order to follow.
