# InMemoryJoinSample

Console application running SQLCute queries that span **two already-loaded lists** — customers and orders — through a per-call `IQueryContext`. This is the general multi-source lane: rows become `IRow` composites exposing every joined side by qualified name.

Every section also prints the SQL the other back-end would send to a database.

## What it shows

| Section | Features covered |
|---------|-----------------|
| Context | `TQueryContext.Map<T>('customers', …).Map<T>('orders', …)` — per call, never global |
| INNER JOIN | one row per matching pair, `customers.*` and `orders.*` both readable |
| LEFT JOIN | unmatched left rows survive with the right side NULL |
| GROUP BY / HAVING | flat aggregate rows: group keys plus `COUNT` / `SUM` / `MAX` |
| Subquery | `WhereInQuery` evaluated against a context source |
| Set operations | `UNION` collapsing duplicate rows |
| Fail-fast | referencing a source the context does not map |

## Running

```
BDS make --project:src/SQLCute.InMemory/samples/InMemoryJoinSample/InMemoryJoinSample.dproj
out\samples\bin\Win32\InMemoryJoinSample.exe
```

See also [InMemoryQuerySample](../InMemoryQuerySample/README.md) for the single-list fast lane, and the [usage guide](../../docs/GUIDE.md).
