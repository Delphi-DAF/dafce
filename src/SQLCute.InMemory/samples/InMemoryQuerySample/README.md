# InMemoryQuerySample

Console application running SQLCute queries over a `TList<TEmployee>` that is already in memory — the **fast typed lane**, where the element type is preserved end to end and no `IRow` is allocated.

Every section also prints the SQL the other back-end would send to a database, so the symmetry is visible line by line.

## What it shows

| Section | Features covered |
|---------|-----------------|
| Filter and sort | `Where`, `OrderByDesc` over `TList<T>`, yielding `TEmployee` |
| Pagination | `ForPage`, deferred execution, `LIMIT` short-circuit |
| Plan reuse | `TInMemory.Compile<T>` once, `Over` two different sources |
| Dynamic rows | `SELECT` with no result class → `IEnumerable<IRow>` |
| Typed projection | `TInMemory.Project<TSource, TResult>`, NULL → `TNullable<Integer>` |
| DISTINCT | de-duplication over the projected column, duplicate NULLs collapsing |
| NULL semantics | three-valued logic: `age > 18`, `NOT(age > 18)`, `IS NULL` |
| Fail-fast | a JOIN query rejected on the single-list entry point |

## Running

```
BDS make --project:src/SQLCute.InMemory/samples/InMemoryQuerySample/InMemoryQuerySample.dproj
out\samples\bin\Win32\InMemoryQuerySample.exe
```

See also [InMemoryJoinSample](../InMemoryJoinSample/README.md) for the multi-source lane, and the [usage guide](../../docs/GUIDE.md).
