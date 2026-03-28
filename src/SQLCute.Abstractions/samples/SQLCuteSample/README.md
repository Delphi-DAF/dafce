# SQLCuteSample

Console application demonstrating all SQLCute fluent query-builder capabilities.

## What it shows

| Section | Features covered |
|---------|-----------------|
| SELECT | `SELECT *`, column list, `DISTINCT`, `LIMIT`/`OFFSET` |
| ORDER BY | `ASC`, `DESC`, `OrderByRaw` |
| WHERE | equality, comparison, `IS NULL`, `IS NOT NULL`, `BETWEEN`, AND/OR, `WhereRaw` |
| WHERE IN | `WhereIn`, `WhereNotIn`, `OrWhereIn`, `OrWhereNotIn` |
| EXISTS | `WhereExists`, `WhereNotExists` |
| JOINs | `INNER JOIN`, `LEFT JOIN`, `RIGHT JOIN`, `CROSS JOIN`, `FULL OUTER JOIN` |
| GROUP BY | single column, multiple columns, `GroupByRaw` |
| HAVING | `Having`, `HavingRaw` |
| SELECT expressions | `SelectRaw`, `SelectCount`, `SelectSum`, `SelectAvg`, `SelectMin`, `SelectMax` |
| Subqueries | `FROM (subquery)`, `WhereInQuery` |
| Set operations | `UNION`, `UNION ALL`, `INTERSECT`, `EXCEPT` |
| CTE | `With`, `WithRecursive` |
| Clone | independent query variants from a shared base |
| INSERT | single row, multiple rows, `INSERT … SELECT` |
| UPDATE | with WHERE conditions |
| DELETE | with WHERE conditions |

## Running

```
BDS make --project:src/SQLCute.Abstractions/samples/SQLCuteSample/SQLCuteSample.dproj
out\samples\bin\SQLCuteSample.exe
```

Each line printed shows the generated SQL and its positional bindings (`?`).
