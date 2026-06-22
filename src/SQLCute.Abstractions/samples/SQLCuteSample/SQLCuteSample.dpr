program SQLCuteSample;

{
  SQLCute Sample — demonstrates all fluent query-builder capabilities:
    SELECT, WHERE, JOINs, GROUP BY/HAVING, ORDER BY, LIMIT/OFFSET,
    UNION / UNION ALL / INTERSECT / EXCEPT, CTE, WITH RECURSIVE,
    DML (INSERT / UPDATE / DELETE), subqueries and raw expressions.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Variants,
  Daf.SQLCute,
  Daf.SQLCute.Compiler;

// ---------------------------------------------------------------------------
//  Helper — prints SQL + bindings for a compiled result
// ---------------------------------------------------------------------------

procedure PrintResult(const Title: string; const R: TSQLResult);
var
  I: Integer;
  BndStr: string;
begin
  Writeln('--- ' + Title + ' ---');
  Writeln('SQL  : ' + R.SQL);
  if Length(R.Bindings) = 0 then
    BndStr := '(none)'
  else
  begin
    BndStr := '';
    for I := 0 to High(R.Bindings) do
    begin
      if I > 0 then BndStr := BndStr + ', ';
      BndStr := BndStr + VarToStr(R.Bindings[I]);
    end;
  end;
  Writeln('Bind : ' + BndStr);
  Writeln;
end;

var
  Compiler: IQueryCompiler;
begin
  ReportMemoryLeaksOnShutdown := True;

  Compiler := TAnsiSqlCompiler.Create;

  // =========================================================================
  //  1. Basic SELECT / FROM / LIMIT / OFFSET
  // =========================================================================

  PrintResult('SELECT *',
    TQuery.New.From('users').Compile(Compiler));

  PrintResult('SELECT columns',
    TQuery.New.Select(['id', 'name', 'email']).From('users').Compile(Compiler));

  PrintResult('SELECT DISTINCT',
    TQuery.New.From('tags').Select('name').Distinct.Compile(Compiler));

  PrintResult('LIMIT + OFFSET',
    TQuery.New.From('products').Limit(10).Offset(20).Compile(Compiler));

  // =========================================================================
  //  2. ORDER BY
  // =========================================================================

  PrintResult('ORDER BY ASC',
    TQuery.New.From('users').OrderBy('name').Compile(Compiler));

  PrintResult('ORDER BY DESC',
    TQuery.New.From('orders').OrderByDesc('created_at').Compile(Compiler));

  PrintResult('ORDER BY RAW',
    TQuery.New.From('users')
      .OrderByRaw('FIELD(status, ''active'', ''pending'', ''closed'')')
      .Compile(Compiler));

  // =========================================================================
  //  3. WHERE conditions
  // =========================================================================

  PrintResult('WHERE equality',
    TQuery.New.From('users').Where('active', True).Compile(Compiler));

  PrintResult('WHERE comparison',
    TQuery.New.From('products').Where('price', '>', 100).Compile(Compiler));

  PrintResult('WHERE IS NULL',
    TQuery.New.From('users').WhereNull('deleted_at').Compile(Compiler));

  PrintResult('WHERE IS NOT NULL',
    TQuery.New.From('users').WhereNotNull('email').Compile(Compiler));

  PrintResult('WHERE BETWEEN',
    TQuery.New.From('orders').WhereBetween('total', 100, 500).Compile(Compiler));

  PrintResult('WHERE AND / OR',
    TQuery.New.From('users')
      .Where('active', True)
      .OrWhere('role', 'admin')
      .Compile(Compiler));

  PrintResult('WHERE RAW',
    TQuery.New.From('logs').WhereRaw('severity IN (1, 2, 3)').Compile(Compiler));

  // =========================================================================
  //  4. WHERE IN / NOT IN
  // =========================================================================

  PrintResult('WHERE IN',
    TQuery.New.From('users').WhereIn('id', [10, 20, 30]).Compile(Compiler));

  PrintResult('WHERE NOT IN',
    TQuery.New.From('products').WhereNotIn('status', ['discontinued', 'archived']).Compile(Compiler));

  PrintResult('OR WHERE IN',
    TQuery.New.From('users')
      .Where('active', True)
      .OrWhereIn('id', [1, 2])
      .Compile(Compiler));

  PrintResult('OR WHERE NOT IN',
    TQuery.New.From('users')
      .Where('active', True)
      .OrWhereNotIn('status', ['banned', 'deleted'])
      .Compile(Compiler));

  // =========================================================================
  //  5. WHERE EXISTS / NOT EXISTS
  // =========================================================================

  PrintResult('WHERE EXISTS',
    TQuery.New.From('users')
      .WhereExists(TQuery.New.From('orders').Where('user_id', 99))
      .Compile(Compiler));

  PrintResult('WHERE NOT EXISTS',
    TQuery.New.From('users')
      .WhereNotExists(TQuery.New.From('orders').Where('user_id', 99))
      .Compile(Compiler));

  // =========================================================================
  //  6. JOINs
  // =========================================================================

  PrintResult('INNER JOIN',
    TQuery.New.From('users')
      .Join('orders', 'users.id = orders.user_id')
      .Compile(Compiler));

  PrintResult('LEFT JOIN',
    TQuery.New.From('users')
      .LeftJoin('posts', 'users.id', 'posts.user_id')
      .Compile(Compiler));

  PrintResult('RIGHT JOIN',
    TQuery.New.From('users')
      .RightJoin('posts', 'users.id', 'posts.user_id')
      .Compile(Compiler));

  PrintResult('CROSS JOIN',
    TQuery.New.From('users').CrossJoin('tags').Compile(Compiler));

  PrintResult('FULL OUTER JOIN',
    TQuery.New.From('users')
      .FullOuterJoin('logs', 'users.id', 'logs.user_id')
      .Compile(Compiler));

  // =========================================================================
  //  7. GROUP BY / HAVING
  // =========================================================================

  PrintResult('GROUP BY',
    TQuery.New.From('orders').GroupBy('status').Compile(Compiler));

  PrintResult('GROUP BY multiple columns',
    TQuery.New.From('sales').GroupBy(['year', 'month', 'dept_id']).Compile(Compiler));

  PrintResult('GROUP BY + HAVING',
    TQuery.New.From('orders')
      .Select('user_id')
      .SelectRaw('COUNT(*) AS cnt')
      .GroupBy('user_id')
      .Having('COUNT(*)', '>', 5)
      .Compile(Compiler));

  PrintResult('GROUP BY RAW',
    TQuery.New.From('events').Select('event').GroupByRaw('DATE(created_at)').Compile(Compiler));

  PrintResult('HAVING RAW',
    TQuery.New.From('orders')
      .Select('dept_id')
      .GroupBy('dept_id')
      .HavingRaw('SUM(total) > 1000')
      .Compile(Compiler));

  // =========================================================================
  //  8. SELECT expressions (raw, aliases, aggregates)
  // =========================================================================

  PrintResult('SELECT RAW expression',
    TQuery.New.From('users').Select('id').SelectRaw('UPPER(name) AS uname').Compile(Compiler));

  PrintResult('SELECT COUNT',
    TQuery.New.From('orders').SelectCount.Compile(Compiler));

  PrintResult('SELECT SUM / AVG / MIN / MAX',
    TQuery.New.From('sales')
      .SelectSum('amount', 'total')
      .SelectAvg('amount', 'avg_amount')
      .SelectMin('amount', 'min_amount')
      .SelectMax('amount', 'max_amount')
      .Compile(Compiler));

  // =========================================================================
  //  9. Subqueries
  // =========================================================================

  PrintResult('FROM (subquery)',
    TQuery.New
      .From(TQuery.New.From('orders').Where('status', 'pending'), 'sub')
      .Select(['sub.id', 'sub.total'])
      .Compile(Compiler));

  PrintResult('WHERE IN (subquery)',
    TQuery.New.From('users')
      .WhereInQuery('id', TQuery.New.From('orders').Select('user_id'))
      .Compile(Compiler));

  // =========================================================================
  //  10. UNION / UNION ALL / INTERSECT / EXCEPT
  // =========================================================================

  PrintResult('UNION',
    TQuery.New.From('active_users').Select('id')
      .Union(TQuery.New.From('archived_users').Select('id'))
      .Compile(Compiler));

  PrintResult('UNION ALL',
    TQuery.New.From('a').Select('id')
      .UnionAll(TQuery.New.From('b').Select('id'))
      .Compile(Compiler));

  PrintResult('INTERSECT',
    TQuery.New.From('a').Select('id')
      .Intersect(TQuery.New.From('b').Select('id'))
      .Compile(Compiler));

  PrintResult('EXCEPT',
    TQuery.New.From('a').Select('id')
      .&Except(TQuery.New.From('b').Select('id'))
      .Compile(Compiler));

  // =========================================================================
  //  11. CTE (WITH / WITH RECURSIVE)
  // =========================================================================

  PrintResult('WITH (CTE)',
    TQuery.New
      .&With('recent', TQuery.New.From('orders').Where('created', '>', '2024-01-01'))
      .From('recent')
      .Compile(Compiler));

  PrintResult('WITH RECURSIVE',
    TQuery.New
      .WithRecursive('nums', TQuery.New.From('base').Select('n'))
      .From('nums')
      .Select('n')
      .Compile(Compiler));

  // =========================================================================
  //  12. Clone
  // =========================================================================

  var Base := TQuery.New.From('users').Where('active', True);
  var Clone1 := Base.Clone.Limit(10);
  var Clone2 := Base.Clone.OrderBy('name').Limit(5);

  PrintResult('Clone — base',  Base.Compile(Compiler));
  PrintResult('Clone — page1', Clone1.Compile(Compiler));
  PrintResult('Clone — page2', Clone2.Compile(Compiler));

  // =========================================================================
  //  13. DML — INSERT
  // =========================================================================

  PrintResult('INSERT single row',
    TQuery.New.From('users')
      .AsInsert(['name', 'email'], ['Alice', 'alice@example.com'])
      .Compile(Compiler));

  PrintResult('INSERT multiple rows',
    TQuery.New.From('users')
      .AsInsertRows(['name', 'email'],
        [['Bob', 'bob@example.com'], ['Carol', 'carol@example.com']])
      .Compile(Compiler));

  PrintResult('INSERT FROM SELECT',
    TQuery.New.From('users_archive')
      .AsInsertFrom(['id', 'name'],
        TQuery.New.From('users').Select(['id', 'name']).Where('active', False))
      .Compile(Compiler));

  // =========================================================================
  //  14. DML — UPDATE / DELETE
  // =========================================================================

  PrintResult('UPDATE',
    TQuery.New.From('orders')
      .Where('user_id', 42)
      .Where('status', 'pending')
      .AsUpdate(['status', 'updated_at'], ['shipped', '2024-06-01'])
      .Compile(Compiler));

  PrintResult('DELETE',
    TQuery.New.From('sessions')
      .Where('expired', True)
      .AsDelete
      .Compile(Compiler));

  Writeln('Done.');
end.
