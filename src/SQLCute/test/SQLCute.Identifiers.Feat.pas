unit SQLCute.Identifiers.Feat;

{
  Como entrecomilla un dialecto un nombre: cada parte de un nombre cualificado por separado, el *
  sin comillas, el caracter de cierre duplicado y el alias separado del nombre.
}

interface

implementation

uses
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres,
  Daf.SQLCute.Compiler.SqlServer,
  Daf.SQLCute.Compiler.Oracle,
  SQLCute.SpecHelpers;

procedure CompileWith(const W: TSQLCuteWorld; const Compiler: IQueryCompiler; const Query: IQuery);
begin
  W.Compiler := Compiler;
  W.LastResult := W.Compiler.Compile(Query);
end;

initialization

Feature('''
TQuery — identifier quoting @unit @sqlcute

  A dialect that quotes identifiers quotes each part of a qualified name on its own, leaves a
  trailing * bare, doubles its closing character inside a name, and splits an alias written in it.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('A qualified name is quoted part by part')

  .Scenario('Postgres quotes table and column separately')
    .When('query calls Select(''users.id'').From(''public.users''), compiled with TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TPostgresCompiler.Create, TQuery.New.Select('users.id').From('public.users'));
      end)
    .&Then('SQL is SELECT "users"."id" FROM "public"."users"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "users"."id" FROM "public"."users"');
      end)

  .Scenario('The asterisk of a table stays unquoted')
    .When('query calls Select(''users.*'').From(''users''), compiled with TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TPostgresCompiler.Create, TQuery.New.Select('users.*').From('users'));
      end)
    .&Then('SQL is SELECT "users".* FROM "users"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "users".* FROM "users"');
      end)

  .Scenario('SQL Server quotes each part with brackets')
    .When('query calls From(''orders'').Join(''users'', ''users.id'', ''orders.user_id''), compiled with TSqlServerCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TSqlServerCompiler.Create, TQuery.New.From('orders').Join('users', 'users.id', 'orders.user_id'));
      end)
    .&Then('SQL is SELECT * FROM [orders] INNER JOIN [users] ON [users].[id] = [orders].[user_id]',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT * FROM [orders] INNER JOIN [users] ON [users].[id] = [orders].[user_id]');
      end)

  .Scenario('Oracle upper-cases each part')
    .When('query calls Select(''users.name'').From(''users''), compiled with TOracleCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TOracleCompiler.Create, TQuery.New.Select('users.name').From('users'));
      end)
    .&Then('SQL is SELECT "USERS"."NAME" FROM "USERS"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "USERS"."NAME" FROM "USERS"');
      end)

// -------------------------------------------------------------------------

.Rule('An alias written in a column name is split')

  .Scenario('Postgres splits the alias')
    .When('query calls Select(''users.name as n'').From(''users''), compiled with TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TPostgresCompiler.Create, TQuery.New.Select('users.name as n').From('users'));
      end)
    .&Then('SQL is SELECT "users"."name" AS "n" FROM "users"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "users"."name" AS "n" FROM "users"');
      end)

// -------------------------------------------------------------------------

.Rule('The closing character inside a name is doubled')

  .Scenario('A double quote inside the name is doubled')
    .When('query calls Select(''a"b'').From(''t''), compiled with TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TPostgresCompiler.Create, TQuery.New.Select('a"b').From('t'));
      end)
    .&Then('SQL is SELECT "a""b" FROM "t"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "a""b" FROM "t"');
      end)

  .Scenario('A closing bracket inside the name is doubled')
    .When('query calls Select(''a]b'').From(''t''), compiled with TSqlServerCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TSqlServerCompiler.Create, TQuery.New.Select('a]b').From('t'));
      end)
    .&Then('SQL is SELECT [a]]b] FROM [t]',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT [a]]b] FROM [t]');
      end)

// -------------------------------------------------------------------------

.Rule('Postgres and SQL Server quote column and table names')

  .Scenario('Postgres ColumnQuoting')
    .When('Q.From(''public.users'').Select(''first name'') is compiled with TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TPostgresCompiler.Create, TQuery.New.From('public.users').Select('first name'));
      end)
    .&Then('SQL is SELECT "first name" FROM "public"."users"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "first name" FROM "public"."users"');
      end)

  .Scenario('SQL Server ColumnQuoting')
    .When('Q.From(''dbo.users'').Select(''first name'') is compiled with TSqlServerCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TSqlServerCompiler.Create, TQuery.New.From('dbo.users').Select('first name'));
      end)
    .&Then('SQL is SELECT [first name] FROM [dbo].[users]',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT [first name] FROM [dbo].[users]');
      end)

// -------------------------------------------------------------------------

.Rule('Oracle writes no AS before an alias')

  .Scenario('A table alias without AS')
    .When('query calls From(''users'', ''u'').Where(''u.active'', 1), compiled with TOracleCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TOracleCompiler.Create, TQuery.New.From('users', 'u').Where('u.active', 1));
      end)
    .&Then('SQL is SELECT * FROM "USERS" "U" WHERE "U"."ACTIVE" = ?',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT * FROM "USERS" "U" WHERE "U"."ACTIVE" = ?');
      end)

  .Scenario('A column alias without AS')
    .When('query calls SelectAs(''name'', ''n'').From(''users''), compiled with TOracleCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        CompileWith(W, TOracleCompiler.Create, TQuery.New.SelectAs('name', 'n').From('users'));
      end)
    .&Then('SQL is SELECT "NAME" "N" FROM "USERS"',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT "NAME" "N" FROM "USERS"');
      end)

;

end.
