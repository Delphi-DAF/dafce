unit SQLCute.Operators.Validation.Feat;

{
  Ningun operador escrito como texto llega al SQL sin validar, e in y not in solo valen en Where.
}

interface

uses
  System.SysUtils,
  Daf.SQLCute,
  SQLCute.SpecHelpers;

type
  TOperatorsWorld = class(TSQLCuteWorld)
  public
    Outcome: string;
    /// Ejecuta una construccion sobre Query y deja en Outcome si lanzo y cuantas clausulas quedaron.
    procedure Attempt(const Build: TProc<IQuery>);
  end;

implementation

uses
  System.Variants,
  Daf.MiniSpec;

procedure TOperatorsWorld.Attempt(const Build: TProc<IQuery>);
begin
  var Before := Length(Query.Clauses);
  try
    Build(Query);
    Outcome := 'no exception';
  except
    on E: Exception do Outcome := E.ClassName;
  end;
  Outcome := Format('%s, clauses added=%d', [Outcome, Length(Query.Clauses) - Before]);
end;

initialization

Feature('''
TQuery — text operators are validated @unit @sqlcute @security

  An operator written as text reaches the SQL only once it is recognised, and what is emitted is
  the recognised operator, never the text received. in and not in are valid only in Where.
''')
.UseWorld<TOperatorsWorld>

// -------------------------------------------------------------------------

.Rule('Every text operator is validated before it reaches the SQL')

  .Scenario('A WhereTime operator that injects is rejected')
    .Given('a query from shifts',
      procedure(W: TOperatorsWorld)
      begin
        W.Query.From('shifts');
      end)
    .When('it calls WhereTime(''starts_at'', ''= ''''10:00'''' OR 1=1 --'', ''10:00'')',
      procedure(W: TOperatorsWorld)
      begin
        W.Attempt(procedure(Q: IQuery) begin Q.WhereTime('starts_at', '= ''10:00'' OR 1=1 --', '10:00'); end);
      end)
    .&Then('the call raises EArgumentException',
      procedure(W: TOperatorsWorld)
      begin
        Expect(W.Outcome).ToEqual('EArgumentException, clauses added=0');
      end)

  .Scenario('A Join operator that injects is rejected')
    .Given('a query from orders',
      procedure(W: TOperatorsWorld)
      begin
        W.Query.From('orders');
      end)
    .When('it calls Join(''users'', ''users.id'', ''orders.user_id'', ''= 1 OR 1=1 --'')',
      procedure(W: TOperatorsWorld)
      begin
        W.Attempt(procedure(Q: IQuery) begin Q.Join('users', 'users.id', 'orders.user_id', '= 1 OR 1=1 --'); end);
      end)
    .&Then('the call raises EArgumentException',
      procedure(W: TOperatorsWorld)
      begin
        Expect(W.Outcome).ToEqual('EArgumentException, clauses added=0');
      end)

  .Scenario('A valid operator is emitted')
    .Given('a query from orders joined to users on users.id >= orders.user_id',
      procedure(W: TOperatorsWorld)
      begin
        W.Query.From('orders').Join('users', 'users.id', 'orders.user_id', '>=');
      end)
    .When('it is compiled with TAnsiSqlCompiler',
      procedure(W: TOperatorsWorld)
      begin
        W.LastResult := W.Compiler.Compile(W.Query);
      end)
    .&Then('SQL is SELECT * FROM orders INNER JOIN users ON users.id >= orders.user_id',
      procedure(W: TOperatorsWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT * FROM orders INNER JOIN users ON users.id >= orders.user_id');
      end)

// -------------------------------------------------------------------------

.Rule('Outside Where, the text operator in is rejected')

  .Scenario('Having with in is rejected')
    .Given('a query counting orders grouped by customer_id',
      procedure(W: TOperatorsWorld)
      begin
        W.Query.From('orders').SelectCount('*', 'n').GroupBy('customer_id');
      end)
    .When('it calls Having(''n'', ''in'', VarArrayOf([1, 2]))',
      procedure(W: TOperatorsWorld)
      begin
        W.Attempt(procedure(Q: IQuery) begin Q.Having('n', 'in', VarArrayOf([1, 2])); end);
      end)
    .&Then('the call raises EArgumentException and leaves no clause',
      procedure(W: TOperatorsWorld)
      begin
        Expect(W.Outcome).ToEqual('EArgumentException, clauses added=0');
      end)

  .Scenario('WhereColumns with not in is rejected')
    .Given('a query from t',
      procedure(W: TOperatorsWorld)
      begin
        W.Query.From('t');
      end)
    .When('it calls WhereColumns(''a'', ''not in'', ''b'')',
      procedure(W: TOperatorsWorld)
      begin
        W.Attempt(procedure(Q: IQuery) begin Q.WhereColumns('a', 'not in', 'b'); end);
      end)
    .&Then('the call raises EArgumentException and leaves no clause',
      procedure(W: TOperatorsWorld)
      begin
        Expect(W.Outcome).ToEqual('EArgumentException, clauses added=0');
      end)

  .Scenario('The other operators keep working')
    .Given('a query from orders grouped by customer_id with Having(''customer_id'', ''>'', 5)',
      procedure(W: TOperatorsWorld)
      begin
        W.Query.From('orders').GroupBy('customer_id').Having('customer_id', '>', 5);
      end)
    .When('it is compiled with TAnsiSqlCompiler',
      procedure(W: TOperatorsWorld)
      begin
        W.LastResult := W.Compiler.Compile(W.Query);
      end)
    .&Then('SQL is SELECT * FROM orders GROUP BY customer_id HAVING customer_id > ? with 1 binding',
      procedure(W: TOperatorsWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM orders GROUP BY customer_id HAVING customer_id > ? | 1');
      end)

;

end.
