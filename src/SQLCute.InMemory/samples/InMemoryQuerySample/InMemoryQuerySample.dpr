program InMemoryQuerySample;

{
  SQLCute In-Memory Sample — the fast typed lane.

  One SQLCute IQuery, two back-ends: the SQL compiler turns it into text for a
  database, and TInMemory runs the very same query over a TList<TEmployee> that
  is already in memory. Demonstrates filtering, sorting, pagination, plan reuse,
  dynamic IRow output, typed projection into a result class, DISTINCT, the
  three-valued NULL logic, and the fail-fast on a query that needs a context.

  The companion sample InMemoryJoinSample covers the multi-source lane.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  System.Generics.Collections,
  Daf.Comparers,
  Daf.Nullable,
  Daf.SQLCute,
  Daf.SQLCute.Core,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Compiler,
  Daf.SQLCute.InMemory.Rows;

type
  /// <summary>The source entity. Age is Variant so it can carry SQL NULL.</summary>
  TEmployee = class
  private
    FId: Integer;
    FName: string;
    FDepartment: string;
    FSalary: Integer;
    FAge: Variant;
  public
    constructor Create(const AId: Integer; const AName, ADepartment: string;
      const ASalary: Integer; const AAge: Variant);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Department: string read FDepartment write FDepartment;
    property Salary: Integer read FSalary write FSalary;
    property Age: Variant read FAge write FAge;
  end;

  /// <summary>Projection target — Age is TNullable so NULL differs from 0.</summary>
  TEmployeeCard = class
  private
    FName: string;
    FAge: TNullable<Integer>;
  public
    property Name: string read FName write FName;
    property Age: TNullable<Integer> read FAge write FAge;
  end;

constructor TEmployee.Create(const AId: Integer; const AName, ADepartment: string;
  const ASalary: Integer; const AAge: Variant);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FDepartment := ADepartment;
  FSalary := ASalary;
  FAge := AAge;
end;

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

procedure Title(const Text: string);
begin
  Writeln;
  Writeln('=== ' + Text + ' ===');
end;

// Shows what the SQL back-end makes of the same query — the symmetry on display.
procedure ShowSql(const Query: IQuery);
begin
  var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
  var R := Query.Compile(Compiler);
  Writeln('  SQL: ' + R.SQL);
end;

function Cell(const Row: IRow; const Column: string): string;
begin
  var V := Row.Get(Column);
  if V.IsEmpty or IsNullValue(V) then
    Exit('NULL');
  Result := VarToStr(V.AsVariant);
end;

function AgeText(const Age: Variant): string;
begin
  if VarIsNull(Age) then
    Exit('NULL');
  Result := VarToStr(Age);
end;

function SeedEmployees: TObjectList<TEmployee>;
begin
  Result := TObjectList<TEmployee>.Create(True {owns});
  Result.Add(TEmployee.Create(1, 'Ann',  'Engineering', 5200, 34));
  Result.Add(TEmployee.Create(2, 'Bob',  'Sales',       3100, 17));
  Result.Add(TEmployee.Create(3, 'Cara', 'Engineering', 4800, 29));
  Result.Add(TEmployee.Create(4, 'Dan',  'Support',     2900, Null));
  Result.Add(TEmployee.Create(5, 'Eve',  'Sales',       6100, 41));
  Result.Add(TEmployee.Create(6, 'Finn', 'Engineering', 4800, 29));
  Result.Add(TEmployee.Create(7, 'Gil',  'Support',     3300, Null));
end;

// ---------------------------------------------------------------------------
//  Demos
// ---------------------------------------------------------------------------

// Filter and sort, preserving the element type: no IRow, no catalog, T in T out.
procedure FilterAndSort(Employees: TObjectList<TEmployee>);
begin
  Title('Filter and sort a TList<TEmployee>, preserving T');
  var Q := TQuery.New.From('employees')
    .Where('salary', '>', 3000)
    .OrderByDesc('salary');
  ShowSql(Q);
  for var E in TInMemory.Elements<TEmployee>(Q, Employees) do
    Writeln(Format('  %-5s %-12s %5d', [E.Name, E.Department, E.Salary]));
end;

// Deferred execution: the queryable is built here, but nothing is scanned until
// the loop starts — and Limit stops consuming the source once it is satisfied.
procedure Paginate(Employees: TObjectList<TEmployee>);
begin
  Title('Page 2 of 3, ordered by name (deferred; Limit short-circuits)');
  var Q := TQuery.New.From('employees').OrderBy('name').ForPage(2, 3);
  ShowSql(Q);
  var Page := TInMemory.Elements<TEmployee>(Q, Employees);   // nothing ran yet
  for var E in Page do
    Writeln('  ' + E.Name);
end;

// A compiled plan is reusable: compile once, run over any source of T.
procedure ReusePlan(Employees: TObjectList<TEmployee>);
begin
  Title('One compiled plan, two sources');
  var Plan := TInMemory.Compile<TEmployee>(
    TQuery.New.From('employees').Where('department', 'Engineering').OrderBy('name'));

  Write('  main list  :');
  for var E in Plan.Over(Employees) do
    Write(' ' + E.Name);
  Writeln;

  var Contractors := TObjectList<TEmployee>.Create(True);
  try
    Contractors.Add(TEmployee.Create(8, 'Hana', 'Engineering', 5000, 38));
    Contractors.Add(TEmployee.Create(9, 'Iker', 'Sales',       3900, 45));
    Write('  contractors:');
    for var E in Plan.Over(Contractors) do
      Write(' ' + E.Name);
    Writeln;
  finally
    Contractors.Free;
  end;
end;

// With a SELECT but no result class the sequence yields dynamic IRow values.
procedure ProjectToRows(Employees: TObjectList<TEmployee>);
begin
  Title('SELECT with no result class yields IRow');
  var Q := TQuery.New.From('employees')
    .Select(['name', 'age'])
    .Where('department', 'Support');
  ShowSql(Q);
  for var R in TInMemory.Rows<TEmployee>(Q, Employees) do
    Writeln(Format('  name=%-5s age=%-4s (department projected away: %s)',
      [Cell(R, 'name'), Cell(R, 'age'), BoolToStr(not R.Has('department'), True)]));
end;

// With a result class each row is materialized by RTTI; a NULL column leaves a
// TNullable member IsNull instead of collapsing to 0.
procedure ProjectToClass(Employees: TObjectList<TEmployee>);
begin
  Title('SELECT into a result class (NULL -> TNullable)');
  var Q := TQuery.New.From('employees').Select(['name', 'age']).OrderBy('name');
  ShowSql(Q);
  var Cards := TObjectList<TEmployeeCard>.Create(True {owns});
  try
    for var C in TInMemory.Project<TEmployee, TEmployeeCard>(Q, Employees) do
      Cards.Add(C);                                   // the caller owns the instances
    for var C in Cards do
      if C.Age.HasValue then
        Writeln(Format('  %-5s %d', [C.Name, C.Age.Value]))
      else
        Writeln(Format('  %-5s (age unknown)', [C.Name]));
  finally
    Cards.Free;
  end;
end;

// DISTINCT de-duplicates over the OUTPUT columns, by value, NULLs equal.
procedure DistinctValues(Employees: TObjectList<TEmployee>);
begin
  Title('DISTINCT over the projected column (duplicate NULLs collapse)');
  var Q := TQuery.New.From('employees').Select('age').Distinct.OrderBy('age');
  ShowSql(Q);
  Write('  ages:');
  for var R in TInMemory.Rows<TEmployee>(Q, Employees) do
    Write(' ' + Cell(R, 'age'));
  Writeln;
end;

// Three-valued logic: a comparison against NULL is unknown, not false — so the
// row is excluded, and NOT(unknown) is still unknown, so it stays excluded.
procedure NullLogic(Employees: TObjectList<TEmployee>);
begin
  Title('Three-valued NULL logic');
  Writeln('  ages in the source:');
  for var E in Employees do
    Write(Format('  %s=%s', [E.Name, AgeText(E.Age)]));
  Writeln;

  Write('  age > 18       :');
  for var E in TInMemory.Elements<TEmployee>(
      TQuery.New.From('employees').Where('age', '>', 18), Employees) do
    Write(' ' + E.Name);
  Writeln;

  Write('  NOT(age > 18)  :');
  for var E in TInMemory.Elements<TEmployee>(
      TQuery.New.From('employees').WhereNot('age', '>', 18), Employees) do
    Write(' ' + E.Name);
  Writeln('        <- Dan and Gil are in neither result: unknown is not true');

  Write('  age IS NULL    :');
  for var E in TInMemory.Elements<TEmployee>(
      TQuery.New.From('employees').WhereNull('age'), Employees) do
    Write(' ' + E.Name);
  Writeln;
end;

// A query needing more than one source is rejected, never mis-executed.
procedure FailFast(Employees: TObjectList<TEmployee>);
begin
  Title('A multi-source query on the single-list entry point fails fast');
  try
    TInMemory.Elements<TEmployee>(
      TQuery.New.From('employees').Join('departments', 'employees.department', 'departments.name'),
      Employees);
    Writeln('  (unexpected: no error)');
  except
    on E: EInMemoryQueryError do
      Writeln('  EInMemoryQueryError: ' + E.Message);
  end;
end;

// ---------------------------------------------------------------------------

var
  Employees: TObjectList<TEmployee>;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Employees := SeedEmployees;
    try
      FilterAndSort(Employees);
      Paginate(Employees);
      ReusePlan(Employees);
      ProjectToRows(Employees);
      ProjectToClass(Employees);
      DistinctValues(Employees);
      NullLogic(Employees);
      FailFast(Employees);
      Writeln;
      Writeln('Done.');
    finally
      Employees.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
