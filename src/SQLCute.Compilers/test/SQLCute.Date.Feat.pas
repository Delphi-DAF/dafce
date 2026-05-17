unit SQLCute.Date.Feat;

{
  Dialect compiler specs for F7: date/time WHERE operations.
  Covers WhereDate, WhereTime, WhereDatePart and their OR variants
  across ANSI, SQL Server, Postgres, MySQL, SQLite, Oracle, and Firebird.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — Date / Time WHERE operations @unit @sqlcute @f7

  WhereDate, WhereTime and WhereDatePart add WHERE conditions that filter
  on date/time portions of a datetime column using dialect-specific SQL.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('WhereDate emits dialect-specific date comparison @f7')

  .Scenario('ANSI fallback emits plain column comparison @f7')
    .Given('a date query from "orders" wheredate "created_at" equals "2024-01-15"')
    .When('compiled with the "ANSI" dialect')
    .&Then('SQL is "SELECT * FROM orders WHERE created_at = ?"')

  .Scenario('SQL Server wraps column in CAST AS date @f7')
    .Given('a date query from "orders" wheredate "created_at" equals "2024-01-15"')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [orders] WHERE CAST([created_at] AS date) = @p0"')

  .Scenario('Postgres uses cast-shorthand ::date @f7')
    .Given('a date query from "orders" wheredate "created_at" equals "2024-01-15"')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL is "SELECT * FROM "orders" WHERE "created_at"::date = $1"')

  .Scenario('MySQL uses DATE() function @f7')
    .Given('a date query from "orders" wheredate "created_at" equals "2024-01-15"')
    .When('compiled with the "MySQL" dialect')
    .&Then('SQL contains "DATE(`created_at`) = ?"')

  .Scenario('SQLite uses date() function @f7')
    .Given('a date query from "orders" wheredate "created_at" equals "2024-01-15"')
    .When('compiled with the "SQLite" dialect')
    .&Then('SQL contains "date("created_at") = ?"')

// -------------------------------------------------------------------------

.Rule('WhereTime emits dialect-specific time comparison @f7')

  .Scenario('SQL Server wraps column in CAST AS time @f7')
    .Given('a date query from "logs" wheretime "happened_at" ">" "08:00:00"')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL contains "CAST([happened_at] AS time) > @p0"')

  .Scenario('Postgres uses cast-shorthand ::time @f7')
    .Given('a date query from "logs" wheretime "happened_at" ">" "08:00:00"')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL contains ""happened_at"::time > $1"')

// -------------------------------------------------------------------------

.Rule('WhereDatePart emits dialect-specific date-part extraction @f7')

  .Scenario('SQL Server uses DATEPART for year extraction @f7')
    .Given('a date query from "t" wheredatepart "dpYear" "created_at" equals 2024')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL contains "DATEPART(year, [created_at]) = @p0"')

  .Scenario('Postgres uses DATE_PART for month extraction @f7')
    .Given('a date query from "t" wheredatepart "dpMonth" "created_at" equals 6')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL contains "DATE_PART(''month'', "created_at") = $1"')

  .Scenario('MySQL uses DAY() function for day extraction @f7')
    .Given('a date query from "t" wheredatepart "dpDay" "dt" equals 15')
    .When('compiled with the "MySQL" dialect')
    .&Then('SQL contains "DAY(`dt`) = ?"')

  .Scenario('SQLite uses strftime for year extraction @f7')
    .Given('a date query from "t" wheredatepart "dpYear" "dt" equals 2024')
    .When('compiled with the "SQLite" dialect')
    .&Then('SQL contains "strftime(''%Y'', "dt") = ?"')

  .Scenario('Oracle uses EXTRACT for year extraction @f7')
    .Given('a date query from "t" wheredatepart "dpYear" "dt" equals 2024')
    .When('compiled with the "Oracle" dialect')
    .&Then('SQL contains "EXTRACT(YEAR FROM "DT") = :p1"')

  .Scenario('Firebird uses EXTRACT for year extraction @f7')
    .Given('a date query from "t" wheredatepart "dpYear" "dt" equals 2024')
    .When('compiled with the "Firebird" dialect')
    .&Then('SQL contains "EXTRACT(YEAR FROM dt) = ?"')

// -------------------------------------------------------------------------

.Rule('OrWhereDate / OrWhereTime / OrWhereDatePart add OR conditions @f7')

  .Scenario('Postgres OrWhereDate connects conditions with OR @f7')
    .Given('a date query from "t" wheredate "d" equals "2024-01-01" orwheredate "d" equals "2024-12-31"')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL contains ""d"::date = $1 OR "d"::date = $2"')

;

end.
