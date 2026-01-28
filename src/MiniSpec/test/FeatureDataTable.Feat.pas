unit FeatureDataTable.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.DataTable;

type
  TUser = record
    Name: string;
    Email: string;
  end;

  TDataTableWorld = class(TFeatureWorld)
  public
    Users: TArray<TUser>;
    Table: TDataTableObj;
    QueryResult: Integer;
  end;

initialization

Feature('''
Feature DataTable @datatable

  As a test author
  I want to pass inline data tables to steps
  So I can provide structured test data concisely
''')

.UseWorld<TDataTableWorld>

.Scenario('Access DataTable from step via ISpecContext')
  .Given('the following users:',
    [['name',  'email'],
     ['Alice', 'alice@test.com'],
     ['Bob',   'bob@test.com'],
     ['Carol', 'carol@test.com']],
    procedure(World: TDataTableWorld)
    var
      Ctx: ISpecContext;
      Row: TArray<TValue>;
      User: TUser;
    begin
      Ctx := World as ISpecContext;
      World.Table := Ctx.DataTable;
      // Store users from table
      for Row in World.Table.Rows do
      begin
        User.Name := Row[0].AsString;
        User.Email := Row[1].AsString;
        World.Users := World.Users + [User];
      end;
    end)
  .When('I count the users', procedure(World: TDataTableWorld)
    begin
      World.QueryResult := Length(World.Users);
    end)
  .&Then('I should have 3 users', procedure(World: TDataTableWorld)
    begin
      Expect(World.QueryResult).ToEqual(3);
    end)

.Scenario('DataTable provides row count')
  .Given('a table with data:',
    [['col1', 'col2'],
     ['a',    'b'],
     ['c',    'd']],
    procedure(World: TDataTableWorld)
    begin
      World.Table := (World as ISpecContext).DataTable;
    end)
  .When('I check the row count', procedure(World: TDataTableWorld)
    begin
      World.QueryResult := World.Table.RowCount;
    end)
  .&Then('it should be 2', procedure(World: TDataTableWorld)
    begin
      Expect(World.QueryResult).ToEqual(2);
    end)

.Scenario('DataTable provides column count')
  .Given('a table with 3 columns:',
    [['a', 'b', 'c'],
     ['1', '2', '3']],
    procedure(World: TDataTableWorld)
    begin
      World.Table := (World as ISpecContext).DataTable;
    end)
  .When('I check the column count', procedure(World: TDataTableWorld)
    begin
      World.QueryResult := World.Table.ColCount;
    end)
  .&Then('it should be 3', procedure(World: TDataTableWorld)
    begin
      Expect(World.QueryResult).ToEqual(3);
    end)

.Scenario('DataTable cell access by column name')
  .Given('a table with named columns:',
    [['name',  'age'],
     ['Alice', '30'],
     ['Bob',   '25']],
    procedure(World: TDataTableWorld)
    begin
      World.Table := (World as ISpecContext).DataTable;
    end)
  .When('I access cell by column name', procedure(World: TDataTableWorld)
    begin
      // Access Bob's age (row 1, column 'age')
      World.QueryResult := StrToInt(World.Table.Cell(1, 'age').AsString);
    end)
  .&Then('I get the correct value', procedure(World: TDataTableWorld)
    begin
      Expect(World.QueryResult).ToEqual(25);
    end)

.Scenario('DataTable is nil when step has no table')
  .Given('a step without a table', procedure(World: TDataTableWorld)
    begin
      World.Table := (World as ISpecContext).DataTable;
    end)
  .When('I check the DataTable', procedure(World: TDataTableWorld)
    begin
      // Nothing to do
    end)
  .&Then('it should be nil', procedure(World: TDataTableWorld)
    begin
      Expect(World.Table = nil).ToBeTrue;
    end)

;

end.
