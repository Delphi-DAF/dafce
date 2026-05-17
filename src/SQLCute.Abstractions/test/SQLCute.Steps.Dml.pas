unit SQLCute.Steps.Dml;

{
  INSERT, UPDATE and DELETE steps
}

interface

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.SQLCute,
  SQLCute.SpecHelpers;

type
  TSQLCuteStepsDml = class
  public
    [Given('a single-row INSERT query')]
    procedure GivenInsertSingleRow(W: TSQLCuteWorld);

    [Given('a multi-row INSERT query')]
    procedure GivenInsertMultiRow(W: TSQLCuteWorld);

    [Given('an INSERT FROM SELECT query')]
    procedure GivenInsertFromSelect(W: TSQLCuteWorld);

    [Given('an UPDATE single-column query')]
    procedure GivenUpdateSingleCol(W: TSQLCuteWorld);

    [Given('an UPDATE multi-column query')]
    procedure GivenUpdateMultiCol(W: TSQLCuteWorld);

    [Given('a query deleting from "(\w+)" where "(\w+)" = (\d+)')]
    procedure GivenDeleteFromWhere(W: TSQLCuteWorld; Table, Col: string; Val: Integer);

    [Given('a query deleting all from "(\w+)"')]
    procedure GivenDeleteAll(W: TSQLCuteWorld; Table: string);

    [Given('an UPDATE with multiple columns and conditions')]
    procedure GivenUpdateMultipleColsAndConditions(W: TSQLCuteWorld);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsDml implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsDml.GivenInsertSingleRow(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .AsInsert(['name', 'email'], ['John Doe', 'john@example.com']);
end;

procedure TSQLCuteStepsDml.GivenInsertMultiRow(W: TSQLCuteWorld);
var
  R1, R2: TArray<Variant>;
  Rows: TArray<TArray<Variant>>;
begin
  R1   := [1, 'first entry'];
  R2   := [2, 'second entry'];
  Rows := [R1, R2];
  W.Query := TQuery.New
    .From('logs')
    .AsInsertRows(['level', 'msg'], Rows);
end;

procedure TSQLCuteStepsDml.GivenInsertFromSelect(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('users').Select(['id', 'name']).Where('active', True);
  W.Query := TQuery.New
    .From('archive')
    .AsInsertFrom(['id', 'name'], Sub);
end;

procedure TSQLCuteStepsDml.GivenUpdateSingleCol(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('id', 1)
    .AsUpdate(['status'], ['inactive']);
end;

procedure TSQLCuteStepsDml.GivenUpdateMultiCol(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('id', 1)
    .AsUpdate(['name', 'email'], ['Jane', 'jane@example.com']);
end;

procedure TSQLCuteStepsDml.GivenDeleteFromWhere(W: TSQLCuteWorld;
  Table, Col: string; Val: Integer);
begin
  W.Query := TQuery.New.From(Table).Where(Col, Val).AsDelete;
end;

procedure TSQLCuteStepsDml.GivenDeleteAll(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.From(Table).AsDelete;
end;

procedure TSQLCuteStepsDml.GivenUpdateMultipleColsAndConditions(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Where('user_id', 5)
    .Where('status', 'pending')
    .AsUpdate(['status', 'updated_at'], ['shipped', '2024-01-15']);
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsDml>;

end.
