unit ReadOnly.Feat;

{
  The in-memory backend is read-only, and now says so by the declared statement
  kind, not by spotting a DML clause: a query that is not a retrieval is rejected
  naming the kind it is. That naming is what tells this whitelist-of-one apart
  from the clause blacklist it replaces — the old message named no kind, so a
  statement kind added later would have slipped through unnamed.
}

interface

uses
  Daf.SQLCute;

type
  /// The statement the scenario sets up, and what compiling it for this backend answered.
  TReadOnlyWorld = class
  public
    Query: IQuery;
    Outcome: string;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Compiler;   // EInMemoryQueryError

/// Runs it and answers what happened, as text: the exception class with its message, or the mark
/// that none was raised. With the call inside the ExpectException, the montage travelled with the
/// event -- any exception out of the setup counted as the expected one -- and a second Then had to
/// build and run it all over again to look at anything else.
function CompileOutcome(const Q: IQuery): string;
begin
  try
    TInMemory.Compile<TObject>(Q);
    Result := '<compiled without refusing>';
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature In-memory read-only @sqlcute @inmemory @readonly

  As the in-memory backend
  I want to reject anything that is not a retrieval by its declared kind
  So a statement kind added later is refused, and named, without a per-clause blacklist
''')

.UseWorld<TReadOnlyWorld>

.Rule('The in-memory backend rejects a modification, naming its kind')

  .Scenario('An insertion is rejected as read-only, naming Insert')
    .Given('an INSERT statement',
      procedure(W: TReadOnlyWorld)
      begin
        W.Query := TQuery.New.From('people').AsInsert(['name'], ['Ann']);
      end)
    .When('it is compiled for the in-memory backend',
      procedure(W: TReadOnlyWorld)
      begin
        W.Outcome := CompileOutcome(W.Query);
      end)
    .&Then('compiling an INSERT for the in-memory backend raises the read-only error',
      procedure(W: TReadOnlyWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)
    .&Then('and the message names the kind it received',
      procedure(W: TReadOnlyWorld)
      begin
        Expect(W.Outcome).ToContain('Insert');
      end)

  .Scenario('An update is rejected as read-only, naming Update')
    .Given('an UPDATE statement',
      procedure(W: TReadOnlyWorld)
      begin
        W.Query := TQuery.New.From('people').Where('id', '=', 1).AsUpdate(['name'], ['Ann']);
      end)
    .When('it is compiled for the in-memory backend',
      procedure(W: TReadOnlyWorld)
      begin
        W.Outcome := CompileOutcome(W.Query);
      end)
    .&Then('the message names Update',
      procedure(W: TReadOnlyWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('Update');
      end)

  .Scenario('A deletion is rejected as read-only, naming Delete')
    .Given('a DELETE statement',
      procedure(W: TReadOnlyWorld)
      begin
        W.Query := TQuery.New.From('people').Where('id', '=', 1).AsDelete;
      end)
    .When('it is compiled for the in-memory backend',
      procedure(W: TReadOnlyWorld)
      begin
        W.Outcome := CompileOutcome(W.Query);
      end)
    .&Then('the message names Delete',
      procedure(W: TReadOnlyWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
        Expect(W.Outcome).ToContain('Delete');
      end);

end.
