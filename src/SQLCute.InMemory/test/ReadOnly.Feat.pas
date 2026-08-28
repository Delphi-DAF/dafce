unit ReadOnly.Feat;

{
  The in-memory backend is read-only, and now says so by the declared statement
  kind, not by spotting a DML clause: a query that is not a retrieval is rejected
  naming the kind it is. That naming is what tells this whitelist-of-one apart
  from the clause blacklist it replaces — the old message named no kind, so a
  statement kind added later would have slipped through unnamed.
}

interface

type
  TReadOnlyWorld = class
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Compiler;   // EInMemoryQueryError

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
    .&Then('compiling an INSERT for the in-memory backend raises the read-only error',
      procedure(W: TReadOnlyWorld)
      begin
        ExpectException(
          procedure
          begin
            TInMemory.Compile<TObject>(
              TQuery.New.From('people').AsInsert(['name'], ['Ann']));
          end).ToBe(EInMemoryQueryError);
      end)
    .&Then('and the message names the kind it received',
      procedure(W: TReadOnlyWorld)
      begin
        ExpectException(
          procedure
          begin
            TInMemory.Compile<TObject>(
              TQuery.New.From('people').AsInsert(['name'], ['Ann']));
          end).ToHaveMessage('Insert');
      end)

  .Scenario('An update is rejected as read-only, naming Update')
    .&Then('the message names Update',
      procedure(W: TReadOnlyWorld)
      begin
        ExpectException(
          procedure
          begin
            TInMemory.Compile<TObject>(
              TQuery.New.From('people').Where('id', '=', 1).AsUpdate(['name'], ['Ann']));
          end).ToHaveMessage('Update');
      end)

  .Scenario('A deletion is rejected as read-only, naming Delete')
    .&Then('the message names Delete',
      procedure(W: TReadOnlyWorld)
      begin
        ExpectException(
          procedure
          begin
            TInMemory.Compile<TObject>(
              TQuery.New.From('people').Where('id', '=', 1).AsDelete);
          end).ToHaveMessage('Delete');
      end);

end.
