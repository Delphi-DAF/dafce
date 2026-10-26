unit SQLCute.Factory.Feat;

{
  The factory seam: TQuery.New returns an implementation this package cannot name,
  so something has to register one. Registration and creation are two halves of
  one thing and live on TQuery.

  The free RegisterIQueryFactory is kept because it shipped in v1.6.0 — a consumer
  who registered an IQuery of their own calls it — so it forwards, and these specs
  are what say the two entry points are indistinguishable rather than merely
  similar.

  Swapping the factory is global state, so the scenario that does it restores the
  behaviour afterwards. The restore registers a factory returning a clone of a
  query built by the real one: TQueryImpl is implementation-local to
  Daf.SQLCute.Core and cannot be named from here, and a clone of an empty query is
  an empty query. Without the restore every later spec in the process would build
  the sentinel instead.
}

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute;

type
  TFactoryWorld = class
  public
    /// The query the sentinel factory handed out, and what TQuery.New returned.
    Sentinel: IQuery;
    FromNew: IQuery;
    /// Built by whatever Core registered, and kept to restore the seam with.
    Pristine: IQuery;
    Shaped: IQuery;
    AfterRestore: IQuery;
    AfterShaped: IQuery;
    /// What each door answered when it was handed nothing to register.
    Outcome: string;
    OtherOutcome: string;
  end;

initialization

Feature('''
Feature The IQuery factory seam @unit @sqlcute @factory

  As the abstractions package, which cannot name an implementation
  I want registration and creation to live on one type
  So the seam is reachable from where it is used, and what shipped keeps working
''')

.UseWorld<TFactoryWorld>

.Rule('The registered factory is what TQuery.New returns')

  .Scenario('CoreRegisteredThroughTQuery')
    // Nothing to set up: what the scenario measures is the registration Core did at unit init,
    // and the only way to ask whether it happened is to ask for a query.
    .Given('the factory seam as Core left it at unit init').NoAction
    .When('a query is asked for through TQuery.New, and given a table',
      procedure(W: TFactoryWorld)
      begin
        W.FromNew := TQuery.New;
        W.Shaped := W.FromNew.From('t');
      end)
    .&Then('TQuery.New yields a usable query, because Core registered one at unit init',
      procedure(W: TFactoryWorld)
      begin
        Expect(W.FromNew <> nil).ToBeTrue;
        Expect(W.Shaped.Clauses <> nil).ToBeTrue;
      end)

  .Scenario('EitherEntryPointRegistersTheSameSeam')
    .Given('a sentinel query to hand out, and a pristine one to restore the seam with',
      procedure(W: TFactoryWorld)
      begin
        W.Pristine := TQuery.New;            // built by whatever Core registered
        W.Sentinel := TQuery.New;
      end)
    .When('a factory goes in through the deprecated forwarder, and the seam is restored after',
      procedure(W: TFactoryWorld)
      begin
        // A local and not W.Pristine: the restore factory stays registered for the rest of the
        // process, so a closure over the World would outlive the World. Over a local, what it
        // keeps alive is the interface reference.
        var Pristine := W.Pristine;
        try
          {$WARN SYMBOL_DEPRECATED OFF}      // the point of the scenario is to call it
          RegisterIQueryFactory(
            function: IQuery
            begin
              Result := W.Sentinel;
            end);
          {$WARN SYMBOL_DEPRECATED ON}
          W.FromNew := TQuery.New;
        finally
          // Restore, or every later spec in this process gets the sentinel.
          TQuery.RegisterFactory(
            function: IQuery
            begin
              Result := Pristine.Clone;
            end);
        end;
        W.AfterRestore := TQuery.New;
        W.AfterShaped := W.AfterRestore.From('t');
      end)
    .&Then('a factory registered through the deprecated forwarder is the one New uses',
      procedure(W: TFactoryWorld)
      begin
        Expect(W.FromNew = W.Sentinel).ToBeTrue;
      end)
    .&Then('and TQuery.New is serviceable again afterwards',
      procedure(W: TFactoryWorld)
      begin
        Expect(W.AfterRestore <> W.Sentinel).ToBeTrue;
        Expect(Length(W.AfterShaped.Clauses)).ToBeGreaterThan(0);
      end)

.Rule('Registering nothing is refused, by both doors')

  .Scenario('UnassignedFactoryIsRefused')
    // Nothing to set up: "nothing registered" is the argument the two doors are handed, not a
    // state that can be prepared beforehand.
    .Given('nothing to register, and the two doors that take it').NoAction
    .When('nil goes in through each door',
      procedure(W: TFactoryWorld)
      begin
        try
          TQuery.RegisterFactory(nil);
          W.Outcome := '<the class method accepted nil>';
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;

        try
          {$WARN SYMBOL_DEPRECATED OFF}
          RegisterIQueryFactory(nil);
          {$WARN SYMBOL_DEPRECATED ON}
          W.OtherOutcome := '<the forwarder accepted nil>';
        except
          on E: Exception do
            W.OtherOutcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the class method raises rather than leaving New to fail unexplained',
      procedure(W: TFactoryWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInvalidOpException');
      end)
    .&Then('and the forwarder refuses identically, since it is the same code',
      procedure(W: TFactoryWorld)
      begin
        Expect(W.OtherOutcome).ToEqual(W.Outcome);
      end);

end.
