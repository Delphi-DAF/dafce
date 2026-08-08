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
    .&Then('TQuery.New yields a usable query, because Core registered one at unit init',
      procedure(W: TFactoryWorld)
      begin
        W.FromNew := TQuery.New;
        Expect(W.FromNew <> nil).ToBeTrue;
        Expect(W.FromNew.From('t').Clauses <> nil).ToBeTrue;
      end)

  .Scenario('EitherEntryPointRegistersTheSameSeam')
    .&Then('a factory registered through the deprecated forwarder is the one New uses',
      procedure(W: TFactoryWorld)
      begin
        var Pristine := TQuery.New;          // built by whatever Core registered
        W.Sentinel := TQuery.New;
        try
          {$WARN SYMBOL_DEPRECATED OFF}      // the point of the scenario is to call it
          RegisterIQueryFactory(
            function: IQuery
            begin
              Result := W.Sentinel;
            end);
          {$WARN SYMBOL_DEPRECATED ON}
          W.FromNew := TQuery.New;
          Expect(W.FromNew = W.Sentinel).ToBeTrue;
        finally
          // Restore, or every later spec in this process gets the sentinel.
          TQuery.RegisterFactory(
            function: IQuery
            begin
              Result := Pristine.Clone;
            end);
        end;
      end)
    .&Then('and TQuery.New is serviceable again afterwards',
      procedure(W: TFactoryWorld)
      begin
        var Q := TQuery.New;
        Expect(Q <> W.Sentinel).ToBeTrue;
        Expect(Length(Q.From('t').Clauses)).ToBeGreaterThan(0);
      end)

.Rule('Registering nothing is refused, by both doors')

  .Scenario('UnassignedFactoryIsRefused')
    .&Then('the class method raises rather than leaving New to fail unexplained',
      procedure(W: TFactoryWorld)
      begin
        ExpectException(
          procedure
          begin
            TQuery.RegisterFactory(nil);
          end).ToBe(EInvalidOpException);
      end)
    .&Then('and the forwarder refuses identically, since it is the same code',
      procedure(W: TFactoryWorld)
      begin
        ExpectException(
          procedure
          begin
            {$WARN SYMBOL_DEPRECATED OFF}
            RegisterIQueryFactory(nil);
            {$WARN SYMBOL_DEPRECATED ON}
          end).ToBe(EInvalidOpException);
      end);

end.
