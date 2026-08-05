program QueryExpressionsSpecs;

{
  The runner that PROVES Decision 6: it links the domain half only. If
  Daf.Query.Expressions ever acquires a SQLCute dependency, this project stops
  compiling — the isolation is a build fact, not a comment.
}

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.Query.Expressions,
  Model.Feat in 'Model.Feat.pas',
  Authoring.Feat in 'Authoring.Feat.pas',
  Metamodel.Feat in 'Metamodel.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Query.Expressions')
    .Run;
end.
