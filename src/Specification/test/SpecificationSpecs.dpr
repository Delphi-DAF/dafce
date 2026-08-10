program SpecificationSpecs;

{
  The runner that PROVES Decision 6: it links the domain half only. If
  Daf.Specification ever acquires a SQLCute dependency, this project stops
  compiling — the isolation is a build fact, not a comment.
}

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.Specification,
  Model.Feat in 'Model.Feat.pas',
  Authoring.Feat in 'Authoring.Feat.pas',
  Metamodel.Feat in 'Metamodel.Feat.pas',
  Specification.Feat in 'Specification.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Specification')
    .Run;
end.
