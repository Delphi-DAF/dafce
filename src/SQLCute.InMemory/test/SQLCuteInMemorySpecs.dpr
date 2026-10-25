program SQLCuteInMemorySpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.Core,
  Daf.SQLCute.InMemory,
  FastPath.Feat in 'FastPath.Feat.pas',
  Projection.Feat in 'Projection.Feat.pas',
  Context.Feat in 'Context.Feat.pas',
  EmptyIn.Feat in 'EmptyIn.Feat.pas',
  Parity.Feat in 'Parity.Feat.pas',
  ReadOnly.Feat in 'ReadOnly.Feat.pas',
  NullableMembers.Feat in 'NullableMembers.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute.InMemory')
    .Run;
end.
