program DISpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  DependencyInjection.Feat in 'DependencyInjection.Feat.pas',
  DependencyInjection.Steps in 'DependencyInjection.Steps.pas',
  Doubles.Feat in 'Doubles.Feat.pas',
  Composition.Feat in 'Composition.Feat.pas',
  Decoration.Feat in 'Decoration.Feat.pas',
  Modules.Feat in 'Modules.Feat.pas',
  RestrictedRtti.Feat in 'RestrictedRtti.Feat.pas',
  Validation.Feat in 'Validation.Feat.pas',
  GarbageCollection.Feat in 'GarbageCollection.Feat.pas',
  ContainerShutdown.Feat in 'ContainerShutdown.Feat.pas',
  Concurrency.Feat in 'Concurrency.Feat.pas',
  ServiceIdentity.Feat in 'ServiceIdentity.Feat.pas',
  RequestedType.Feat in 'RequestedType.Feat.pas',
  OpenGenerics.Feat in 'OpenGenerics.Feat.pas',
  Options.Feat in 'Options.Feat.pas',
  ScopeLifetime.Feat in 'ScopeLifetime.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('DependencyInjection')
    .Run;
end.
