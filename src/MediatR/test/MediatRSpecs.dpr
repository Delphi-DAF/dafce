program MediatRSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  MediatR.Feat in 'MediatR.Feat.pas',
  MediatR.Steps in 'MediatR.Steps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('MediatR')
    .Run;
end.
