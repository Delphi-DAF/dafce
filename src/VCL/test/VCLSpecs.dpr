program VCLSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.VCL.Application,
  SpecForm in 'SpecForm.pas' {SpecForm},
  VclVariant.Feat in 'VclVariant.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('VCL')
    .Run;
end.
