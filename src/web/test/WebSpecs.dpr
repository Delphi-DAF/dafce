program WebSpecs;

{
  The module's first suite. It links all six units of Daf.Web, so the runner is
  also the smallest statement of what the package contains.

  STRONGLINKTYPES is not optional here: the router discovers controllers by
  scanning RTTI, and without it the linker drops the spec controllers — which
  nothing references by name — and every routing scenario passes by finding
  nothing to run.
}

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.Extensions.HttpServer,
  Daf.HttpServer,
  Daf.Web.Router,
  Daf.Web.Controller,
  Daf.Web.Render,
  Daf.Web.ActionResult,
  Web.Doubles in 'Web.Doubles.pas',
  Routing.Feat in 'Routing.Feat.pas',
  ActionResults.Feat in 'ActionResults.Feat.pas',
  Server.Feat in 'Server.Feat.pas',
  Daf.Web.Application,
  WebVariant.Feat in 'WebVariant.Feat.pas',
  Binding.Feat in 'Binding.Feat.pas',
  Http.Feat in 'Http.Feat.pas',
  StaticFiles.Feat in 'StaticFiles.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Web')
    .Run;
end.
