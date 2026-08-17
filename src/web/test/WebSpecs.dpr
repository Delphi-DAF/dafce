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
  Doubles.Feat in 'Doubles.Feat.pas',
  Routing.Feat in 'Routing.Feat.pas',
  ActionResults.Feat in 'ActionResults.Feat.pas',
  Server.Feat in 'Server.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Web')
    .Run;
end.
