unit MediatRSample.Behaviors;

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.MediatR.Contracts;

type
  // Global behavior: logs every Send call to the Windows debug output.
  // Demonstrates the Handle/Next wrapping pattern for cross-cutting concerns.
  TLoggingBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

implementation

uses
  Winapi.Windows;

function TLoggingBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
var
  Start: Cardinal;
begin
  OutputDebugString(PChar('[MediatR] >> ' + Request.ClassName));
  Start := GetTickCount;
  try
    Result := Next();
    OutputDebugString(PChar(Format('[MediatR] << %s (%d ms)', [Request.ClassName, GetTickCount - Start])));
  except on E: Exception do
  begin
    OutputDebugString(PChar(Format('[MediatR] !! %s raised %s: %s', [Request.ClassName, E.ClassName, E.Message])));
    raise;
  end;
  end;
end;

end.
