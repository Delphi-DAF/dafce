unit Daf.NNLog.Targets.Debug;

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.NNLog;

type
  TDebugTarget = class(TTarget)
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    constructor Create; override;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;
{$ENDIF}

{ TDebugTarget }

constructor TDebugTarget.Create;
begin
  inherited;

end;

procedure TDebugTarget.DoWrite(const Entry: TLogEntry);
begin
  var Msg := RenderEvent(Entry);
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(Msg));
{$ENDIF}
end;

end.

