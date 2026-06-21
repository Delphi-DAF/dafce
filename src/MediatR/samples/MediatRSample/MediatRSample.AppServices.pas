unit MediatRSample.AppServices;

interface

uses
  System.SysUtils;

type
  IAppLog = interface(IInvokable)
    ['{B2C8A1D4-3E5F-4A7B-9C2D-1E4F5A6B7C8D}']
    procedure Log(const Msg: string);
    procedure SetSink(const Sink: TProc<string>);
  end;

  // Singleton log router — wire the sink from the form after the UI is ready.
  TAppLog = class(TInterfacedObject, IAppLog)
  private
    FSink: TProc<string>;
  public
    procedure Log(const Msg: string);
    procedure SetSink(const Sink: TProc<string>);
  end;

implementation

procedure TAppLog.Log(const Msg: string);
begin
  if Assigned(FSink) then
    FSink(Msg);
end;

procedure TAppLog.SetSink(const Sink: TProc<string>);
begin
  FSink := Sink;
end;

end.
