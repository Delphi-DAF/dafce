unit HostQuickStart.Worker;

interface

uses
  Daf.Extensions.Hosting;

type
  /// <summary>Tiene algo que parar, asi que es IHostedService y no IStartupTask.</summary>
  TGreetingWorker = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

implementation

procedure TGreetingWorker.Start;
begin
  Writeln('El servicio arranco');
end;

procedure TGreetingWorker.Stop;
begin
  Writeln('El servicio paro');
end;

end.
