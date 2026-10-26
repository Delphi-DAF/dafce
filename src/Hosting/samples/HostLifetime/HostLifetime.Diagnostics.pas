unit HostLifetime.Diagnostics;

interface

uses
  Daf.Extensions.Hosting;

type
  /// <summary>
  /// Consulta el host mientras existe, con una copia de su referencia. StopAndNil solo vacia la
  /// variable de quien para, asi que la copia se suelta al notificarse ApplicationStopped, que
  /// llega con el host todavia vivo. Tiene que vivir hasta despues de parar el host.
  /// </summary>
  THostDiagnostics = class
  private
    FHost: IHost;
  public
    constructor Create(const Host: IHost);
    function Describe: string;
  end;

implementation

uses
  Daf.Extensions.DependencyInjection;

constructor THostDiagnostics.Create(const Host: IHost);
begin
  inherited Create;
  FHost := Host;
  FHost.Services.GetRequiredService<IHostApplicationLifetime>.ApplicationStopped.Register(
    procedure
    begin
      FHost := nil;
      Writeln('Diagnostico        : suelta su copia del host');
    end);
end;

function THostDiagnostics.Describe: string;
begin
  if Assigned(FHost) then
    Result := 'Diagnostico        : el host de ' +
      FHost.Services.GetRequiredService<IHostEnvironment>.ApplicationName + ' esta vivo'
  else
    Result := 'Diagnostico        : sin host';
end;

end.
