unit Daf.FMX.BootstrapSink;

// Destino del canal de arranque para las plataformas donde Debugger.Write no escribe.
//
// Vive en Daf.FMX porque necesita FireMonkey, y esa dependencia es la razon de que el paquete
// exista: Daf.Logging.Abstractions es el paquete de contratos, lo usa todo el mundo y su
// restriccion es no depender de nada, asi que meterle FireMonkey seria el peor sitio posible.
// Quien quiere este destino referencia Daf.FMX y lo registra en el canal:
//
//   BootstrapLog.AddSink(TPlatformLogBootstrapSink.Create);
//
// El coste donde aplica es cero: fuera de Windows una aplicacion Delphi con interfaz es FMX por
// definicion, porque la VCL es solo Windows. En Windows la salida de depuracion ya la da
// TDebugOutputBootstrapSink por OutputDebugString, sin arrastrar FireMonkey a una aplicacion VCL.
//
// Sin {$IFDEF} por plataforma, y a proposito: un IFDEF responderia por la compilacion lo que
// IsAvailable tiene que preguntarle al sistema, y se equivocaria en cuanto alguien instalase FMX
// donde la lista decia que no lo hay (FMXLinux, por ejemplo, se instala por GetIt). Donde no haya
// FireMonkey para el target, esta unit no compila -- y el error nombra lo que falta.

interface

uses
  DAF.Extensions.Logging;

type
  TPlatformLogBootstrapSink = class(TInterfacedObject, IBootstrapSink)
  public
    /// La disponibilidad la responde la plataforma, no un {$IFDEF} que alguien mantiene a mano.
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

implementation

uses
  FMX.Platform;

function TryLoggingService(out Service: IFMXLoggingService): Boolean;
begin
  Result := False;
  if TPlatformServices.Current = nil then Exit;
  Result := TPlatformServices.Current.SupportsPlatformService(IFMXLoggingService, IInterface(Service));
end;

{ TPlatformLogBootstrapSink }

function TPlatformLogBootstrapSink.IsAvailable: Boolean;
begin
  var Service: IFMXLoggingService;
  Result := TryLoggingService(Service);
end;

procedure TPlatformLogBootstrapSink.Write(const Line: string);
begin
  var Service: IFMXLoggingService;
  if TryLoggingService(Service) then
    Service.Log('%s', [Line]);
end;

end.
