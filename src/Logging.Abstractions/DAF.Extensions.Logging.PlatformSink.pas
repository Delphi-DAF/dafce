unit DAF.Extensions.Logging.PlatformSink;

// Destino del canal de arranque para las plataformas donde Debugger.Write no escribe.
//
// Esta unit NO esta en Daf.Logging.Abstractions.dpk, y no es un descuido: ese es el paquete de
// contratos, lo usa todo el mundo y su restriccion es no depender de nada, asi que meterle
// FireMonkey seria el peor sitio posible. Vive suelta y se enlaza solo donde alguien la nombra --
// que es justo para lo que sirve el registro de destinos del canal:
//
//   BootstrapLog.AddSink(TPlatformLogBootstrapSink.Create);
//
// El coste donde aplica es cero: fuera de Windows una aplicacion Delphi con interfaz es FMX por
// definicion, porque la VCL es solo Windows. En Windows la salida de depuracion ya la da
// TDebugOutputBootstrapSink por OutputDebugString, sin arrastrar FireMonkey a una aplicacion VCL.
//
// Esta unit necesita FireMonkey, y no lo disimula: donde no lo haya para el target, no compila --
// y el error nombra lo que falta. Preferible a un {$IFDEF} por plataforma, que responderia por la
// compilacion lo que IsAvailable tiene que preguntarle al sistema, y que se equivocaria en cuanto
// alguien instalase FMX donde la lista decia que no lo hay (FMXLinux, por ejemplo, se instala por
// GetIt). Quien nombra esta unit tiene FireMonkey; ese es todo el contrato.

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
