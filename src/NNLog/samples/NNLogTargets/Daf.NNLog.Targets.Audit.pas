unit Daf.NNLog.Targets.Audit;

{
  Un destino propio. El nombre de la unit y el de la clase NO son estilo: el cargador
  resuelve un targetType por RTTI a Daf.NNLog.Targets.<Tipo>.T<Tipo>Target, asi que
  "Audit" en la configuracion solo encuentra a esta clase si la unit se llama asi.
  Y distingue mayusculas de minusculas.
}

interface

uses
  Daf.Extensions.Logging,
  Daf.NNLog;

type
  TAuditTarget = class(TTarget)
  private
    FPrefix: string;
  protected
    /// Lo que implementa un destino es DoWrite, no Write. Write no es virtual: toma un
    /// candado propio de esta instancia y luego llama aqui, asi que dos hilos que
    /// escriban por este destino no se solapan sin que el destino haga nada. Y si esto
    /// lanza, quien registraba no se entera: el fallo sale por el canal de arranque.
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    /// El interruptor del sample. De clase y no de instancia porque el destino lo
    /// construye el cargador, y quien escribe el sample no tiene la instancia.
    class var FailOnce: Boolean;
    constructor Create; override;
    /// Cualquier clave de la seccion del destino que sea propiedad escribible de la
    /// clase se asigna al cargarla. Esta llega de la configuracion como texto.
    property Prefix: string read FPrefix write FPrefix;
  end;

implementation

uses
  System.SysUtils;

{ TAuditTarget }

constructor TAuditTarget.Create;
begin
  inherited Create;
  FPrefix := 'audit';
end;

procedure TAuditTarget.DoWrite(const Entry: TLogEntry);
begin
  if FailOnce then
  begin
    FailOnce := False;
    raise Exception.Create('el almacen de auditoria no responde');
  end;
  Writeln('  [', FPrefix, '] ', RenderEvent(Entry));
end;

end.
