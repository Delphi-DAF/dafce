unit Daf.NNLog.Targets.Probe;

// Un destino de prueba, con el nombre que la convención del loader espera para encontrarlo por
// RTTI: Daf.NNLog.Targets.<Tipo>.T<Tipo>Target. Con targetType "Probe" el loader lo construye
// como a cualquier otro, así que las specs recorren el camino real —bind, construcción de reglas,
// creación del logger— y no un atajo.
//
// En vez de escribir a ningún sitio, anota lo que recibe: por cada entrada, el nombre del propio
// destino y la categoría y el nivel de la línea. Es la única forma de afirmar QUÉ llegó cuando los
// dos defectos que se arreglan fallan en silencio y en direcciones opuestas —uno no escribe, el
// otro escribe de más—: «se escribió» y «no se lanzó» los cumple también el código roto.

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.NNLog;

type
  TProbeRecorder = class
  strict private
    FLines: TArray<string>;
  public
    procedure Clear;
    procedure Took(const TargetName, Category: string; const Level: TLogLevel);
    function Count: Integer;
    function CategoriesFor(const TargetName: string): TArray<string>;
    function ReceivedFrom(const TargetName, Category: string): Boolean;
    function Text: string;
  end;

  /// Sus dos propiedades las rellena el loader desde la configuración. MaxSize arranca en un valor
  /// que no es el cero de su tipo, para que «conservó su valor» y «se escribió el cero» no coincidan.
  TProbeTarget = class(TTarget)
  private
    FMaxSize: Int64;
    FRatio: Double;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    /// El último destino de prueba construido, mientras vive: el escenario solo tiene el proveedor.
    class var Last: TProbeTarget;
    constructor Create; override;
    destructor Destroy; override;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
    property Ratio: Double read FRatio write FRatio;
  end;

// Grabador global, de proceso: cada escenario lo limpia al empezar (TRuleWorld.Create). Es global
// porque el destino lo construye el loader por RTTI y el escenario solo tiene el proveedor; no hay
// por dónde pasarle una referencia.
function NNLogProbe: TProbeRecorder;

implementation

var
  GProbe: TProbeRecorder = nil;

function NNLogProbe: TProbeRecorder;
begin
  if GProbe = nil then
    GProbe := TProbeRecorder.Create;
  Result := GProbe;
end;

{ TProbeRecorder }

procedure TProbeRecorder.Clear;
begin
  FLines := nil;
end;

procedure TProbeRecorder.Took(const TargetName, Category: string; const Level: TLogLevel);
begin
  FLines := FLines + [Format('%s|%s|%s', [TargetName, Category, Level.ToString])];
end;

function TProbeRecorder.Count: Integer;
begin
  Result := Length(FLines);
end;

function TProbeRecorder.CategoriesFor(const TargetName: string): TArray<string>;
begin
  SetLength(Result, 0);
  for var Line in FLines do
    if Line.StartsWith(TargetName + '|') then
    begin
      var Parts := Line.Split(['|']);
      Result := Result + [Parts[1]];
    end;
end;

function TProbeRecorder.ReceivedFrom(const TargetName, Category: string): Boolean;
begin
  Result := False;
  for var C in CategoriesFor(TargetName) do
    if C = Category then
      Exit(True);
end;

function TProbeRecorder.Text: string;
begin
  Result := string.Join(sLineBreak, FLines);
end;

{ TProbeTarget }

constructor TProbeTarget.Create;
begin
  inherited Create;
  FMaxSize := 1024;
  Last := Self;
end;

destructor TProbeTarget.Destroy;
begin
  if Last = Self then
    Last := nil;
  inherited;
end;

procedure TProbeTarget.DoWrite(const Entry: TLogEntry);
begin
  NNLogProbe.Took(Self.Name, Entry.Category, Entry.Level);
end;

initialization

finalization
  FreeAndNil(GProbe);

end.
