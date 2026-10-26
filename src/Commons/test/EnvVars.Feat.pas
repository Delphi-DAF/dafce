unit EnvVars.Feat;

// Cada escenario fija DafEnvProbe, un nombre que ningun entorno real usa, y el World la deja como
// estaba al destruirse: con su valor anterior, o sin ella.

interface

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib,
{$ENDIF POSIX}
  System.SysUtils,
  Daf.Types,
  Daf.MiniSpec;

const
  Probe = 'DafEnvProbe';

function ReadProcessVar(const Name: string; out Value: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := Winapi.Windows.GetEnvironmentVariable(PChar(Name), nil, 0) > 0;
  Value := System.SysUtils.GetEnvironmentVariable(Name);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  var Raw := getenv(MarshaledAString(UTF8String(Name)));
  Result := Raw <> nil;
  Value := UTF8ToString(Raw);
{$ENDIF POSIX}
end;

procedure WriteProcessVar(const Name, Value: string);
begin
{$IFDEF MSWINDOWS}
  Winapi.Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  setenv(MarshaledAString(UTF8String(Name)), MarshaledAString(UTF8String(Value)), 1);
{$ENDIF POSIX}
end;

procedure RemoveProcessVar(const Name: string);
begin
{$IFDEF MSWINDOWS}
  Winapi.Windows.SetEnvironmentVariable(PChar(Name), nil);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  unsetenv(MarshaledAString(UTF8String(Name)));
{$ENDIF POSIX}
end;

type
  TEnvVarsWorld = class
  strict private
    FProbeTouched: Boolean;
    FHadProbe: Boolean;
    FPriorProbe: string;
  public
    Env: TEnvVars;
    /// Lo que devolvio la busqueda del When: `found=<valor>` o `missing`.
    Lookup: string;
    constructor Create;
    destructor Destroy; override;
    procedure SetProbe(const Value: string);
    /// La clave tal como quedo guardada, o vacia si no esta.
    function ProbeKey: string;
    function ProbeValue: string;
  end;

{ TEnvVarsWorld }

constructor TEnvVarsWorld.Create;
begin
  inherited;
  Env := TEnvVars.Create;
end;

destructor TEnvVarsWorld.Destroy;
begin
  if FProbeTouched then
  begin
    if FHadProbe then
      WriteProcessVar(Probe, FPriorProbe)
    else
      RemoveProcessVar(Probe);
  end;
  Env.Free;
  inherited;
end;

procedure TEnvVarsWorld.SetProbe(const Value: string);
begin
  if not FProbeTouched then
    FHadProbe := ReadProcessVar(Probe, FPriorProbe);
  FProbeTouched := True;
  WriteProcessVar(Probe, Value);
end;

function TEnvVarsWorld.ProbeKey: string;
begin
  Result := '';
  for var Key in Env.Vars.Keys do
    if SameText(Key, Probe) then
      Exit(Key);
end;

function TEnvVarsWorld.ProbeValue: string;
begin
  Result := '';
  for var Pair in Env.Vars do
    if SameText(Pair.Key, Probe) then
      Exit(Pair.Value);
end;

initialization

Feature('''
Feature Las variables de entorno del proceso @commons @envvars

  Como quien configura una aplicación con variables de entorno
  Quiero leerlas con la misma semántica en todas las plataformas
  Para que una clave se encuentre igual se escriba como se escriba
''')

.UseWorld<TEnvVarsWorld>

.Rule('Las variables del proceso se leen con la clave en mayúsculas')

  .Scenario('Una variable del proceso aparece con su clave en mayúsculas')
    .Given('una variable DafEnvProbe con valor uno, fijada en el proceso',
      procedure(W: TEnvVarsWorld)
      begin
        W.SetProbe('uno');
      end)
    .When('se lee el entorno',
      procedure(W: TEnvVarsWorld)
      begin
        W.Env.Refresh;
      end)
    .&Then('la variable aparece con la clave DAFENVPROBE',
      procedure(W: TEnvVarsWorld)
      begin
        Expect(W.ProbeKey).ToEqual('DAFENVPROBE');
      end)
    .&And('su valor es uno',
      procedure(W: TEnvVarsWorld)
      begin
        Expect(W.ProbeValue).ToEqual('uno');
      end)

  .Scenario('La clave se encuentra sin distinguir mayúsculas')
    .Given('el entorno leído con la variable DafEnvProbe de valor uno',
      procedure(W: TEnvVarsWorld)
      begin
        W.SetProbe('uno');
        W.Env.Refresh;
      end)
    .When('se busca la clave dafenvprobe',
      procedure(W: TEnvVarsWorld)
      begin
        var Value: string;
        if W.Env.Vars.TryGetValue('dafenvprobe', Value) then
          W.Lookup := 'found=' + Value
        else
          W.Lookup := 'missing';
      end)
    .&Then('se encuentra, con el valor uno',
      procedure(W: TEnvVarsWorld)
      begin
        Expect(W.Lookup).ToEqual('found=uno');
      end)

  .Scenario('Un igual dentro del valor no lo corta')
    .Given('una variable DafEnvProbe con valor a=b, fijada en el proceso',
      procedure(W: TEnvVarsWorld)
      begin
        W.SetProbe('a=b');
      end)
    .When('se lee el entorno',
      procedure(W: TEnvVarsWorld)
      begin
        W.Env.Refresh;
      end)
    .&Then('su valor es a=b',
      procedure(W: TEnvVarsWorld)
      begin
        Expect(W.ProbeValue).ToEqual('a=b');
      end)

  .Scenario('Volver a leer recoge lo que cambió')
    .Given('el entorno ya leído, y después DafEnvProbe fijada en el proceso',
      procedure(W: TEnvVarsWorld)
      begin
        W.Env.Refresh;
        Expect(W.ProbeKey).ToEqual('');
        W.SetProbe('nueva');
      end)
    .When('se vuelve a leer el entorno',
      procedure(W: TEnvVarsWorld)
      begin
        W.Env.Refresh;
      end)
    .&Then('la variable nueva aparece',
      procedure(W: TEnvVarsWorld)
      begin
        Expect(W.ProbeValue).ToEqual('nueva');
      end);

end.
