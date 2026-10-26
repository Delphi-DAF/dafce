unit StartupReport.Payroll;

interface

uses
  Daf.Extensions.DependencyInjection;

/// <summary>Donde viven los datos de esta aplicacion. Una de verdad lo lee de su configuracion;
/// lo que importa aqui es que la ruta la pone la aplicacion, no el framework.</summary>
function DataFolder: string;

/// <summary>Los registros del dominio, agrupados en una funcion: no necesitan un tipo, asi que no
/// son un modulo. Con <c>--fail</c> aborta aqui, que es lo que el .dpr esta preparado para
/// informar.</summary>
procedure AddPayroll(const Services: IServiceCollection);

implementation

uses
  System.IOUtils,
  System.SysUtils,
  Daf.Extensions.Hosting;

type
  /// <summary>Tiene algo que parar, asi que es IHostedService y no IStartupTask.</summary>
  TPayrollWorker = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

function DataFolder: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'data');
  TDirectory.CreateDirectory(Result);
end;

// El conjunto de FindCmdLineSwitch es de CARACTERES, asi que '--' no cabe en el: se recorren los
// parametros, que ademas es lo que el README documenta.
function PideFallo: Boolean;
begin
  for var I := 1 to ParamCount do
    if SameText(ParamStr(I), '--fail') then
      Exit(True);
  Result := False;
end;

procedure AddPayroll(const Services: IServiceCollection);
begin
  if PideFallo then
    raise EInvalidOpException.Create('la nomina no tiene ejercicio abierto');

  Services.AddHostedService<TPayrollWorker>;
end;

{ TPayrollWorker }

procedure TPayrollWorker.Start;
begin
  Writeln('La nomina esta en marcha');
end;

procedure TPayrollWorker.Stop;
begin
  Writeln('La nomina para');
end;

end.
