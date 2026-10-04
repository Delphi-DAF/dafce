unit Daf.Extensions.Hosting.Failures;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  /// Los desenlaces que una variante dueña del proceso decide por su cuenta. Los demas los devuelve
  /// el trabajo.
  /// </summary>
  /// <remarks>
  ///   Son contrato con el sistema operativo: quien invoca la aplicacion desde un script o la
  ///   supervisa lee ESTO, y un numero cambiado en silencio rompe a quien lo lee sin romper ninguna
  ///   compilacion.
  /// </remarks>
  TAppExit = record
  public
    const Ok = 0;
    /// <summary>Lo que devuelve la politica por defecto ante una excepcion no capturada.</summary>
    const Failure = 1;
    /// <summary>Ya habia otra instancia, y se declaro que solo puede haber una.</summary>
    const AlreadyRunning = 3;
  end;

  /// <summary>
  /// Lo que la politica recibe de un fallo.
  /// </summary>
  TAppFailure = record
  public
    /// <summary>La excepcion que se escapo.</summary>
    Error: Exception;
    /// <summary>
    /// El contenedor, o nil si no lo hay: antes de que Build lo construya, en una invocacion que se
    /// resolvio sin host, y en la parada, que ya lo apago.
    /// </summary>
    Services: IServiceProvider;
    /// <summary>
    /// El host ya lo informo al ocurrir: un fallo de su construccion, de su arranque o de su parada.
    /// </summary>
    Reported: Boolean;
    constructor Create(const AError: Exception; const AServices: IServiceProvider;
      const AReported: Boolean);
  end;

  /// <summary>
  /// Que hacer con lo que se escapa, y con que codigo termina el proceso.
  /// </summary>
  /// <remarks>
  ///   Es un callback y no un enumerado cerrado porque una suite de aplicaciones tiene su
  ///   propio criterio de codigos de salida, y con un enumerado cada criterio nuevo seria un
  ///   cambio en el framework. Las de siempre estan escritas en <see cref="ExitPolicy"/>.
  /// </remarks>
  TAppExceptionPolicy = reference to function(const Failure: TAppFailure): Integer;

  /// <summary>
  /// Las politicas de siempre, ya escritas. Fachada <c>class</c> y no <c>record</c>: se hereda
  /// para agrupar la de una suite, que es justo lo que un record no deja hacer.
  /// </summary>
  ExitPolicy = class
  public
    /// <summary>Informa por el canal de arranque y devuelve Failure. Lo que el host ya informo no lo
    /// vuelve a informar.</summary>
    class function LogAndFail: TAppExceptionPolicy; virtual;
    /// <summary>Propaga: la excepcion sale del Run y la ve quien invoco.</summary>
    class function Rethrow: TAppExceptionPolicy; virtual;
    /// <summary>Se traga la excepcion y devuelve Failure, sin decir nada.</summary>
    /// <remarks>Existe para que callar sea una DECISION escrita y no un except vacio.</remarks>
    class function Silent: TAppExceptionPolicy; virtual;
  end;

implementation

uses
  Daf.Extensions.Logging;

{ TAppFailure }

constructor TAppFailure.Create(const AError: Exception; const AServices: IServiceProvider;
  const AReported: Boolean);
begin
  Error := AError;
  Services := AServices;
  Reported := AReported;
end;

{ ExitPolicy }

class function ExitPolicy.LogAndFail: TAppExceptionPolicy;
begin
  Result :=
    function(const Failure: TAppFailure): Integer
    begin
      // Por el canal de arranque y no por el pipeline: a estas alturas el proveedor puede estar
      // ya soltandose, o no haber existido nunca, y este informe es lo ultimo que se puede decir.
      if not Failure.Reported then
        BootstrapLog.LoggerFor('ExitPolicy').LogError(Failure.Error,
          'the application ended on an unhandled exception');
      Result := TAppExit.Failure;
    end;
end;

class function ExitPolicy.Rethrow: TAppExceptionPolicy;
begin
  Result :=
    function(const Failure: TAppFailure): Integer
    begin
      // Es la excepcion que el handler de Run esta manejando, y el handler la libera al salir por
      // otra. Sin quitarsela antes, quien la capture fuera recibe memoria ya devuelta.
      AcquireExceptionObject;
      raise Failure.Error at ReturnAddress;
    end;
end;

class function ExitPolicy.Silent: TAppExceptionPolicy;
begin
  Result :=
    function(const Failure: TAppFailure): Integer
    begin
      Result := TAppExit.Failure;
    end;
end;

end.
