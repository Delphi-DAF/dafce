unit Daf.Extensions.Options;

{
  El contrato del patron de opciones, y nada mas que el contrato.

  Vive en el paquete de contratos de inyeccion de dependencias, y no en el de configuracion, por
  la misma razon por la que en .NET Microsoft.Extensions.Options no depende de
  Microsoft.Extensions.Configuration: unos ajustes son una DEPENDENCIA que un servicio declara,
  y de donde salgan sus valores es asunto de quien compone. Leerlos de un fichero es solo una de
  las formas; fijarlos con codigo es la otra, y ninguna de las dos esta aqui.
}

interface

uses
  System.SysUtils;

type
  /// <summary>
  ///   Unos ajustes ya construidos, validados, y CON DUEÑO.
  /// </summary>
  /// <remarks>
  ///   La contrapartida, que es la trampa del patron en un lenguaje sin recolector: Value
  ///   devuelve un objeto CRUDO, y su dueño es la opcion. Quien se guarde el objeto y suelte la
  ///   opcion se queda con un puntero colgando. Las dos formas correctas de consumirlo son
  ///   sostener la IOptions&lt;T&gt; en un campo, o copiar en el constructor lo que se necesite.
  ///
  ///   Sobre T no se pide mas que ser una clase. Los ajustes son un POCO con {$M+} declarado en
  ///   la interface de una unit, que es lo que el enlazador de configuracion ya exigia: este
  ///   contrato no anade ninguna condicion nueva.
  /// </remarks>
  IOptions<T: class> = interface(IInvokable)
    ['{051FB612-970E-456D-BED8-20AEF5059EAE}']
    function GetValue: T;
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   Valida unos ajustes y dice POR QUE no valen.
  /// </summary>
  /// <remarks>
  ///   Devuelve el motivo por un parametro de salida, y no un Boolean a secas, porque el motivo
  ///   ES el requisito: un Port a cero que aborte diciendo "configuracion invalida" obliga a
  ///   leerse el fichero entero, que es exactamente el trabajo que este patron viene a quitar.
  ///   El texto tiene que nombrar el ajuste que falla.
  /// </remarks>
  TOptionsValidator<T: class> = reference to function(const Value: T; out Error: string): Boolean;

  /// <summary>
  ///   Unos ajustes que no pasaron su validacion. Se levanta al CONSTRUIR el valor --la primera
  ///   vez que se pide-- y no en cada uso: el coste es uno, y la respuesta siempre la misma.
  /// </summary>
  EOptionsValidationError = class(Exception)
  end;

  /// <summary>
  ///   Una configuracion de unos ajustes. Todas las de un mismo tipo se aplican, en orden de
  ///   registro, sobre el mismo objeto.
  /// </summary>
  IConfigureOptions<T: class> = interface(IInvokable)
    ['{7AA31F4F-267D-460B-ADD3-86121083584E}']
    procedure Configure(const Options: T);
  end;

  /// <summary>
  ///   Lo que responde una validacion: que los ajustes valen, que no le tocaba mirarlos, o por que
  ///   no valen, con uno o con varios motivos.
  /// </summary>
  TValidateOptionsResult = record
  strict private
    FSucceeded: Boolean;
    FSkipped: Boolean;
    FFailures: TArray<string>;
    function GetFailed: Boolean;
    function GetFailureMessage: string;
  public
    class function Success: TValidateOptionsResult; static;
    class function Skip: TValidateOptionsResult; static;
    class function Fail(const Failure: string): TValidateOptionsResult; overload; static;
    class function Fail(const Failures: TArray<string>): TValidateOptionsResult; overload; static;
    property Succeeded: Boolean read FSucceeded;
    property Skipped: Boolean read FSkipped;
    property Failed: Boolean read GetFailed;
    property Failures: TArray<string> read FFailures;
    /// <summary>Los motivos, unidos por '; '.</summary>
    property FailureMessage: string read GetFailureMessage;
  end;

  /// <summary>
  ///   Una validacion de unos ajustes. Corren todas las de un mismo tipo, despues de aplicar todas
  ///   las configuraciones, y un fallo reune los motivos de todas.
  /// </summary>
  /// <remarks>
  ///   Name es siempre la cadena vacia: no hay ajustes con nombre.
  /// </remarks>
  IValidateOptions<T: class> = interface(IInvokable)
    ['{254D0A45-5EE2-410E-8C65-0BFFE4233A55}']
    function Validate(const Name: string; const Options: T): TValidateOptionsResult;
  end;

implementation

{ TValidateOptionsResult }

class function TValidateOptionsResult.Success: TValidateOptionsResult;
begin
  Result := Default(TValidateOptionsResult);
  Result.FSucceeded := True;
end;

class function TValidateOptionsResult.Skip: TValidateOptionsResult;
begin
  Result := Default(TValidateOptionsResult);
  Result.FSkipped := True;
end;

class function TValidateOptionsResult.Fail(const Failure: string): TValidateOptionsResult;
begin
  Result := Fail([Failure]);
end;

class function TValidateOptionsResult.Fail(const Failures: TArray<string>): TValidateOptionsResult;
begin
  Result := Default(TValidateOptionsResult);
  Result.FFailures := Failures;
end;

function TValidateOptionsResult.GetFailed: Boolean;
begin
  Result := not (FSucceeded or FSkipped);
end;

function TValidateOptionsResult.GetFailureMessage: string;
begin
  Result := string.Join('; ', FFailures);
end;

end.
