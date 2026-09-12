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

implementation

end.
