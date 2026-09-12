unit Daf.Options.Configuration;

{
  La costura entre el patron de opciones y la configuracion.

  Vive en el modulo de configuracion --y no junto al contrato-- porque es la mitad del patron que
  SI conoce de donde salen los valores. Es la separacion que en .NET marcan
  Microsoft.Extensions.Options y ...Options.ConfigurationExtensions, aqui sin paquete nuevo: lo
  unico que cambia es una arista, la de este modulo hacia los contratos de inyeccion de
  dependencias.
}

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Options,
  Daf.Options,
  Daf.Extensions.Configuration,
  Daf.Configuration.Binder;

type
  /// <summary>
  ///   Registrar unos ajustes cuyos valores se leen de una seccion de configuracion.
  /// </summary>
  ConfigOptions = class
  public
    /// <summary>
    ///   Registra IOptions&lt;T&gt; como singleton, llenandolo desde Config. Path es la seccion,
    ///   y la cadena vacia significa la raiz; Option decide si se enlaza por propiedades o por
    ///   campos, igual que en el enlazador. Validate, si se pasa, corre al construir el valor y
    ///   aborta nombrando el ajuste.
    /// </summary>
    /// <remarks>
    ///   La lectura ocurre al CONSTRUIR el valor, no al registrar, y una sola vez: lo que el
    ///   servicio recibe es una foto de la configuracion en ese momento. Sin notificacion de
    ///   cambio en los proveedores no hay forma de saber cuando cambio, asi que releer en cada
    ///   acceso daria a dos lecturas del mismo objeto dos respuestas distintas.
    ///
    ///   Y una sub-seccion NO se deriva de la clase que la contiene: se registra contra su
    ///   propia ruta. Si se registran las dos, el objeto anidado de la clase padre y el de la
    ///   sub-seccion son dos instancias distintas -- igual que en .NET.
    /// </remarks>
    class function Bind<T: class, constructor>(const Services: IServiceCollection;
      const Config: IConfiguration; const Path: string = '';
      const Option: TBindOption = boProperties;
      const Validate: TOptionsValidator<T> = nil): IServiceCollection; static;
  end;

implementation

{ ConfigOptions }

class function ConfigOptions.Bind<T>(const Services: IServiceCollection; const Config: IConfiguration;
  const Path: string; const Option: TBindOption; const Validate: TOptionsValidator<T>): IServiceCollection;
begin
  // Por factory y no por clase, por lo mismo que AppOptions.Configure: el GUID declarado lo
  // comparten todas las instanciaciones, asi que el Supports del camino por clase no comprueba
  // nada entre ellas.
  Result := Services.AddSingleton<IOptions<T>>(
    function(Provider: IServiceProvider): IOptions<T>
    begin
      Result := TOptions<T>.Create(
        function: T
        begin
          // El enlazador construye el objeto y lo devuelve; a partir de aqui el dueño es la
          // opcion. Los miembros de tipo objeto que el enlazador construya dentro los libera el
          // POCO en su destructor: la opcion libera la raiz, y nada mas.
          Result := TConfigurationBinder.Bind<T>(Config, Path, Option);
        end,
        Validate);
    end);
end;

end.
