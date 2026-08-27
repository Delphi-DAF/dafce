unit Daf.Options;

{
  La implementacion del contrato de opciones, y la fachada de registro POR CODIGO.

  Aqui no se nombra la configuracion. La otra mitad del patron --llenar unos ajustes desde una
  seccion de un fichero-- vive en el modulo Configuration, que es el que sabe leerla.

  Delphi no tiene funciones libres genericas, asi que la fachada es un metodo de clase; y como
  una clase no se puede ampliar desde otra unit, las dos mitades son dos fachadas distintas. Cada
  nombre dice de donde salen los valores, que es lo unico que las separa: AppOptions los recibe de
  codigo, ConfigOptions los lee de una seccion.
}

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Options;

type
  /// <summary>
  ///   La opcion: construye el valor la primera vez que se pide, lo guarda, y lo libera al
  ///   destruirse. Registrada como singleton, eso quiere decir que el valor vive hasta que se
  ///   cierra el contenedor.
  /// </summary>
  /// <remarks>
  ///   Perezosa porque componer no es arrancar: registrar unos ajustes que la ejecucion no llega
  ///   a usar no debe leer un fichero ni validar nada. Y una sola vez porque el valor es un
  ///   objeto mutable, y dos lecturas que devolvieran objetos distintos harian que escribir en el
  ///   significase cosas distintas segun quien preguntara.
  ///
  ///   El delegado de construccion, y no un T.Create aqui dentro, es lo que permite que la misma
  ///   clase sirva a unos ajustes fijados con codigo y a unos leidos de una seccion: lo que
  ///   cambia entre las dos mitades del patron es de donde sale el objeto, no que se hace con el.
  /// </remarks>
  TOptions<T: class> = class(TInterfacedObject, IOptions<T>)
  strict private
    FBuild: TFunc<T>;
    FValidate: TOptionsValidator<T>;
    FValue: T;
    FBuilt: Boolean;
    procedure Check(const Value: T);
  public
    constructor Create(const ABuild: TFunc<T>; const AValidate: TOptionsValidator<T> = nil);
    destructor Destroy; override;
    function GetValue: T;
  end;

  /// <summary>
  ///   Registrar unos ajustes cuyos valores pone el codigo, sin configuracion de por medio.
  /// </summary>
  AppOptions = class
  public
    /// <summary>
    ///   Registra IOptions&lt;T&gt; como singleton. Setup recibe el objeto recien construido para
    ///   rellenarlo; Validate, si se pasa, corre al construirlo y aborta nombrando el ajuste.
    /// </summary>
    class function Configure<T: class, constructor>(const Services: IServiceCollection;
      const Setup: TProc<T>; const Validate: TOptionsValidator<T> = nil): IServiceCollection; static;
  end;

implementation

{ TOptions<T> }

constructor TOptions<T>.Create(const ABuild: TFunc<T>; const AValidate: TOptionsValidator<T>);
begin
  inherited Create;
  FBuild := ABuild;
  FValidate := AValidate;
end;

destructor TOptions<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TOptions<T>.Check(const Value: T);
begin
  if not Assigned(FValidate) then Exit;

  var Error := '';
  if FValidate(Value, Error) then Exit;

  raise EOptionsValidationError.CreateFmt('Invalid options %s: %s', [Value.ClassName, Error]);
end;

function TOptions<T>.GetValue: T;
begin
  if not FBuilt then
  begin
    var Value := FBuild();
    // Validado ANTES de guardarse, y liberado aqui si no pasa: unos ajustes invalidos no se
    // quedan en el campo, asi que no hay puerta de atras por la que leerlos, y tampoco se fugan
    // esperando al destructor de una opcion que ya nadie va a usar.
    try
      Check(Value);
    except
      Value.Free;
      raise;
    end;
    FValue := Value;
    FBuilt := True;
  end;
  Result := FValue;
end;

{ AppOptions }

class function AppOptions.Configure<T>(const Services: IServiceCollection; const Setup: TProc<T>;
  const Validate: TOptionsValidator<T>): IServiceCollection;
begin
  // SIEMPRE por factory, nunca por clase, y no es una preferencia de estilo. Todas las
  // instanciaciones de una interfaz generica comparten el GUID que se declaro una vez, asi que
  // el Supports en el que se apoya el registro por clase no comprueba nada entre ellas: daria
  // verde a AddSingleton<IOptions<TA>, TOptions<TB>>. El camino que no se pisa no engana.
  Result := Services.AddSingleton<IOptions<T>>(
    function(Provider: IServiceProvider): IOptions<T>
    begin
      Result := TOptions<T>.Create(
        function: T
        begin
          Result := T.Create;
          try
            if Assigned(Setup) then
              Setup(Result);
          except
            Result.Free;
            raise;
          end;
        end,
        Validate);
    end);
end;

end.
