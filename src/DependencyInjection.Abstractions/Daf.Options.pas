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
  ///   Construir es crear T, aplicarle todas las configuraciones en orden de registro y correr
  ///   todas las validaciones: un fallo reune los motivos de todas en un solo error.
  /// </remarks>
  TOptions<T: class, constructor> = class(TInterfacedObject, IOptions<T>)
  strict private
    FConfigures: TArray<IConfigureOptions<T>>;
    FValidators: TArray<IValidateOptions<T>>;
    FValue: T;
    FBuilt: Boolean;
    function Build: T;
    procedure Check(const Value: T);
  public
    constructor Create(const AConfigures: TArray<IConfigureOptions<T>>;
      const AValidators: TArray<IValidateOptions<T>>);
    destructor Destroy; override;
    function GetValue: T;
    /// <summary>
    ///   Registra IOptions&lt;T&gt; como singleton si nadie lo ha registrado: el que reune las
    ///   configuraciones y las validaciones de T que haya en el contenedor.
    /// </summary>
    class function TryAdd(const Services: IServiceCollection): IServiceCollection; static;
  end;

  /// <summary>Una configuracion que es un delegado sobre el objeto.</summary>
  TConfigureOptions<T: class> = class(TInterfacedObject, IConfigureOptions<T>)
  strict private
    FAction: TProc<T>;
  public
    constructor Create(const AAction: TProc<T>);
    procedure Configure(const Options: T);
  end;

  /// <summary>Una validacion que es un TOptionsValidator&lt;T&gt;.</summary>
  TValidateOptions<T: class> = class(TInterfacedObject, IValidateOptions<T>)
  strict private
    FValidate: TOptionsValidator<T>;
  public
    constructor Create(const AValidate: TOptionsValidator<T>);
    function Validate(const Name: string; const Options: T): TValidateOptionsResult;
  end;

  /// <summary>
  ///   Registrar unos ajustes cuyos valores pone el codigo, sin configuracion de por medio.
  /// </summary>
  AppOptions = class
  public
    /// <summary>
    ///   Registra una configuracion de T y, si nadie lo ha hecho, IOptions&lt;T&gt; como singleton.
    ///   Setup rellena el objeto en su turno, despues de las configuraciones registradas antes;
    ///   Validate, si se pasa, corre con las demas validaciones al construirlo y aborta nombrando
    ///   el ajuste.
    /// </summary>
    class function Configure<T: class, constructor>(const Services: IServiceCollection;
      const Setup: TProc<T>; const Validate: TOptionsValidator<T> = nil): IServiceCollection; static;
  end;

implementation

{ TOptions<T> }

constructor TOptions<T>.Create(const AConfigures: TArray<IConfigureOptions<T>>;
  const AValidators: TArray<IValidateOptions<T>>);
begin
  inherited Create;
  FConfigures := AConfigures;
  FValidators := AValidators;
end;

destructor TOptions<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TOptions<T>.Check(const Value: T);
begin
  var Failures: TArray<string> := [];
  for var Validator in FValidators do
  begin
    var Outcome := Validator.Validate('', Value);
    if Outcome.Failed then
      Failures := Failures + Outcome.Failures;
  end;
  if Length(Failures) = 0 then Exit;

  raise EOptionsValidationError.CreateFmt('Invalid options %s: %s',
    [Value.ClassName, string.Join('; ', Failures)]);
end;

function TOptions<T>.Build: T;
begin
  // Validado ANTES de guardarse, y liberado aqui si no pasa: unos ajustes invalidos no se quedan
  // en el campo, asi que no hay puerta de atras por la que leerlos, y tampoco se fugan esperando
  // al destructor de una opcion que ya nadie va a usar.
  Result := T.Create;
  try
    for var Configure in FConfigures do
      Configure.Configure(Result);
    Check(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TOptions<T>.GetValue: T;
begin
  // Comprobar y construir bajo el mismo cerrojo: dos primeras lecturas a la vez construyen,
  // configuran y validan una sola vez, y las dos reciben el mismo objeto.
  TMonitor.Enter(Self);
  try
    if not FBuilt then
    begin
      FValue := Build;
      FBuilt := True;
    end;
    Result := FValue;
  finally
    TMonitor.Exit(Self);
  end;
end;

class function TOptions<T>.TryAdd(const Services: IServiceCollection): IServiceCollection;
begin
  // SIEMPRE por factory, nunca por clase, y no es una preferencia de estilo. Todas las
  // instanciaciones de una interfaz generica comparten el GUID que se declaro una vez, asi que
  // el Supports en el que se apoya el registro por clase no comprueba nada entre ellas: daria
  // verde a AddSingleton<IOptions<TA>, TOptions<TB>>. El camino que no se pisa no engana.
  Result := Services.TryAddSingleton<IOptions<T>>(
    function(Provider: IServiceProvider): IOptions<T>
    begin
      var Configures: TArray<IConfigureOptions<T>> := [];
      for var Configure in Provider.GetServices<IConfigureOptions<T>> do
        Configures := Configures + [Configure];
      var Validators: TArray<IValidateOptions<T>> := [];
      for var Validator in Provider.GetServices<IValidateOptions<T>> do
        Validators := Validators + [Validator];
      Result := TOptions<T>.Create(Configures, Validators);
    end);
end;

{ TConfigureOptions<T> }

constructor TConfigureOptions<T>.Create(const AAction: TProc<T>);
begin
  inherited Create;
  FAction := AAction;
end;

procedure TConfigureOptions<T>.Configure(const Options: T);
begin
  if Assigned(FAction) then
    FAction(Options);
end;

{ TValidateOptions<T> }

constructor TValidateOptions<T>.Create(const AValidate: TOptionsValidator<T>);
begin
  inherited Create;
  FValidate := AValidate;
end;

function TValidateOptions<T>.Validate(const Name: string; const Options: T): TValidateOptionsResult;
begin
  var Error := '';
  if FValidate(Options, Error) then
    Result := TValidateOptionsResult.Success
  else
    Result := TValidateOptionsResult.Fail(Error);
end;

{ AppOptions }

class function AppOptions.Configure<T>(const Services: IServiceCollection; const Setup: TProc<T>;
  const Validate: TOptionsValidator<T>): IServiceCollection;
begin
  Services.AddSingleton<IConfigureOptions<T>>(
    function(Provider: IServiceProvider): IConfigureOptions<T>
    begin
      Result := TConfigureOptions<T>.Create(Setup);
    end);
  if Assigned(Validate) then
    Services.AddSingleton<IValidateOptions<T>>(
      function(Provider: IServiceProvider): IValidateOptions<T>
      begin
        Result := TValidateOptions<T>.Create(Validate);
      end);
  Result := TOptions<T>.TryAdd(Services);
end;

end.
