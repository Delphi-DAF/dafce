unit Daf.Configuration.Env;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Configuration.ConfigurationProvider,
  Daf.Configuration.Builder;

type
  /// <summary>
  ///   Proveedor de configuración para cargar variables de entorno.
  ///   Permite filtrar por prefijo y reemplazar '__' por ':' para sub-secciones.
  /// </summary>
  TEnvironmentVariablesConfigurationProvider = class(TConfigurationProvider)
  private
    FPrefix: string;
    FConvertDoubleUnderscore: Boolean;
    FStripPrefix: Boolean;
  public
    /// <param name="APrefix">
    ///   Prefijo que deben tener las variables de entorno para ser incluidas (opcional).
    /// </param>
    /// <param name="AConvertDoubleUnderscore">
    ///   Indica si se convierte '__' en ':' para simular sub-secciones (true por defecto).
    /// </param>
    /// <param name="AStripPrefix">
    ///   Si el prefijo se quita de la clave resultante (true por defecto, que es lo que hace
    ///   .NET). En false la clave conserva su nombre completo: es lo que necesita la
    ///   configuracion de host de DAF, donde DAF_APP_ENV se lee con ese mismo nombre venga de
    ///   la variable de entorno o de --DAF_APP_ENV=... en la linea de comandos. Con el prefijo
    ///   quitado las dos vias produciran claves distintas, y una de las dos dejaria de valer.
    /// </param>
    constructor Create(const APrefix: string = ''; AConvertDoubleUnderscore: Boolean = True;
      const AStripPrefix: Boolean = True);

    /// <summary>
    ///   Carga (o recarga) todas las variables de entorno en la propiedad Data.
    ///   Se filtran por FPrefix y se reemplaza '__' por ':' si así se configura.
    /// </summary>
    procedure Load; override;
  end;

  /// <summary>
  ///   Source que crea un TEnvironmentVariablesConfigurationProvider.
  /// </summary>
  TEnvironmentVariablesConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    FPrefix: string;
    FConvertDoubleUnderscore: Boolean;
    FStripPrefix: Boolean;
  public
    constructor Create(const APrefix: string = ''; AConvertDoubleUnderscore: Boolean = True;
      const AStripPrefix: Boolean = True);
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  /// <summary>
  ///   Clase helper para exponer AddEnvironmentVariables en TConfigurationBuilder,
  ///   similar a Microsoft.Extensions.Configuration en .NET.
  /// </summary>
  TEnvironmentVariablesConfigurationBuilderExtensions = class helper for TConfigurationBuilder
  public
    /// <summary>
    ///   Agrega las variables de entorno al builder,
    ///   con posibilidad de filtrar por prefijo (p.ej. 'MyApp_')
    ///   y reemplazar '__' por ':' (para secciones).
    /// </summary>
    function AddEnvironmentVariables(
      const APrefix: string = '';
      const AConvertDoubleUnderscore: Boolean = True
    ): TConfigurationBuilder;
  end;

implementation
uses
  Daf.Types;
{ TEnvironmentVariablesConfigurationProvider }

constructor TEnvironmentVariablesConfigurationProvider.Create(const APrefix: string; AConvertDoubleUnderscore: Boolean;
  const AStripPrefix: Boolean);
begin
  inherited Create;
  FPrefix := APrefix.ToUpper; // Forzamos mayúsculas para filtrar consistentemente
  FConvertDoubleUnderscore := AConvertDoubleUnderscore;
  FStripPrefix := AStripPrefix;
end;

procedure TEnvironmentVariablesConfigurationProvider.Load;
var
  Env: TEnvVars;
  kvp: TPair<string,string>;
  UpperKey, KeyFiltered: string;
begin
  Data.Clear;

  Env := TEnvVars.Create;
  try
    // Recorremos todas las variables
    for kvp in Env.Vars do
    begin
      UpperKey := kvp.Key.ToUpper;

      if (FPrefix <> '') and not UpperKey.StartsWith(FPrefix) then
        Continue;

      KeyFiltered := kvp.Key;
      if (FPrefix <> '') and FStripPrefix then
        KeyFiltered := KeyFiltered.Substring(FPrefix.Length);

      if FConvertDoubleUnderscore then
        KeyFiltered := KeyFiltered.Replace('__', TConfigurationPath.KeyDelimiter, [rfReplaceAll]);

      Data.AddOrSetValue(KeyFiltered, kvp.Value);
    end;
  finally
    Env.Free;
  end;

  OnReload;
end;

{ TEnvironmentVariablesConfigurationSource }

constructor TEnvironmentVariablesConfigurationSource.Create(const APrefix: string; AConvertDoubleUnderscore: Boolean;
  const AStripPrefix: Boolean);
begin
  inherited Create;
  FPrefix := APrefix;
  FConvertDoubleUnderscore := AConvertDoubleUnderscore;
  FStripPrefix := AStripPrefix;
end;

function TEnvironmentVariablesConfigurationSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TEnvironmentVariablesConfigurationProvider.Create(FPrefix, FConvertDoubleUnderscore, FStripPrefix);
end;

{ TEnvironmentVariablesConfigurationBuilderExtensions }

function TEnvironmentVariablesConfigurationBuilderExtensions.AddEnvironmentVariables(
  const APrefix: string;
  const AConvertDoubleUnderscore: Boolean
): TConfigurationBuilder;
var
  Source: IConfigurationSource;
begin
  Source := TEnvironmentVariablesConfigurationSource.Create(APrefix, AConvertDoubleUnderscore);
  Self.Add(Source);
  Result := Self;
end;

end.

