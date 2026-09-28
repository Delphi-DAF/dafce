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
  public
    /// <param name="APrefix">
    ///   Prefijo que deben tener las variables de entorno para ser incluidas (opcional). Se
    ///   quita de la clave, como en .NET.
    /// </param>
    /// <param name="AConvertDoubleUnderscore">
    ///   Indica si se convierte '__' en ':' para simular sub-secciones (true por defecto).
    /// </param>
    constructor Create(const APrefix: string = ''; AConvertDoubleUnderscore: Boolean = True);

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
  public
    constructor Create(const APrefix: string = ''; AConvertDoubleUnderscore: Boolean = True);
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

constructor TEnvironmentVariablesConfigurationProvider.Create(const APrefix: string; AConvertDoubleUnderscore: Boolean);
begin
  inherited Create;
  FPrefix := APrefix.ToUpper; // Forzamos mayúsculas para filtrar consistentemente
  FConvertDoubleUnderscore := AConvertDoubleUnderscore;
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
      if FPrefix <> '' then
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

constructor TEnvironmentVariablesConfigurationSource.Create(const APrefix: string; AConvertDoubleUnderscore: Boolean);
begin
  inherited Create;
  FPrefix := APrefix;
  FConvertDoubleUnderscore := AConvertDoubleUnderscore;
end;

function TEnvironmentVariablesConfigurationSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TEnvironmentVariablesConfigurationProvider.Create(FPrefix, FConvertDoubleUnderscore);
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

