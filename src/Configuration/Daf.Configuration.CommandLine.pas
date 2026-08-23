unit Daf.Configuration.CommandLine;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Configuration.ConfigurationProvider,
  Daf.Configuration.Builder;

type
  /// <summary>
  ///   Alias corto de un argumento y la clave de configuracion a la que aterriza.
  ///   El alias debe empezar por uno o dos guiones.
  /// </summary>
  TSwitchMapping = TPair<string, string>;
  TSwitchMappings = TArray<TSwitchMapping>;

  CommandLineConfigError = class(Exception);

  /// <summary>
  ///   Lee los argumentos del proceso como claves de configuracion, con la sintaxis del
  ///   proveedor equivalente de .NET:
  ///   <code>
  ///   clave=valor        --clave=valor      --clave valor
  ///                      /clave=valor       /clave valor
  ///   </code>
  ///   Un argumento de un solo guion solo se lee si tiene alias declarado.
  ///
  ///   El proveedor NO interpreta el separador de seccion: entrega la clave tal y como
  ///   viene escrita, de modo que <c>--Logging:LogLevel:Default=Debug</c> aterriza bajo
  ///   la clave literal y es IConfiguration quien la secciona.
  /// </summary>
  TCommandLineConfigurationProvider = class(TConfigurationProvider)
  private
    FArgs: TArray<string>;
    FSwitchMappings: TDictionary<string, string>;
  public
    constructor Create(const Args: TArray<string>; const SwitchMappings: TSwitchMappings);
    destructor Destroy; override;
    procedure Load; override;
  end;

  TCommandLineConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    FArgs: TArray<string>;
    FSwitchMappings: TSwitchMappings;
  public
    /// <summary>
    ///   Valida los alias al montar la fuente: declarar el mismo dos veces, o uno que no
    ///   empiece por guion, es un error de composicion y no de invocacion.
    /// </summary>
    constructor Create(const Args: TArray<string>; const SwitchMappings: TSwitchMappings);
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  /// <summary>
  ///   Fachada al estilo de JsonConfig.AddFile / IniConfig.AddFile.
  /// </summary>
  CommandLineConfig = class
  public
    /// <summary>Los argumentos con los que se invoco este proceso, sin el ejecutable.</summary>
    class function ProcessArgs: TArray<string>;

    class function AddArgs(const Builder: IConfigurationBuilder): IConfigurationBuilder; overload;
    class function AddArgs(const Builder: IConfigurationBuilder; const Args: TArray<string>): IConfigurationBuilder; overload;
    class function AddArgs(const Builder: IConfigurationBuilder; const Args: TArray<string>;
      const SwitchMappings: TSwitchMappings): IConfigurationBuilder; overload;

    class function Source: IConfigurationSource; overload;
    class function Source(const Args: TArray<string>): IConfigurationSource; overload;
    class function Source(const Args: TArray<string>; const SwitchMappings: TSwitchMappings): IConfigurationSource; overload;
  end;

implementation

uses
  System.Generics.Defaults;

const
  LongPrefix  = '--';
  ShortPrefix = '-';
  AltPrefix   = '/';
  Assignment  = '=';

{ TCommandLineConfigurationProvider }

constructor TCommandLineConfigurationProvider.Create(const Args: TArray<string>; const SwitchMappings: TSwitchMappings);
begin
  inherited Create;
  FArgs := Args;
  // Como en .NET, el alias no distingue mayusculas.
  FSwitchMappings := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  for var Mapping in SwitchMappings do
    FSwitchMappings.AddOrSetValue(Mapping.Key, Mapping.Value);
end;

destructor TCommandLineConfigurationProvider.Destroy;
begin
  FSwitchMappings.Free;
  inherited;
end;

procedure TCommandLineConfigurationProvider.Load;
var
  Idx: Integer;
  Arg, Current, Key, Value, Mapped: string;
  KeyStart, Sep: Integer;
begin
  Data.Clear;
  Idx := 0;
  while Idx < Length(FArgs) do
  begin
    Arg := FArgs[Idx];
    Current := Arg;
    KeyStart := 0;

    if Current.StartsWith(LongPrefix) then
      KeyStart := Length(LongPrefix)
    else if Current.StartsWith(ShortPrefix) then
      KeyStart := Length(ShortPrefix)
    else if Current.StartsWith(AltPrefix) then
    begin
      // "/x" equivale a "--x" al resolver alias, asi que se normaliza y el resto es comun.
      Current := LongPrefix + Current.Substring(Length(AltPrefix));
      KeyStart := Length(LongPrefix);
    end;

    Sep := Current.IndexOf(Assignment);

    if Sep < 0 then
    begin
      // Sin '=' y sin prefijo no nombra ninguna clave.
      if KeyStart = 0 then
      begin
        Inc(Idx);
        Continue;
      end;

      if FSwitchMappings.TryGetValue(Current, Mapped) then
        Key := Mapped
      else if KeyStart = Length(ShortPrefix) then
      begin
        // Un guion corto sin alias declarado no se interpreta.
        Inc(Idx);
        Continue;
      end
      else
        Key := Current.Substring(KeyStart);

      // El valor es el argumento siguiente; si no hay, no hay clave.
      Inc(Idx);
      if Idx >= Length(FArgs) then
        Break;
      Value := FArgs[Idx];
    end
    else
    begin
      if FSwitchMappings.TryGetValue(Current.Substring(0, Sep), Mapped) then
        Key := Mapped
      else if KeyStart = Length(ShortPrefix) then
      begin
        // DIVERGENCIA DELIBERADA de .NET, que aqui lanza FormatException: se ignora, porque
        // leer la linea de comandos no debe poder impedir el arranque por un guion de mas.
        Inc(Idx);
        Continue;
      end
      else
        Key := Current.Substring(KeyStart, Sep - KeyStart);

      // Un '=' sin nada detras deja la clave con cadena vacia, que es un valor.
      Value := Current.Substring(Sep + 1);
    end;

    if not Key.IsEmpty then
      Data.AddOrSetValue(Key, Value); // clave repetida: gana la ultima

    Inc(Idx);
  end;
  OnReload;
end;

{ TCommandLineConfigurationSource }

constructor TCommandLineConfigurationSource.Create(const Args: TArray<string>; const SwitchMappings: TSwitchMappings);
begin
  inherited Create;
  var Seen := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  try
    for var Mapping in SwitchMappings do
    begin
      if not (Mapping.Key.StartsWith(LongPrefix) or Mapping.Key.StartsWith(ShortPrefix)) then
        raise CommandLineConfigError.CreateFmt('Switch "%s" must start with "-" or "--"', [Mapping.Key]);
      if Seen.ContainsKey(Mapping.Key) then
        raise CommandLineConfigError.CreateFmt('Switch "%s" is mapped more than once', [Mapping.Key]);
      Seen.Add(Mapping.Key, Mapping.Value);
    end;
  finally
    Seen.Free;
  end;
  FArgs := Args;
  FSwitchMappings := SwitchMappings;
end;

function TCommandLineConfigurationSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TCommandLineConfigurationProvider.Create(FArgs, FSwitchMappings);
end;

{ CommandLineConfig }

class function CommandLineConfig.ProcessArgs: TArray<string>;
begin
  SetLength(Result, ParamCount);
  for var i := 1 to ParamCount do
    Result[i - 1] := ParamStr(i);
end;

class function CommandLineConfig.Source: IConfigurationSource;
begin
  Result := Source(ProcessArgs, []);
end;

class function CommandLineConfig.Source(const Args: TArray<string>): IConfigurationSource;
begin
  Result := Source(Args, []);
end;

class function CommandLineConfig.Source(const Args: TArray<string>; const SwitchMappings: TSwitchMappings): IConfigurationSource;
begin
  Result := TCommandLineConfigurationSource.Create(Args, SwitchMappings);
end;

class function CommandLineConfig.AddArgs(const Builder: IConfigurationBuilder): IConfigurationBuilder;
begin
  Result := AddArgs(Builder, ProcessArgs, []);
end;

class function CommandLineConfig.AddArgs(const Builder: IConfigurationBuilder; const Args: TArray<string>): IConfigurationBuilder;
begin
  Result := AddArgs(Builder, Args, []);
end;

class function CommandLineConfig.AddArgs(const Builder: IConfigurationBuilder; const Args: TArray<string>;
  const SwitchMappings: TSwitchMappings): IConfigurationBuilder;
begin
  Builder.Add(Source(Args, SwitchMappings));
  Result := Builder;
end;

end.
