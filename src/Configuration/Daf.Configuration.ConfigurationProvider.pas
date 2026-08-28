unit Daf.Configuration.ConfigurationProvider;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Daf.Extensions.Configuration;

type
  TConfigurationProvider = class(TInterfacedObject, IConfigurationProvider)
  private
    FData: TDictionary<string, string>;
  class function Segment(const Key: string; const PrefixLength: Integer): string;
  public
    /// <summary>
    /// Initializes a new <see cref="IConfigurationProvider"/>
    /// </summary>
    constructor Create;
    destructor Destroy;override;

    /// <summary>
    /// The configuration Key value pairs for this provider.
    /// </summary>
    property Data: TDictionary<string, string> read FData;

    /// <summary>
    /// Triggers the reload change token and creates a new one.
    /// </summary>
    procedure OnReload;

    /// <summary>
    /// Attempts to find a value with the given Key, returns true if one is found, false otherwise.
    /// </summary>
    /// <param name="Key">The Key to lookup.</param>
    /// <param name="value">The value found at Key if one is found.</param>
    /// <returns>True if Key has a value, false otherwise.</returns>
    function TryGet(const Key: string; out value: string): Boolean; virtual;
    /// <summary>
    /// Sets a value for a given Key.
    /// </summary>
    /// <param name="Key">The configuration Key to set.</param>
    /// <param name="value">The value to set.</param>
    procedure &Set(const Key: string; const value: string);virtual;

    /// <summary>
    /// Loads (or reloads) the data for this provider.
    /// </summary>
    procedure Load; virtual;

    /// <summary>
    /// Returns the list of keys that this provider has.
    /// </summary>
    /// <param name="EarlierKeys">The earlier keys that other providers contain.</param>
    /// <param name="ParentPath">The path for the parent IConfiguration.</param>
    /// <returns>The list of keys for this provider.</returns>
    function GetChildKeys(const EarlierKeys: IEnumerable<string>; const ParentPath: string): IEnumerable<string>; virtual;
    /// <summary>
    /// Returns a <see cref="IChangeToken"/> that can be used to listen when this provider is reloaded.
    /// </summary>
    /// <returns>The <see cref="IChangeToken"/>.</returns>
    /// function  GetReloadToken: IChangeToken;
  public
    /// <summary>
    /// Generates a string representing this provider name and relevant details.
    /// </summary>
    /// <returns> The configuration name. </returns>
    function ToString: string; override;
  end;

implementation
uses
  System.Generics.Defaults,
  Daf.Enumerable;

{ TConfigurationProvider }

constructor TConfigurationProvider.Create;
begin
  inherited;
  FData := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
end;

function TConfigurationProvider.ToString: string;
begin
  Result := ClassName;
end;

procedure TConfigurationProvider.Load;
begin

end;

procedure TConfigurationProvider.OnReload;
begin

end;

function TConfigurationProvider.TryGet(const Key: string; out value: string): Boolean;
begin
  Result := Data.TryGetValue(Key, value);
end;

class function TConfigurationProvider.Segment(const Key: string; const PrefixLength: Integer): string;
begin
    var idx := Key.IndexOf(TConfigurationPath.KeyDelimiter, PrefixLength);

    if idx < 0 then
      Result := Key.Substring(PrefixLength)
    else
      Result := Key.Substring(PrefixLength, idx - PrefixLength);
end;

procedure TConfigurationProvider.&Set(const Key: string; const value: string);
begin
  // AddOrSetValue y no Data[Key] := : el setter de TDictionary EXIGE que la clave ya exista
  // y levanta 'Item not found' si no. Escribir una clave nueva es lo normal en una
  // configuracion --y lo unico que hace SetItem--, asi que con el indexador este proveedor
  // solo sabia sobrescribir. No se veia porque la configuracion de host no llevaba ningun
  // proveedor de estos hasta que el entorno paso a derivarse de ella.
  Data.AddOrSetValue(Key, value);
end;

destructor TConfigurationProvider.Destroy;
begin
  FData.Free;
  inherited;
end;

function TConfigurationProvider.GetChildKeys(const EarlierKeys: IEnumerable<string>; const ParentPath: string): IEnumerable<string>;
begin
  var Results := TList<string>.Create(TIStringComparer.Ordinal);

  if ParentPath.isEmpty then
  begin
    for var kv in Data do
    begin
      Results.Add(Segment(kv.Key, 0));
    end
  end
  else
  begin
    for var kv in Data do
    begin
      if (kv.Key.Length > ParentPath.Length)
          and kv.Key.StartsWith(ParentPath, True)
          and (kv.Key[ParentPath.Length + 1] = TConfigurationPath.KeyDelimiter) then
      begin
        var newKey := Segment(kv.Key, ParentPath.Length + 1);
        if not Results.Contains(newKey) then
          Results.Add(newKey);
      end;
    end;
  end;

  if Assigned(EarlierKeys) then
    for var Key in EarlierKeys do
      if not Results.Contains(Key) then
        Results.Add(Key);

  Results.Sort;
  Result := ToIEnumerable<string>(Results);
end;

end.
