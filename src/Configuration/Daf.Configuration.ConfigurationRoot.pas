unit Daf.Configuration.ConfigurationRoot;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Nullable,
  Daf.Extensions.Configuration;

type
  TConfigurationSection = class(TInterfacedObject, IConfigurationSection, IConfiguration)
  private
    FPath: string;
    FRoot: IConfigurationRoot;
    function GetItem(const Key: string): string;
    procedure SetItem(const Key, Value: string);
  public
    constructor Create(const Root: IConfigurationRoot; const Path: string);
    /// <summary>
    /// Gets a configuration sub-section with the specified key.
    /// </summary>
    /// <param name="key">The key of the configuration section.</param>
    /// <returns>The <see cref="IConfigurationSection"/>.</returns>
    /// <remarks>
    ///     This method will never return <c>null</c>. If no matching sub-section is found with the specified key,
    ///     an empty <see cref="IConfigurationSection"/> will be returned.
    /// </remarks>
    function GetSection(const Key: string): IConfigurationSection;
    /// <summary>Presence of a child Key, answered against this section's own Path.</summary>
    function Exists(const Key: string): Boolean;
    /// <summary>Presence and Value of a child Key, answered against this section's own Path.</summary>
    function Find(const Key: string): TNullable<string>;
    function GetKey: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetPath: string;
    function HasChildren: Boolean;
    function GetChildren: IEnumerable<IConfigurationSection>;
    property Key: string read GetKey;
    property Value: string read GetValue write SetValue;
    property Path: string read GetPath;
    property Items[const Key: string]: string read GetItem write SetItem; default;
  end;

 TConfigurationRoot = class(TInterfacedObject, IConfigurationRoot, IConfiguration)
  private
    FProviders: IEnumerable<IConfigurationProvider>;
    procedure NotifyChanged;
    class procedure SetConfiguration(const Providers: IEnumerable<IConfigurationProvider>; const Key: string; Value: string);
    class function GetConfiguration(const Providers: IEnumerable<IConfigurationProvider>; const Key: string): string;
    class function FindConfiguration(const Providers: IEnumerable<IConfigurationProvider>; const Key: string): TNullable<string>;
    function InternalGetChildKeys(const Prefix: string): TArray<string>;
    function GetChildrenImplementation(const Path: string): IEnumerable<IConfigurationSection>;
    /// <summary>
    /// Returns a <see cref="IChangeToken"/> that can be used to observe when this configuration is reloaded.
    /// </summary>
    /// <returns>The <see cref="IChangeToken"/>.</returns>

  public
    constructor Create(const Providers: TList<IConfigurationProvider>);
    function GetItem(const Key: string): string;
    procedure SetItem(const Key, Value: string);
    function Exists(const Key: string): Boolean;
    function Find(const Key: string): TNullable<string>;
    function GetSection(const Key: string): IConfigurationSection;
    function GetProviders: IEnumerable<IConfigurationProvider>;
    function GetChildren: IEnumerable<IConfigurationSection>;
    procedure Reload;
    property Providers: IEnumerable<IConfigurationProvider> read GetProviders;
    property Items[const Key: string]: string read GetItem write SetItem; default;
  end;

implementation
uses
  System.Generics.Defaults,
  Daf.Enumerable;

{ TConfigurationSection }

constructor TConfigurationSection.Create(const Root: IConfigurationRoot; const Path: string);
begin
  inherited Create;
  FPath:= Path;
  FRoot := Root;
end;

function TConfigurationSection.GetSection(const Key: string): IConfigurationSection;
begin
  Result := FRoot.GetSection(TConfigurationPath.Combine([Path, Key]));
end;

function TConfigurationSection.GetPath: string;
begin
  Result := FPath;
end;

function TConfigurationSection.GetKey: string;
begin
  Result := TConfigurationPath.GetSectionKey(FPath);
end;

function TConfigurationSection.GetValue: string;
begin
  Result := FRoot[FPath];
end;

function TConfigurationSection.HasChildren: Boolean;
begin
  Result := GetChildren.GetEnumerator.MoveNext;
end;

function TConfigurationSection.GetItem(const Key: string): string;
begin
  Result := FRoot[TConfigurationPath.Combine([Path, Key])];
end;

function TConfigurationSection.Exists(const Key: string): Boolean;
begin
  Result := FRoot.Exists(TConfigurationPath.Combine([Path, Key]));
end;

function TConfigurationSection.Find(const Key: string): TNullable<string>;
begin
  Result := FRoot.Find(TConfigurationPath.Combine([Path, Key]));
end;

procedure TConfigurationSection.SetItem(const Key, Value: string);
begin
  FRoot[TConfigurationPath.Combine([Path, Key])] := Value;
end;

procedure TConfigurationSection.SetValue(const Value: string);
begin
  FRoot[FPath] := Value;
end;

function TConfigurationSection.GetChildren: IEnumerable<IConfigurationSection>;
begin
  Result := TConfigurationRoot(FRoot).GetChildrenImplementation(FPath);
end;

{ TConfigurationRoot }

constructor TConfigurationRoot.Create(const Providers: TList<IConfigurationProvider>);
begin
  inherited Create;
  Assert(Assigned(Providers), 'Providers passed to TConfigurationRoot is nil');
  FProviders := ToIEnumerable<IConfigurationProvider>(Providers);
  for var Provider in FProviders do
    Provider.Load;
end;

function TConfigurationRoot.GetProviders: IEnumerable<IConfigurationProvider>;
begin
  Result := FProviders;
end;

function TConfigurationRoot.GetSection(const Key: string): IConfigurationSection;
begin
  Result := TConfigurationSection.Create(Self, Key);
end;

function TConfigurationRoot.GetItem(const Key: string): string;
begin
  Result := GetConfiguration(FProviders, Key);
end;

procedure TConfigurationRoot.SetItem(const Key, Value: string);
begin
  SetConfiguration(FProviders, Key, Value);
end;

procedure TConfigurationRoot.Reload;
begin
  for var Provider in FProviders do
    Provider.Load;
  NotifyChanged;
end;

procedure TConfigurationRoot.NotifyChanged;
begin

end;

class function TConfigurationRoot.GetConfiguration(const Providers: IEnumerable<IConfigurationProvider>; const Key: string): string;
begin
  Result := '';
  var Temp: string;
  for var Provider in Providers do
    if Provider.TryGet(key, Temp) then Result := Temp;
end;

class function TConfigurationRoot.FindConfiguration(const Providers: IEnumerable<IConfigurationProvider>; const Key: string): TNullable<string>;
begin
  // Same traversal as GetConfiguration: walking forward and keeping the last hit is
  // walking backwards and taking the first one, so the winning provider is the same.
  Result := TNullable<string>.Null;
  if not Assigned(Providers) then Exit;

  var Temp: string;
  for var Provider in Providers do
    if Provider.TryGet(Key, Temp) then
      Result := TNullable<string>.From(Temp);
end;

function TConfigurationRoot.Find(const Key: string): TNullable<string>;
begin
  Result := FindConfiguration(FProviders, Key);
end;

function TConfigurationRoot.Exists(const Key: string): Boolean;
begin
  // .NET's IConfigurationSection.Exists(): a value of its own OR children. A section
  // that only groups has no value, and answering that it is not there while its
  // children are being read is a false answer about the shape of the document.
  Result := FindConfiguration(FProviders, Key).HasValue or (Length(InternalGetChildKeys(Key)) > 0);
end;

class procedure TConfigurationRoot.SetConfiguration(const Providers: IEnumerable<IConfigurationProvider>; const Key: string; Value: string);
begin
  if not Assigned(Providers) then Exit;

  for var Provider in Providers do
    Provider.&Set(key, value);
end;

function TConfigurationRoot.GetChildren: IEnumerable<IConfigurationSection>;
begin
  Result := GetChildrenImplementation('');
end;

function TConfigurationRoot.GetChildrenImplementation(const Path: string): IEnumerable<IConfigurationSection>;
begin
  var Aux := TList<IConfigurationSection>.Create;
  var Keys := InternalGetChildKeys(Path);
  var Prefix := Path;
  if not Prefix.IsEmpty then
    Prefix := Prefix + TConfigurationPath.KeyDelimiter;

  for var ChildKey in Keys do
    Aux.Add(TConfigurationSection.Create(Self, Prefix + ChildKey));
  Result := ToIEnumerable<IConfigurationSection>(Aux);
end;

function TConfigurationRoot.InternalGetChildKeys(const Prefix: string): TArray<string>;
begin
  var AllKeys := TList<string>.Create(TIStringComparer.Ordinal);

  // this owns the TList
  var AllKeysEnum := ToIEnumerable<string>(AllKeys);
  if Assigned(FProviders) then
  for var Provider in FProviders do
  begin
    var ProviderKeys := ToIENumerable<string>(Provider.GetChildKeys(AllKeysEnum, Prefix)).ToArray;
    for var ChildKey in ProviderKeys do
      if not AllKeys.Contains(ChildKey) then
        AllKeys.Add(ChildKey);
  end;
  Result := AllKeys.ToArray;
end;

end.

