unit Daf.Extensions.Configuration;

interface
uses
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti;

type

  IConfigurationRoot = interface;
  IConfigurationBuilder = interface;

  /// <summary>
  /// Provides configuration Key/values for an application.
  /// </summary>
  IConfigurationProvider = interface(IInvokable)
    ['{C3CE2AFE-35B3-45B4-973D-49029D093AEA}']
    /// <summary>
    /// Tries to get a configuration value for the specified Key.
    /// </summary>
    /// <param name="Key">The Key.</param>
    /// <param name="Value">The Value.</param>
    /// <returns><c>True</c> if a Value for the specified Key was found, otherwise <c>false</c>.</returns>
    function TryGet(const Key: string; out Value: string): Boolean;

    /// <summary>
    /// Sets a configuration Value for the specified Key.
    /// </summary>
    /// <param name="Key">The Key.</param>
    /// <param name="Value">The Value.</param>
    procedure &Set(const Key: string; const Value: string);

    /// <summary>
    /// Loads configuration values from the source represented by this <see cref="IConfigurationProvider"/>.
    /// </summary>
    procedure Load;

    /// <summary>
    /// Returns the immediate descendant configuration keys for a given parent path based on this
    /// <see cref="IConfigurationProvider"/>s data and the set of keys returned by all the preceding
    /// <see cref="IConfigurationProvider"/>s.
    /// </summary>
    /// <param name="earlierKeys">The child keys returned by the preceding providers for the same parent path.</param>
    /// <param name="ParentPath">The parent path.</param>
    /// <returns>The child keys.</returns>
    function GetChildKeys(const EarlierKeys: IEnumerable<string>; const ParentPath: string): IEnumerable<string>;
  end;

  IConfigurationSource = interface(IInvokable)
    ['{505F89D6-1B62-4885-A029-0788EA32F2F9}']
    /// <summary>
    /// Builds the <see cref="IConfigurationProvider"/> for this source.
    /// </summary>
    /// <param name="builder">The <see cref="IConfigurationBuilder"/>.</param>
    /// <returns>An <see cref="IConfigurationProvider"/></returns>
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  TConfigurationSourceOption = (csoOptional, csoReloadOnChange);
  TConfigurationSourceOptions = set of TConfigurationSourceOption;


  /// <summary>
  /// Represents a type used to build application configuration.
  /// </summary>
  IConfigurationBuilder = interface(IInvokable)
    ['{9F341984-D141-4782-8AD4-3649E8A28721}']
    function GetSources: IEnumerable<IConfigurationSource>;
    function GetProperties: TDictionary<string, TValue>;

    /// <summary>
    /// Gets a Key/Value collection that can be used to share data between the <see cref="IConfigurationBuilder"/>
    /// and the registered <see cref="IConfigurationSource"/>s.
    /// </summary>
    property Properties: TDictionary<string, TValue> read GetProperties;

    /// <summary>
    /// Gets the sources used to obtain configuration values
    /// </summary>
    property Sources: IEnumerable<IConfigurationSource> read GetSources;

    /// <summary>
    /// Adds a new configuration source.
    /// </summary>
    /// <param name="source">The configuration source to add.</param>
    /// <returns>The same <see cref="IConfigurationBuilder"/>.</returns>
    function Add(const Source: IConfigurationSource): IConfigurationBuilder;

    /// <summary>
    /// Builds an <see cref="IConfiguration"/> with keys and values from the set of sources registered in
    /// <see cref="Sources"/>.
    /// </summary>
    /// <returns>An <see cref="IConfigurationRoot"/> with keys and values from the registered sources.</returns>
    function Build: IConfigurationRoot;
  end;

  IConfigurationSection = interface;

  IConfiguration = interface(IInvokable)
    ['{270815EB-F099-4934-B3D7-DF278D40E601}']
    function GetSection(const Key: string): IConfigurationSection;
    function GetItem(const Key: string): string;
    procedure SetItem(const Key: string; const Value: string);
    function GetChildren: IEnumerable<IConfigurationSection>;
    property Item[const Key: string]: string read GetItem write SetItem; default;
  end;

  IConfigurationRoot = interface(IConfiguration)
    ['{D2CDFE3B-5043-4951-A87C-624DCB6BEE4D}']
      function GetProviders: IEnumerable<IConfigurationProvider>;

     /// <summary>
      /// Force the configuration values to be reloaded from the underlying <see cref="IConfigurationProvider"/>s.
      /// </summary>
      procedure Reload;

      /// <summary>
      /// The <see cref="IConfigurationProvider"/>s for this configuration.
      /// </summary>
      property Providers: IEnumerable<IConfigurationProvider> read GetProviders;
  end;

  IConfigurationSection = interface(IConfiguration)
    ['{5829A0F0-2DD4-4CDE-B26C-9E775B3E0EE6}']
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetKey: string;
    function GetPath: string;
    function GetChildren: IEnumerable<IConfigurationSection>;
    function HasChildren: Boolean;
  /// <summary>
    /// Gets the Key this section occupies in its parent.
    /// </summary>
    property Key: string read GetKey;
    /// <summary>
    /// Gets the full path to this section within the <see cref="IConfiguration"/>.
    /// </summary>
    property Path: string read GetPath;
    /// <summary>
    /// Gets or sets the section Value.
    /// </summary>
    property Value: string read GetValue write SetValue;
  end;

  /// <summary>
  /// Utility methods and constants for manipulating Configuration paths
  /// </summary>
  TConfigurationPath = class
  public
  /// <summary>
  /// The delimiter ":" used to separate individual keys in a Path.
  /// </summary>
    const
    KeyDelimiter = ':';
    /// <summary>
    /// Combines Path segments into one Path.
    /// </summary>
    /// <param name="PathSegments">The Path segments to combine.</param>
    /// <returns>The combined Path.</returns>
    class function Combine(const PathSegments: array of string): string; overload;
    /// <summary>
    /// Combines Path segments into one Path.
    /// </summary>
    /// <param name="PathSegments">The Path segments to combine.</param>
    /// <returns>The combined Path.</returns>
    class function Combine(const PathSegments: IEnumerable<string>): string; overload;
    /// <summary>
    /// Extracts the last Path segment from the Path.
    /// </summary>
    /// <param name="Path">The Path.</param>
    /// <returns>The last Path segment of the Path.</returns>
    class function GetSectionKey(const Path: string): string;
    /// <summary>
    /// Extracts the Path corresponding to the parent node for a given Path.
    /// </summary>
    /// <param name="Path">The Path.</param>
    /// <returns>The original Path minus the last individual segment found in it. Null if the original Path corresponds to a top level node.</returns>
    class function GetParentPath(const Path: string): string;
  end;

  ConfigurationKeyNameAttribute = class(TCustomAttribute)
  strict private
    FName: string;
  public
    /// <summary>
    /// Initializes a new instance of <see cref="ConfigurationKeyNameAttribute"/>.
    /// </summary>
    /// <param name="name">The key name.</param>
    constructor Create(Name: string);

    /// <summary>
    /// Gets the key name for a configuration property.
    /// </summary>
    property Name: string read FName;
  end;

implementation

{ TConfigurationPath }

class function TConfigurationPath.Combine(const PathSegments: array of string): string;
begin
  Result := string.Join(KeyDelimiter, PathSegments);
end;

class function TConfigurationPath.Combine(const PathSegments: IEnumerable<string>): string;
begin
  Result := string.Join(KeyDelimiter, PathSegments);
end;

class function TConfigurationPath.GetSectionKey(const Path: string): string;
begin
  if Path.IsEmpty then
    Exit(Path);

  var
  lastDelimiterIndex := Path.LastIndexOf(KeyDelimiter);

  if lastDelimiterIndex < 0 then
    Exit(Path);
  Result := Path.Substring(lastDelimiterIndex + 1);
end;

class function TConfigurationPath.GetParentPath(const Path: string): string;
begin
  if Path.IsEmpty then
    Exit('');
  var
  lastDelimiterIndex := Path.LastIndexOf(KeyDelimiter);

  if lastDelimiterIndex < 0 then
    Exit('');
  Result := Path.Substring(0, lastDelimiterIndex);
end;

{ ConfigurationKeyNameAttribute }

constructor ConfigurationKeyNameAttribute.Create(Name: string);
begin
  inherited Create;
  FName := Name;
end;

end.
