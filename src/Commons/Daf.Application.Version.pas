unit Daf.Application.Version;

interface

uses
  System.SysUtils;

const
  /// <summary>Formato por defecto: la version semver con la seccion de build que declare el tag.</summary>
  VersionTemplate = '${Major}.${Minor}.${Patch}${PreRelease}${Build}';
  /// <summary>El mismo con los datos de compilacion unidos dentro de esa unica seccion.</summary>
  VersionTemplateWithBuild = '${Major}.${Minor}.${Patch}${PreRelease}${BuildFull}';

type
  TVersionInfo = record
  strict private
    FTag: string;
    FApplicationName: string;
    FCompany: string;
    FMajor: Integer;
    FMinor: Integer;
    FPatch: Integer;
    FPreRelease: string;
    FMetadata: string;
    FArchBits: Integer;
    FPlatform: string;
    FDebug: Boolean;
    FIsSemVer: Boolean;
    function CompiledPlatform: string;inline;
    function CompiledArchBits: Integer;inline;
    procedure ParseSemVer(const SemVerTag: string);
  public
    class function GetFrom(const AClass: TClass): TVersionInfo;static;
    /// <summary>Sin declarar la configuracion de depuracion: Debug es False.</summary>
    constructor Create(SemVerTag: string; AppName: string; Company: string); overload;
    /// <param name="Debug">
    ///   Si el binario que informa es una build de depuracion. Lo sabe su propio codigo y no el
    ///   framework, que se compila aparte: se escribe {$IFDEF DEBUG}True{$ELSE}False{$ENDIF} en el
    ///   codigo del binario.
    /// </param>
    constructor Create(SemVerTag: string; AppName: string; Company: string; Debug: Boolean); overload;
    /// <summary>No se fijo ninguna version: es distinto de haberla fijado y no entenderla.</summary>
    function IsEmpty: Boolean;
    /// <summary>El tag fijado sigue la gramatica semver, y por tanto las piezas estan pobladas.</summary>
    function IsSemVer: Boolean;
    /// <summary>
    ///   Formatea la version con una plantilla de marcadores <c>${...}</c>. Los de clase seccion
    ///   --Build y BuildFull-- abren la seccion de build y son excluyentes entre si; los demas
    ///   emiten su pieza cruda.
    /// </summary>
    function VersionTag(const Template: string = VersionTemplate): string;
    property Tag: string read FTag;
    property ApplicationName: string read FApplicationName;
    property Company: string read FCompany;
    property Major: Integer read FMajor;
    property Minor: Integer read FMinor;
    property Patch: Integer read FPatch;
    property PreRelease: string read FPreRelease;
    property Metadata: string read FMetadata;
    property ArchBits: Integer read FArchBits;
    property Platform: string read FPlatform;
    /// <summary>
    ///   Lo que declaro quien construyo la informacion: False si no declaro nada. Se fija al
    ///   construirse, como la plataforma y el tamano de palabra.
    /// </summary>
    property Debug: Boolean read FDebug;
  end;

  VersionAttribute = class(TCustomAttribute)
  strict private
    FVersionInfo: TVersionInfo;
  public
    constructor Create(SemVerTag: string; AppName: string; Company: string); overload;
    /// <param name="Debug">
    ///   Si el binario es una build de depuracion. Los argumentos de un atributo se evaluan al
    ///   compilar el codigo que lo lleva, asi que {$IFDEF DEBUG}True{$ELSE}False{$ENDIF} escrito
    ///   aqui es la configuracion del binario.
    /// </param>
    constructor Create(SemVerTag: string; AppName: string; Company: string; Debug: Boolean); overload;
    property VersionInfo: TVersionInfo read FVersionInfo;
  end;

implementation
uses
  System.Rtti,
  System.StrUtils,
  System.RegularExpressions,
  Daf.Types;

{ TVersionInfo }

class function TVersionInfo.GetFrom(const AClass: TClass): TVersionInfo;
begin
  // Una clase sin atributo devuelve un record vacio, y eso tiene que ser contrato y no
  // suerte: sin esto, Result se queda con lo que hubiera en la memoria del destino, y sus
  // campos numericos solo salen a cero cuando el destino venia limpio.
  Result := Default(TVersionInfo);
  var
    RC := TRttiContext.Create;
  try
    var
    RType := RC.GetType(AClass);
    var
    Attribute := RType.GetAttribute<VersionAttribute>;
    if Attribute <> nil then
      Result := Attribute.VersionInfo;
  finally
    RC.Free;
  end;
end;

constructor TVersionInfo.Create(SemVerTag, AppName, Company: string);
begin
  Self := TVersionInfo.Create(SemVerTag, AppName, Company, False);
end;

constructor TVersionInfo.Create(SemVerTag, AppName, Company: string; Debug: Boolean);
begin
  Self := Default(TVersionInfo);
  FTag := SemVerTag;
  FApplicationName := AppName;
  FCompany := Company;
  ParseSemVer(SemVerTag);
  FArchBits := CompiledArchBits;
  FPlatform := CompiledPlatform;
  FDebug := Debug;
end;

function TVersionInfo.IsEmpty: Boolean;
begin
  Result := FTag.IsEmpty;
end;

function TVersionInfo.IsSemVer: Boolean;
begin
  Result := FIsSemVer;
end;

function TVersionInfo.CompiledArchBits: Integer;
begin
  Result := SizeOf(NativeInt) * 8;
end;

function TVersionInfo.CompiledPlatform: string;
begin
{$IFDEF MSWINDOWS}
  Result := 'Win';
{$ENDIF}
{$IFDEF MACOS}
  Result := 'MacOS';
{$ENDIF}
{$IFDEF IOS}
  Result := 'iOS';
{$ENDIF}
{$IFDEF ANDROID}
  Result := 'Android';
{$ENDIF}
{$IFDEF LINUX}
  Result := 'Linux';
{$ENDIF}
{$IFDEF MSWINDOWSRT}
  Result := 'WinRT';
{$ENDIF}
end;

procedure TVersionInfo.ParseSemVer(const SemVerTag: string);
const
  SemverRegEx =
'^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$';
var
  RegEx: TRegEx;
  Match: TMatch;
  Group: TGroup;
  Value: string;
begin
  //  https://semver.org/
  // usar la regex mencionada en la pagina para extraer los valores
  RegEx := TRegEx.Create(SemverRegEx, [roIgnoreCase]);
  Match := RegEx.Match(SemVerTag);
  FIsSemVer := Match.Success;
  if Match.Success then
  begin
    Group := Match.Groups.Item['major'];
    Value := Group.Value;
    if not Value.IsEmpty then
      FMajor := Value.ToInteger;

    Group := Match.Groups.Item['minor'];
    Value := Group.Value;
    if not Value.IsEmpty then
      FMinor := Value.ToInteger;

    Group := Match.Groups.Item['patch'];
    Value := Group.Value;
    if not Value.IsEmpty then
      FPatch := Value.ToInteger;

    if Match.Groups.TryGetNamedGroup('prerelease', Group) then
    begin
      Value := Group.Value;
      if not Value.IsEmpty then
        FPreRelease := Value;
    end;

    if Match.Groups.TryGetNamedGroup('buildmetadata', Group) then
    begin
      Value := Group.Value;
      if not Value.IsEmpty then
        FMetadata := Value;
    end;
  end;
end;

function TVersionInfo.VersionTag(const Template: string = VersionTemplate): string;
begin
  // Sin version fijada no hay nada que informar. Recomponer desde las piezas daria 0.0.0, que se
  // lee como una version valida.
  if IsEmpty then
    Exit('');

  // Entender el tag sirve para poder responder por sus piezas, no para sustituirlo: el que no se
  // entiende se informa tal cual, porque es lo que su autor escribio.
  if not IsSemVer then
    Exit(FTag);

  var Compiled := Format('%s%d%s', [Self.Platform, ArchBits, IfThen(Self.Debug, '.Dbg', '')]);
  var Macros := TMacrosList.Create;
  try
    Macros['Major'] := FMajor.ToString;
    Macros['Minor'] := FMinor.ToString;
    Macros['Patch'] := FPatch.ToString;
    Macros['Tag'] := FTag;
    Macros['Platform'] := FPlatform;
    Macros['ArchBits'] := FArchBits.ToString;
    Macros['Metadata'] := FMetadata;
    Macros['CompiledMeta'] := Compiled;

    // Los que pueden faltar llevan dentro su separador: una plantilla plana no sabe omitirlo.
    Macros['PreRelease'] := IfThen(FPreRelease.IsEmpty, '', '-' + FPreRelease);
    Macros['Build'] := IfThen(FMetadata.IsEmpty, '', '+' + FMetadata);

    // Semver admite UNA seccion de build: el '+' lo escribe quien la abre y el '.' une dentro.
    if FMetadata.IsEmpty then
      Macros['BuildFull'] := '+' + Compiled
    else
      Macros['BuildFull'] := '+' + FMetadata + '.' + Compiled;

    Result := Macros.ResolveMacros(Template);
  finally
    Macros.Free;
  end;
end;

{ VersionAttribute }

constructor VersionAttribute.Create(SemVerTag, AppName, Company: string);
begin
  Create(SemVerTag, AppName, Company, False);
end;

constructor VersionAttribute.Create(SemVerTag, AppName, Company: string; Debug: Boolean);
begin
  inherited Create;
  FVersionInfo := TVersionInfo.Create(SemVerTag, AppName, Company, Debug);
end;

end.
