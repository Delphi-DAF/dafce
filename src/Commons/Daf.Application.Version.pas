unit Daf.Application.Version;

interface

uses
  System.SysUtils;

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
    function CompiledPlatform: string;inline;
    function CompiledDebug: Boolean;inline;
    function CompiledArchBits: Integer;inline;
    procedure ParseSemVer(const SemVerTag: string);
  public
    class function GetFrom(const AClass: TClass): TVersionInfo;static;
    constructor Create(SemVerTag: string; AppName: string; Company: string);
    function VersionTag(const WithCompiledMeta: Boolean = False): string;
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
    property Debug: Boolean read FDebug write FDebug;
  end;

  VersionAttribute = class(TCustomAttribute)
  strict private
    FVersionInfo: TVersionInfo;
  public
    constructor Create(SemVerTag: string; AppName: string; Company: string);
    property VersionInfo: TVersionInfo read FVersionInfo;
  end;

implementation
uses
  System.Rtti,
  System.StrUtils,
  System.RegularExpressions;

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
  FTag := SemVerTag;
  FApplicationName := AppName;
  FCompany := Company;
  ParseSemVer(SemVerTag);
  FArchBits := CompiledArchBits;
  FPlatform := CompiledPlatform;
  FDebug := CompiledDebug;
end;

function TVersionInfo.CompiledDebug: Boolean;
begin
  {$IFDEF DEBUG}
  Result := True;
  {$else}
  Result := False;
  {$ENDIF}
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

function TVersionInfo.VersionTag(const WithCompiledMeta: Boolean = False): string;
begin
  Result := Format('%d.%d.%d', [FMajor, FMinor, FPatch]);

  if not FPreRelease.IsEmpty then
    Result := Result + '-' + FPreRelease;

  if not FMetadata.IsEmpty then
    Result := Result + '+' + FMetadata;

  var CompiledMeta := Format('%s%d%s', [Self.Platform, ArchBits, IfThen(Self.Debug, '.Dbg', '')]);
  if WithCompiledMeta and not CompiledMeta.IsEmpty then
    Result := Result + '+' + CompiledMeta;
end;

{ VersionAttribute }

constructor VersionAttribute.Create(SemVerTag, AppName, Company: string);
begin
  inherited Create;
  Self.FVersionInfo.Create(SemVerTag, AppName, Company);
end;

end.
