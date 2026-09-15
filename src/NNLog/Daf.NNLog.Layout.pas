unit Daf.NNLog.Layout;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging;

type
  TLayoutRenderer = reference to function(const Arg: string; const Entry: TLogEntry): string;
  TLogLayoutEngine = class
  private
  public
    class procedure RegisterLayoutRenderers(const Environment: IHostEnvironment);
    class function ResolveToken(const Token: string; const Entry: TLogEntry): string;
    class procedure RegisterRenderer(const Name: string; const Renderer: TLayoutRenderer);
    class function ResolveLayout(const Layout: string; const Entry: TLogEntry): string;
  end;

implementation

uses
  System.TypInfo,
  System.Generics.Defaults,
  System.Rtti,
  System.StrUtils,
  System.Character,
  Daf.Types;

var
  RendererMap: TDictionary<string, TLayoutRenderer> = nil;

{ TLogLayoutEngine }

class procedure TLogLayoutEngine.RegisterRenderer(const Name: string; const Renderer: TLayoutRenderer);
begin
  if RendererMap = nil then
    RendererMap := TDictionary<string, TLayoutRenderer>.Create(TIStringComparer.Ordinal);
  RendererMap.AddOrSetValue(Name, Renderer);
end;

class function TLogLayoutEngine.ResolveToken(const Token: string; const Entry: TLogEntry): string;
var
  Name, Arg: string;
  Renderer: TLayoutRenderer;
begin
  var ColonPos := Token.IndexOf(':');
  if ColonPos > 0 then
  begin
    Name := Token.Substring(0, ColonPos);
    Arg := Token.Substring(ColonPos + 1);
  end
  else
  begin
    Name := Token;
    Arg := '';
  end;
  Renderer := nil;
  if Assigned(RendererMap) and RendererMap.TryGetValue(Name, Renderer) then
    Result := Renderer(Arg, Entry)
  else
    // Sin sustituir, a proposito. Resolverlo a vacio deja el fallo invisible por los dos lados: un
    // token mal escrito pierde una columna entera sin decirlo, y un destino usado antes de que el
    // host registre los renderers escribe lineas sin contenido mientras todo lo observable --escribe,
    // rota, no lanza-- dice que funciona.
    Result := '${' + Token + '}';
  Renderer := nil;
end;

class function TLogLayoutEngine.ResolveLayout(const Layout: string; const Entry: TLogEntry): string;
begin
  Result := Layout;
  var Matches := TRegEx.Matches(Layout, '\$\{(.*?)\}');
  for var Match in Matches do
  begin
    var Token := Match.Groups[1].Value;
    var Resolved := ResolveToken(Token, Entry);
    Result := Result.Replace(Match.Value, Resolved);
  end;
end;

class procedure TLogLayoutEngine.RegisterLayoutRenderers(const Environment: IHostEnvironment);
begin

  // local copy of variables to no reference Envrinoment inside renderers
  var EnvironmentName := Environment.EnvironmentName;
  var ContentRootPath := Environment.ContentRootPath;
  var ApplicationName := Environment.ApplicationName;
  var BinPath := Environment.BinPath;

  TLogLayoutEngine.RegisterRenderer('environment',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := EnvironmentName;
    end);

  TLogLayoutEngine.RegisterRenderer('contentRootPath',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := ContentRootPath;
    end);

  TLogLayoutEngine.RegisterRenderer('ApplicationName',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := ApplicationName;
    end);

  TLogLayoutEngine.RegisterRenderer('binPath',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := BinPath;
    end);
  TLogLayoutEngine.RegisterRenderer('exception',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      if not Assigned(Entry.Exception) then
        Exit('');

      if Arg.ToLower = 'stacktrace' then
        Result := Entry.Exception.StackTrace
      else
        Result := Entry.Exception.ClassName + ': ' + Entry.Exception.Message;
    end);
  TLogLayoutEngine.RegisterRenderer('level',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := Entry.Level.ToString;
    end);

  TLogLayoutEngine.RegisterRenderer('category',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := Entry.Category;
    end);

  // El pipeline rellena Scope en cada entrada que compone. Sin este renderer el campo no lo leia
  // nadie, asi que BeginScope adjuntaba un contexto que ningun destino podia enseñar. Sale tal cual
  // lo compone TLogger.GetScopeMessage, que ya trae su propio separador delante.
  TLogLayoutEngine.RegisterRenderer('scope',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := Entry.Scope;
    end);

  TLogLayoutEngine.RegisterRenderer('message',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := Entry.Message;
    end);

  TLogLayoutEngine.RegisterRenderer('env',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := GetEnvironmentVariable(Arg);
    end);

  TLogLayoutEngine.RegisterRenderer('event-properties',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      var Value: TValue;
      if Entry.State.TryGetValue(Arg, Value) then
        Result := Value.ToString()
      else
        Result := '';
    end);

  TLogLayoutEngine.RegisterRenderer('date',
    function(const Arg: string; const Entry: TLogEntry): string
    var
      FormatStr: string;
    begin
      FormatStr := Arg;
      if FormatStr.IsEmpty then
        FormatStr := 'yyyy-MM-dd';
      Result := FormatDateTime(FormatStr, Now);
    end);

  TLogLayoutEngine.RegisterRenderer('timestamp',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := FormatDateTime('yyyy-MM-dd HH:mm:ss.zzz', Now);
    end);

  TLogLayoutEngine.RegisterRenderer('thread',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := IntToStr(TThread.Current.ThreadID);
    end);

  TLogLayoutEngine.RegisterRenderer('pid',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := IntToStr(Debugger.CurrentProcessId);
    end);

  TLogLayoutEngine.RegisterRenderer('newline',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := sLineBreak;
    end);

  TLogLayoutEngine.RegisterRenderer('uppercase',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := UpperCase(Arg);
    end);

  TLogLayoutEngine.RegisterRenderer('lowercase',
    function(const Arg: string; const Entry: TLogEntry): string
    begin
      Result := LowerCase(Arg);
    end);

end;

initialization

finalization
  FreeAndNil(RendererMap);
end.

