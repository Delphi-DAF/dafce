unit Daf.Web.Router;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.RegularExpressions,
  Daf.Extensions.Logging,
  Daf.Extensions.DependencyInjection,
  Generics.Collections,
  Web.HTTPApp;

type
  WebRoutePrefixAttribute = class(TCustomAttribute)
  private
    FRoutePrefix: string;
  public
    constructor Create(const RoutePrefix: string);
    property RoutePrefix: string read FRoutePrefix;
  end;

  WebRouteAttribute = class(TCustomAttribute)
  private
    FWebMethod: TMethodType;
    FRoute: string;
  public
    constructor Create(const WebMethod: TMethodType; const ARoute: string);
    property Route: string read FRoute;
    property WebMethod: TMethodType read FWebMethod;
  end;

  FromQueryAttribute = class(TCustomAttribute);
  FromBodyAttribute = class(TCustomAttribute);
  FromServicesAttribute = class(TCustomAttribute);

  TMethodTypeHelper = record helper for TMethodType
  public
    function AsString: string;
  end;

  type
  TBindingKind = (bkRoute, bkQuery, bkBody, bkServices, bkRequest, bkResponse);

  TBoundParameter = record
    Index: Integer;
    Name: string;
    Kind: TBindingKind;
    Param: TRttiParameter;
    Value: TValue;
  end;

  TRouteParameter = record
    Name: string;
    Value: string;
  end;

  IWebRouter = interface(IInvokable)
    ['{D8CA562E-1B44-4AB2-8DE0-DEBDBF27FB7A}']
    procedure SetRootDirectory(const Value: string);
    function GetRootDirectory: string;
    /// <summary>Engancha el enrutado al BeforeDispatch de quien despacha.</summary>
    /// <remarks>
    ///   Toma el tipo BASE y no TWebDispatcher porque lo unico que necesita --BeforeDispatch--
    ///   esta ahi, y un TWebModule NO es un TWebDispatcher: los dos descienden de este. Con el
    ///   tipo estrecho, el modulo web que crea WebBroker no cabia por la puerta.
    /// </remarks>
    procedure UseDispatcher(WebDispatcher: TCustomWebDispatcher);
    property RootDirectory: string read GetRootDirectory write SetRootDirectory;
  end;

  TWebRouteMatcher = class;
  TWebRouter = class(TInterfacedObject, IWebRouter)
  private
    FMatchers: TObjectList<TWebRouteMatcher>;
    FContext: TRttiContext;
    FRootDirectory: string;
    FLogger: ILogger;
    FScopeFactory: IServiceScopeFactory;
    procedure SetRootDirectory(const Value: string);
    function GetRootDirectory: string;
  protected
    procedure DiscoverMethods(ControllerType: TRttiInstanceType; PrefixAttr: WebRoutePrefixAttribute);
  public
    constructor Create(const ScopeFactory: IServiceScopeFactory; const LoggerFactory: ILoggerFactory = nil); reintroduce;
    destructor Destroy; override;
    procedure UseDispatcher(WebDispatcher: TCustomWebDispatcher);
    procedure DiscoverControllers;
    procedure WebDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    property RootDirectory: string read GetRootDirectory write SetRootDirectory;
    property Logger: ILogger read FLogger;
    property ScopeFactory: IServiceScopeFactory read FScopeFactory;
  end;

  TWebRouteMatcher = class
  private
    FWebRouter: TWebRouter;
    FControllerType: TRttiInstanceType;
    FControllerMethod: TRttiMethod;
    FWebMethod: string;
    FRoutePrefix: string;
    FRoute: string;
    FRegexPattern: string;
    FRouteParameters: TArray<TRouteParameter>;
    FBoundParams: TArray<TBoundParameter>;
    function GetPathPattern: string;
    function BuildRegexFromRoute(const RouteTemplate: string): string;
    function ExtractRouteParams(const RouteTemplate: string): TArray<TRouteParameter>;
    procedure GetRouteParamValues(const Path: string);
    function GetBodyValue(Param: TRttiParameter; const Request: TWebRequest): TValue;
    function GetServiceValue(Param: TRttiParameter; const Services: IServiceProvider): TValue;
    function GetQueryValue(const Param: TRttiParameter; const Request: TWebRequest): string;
    procedure BindParams(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse);
    function GetRouteValue(Param: TRttiParameter): TValue;
  public
    constructor Create(const ControllerType: TRttiInstanceType; const ControllerMethod: TRttiMethod;
      const PrefixAttr: WebRoutePrefixAttribute; const RouteAttr: WebRouteAttribute);
    function Accept(Request: TWebRequest): Boolean;
    procedure WebDispatch(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse);
    property WebRouter: TWebRouter read FWebRouter;
    property ControllerType: TRttiInstanceType read FControllerType;
    property ControllerMethod: TRttiMethod read FControllerMethod;
    property WebMethod: string read FWebMethod;
    property RoutePrefix: string read FRoutePrefix;
    property Route: string read FRoute;
    property PathPattern: string read GetPathPattern;
  end;

implementation

uses
  System.Generics.Defaults,
  System.IOUtils,
  System.JSON,
  REST.Json,
  REST.Json.Types,
  Daf.Web.ActionResult,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.Web.Controller;

type
  TMimeTypes = class
  public
    class function GetContentType(const FileName: string): string; static;
  end;



class function TMimeTypes.GetContentType(const FileName: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.html' then
    Result := 'text/html'
  else if Ext = '.htm' then
    Result := 'text/html'
  else if Ext = '.css' then
    Result := 'text/css'
  else if Ext = '.js' then
    Result := 'application/javascript'
  else if Ext = '.json' then
    Result := 'application/json'
  else if Ext = '.png' then
    Result := 'image/png'
  else if Ext = '.jpg' then
    Result := 'image/jpeg'
  else if Ext = '.jpeg' then
    Result := 'image/jpeg'
  else if Ext = '.gif' then
    Result := 'image/gif'
  else if Ext = '.svg' then
    Result := 'image/svg+xml'
  else if Ext = '.ico' then
    Result := 'image/x-icon'
  else if Ext = '.woff' then
    Result := 'font/woff'
  else if Ext = '.woff2' then
    Result := 'font/woff2'
  else if Ext = '.ttf' then
    Result := 'font/ttf'
  else if Ext = '.eot' then
    Result := 'application/vnd.ms-fontobject'
  else if Ext = '.otf' then
    Result := 'font/otf'
  else
    Result := 'application/octet-stream'; // Default
end;

{ TMethodTypeHelper }

function TMethodTypeHelper.AsString: string;
begin
  Result := GetEnumName(TypeInfo(TMethodType), Ord(Self)).Substring(2).ToUpperInvariant;
end;

{ WebRoutePrefixAttribute }

constructor WebRoutePrefixAttribute.Create(const RoutePrefix: string);
begin
  inherited Create;
  FRoutePrefix := RoutePrefix;
end;

{ WebRouteAttribute }

constructor WebRouteAttribute.Create(const WebMethod: TMethodType; const ARoute: string);
begin
  inherited Create;
  FRoute := ARoute;
  FWebMethod := WebMethod;
end;

{ TWebRouteMatcher }

constructor TWebRouteMatcher.Create(const ControllerType: TRttiInstanceType; const ControllerMethod: TRttiMethod;
  const PrefixAttr: WebRoutePrefixAttribute; const RouteAttr: WebRouteAttribute);
var
  FullPattern: string;
begin
  inherited Create;
  FControllerType := ControllerType;
  FControllerMethod := ControllerMethod;
  FRoutePrefix := PrefixAttr.RoutePrefix;
  FRoute := RouteAttr.Route;
  FWebMethod := RouteAttr.WebMethod.AsString;

  FullPattern := GetPathPattern;
  FRouteParameters := ExtractRouteParams(FullPattern);
  FRegexPattern := BuildRegexFromRoute(FullPattern);

  var Params := FControllerMethod.GetParameters;
  SetLength(FBoundParams, Length(Params));

  for var I := 0 to High(Params) do
  begin
    var Param := Params[I];
    var Kind: TBindingKind;

    if Param.ParamType.Handle = TypeInfo(TWebRequest) then
      Kind := bkRequest
    else if Param.ParamType.Handle = TypeInfo(TWebResponse) then
      Kind := bkResponse
    else if Param.HasAttribute<FromServicesAttribute> then
      Kind := bkServices
    else if Param.HasAttribute<FromBodyAttribute> then
      Kind := bkBody
    else if Param.HasAttribute<FromQueryAttribute> then
      Kind := bkQuery
    else
      Kind := bkRoute;

    FBoundParams[I].Index := I;
    FBoundParams[I].Name := Param.Name;
    FBoundParams[I].Kind := Kind;
    FBoundParams[I].Param := Param;
  end;
end;

function TWebRouteMatcher.GetPathPattern: string;
begin
  Result := RoutePrefix + FRoute;
end;

function TWebRouteMatcher.ExtractRouteParams(const RouteTemplate: string): TArray<TRouteParameter>;
var
  StartIdx, EndIdx, CurrIdx: Integer;
  Parameter: TRouteParameter;
begin
  CurrIdx := 1;
  Result := [];
  while CurrIdx <= Length(RouteTemplate) do
  begin
    if RouteTemplate[CurrIdx] = '{' then
    begin
      StartIdx := CurrIdx + 1;
      EndIdx := RouteTemplate.IndexOf('}', StartIdx);
      Inc(EndIdx); //IndexOf es zero based
      if EndIdx = 0 then
        raise Exception.Create('Invalid route template: missing }');

      Parameter.Name := RouteTemplate.Substring(StartIdx - 1, EndIdx - StartIdx);

      Result := Result + [Parameter];

      CurrIdx := EndIdx + 1;
    end
    else
      Inc(CurrIdx);
  end;
end;

function TWebRouteMatcher.BuildRegexFromRoute(const RouteTemplate: string): string;
var
  i, StartIdx, EndIdx: Integer;
begin
  Result := '';
  i := 0;
  while i < Length(RouteTemplate) do
  begin
    if RouteTemplate[i + 1] = '{' then
    begin
      StartIdx := i + 1;
      EndIdx := RouteTemplate.IndexOf('}', StartIdx);
      if EndIdx = 0 then
        raise Exception.Create('Invalid route template');
      Result := Result + '([^/]+)';
      i := EndIdx + 1;
    end
    else
    begin
      Result := Result + RouteTemplate[i + 1];
      Inc(i);
    end;
  end;
  Result := '^' + Result + '$';
end;


procedure TWebRouteMatcher.GetRouteParamValues(const Path: string);
begin
  var Regex := TRegEx.Create(FRegexPattern, [roIgnoreCase]);
  var Match := Regex.Match(Path);
  if not Match.Success then
    Exit;
  for var i := 1 to Match.Groups.Count - 1 do
    FRouteParameters[i - 1].Value := Match.Groups[i].Value;
end;

function TWebRouteMatcher.GetQueryValue(const Param: TRttiParameter; const Request: TWebRequest): string;
begin
  Result := Request.QueryFields.Values[Param.Name];
end;

function TWebRouteMatcher.GetBodyValue(Param: TRttiParameter; const Request: TWebRequest): TValue;
begin
  var Body := Request.Content;
  case Param.ParamType.TypeKind of
    tkUnknown: ;
    tkInteger: ;
    tkChar: ;
    tkEnumeration: ;
    tkFloat: ;
    tkString: ;
    tkSet: ;
    tkClass: begin
      var Obj := (Param.ParamType as TRttiInstanceType).MetaclassType.Create;
      var Json := TJSonValue.ParseJSONValue(Body) as TJSonObject;
      TJSOn.JsonToObject(Obj, Json, []);
      Result := Obj;
    end;
    tkMethod: ;
    tkWChar: ;
    tkLString: ;
    tkWString: ;
    tkVariant: ;
    tkArray: ;
    tkRecord: ;
    tkInterface: ;
    tkInt64: ;
    tkDynArray: ;
    tkUString: ;
    tkClassRef: ;
    tkPointer: ;
    tkProcedure: ;
    tkMRecord: ;
  end;

end;

function TWebRouteMatcher.GetServiceValue(Param: TRttiParameter; const Services: IServiceProvider): TValue;
begin
  var Intf: IInterface;
  Services.GetService(Param.ParamType.Handle, Intf);
  Result := TValue.From(Intf);
end;

function TWebRouteMatcher.GetRouteValue(Param: TRttiParameter): TValue;
begin
  for var RouteParam in FRouteParameters do
  begin
    if SameText(RouteParam.Name, Param.Name) then
    case Param.ParamType.TypeKind of
      tkString, tkUString, tkLString, tkWString:
        Result := RouteParam.Value;
      tkInteger:
        Result := StrToInt(RouteParam.Value);
      tkInt64:
        Result := StrToInt64(RouteParam.Value);
      tkFloat:
        if Param.ParamType.Handle = TypeInfo(TDateTime) then
          Result := StrToDateTime(RouteParam.Value)
        else
          Result := StrToFloat(RouteParam.Value);
      tkEnumeration:
        if Param.ParamType.Handle = TypeInfo(Boolean) then
          Result := SameText(RouteParam.Value, 'true') or (RouteParam.Value = '1')
        else
          Result := GetEnumValue(Param.ParamType.Handle, RouteParam.Value);
    else
      raise Exception.CreateFmt('Unsupported parameter type for "%s"', [Param.Name]);
    end;
  end;
end;

procedure TWebRouteMatcher.BindParams(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse);
begin
  GetRouteParamValues(Request.PathInfo);

  for var P in FBoundParams do
  begin
    case P.Kind of
      bkRequest: FBoundParams[P.Index].Value := Request;
      bkResponse: FBoundParams[P.Index].Value := Response;
      bkServices:
        FBoundParams[P.Index].Value := GetServiceValue(P.Param, Services);
      bkBody:
        FBoundParams[P.Index].Value := GetBodyValue(P.Param, Request);
      bkQuery:
        FBoundParams[P.Index].Value := GetQueryValue(P.Param, Request);
      bkRoute:
          FBoundParams[P.Index].Value := GetRouteValue(P.Param);
      else
          raise Exception.CreateFmt('Missing route param "%s"', [P.Name]);
    end;
  end;
end;

function TWebRouteMatcher.Accept(Request: TWebRequest): Boolean;
var
  Regex: TRegEx;
  Match: TMatch;
begin
  Result := False;
  if (WebMethod <> 'ANY') and not SameText(WebMethod, Request.Method) then
    Exit;

  Regex := TRegEx.Create(FRegexPattern, [roIgnoreCase]);
  Match := Regex.Match(Request.PathInfo);
  Result := Match.Success;
end;

procedure TWebRouteMatcher.WebDispatch(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse);
begin
  var Controller := TActivatorUtilities.CreateInstance(Services, ControllerType.MetaclassType);

  if not Assigned(Controller) then
    Exit;
  try
    TWebController(Controller).ConfigureRenderer(WebRouter.RootDirectory);
    BindParams(Services, Request, Response);
    var Params: TArray<TValue>;
    Setlength(Params, Length(FBoundParams));
    for var idx := Low(Params) to High(Params) do
      Params[idx] := FBoundParams[idx].Value;

    var ResultValue := FControllerMethod.Invoke(Controller, Params);

    if ResultValue.IsEmpty then Exit;

    if ResultValue.IsType<IActionResult> then
      ResultValue.AsType<IActionResult>.Execute(Response)
    else
    if ResultValue.IsObject and (ResultValue.AsObject is TActionResult) then
      TActionResult(ResultValue.AsObject).Execute(Response)
    else if ResultValue.Kind = tkString then
    begin
      Response.ContentType := 'text/plain';
      Response.Content := ResultValue.AsString;
    end
    else
      raise Exception.Create('Unsupported return type from controller action.');
  finally
    Controller.Free;
  end;
end;

{ TWebRouter }

constructor TWebRouter.Create(const ScopeFactory: IServiceScopeFactory; const LoggerFactory: ILoggerFactory = nil);
begin
  inherited Create;
  FContext := TRttiContext.Create;
  FMatchers := TObjectList<TWebRouteMatcher>.Create;
  if Assigned(LoggerFactory) then
    FLogger := LoggerFactory.CreateLogger(Self.ClassType)
  else
    FLogger := TNullLogger.Create;
  FScopeFactory := ScopeFactory;
  DiscoverControllers;
end;

destructor TWebRouter.Destroy;
begin
  FMatchers.Free;
  FContext.Free;
  inherited;
end;

procedure TWebRouter.DiscoverMethods(ControllerType: TRttiInstanceType; PrefixAttr: WebRoutePrefixAttribute);
begin
  for var ControllerMethod in ControllerType.GetMethods do
  begin
    var
    Attr := ControllerMethod.GetAttribute<WebRouteAttribute>;
    if Assigned(Attr) then
    begin
      var
      Matcher := TWebRouteMatcher.Create(ControllerType, ControllerMethod, PrefixAttr, Attr);
      Matcher.FWebRouter := Self;
      FMatchers.Add(Matcher);
    end;
  end;
end;

function TWebRouter.GetRootDirectory: string;
begin
  Result := FRootDirectory;
end;

procedure TWebRouter.DiscoverControllers;
begin
  for var RttiType in FContext.GetTypes do
  begin
    if RttiType.TypeKind <> tkClass then
      Continue;
    var
    Attr := RttiType.GetAttribute<WebRoutePrefixAttribute>;
    if Assigned(Attr) then
      DiscoverMethods(RttiType.AsInstance, Attr);
  end;
end;

procedure TWebRouter.SetRootDirectory(const Value: string);
begin
  FRootDirectory := Value;
end;

type
  // BeforeDispatch es protected en TCustomWebDispatcher y solo se publica en los
  // descendientes. La clase de acceso es el modo estandar de llegar a un protected del que
  // se hereda igualmente, y evita estrechar el parametro a un tipo por el que el modulo web
  // de WebBroker no pasa.
  TDispatcherAccess = class(TCustomWebDispatcher);

procedure TWebRouter.UseDispatcher(WebDispatcher: TCustomWebDispatcher);
begin
  TDispatcherAccess(WebDispatcher).BeforeDispatch := WebDispatch;
end;

procedure TWebRouter.WebDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  TMonitor.Enter(Self);
  try
    Handled := True;
    Logger.LogInformation('[%s] %s?%s', [Request.Method, Request.PathInfo, Request.Query]);
    var
    StaticFilePath := TPath.Combine(FRootDirectory, Request.PathInfo.TrimLeft(['/']));
    if TFile.Exists(StaticFilePath) then
    begin
      Response.ContentType := TMimeTypes.GetContentType(StaticFilePath);
      Response.ContentStream := TFileStream.Create(StaticFilePath, fmOpenRead or fmShareDenyWrite);
      Exit;
    end;

    for var Matcher in FMatchers do
    begin
      if not Matcher.Accept(Request) then Continue;
      // WebRouter es singleton, la request debe ser scoped:
      var Scope := Self.ScopeFactory.CreateScope;
      Matcher.WebDispatch(Scope.ServiceProvider, Request, Response);
      Exit;
    end;
    Handled := False;
    Logger.LogDebug('[%s] %s?%s d''ont match any route', [Request.Method, Request.PathInfo, Request.Query]);
  finally
    TMonitor.Exit(Self);
  end;
end;

end.
