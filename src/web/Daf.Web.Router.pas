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
    FOrder: Integer;
    function GetPathPattern: string;
    function BuildRegexFromRoute(const RouteTemplate: string): string;
    function ExtractRouteParams(const RouteTemplate: string): TArray<TRouteParameter>;
    procedure GetRouteParamValues(const Path: string);
    function GetBodyValue(Param: TRttiParameter; const Request: TWebRequest; const Owned: TObjectList<TObject>): TValue;
    function GetServiceValue(Param: TRttiParameter; const Services: IServiceProvider): TValue;
    function GetQueryValue(const Param: TRttiParameter; const Request: TWebRequest): TValue;
    function BindParams(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse;
      const Owned: TObjectList<TObject>): TArray<TValue>;
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
  System.DateUtils,
  System.IOUtils,
  System.JSON,
  REST.Json,
  REST.Json.Types,
  Daf.Activator,
  Daf.Web.ActionResult,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.Web.Controller;

type
  TMimeTypes = class
  public
    class function GetContentType(const FileName: string): string; static;
  end;

  /// What the request carries cannot bind a parameter: the action is not invoked and the answer is 400.
  EBadRequest = class(Exception);



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


// The path as it came on the wire, without its query: Indy decodes PathInfo, %2F included, and an
// encoded slash would split its segment in two.
function RoutePath(const Request: TWebRequest): string;
begin
  Result := Request.RawPathInfo;
  var Query := Result.IndexOf('?');
  if Query >= 0 then
    Result := Result.Substring(0, Query);
end;

// As ASP.NET Core decodes a route value: every escape but %2F, and a '+' is a plus sign in a path.
function DecodeRouteValue(const Raw: string): string;
var
  Bytes: TBytes;
  Hex, I: Integer;
begin
  if Raw.IndexOf('%') < 0 then
    Exit(Raw);
  Bytes := nil;
  I := 1;
  while I <= Length(Raw) do
  begin
    if (Raw[I] = '%') and (I + 2 <= Length(Raw)) and not SameText(Copy(Raw, I + 1, 2), '2F')
      and TryStrToInt('$' + Copy(Raw, I + 1, 2), Hex) then
    begin
      Bytes := Bytes + [Byte(Hex)];
      Inc(I, 3);
    end
    else
    begin
      Bytes := Bytes + TEncoding.UTF8.GetBytes(string(Raw[I]));
      Inc(I);
    end;
  end;
  Result := TEncoding.UTF8.GetString(Bytes);
end;

// As ASP.NET Core binds a simple type: with the invariant culture, dates in ISO 8601 or the invariant
// format, booleans as true or false, enumerations by name or by number. What does not convert is the
// client's mistake, and it answers 400.
function ConvertValue(const Param: TRttiParameter; const Raw: string): TValue;
var
  Int: Integer;
  Int64Value: Int64;
  Float: Extended;
  Date: TDateTime;
begin
  var Info := Param.ParamType.Handle;
  case Param.ParamType.TypeKind of
    tkString, tkUString, tkLString, tkWString:
      Exit(Raw);
    tkInteger:
      if TryStrToInt(Raw, Int) then
        Exit(TValue.FromOrdinal(Info, Int));
    tkInt64:
      if TryStrToInt64(Raw, Int64Value) then
        Exit(Int64Value);
    tkFloat:
      if (Info = TypeInfo(TDateTime)) or (Info = TypeInfo(TDate)) or (Info = TypeInfo(TTime)) then
      begin
        if TryISO8601ToDate(Raw, Date, False) or TryStrToDateTime(Raw, Date, TFormatSettings.Invariant) then
          Exit(TValue.From<TDateTime>(Date).Cast(Info));
      end
      else if TryStrToFloat(Raw, Float, TFormatSettings.Invariant) then
        Exit(TValue.From<Extended>(Float).Cast(Info));
    tkEnumeration:
      if Info = TypeInfo(Boolean) then
      begin
        if SameText(Raw, 'true') then
          Exit(True);
        if SameText(Raw, 'false') then
          Exit(False);
      end
      else
      begin
        Int := GetEnumValue(Info, Raw);
        if (Int < 0) and TryStrToInt(Raw, Int) and
          ((Int < GetTypeData(Info).MinValue) or (Int > GetTypeData(Info).MaxValue)) then
          Int := -1;
        if Int >= 0 then
          Exit(TValue.FromOrdinal(Info, Int));
      end;
  else
    raise Exception.CreateFmt('Unsupported parameter type for "%s"', [Param.Name]);
  end;
  raise EBadRequest.CreateFmt('The value "%s" for "%s" is not a valid %s', [Raw, Param.Name, Param.ParamType.Name]);
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

// An absent key arrives as its type's default, and deciding whether that is an error is the action's.
function TWebRouteMatcher.GetQueryValue(const Param: TRttiParameter; const Request: TWebRequest): TValue;
begin
  var Index := Request.QueryFields.IndexOfName(Param.Name);
  if Index < 0 then
    TValue.Make(nil, Param.ParamType.Handle, Result)
  else
    Result := ConvertValue(Param, Request.QueryFields.ValueFromIndex[Index]);
end;

// As an ASP.NET Core API controller: the object is built by its constructor and filled from a body
// that has to be a JSON object, or the request is rejected. It lives as long as the request, in Owned.
function TWebRouteMatcher.GetBodyValue(Param: TRttiParameter; const Request: TWebRequest;
  const Owned: TObjectList<TObject>): TValue;
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
      var Json := TJSONValue.ParseJSONValue(Body);
      try
        if not (Json is TJSONObject) then
          raise EBadRequest.CreateFmt('The body for "%s" is not a JSON object', [Param.Name]);
        var Obj := TActivator.CreateInstance((Param.ParamType as TRttiInstanceType).MetaclassType);
        Owned.Add(Obj);
        try
          TJson.JsonToObject(Obj, TJSONObject(Json), []);
        except
          on E: Exception do
            raise EBadRequest.CreateFmt('The body for "%s" does not bind: %s', [Param.Name, E.Message]);
        end;
        Result := Obj;
      finally
        Json.Free;
      end;
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
  Services.GetRequiredService(Param.ParamType.Handle, Intf);
  // Typed as the parameter: Invoke does not cast a TValue of IInterface to the interface the
  // action declares.
  TValue.Make(@Intf, Param.ParamType.Handle, Result);
end;

function TWebRouteMatcher.GetRouteValue(Param: TRttiParameter): TValue;
begin
  for var RouteParam in FRouteParameters do
    if SameText(RouteParam.Name, Param.Name) then
      Result := ConvertValue(Param, DecodeRouteValue(RouteParam.Value));
end;

// The values of this request, in a local array: the objects it creates for them go to Owned, which
// whoever dispatches frees when the request is over.
function TWebRouteMatcher.BindParams(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse;
  const Owned: TObjectList<TObject>): TArray<TValue>;
begin
  GetRouteParamValues(RoutePath(Request));

  SetLength(Result, Length(FBoundParams));
  for var P in FBoundParams do
  begin
    case P.Kind of
      bkRequest: Result[P.Index] := Request;
      bkResponse: Result[P.Index] := Response;
      bkServices:
        Result[P.Index] := GetServiceValue(P.Param, Services);
      bkBody:
        Result[P.Index] := GetBodyValue(P.Param, Request, Owned);
      bkQuery:
        Result[P.Index] := GetQueryValue(P.Param, Request);
      bkRoute:
          Result[P.Index] := GetRouteValue(P.Param);
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
  Match := Regex.Match(RoutePath(Request));
  Result := Match.Success;
end;

procedure TWebRouteMatcher.WebDispatch(Services: IServiceProvider; Request: TWebRequest; Response: TWebResponse);
begin
  var Controller := TActivatorUtilities.CreateInstance(Services, ControllerType.MetaclassType);

  if not Assigned(Controller) then
    Exit;
  // What the binding creates dies with the controller, after the result has been executed.
  var Owned := TObjectList<TObject>.Create(True);
  try
    TWebController(Controller).ConfigureRenderer(WebRouter.RootDirectory);
    var Params: TArray<TValue>;
    try
      Params := BindParams(Services, Request, Response, Owned);
    except
      on E: EBadRequest do
      begin
        Response.StatusCode := 400;
        Response.ContentType := 'text/plain';
        Response.Content := E.Message;
        Exit;
      end;
    end;

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
    Owned.Free;
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
      Matcher.FOrder := FMatchers.Count;
      FMatchers.Add(Matcher);
    end;
  end;
end;

function TWebRouter.GetRootDirectory: string;
begin
  Result := FRootDirectory;
end;

// Negative when the route A has to be tried before B: at the first position where one has a
// literal segment and the other a parameter, the literal goes first.
function ComparePrecedence(const A, B: string): Integer;
begin
  var SegmentsA := A.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  var SegmentsB := B.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  for var I := 0 to High(SegmentsA) do
  begin
    if I > High(SegmentsB) then
      Break;
    var ParamA := SegmentsA[I].Contains('{');
    var ParamB := SegmentsB[I].Contains('{');
    if ParamA <> ParamB then
      Exit(Ord(ParamA) - Ord(ParamB));
  end;
  Result := 0;
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
  // WebDispatch takes the first matcher that accepts, so this order is the precedence: a literal
  // wins over a parameter whatever order the actions were declared in, and routes that tie keep
  // the order they were discovered in.
  FMatchers.Sort(TComparer<TWebRouteMatcher>.Construct(
    function(const A, B: TWebRouteMatcher): Integer
    begin
      Result := ComparePrecedence(A.PathPattern, B.PathPattern);
      if Result = 0 then
        Result := A.FOrder - B.FOrder;
    end));
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

// The file a request path names inside the root, or '' when there is no root or the path
// resolves outside it -- with .., or as an absolute path, which Combine would return as is.
function StaticFileUnder(const Root, PathInfo: string): string;
begin
  Result := '';
  if Root = '' then
    Exit;
  try
    var FullRoot := IncludeTrailingPathDelimiter(TPath.GetFullPath(Root));
    var Candidate := TPath.GetFullPath(TPath.Combine(FullRoot, PathInfo.TrimLeft(['/'])));
    if Candidate.StartsWith(FullRoot, True) and TFile.Exists(Candidate) then
      Result := Candidate;
  except
    // A path with characters no file name can hold names no file.
    on EArgumentException do
      Result := '';
  end;
end;

procedure TWebRouter.WebDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  TMonitor.Enter(Self);
  try
    Handled := True;
    Logger.LogInformation('[%s] %s?%s', [Request.Method, Request.PathInfo, Request.Query]);
    var StaticFilePath := StaticFileUnder(FRootDirectory, Request.PathInfo);
    if StaticFilePath <> '' then
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
