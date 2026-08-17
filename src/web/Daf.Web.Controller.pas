unit Daf.Web.Controller;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Daf.Web.Router,
  Daf.Web.Render;

type
  // to simplify uses clause in WebControllerss descendents
  TMethodType =  Web.HTTPApp.TMethodType;
  WebRoutePrefixAttribute =   Daf.Web.Router.WebRoutePrefixAttribute;
  WebRouteAttribute =   Daf.Web.Router.WebRouteAttribute;
  FromQueryAttribute =  Daf.Web.Router.FromQueryAttribute;
  FromBodyAttribute =  Daf.Web.Router.FromBodyAttribute;
  FromServicesAttribute = Daf.Web.Router.FromServicesAttribute;

  IActionResult = interface
    ['{136447F2-68E8-4150-A4FF-3BA2886E891B}']
    procedure Execute(Response: TWebResponse);
  end;

  TActionResult = class(TInterfacedObject, IActionResult)
  public
    procedure Execute(Response: TWebResponse); virtual; abstract;
  end;

  TWebControllerBaseClass = class of TWebControllerBase;
  TWebControllerBase = class
  private
    FRenderer: TWebRenderer;
    procedure SetRenderer(const Value: TWebRenderer);
  protected
    property Renderer: TWebRenderer read FRenderer write SetRenderer;
    function Ok(const Value: TObject): IActionResult; overload;
    function Ok(const Value: string): IActionResult; overload;
    function Ok(const Value: Boolean): IActionResult; overload;

    function NotFound(const Msg: string = 'Not found'): IActionResult;
    function BadRequest(const Msg: string = 'Bad request'): IActionResult;
    function InternalServerError(const Msg: string = 'Internal server error'): IActionResult;
    function Unauthorized(const Msg: string = 'Unauthorized'): IActionResult;
    function Forbidden(const Msg: string = 'Forbidden'): IActionResult;
    function UnprocessableEntity(const Msg: string = 'Unprocessable entity'): IActionResult;
    function Created(const Msg: string = 'Created'): IActionResult;
    function Accepted(const Msg: string = 'Accepted'): IActionResult;
    function NoContent(const Msg: string = 'No content'): IActionResult;
    function NotModified(const Msg: string = 'Not modified'): IActionResult;
    function NotAcceptable(const Msg: string = 'Not acceptable'): IActionResult;
    function Conflict(const Msg: string = 'Conflict'): IActionResult;
    function MovedPermanently(const Msg: string = 'Moved permanently'): IActionResult;
    function MovedTemporarily(const Msg: string = 'Moved temporarily'): IActionResult;


    function Html(const HtmlText: string): IActionResult;
    function Page(const FileName: string): IActionResult;
    function Partial(const FileName: string): IActionResult;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConfigureRenderer(const ARootDirectory: string);
  end;

  TWebControllerClass = class of TWebController;
  TWebController = class(TDataModule)
  private
    FRenderer: TWebRenderer;
    procedure SetRootDirectory(const Value: string);
  protected
    property Renderer: TWebRenderer read FRenderer;

    function Ok(const Value: TObject): IActionResult; overload;
    function Ok(const Value: string): IActionResult; overload;
    function Ok(const Value: Boolean): IActionResult; overload;

    function NotFound(const Msg: string = 'Not found'): IActionResult;
    function BadRequest(const Msg: string = 'Bad request'): IActionResult;
    function InternalServerError(const Msg: string = 'Internal server error'): IActionResult;
    function Unauthorized(const Msg: string = 'Unauthorized'): IActionResult;
    function Forbidden(const Msg: string = 'Forbidden'): IActionResult;
    function UnprocessableEntity(const Msg: string = 'Unprocessable entity'): IActionResult;
    function Created(const Msg: string = 'Created'): IActionResult;
    function Accepted(const Msg: string = 'Accepted'): IActionResult;
    function NoContent(const Msg: string = 'No content'): IActionResult;
    function NotModified(const Msg: string = 'Not modified'): IActionResult;
    function NotAcceptable(const Msg: string = 'Not acceptable'): IActionResult;
    function Conflict(const Msg: string = 'Conflict'): IActionResult;
    function MovedPermanently(const Msg: string = 'Moved permanently'): IActionResult;
    function MovedTemporarily(const Msg: string = 'Moved temporarily'): IActionResult;


    function Html(const HtmlText: string): IActionResult;
    function Page(const FileName: string): IActionResult;
    function Partial(const FileName: string): IActionResult;
    procedure AddVar(const AName: string; AObject: TObject; AOwned: Boolean = True);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure ConfigureRenderer(const RootDirectory: string);
  end;

  // Designer interceptor class to avoid use of an expert:
  // we want easy of install and use for DAF
  TDataModule = TWebController;

implementation

// TWebController descends from TDataModule and constructs with an owner, so
// InitInheritedComponent needs its form resource present. Without this line the
// .dfm sits in the repository linked to nothing and the first construction of
// any descendant raises EResNotFound — at runtime, with nothing said at compile
// time.
{$R *.dfm}

uses
  Daf.Web.ActionResult;

{ TWebControllerBase }

constructor TWebControllerBase.Create;
begin
  inherited Create;
  FRenderer := TWebRenderer.Create(nil);
end;

destructor TWebControllerBase.Destroy;
begin
  FRenderer.Free;
  inherited;
end;

procedure TWebControllerBase.ConfigureRenderer(const ARootDirectory: string);
begin
  if Assigned(FRenderer) then
    FRenderer.RootDirectory := ARootDirectory;
end;

procedure TWebControllerBase.SetRenderer(const Value: TWebRenderer);
begin
  if Assigned(FRenderer) then
    FRenderer.Free;
  FRenderer := Value;
end;

function TWebControllerBase.Ok(const Value: TObject): IActionResult;
begin
  Result := TJsonResult.Create(Value);
end;

function TWebControllerBase.Ok(const Value: string): IActionResult;
begin
  Result := TJsonResult.Create(Value);
end;

function TWebControllerBase.Ok(const Value: Boolean): IActionResult;
begin
  Result := TJsonResult.Create(Value);
end;

function TWebControllerBase.NotFound(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(404, Msg);
end;

function TWebControllerBase.BadRequest(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(400, Msg);
end;

function TWebControllerBase.InternalServerError(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(500, Msg);
end;

function TWebControllerBase.Unauthorized(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(401, Msg);
end;

function TWebControllerBase.Forbidden(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(403, Msg);
end;

function TWebControllerBase.UnprocessableEntity(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(422, Msg);
end;

function TWebControllerBase.Created(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(201, Msg);
end;

function TWebControllerBase.Accepted(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(202, Msg);
end;

function TWebControllerBase.NoContent(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(204, Msg);
end;

function TWebControllerBase.NotModified(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(304, Msg);
end;

function TWebControllerBase.NotAcceptable(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(406, Msg);
end;

function TWebControllerBase.Conflict(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(409, Msg);
end;

function TWebControllerBase.MovedPermanently(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(301, Msg);
end;

function TWebControllerBase.MovedTemporarily(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(302, Msg);
end;

function TWebControllerBase.Html(const HtmlText: string): IActionResult;
begin
  Result := TContentResult.Create(HtmlText, 'text/html; charset=utf-8');
end;

function TWebControllerBase.Page(const FileName: string): IActionResult;
begin
  Result := TPageResult.Create(FileName, Renderer);
end;

function TWebControllerBase.Partial(const FileName: string): IActionResult;
begin
  Result := TPartialResult.Create(FileName, Renderer);
end;

{ TWebController }

constructor TWebController.Create;
begin
  inherited Create(nil);
  FRenderer := TWebRenderer.Create(Self);
end;

destructor TWebController.Destroy;
begin
  FRenderer.Free;
  inherited;
end;

procedure TWebController.ConfigureRenderer(const RootDirectory: string);
begin
  SetRootDirectory(RootDirectory);
end;

procedure TWebController.SetRootDirectory(const Value: string);
begin
  FRenderer.RootDirectory := Value;
end;

function TWebController.Ok(const Value: TObject): IActionResult;
begin
  Result := TJsonResult.Create(Value);
end;

function TWebController.Ok(const Value: string): IActionResult;
begin
  Result := TJsonResult.Create(Value);
end;

function TWebController.Ok(const Value: Boolean): IActionResult;
begin
  Result := TJsonResult.Create(Value);
end;

function TWebController.NotFound(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(404, Msg);
end;

procedure TWebController.AddVar(const AName: string; AObject: TObject; AOwned: Boolean);
begin
  FRenderer.AddVar(AName, AObject, AOwned)
end;

function TWebController.BadRequest(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(400, Msg);
end;

function TWebController.InternalServerError(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(500, Msg);
end;

function TWebController.Unauthorized(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(401, Msg);
end;

function TWebController.Forbidden(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(403, Msg);
end;

function TWebController.UnprocessableEntity(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(422, Msg);
end;

function TWebController.Created(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(201, Msg);
end;

function TWebController.Accepted(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(202, Msg);
end;

function TWebController.NoContent(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(204, Msg);
end;

function TWebController.NotModified(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(304, Msg);
end;

function TWebController.NotAcceptable(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(406, Msg);
end;

function TWebController.Conflict(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(409, Msg);
end;

function TWebController.MovedPermanently(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(301, Msg);
end;

function TWebController.MovedTemporarily(const Msg: string): IActionResult;
begin
  Result := TStatusResult.Create(302, Msg);
end;

function TWebController.Html(const HtmlText: string): IActionResult;
begin
  Result := TContentResult.Create(HtmlText, 'text/html; charset=utf-8');
end;

function TWebController.Page(const FileName: string): IActionResult;
begin
  Result := TPageResult.Create(FileName, Renderer);
end;

function TWebController.Partial(const FileName: string): IActionResult;
begin
  Result := TPartialResult.Create(FileName, Renderer);
end;

end.
