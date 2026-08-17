unit ActionResults.Feat;

{
  An action says what happened and the framework turns that into a status code
  and a body. The controller here declares no routes on purpose: it exists to
  reach the result helpers, which are protected, without also being discovered
  by the router.

  It is built lazily, inside a step, never in the World's constructor. A World
  that raises while being built takes its whole Feature out of the report — no
  failure, no scenario, nothing — and raising while being built is exactly what
  a controller does until its form resource is linked.
}

interface

uses
  Daf.Web.Controller,
  Doubles.Feat;

type
  TSpecPayload = class
  private
    FName: string;
    FQty: Integer;
  public
    constructor Create(const AName: string; const AQty: Integer);
    property Name: string read FName write FName;
    property Qty: Integer read FQty write FQty;
  end;

  TResultsController = class(TWebController)
  public
    /// Every member of the family, reachable by name so one outline can walk
    /// the whole list instead of fifteen near-identical scenarios.
    function Produce(const Helper: string): IActionResult;
    function OkObject(const Payload: TObject): IActionResult;
  end;

  TActionResultsWorld = class
  public
    Helper: string;
    Code: Integer;
    Controller: TResultsController;
    Request: TSpecWebRequest;
    Response: TSpecWebResponse;
    Payload: TSpecPayload;
    constructor Create;
    destructor Destroy; override;
    function Ctrl: TResultsController;
    procedure Execute(const ActionResult: IActionResult);
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TSpecPayload }

constructor TSpecPayload.Create(const AName: string; const AQty: Integer);
begin
  inherited Create;
  FName := AName;
  FQty := AQty;
end;

{ TResultsController }

function TResultsController.Produce(const Helper: string): IActionResult;
begin
  if SameText(Helper, 'Ok') then Result := Ok('done')
  else if SameText(Helper, 'Created') then Result := Created
  else if SameText(Helper, 'Accepted') then Result := Accepted
  else if SameText(Helper, 'NoContent') then Result := NoContent
  else if SameText(Helper, 'MovedPermanently') then Result := MovedPermanently
  else if SameText(Helper, 'MovedTemporarily') then Result := MovedTemporarily
  else if SameText(Helper, 'NotModified') then Result := NotModified
  else if SameText(Helper, 'BadRequest') then Result := BadRequest
  else if SameText(Helper, 'Unauthorized') then Result := Unauthorized
  else if SameText(Helper, 'Forbidden') then Result := Forbidden
  else if SameText(Helper, 'NotFound') then Result := NotFound
  else if SameText(Helper, 'NotAcceptable') then Result := NotAcceptable
  else if SameText(Helper, 'Conflict') then Result := Conflict
  else if SameText(Helper, 'UnprocessableEntity') then Result := UnprocessableEntity
  else if SameText(Helper, 'InternalServerError') then Result := InternalServerError
  else
    raise Exception.CreateFmt('the spec asked for a result helper that does not exist: %s', [Helper]);
end;

function TResultsController.OkObject(const Payload: TObject): IActionResult;
begin
  Result := Ok(Payload);
end;

{ TActionResultsWorld }

constructor TActionResultsWorld.Create;
begin
  inherited;
  Request := TSpecWebRequest.Create('GET', '/spec/results');
  Response := TSpecWebResponse.Create(Request);
end;

destructor TActionResultsWorld.Destroy;
begin
  Payload.Free;
  Response.Free;
  Request.Free;
  Controller.Free;
  inherited;
end;

function TActionResultsWorld.Ctrl: TResultsController;
begin
  if not Assigned(Controller) then
    Controller := TResultsController.Create;
  Result := Controller;
end;

procedure TActionResultsWorld.Execute(const ActionResult: IActionResult);
begin
  ActionResult.Execute(Response);
end;

initialization

Feature('''
Feature Action results @results

  As an action author
  I want to return what happened rather than write to the response
  So status and body are the framework's job and the action stays a plain method
''')

.UseWorld<TActionResultsWorld>

.Rule('Action results carry status and body without the action touching the response')

  .ScenarioOutline('<Helper> answers <Code>')
    .When('the action returns <Helper>',
      procedure(W: TActionResultsWorld)
      begin
        W.Execute(W.Ctrl.Produce(W.Helper));
      end)
    .&Then('the response carries status <Code>',
      procedure(W: TActionResultsWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(W.Code);
      end)
    .Examples(
      [['Helper', 'Code'],
       ['Ok', 200],
       ['Created', 201],
       ['Accepted', 202],
       ['NoContent', 204],
       ['MovedPermanently', 301],
       ['MovedTemporarily', 302],
       ['NotModified', 304],
       ['BadRequest', 400],
       ['Unauthorized', 401],
       ['Forbidden', 403],
       ['NotFound', 404],
       ['NotAcceptable', 406],
       ['Conflict', 409],
       ['UnprocessableEntity', 422],
       ['InternalServerError', 500]])

  .Scenario('An object result is serialized')
    .Given('an object to answer with',
      procedure(W: TActionResultsWorld)
      begin
        W.Payload := TSpecPayload.Create('Bolt', 3);
      end)
    .When('the action returns it as an ok result',
      procedure(W: TActionResultsWorld)
      begin
        W.Execute(W.Ctrl.OkObject(W.Payload));
      end)
    .&Then('the body is its JSON representation, declared as JSON',
      procedure(W: TActionResultsWorld)
      begin
        Expect(W.Response.Content).ToContain('Bolt');
        Expect(W.Response.Content).ToContain('3');
        Expect(W.Response.ContentType).ToEqual('application/json');
      end);

end.
