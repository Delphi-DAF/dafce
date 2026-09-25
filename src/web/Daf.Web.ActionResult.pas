unit Daf.Web.ActionResult;

interface

uses
  Web.HTTPApp,
  System.SysUtils,
  System.JSON,
  Daf.Web.Controller,
  Daf.Web.Render;

type
  TJsonResult = class(TActionResult)
  private
    FContent: string;
  public
    constructor Create(const AContent: string); overload;
    constructor Create(const AObject: TObject); overload;
    constructor Create(const ABool: Boolean); overload;
    procedure Execute(Response: TWebResponse); override;
  end;

  TContentResult = class(TActionResult)
  private
    FContent: string;
    FContentType: string;
  public
    constructor Create(const AContent: string; const AContentType: string = 'text/plain');
    procedure Execute(Response: TWebResponse); override;
  end;

  TStatusResult = class(TActionResult)
  private
    FCode: Integer;
    FMessage: string;
  public
    constructor Create(Code: Integer; const Msg: string = '');
    procedure Execute(Response: TWebResponse); override;
  end;

  TPageResult = class(TActionResult)
  private
    FHtml: string;
  public
    constructor Create(const FileName: string; const Renderer: TWebRenderer);
    procedure Execute(Response: TWebResponse); override;
  end;

  TPartialResult = class(TActionResult)
  private
    FHtml: string;
  public
    constructor Create(const FileName: string; const Renderer: TWebRenderer);
    procedure Execute(Response: TWebResponse); override;
  end;

implementation

uses
  Rest.JSON;

{ TJsonResult }

constructor TJsonResult.Create(const AContent: string);
begin
  FContent := AContent;
end;

constructor TJsonResult.Create(const AObject: TObject);
begin
  FContent := TJson.ObjectToJsonString(AObject);
end;

constructor TJsonResult.Create(const ABool: Boolean);
begin
  var Json := TJSONBool.Create(ABool);
  try
    FContent := Json.ToJSON;
  finally
    Json.Free;
  end;
end;

procedure TJsonResult.Execute(Response: TWebResponse);
begin
  Response.ContentType := 'application/json';
  Response.Content := FContent;
end;

{ TContentResult }

constructor TContentResult.Create(const AContent, AContentType: string);
begin
  FContent := AContent;
  FContentType := AContentType;
end;

procedure TContentResult.Execute(Response: TWebResponse);
begin
  Response.ContentType := FContentType;
  Response.Content := FContent;
end;

{ TStatusResult }

constructor TStatusResult.Create(Code: Integer; const Msg: string);
begin
  FCode := Code;
  FMessage := Msg;
end;

procedure TStatusResult.Execute(Response: TWebResponse);
begin
  Response.StatusCode := FCode;
  if FMessage <> '' then
    Response.Content := FMessage;
end;

{ TPageResult }

constructor TPageResult.Create(const FileName: string; const Renderer: TWebRenderer);
begin
  FHtml := Renderer.RenderPage(FileName);
end;

procedure TPageResult.Execute(Response: TWebResponse);
begin
  Response.ContentType := 'text/html; charset=utf-8';
  Response.Content := FHtml;
end;

{ TPartialResult }

constructor TPartialResult.Create(const FileName: string; const Renderer: TWebRenderer);
begin
  FHtml := Renderer.RenderPartial(FileName);
end;

procedure TPartialResult.Execute(Response: TWebResponse);
begin
  Response.ContentType := 'text/html; charset=utf-8';
  Response.Content := FHtml;
end;

end.
