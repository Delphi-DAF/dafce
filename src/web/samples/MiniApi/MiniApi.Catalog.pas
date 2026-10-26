unit MiniApi.Catalog;

{
  El controlador y el servicio que consume. El controlador esta aqui y no en el .dpr
  porque el router lo descubre por RTTI, y el RTTI solo ve tipos de la seccion interface
  de una unit.
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Web.HTTPApp,
  Daf.Web.Controller;

type
  /// Lo que el controlador devuelve como cuerpo. Un objeto llano: Ok(TObject) lo
  /// serializa a JSON con REST.Json.
  TProductDTO = class
  private
    FId: Integer;
    FName: string;
    FPrice: Currency;
  public
    constructor Create(AId: Integer; const AName: string; APrice: Currency);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Price: Currency read FPrice write FPrice;
  end;

  ICatalog = interface(IInvokable)
    ['{1D8C4F60-2E93-4A57-B0E8-7C51A9D36F24}']
    function Find(Id: Integer): TProductDTO;
    function Count: Integer;
  end;

  TCatalog = class(TInterfacedObject, ICatalog)
  strict private
    FItems: TObjectDictionary<Integer, TProductDTO>;
  public
    constructor Create;
    destructor Destroy; override;
    /// Devuelve una copia: el cuerpo de la respuesta se libera tras serializarse, y el
    /// catalogo no puede quedarse sin sus productos porque alguien pidio uno.
    function Find(Id: Integer): TProductDTO;
    function Count: Integer;
  end;

  [WebRoutePrefix('/api/products')]
  TProductsController = class(TWebController)
  strict private
    FCatalog: ICatalog;
  public
    /// El controlador lo construye el router, resolviendo sus dependencias del
    /// contenedor. Reintroduce porque TWebController.Create no toma parametros.
    constructor Create(const Catalog: ICatalog); reintroduce;

    /// GET /api/products/42 -- el segmento {Id} entra por nombre en el parametro.
    [WebRoute(mtGet, '/{Id}')]
    function GetById(Id: Integer): IActionResult;

    /// DELETE /api/products/42 -- la ruta casa, el verbo decide otra accion.
    [WebRoute(mtDelete, '/{Id}')]
    function Remove(Id: Integer): IActionResult;

    /// GET /api/products/search?q=taladro -- de la cadena de consulta. Declarada despues
    /// de /{Id}, y la gana igual: un segmento literal se prueba antes que un parametro.
    [WebRoute(mtGet, '/search')]
    function Search([FromQuery] Q: string): IActionResult;

    /// GET /api/products/health -- mtAny, asi que contesta a cualquier verbo.
    [WebRoute(mtAny, '/health')]
    function Health: IActionResult;
  end;

implementation

uses
  Daf.Web.ActionResult;

{ TProductDTO }

constructor TProductDTO.Create(AId: Integer; const AName: string; APrice: Currency);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FPrice := APrice;
end;

{ TCatalog }

constructor TCatalog.Create;
begin
  inherited Create;
  FItems := TObjectDictionary<Integer, TProductDTO>.Create([doOwnsValues]);
  FItems.Add(42, TProductDTO.Create(42, 'Taladro', 89.9));
  FItems.Add(43, TProductDTO.Create(43, 'Sierra', 54.5));
end;

destructor TCatalog.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TCatalog.Find(Id: Integer): TProductDTO;
begin
  var Item: TProductDTO;
  if FItems.TryGetValue(Id, Item) then
    Result := TProductDTO.Create(Item.Id, Item.Name, Item.Price)
  else
    Result := nil;
end;

function TCatalog.Count: Integer;
begin
  Result := FItems.Count;
end;

{ TProductsController }

constructor TProductsController.Create(const Catalog: ICatalog);
begin
  inherited Create;
  FCatalog := Catalog;
end;

function TProductsController.GetById(Id: Integer): IActionResult;
begin
  var Item := FCatalog.Find(Id);
  if Item = nil then
    Exit(NotFound(Format('no hay producto %d', [Id])));
  try
    Result := Ok(Item);
  finally
    Item.Free;
  end;
end;

function TProductsController.Search([FromQuery] Q: string): IActionResult;
begin
  if Q.IsEmpty then
    Exit(BadRequest('falta el parametro q'));
  Result := Ok(Format('buscando "%s" entre %d producto(s)', [Q, FCatalog.Count]));
end;

function TProductsController.Health: IActionResult;
begin
  Result := Ok(FCatalog.Count > 0);
end;

function TProductsController.Remove(Id: Integer): IActionResult;
begin
  // Find entrega una copia de la que se hace cargo el llamante.
  var Item := FCatalog.Find(Id);
  if Item = nil then
    Exit(NotFound(Format('no hay producto %d', [Id])));
  Item.Free;
  Result := NoContent(Format('producto %d retirado', [Id]));
end;

end.
