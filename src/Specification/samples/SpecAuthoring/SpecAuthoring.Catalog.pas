unit SpecAuthoring.Catalog;

{
  El dominio del sample: una entidad sin clase base, su metamodelo y las reglas del
  catalogo como valores. La clausula uses es Daf.Specification y la RTL -- no hay
  back-end aqui, y ese es el punto: una regla se escribe, se compone y se inspecciona
  sin que exista nada que la ejecute.
}

interface

uses
  Daf.Specification;

type
  /// <summary>Una entidad llana: propiedades publicas y ninguna clase base.</summary>
  TProduct = class
  private
    FName: string;
    FCategory: string;
    FPrice: Variant;
    FStock: Integer;
  public
    property Name: string read FName write FName;
    property Category: string read FCategory write FCategory;
    /// Variant, para que pueda llevar el NULL de un producto sin precio.
    property Price: Variant read FPrice write FPrice;
    property Stock: Integer read FStock write FStock;
  end;

  /// <summary>
  ///   El metamodelo: una clase acompañante escrita a mano que expone los miembros de
  ///   la entidad como hojas de expresion. Es opcional -- la via de ejecucion llega a
  ///   todo -- y lo que aporta es que ProductMeta.Pric no compile.
  /// </summary>
  ProductMeta = class
  public
    class var Name: TPropExpr<TProduct>;
    class var Category: TPropExpr<TProduct>;
    class var Price: TPropExpr<TProduct>;
    class var Stock: TPropExpr<TProduct>;
    class constructor Create;
  end;

  /// <summary>Las reglas del catalogo, como valores que se pueden mirar por dentro.</summary>
  TCatalogRules = record
    class function Cheap: TExpr; static;
    class function InStock: TExpr; static;
    class function Unpriced: TExpr; static;
    class function Page(const Category: string; Skip, Take: Int64): TSpecification<TProduct>; static;
  end;

implementation

{ ProductMeta }

class constructor ProductMeta.Create;
begin
  // Cada hoja valida su nombre contra el RTTI de TProduct al inicializarse la clase, asi
  // que un nombre caducado tira el arranque en vez de fallar a mitad de una consulta.
  Name := TPropExpr<TProduct>.Create('Name');
  Category := TPropExpr<TProduct>.Create('Category');
  Price := TPropExpr<TProduct>.Create('Price');
  Stock := TPropExpr<TProduct>.Create('Stock');
end;

{ TCatalogRules }

class function TCatalogRules.Cheap: TExpr;
begin
  Result := ProductMeta.Price.Between(0, 50);
end;

class function TCatalogRules.InStock: TExpr;
begin
  Result := ProductMeta.Stock > 0;
end;

class function TCatalogRules.Unpriced: TExpr;
begin
  Result := ProductMeta.Price.IsNull;
end;

class function TCatalogRules.Page(const Category: string; Skip, Take: Int64): TSpecification<TProduct>;
begin
  Result := TSpecification<TProduct>.New
    .Where((ProductMeta.Category = Category) and InStock)
    .OrderBy('Name')
    .Skip(Skip)
    .Take(Take);
end;

end.
