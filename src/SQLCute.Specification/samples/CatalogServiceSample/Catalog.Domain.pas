unit Catalog.Domain;

{
  The domain. Look at the uses clause: Daf.Specification and the RTL, and
  that is all. No SQLCute, no adapter, no connection, no driver.

  That is the whole point of the sample. This unit states query rules, hands them
  to a port, and could not name a database if it wanted to — the port's signature
  mentions no SQL and constrains TProduct to no base class.
}

interface

uses
  Daf.Specification;

type
  /// <summary>A plain entity: public properties, a parameterless constructor, no base class.</summary>
  TProduct = class
  private
    FName: string;
    FCategory: string;
    FPrice: Variant;
    FActive: Boolean;
  public
    constructor Create; overload;
    constructor Create(const AName, ACategory: string; const APrice: Variant;
      AActive: Boolean); overload;
    property Name: string read FName write FName;
    property Category: string read FCategory write FCategory;
    /// Variant, so it can carry SQL NULL — an unpriced product.
    property Price: Variant read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
  end;

  /// <summary>
  ///   The rules this domain knows, as values. Each is inspectable, assertable
  ///   with no database, and translatable by construction — there is no overload
  ///   of Where that would accept a closure.
  /// </summary>
  TCatalogRules = record
    class function Bargains: TSpecification<TProduct>; static;
    class function InCategory(const Category: string): TSpecification<TProduct>; static;
    class function Unpriced: TSpecification<TProduct>; static;
    class function TopByPrice(Count: Integer): TSpecification<TProduct>; static;
  end;

  /// <summary>
  ///   A domain service. It depends on the port and on nothing else, so it is
  ///   testable with a five-line stub and knows nothing about where its products
  ///   come from.
  /// </summary>
  TCatalogService = class
  strict private
    FProducts: IRepository<TProduct>;
  public
    constructor Create(const Products: IRepository<TProduct>);
    function Bargains: TArray<TProduct>;
    function InCategory(const Category: string): TArray<TProduct>;
    function Unpriced: TArray<TProduct>;
    function TopByPrice(Count: Integer): TArray<TProduct>;
    /// <summary>A rule composed here rather than declared above — rules are values.</summary>
    function ActiveIn(const Category: string): TArray<TProduct>;
  end;

implementation

{ TProduct }

constructor TProduct.Create;
begin
  inherited Create;
end;

constructor TProduct.Create(const AName, ACategory: string; const APrice: Variant;
  AActive: Boolean);
begin
  inherited Create;
  FName := AName;
  FCategory := ACategory;
  FPrice := APrice;
  FActive := AActive;
end;

{ TCatalogRules }

class function TCatalogRules.Bargains: TSpecification<TProduct>;
begin
  Result := TSpecification<TProduct>.New
    .Where((Prop('Price') < 50) and (Prop('Active') = True))
    .OrderBy('Price');
end;

class function TCatalogRules.InCategory(const Category: string): TSpecification<TProduct>;
begin
  Result := TSpecification<TProduct>.New
    .Where(Prop('Category') = Category)
    .OrderBy('Name');
end;

class function TCatalogRules.Unpriced: TSpecification<TProduct>;
begin
  // IsNull, not "= Null": comparing against NULL is unknown, in SQL and here.
  Result := TSpecification<TProduct>.New.Where(Prop('Price').IsNull);
end;

class function TCatalogRules.TopByPrice(Count: Integer): TSpecification<TProduct>;
begin
  Result := TSpecification<TProduct>.New
    .Where(Prop('Active') = True)
    .OrderByDesc('Price')
    .Take(Count);
end;

{ TCatalogService }

constructor TCatalogService.Create(const Products: IRepository<TProduct>);
begin
  inherited Create;
  FProducts := Products;
end;

function TCatalogService.Bargains: TArray<TProduct>;
begin
  Result := FProducts.List(TCatalogRules.Bargains);
end;

function TCatalogService.InCategory(const Category: string): TArray<TProduct>;
begin
  Result := FProducts.List(TCatalogRules.InCategory(Category));
end;

function TCatalogService.Unpriced: TArray<TProduct>;
begin
  Result := FProducts.List(TCatalogRules.Unpriced);
end;

function TCatalogService.TopByPrice(Count: Integer): TArray<TProduct>;
begin
  Result := FProducts.List(TCatalogRules.TopByPrice(Count));
end;

function TCatalogService.ActiveIn(const Category: string): TArray<TProduct>;
begin
  Result := FProducts.List(TSpecification<TProduct>.New
    .Where((Prop('Category') = Category) and (Prop('Active') = True))
    .OrderByDesc('Price'));
end;

end.
