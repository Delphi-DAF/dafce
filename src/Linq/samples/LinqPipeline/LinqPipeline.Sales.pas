unit LinqPipeline.Sales;

{
  El dominio del sample. La clausula uses es la RTL y Daf.FilterSpec, que es de
  Commons: un filtro es un valor de Commons, no un tipo de Linq, y por eso el mismo
  valor sirve a la API perezosa de aqui y a la ansiosa de TSmartArray.
}

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.FilterSpec;

type
  TOrder = record
    Id: Integer;
    Customer: string;
    Region: string;
    Total: Currency;
    constructor Create(AId: Integer; const ACustomer, ARegion: string; ATotal: Currency);
    function ToString: string;
  end;

  /// <summary>
  ///   Las reglas del dominio como valores. Un TFilterSpec se compone con and / or /
  ///   not antes de llegar a Where, asi que una regla nueva es una expresion sobre las
  ///   que ya hay y no una copia del predicado con una condicion mas.
  /// </summary>
  TSales = record
    class function Orders: TArray<TOrder>; static;
    class function Large: TFilterSpec<TOrder>; static;
    class function InNorth: TFilterSpec<TOrder>; static;
    class function ByTotalDesc: IComparer<TOrder>; static;
  end;

implementation

{ TOrder }

constructor TOrder.Create(AId: Integer; const ACustomer, ARegion: string; ATotal: Currency);
begin
  Id := AId;
  Customer := ACustomer;
  Region := ARegion;
  Total := ATotal;
end;

function TOrder.ToString: string;
begin
  Result := Format('#%d %-9s %-6s %10.2f', [Id, Customer, Region, Total]);
end;

{ TSales }

class function TSales.Orders: TArray<TOrder>;
begin
  Result := [
    TOrder.Create(1, 'Acme', 'Norte', 1250.00),
    TOrder.Create(2, 'Brio', 'Sur', 340.00),
    TOrder.Create(3, 'Cima', 'Norte', 980.50),
    TOrder.Create(4, 'Delta', 'Este', 2100.75),
    TOrder.Create(5, 'Eolo', 'Norte', 75.20),
    TOrder.Create(6, 'Faro', 'Sur', 1500.00)];
end;

class function TSales.Large: TFilterSpec<TOrder>;
begin
  Result := TPredicate<TOrder>(
    function(O: TOrder): Boolean
    begin
      Result := O.Total >= 1000;
    end);
end;

class function TSales.InNorth: TFilterSpec<TOrder>;
begin
  Result := TPredicate<TOrder>(
    function(O: TOrder): Boolean
    begin
      Result := O.Region = 'Norte';
    end);
end;

class function TSales.ByTotalDesc: IComparer<TOrder>;
begin
  Result := TComparer<TOrder>.Construct(
    function(const L, R: TOrder): Integer
    begin
      Result := TComparer<Currency>.Default.Compare(R.Total, L.Total);
    end);
end;

end.
