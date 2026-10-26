program LinqPipeline;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Daf.FilterSpec,
  Daf.Linq,
  LinqPipeline.Sales in 'LinqPipeline.Sales.pas';

var
  /// Cuenta cuantos elementos llega a tocar el proyector. Es la unica forma de ver
  /// desde fuera que un operador es perezoso: el resultado de Take(2) es el mismo
  /// consumiendo dos elementos que consumiendo los seis.
  Mapped: Integer = 0;

procedure Section(const Title: string);
begin
  Writeln;
  Writeln('== ', Title);
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  var Orders := TSales.Orders;

  Section('Filtrar, ordenar y quedarse con los primeros');
  for var O in Linq.From<TOrder>(Orders)
                 .Where(TSales.Large)
                 .OrderBy(TSales.ByTotalDesc)
                 .Take(2) do
    Writeln('  ', O.ToString);

  Section('El filtro se compone antes de entrar en la consulta');
  var LargeInNorth := TSales.Large and TSales.InNorth;
  for var O in Linq.From<TOrder>(Orders).Where(LargeInNorth) do
    Writeln('  ', O.ToString);
  Writeln('  y su negacion deja ', Length(Linq.From<TOrder>(Orders).Where(not LargeInNorth).ToArray),
          ' de ', Length(Orders));

  Section('Proyectar a otro tipo');
  for var Line in Linq.From<TOrder>(Orders)
                    .Where(TSales.InNorth)
                    .Select<string>(
                      function(O: TOrder): string
                      begin
                        Result := Format('%s debe %.2f', [O.Customer, O.Total]);
                      end) do
    Writeln('  ', Line);

  Section('Agrupar por clave');
  for var G in Linq.From<TOrder>(Orders).GroupBy<string>(
                 function(O: TOrder): string
                 begin
                   Result := O.Region;
                 end) do
    Writeln(Format('  %-6s %d pedido(s)', [G.Key, Length(G.Members)]));

  Section('Perezoso: Take deja de consumir la fuente');
  Mapped := 0;
  var FirstTwo := Linq.From<TOrder>(Orders)
                    .Select<string>(
                      function(O: TOrder): string
                      begin
                        Inc(Mapped);
                        Result := O.Customer;
                      end)
                    .Take(2)
                    .ToArray;
  Writeln(Format('  %d de %d pedidos proyectados para devolver %d',
                 [Mapped, Length(Orders), Length(FirstTwo)]));

  Section('Una secuencia vacia y un Default(T) no son lo mismo');
  var OnMars := Linq.From<TOrder>(Orders).Where(
    function(O: TOrder): Boolean
    begin
      Result := O.Region = 'Marte';
    end);
  var Found: TOrder;
  Writeln('  TryFirst contesta ', BoolToStr(OnMars.TryFirst(Found), True),
          ', y FirstOrDefault devuelve Id=', OnMars.FirstOrDefault.Id);
  Writeln('  IsEmpty contesta ', BoolToStr(OnMars.IsEmpty, True));

  Section('Un terminal no consume la consulta');
  var Northern := Linq.From<TOrder>(Orders).Where(TSales.InNorth);
  Writeln('  contados ', Length(Northern.ToArray), ' y otra vez ', Length(Northern.ToArray));
  Writeln('  el mayor de ellos es ', Northern.OrderBy(TSales.ByTotalDesc).FirstOrDefault.Customer);

  Writeln;
end.
