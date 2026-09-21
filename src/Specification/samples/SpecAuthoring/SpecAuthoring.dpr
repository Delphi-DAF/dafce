program SpecAuthoring;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Daf.Nullable,
  Daf.Specification,
  SpecAuthoring.Catalog in 'SpecAuthoring.Catalog.pas';

procedure Section(const Title: string);
begin
  Writeln;
  Writeln('== ', Title);
end;

procedure Show(const Label_: string; const E: TExpr);
begin
  Writeln(Format('  %-22s %s', [Label_, ExprToText(E.Node)]));
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  Section('Las tres vias de escritura producen el mismo nodo');
  // 1. Ejecucion: el nombre es una cadena y no se comprueba contra nada.
  Show('Prop', Prop('Price') > 100);
  // 2. Proxy de entidad: el nombre se resuelve contra el RTTI de TProduct.
  var Entity := TEntityExpr<TProduct>.Create(nil);
  Show('TEntityExpr<TProduct>', Entity.Prop('Price') > 100);
  // 3. Metamodelo: el nombre es un campo, asi que una errata no compila.
  Show('ProductMeta', ProductMeta.Price > 100);

  Section('El proxy contesta ademas por los nombres del almacen');
  Writeln('  tabla  : ', Entity.Table);
  Writeln('  columna: ', Entity.Column('Price'));

  Section('Un nombre que ya no existe se queja, y dice de quien');
  try
    Entity.Prop('Pricee');
    Writeln('  (no deberia llegar aqui)');
  except
    on E: EExprMemberError do
      Writeln('  ', E.Message);
  end;

  Section('Componer: and, or, not');
  Show('barato y con stock', TCatalogRules.Cheap and TCatalogRules.InStock);
  Show('barato o sin precio', TCatalogRules.Cheap or TCatalogRules.Unpriced);
  Show('ni barato', not TCatalogRules.Cheap);

  Section('La anchura del arbol, que es acotada a proposito');
  Show('Contains', ProductMeta.Name.Contains('taladro'));
  Show('StartsWith', ProductMeta.Name.StartsWith('Ta'));
  Show('Between', ProductMeta.Price.Between(10, 200));
  Show('IsIn', ProductMeta.Category.IsIn(['Herramientas', 'Jardin']));
  Show('IsNull', ProductMeta.Price.IsNull);

  Section('Un opcional sin valor se estrecha a NULL, no a cero');
  // Lo que decide que un filtro opcional se escriba o no es de quien lo escribe; lo que
  // TExprValue resuelve es que un TNullable ausente entre en el arbol como NULL y no como
  // el Default(T), que para un Integer es un 0 perfectamente comparable.
  var Ceiling := TNullable<Integer>.Null;
  Show('ausente', Prop('Price') = TExprValue.From<Integer>(Ceiling));
  Ceiling := TNullable<Integer>.From(500);
  Show('presente', Prop('Price') = TExprValue.From<Integer>(Ceiling));

  Section('Una especificacion lleva filtro, orden y paginacion');
  var Spec := TCatalogRules.Page('Herramientas', 20, 10);
  Writeln('  filtro : ', ExprToText(Spec.Filter));
  for var Term in Spec.Order do
    if Term.Descending then
      Writeln('  orden  : ', Term.Member, ' desc')
    else
      Writeln('  orden  : ', Term.Member);
  Writeln(Format('  pagina : skip=%d take=%d', [Spec.SkipCount, Spec.TakeCount]));

  Section('Y sabe decir su clave, que es lo que hace cacheable una consulta');
  Writeln('  ', Spec.Key);
  Writeln('  la misma regla, otra pagina:');
  Writeln('  ', TCatalogRules.Page('Herramientas', 30, 10).Key);

  Writeln;
end.
