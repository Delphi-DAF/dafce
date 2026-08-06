program CatalogServiceSample;

{
  The hexagonal claim, demonstrated rather than asserted.

  Catalog.Domain declares the entity, its query rules and a service, and its uses
  clause names Daf.Query.Expressions and nothing else. This file — the composition
  root — is the only place that names an adapter, and the only place that links
  SQLCute at all.

  Swapping the in-memory adapter for a database-backed one would be an edit to
  this file. The domain would not change, and neither would its rules.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.Core,                 // registers the IQuery factory TQuery.New uses
  Daf.SQLCute.Compiler,             // to show the same rule as SQL
  Daf.Query.Expressions,
  Daf.SQLCute.Expressions,          // the adapter — named here and nowhere else
  Catalog.Domain in 'Catalog.Domain.pas';

// ---------------------------------------------------------------------------
//  The data. Somewhere in a real application this came from a database, a file,
//  or a service call. Once it is loaded, the same rules run over it.
// ---------------------------------------------------------------------------

function LoadCatalog: TObjectList<TProduct>;
begin
  Result := TObjectList<TProduct>.Create(True {owns});
  Result.Add(TProduct.Create('Anvil',   'tools', 120,  True));
  Result.Add(TProduct.Create('Bolt',    'parts', 5,    True));
  Result.Add(TProduct.Create('Crane',   'tools', 900,  False));
  Result.Add(TProduct.Create('Drill',   'tools', Null, True));    // unpriced
  Result.Add(TProduct.Create('Engine',  'parts', 150,  True));
  Result.Add(TProduct.Create('File',    'tools', 5,    False));
  Result.Add(TProduct.Create('Gasket',  'parts', 2,    True));
end;

procedure Show(const Title: string; const Items: TArray<TProduct>);
begin
  Writeln;
  Writeln(Title);
  Writeln(StringOfChar('-', Length(Title)));
  if Length(Items) = 0 then
    Writeln('  (none)');
  for var P in Items do
    if VarIsNull(P.Price) then
      Writeln(Format('  %-8s %-6s %8s  %s',
        [P.Name, P.Category, 'NULL', BoolToStr(P.Active, True)]))
    else
      Writeln(Format('  %-8s %-6s %8d  %s',
        [P.Name, P.Category, Integer(P.Price), BoolToStr(P.Active, True)]));
end;

// ---------------------------------------------------------------------------
//  The same rule, seen as SQL. Nothing in the domain asked for this — it is here
//  to make the point that the rule the service just ran is the rule a database
//  would receive, from one emission.
// ---------------------------------------------------------------------------

procedure ShowAsSql;
begin
  var Q := TExprEmitter.Apply<TProduct>(
    TQuery.New.From('Product'), TCatalogRules.Bargains);

  var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
  var Compiled := Q.Compile(Compiler);

  Writeln;
  Writeln('The same rule, emitted once and compiled for a database');
  Writeln('-------------------------------------------------------');
  Writeln('  ', Compiled.SQL);
  Write('  bindings: ');
  for var B in Compiled.Bindings do
    Write(VarToStr(B), ' ');
  Writeln;
end;

begin
  try
    var Catalog := LoadCatalog;
    try
      // --- the composition root: the one place an adapter is named -----------
      var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Catalog);
      var Service := TCatalogService.Create(Repo);
      try
        Show('Bargains (active, under 50, cheapest first)', Service.Bargains);
        Show('Category "tools", by name', Service.InCategory('tools'));
        Show('Active tools, priciest first', Service.ActiveIn('tools'));
        Show('Unpriced (Price IS NULL)', Service.Unpriced);
        Show('Top 2 active by price', Service.TopByPrice(2));

        // Five rules, one loaded list. Nothing was reloaded, and a repeated rule
        // would not even recompile its plan.
        Writeln;
        Writeln(Format('Plans compiled: %d - over one collection, loaded once.',
          [TInMemoryRepository<TProduct>(Repo).Compiled]));
      finally
        Service.Free;
      end;

      ShowAsSql;
    finally
      Catalog.Free;      // the repository referenced the list; it never owned it
    end;
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
