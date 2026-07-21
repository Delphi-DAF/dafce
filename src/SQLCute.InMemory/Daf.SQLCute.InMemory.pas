unit Daf.SQLCute.InMemory;

{
  Daf.SQLCute.InMemory — the in-memory compiler: a second SQLCute back-end
  (analogue of IQueryCompiler) that consumes an IQuery AST and emits an
  executable plan over in-memory collections via Daf.Linq, instead of SQL text.

  Depends on Daf.Linq (the sequence engine) + Daf.SQLCute (the AST) + Commons
  (Daf.Rtti / Daf.Nullable). Only this thin compiler lives in the SQLCute
  footprint (Decision 9). The AST walker, IRow + adapters, IQueryContext and the
  RTTI materializer are added across Phases 1-3.
}

interface

implementation

end.
