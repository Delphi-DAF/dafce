unit Reproducibility.Feat;

interface

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.Rtti,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  Daf.MediatR.Contracts,
  Daf.MediatR.DependencyInjection,
  MediatR.Feat;

type
  TScanWorld = class
  public
    HandlersFirst, HandlersSecond: Integer;
    BehaviorsFirst, BehaviorsSecond: Integer;
    constructor Create;
  end;

constructor TScanWorld.Create;
begin
  inherited;
end;

/// Devuelve cuantos tipos de servicio distintos quedan registrados tras escanear el
/// paquete: es la medida directa de "registra lo mismo", sin depender de un handler concreto.
function ScanHandlers(const Services: IServiceCollection): Integer;
begin
  // Se usan los metodos del record helper y no la clase MediatR: en esta unit el
  // identificador MediatR es el prefijo de namespace de la unit MediatR.Feat.
  Services.AddMediatR;
  Services.AddMediatRClasses(_T.PackageOf<TPingHandler>);
  Result := Services.Count;
end;

initialization

Feature('''
Feature MediatR registration is reproducible @mediatr @composition

  As a spec author
  I want composing twice in one process to give the same result
  So that a harness that builds and tears down hosts sees what it registered, every time
''')

.UseWorld<TScanWorld>

.Rule('The record of what was already scanned belongs to the collection, not to the process')

  .Scenario('Two collections scan the same package and end up equal')
    .When('two fresh collections scan the same package in the same process',
      procedure(W: TScanWorld)
      begin
        var First: IServiceCollection := TServiceCollection.Create;
        var Second: IServiceCollection := TServiceCollection.Create;
        W.HandlersFirst := ScanHandlers(First);
        W.HandlersSecond := ScanHandlers(Second);
      end)
    .&Then('the second one registered the same handlers as the first',
      procedure(W: TScanWorld)
      begin
        Expect(W.HandlersFirst).ToBeGreaterThan(0);
        Expect(W.HandlersSecond).ToEqual(W.HandlersFirst);
      end)

  .Scenario('Two live collections composed interleaved do not contaminate each other')
    .When('the behaviours of the same package are registered into two collections, interleaved',
      procedure(W: TScanWorld)
      begin
        var Package := _T.PackageOf<TOuterBehavior>;
        var First: IServiceCollection := TServiceCollection.Create;
        var Second: IServiceCollection := TServiceCollection.Create;
        First.AddMediatR;
        Second.AddMediatR;
        First.AddMediatRBehaviors(Package);
        Second.AddMediatRBehaviors(Package);
        First.AddMediatRBehaviors(Package);
        Second.AddMediatRBehaviors(Package);
        W.BehaviorsFirst := Length(First.Descriptors(TypeInfo(IPipelineBehavior)));
        W.BehaviorsSecond := Length(Second.Descriptors(TypeInfo(IPipelineBehavior)));
      end)
    .&Then('both end up with the same content, as if each had been composed alone',
      procedure(W: TScanWorld)
      begin
        Expect(W.BehaviorsFirst).ToBeGreaterThan(0);
        Expect(W.BehaviorsSecond).ToEqual(W.BehaviorsFirst);
      end)

  .Scenario('The scan itself is untouched: a package is still visited only once per collection')
    .When('the same package is scanned twice into the same collection',
      procedure(W: TScanWorld)
      begin
        var Services: IServiceCollection := TServiceCollection.Create;
        W.HandlersFirst := ScanHandlers(Services);
        W.HandlersSecond := ScanHandlers(Services);
      end)
    .&Then('the second scan adds nothing: what changed is where the record lives, not the scan',
      procedure(W: TScanWorld)
      begin
        Expect(W.HandlersSecond).ToEqual(W.HandlersFirst);
      end);

end.
