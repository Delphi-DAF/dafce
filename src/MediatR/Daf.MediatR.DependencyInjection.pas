unit Daf.MediatR.DependencyInjection;

interface
uses
  System.Rtti,
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  TMediatRServiceCollection = record helper for IServiceCollection
  public
    procedure AddMediatR;
    procedure AddMediatRClasses(const Package: TRttiPackage);
    procedure AddMediatRBehaviors(const Package: TRttiPackage);
  end;

  // In case of previous helper goes shadowed
  MediatR = class
  public
    class procedure AddTo(const ServiceCollection: IServiceCollection);overload;
    class procedure AddTo(const ServiceCollection: IServiceCollection; const Package: TRttiPackage);overload;
  end;

implementation

uses
  System.Generics.Collections,
  Daf.Rtti,
  Daf.MediatR.Contracts,
  Daf.MediatR.Mediator;

{ TMediatRServiceCollection }

var Visited: TArray<string>;
var VisitedBehaviors: TArray<string>;
procedure TMediatRServiceCollection.AddMediatR;
begin
  if Contains<IMediatorImpl> then
  Exit;
  Visited := nil;
  VisitedBehaviors := nil;
  AddTransient<IMediatorImpl, TMediator>;
end;

procedure TMediatRServiceCollection.AddMediatRClasses(const Package: TRttiPackage);
begin
  if TArray.Contains(Visited, Package.Name) then Exit;
  var More: TArray<string> := [Package.Name];
  Visited := TArray.Concat<string>([Visited, More]);
  var ServiceCollection := Self;
  Package.DiscoverImpl<IBaseHandler>(True,
    function(T: TRttiType): Boolean
    begin
      Result := not T.HasAttribute<MediatorAbstractAttribute>;
    end,
    procedure(RIntf: TRttiInterfaceType; RClass: TRttiInstanceType)
    begin
      ServiceCollection.AddTransient(RIntf.Handle, RClass.MetaclassType);
    end);
end;

procedure TMediatRServiceCollection.AddMediatRBehaviors(const Package: TRttiPackage);
begin
  if TArray.Contains(VisitedBehaviors, Package.Name) then Exit;
  var More: TArray<string> := [Package.Name];
  VisitedBehaviors := TArray.Concat<string>([VisitedBehaviors, More]);
  var ServiceCollection := Self;
  Package.DiscoverImpl<IBasePipelineBehavior>(True,
    function(T: TRttiType): Boolean
    begin
      Result := not T.HasAttribute<MediatorAbstractAttribute>;
    end,
    procedure(RIntf: TRttiInterfaceType; RClass: TRttiInstanceType)
    begin
      ServiceCollection.AddTransient(RIntf.Handle, RClass.MetaclassType);
    end);
end;

{ MediatR }

class procedure MediatR.AddTo(const ServiceCollection: IServiceCollection; const Package: TRttiPackage);
begin
  ServiceCollection.AddMediatRClasses(Package);
end;

class procedure MediatR.AddTo(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddMediatR;
end;

end.
