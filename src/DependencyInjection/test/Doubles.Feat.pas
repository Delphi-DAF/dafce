unit Doubles.Feat;

interface

uses
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  /// Servicio de juguete comun a las specs de composicion: lo unico que se le pregunta
  /// es quien lo implementa, que es lo que distingue "gano la app" de "gano la libreria".
  /// </summary>
  IFoo = interface(IInvokable)
    ['{95C9CD92-4BC0-43A0-9E0D-83033EC456C9}']
    function Name: string;
  end;

  IBar = interface(IInvokable)
    ['{B26C1005-8E01-4989-A844-EC3C1B6AEF35}']
    function Name: string;
  end;

  TAppFoo = class(TInterfacedObject, IFoo)
  public
    function Name: string;
  end;

  TLibFoo = class(TInterfacedObject, IFoo)
  public
    function Name: string;
  end;

  TOtherFoo = class(TInterfacedObject, IFoo)
  public
    function Name: string;
  end;

  TAppBar = class(TInterfacedObject, IBar)
  public
    function Name: string;
  end;

  /// <summary>
  /// Decorador escrito a mano, sin ayuda del framework: envuelve otro IFoo y le anade
  /// una marca. Sirve tanto a las specs de Decorate como a la de extensibilidad.
  /// </summary>
  TWrappedFoo = class(TInterfacedObject, IFoo)
  strict private
    FInner: IFoo;
    FMark: string;
  public
    constructor Create(const Inner: IFoo; const Mark: string);
    function Name: string;
  end;

  TFooWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Resolved: IFoo;
    Names: TArray<string>;
    Failure: string;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    function ResolveName: string;
  end;

implementation

uses
  System.SysUtils,
  Daf.DependencyInjection;

{ TAppFoo }

function TAppFoo.Name: string;
begin
  Result := 'app';
end;

{ TLibFoo }

function TLibFoo.Name: string;
begin
  Result := 'lib';
end;

{ TOtherFoo }

function TOtherFoo.Name: string;
begin
  Result := 'other';
end;

{ TAppBar }

function TAppBar.Name: string;
begin
  Result := 'bar';
end;

{ TWrappedFoo }

constructor TWrappedFoo.Create(const Inner: IFoo; const Mark: string);
begin
  inherited Create;
  FInner := Inner;
  FMark := Mark;
end;

function TWrappedFoo.Name: string;
begin
  Result := FMark + '(' + FInner.Name + ')';
end;

{ TFooWorld }

constructor TFooWorld.Create;
begin
  inherited;
  Services := TServiceCollection.Create;
end;

destructor TFooWorld.Destroy;
begin
  Resolved := nil;
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TFooWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

function TFooWorld.ResolveName: string;
begin
  Build;
  Result := Provider.GetRequiredService<IFoo>.Name;
end;

end.
