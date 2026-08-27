unit RequestedType.Feat;

{
  Una factoria escrita a mano se entera de QUE se le esta pidiendo.

  El fallo que esto arregla no lanza: una factoria registrada sin fijar sirve el mismo objeto a
  todos los cierres --un IProbe<TAlpha> con los datos de otro cierre-- y nadie se queja. Asi que
  casi ninguna afirmacion de aqui es un recuento: se afirma sobre QUE tipo vio la factoria y QUE
  objeto trajo cada resolucion. Un contenedor que le pasara siempre el mismo tipo pasaria cualquier
  spec que solo contara.

  El nombre del tipo pedido es lo unico que separa "llego su cierre" de "llego el mismo dos veces".
}

interface

uses
  System.SysUtils,
  System.TypInfo,
  Daf.Rtti,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>Argumentos de cierre. Solo tienen que ser clases distinguibles por su nombre.</summary>
  TAlpha = class
  end;

  TBeta = class
  end;

  /// <summary>
  ///   Contrato de un argumento, registrable sin fijar: T no aparece en ninguna firma, asi que el
  ///   codigo maquina es el mismo sea cual sea el cierre. SeenName trae lo que la factoria vio;
  ///   Serial numera para poder afirmar sobre QUE trae cada instancia y no solo que hay una.
  /// </summary>
  IProbe<T: class> = interface(IInvokable)
    ['{A1B2C3D4-0001-4E5F-8A9B-0C1D2E3F4A50}']
    function SeenName: string;
    function Serial: Integer;
  end;

  /// <summary>Contrato corriente, no generico: para el, el tipo pedido es el del propio servicio.</summary>
  IWidget = interface(IInvokable)
    ['{A1B2C3D4-0002-4E5F-8A9B-0C1D2E3F4A50}']
    function Seen: string;
  end;

  /// <summary>Un segundo contrato corriente, para tener las dos formas en una sola composicion.</summary>
  IGadget = interface(IInvokable)
    ['{A1B2C3D4-0003-4E5F-8A9B-0C1D2E3F4A50}']
    function Seen: string;
  end;

  TProbe<T: class> = class(TInterfacedObject, IProbe<T>)
  strict private
    FSeen: string;
    FSerial: Integer;
  public
    // Compartido por todos los cierres a proposito: TProbe<_> es UNA sola clase, y contar
    // construcciones es lo unico para lo que un valor comun vale.
    class var Built: Integer;
    constructor Create(const ASeen: string);
    function SeenName: string;
    function Serial: Integer;
  end;

  TWidget = class(TInterfacedObject, IWidget)
  strict private
    FSeen: string;
  public
    constructor Create(const ASeen: string);
    function Seen: string;
  end;

  TGadget = class(TInterfacedObject, IGadget)
  strict private
    FSeen: string;
  public
    constructor Create(const ASeen: string);
    function Seen: string;
  end;

  TRequestedWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Told: string;
    Same: Boolean;
    Other: Boolean;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    /// Captura clase y mensaje del fallo, o un centinela si no lo hubo: afirmar solo que "fallo"
    /// da por bueno cualquier fallo, incluido uno que no viene al caso.
    procedure Attempt(const Action: TProc);
  end;

const
  SinFallo = '<sin fallo>';

/// <summary>
///   El nombre del argumento del tipo pedido -- lo que un ILogger<T> usaria como categoria. Para
///   un contrato corriente no hay argumento, asi que devuelve el nombre del propio tipo; para nil,
///   un centinela. Asi el dato significa algo en los dos casos.
/// </summary>
function FirstArgName(const PInfo: PTypeInfo): string;

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection;

function FirstArgName(const PInfo: PTypeInfo): string;
begin
  if PInfo = nil then Exit('<nada>');
  var Args := _T.GenericArgs(PInfo);
  if Length(Args) = 0 then Exit(string(PInfo.Name));
  Result := string(Args[0].Name);
end;

{ TProbe<T> }

constructor TProbe<T>.Create(const ASeen: string);
begin
  inherited Create;
  Inc(Built);
  FSerial := Built;
  FSeen := ASeen;
end;

function TProbe<T>.SeenName: string;
begin
  Result := FSeen;
end;

function TProbe<T>.Serial: Integer;
begin
  Result := FSerial;
end;

{ TWidget }

constructor TWidget.Create(const ASeen: string);
begin
  inherited Create;
  FSeen := ASeen;
end;

function TWidget.Seen: string;
begin
  Result := FSeen;
end;

{ TGadget }

constructor TGadget.Create(const ASeen: string);
begin
  inherited Create;
  FSeen := ASeen;
end;

function TGadget.Seen: string;
begin
  Result := FSeen;
end;

{ TRequestedWorld }

constructor TRequestedWorld.Create;
begin
  inherited;
  TProbe<_>.Built := 0;
  Told := '';
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TRequestedWorld.Destroy;
begin
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TRequestedWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TRequestedWorld.Attempt(const Action: TProc);
begin
  Outcome := SinFallo;
  try
    Action;
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

// --- Feature definition ---

initialization

Feature('''
Feature A hand-written factory knows what it is being asked for @di @factory @requested-type

  As an application author
  I want a hand-written factory to receive the type being resolved, not only the provider
  So that one registration serving more than one type can build what each one asked for
''')

.UseWorld<TRequestedWorld>

.Rule('A factory can receive the type being asked for')

  .Scenario('The closure that was resolved is the one that arrives')
    .Given('a generic contract registered open, with a hand-written factory that records what it saw',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IProbe<_>>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IProbe<_>
          begin
            Result := TProbe<_>.Create(FirstArgName(RequestedType));
          end);
      end)
    .When('two different closures are resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IProbe<TAlpha>>.SeenName + '|' +
                  W.Provider.GetRequiredService<IProbe<TBeta>>.SeenName;
      end)
    .&Then('the factory saw each closure, and not the same one twice',
      procedure(W: TRequestedWorld)
      begin
        // Sobre el NOMBRE que vio, no sobre que resolviera: pasar siempre el tipo del registro
        // daria "_|_", el marcador dos veces, que es justo el fallo que esto cierra.
        Expect(W.Told).ToEqual('TAlpha|TBeta');
      end)

  .Scenario('Each closure gets what its type asked for')
    .Given('the same open contract, with a factory that builds according to the type it receives',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IProbe<_>>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IProbe<_>
          begin
            var Args := _T.GenericArgs(RequestedType);
            var Made := '<sin cierre>';
            if Length(Args) > 0 then
            begin
              if Args[0] = System.TypeInfo(TAlpha) then Made := 'made-for-alpha'
              else if Args[0] = System.TypeInfo(TBeta) then Made := 'made-for-beta'
              else Made := 'made-for-' + string(Args[0].Name);
            end;
            Result := TProbe<_>.Create(Made);
          end);
      end)
    .When('two different closures are resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IProbe<TAlpha>>.SeenName + '|' +
                  W.Provider.GetRequiredService<IProbe<TBeta>>.SeenName;
      end)
    .&Then('each one carries the object built for its own type',
      procedure(W: TRequestedWorld)
      begin
        Expect(W.Told).ToEqual('made-for-alpha|made-for-beta');
      end)

  .Scenario('An ordinary registration receives the service type')
    .Given('a non-generic contract registered with a factory that receives the requested type',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IWidget>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IWidget
          begin
            Result := TWidget.Create(FirstArgName(RequestedType));
          end);
      end)
    .When('the service is resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IWidget>.Seen;
      end)
    .&Then('the factory received the type of the service itself, not nothing',
      procedure(W: TRequestedWorld)
      begin
        // '<nada>' seria el centinela de nil: el dato significa algo tambien en un registro
        // corriente, y es el tipo del servicio.
        Expect(W.Told).ToEqual('IWidget');
      end)

.Rule('The factory that does not ask still registers the same way')

  .Scenario('A registration written before the change still works')
    .Given('a factory that only takes the provider',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IWidget>(
          function(P: IServiceProvider): IWidget
          begin
            Result := TWidget.Create('short-form');
          end);
      end)
    .When('the service is resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Told := W.Provider.GetRequiredService<IWidget>.Seen; end);
      end)
    .&Then('it resolves exactly as before',
      procedure(W: TRequestedWorld)
      begin
        // Sobre lo que devuelve, no sobre que no lanzara: la forma corta no paso a ser un no-op.
        Expect(W.Told).ToEqual('short-form');
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

  .Scenario('Both forms live in the same composition')
    .Given('one service with the short form and another with the one that receives the type',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IWidget>(
          function(P: IServiceProvider): IWidget
          begin
            Result := TWidget.Create('short-form');
          end);
        W.Services.AddSingleton<IGadget>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IGadget
          begin
            Result := TGadget.Create(FirstArgName(RequestedType));
          end);
      end)
    .When('both are resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IWidget>.Seen + '|' +
                  W.Provider.GetRequiredService<IGadget>.Seen;
      end)
    .&Then('each factory received what it declared to receive',
      procedure(W: TRequestedWorld)
      begin
        // La corta no pregunta y sirve lo suyo; la consciente recibe el tipo del servicio.
        Expect(W.Told).ToEqual('short-form|IGadget');
      end)

  .Scenario('Another registration path also hands over the requested type')
    // Una via que solo existiera para AddSingleton seria una trampa: aqui se registra como
    // transient para ver que la sobrecarga resuelve y baja igual en otro ciclo de vida.
    .Given('an open registration made transient with a factory that records what it saw',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddTransient<IProbe<_>>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IProbe<_>
          begin
            Result := TProbe<_>.Create(FirstArgName(RequestedType));
          end);
      end)
    .When('two different closures are resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        var Alpha := W.Provider.GetRequiredService<IProbe<TAlpha>>;
        var Beta := W.Provider.GetRequiredService<IProbe<TBeta>>;
        W.Told := Alpha.SeenName + '|' + Beta.SeenName;
      end)
    .&Then('each one still saw its own closure',
      procedure(W: TRequestedWorld)
      begin
        Expect(W.Told).ToEqual('TAlpha|TBeta');
      end)

  .Scenario('And a TryAdd path too')
    .Given('an open registration made with TryAddScoped and a factory that records what it saw',
      procedure(W: TRequestedWorld)
      begin
        W.Services.TryAddScoped<IProbe<_>>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IProbe<_>
          begin
            Result := TProbe<_>.Create(FirstArgName(RequestedType));
          end);
      end)
    .When('a closure is resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IProbe<TAlpha>>.SeenName;
      end)
    .&Then('it too saw the closure that was asked for',
      procedure(W: TRequestedWorld)
      begin
        Expect(W.Told).ToEqual('TAlpha');
      end)

.Rule('The lifetime and identity do not change with the shape of the factory')

  .Scenario('A shared registration still counts per requested type')
    .Given('an open registration with a shared lifetime and a factory that receives the type',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IProbe<_>>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IProbe<_>
          begin
            Result := TProbe<_>.Create(FirstArgName(RequestedType));
          end);
      end)
    .When('the same type is resolved twice and another one once',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        var One := W.Provider.GetRequiredService<IProbe<TAlpha>>;
        var Again := W.Provider.GetRequiredService<IProbe<TAlpha>>;
        var Beta := W.Provider.GetRequiredService<IProbe<TBeta>>;
        W.Same := One = Again;
        W.Other := One = IProbe<TAlpha>(Pointer(Beta));
        W.Told := Format('%d|%d|%d', [One.Serial, Again.Serial, Beta.Serial]);
      end)
    .&Then('the first gives the same instance twice, and the other one of its own',
      procedure(W: TRequestedWorld)
      begin
        // Los seriales dicen QUE trae cada uno: el mismo cierre reusa el 1, el otro cierre es el 2.
        Expect(W.Told).ToEqual('1|1|2');
      end)
    .&And('and the same closure is literally the same reference, the other is not',
      procedure(W: TRequestedWorld)
      begin
        Expect(W.Same).ToBeTrue;
        Expect(W.Other).ToBeFalse;
      end)

  .Scenario('A concrete registration still wins')
    .Given('an open factory registration and, on top of it, a concrete one for one closure',
      procedure(W: TRequestedWorld)
      begin
        W.Services.AddSingleton<IProbe<_>>(
          function(P: IServiceProvider; RequestedType: PTypeInfo): IProbe<_>
          begin
            Result := TProbe<_>.Create(FirstArgName(RequestedType));
          end);
        W.Services.AddSingleton<IProbe<TAlpha>>(
          function(P: IServiceProvider): IProbe<TAlpha>
          begin
            Result := TProbe<TAlpha>.Create('fixed');
          end);
      end)
    .When('both closures are resolved',
      procedure(W: TRequestedWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IProbe<TAlpha>>.SeenName + '|' +
                  W.Provider.GetRequiredService<IProbe<TBeta>>.SeenName;
      end)
    .&Then('the concrete one comes from its own registration, and the other from the open one',
      procedure(W: TRequestedWorld)
      begin
        Expect(W.Told).ToEqual('fixed|TBeta');
      end);

end.
