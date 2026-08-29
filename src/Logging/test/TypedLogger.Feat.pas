unit TypedLogger.Feat;

{
  ILogger<T>: un servicio pide un registrador y le llega uno que YA lleva su categoria, sin
  haberla escrito y sin una linea de composicion por cada clase que lo quiera.

  El contrato llevaba declarado desde siempre y no lo usaba nadie, porque no habia forma de
  registrarlo. Esto es lo que demuestra que la forma nueva de registro se paga sola.
}

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>Un proveedor que se queda con la CATEGORIA de cada entrada. Mirar el mensaje no
  /// serviria: lo que hay que ver es bajo que nombre aparece.</summary>
  ICategorySpy = interface(ILoggerProvider)
    ['{7D4A1E96-3F82-4C50-B1A7-0E5C93D6B248}']
    function Lines: string;
  end;

  IPayroll = interface(IInvokable)
    ['{2F60C8B3-59D1-4E27-9A84-B0C1E7F35A69}']
    procedure Run;
  end;

  IInvoicing = interface(IInvokable)
    ['{81C3A5D7-0E46-4B92-8F1D-3A7B6C204E58}']
    procedure Run;
  end;

  /// <summary>
  ///   Dos servicios corrientes. Ninguno declara su categoria y nadie registra nada por ellos:
  ///   piden ILogger tipado por si mismos y ya viene con su nombre puesto.
  /// </summary>
  TPayrollService = class(TInterfacedObject, IPayroll)
  strict private
    FLog: ILogger<TPayrollService>;
  public
    constructor Create(const Log: ILogger<TPayrollService>);
    procedure Run;
  end;

  TInvoicingService = class(TInterfacedObject, IInvoicing)
  strict private
    FLog: ILogger<TInvoicingService>;
  public
    constructor Create(const Log: ILogger<TInvoicingService>);
    procedure Run;
  end;

  TTypedLoggerWorld = class
  strict private
    FProvider: IServiceProvider;
  public
    Services: IServiceCollection;
    Spy: ICategorySpy;
    Told: string;
    constructor Create;
    destructor Destroy; override;
    /// La composicion por defecto y nada mas: un proveedor, y ni una linea por servicio.
    procedure Compose;
    function Provider: IServiceProvider;
  end;

function NewCategorySpy: ICategorySpy;
/// La categoria que le toca a una clase, calculada a partir de la clase misma.
function CategoryOf(const AClass: TClass): string;

implementation

uses
  System.TypInfo,
  Daf.MiniSpec,
  Daf.Logging,
  DAF.Logging.Builder,
  Daf.DependencyInjection;

type
  TCategorySpy = class(TInterfacedObject, ILoggerProvider, ICategorySpy)
  strict private
    FLines: TArray<string>;
  public
    function CreateLogger(const Category: string): ILogger;
    function Lines: string;
    procedure Took(const Line: string);
  end;

  TSpyLogger = class(TLogger)
  strict private
    FOwner: TCategorySpy;
  public
    constructor Create(const Category: string; const Owner: TCategorySpy);
    procedure Log(const Entry: TLogEntry); override;
  end;

{ TSpyLogger }

constructor TSpyLogger.Create(const Category: string; const Owner: TCategorySpy);
begin
  inherited Create(Category);
  FOwner := Owner;
end;

procedure TSpyLogger.Log(const Entry: TLogEntry);
begin
  // La categoria de la ENTRADA, no la del logger que la recibe: es la que viaja hasta el destino
  // y la unica que un destino real puede escribir.
  FOwner.Took(Entry.Category);
end;

{ TCategorySpy }

function TCategorySpy.CreateLogger(const Category: string): ILogger;
begin
  Result := TSpyLogger.Create(Category, Self);
end;

procedure TCategorySpy.Took(const Line: string);
begin
  FLines := FLines + [Line];
end;

function TCategorySpy.Lines: string;
begin
  Result := string.Join('|', FLines);
end;

function NewCategorySpy: ICategorySpy;
begin
  Result := TCategorySpy.Create;
end;

function CategoryOf(const AClass: TClass): string;
begin
  Result := LogCategoryOf(AClass.ClassInfo);
end;

{ TPayrollService }

constructor TPayrollService.Create(const Log: ILogger<TPayrollService>);
begin
  inherited Create;
  FLog := Log;
end;

procedure TPayrollService.Run;
begin
  FLog.LogInformation('the payroll ran');
end;

{ TInvoicingService }

constructor TInvoicingService.Create(const Log: ILogger<TInvoicingService>);
begin
  inherited Create;
  FLog := Log;
end;

procedure TInvoicingService.Run;
begin
  FLog.LogInformation('the invoices went out');
end;

{ TTypedLoggerWorld }

constructor TTypedLoggerWorld.Create;
begin
  inherited;
  Services := TServiceCollection.Create;
  Spy := NewCategorySpy;
  Told := '';
end;

destructor TTypedLoggerWorld.Destroy;
begin
  if FProvider <> nil then
    FProvider.ShutDown;
  inherited;
end;

procedure TTypedLoggerWorld.Compose;
begin
  var Watcher := Spy;
  AddLogging(Services,
    procedure(Builder: ILoggingBuilder)
    begin
      Builder.AddProvider(Watcher);
    end);
end;

function TTypedLoggerWorld.Provider: IServiceProvider;
begin
  if FProvider = nil then
    FProvider := Services.BuildServiceProvider;
  Result := FProvider;
end;

initialization

Feature('''
Feature A service gets a logger that already carries its own category @logging @typed @generics

  As someone writing a service
  I want the logger I am handed to be labelled with my own type
  So that the category cannot drift from the class it names, and nobody registers one per class
''')

.UseWorld<TTypedLoggerWorld>

.Rule('The category comes from whoever asked for the logger')

  .Scenario('Two services, two categories, one registration')
    .Given('the default composition, with nothing registered per service',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Compose;
        W.Services.AddSingleton<IPayroll, TPayrollService>;
        W.Services.AddSingleton<IInvoicing, TInvoicingService>;
      end)
    .When('two different services each write through the logger they were handed',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Provider.GetRequiredService<IPayroll>.Run;
        W.Provider.GetRequiredService<IInvoicing>.Run;
        W.Told := W.Spy.Lines;
      end)
    .&Then('each entry came out under the category of the service that wrote it',
      procedure(W: TTypedLoggerWorld)
      begin
        Expect(W.Told).ToEqual(CategoryOf(TPayrollService) + '|' + CategoryOf(TInvoicingService));
      end)

  .Scenario('The category is derived from the class, not written down beside it')
    // Lo que se espera se calcula a partir de la clase, asi que si alguien la renombra esta
    // afirmacion se mueve con ella y la composicion de arriba no se toca: eso ES "renombrar
    // no deja la categoria atras".
    .Given('the default composition and one service',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Compose;
        W.Services.AddSingleton<IPayroll, TPayrollService>;
      end)
    .When('the service writes',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Provider.GetRequiredService<IPayroll>.Run;
        W.Told := W.Spy.Lines;
      end)
    .&Then('the category is the unit and name of the class itself',
      procedure(W: TTypedLoggerWorld)
      begin
        Expect(W.Told).ToEqual(CategoryOf(TPayrollService));
      end)
    .&And('and it names the class, which is what a renamed class would carry with it',
      procedure(W: TTypedLoggerWorld)
      begin
        Expect(W.Told).ToContain(TPayrollService.ClassName);
      end)

  .Scenario('Asking for the same service twice does not hand out two loggers')
    // Un registro sin fijar aplica su ciclo de vida POR CIERRE: compartido para el mismo cierre.
    .Given('the default composition',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Compose;
      end)
    .When('the same typed logger is asked for twice',
      procedure(W: TTypedLoggerWorld)
      begin
        var One := W.Provider.GetRequiredService<ILogger<TPayrollService>>;
        var Other := W.Provider.GetRequiredService<ILogger<TPayrollService>>;
        if One = Other then W.Told := 'the same one' else W.Told := 'two of them';
      end)
    .&Then('it is the same one',
      procedure(W: TTypedLoggerWorld)
      begin
        Expect(W.Told).ToEqual('the same one');
      end)

  .Scenario('And two different ones are two, each with its own category')
    // El control del anterior: sin el, un contenedor que devolviera SIEMPRE el mismo registrador
    // -- el defecto que este change existe para no tener -- tambien lo pasaria.
    .Given('the default composition',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Compose;
      end)
    .When('two different typed loggers are asked for and both write',
      procedure(W: TTypedLoggerWorld)
      begin
        W.Provider.GetRequiredService<ILogger<TPayrollService>>.LogInformation('one');
        W.Provider.GetRequiredService<ILogger<TInvoicingService>>.LogInformation('two');
        W.Told := W.Spy.Lines;
      end)
    .&Then('each wrote under its own category',
      procedure(W: TTypedLoggerWorld)
      begin
        Expect(W.Told).ToEqual(CategoryOf(TPayrollService) + '|' + CategoryOf(TInvoicingService));
      end);

end.
