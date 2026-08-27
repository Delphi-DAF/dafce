unit OpenGenerics.Feat;

{
  Registrar un contrato generico dejando sus argumentos de tipo SIN FIJAR, y que ese unico
  registro sirva a todos sus cierres.

  Tres modos de fallo de aqui se leen como acierto, y cada escenario dice cual mira:

    - un cierre que recibe la instancia de OTRO cierre: hay objeto y responde
    - un constructor que no se elige porque CanResolve dijo que no: la clase se construye
      igual, con el de TObject, y queda a medias
    - un emparejamiento invalido: no lanza, corrompe la pila

  Por eso casi ninguna afirmacion es un recuento: se afirma sobre QUE trae cada instancia.
}

interface

uses
  System.SysUtils,
  System.TypInfo,
  Daf.Rtti,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>La familia que el contrato acepta, o sea la cota.</summary>
  TSettings = class
  end;

  TSettingsClass = class of TSettings;

  TAlphaSettings = class(TSettings)
  end;

  TBetaSettings = class(TSettings)
  end;

  /// <summary>Ni TSettings ni descendiente suyo: el cierre que la cota tiene que rechazar.</summary>
  TStranger = class
  end;

  /// <summary>
  ///   Contrato de UN argumento. T solo aparece devuelto como referencia, que es la condicion
  ///   que hace registrable un contrato sin fijar: el codigo maquina de Held es el mismo sea
  ///   cual sea T, un puntero.
  /// </summary>
  IBox<T: class> = interface(IInvokable)
    ['{9E0F5B22-4E13-4B7A-9E0C-2C0B8F5A1D40}']
    function Held: T;
    function Tag: string;
    function Serial: Integer;
  end;

  /// <summary>
  ///   Contrato de DOS argumentos que se llama IGUAL que el de arriba. Existe para que la
  ///   aridad tenga algo con lo que confundirse: el nombre hasta el primer &lt; no los separa.
  /// </summary>
  IBox<T1, T2: class> = interface(IInvokable)
    ['{0C2A6F31-7B55-49E8-8B6E-1F3D4A9C7E52}']
    function Both: string;
  end;

  /// <summary>
  ///   Contrato cuyo argumento viaja POR VALOR cuando se cierra sobre algo que no es una
  ///   clase. Es el caso que hay que rechazar en vez de ejecutar.
  /// </summary>
  IValued<T> = interface(IInvokable)
    ['{5A7E3C08-6D24-4F1B-A0C9-8E2B7D6F3A14}']
    function Value: T;
  end;

  IConsumer = interface(IInvokable)
    ['{3B8D1F47-2C60-4A95-B7E3-6D9A0C5E8F21}']
    function Report: string;
  end;

  /// <summary>
  ///   El record que el contenedor rellena. Va FUERA de la clase por obligacion: dentro, T1 ya
  ///   es el parametro generico y Delphi da E2004. El tipo del campo es la cota.
  /// </summary>
  TBoxArgs = record
    T1: TSettingsClass;
  end;

  TPairArgs = record
    T1: TSettingsClass;
    T2: TSettingsClass;
  end;

  TAnyArgs = record
    T1: TClass;
  end;

  TBox<T: class> = class(TInterfacedObject, IBox<T>)
  strict private
    FTag: string;
    FSerial: Integer;
  public
    // Publico porque el contenedor lo escribe por RTTI, y la RTTI solo ve los campos publicos.
    // Se lee todo lo que haga falta; lo que no se hace nunca es asignarlo.
    // Y va ANTES del class var de abajo, no despues: en Delphi un bloque class var sigue
    // abierto hasta el siguiente especificador de visibilidad, asi que un campo escrito detras
    // es tambien un class var -- que la RTTI no expone y que todos los cierres compartirian.
    // Medido aqui mismo: escrito debajo, el registro falla diciendo que no hay donde escribir.
    _ClosedArgs: TBoxArgs;
    // Compartido por todos los cierres a proposito, y no es un descuido: TBox<_> es UNA sola
    // clase. Sirve para contar construcciones, que es lo unico para lo que un valor comun vale.
    class var Built: Integer;
    constructor Create;
    function Held: T;
    function Tag: string;
    function Serial: Integer;
  end;

  TDuo<T1, T2: class> = class(TInterfacedObject, IBox<T1, T2>)
  public
    _ClosedArgs: TPairArgs;
    function Both: string;
  end;

  TValued<T> = class(TInterfacedObject, IValued<T>)
  public
    _ClosedArgs: TAnyArgs;
    function Value: T;
  end;

  /// <summary>Sin _ClosedArgs: no hay donde escribir, y eso es un registro imposible.</summary>
  TBlindBox<T: class> = class(TInterfacedObject, IBox<T>)
  public
    function Held: T;
    function Tag: string;
    function Serial: Integer;
  end;

  /// <summary>Dos campos para un contrato de un argumento: la posicion no cuadraria.</summary>
  TWideBox<T: class> = class(TInterfacedObject, IBox<T>)
  public
    _ClosedArgs: TPairArgs;
    function Held: T;
    function Tag: string;
    function Serial: Integer;
  end;

  /// <summary>Un registro concreto para un cierre, que tiene que ganar al registro sin fijar.</summary>
  TFixedBox = class(TInterfacedObject, IBox<TAlphaSettings>)
  public
    function Held: TAlphaSettings;
    function Tag: string;
    function Serial: Integer;
  end;

  /// <summary>
  ///   Pide un cierre por constructor. Es el que caza el fallo mudo: si CanResolve no sabe del
  ///   emparejamiento, este constructor no se elige, la clase se construye con el de TObject y
  ///   Report contesta igual -- con un hueco dentro.
  /// </summary>
  TConsumer = class(TInterfacedObject, IConsumer)
  strict private
    FBox: IBox<TAlphaSettings>;
  public
    constructor Create(const Box: IBox<TAlphaSettings>);
    function Report: string;
  end;

  TOpenWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Told: string;
    Outcome: string;
    Same: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    /// Captura clase y mensaje del fallo, o un centinela si no lo hubo: afirmar solo que
    /// "fallo" da por bueno cualquier fallo, incluido uno que no tiene que ver.
    procedure Attempt(const Action: TProc);
  end;

const
  SinFallo = '<sin fallo>';

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection;

{ TBox<T> }

constructor TBox<T>.Create;
begin
  inherited Create;
  Inc(Built);
  FSerial := Built;
  // Lo que se afirma en casi todos los escenarios sale de aqui: si el contenedor no hubiera
  // escrito el campo ANTES de llegar a este constructor, no habria nada que leer.
  if _ClosedArgs.T1 = nil then
    FTag := '<sin cierre>'
  else
    FTag := _ClosedArgs.T1.ClassName;
end;

function TBox<T>.Held: T;
begin
  // Dentro de TBox<_>, T ES el marcador: no hay nada que construir aqui. Esta para que la
  // comprobacion de firmas tenga un metodo que mencione T y pueda compararlo.
  Result := nil;
end;

function TBox<T>.Serial: Integer;
begin
  Result := FSerial;
end;

function TBox<T>.Tag: string;
begin
  Result := FTag;
end;

{ TDuo<T1, T2> }

function TDuo<T1, T2>.Both: string;
begin
  Result := _ClosedArgs.T1.ClassName + '+' + _ClosedArgs.T2.ClassName;
end;

{ TValued<T> }

function TValued<T>.Value: T;
begin
  Result := Default(T);
end;

{ TBlindBox<T> }

function TBlindBox<T>.Held: T;
begin
  Result := nil;
end;

function TBlindBox<T>.Serial: Integer;
begin
  Result := 0;
end;

function TBlindBox<T>.Tag: string;
begin
  Result := 'blind';
end;

{ TWideBox<T> }

function TWideBox<T>.Held: T;
begin
  Result := nil;
end;

function TWideBox<T>.Serial: Integer;
begin
  Result := 0;
end;

function TWideBox<T>.Tag: string;
begin
  Result := 'wide';
end;

{ TFixedBox }

function TFixedBox.Held: TAlphaSettings;
begin
  Result := nil;
end;

function TFixedBox.Serial: Integer;
begin
  Result := -1;
end;

function TFixedBox.Tag: string;
begin
  Result := 'registered by hand';
end;

{ TConsumer }

constructor TConsumer.Create(const Box: IBox<TAlphaSettings>);
begin
  inherited Create;
  FBox := Box;
end;

function TConsumer.Report: string;
begin
  // Defensivo a proposito: sin esto el fallo que este escenario busca llega como una violacion
  // de acceso, y lo que hace falta es que se lea como lo que es -- una clase construida a medias.
  if FBox = nil then Exit('consumer(nothing was injected)');
  Result := 'consumer(' + FBox.Tag + ')';
end;

{ TOpenWorld }

constructor TOpenWorld.Create;
begin
  inherited;
  TBox<_>.Built := 0;
  Told := '';
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TOpenWorld.Destroy;
begin
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TOpenWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TOpenWorld.Attempt(const Action: TProc);
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
Feature One registration for every closure of a generic contract @di @generics @open

  As an application author
  I want to register a generic contract once, with its type arguments left open
  So that a default meant for any closure does not cost one composition line per type
''')

.UseWorld<TOpenWorld>

.Rule('One registration serves every closure')

  .Scenario('A registration with its arguments left open serves two closures')
    .Given('a generic contract registered without fixing its arguments',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('two different closures are resolved, neither of them registered',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := W.Provider.GetRequiredService<IBox<TAlphaSettings>>.Tag + '|' +
                      W.Provider.GetRequiredService<IBox<TBetaSettings>>.Tag;
          end);
      end)
    .&Then('each one arrives carrying its own closed type',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('TAlphaSettings|TBetaSettings');
      end)
    .&And('and nothing was raised on the way',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

  .Scenario('A closure registered by hand still wins')
    .Given('an open registration and, on top of it, a concrete one for one closure',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
        W.Services.AddSingleton<IBox<TAlphaSettings>, TFixedBox>;
      end)
    .When('both closures are resolved',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IBox<TAlphaSettings>>.Tag + '|' +
                  W.Provider.GetRequiredService<IBox<TBetaSettings>>.Tag;
      end)
    .&Then('the one registered by hand comes from its own registration, and the other from the open one',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('registered by hand|TBetaSettings');
      end)

  .Scenario('The open registration takes every lifetime, transient included')
    .Given('an open registration with a per-use lifetime',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddTransient<IBox<_>, TBox<_>>;
      end)
    .When('the same closure is resolved twice',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        var One := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        var Other := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        W.Told := Format('%s#%d|%s#%d', [One.Tag, One.Serial, Other.Tag, Other.Serial]);
      end)
    .&Then('each use got its own, and both carry the right closure',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('TAlphaSettings#1|TAlphaSettings#2');
      end)

  .Scenario('And a scoped one is one per scope')
    .Given('an open registration with a per-scope lifetime',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddScoped<IBox<_>, TBox<_>>;
      end)
    .When('the same closure is resolved twice in one scope and once in another',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        var Here := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        var Again := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        var Elsewhere := W.Provider.CreateScope.ServiceProvider.GetRequiredService<IBox<TAlphaSettings>>;
        W.Told := Format('%d|%d|%d', [Here.Serial, Again.Serial, Elsewhere.Serial]);
      end)
    .&Then('the scope decides, exactly as it does for an ordinary registration',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('1|1|2');
      end)

.Rule('The arity is part of what identifies the contract')

  .Scenario('A contract of the same name and another arity does not match')
    .Given('an open registration of a contract that takes one argument',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('a closure of the same name with two arguments is asked for',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := W.Provider.GetRequiredService<IBox<TAlphaSettings, TBetaSettings>>.Both;
          end)
      end)
    .&Then('it does not match: nothing registers that one',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('Cannot resolve service');
      end)

  .Scenario('And the other way round as well')
    .Given('an open registration of a contract that takes two arguments',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_, _>, TDuo<_, _>>;
      end)
    .When('a closure of the same name with one argument is asked for',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := W.Provider.GetRequiredService<IBox<TAlphaSettings>>.Tag;
          end)
      end)
    .&Then('it does not match either',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('Cannot resolve service');
      end)

  .Scenario('A two-argument contract serves its own closures')
    // El control de las dos de arriba: sin el, un contenedor que rechazara todos los cierres de
    // dos argumentos las pasaria igual.
    .Given('an open registration of a contract that takes two arguments',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_, _>, TDuo<_, _>>;
      end)
    .When('a closure of two arguments is resolved',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IBox<TAlphaSettings, TBetaSettings>>.Both;
      end)
    .&Then('both arguments arrive, in the order they were written',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('TAlphaSettings+TBetaSettings');
      end)

.Rule('A match that would not be safe is refused instead of entered')

  .Scenario('A contract whose argument travels by value is refused, naming the method')
    .Given('an open registration of a contract that returns its argument',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IValued<_>, TValued<_>>;
      end)
    .When('a closure whose argument is not a reference is asked for',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := IntToStr(W.Provider.GetRequiredService<IValued<Integer>>.Value);
          end)
      end)
    .&Then('it is refused rather than handed over',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('same call shape');
      end)
    .&And('and the message names the method, not just the contract',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('Value');
      end)

  .Scenario('And it is refused the same way when the registration was made with a factory')
    // El registro por clase es la forma abreviada del registro por factory: AddSingleton con una
    // clase calcula una factory por defecto y guarda la clase para que la validacion del grafo
    // tenga algo que mirar. Asi que lo que decide si un emparejamiento es seguro no puede
    // depender de por cual de las dos se haya entrado -- es una propiedad del PAR contrato
    // abierto/cierre, no de quien construya el objeto.
    .Given('an open registration made with a factory instead of a class',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IValued<_>>(
          function(P: IServiceProvider): IValued<_>
          begin
            Result := TValued<_>.Create;
          end);
      end)
    .When('a closure whose argument is not a reference is asked for',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := IntToStr(W.Provider.GetRequiredService<IValued<Integer>>.Value);
          end)
      end)
    .&Then('it is refused rather than handed over',
      procedure(W: TOpenWorld)
      begin
        // Medido antes de que esto existiera: se entregaba y devolvia 0, sin excepcion. Un
        // cero que sale de leer un puntero nil como entero es el fallo mudo entero, en una
        // sola cifra que parece un resultado.
        Expect(W.Outcome).ToContain('same call shape');
      end)
    .&And('and the message names the method, exactly as by the other route',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('Value');
      end)

  .Scenario('And a factory registration serves the safe closure, as the other route does')
    // El control de la de arriba, por la misma razon que el de la ruta por clase: sin el,
    // rechazar TODO emparejamiento hecho por factory la pasaria.
    .Given('an open registration made with a factory',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IValued<_>>(
          function(P: IServiceProvider): IValued<_>
          begin
            Result := TValued<_>.Create;
          end);
      end)
    .When('a closure whose argument is a class is asked for',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            if W.Provider.GetRequiredService<IValued<TAlphaSettings>> <> nil then
              W.Told := 'served';
          end)
      end)
    .&Then('it is served',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('served');
      end)
    .&And('and nothing was raised on the way',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

  .Scenario('A contract whose arguments travel by reference is accepted')
    // Sin este caso la escena de arriba pasaria con un contenedor que rechazara TODO
    // emparejamiento, que es peor que no comprobar nada.
    .Given('the same open registration',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IValued<_>, TValued<_>>;
      end)
    .When('a closure whose argument is a class is asked for',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            if W.Provider.GetRequiredService<IValued<TAlphaSettings>> <> nil then
              W.Told := 'served';
          end)
      end)
    .&Then('it is served',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('served');
      end)
    .&And('and nothing was raised on the way',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

.Rule('The implementation receives the closed types before it is built')

  .Scenario('Two closures alive at once do not overwrite each other')
    // El que caza un class var si alguien lo reintroduce: TBox<_> es UNA clase, asi que un
    // valor compartido lo pisaria el segundo cierre y este escenario lo veria.
    .Given('an open registration',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('two closures are resolved and both kept alive',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        var Alpha := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        var Beta := W.Provider.GetRequiredService<IBox<TBetaSettings>>;
        // Se leen DESPUES de tener las dos: leerlas segun se crean no distinguiria un valor
        // compartido de uno por instancia.
        W.Told := Alpha.Tag + '|' + Beta.Tag;
      end)
    .&Then('each one still sees the types of its own closure',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('TAlphaSettings|TBetaSettings');
      end)

  .Scenario('An implementation with nowhere to receive them is refused when it is registered')
    .When('an implementation that does not expose the field is registered open',
      procedure(W: TOpenWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Services.AddSingleton<IBox<_>, TBlindBox<_>>;
          end)
      end)
    .&Then('the registration itself fails, instead of leaving it for the first resolution',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('nowhere to write');
      end)
    .&And('and the message names the implementation and what it is missing',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('TBlindBox');
        Expect(W.Outcome).ToContain('_ClosedArgs');
      end)

  .Scenario('And so is one whose field count does not match the contract')
    .When('an implementation with two fields is registered for a contract of one argument',
      procedure(W: TOpenWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Services.AddSingleton<IBox<_>, TWideBox<_>>;
          end)
      end)
    .&Then('the registration fails, because the two are bound by position',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('BY POSITION');
      end)
    .&And('and the message names the implementation',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('TWideBox');
      end)

.Rule('The bounds the implementation declares are checked when a closure is resolved')

  .Scenario('A closure that satisfies the bound resolves')
    .Given('an open registration whose implementation only accepts settings',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('a closure whose argument is one is resolved',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Told := W.Provider.GetRequiredService<IBox<TAlphaSettings>>.Tag; end)
      end)
    .&Then('it resolves like any other',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('TAlphaSettings');
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

  .Scenario('One that does not is refused, saying which argument and which bound')
    .Given('the same open registration',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('a closure whose argument is not one is resolved',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Told := W.Provider.GetRequiredService<IBox<TStranger>>.Tag; end)
      end)
    .&Then('it is refused naming the argument that does not fit',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('TStranger');
      end)
    .&And('and the bound it fails to meet',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('TSettings');
      end)

.Rule('The lifetime of an open registration counts per closed type')

  .Scenario('Two closures of one shared registration are two instances')
    .Given('an open registration with a shared lifetime',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('two different closures are resolved',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        var Alpha := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        var Beta := W.Provider.GetRequiredService<IBox<TBetaSettings>>;
        W.Told := Format('%s#%d|%s#%d', [Alpha.Tag, Alpha.Serial, Beta.Tag, Beta.Serial]);
        W.Same := Alpha = IBox<TAlphaSettings>(Pointer(Beta));
      end)
    .&Then('each closure got one of its own, carrying its own types',
      procedure(W: TOpenWorld)
      begin
        // Sobre QUE trae cada una, y no sobre que haya dos: el recuento sale igual cuando la
        // segunda resolucion devuelve la instancia de la primera, que es el fallo real.
        Expect(W.Told).ToEqual('TAlphaSettings#1|TBetaSettings#2');
      end)
    .&And('and they are not the same object',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Same).ToBeFalse;
      end)

  .Scenario('The same closure twice is one instance')
    // El control del anterior: sin el, construir una por resolucion tambien lo pasaria, y eso
    // seria un defecto peor que el que se esta arreglando.
    .Given('an open registration with a shared lifetime',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
      end)
    .When('the same closure is resolved twice',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        var One := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        var Other := W.Provider.GetRequiredService<IBox<TAlphaSettings>>;
        W.Same := One = Other;
        W.Told := IntToStr(TBox<_>.Built);
      end)
    .&Then('it is literally the same reference',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Same).ToBeTrue;
      end)
    .&And('and it was built once',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Told).ToEqual('1');
      end)

.Rule('A constructor asking for a closure served by an open registration is chosen')

  .Scenario('The class that needs one is built with the constructor that takes it')
    .Given('an open registration and a class whose constructor asks for one of its closures',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
        W.Services.AddSingleton<IConsumer, TConsumer>;
      end)
    .When('that class is resolved',
      procedure(W: TOpenWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IConsumer>.Report;
      end)
    .&Then('it was built with its own constructor, and what it asked for arrived',
      procedure(W: TOpenWorld)
      begin
        // Sin un CanResolve consciente del emparejamiento este constructor no se elige: la
        // clase se construye con el de TObject, Report contesta igual y no falla nada. Por eso
        // se afirma sobre el CONTENIDO.
        Expect(W.Told).ToEqual('consumer(TAlphaSettings)');
      end)

  .Scenario('And validating the graph does not report it as unresolvable')
    .Given('the same composition',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
        W.Services.AddSingleton<IConsumer, TConsumer>;
      end)
    .When('the provider is built with validation on',
      procedure(W: TOpenWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False));
          end)
      end)
    .&Then('validation passes: what the open registration serves is resolvable',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

  .Scenario('What really is missing is still reported')
    // El control del anterior: una validacion que dejase de mirar los genericos tambien
    // pasaria la escena de arriba.
    .Given('a class whose constructor asks for a closure nobody serves',
      procedure(W: TOpenWorld)
      begin
        W.Services.AddSingleton<IConsumer, TConsumer>;
      end)
    .When('the provider is built with validation on',
      procedure(W: TOpenWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False));
          end)
      end)
    .&Then('validation says nothing registers it',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('nothing registers it');
      end)
    .&And('and it names the closure that is missing',
      procedure(W: TOpenWorld)
      begin
        Expect(W.Outcome).ToContain('IBox<');
      end);

end.
