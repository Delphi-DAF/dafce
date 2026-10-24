unit ServiceIdentity.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  /// Contrato con dos implementaciones. Lo que separa "dos instancias" de "la misma dos
  /// veces" es CUAL implementa cada una: el recuento sale bien en los dos casos, asi que
  /// contar no distingue nada.
  /// </summary>
  IMark = interface(IInvokable)
    ['{1ED1D897-904B-4D96-81FD-D640F0D5C8A1}']
    function Mark: string;
  end;

  TMarkA = class(TInterfacedObject, IMark)
  public
    // Publica a proposito: es el instrumento de medida del spec, no estado del servicio.
    class var Created: Integer;
    constructor Create;
    function Mark: string;
  end;

  TMarkB = class(TInterfacedObject, IMark)
  public
    function Mark: string;
  end;

  /// <summary>
  /// Un servicio que depende de otro cuyo tipo no es nombrable cualificadamente. Existe
  /// para llegar a una ruta de error del contenedor: la de un singleton que depende de un
  /// servicio con ambito.
  /// </summary>
  IOuter = interface(IInvokable)
    ['{4A540D14-6F2A-4FA3-B8A0-E7ACEAD47D4C}']
    function Mark: string;
  end;

  /// <summary>
  /// Tres contratos distintos, y no tres registros del mismo, porque el orden de liberacion
  /// tiene que poder afirmarse tambien con la identidad vieja: con un solo tipo la identidad
  /// vieja guardaria una sola instancia y el spec caeria por otro motivo.
  /// </summary>
  IFirst = interface(IInvokable)
    ['{2D7498E3-7D7D-45FD-B3E2-E9FF8133552B}']
    function Tag: string;
  end;

  ISecond = interface(IInvokable)
    ['{376989C1-9DC2-42FE-977F-3073C9C64C95}']
    function Tag: string;
  end;

  IThird = interface(IInvokable)
    ['{A55E05B3-C787-4A73-8EDB-BE4C927A93B9}']
    function Tag: string;
  end;

  /// <summary>
  /// Anota su etiqueta al destruirse. El orden de liberacion solo se puede afirmar viendolo:
  /// un contador dice cuantos se liberaron, no en que orden.
  /// </summary>
  TStep = class(TInterfacedObject)
  public
    class var Released: string;
    function Tag: string; virtual; abstract;
    destructor Destroy; override;
  end;

  TStepA = class(TStep, IFirst)
  public
    function Tag: string; override;
  end;

  TStepB = class(TStep, ISecond)
  public
    function Tag: string; override;
  end;

  TStepC = class(TStep, IThird)
  public
    function Tag: string; override;
  end;

  TIdentityWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Marks: string;
    Found: Integer;
    Outcome: string;
    SameInstance: Boolean;
    FactoryCalls: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    /// Captura clase y mensaje del fallo, o un centinela con texto si no lo hubo. Asi el
    /// spec puede afirmar CUAL fue el fallo: decir solo "fallo" da por bueno cualquiera.
    procedure Attempt(const Action: TProc);
  end;

const
  SinFallo = '<sin fallo>';

implementation

uses
  System.Rtti,
  Daf.MiniSpec,
  Daf.DependencyInjection;

type
  /// <summary>
  /// Declarada AQUI, en la implementation, y ese es todo el truco: es exactamente lo que
  /// QualifiedName no sabe nombrar. No es un tipo raro -- es lo que le pasa a una
  /// instanciacion cerrada de un generico que solo vive dentro del cuerpo de un metodo,
  /// que es el caso real que trajo esto. Hay un escenario de control que comprueba que
  /// de verdad no es nombrable: sin el, los escenarios que la usan podrian estar en verde
  /// por no ser el caso que dicen ser.
  /// </summary>
  IHidden = interface(IInvokable)
    ['{9276614E-FB94-448F-8B14-159987D67A9F}']
    function Mark: string;
  end;

  THidden = class(TInterfacedObject, IHidden)
  public
    function Mark: string;
  end;

  TOuter = class(TInterfacedObject, IOuter)
  strict private
    FInner: IHidden;
  public
    constructor Create(const Inner: IHidden);
    function Mark: string;
  end;

  /// Lo que devuelve una factory que no cumple su contrato: no implementa IMark.
  TNotAMark = class(TInterfacedObject)
  end;

  INeedsMark = interface(IInvokable)
    ['{6C1F0B2E-3D4A-4E8B-9F21-7A5C6D8E9B03}']
    function Received: string;
  end;

  /// Anota si su constructor recibio un IMark o nil.
  TNeedsMark = class(TInterfacedObject, INeedsMark)
  strict private
    FReceived: string;
  public
    constructor Create(const Mark: IMark);
    function Received: string;
  end;

{ TMarkA }

constructor TMarkA.Create;
begin
  inherited Create;
  Inc(Created);
end;

function TMarkA.Mark: string;
begin
  Result := 'A';
end;

{ TMarkB }

function TMarkB.Mark: string;
begin
  Result := 'B';
end;

{ TStep }

destructor TStep.Destroy;
begin
  // Tag es abstracto y se llama desde el destructor a proposito: en Delphi el objeto sigue
  // siendo de su clase real mientras se destruye, asi que resuelve a la subclase.
  Released := Released + Tag;
  inherited;
end;

{ TStepA }

function TStepA.Tag: string;
begin
  Result := 'A';
end;

{ TStepB }

function TStepB.Tag: string;
begin
  Result := 'B';
end;

{ TStepC }

function TStepC.Tag: string;
begin
  Result := 'C';
end;

{ THidden }

function THidden.Mark: string;
begin
  Result := 'hidden';
end;

{ TOuter }

constructor TOuter.Create(const Inner: IHidden);
begin
  inherited Create;
  FInner := Inner;
end;

function TOuter.Mark: string;
begin
  Result := 'outer(' + FInner.Mark + ')';
end;

{ TNeedsMark }

constructor TNeedsMark.Create(const Mark: IMark);
begin
  inherited Create;
  if Mark = nil then
    FReceived := 'nil'
  else
    FReceived := Mark.Mark;
end;

function TNeedsMark.Received: string;
begin
  Result := FReceived;
end;

/// Una factory de IMark que devuelve nil y cuenta sus llamadas en el World.
function NilMark(const W: TIdentityWorld): TServiceFactory;
begin
  Result :=
    function(Provider: IServiceProvider): IInterface
    begin
      Inc(W.FactoryCalls);
      Result := nil;
    end;
end;

{ TIdentityWorld }

constructor TIdentityWorld.Create;
begin
  inherited;
  TMarkA.Created := 0;
  TStep.Released := '';
  Marks := '';
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TIdentityWorld.Destroy;
begin
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TIdentityWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TIdentityWorld.Attempt(const Action: TProc);
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
Feature An instance belongs to its registration @di @lifetime @identity

  As an application author
  I want each registration to get its own instance, whatever its service type looks like
  So that registering a second implementation adds one instead of shadowing the first
''')

.UseWorld<TIdentityWorld>

.Rule('An instance belongs to its registration, not to its service type')

  .Scenario('Two registrations, two live implementations')
    .Given('IMark registered twice, with a different implementation each time',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IMark, TMarkA>;
        W.Services.AddSingleton<IMark, TMarkB>;
      end)
    .When('all the services of that type are asked for',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        var All := W.Provider.GetServices<IMark>;
        W.Found := All.Count;
        for var Each in All do
          W.Marks := W.Marks + Each.Mark;
      end)
    .&Then('each registration contributed its own implementation',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Marks).ToEqual('AB');
      end)
    .&And('there are as many as were registered, which was never the part that failed',
      procedure(W: TIdentityWorld)
      begin
        // Pasa antes y despues, y va escrito para que se vea: el recuento es justo lo que
        // disfrazaba el defecto, asi que no puede ser lo que lo detecte.
        Expect(W.Found).ToEqual(2);
      end)

  .Scenario('A copy of a registration is still the same registration')
    // Descriptors reparte COPIAS, y el ambito resuelve a partir de ellas: TryGetAll enumera
    // la lista y cada vuelta entrega una copia. Si la identidad no viajase con la copia, dos
    // registros distintos podrian acabar compartiendola, que es el defecto de partida otra vez.
    .Given('two registrations of the same service type',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IMark, TMarkA>;
        W.Services.AddSingleton<IMark, TMarkB>;
      end)
    .When('their descriptors are read out of the collection twice',
      procedure(W: TIdentityWorld)
      begin
        var First := W.Services.Descriptors<IMark>;
        var Second := W.Services.Descriptors<IMark>;
        if First[0].Identity <> Second[0].Identity then
          W.Marks := 'the copy lost its registration'
        else if First[0].Identity = First[1].Identity then
          W.Marks := 'two registrations share one identity'
        else
          W.Marks := 'each copy keeps its own registration';
      end)
    .&Then('each copy carries the identity of the registration it came from',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Marks).ToEqual('each copy keeps its own registration');
      end)

  .Scenario('One registration resolved twice is still one instance')
    .Given('a single registration of shared lifetime',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IMark, TMarkA>;
      end)
    .When('that service is resolved twice',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        var One := W.Provider.GetRequiredService<IMark>;
        var Other := W.Provider.GetRequiredService<IMark>;
        W.SameInstance := One = Other;
      end)
    .&Then('both resolutions gave the same instance',
      procedure(W: TIdentityWorld)
      begin
        // El control de la escena anterior: sin el, "construir una por resolucion" tambien
        // la pasaria, y eso seria un defecto peor que el que se esta arreglando.
        Expect(TMarkA.Created).ToEqual(1);
      end)
    .&And('and it is literally the same reference',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.SameInstance).ToBeTrue;
      end)

  .Scenario('Resolving one still gives the last registered')
    .Given('two registrations of the same service type',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IMark, TMarkA>;
        W.Services.AddSingleton<IMark, TMarkB>;
      end)
    .When('the service is resolved without asking for all of them',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Marks := W.Provider.GetRequiredService<IMark>.Mark;
      end)
    .&Then('the last registration wins, as before',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Marks).ToEqual('B');
      end)

.Rule('Resolving a service does not require its type to be visible to reflection')

  .Scenario('The type these scenarios are about really is not nameable')
    // Sin este control los dos escenarios de abajo podrian estar en verde porque el tipo
    // resulto ser nombrable despues de todo, y sus titulos mentirian.
    .When('reflection is asked for its qualified name',
      procedure(W: TIdentityWorld)
      begin
        W.Attempt(
          procedure
          begin
            var RC := TRttiContext.Create;
            try
              W.Marks := RC.GetType(System.TypeInfo(IHidden)).QualifiedName;
            finally
              RC.Free;
            end;
          end);
      end)
    .&Then('reflection refuses, which is what makes this the case under test',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('ENonPublicType');
      end)

  .Scenario('A type reflection cannot name registers and resolves')
    .Given('a service whose type is not qualifiably nameable',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IHidden, THidden>;
      end)
    .When('it is resolved',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Marks := W.Provider.GetRequiredService<IHidden>.Mark; end);
      end)
    .&Then('it resolves like any other',
      procedure(W: TIdentityWorld)
      begin
        // Que funcione, no que falle distinto: se afirma sobre lo que devolvio.
        Expect(W.Marks).ToEqual('hidden');
      end)
    .&And('and nothing was raised on the way',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToEqual(SinFallo);
      end)

.Rule('Naming a service to compose an error message never fails')

  .Scenario('Validation reports the missing registration, not the visibility of a type')
    // La otra ruta de error que nombra un tipo: la validacion al construir. Nombra el tipo del
    // PARAMETRO que no se pudo resolver, y ese tipo tampoco tiene por que ser nombrable.
    .Given('a service whose constructor needs a type reflection cannot name, and nobody registers it',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IOuter, TOuter>;
      end)
    .When('the provider is built with validation on',
      procedure(W: TIdentityWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False));
          end);
      end)
    .&Then('what is reported is that nothing registers it',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('nothing registers it');
      end)
    .&And('and the message names the type that is missing',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('IHidden');
      end)

  .Scenario('The failure reported is the one that happened')
    .Given('a singleton that depends on a scoped service reflection cannot name',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddScoped<IHidden, THidden>;
        W.Services.AddSingleton<IOuter, TOuter>;
      end)
    .When('the singleton is resolved',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Marks := W.Provider.GetRequiredService<IOuter>.Mark; end);
      end)
    .&Then('what is reported is the captured dependency, not the visibility of a type',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('cannot depends on scoped');
      end)
    .&And('and the message identifies the service',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('IHidden');
      end)

.Rule('Instances are released in reverse creation order')

  .Scenario('The last one created is the first one released')
    .Given('three services created in a known order',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton<IFirst, TStepA>;
        W.Services.AddSingleton<ISecond, TStepB>;
        W.Services.AddSingleton<IThird, TStepC>;
        W.Build;
        W.Provider.GetRequiredService<IFirst>;
        W.Provider.GetRequiredService<ISecond>;
        W.Provider.GetRequiredService<IThird>;
      end)
    .When('the scope is closed',
      procedure(W: TIdentityWorld)
      begin
        W.Provider.ShutDown;
      end)
    .&Then('they are released in the reverse of the order they were created in',
      procedure(W: TIdentityWorld)
      begin
        Expect(TStep.Released).ToEqual('CBA');
      end)

.Rule('What does not implement the service fails as an interface conversion')

  .Scenario('The factory returns something that does not implement the service')
    .Given('a service registered with a factory that returns an object of another contract',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IMark),
          function(Provider: IServiceProvider): IInterface
          begin
            Result := TNotAMark.Create;
          end);
      end)
    .When('it is resolved',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Marks := W.Provider.GetRequiredService<IMark>.Mark; end);
      end)
    .&Then('it fails with EIntfCastError',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('EIntfCastError: ');
      end)
    .&And('the message names the class of what was returned and the service',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('TNotAMark');
        Expect(W.Outcome).ToContain('IMark');
      end)

.Rule('A factory that returns nil hands nil')

  .Scenario('GetService hands nil')
    .Given('a service registered with a factory that returns nil',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IMark), NilMark(W));
      end)
    .When('it is asked for with GetService',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Marks := BoolToStr(W.Provider.GetService<IMark> = nil, True); end);
      end)
    .&Then('nil comes back, with no error',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome + ' / nil=' + W.Marks).ToEqual(SinFallo + ' / nil=True');
      end)

  .Scenario('GetRequiredService fails as for a service nobody registered')
    .Given('the same service',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IMark), NilMark(W));
      end)
    .When('it is asked for with GetRequiredService',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Provider.GetRequiredService<IMark>; end);
      end)
    .&Then('it fails with EServiceProviderError, naming the service',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('IMark');
      end)

  .Scenario('A constructor receives nil')
    .Given('a registered class whose constructor asks for that service',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IMark), NilMark(W));
        W.Services.AddSingleton<INeedsMark, TNeedsMark>;
      end)
    .When('it is resolved',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Marks := W.Provider.GetRequiredService<INeedsMark>.Received; end);
      end)
    .&Then('it is built, and its parameter received nil',
      procedure(W: TIdentityWorld)
      begin
        Expect(W.Outcome + ' / received ' + W.Marks).ToEqual(SinFallo + ' / received nil');
      end)

  .Scenario('A Singleton does not keep the nil')
    .Given('a Singleton whose factory returns nil and counts its calls',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IMark), NilMark(W));
      end)
    .When('it is resolved twice',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Provider.GetService<IMark>;
            W.Provider.GetService<IMark>;
          end);
      end)
    .&Then('the factory has been called twice',
      procedure(W: TIdentityWorld)
      begin
        Expect(Format('%s / %d call(s)', [W.Outcome, W.FactoryCalls])).ToEqual(SinFallo + ' / 2 call(s)');
      end)

  .Scenario('A Scoped keeps the nil in its scope')
    .Given('a Scoped whose factory returns nil and counts its calls',
      procedure(W: TIdentityWorld)
      begin
        W.Services.AddScoped(TypeInfo(IMark), NilMark(W));
      end)
    .When('it is resolved twice from the same scope',
      procedure(W: TIdentityWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            var Scope := W.Provider.CreateScope;
            Scope.ServiceProvider.GetService<IMark>;
            Scope.ServiceProvider.GetService<IMark>;
          end);
      end)
    .&Then('the factory has been called once',
      procedure(W: TIdentityWorld)
      begin
        Expect(Format('%s / %d call(s)', [W.Outcome, W.FactoryCalls])).ToEqual(SinFallo + ' / 1 call(s)');
      end);

end.
