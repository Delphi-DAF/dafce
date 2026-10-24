unit ConstructorSelection.Feat;

// Que constructor elige el contenedor, y el activador, para una clase. Cada clase de aqui anota en
// Chosen cual corrio: que la construccion no falle no dice cual se eligio.

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities;

type
  ISel = interface(IInvokable)
    ['{3B7E2C41-9A5D-4F18-B6C2-0D4E8F1A7C95}']
    function Chosen: string;
  end;

  IAlphaSel = interface(IInvokable)
    ['{8D1F4A63-2B7C-4E95-A0D3-6C5B9E2F1A48}']
  end;

  IBetaSel = interface(IInvokable)
    ['{C4A9E715-6D2B-4F80-93E1-7B0C5D8A2F36}']
  end;

  IGammaSel = interface(IInvokable)
    ['{5E2B8F09-1C4D-4A73-B6E5-9F0A3D7C2B14}']
  end;

  TAlphaSel = class(TInterfacedObject, IAlphaSel);
  TBetaSel = class(TInterfacedObject, IBetaSel);
  TGammaSel = class(TInterfacedObject, IGammaSel);

  TChoosing = class(TInterfacedObject, ISel)
  strict protected
    FChosen: string;
  public
    function Chosen: string;
  end;

  /// Un solo constructor, que pide IAlphaSel.
  TNeedsAlpha = class(TChoosing)
  public
    constructor Create(const A: IAlphaSel);
  end;

  /// Declara un constructor; su descendiente no declara ninguno.
  TBaseWithCtor = class(TChoosing)
  public
    constructor Create(const A: IAlphaSel);
  end;

  TDerivedNoCtor = class(TBaseWithCtor);

  /// Dos constructores, cada uno con un servicio distinto.
  TTwoCtors = class(TChoosing)
  public
    constructor Create(const B: IBetaSel); overload;
    constructor Create(const A: IAlphaSel); overload;
  end;

  /// Un constructor marcado que pide IBetaSel, y otro sin parametros.
  TMarkedOrEmpty = class(TChoosing)
  public
    class var EmptyRuns: Integer;
    [ActivatorUtilitiesConstructor]
    constructor Create(const B: IBetaSel); overload;
    constructor Create; overload;
  end;

  /// Un constructor marcado de un parametro, y otro sin marcar de dos.
  TMarkedShort = class(TChoosing)
  public
    [ActivatorUtilitiesConstructor]
    constructor Create(const A: IAlphaSel); overload;
    constructor Create(const A: IAlphaSel; const C: IGammaSel); overload;
  end;

  /// Dos constructores de un parametro cada uno, con servicios distintos.
  TAlphaOrBeta = class(TChoosing)
  public
    constructor Create(const A: IAlphaSel); overload;
    constructor Create(const B: IBetaSel); overload;
  end;

  /// Uno de dos parametros, y otro mas corto contenido en el.
  TBothOrAlpha = class(TChoosing)
  public
    constructor Create(const A: IAlphaSel; const B: IBetaSel); overload;
    constructor Create(const A: IAlphaSel); overload;
  end;

  /// Uno de dos parametros, y otro mas corto que pide otra cosa.
  TBothOrGamma = class(TChoosing)
  public
    constructor Create(const A: IAlphaSel; const B: IBetaSel); overload;
    constructor Create(const C: IGammaSel); overload;
  end;

  /// Un solo constructor, que pide IAlphaSel en diferido.
  TLazyNeeds = class(TChoosing)
  public
    constructor Create(const L: ILazy<IAlphaSel>);
  end;

  /// Un constructor que pide IAlphaSel en diferido, y otro sin parametros.
  TLazyOrEmpty = class(TChoosing)
  public
    constructor Create(const L: ILazy<IAlphaSel>); overload;
    constructor Create; overload;
  end;

  /// Su constructor lanza; cuenta sus destrucciones.
  TRaisingCtor = class(TChoosing)
  public
    class var Destroyed: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TSelectionWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Report: string;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    procedure Attempt(const Action: TProc);
    /// Construye AClass con TActivatorUtilities.CreateInstance y anota que constructor corrio.
    procedure Activate(const AClass: TClass);
  end;

const
  NoFailure = '<sin error>';

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection;

{ TChoosing }

function TChoosing.Chosen: string;
begin
  Result := FChosen;
end;

constructor TNeedsAlpha.Create(const A: IAlphaSel);
begin
  inherited Create;
  FChosen := 'alpha';
end;

constructor TBaseWithCtor.Create(const A: IAlphaSel);
begin
  inherited Create;
  if A = nil then
    FChosen := 'base(nil)'
  else
    FChosen := 'base(alpha)';
end;

constructor TTwoCtors.Create(const B: IBetaSel);
begin
  inherited Create;
  FChosen := 'beta';
end;

constructor TTwoCtors.Create(const A: IAlphaSel);
begin
  inherited Create;
  FChosen := 'alpha';
end;

constructor TMarkedOrEmpty.Create(const B: IBetaSel);
begin
  inherited Create;
  FChosen := 'marked';
end;

constructor TMarkedOrEmpty.Create;
begin
  inherited Create;
  Inc(EmptyRuns);
  FChosen := 'empty';
end;

constructor TMarkedShort.Create(const A: IAlphaSel);
begin
  inherited Create;
  FChosen := 'marked';
end;

constructor TMarkedShort.Create(const A: IAlphaSel; const C: IGammaSel);
begin
  inherited Create;
  FChosen := 'two';
end;

constructor TAlphaOrBeta.Create(const A: IAlphaSel);
begin
  inherited Create;
  FChosen := 'alpha';
end;

constructor TAlphaOrBeta.Create(const B: IBetaSel);
begin
  inherited Create;
  FChosen := 'beta';
end;

constructor TBothOrAlpha.Create(const A: IAlphaSel; const B: IBetaSel);
begin
  inherited Create;
  FChosen := 'two';
end;

constructor TBothOrAlpha.Create(const A: IAlphaSel);
begin
  inherited Create;
  FChosen := 'alpha';
end;

constructor TBothOrGamma.Create(const A: IAlphaSel; const B: IBetaSel);
begin
  inherited Create;
  FChosen := 'two';
end;

constructor TBothOrGamma.Create(const C: IGammaSel);
begin
  inherited Create;
  FChosen := 'gamma';
end;

constructor TLazyNeeds.Create(const L: ILazy<IAlphaSel>);
begin
  inherited Create;
  FChosen := 'lazy';
end;

constructor TLazyOrEmpty.Create(const L: ILazy<IAlphaSel>);
begin
  inherited Create;
  FChosen := 'lazy';
end;

constructor TLazyOrEmpty.Create;
begin
  inherited Create;
  FChosen := 'empty';
end;

constructor TRaisingCtor.Create;
begin
  inherited Create;
  raise Exception.Create('the constructor refused');
end;

destructor TRaisingCtor.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

/// Registra IAlphaSel, IBetaSel e IGammaSel.
procedure RegisterAll(const W: TSelectionWorld);
begin
  W.Services.AddTransient<IAlphaSel, TAlphaSel>;
  W.Services.AddTransient<IBetaSel, TBetaSel>;
  W.Services.AddTransient<IGammaSel, TGammaSel>;
end;

{ TSelectionWorld }

constructor TSelectionWorld.Create;
begin
  inherited;
  TMarkedOrEmpty.EmptyRuns := 0;
  TRaisingCtor.Destroyed := 0;
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TSelectionWorld.Destroy;
begin
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TSelectionWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TSelectionWorld.Attempt(const Action: TProc);
begin
  Outcome := NoFailure;
  try
    Action;
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TSelectionWorld.Activate(const AClass: TClass);
begin
  Build;
  Attempt(
    procedure
    begin
      var Sel: ISel;
      if Supports(TActivatorUtilities.CreateInstance(Provider, AClass), ISel, Sel) then
        Report := Sel.Chosen;
    end);
end;

initialization

Feature('''
Feature Constructor selection @di @constructor-selection

  As whoever registers a class in the container
  I want to know which constructor builds it, and a constructor nobody can satisfy to fail
  So that a class is never handed over half built with a dependency left at nil
''')

.UseWorld<TSelectionWorld>

.Rule('A constructor nobody can satisfy makes the construction fail')

  .Scenario('A parameter nobody registered makes the construction fail')
    .Given('a registered class whose only constructor asks for a service nobody registered',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<ISel, TNeedsAlpha>;
      end)
    .When('it is resolved',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it fails with EServiceProviderError, naming the parameter, its type and the class',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('"A"');
        Expect(W.Outcome).ToContain('IAlphaSel');
        Expect(W.Outcome).ToContain('TNeedsAlpha');
      end)
    .&And('no instance is handed over',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Report).ToEqual('');
      end)

  .Scenario('A class with no constructor of its own uses the one of its ancestor')
    .Given('a registered class that declares no constructor, whose ancestor declares one the container can satisfy',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<IAlphaSel, TAlphaSel>;
        W.Services.AddTransient<ISel, TDerivedNoCtor>;
      end)
    .When('it is resolved',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it is built with the constructor of the ancestor, with its argument resolved',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / base(alpha)');
      end)

  .Scenario('Between two constructors the one that can be satisfied wins')
    .Given('a class with a constructor that asks for an unregistered service and another that asks for a registered one',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<IAlphaSel, TAlphaSel>;
        W.Services.AddTransient<ISel, TTwoCtors>;
      end)
    .When('it is resolved',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it is built with the second one, with no error',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / alpha');
      end)

.Rule('The error of a constructor that raises reaches whoever resolves')

  .Scenario('A constructor that raises')
    .Given('a registered class whose constructor raises',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<ISel, TRaisingCtor>;
      end)
    .When('it is resolved',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('the error that reaches the caller is the one the constructor raised',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToEqual('Exception: the constructor refused');
      end)
    .&And('the half built instance was destroyed',
      procedure(W: TSelectionWorld)
      begin
        Expect(TRaisingCtor.Destroyed).ToEqual(1);
      end)

.Rule('The constructor mark only counts in the activator')

  .Scenario('The activator fails if it cannot satisfy the marked constructor')
    .Given('a class with a marked constructor that asks for an unregistered service, and another with no parameters',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
      end)
    .When('it is built with TActivatorUtilities.CreateInstance',
      procedure(W: TSelectionWorld)
      begin
        W.Activate(TMarkedOrEmpty);
      end)
    .&Then('it fails with EServiceProviderError, naming the parameter of the marked constructor',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('"B"');
      end)
    .&And('the constructor with no parameters has not run',
      procedure(W: TSelectionWorld)
      begin
        Expect(TMarkedOrEmpty.EmptyRuns).ToEqual(0);
      end)

  .Scenario('The activator uses the marked one even if another has more parameters')
    .Given('a class with a marked constructor of one parameter and an unmarked one of two, all their services registered',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<IAlphaSel, TAlphaSel>;
        W.Services.AddTransient<IGammaSel, TGammaSel>;
      end)
    .When('it is built with TActivatorUtilities.CreateInstance',
      procedure(W: TSelectionWorld)
      begin
        W.Activate(TMarkedShort);
      end)
    .&Then('it is built with the marked one',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / marked');
      end)

  .Scenario('The container does not take the mark into account')
    .Given('the same class, registered by class',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<IAlphaSel, TAlphaSel>;
        W.Services.AddTransient<IGammaSel, TGammaSel>;
        W.Services.AddTransient<ISel, TMarkedShort>;
      end)
    .When('the container resolves it',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it is built with the constructor of two parameters',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / two');
      end)

  .Scenario('A marked constructor that cannot be satisfied does not block the container')
    .Given('a class registered by class with a marked constructor that asks for an unregistered service, ' +
      'and another with no parameters',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<ISel, TMarkedOrEmpty>;
      end)
    .When('the container resolves it',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it is built with the constructor with no parameters, with no error',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / empty');
      end)

.Rule('Two constructors that compete make the choice fail')

  .Scenario('In the container, two constructors with different parameters compete')
    .Given('a class registered by class with a constructor that asks for IAlphaSel and another for IBetaSel, both registered',
      procedure(W: TSelectionWorld)
      begin
        RegisterAll(W);
        W.Services.AddTransient<ISel, TAlphaOrBeta>;
      end)
    .When('the container resolves it',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it fails with EServiceProviderError, naming the class and both constructors',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('TAlphaOrBeta');
        Expect(W.Outcome).ToContain('IAlphaSel');
        Expect(W.Outcome).ToContain('IBetaSel');
      end)

  .Scenario('In the container, a shorter one contained in the chosen one does not compete')
    .Given('a class registered by class with a constructor that asks for IAlphaSel and IBetaSel, and another for IAlphaSel alone',
      procedure(W: TSelectionWorld)
      begin
        RegisterAll(W);
        W.Services.AddTransient<ISel, TBothOrAlpha>;
      end)
    .When('the container resolves it',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it is built with the one of two parameters, with no error',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / two');
      end)

  .Scenario('In the container, a shorter one that asks for something else competes')
    .Given('a class registered by class with a constructor that asks for IAlphaSel and IBetaSel, and another for IGammaSel',
      procedure(W: TSelectionWorld)
      begin
        RegisterAll(W);
        W.Services.AddTransient<ISel, TBothOrGamma>;
      end)
    .When('the container resolves it',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it fails with EServiceProviderError, naming the class and both constructors',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('TBothOrGamma');
        Expect(W.Outcome).ToContain('IGammaSel');
      end)

  .Scenario('In the activator, the longest ones tie')
    .Given('a class with a constructor that asks for IAlphaSel and another for IBetaSel, both registered',
      procedure(W: TSelectionWorld)
      begin
        RegisterAll(W);
      end)
    .When('it is built with TActivatorUtilities.CreateInstance',
      procedure(W: TSelectionWorld)
      begin
        W.Activate(TAlphaOrBeta);
      end)
    .&Then('it fails with EServiceProviderError, naming the class and both constructors',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('TAlphaOrBeta');
        Expect(W.Outcome).ToContain('IAlphaSel');
        Expect(W.Outcome).ToContain('IBetaSel');
      end)

  .Scenario('In the activator, a shorter one that asks for something else does not compete')
    .Given('a class with a constructor that asks for IAlphaSel and IBetaSel, and another for IGammaSel, all registered',
      procedure(W: TSelectionWorld)
      begin
        RegisterAll(W);
      end)
    .When('it is built with TActivatorUtilities.CreateInstance',
      procedure(W: TSelectionWorld)
      begin
        W.Activate(TBothOrGamma);
      end)
    .&Then('it is built with the one of two parameters, with no error',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / two');
      end)

  .Scenario('Validation reports two constructors that compete')
    .Given('the class of the first scenario, registered by class',
      procedure(W: TSelectionWorld)
      begin
        RegisterAll(W);
        W.Services.AddTransient<ISel, TAlphaOrBeta>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TSelectionWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False));
          end);
      end)
    .&Then('it fails naming the class and both constructors',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('TAlphaOrBeta');
        Expect(W.Outcome).ToContain('IAlphaSel');
        Expect(W.Outcome).ToContain('IBetaSel');
      end)

.Rule('An ILazy counts as satisfiable only if its service is')

  .Scenario('Validation reports an ILazy whose service nobody registered')
    .Given('a registered class whose constructor asks for an ILazy of a service nobody registered',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<ISel, TLazyNeeds>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TSelectionWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Provider := W.Services.BuildServiceProvider(TServiceProviderOptions.Create(True, False));
          end);
      end)
    .&Then('it fails naming the missing service, the parameter and the class',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('IAlphaSel');
        Expect(W.Outcome).ToContain('"L"');
        Expect(W.Outcome).ToContain('TLazyNeeds');
      end)

  .Scenario('Between two constructors the one with the ILazy that cannot be satisfied is not chosen')
    .Given('a class with a constructor that asks for an ILazy of an unregistered service, and another with no parameters',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<ISel, TLazyOrEmpty>;
      end)
    .When('it is resolved',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it is built with the constructor with no parameters, with no error',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / empty');
      end)

  .Scenario('With a single constructor, the ILazy that cannot be satisfied fails when building')
    .Given('a class whose only constructor asks for an ILazy of a service nobody registered',
      procedure(W: TSelectionWorld)
      begin
        W.Services.AddTransient<ISel, TLazyNeeds>;
      end)
    .When('it is resolved',
      procedure(W: TSelectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<ISel>.Chosen; end);
      end)
    .&Then('it fails when building it, naming the missing service',
      procedure(W: TSelectionWorld)
      begin
        Expect(W.Outcome).ToContain('EServiceProviderError: ');
        Expect(W.Outcome).ToContain('IAlphaSel');
      end);

end.
