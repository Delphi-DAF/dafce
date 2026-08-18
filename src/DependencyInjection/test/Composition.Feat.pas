unit Composition.Feat;

interface

uses
  Daf.Extensions.DependencyInjection;

/// <summary>
/// Escrita como si viniera de fuera de DAF: lee los descriptores ya registrados de un
/// servicio y registra uno derivado de ellos, usando solo la API publica de
/// IServiceCollection. Es la prueba de que exponer los descriptores en solo lectura basta
/// para que un tercero escriba su propia composicion — su propio Scrutor — sin tocar DAF.
/// </summary>
function AddWrappedFoo(const Services: IServiceCollection; const Mark: string): IServiceCollection;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Doubles.Feat;

function AddWrappedFoo(const Services: IServiceCollection; const Mark: string): IServiceCollection;
begin
  Result := Services;
  var Current := Services.Descriptors<IFoo>;
  if Length(Current) = 0 then Exit;

  var Inner := Current[High(Current)].Factory;
  var LifeTime := Current[High(Current)].LifeTime;
  Services.Replace(LifeTime, TypeInfo(IFoo),
    function(Provider: IServiceProvider): IInterface
    begin
      var Original: IFoo;
      Supports(Inner(Provider), IFoo, Original);
      Result := TWrappedFoo.Create(Original, Mark);
    end);
end;

initialization

Feature('''
Feature Service collection composition @di @composition

  As a package author
  I want to register defaults without overwriting what the application configured
  So that a reusable AddXxx can say "the library sets the default, the application decides"
''')

.UseWorld<TFooWorld>

.Rule('Registration can be idempotent')

  .Scenario('The application beats the library')
    .Given('the startup registered IFoo as TAppFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
      end)
    .When('a library function calls TryAddSingleton<IFoo, TLibFoo>',
      procedure(W: TFooWorld)
      begin
        W.Services.TryAddSingleton<IFoo, TLibFoo>;
      end)
    .&Then('resolving IFoo still gives TAppFoo',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('app');
      end)
    .&And('there is a single descriptor for IFoo',
      procedure(W: TFooWorld)
      begin
        Expect(Length(W.Services.Descriptors<IFoo>)).ToEqual(1);
      end)

  .Scenario('With no previous registration the default gets in')
    .Given('a collection with no IFoo',
      procedure(W: TFooWorld)
      begin
        Expect(W.Services.Contains<IFoo>).ToBeFalse;
      end)
    .When('TryAddSingleton<IFoo, TLibFoo> is called',
      procedure(W: TFooWorld)
      begin
        W.Services.TryAddSingleton<IFoo, TLibFoo>;
      end)
    .&Then('resolving IFoo gives TLibFoo',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('lib');
      end)

.Rule('A registration can be replaced and removed')

  .Scenario('Replace leaves a single registration')
    .Given('two registrations for IFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
        W.Services.AddSingleton<IFoo, TLibFoo>;
        Expect(Length(W.Services.Descriptors<IFoo>)).ToEqual(2);
      end)
    .When('Replace is called for IFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.Replace<IFoo, TOtherFoo>(Singleton);
      end)
    .&Then('exactly one descriptor is left, the new one',
      procedure(W: TFooWorld)
      begin
        Expect(Length(W.Services.Descriptors<IFoo>)).ToEqual(1);
        Expect(W.ResolveName).ToEqual('other');
      end)

  .Scenario('A spec harness can take a registration apart')
    .Given('IFoo registered by production code',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
      end)
    .When('the harness calls RemoveAll for IFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.RemoveAll<IFoo>;
      end)
    .&Then('resolving it fails as if it had never been registered',
      procedure(W: TFooWorld)
      begin
        Expect(W.Services.Contains<IFoo>).ToBeFalse;
        W.Build;
        ExpectException(
          procedure
          begin
            W.Provider.GetRequiredService<IFoo>;
          end).ToBe<EServiceProviderError>;
      end)

.Rule('Registered descriptors are readable, and only readable')

  .Scenario('A third party can write its own composition extension')
    .Given('IFoo registered with a factory the extension does not know',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IFoo),
          function(Provider: IServiceProvider): IInterface
          begin
            Result := TAppFoo.Create as IFoo;
          end);
      end)
    .When('a function written outside DAF reads the descriptors and registers a derived one',
      procedure(W: TFooWorld)
      begin
        AddWrappedFoo(W.Services, 'outside');
      end)
    .&Then('it compiles and works using only the public API',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('outside(app)');
      end)

  .Scenario('What is read is not a back door into the collection')
    .Given('IFoo registered as TAppFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
      end)
    .When('whoever reads the descriptors modifies what it got',
      procedure(W: TFooWorld)
      begin
        var Snapshot := W.Services.Descriptors<IFoo>;
        Snapshot[0] := TServiceDescriptor.Create(Factory.From(TypeInfo(IFoo), TLibFoo), Singleton, TypeInfo(IFoo));
        SetLength(Snapshot, 0);
      end)
    .&Then('the collection did not change, and resolving still gives what was registered',
      procedure(W: TFooWorld)
      begin
        Expect(Length(W.Services.Descriptors<IFoo>)).ToEqual(1);
        Expect(W.ResolveName).ToEqual('app');
      end)

.Rule('Composition stops once the provider is built')

  .Scenario('TryAdd on a frozen collection fails out loud')
    .Given('a collection whose provider was already built',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
        W.Build;
      end)
    .When('TryAddSingleton is called',
      procedure(W: TFooWorld)
      begin
        W.Failure := '';
        try
          W.Services.TryAddSingleton<IBar, TAppBar>;
        except
          on E: Exception do W.Failure := E.Message;
        end;
      end)
    .&Then('it raises saying the collection admits no more changes',
      procedure(W: TFooWorld)
      begin
        Expect(W.Failure).ToContain('frozen');
      end)

  .Scenario('Replace on a frozen collection fails out loud')
    .Given('a collection whose provider was already built',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
        W.Build;
      end)
    .When('Replace is called',
      procedure(W: TFooWorld)
      begin
        W.Failure := '';
        try
          W.Services.Replace<IFoo, TLibFoo>(Singleton);
        except
          on E: Exception do W.Failure := E.Message;
        end;
      end)
    .&Then('it raises saying the collection admits no more changes',
      procedure(W: TFooWorld)
      begin
        Expect(W.Failure).ToContain('frozen');
      end)

  .Scenario('RemoveAll on a frozen collection fails out loud')
    .Given('a collection whose provider was already built',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
        W.Build;
      end)
    .When('RemoveAll is called',
      procedure(W: TFooWorld)
      begin
        W.Failure := '';
        try
          W.Services.RemoveAll<IFoo>;
        except
          on E: Exception do W.Failure := E.Message;
        end;
      end)
    .&Then('it raises saying the collection admits no more changes',
      procedure(W: TFooWorld)
      begin
        Expect(W.Failure).ToContain('frozen');
      end);

end.
