unit Decoration.Feat;

interface

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.Extensions.DependencyInjection,
  DependencyInjection.Doubles;

initialization

Feature('''
Feature Service decoration @di @composition @decorate

  As an application author
  I want to wrap a registration made by someone else
  So I can add logging, caching or retries without duplicating its factory
''')

.UseWorld<TFooWorld>

.Rule('The decorator wraps what the original registration produces')

  .Scenario('The decorator gets the original built by its own factory')
    .Given('IFoo registered with a factory the decorator does not know',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton(TypeInfo(IFoo),
          function(Provider: IServiceProvider): IInterface
          begin
            Result := TAppFoo.Create;
          end);
      end)
    .When('IFoo is decorated and resolved',
      procedure(W: TFooWorld)
      begin
        W.Services.Decorate<IFoo>(
          function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
          begin
            Result := TWrappedFoo.Create(Inner, 'log');
          end);
      end)
    .&Then('the consumer gets the decorated instance, wrapping the original one',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('log(app)');
      end)

  .Scenario('Decorating twice nests in registration order')
    .Given('IFoo registered as TAppFoo',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
      end)
    .When('Decorate is called twice on the same service',
      procedure(W: TFooWorld)
      begin
        W.Services.Decorate<IFoo>(
          function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
          begin
            Result := TWrappedFoo.Create(Inner, 'inner');
          end);
        W.Services.Decorate<IFoo>(
          function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
          begin
            Result := TWrappedFoo.Create(Inner, 'outer');
          end);
      end)
    .&Then('the second decorator wraps the first, which wraps the original',
      procedure(W: TFooWorld)
      begin
        Expect(W.ResolveName).ToEqual('outer(inner(app))');
      end)

.Rule('Decoration preserves the lifetime of the registration it wraps')

  .Scenario('A decorated singleton is still a singleton')
    .Given('IFoo registered as a decorated singleton',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
        W.Services.Decorate<IFoo>(
          function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
          begin
            Result := TWrappedFoo.Create(Inner, 'log');
          end);
      end)
    .When('it is resolved twice from the root provider',
      procedure(W: TFooWorld)
      begin
        W.Build;
        var First := W.Provider.GetRequiredService<IFoo>;
        var Second := W.Provider.GetRequiredService<IFoo>;
        W.Names := [BoolToStr(First = Second, True)];
      end)
    .&Then('both times it is the same decorated instance',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[0]).ToEqual(BoolToStr(True, True));
      end)

  .Scenario('A decorated scoped service is still scoped')
    .Given('IFoo registered as a decorated scoped service',
      procedure(W: TFooWorld)
      begin
        W.Services.AddScoped<IFoo, TAppFoo>;
        W.Services.Decorate<IFoo>(
          function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
          begin
            Result := TWrappedFoo.Create(Inner, 'log');
          end);
      end)
    .When('it is resolved twice in one scope and once in another',
      procedure(W: TFooWorld)
      begin
        W.Build;
        var ScopeA := W.Provider.CreateScope;
        var ScopeB := W.Provider.CreateScope;
        var A1 := ScopeA.ServiceProvider.GetRequiredService<IFoo>;
        var A2 := ScopeA.ServiceProvider.GetRequiredService<IFoo>;
        var B1 := ScopeB.ServiceProvider.GetRequiredService<IFoo>;
        W.Names := [BoolToStr(A1 = A2, True), BoolToStr(A1 = B1, True)];
      end)
    .&Then('within a scope it is always the same instance, and across scopes it is not',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[0]).ToEqual(BoolToStr(True, True));
        Expect(W.Names[1]).ToEqual(BoolToStr(False, True));
      end)

  .Scenario('A decorated transient is still transient')
    .Given('IFoo registered as a decorated transient',
      procedure(W: TFooWorld)
      begin
        W.Services.AddTransient<IFoo, TAppFoo>;
        W.Services.Decorate<IFoo>(
          function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
          begin
            Result := TWrappedFoo.Create(Inner, 'log');
          end);
      end)
    .When('it is resolved twice',
      procedure(W: TFooWorld)
      begin
        W.Build;
        var First := W.Provider.GetRequiredService<IFoo>;
        var Second := W.Provider.GetRequiredService<IFoo>;
        W.Names := [BoolToStr(First = Second, True)];
      end)
    .&Then('each resolution is a new decorated instance',
      procedure(W: TFooWorld)
      begin
        Expect(W.Names[0]).ToEqual(BoolToStr(False, True));
      end)

.Rule('Decorating is a change to the collection, so it stops when the collection does')

  .Scenario('Decorating after building fails out loud')
    .Given('a collection whose provider was already built',
      procedure(W: TFooWorld)
      begin
        W.Services.AddSingleton<IFoo, TAppFoo>;
        W.Build;
      end)
    .When('Decorate is called on it',
      procedure(W: TFooWorld)
      begin
        W.Failure := '';
        try
          W.Services.Decorate<IFoo>(
            function(const Inner: IFoo; const Provider: IServiceProvider): IFoo
            begin
              Result := Inner;
            end);
        except
          on E: Exception do W.Failure := E.Message;
        end;
      end)
    .&Then('it raises just like an Add would, saying the collection admits no more changes',
      procedure(W: TFooWorld)
      begin
        Expect(W.Failure).ToContain('frozen');
      end);

end.
