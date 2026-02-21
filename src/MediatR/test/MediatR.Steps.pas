unit MediatR.Steps;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.Extensions.DependencyInjection,
  Daf.MediatR.Contracts,
  MediatR.Feat;

type
  /// <summary>
  /// Step bindings for the MediatR feature.
  /// Regex captures are auto-converted to method parameters.
  /// </summary>
  TMediatRSteps = class
  public
    // === Given ===

    [Given('a configured mediator')]
    procedure GivenMediatorReady(W: TMediatRWorld);

    // === When ===

    [When('I send a TJing request')]
    procedure SendJing(W: TMediatRWorld);

    [When('I send a TPing request and get Pong1')]
    procedure SendPingAndVerifyPong1(W: TMediatRWorld);

    [When('I publish a TPonged notification')]
    procedure PublishPonged(W: TMediatRWorld);

    [When('I send requests from different scopes')]
    procedure SendFromScopes(W: TMediatRWorld);

    // === Then ===

    [ThenAttribute('the TJing handler should have been invoked')]
    procedure VerifyJingHandled(W: TMediatRWorld);

    [ThenAttribute('the dependency mock should have been visited once')]
    procedure VerifyDependencyVisited(W: TMediatRWorld);

    [ThenAttribute('handler 1 should have been invoked')]
    procedure VerifyPongedHandler1(W: TMediatRWorld);

    [ThenAttribute('handler 2 should have been invoked')]
    procedure VerifyPongedHandler2(W: TMediatRWorld);

    [ThenAttribute('each scope has its own dependency instance')]
    procedure VerifyScopedDependencies(W: TMediatRWorld);
  end;

{ TMediatRSteps }

procedure TMediatRSteps.GivenMediatorReady(W: TMediatRWorld);
begin
  // Reset all class-level handler flags to ensure test isolation
  TJingHandler.Done := False;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.SendJing(W: TMediatRWorld);
begin
  W.Mediator.Send(TJing.Create);
end;

procedure TMediatRSteps.SendPingAndVerifyPong1(W: TMediatRWorld);
var
  Response: string;
begin
  Response := W.Mediator.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');
end;

procedure TMediatRSteps.PublishPonged(W: TMediatRWorld);
begin
  W.Mediator.Publish(TPonged.Create);
end;

procedure TMediatRSteps.SendFromScopes(W: TMediatRWorld);
var
  Scope1, Scope2: IServiceScope;
  Mediator1, Mediator2: IMediator;
  Response: string;
begin
  Scope1 := W.RootProvider.CreateScope;
  Scope2 := W.RootProvider.CreateScope;

  Mediator1 := Scope1.ServiceProvider.GetRequiredService<IMediatorImpl>;
  Mediator2 := Scope2.ServiceProvider.GetRequiredService<IMediatorImpl>;

  Response := Mediator1.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');

  Response := Mediator1.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong2');

  Response := Mediator2.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');

  Response := Mediator2.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong2');
end;

procedure TMediatRSteps.VerifyJingHandled(W: TMediatRWorld);
begin
  Expect(TJingHandler.Done).ToBeTrue;
end;

procedure TMediatRSteps.VerifyDependencyVisited(W: TMediatRWorld);
var
  D: IDependencyMock;
begin
  D := W.RootProvider.GetRequiredService<IDependencyMock>;
  Expect(D.Visites).ToEqual(1);
end;

procedure TMediatRSteps.VerifyPongedHandler1(W: TMediatRWorld);
begin
  Expect(TPongedHandler1.Done).ToBeTrue;
end;

procedure TMediatRSteps.VerifyPongedHandler2(W: TMediatRWorld);
begin
  Expect(TPongedHandler2.Done).ToBeTrue;
end;

procedure TMediatRSteps.VerifyScopedDependencies(W: TMediatRWorld);
var
  Scope1, Scope2: IServiceScope;
  D1, D2: IDependencyMock;
begin
  Scope1 := W.RootProvider.CreateScope;
  Scope2 := W.RootProvider.CreateScope;

  D1 := Scope1.ServiceProvider.GetRequiredService<IDependencyMock>;
  D2 := Scope2.ServiceProvider.GetRequiredService<IDependencyMock>;
  // Fresh scopes = fresh dependency instances, not visited yet
  Expect(D1.Visites).ToEqual(0);
  Expect(D2.Visites).ToEqual(0);
  // Different instances
  Expect(D1 = D2).ToBeFalse;
end;

initialization
  Bindings.RegisterSteps<TMediatRSteps>;

end.
