# MiniSpec Doubles — Test Doubles for Delphi

**🌍 Language: English | [Español](doubles.es.md)**

A minimalist, fluent API for test doubles that matches MiniSpec's elegance. Create stubs, mocks, and spies with an intuitive chainable syntax.

---

## Table of Contents

- [Quick Start](#quick-start)
- [Stubs](#stubs)
- [Mocks](#mocks)
- [Spies](#spies)
- [Argument Matchers](#argument-matchers)
- [Best Practices](#best-practices)

---

## Quick Start

```pascal
uses
  Daf.MiniSpec,
  Daf.MiniSpec.Doubles;

type
  IUserRepository = interface
    function FindById(Id: Integer): IUser;
    procedure Save(User: IUser);
  end;

initialization

Feature('User Service')
.UseWorld<TUserWorld>

.Scenario('Update user email')
  .Given('a user repository', procedure(W: TUserWorld)
    begin
      W.Repo := Stub<IUserRepository>
        .Setup('FindById').WithArgs([42]).Returns(TUser.Create('John'))
        .Instance;
    end)
  .When('updating the email', procedure(W: TUserWorld)
    begin
      W.Service := TUserService.Create(W.Repo);
      W.Service.UpdateEmail(42, 'john@new.com');
    end)
  .&Then('the user is saved', procedure(W: TUserWorld)
    begin
      Expect(W.Repo.WasCalled('Save')).ToBeTrue;
    end)
```

---

## Stubs

Stubs provide canned answers to method calls. Use them when you need to control what collaborators return.

### Basic Stub

```pascal
var Calculator := Stub<ICalculator>
  .Setup('Add').Returns(42)
  .Instance;

// Calculator.Add(anything) returns 42
```

### Conditional Returns with Arguments

```pascal
var Service := Stub<IUserService>
  .Setup('FindById').WithArgs([1]).Returns(User1)
  .Setup('FindById').WithArgs([2]).Returns(User2)
  .Setup('FindById').Returns(nil)  // Default fallback
  .Instance;

// Service.FindById(1) → User1
// Service.FindById(2) → User2  
// Service.FindById(999) → nil
```

### Raising Exceptions

```pascal
var Service := Stub<IPaymentService>
  .Setup('Charge').WithArgs([0]).Raises(EInvalidAmount.Create('Amount must be positive'))
  .Setup('Charge').Returns(True)
  .Instance;

// Service.Charge(0) raises EInvalidAmount
// Service.Charge(100) returns True
```

### Custom Behavior

```pascal
var Counter := 0;
var Service := Stub<ICounter>
  .Setup('Increment').Executes(
    procedure(Args: TArray<TValue>)
    begin
      Inc(Counter);
    end)
  .Instance;
```

### Checking Calls

```pascal
var Stub := Stub<ILogger>
  .Setup('Log').Returns(nil)
  .Instance;

// ... use the stub ...

if Stub.WasCalled('Log') then
  WriteLn('Log was called ', Stub.CallsTo('Log'), ' times');

// Get last call details
var LastCall := Stub.LastCallTo('Log');
WriteLn('Last message: ', LastCall.Args[0].AsString);
```

---

## Mocks

Mocks verify interactions. Use them when the behavior you're testing is about *how* your code interacts with collaborators.

### Basic Expectations

```pascal
var Logger := Mock<ILogger>
  .Expects('Log').Once
  .Expects('Flush').AtLeastOnce
  .Instance;

// ... use logger in your code ...

Logger.Verify;  // Raises EExpectationFailed if expectations not met
```

### Expectation Types

```pascal
.Expects('Method').Never        // Must not be called
.Expects('Method').Once         // Exactly one call
.Expects('Method').Twice        // Exactly two calls
.Expects('Method').AtLeastOnce  // One or more calls
.Expects('Method').AtMostOnce   // Zero or one call
.Expects('Method').Exactly(5)   // Exactly N calls
```

### Combined Setup and Expectations

```pascal
var PaymentGateway := Mock<IPaymentGateway>
  // Setup return values
  .Setup('Authorize').Returns(True)
  .Setup('Capture').Returns('TXN-123')
  // Define expectations
  .Expects('Authorize').Once
  .Expects('Capture').Once
  .Expects('Refund').Never
  .Instance;

// ... process payment ...

PaymentGateway.Verify;
```

### Argument-Specific Expectations

```pascal
var Notifier := Mock<INotifier>
  .Expects('Send').WithArgs(['admin@example.com']).Once
  .Expects('Send').WithArgs(['user@example.com']).Once
  .Instance;
```

---

## Spies

Spies wrap real instances, tracking calls while preserving original behavior.

```pascal
var RealCalc := TCalculator.Create as ICalculator;

var Spy := SpyOn<ICalculator>(RealCalc)
  .TrackAll
  .Instance;

// Use spy - calls go to real implementation
var Result := Spy.Add(2, 3);  // Returns 5 (real behavior)

// Check what happened
Expect(Spy.CallsTo('Add')).ToEqual(1);
Expect(Spy.LastCallTo('Add').Args[0].AsInteger).ToEqual(2);
```

### Selective Tracking

```pascal
var Spy := SpyOn<IService>(RealService)
  .Track('ImportantMethod')
  .Track('AnotherMethod')
  .Instance;
```

---

## Argument Matchers

For flexible argument matching in stubs and mocks.

### Built-in Matchers

```pascal
uses Daf.MiniSpec.Doubles;

// Match any value
.Setup('Process').WithArgs([Arg.Any<Integer>]).Returns(True)

// Match non-null/non-empty
.Setup('Save').WithArgs([Arg.NotNull]).Returns(True)

// Match string containing substring
.Setup('Log').WithArgs([Arg.Contains('error')]).Returns(nil)

// Match value in range
.Setup('SetVolume').WithArgs([Arg.InRange<Integer>(0, 100)]).Returns(nil)

// Custom predicate
.Setup('Validate').WithArgs([
  Arg.Matching<string>(
    function(S: string): Boolean
    begin
      Result := Length(S) >= 5;
    end)
]).Returns(True)
```

### Example with Matchers

```pascal
var EmailService := Stub<IEmailService>
  .Setup('Send')
    .WithArgs([Arg.Contains('@admin'), Arg.Any<string>])
    .Raises(ESecurityException.Create('Cannot email admins'))
  .Setup('Send')
    .WithArgs([Arg.NotNull, Arg.NotNull])
    .Returns(True)
  .Instance;

// EmailService.Send('admin@co.com', 'Hi') → raises
// EmailService.Send('user@co.com', 'Hi') → True
// EmailService.Send('', 'Hi') → False (NotNull fails)
```

---

## Best Practices

### 1. Prefer Stubs Over Mocks

Use stubs when you only need to provide test data. Reserve mocks for when the interaction itself is what you're testing.

```pascal
// ✅ Good: Stub for data
var Repo := Stub<IRepository>
  .Setup('Find').Returns(TestData)
  .Instance;

// ✅ Good: Mock when interaction matters
var Notifier := Mock<INotifier>
  .Expects('SendAlert').Once
  .Instance;
```

### 2. Keep Doubles Simple

Don't over-configure. Set up only what the test needs.

```pascal
// ❌ Too much setup
var Service := Stub<IService>
  .Setup('Method1').Returns(...)
  .Setup('Method2').Returns(...)
  .Setup('Method3').Returns(...)
  .Setup('Method4').Returns(...)
  .Instance;

// ✅ Just what this test needs
var Service := Stub<IService>
  .Setup('GetUser').Returns(TestUser)
  .Instance;
```

### 3. One Mock Per Test

Avoid multiple mocks in a single scenario. If you need many, consider if your design needs refactoring.

### 4. Verify at the End

Call `Verify` in your Then step, after all interactions have occurred.

```pascal
.&Then('notifications were sent', procedure(W: TWorld)
  begin
    W.NotifierMock.Verify;
  end)
```

### 5. Use Descriptive Failures

The verification failures are descriptive:

```
Mock verification failed:
"SendEmail" expected once, but called 0 times
"Log" expected never, but called 3 times
```

---

## Integration with MiniSpec

Doubles work seamlessly with MiniSpec's World pattern:

```pascal
type
  TOrderWorld = class
    Repo: IStubSetup<IOrderRepository>;
    Notifier: IMockExpect<INotifier>;
    Service: TOrderService;
    destructor Destroy; override;
  end;

destructor TOrderWorld.Destroy;
begin
  Service.Free;
  inherited;
end;

initialization

Feature('Order Processing')
.UseWorld<TOrderWorld>

.Background
  .Given('a stubbed repository', procedure(W: TOrderWorld)
    begin
      W.Repo := Stub<IOrderRepository>
        .Setup('FindById').Returns(TOrder.Create(1, 'Pending'));
    end)
  .&And('a mock notifier', procedure(W: TOrderWorld)
    begin
      W.Notifier := Mock<INotifier>
        .Setup('Notify').Returns(nil);
    end)

.Scenario('Completing an order sends notification')
  .Given('the service', procedure(W: TOrderWorld)
    begin
      W.Notifier.Expects('Notify').Once;
      W.Service := TOrderService.Create(W.Repo.Instance, W.Notifier.Instance);
    end)
  .When('completing order #1', procedure(W: TOrderWorld)
    begin
      W.Service.Complete(1);
    end)
  .&Then('customer is notified', procedure(W: TOrderWorld)
    begin
      W.Notifier.Verify;
    end)
```

---

## Requirements

- Delphi 12 Athens or later
- Interfaces with RTTI enabled (`{$M+}` or `{$TYPEINFO ON}`)

---

<p align="center">
  <sub>Part of <a href="../../README.md">MiniSpec</a> — BDD Testing Framework for Delphi</sub>
</p>
