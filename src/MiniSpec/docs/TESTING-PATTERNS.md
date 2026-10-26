# Testing Patterns with MiniSpec

**🌍 Language: English | [Español](TESTING-PATTERNS.es.md)**

This guide complements the [User Guide](GUIDE.md) by showing how to adapt BDD to different testing levels. Although BDD was born to capture business requirements, its expressive vocabulary is useful at any level of the test pyramid.

⚠️ **The file names in this guide are a suggestion for your project, not DAF's own convention.** Here the suffix carries the test level: `.Spec.pas` for a class, `.Feat.pas` for a feature. DAF's own tree names every spec `[Feature].Feat.pas` and every step file `[Feature].Steps.pas`, whatever the level.

---

## Table of Contents

- [The Test Pyramid](#the-test-pyramid)
- [E2E Tests: Specifying the System](#e2e-tests-specifying-the-system)
- [Unit Tests: Specifying Classes](#unit-tests-specifying-classes)
- [Integration Tests: Specifying APIs](#integration-tests-specifying-apis)
- [Organizing by Test Type](#organizing-by-test-type)
- [Conclusion](#conclusion)

---

## The Test Pyramid

```
        /\
       /  \      E2E Tests
      /    \     (Complete system, business requirements)
     /------\
    /        \   Integration Tests
   /          \  (APIs, services, connected components)
  /------------\
 /              \ Unit Tests
/________________\(Individual classes, pure functions)
```

| Level | SUT | Speed | Fragility | Focus |
|-------|-----|-------|-----------|-------|
| **E2E** | Complete system | Slow | High | Business requirements |
| **Integration** | API / Service | Medium | Medium | Contracts between components |
| **Unit** | Class / Function | Fast | Low | Isolated behavior |

MiniSpec can be used at all three levels. The key is **what you describe** and **how you structure** your specifications.

---

## E2E Tests: Specifying the System

This is the "classic" use of BDD, covered in detail in the [User Guide](GUIDE.md). The SUT is the **complete system** and you describe **business requirements**:

```pascal
Feature('''
Checkout Process

  As a customer
  I need to complete my purchase
  To receive products at my address

  @e2e @checkout
''')

.UseWorld<TCheckoutWorld>

.Scenario('Successful purchase with credit card')
  .Given('I have products in the cart', procedure(W: TCheckoutWorld)
    begin
      W.Cart.Add(TProduct.Create('Laptop', 999.99));
    end)
  .When('I complete payment with valid card', procedure(W: TCheckoutWorld)
    begin
      W.Checkout.Pay(TCreditCard.Create('4111111111111111'));
    end)
  .&Then('I receive order confirmation', procedure(W: TCheckoutWorld)
    begin
      Expect(W.Checkout.OrderConfirmed).ToBeTrue;
      Expect(W.Checkout.OrderNumber).ToMatch('^\d{8}$');
    end)
```

**E2E test characteristics:**

- **Business** vocabulary, not technical
- The World orchestrates multiple components
- Can be slow (real database, external services)
- Ideal for **acceptance criteria**

---

## Unit Tests: Specifying Classes

When the SUT is an **individual class**, BDD remains useful. Think of frameworks like RSpec (Ruby) or Jest (JavaScript) that use `describe()` to group behaviors.

### The Pattern: Feature = Class, Scenario = Method/Behavior

```pascal
unit TStringBuilder.Spec;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

type
  TStringBuilderWorld = class
  public
    SUT: TStringBuilder;  // System Under Test
    Result: string;
    destructor Destroy; override;
  end;

destructor TStringBuilderWorld.Destroy;
begin
  SUT.Free;
  inherited;
end;

initialization

Feature('''
TStringBuilder @unit

  Class for building strings efficiently
  through incremental concatenation.
''')

.UseWorld<TStringBuilderWorld>

.Background
  .Given('an empty StringBuilder', procedure(W: TStringBuilderWorld)
    begin
      W.SUT := TStringBuilder.Create;
    end)

.Rule('Append: adds text at the end')

  .Scenario('Append a string')
    .When('I add "Hello"', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append('Hello');
      end)
    .&Then('content is "Hello"', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.ToString).ToEqual('Hello');
      end)

  .Scenario('Chained Append')
    .When('I add "Hello" then " World"', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append('Hello').Append(' World');
      end)
    .&Then('content is "Hello World"', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.ToString).ToEqual('Hello World');
      end)

.Rule('Clear: empties the content')

  .Scenario('Clear after adding')
    .Given('existing content', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append('Existing content');
      end)
    .When('I call Clear', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Clear;
      end)
    .&Then('content is empty', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.ToString).ToEqual('');
        Expect(W.SUT.Length).ToEqual(0);
      end)

.Rule('Length: returns current length')

  .Scenario('Initial Length is zero')
    .&Then('Length is 0', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.Length).ToEqual(0);
      end)

  .ScenarioOutline('Length after Append')
    .When('I add <text>', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append(W.Text);
      end)
    .&Then('Length is <length>', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.Length).ToEqual(W.Length);
      end)
    .Examples([
      ['text',    'length'],
      ['',        0],
      ['a',       1],
      ['Hello',   5],
      ['こんにちは', 5]  // Unicode
    ])

end.
```

### Conventions for Unit Tests

| Element | Convention | Example |
|---------|------------|---------|
| Feature | Class name | `TStringBuilder`, `TCalculator` |
| Rule | Method or behavior group | `Append: adds text`, `Input validation` |
| Scenario | Specific behavior case | `Append empty string` |
| Tag | `@unit` for filtering | `TStringBuilder @unit` |
| World | Contains only SUT and test data | `TStringBuilderWorld` |

### Comparison with RSpec

If you come from RSpec, here's the equivalence:

```ruby
# RSpec (Ruby)
describe TStringBuilder do
  describe '#append' do
    it 'adds text to the end' do
      builder = TStringBuilder.new
      builder.append('Hello')
      expect(builder.to_s).to eq('Hello')
    end
  end
end
```

```pascal
// MiniSpec (Delphi)
Feature('TStringBuilder @unit')
.UseWorld<TWorld>

.Rule('Append: adds text at the end')
  .Scenario('Append a string')
    .Given('an empty StringBuilder', ...)
    .When('I add "Hello"', ...)
    .&Then('content is "Hello"', ...)
```

The structure is similar:
- `describe Class` → `Feature('Class')`
- `describe '#method'` → `.Rule('Method: description')`
- `it 'behavior'` → `.Scenario('behavior')`

---

## Integration Tests: Specifying APIs

When the SUT is an **API** (REST, GraphQL, gRPC...), each **endpoint** or **operation** can be a Feature or Rule.

### The Pattern: Feature = Resource/Endpoint, Scenario = Operation

```pascal
unit API.Users.Spec;

interface

implementation

uses
  System.SysUtils,
  System.JSON,
  System.Net.HttpClient,
  Daf.MiniSpec;

type
  TApiWorld = class
  public
    Client: THTTPClient;
    Response: IHTTPResponse;
    ResponseJson: TJSONObject;
    UserId: string;
    destructor Destroy; override;
  end;

destructor TApiWorld.Destroy;
begin
  ResponseJson.Free;
  Client.Free;
  inherited;
end;

initialization

Feature('''
API: /users @integration @api

  User management via REST API.
  Base URL: http://localhost:3000/api/v1
''')

.UseWorld<TApiWorld>

.Background
  .Given('a configured HTTP client', procedure(W: TApiWorld)
    begin
      W.Client := THTTPClient.Create;
      W.Client.ContentType := 'application/json';
    end)

.Rule('GET /users - List users')

  .Scenario('Empty list when no users exist')
    .When('GET /users', procedure(W: TApiWorld)
      begin
        W.Response := W.Client.Get('http://localhost:3000/api/v1/users');
      end)
    .&Then('responds 200 OK', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(200);
      end)
    .&And('returns empty array', procedure(W: TApiWorld)
      begin
        var Json := TJSONObject.ParseJSONValue(W.Response.ContentAsString);
        Expect(Json is TJSONArray).ToBeTrue;
        Expect((Json as TJSONArray).Count).ToEqual(0);
        Json.Free;
      end)

.Rule('POST /users - Create user')

  .Scenario('Create user with valid data')
    .When('POST /users with name and email', procedure(W: TApiWorld)
      begin
        var Body := TJSONObject.Create;
        Body.AddPair('name', 'John Doe');
        Body.AddPair('email', 'john@example.com');
        W.Response := W.Client.Post(
          'http://localhost:3000/api/v1/users',
          TStringStream.Create(Body.ToString)
        );
        Body.Free;
      end)
    .&Then('responds 201 Created', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(201);
      end)
    .&And('returns user with ID', procedure(W: TApiWorld)
      begin
        W.ResponseJson := TJSONObject.ParseJSONValue(
          W.Response.ContentAsString) as TJSONObject;
        Expect(W.ResponseJson.GetValue('id')).ToNotBeNull;
        Expect(W.ResponseJson.GetValue<string>('name')).ToEqual('John Doe');
        W.UserId := W.ResponseJson.GetValue<string>('id');
      end)

  .Scenario('Error with invalid email')
    .When('POST /users with malformed email', procedure(W: TApiWorld)
      begin
        var Body := TJSONObject.Create;
        Body.AddPair('name', 'John');
        Body.AddPair('email', 'not-an-email');
        W.Response := W.Client.Post(
          'http://localhost:3000/api/v1/users',
          TStringStream.Create(Body.ToString)
        );
        Body.Free;
      end)
    .&Then('responds 400 Bad Request', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(400);
      end)
    .&And('includes error message', procedure(W: TApiWorld)
      begin
        W.ResponseJson := TJSONObject.ParseJSONValue(
          W.Response.ContentAsString) as TJSONObject;
        Expect(W.ResponseJson.GetValue<string>('error')).ToContain('email');
      end)

.Rule('GET /users/{id} - Fetch a user')

  .Scenario('User exists')
    .Given('an existing user', procedure(W: TApiWorld)
      begin
        // Create the user first
        var Body := TJSONObject.Create;
        Body.AddPair('name', 'Jane');
        Body.AddPair('email', 'jane@example.com');
        var Resp := W.Client.Post(
          'http://localhost:3000/api/v1/users',
          TStringStream.Create(Body.ToString)
        );
        var Json := TJSONObject.ParseJSONValue(Resp.ContentAsString) as TJSONObject;
        W.UserId := Json.GetValue<string>('id');
        Json.Free;
        Body.Free;
      end)
    .When('GET /users/{id}', procedure(W: TApiWorld)
      begin
        W.Response := W.Client.Get(
          'http://localhost:3000/api/v1/users/' + W.UserId);
      end)
    .&Then('responds 200 OK', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(200);
      end)
    .&And('returns the user data', procedure(W: TApiWorld)
      begin
        W.ResponseJson := TJSONObject.ParseJSONValue(
          W.Response.ContentAsString) as TJSONObject;
        Expect(W.ResponseJson.GetValue<string>('name')).ToEqual('Jane');
      end)

  .Scenario('User does not exist')
    .When('GET /users/999999', procedure(W: TApiWorld)
      begin
        W.Response := W.Client.Get(
          'http://localhost:3000/api/v1/users/999999');
      end)
    .&Then('responds 404 Not Found', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(404);
      end)

end.
```

### Conventions for Integration Tests

| Element | Convention | Example |
|---------|------------|---------|
| Feature | Endpoint or resource | `API: /users`, `GraphQL: User` |
| Rule | HTTP Method + Route | `GET /users`, `POST /users` |
| Scenario | Specific use case | `Create user with valid data` |
| Tag | `@integration` or `@api` | `API: /users @integration @api` |

### Using FeatureContext for Shared State

API specs often need data created in one scenario to be available in another:

```pascal
type
  TApiContext = class
  public
    BaseUrl: string;
    AuthToken: string;
    CreatedIds: TList<string>;
    constructor Create;
    destructor Destroy; override;
  end;

  TApiWorld = class
  private
    [MiniInject] FCtx: TApiContext;
  public
    Client: THTTPClient;
    Response: IHTTPResponse;
    property Ctx: TApiContext read FCtx;
  end;

Feature('API: /orders @integration')
  .UseFeatureContext<TApiContext>
  .UseWorld<TApiWorld>

  .Before('Start test server', procedure
    begin
      TestServer.Start;
    end)

  .After('Clean up test data', procedure
    begin
      TestServer.CleanupTestData;
      TestServer.Stop;
    end)
```

### Common HTTP Status Codes

To assert on HTTP responses:

```pascal
// Reusable helpers
procedure ExpectSuccess(Response: IHTTPResponse);
begin
  Expect(Response.StatusCode).ToBeBetween(200, 299);
end;

procedure ExpectClientError(Response: IHTTPResponse);
begin
  Expect(Response.StatusCode).ToBeBetween(400, 499);
end;

procedure ExpectServerError(Response: IHTTPResponse);
begin
  Expect(Response.StatusCode).ToBeBetween(500, 599);
end;
```

---

## Organizing by Test Type

When you have unit, integration, and E2E tests in the same project, you need a strategy to organize and run them.

### Option A: Single Executable with Tags (Recommended)

Every spec in one project, filtered by tag at run time.

```
MyProject/
├── src/                        # Production code
├── specs/
│   ├── AllSpecs.dpr           # Single executable
│   ├── AllSpecs.groupproj
│   │
│   ├── unit/                  # Unit tests
│   │   ├── TCalculator.Spec.pas
│   │   ├── TStringBuilder.Spec.pas
│   │   └── TValidator.Spec.pas
│   │
│   ├── integration/           # Integration tests
│   │   ├── API.Users.Spec.pas
│   │   ├── API.Orders.Spec.pas
│   │   └── Database.Spec.pas
│   │
│   └── e2e/                   # End-to-end tests
│       ├── Checkout.Feat.pas
│       ├── Registration.Feat.pas
│       └── Search.Feat.pas
```

**Tag every spec with its level:**

```pascal
// Unit test
Feature('TCalculator @unit @math')

// Integration test
Feature('API: /users @integration @api')

// E2E test
Feature('Checkout process @e2e @checkout')
```

**Run by type:**

```bash
# Only unit tests (fast)
AllSpecs.exe -f "@unit"

# Only integration
AllSpecs.exe -f "@integration"

# Only E2E
AllSpecs.exe -f "@e2e"

# Unit + integration — the comma is shorthand for `or`
AllSpecs.exe -f "@unit,@integration"

# Exclude the slow ones — `~` negates
AllSpecs.exe -f "~@slow"

# Unit and integration, but not E2E
AllSpecs.exe -f "(@unit or @integration) and ~@e2e"
```

**Tag conventions:**
- `@unit` — unit tests (fast, no dependencies)
- `@integration` — integration tests (require services)
- `@e2e` — end-to-end tests (complete system)
- `@slow` — slow tests (database, network)
- `@pending` — not yet implemented

**Advantages:** one project, flexible filtering.
**Disadvantages:** the executable links every dependency, E2E included.

---

### Option B: One Executable per Level

Each level gets its own `.dpr`. Worth it when the levels have very different dependencies, or for CI/CD pipelines that run them in parallel.

```
MyProject/
├── src/                        # Production code
├── specs/
│   ├── Specs.groupproj        # Groups every spec project
│   │
│   ├── unit/
│   │   ├── UnitSpecs.dpr      # Executable: unit tests only
│   │   ├── UnitSpecs.dproj
│   │   ├── TCalculator.Spec.pas
│   │   ├── TStringBuilder.Spec.pas
│   │   └── TValidator.Spec.pas
│   │
│   ├── integration/
│   │   ├── IntegrationSpecs.dpr   # Executable: integration only
│   │   ├── IntegrationSpecs.dproj
│   │   ├── API.Users.Spec.pas
│   │   ├── API.Orders.Spec.pas
│   │   └── Database.Spec.pas
│   │
│   └── e2e/
│       ├── E2ESpecs.dpr       # Executable: E2E only
│       ├── E2ESpecs.dproj
│       ├── Checkout.Feat.pas
│       ├── Registration.Feat.pas
│       └── Search.Feat.pas
```

**Each executable configures its own level:**

```pascal
// UnitSpecs.dpr - fast, no external dependencies
program UnitSpecs;
{$APPTYPE CONSOLE}
uses
  Daf.MiniSpec,
  TCalculator.Spec in 'TCalculator.Spec.pas',
  TStringBuilder.Spec in 'TStringBuilder.Spec.pas',
  TValidator.Spec in 'TValidator.Spec.pas';
begin
  MiniSpec
    .Category('Unit Tests')
    .Run;
end.

// IntegrationSpecs.dpr - needs external services
program IntegrationSpecs;
{$APPTYPE CONSOLE}
uses
  Daf.MiniSpec,
  TestServer in '..\common\TestServer.pas',
  API.Users.Spec in 'API.Users.Spec.pas',
  API.Orders.Spec in 'API.Orders.Spec.pas';
begin
  MiniSpec
    .Category('Integration Tests')
    .Before('Start test server', procedure
      begin
        TestServer.Start;
      end)
    .After('Stop test server', procedure
      begin
        TestServer.Stop;
      end)
    .Run;
end.

// E2ESpecs.dpr - the whole system
program E2ESpecs;
{$APPTYPE CONSOLE}
uses
  Daf.MiniSpec,
  Checkout.Feat in 'Checkout.Feat.pas',
  Registration.Feat in 'Registration.Feat.pas';
begin
  MiniSpec
    .Category('E2E Tests')
    .Before('Initialize full system', procedure
      begin
        TestEnvironment.Setup;
      end)
    .After('Cleanup', procedure
      begin
        TestEnvironment.Teardown;
      end)
    .Run;
end.
```

**Advantages:** small executables, separated dependencies, easy to parallelise in CI.
**Disadvantages:** more projects to maintain.

### Summary: When to Use Each Level

| Question | Unit | Integration | E2E |
|----------|------|-------------|-----|
| Is the SUT an isolated class? | ✓ | | |
| Are you testing an API or a service? | | ✓ | |
| Are you describing business requirements? | | | ✓ |
| Do you need a real database? | | Sometimes | ✓ |
| Must the test be very fast? | ✓ | | |
| Will non-technical stakeholders read it? | | | ✓ |

---

## Conclusion

BDD is not exclusive to E2E tests. MiniSpec's vocabulary — Feature, Rule, Scenario, Given/When/Then — gives a clear structure at **any testing level**:

- **Unit tests**: Feature = class, Rule = method
- **Integration tests**: Feature = API/resource, Rule = endpoint
- **E2E tests**: Feature = business capability, Scenario = acceptance criterion

What changes is the **vocabulary** and the **granularity**, never the underlying principle: **specify behaviour with concrete examples**.

*For the complete guide, please refer to the [Spanish version](TESTING-PATTERNS.es.md) while we complete the English translation.*
