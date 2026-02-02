# Testing Patterns with MiniSpec

**🌍 Language: English | [Español](TESTING-PATTERNS.es.md)**

This guide complements the [User Guide](GUIDE.md) by showing how to adapt BDD to different testing levels. Although BDD was born to capture business requirements, its expressive vocabulary is useful at any level of the test pyramid.

---

## Table of Contents

- [The Test Pyramid](#the-test-pyramid)
- [E2E Tests: Specifying the System](#e2e-tests-specifying-the-system)
- [Unit Tests: Specifying Classes](#unit-tests-specifying-classes)
- [Integration Tests: Specifying APIs](#integration-tests-specifying-apis)
- [Organizing by Test Type](#organizing-by-test-type)

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
unit TStringBuilder.Spec.pas;

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
unit API.Users.Spec.pas;

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

end.
```

### Conventions for Integration Tests

| Element | Convention | Example |
|---------|------------|---------|
| Feature | Endpoint or resource | `API: /users`, `GraphQL: User` |
| Rule | HTTP Method + Route | `GET /users`, `POST /users` |
| Scenario | Specific use case | `Create user with valid data` |
| Tag | `@integration` or `@api` | `API: /users @integration @api` |

---

## Organizing by Test Type

When you have unit, integration, and E2E tests in the same project, you need a strategy to organize and run them.

### Option A: Single Executable with Tags (Recommended)

All specs in a single project. Use tags to filter:

```bash
# Only unit tests (fast)
MySpecs.exe -f "@unit"

# Only integration (require services)
MySpecs.exe -f "@integration"

# Only E2E (slow, complete environment)
MySpecs.exe -f "@e2e"

# Unit + integration
MySpecs.exe -f "@unit,@integration"

# Exclude slow ones
MySpecs.exe -f "-@slow"
```

**Advantages:**
- Single project to maintain
- Easy to run everything at once
- CI/CD can run different sets in stages

**Tag conventions:**
- `@unit` — unit tests (fast, no dependencies)
- `@integration` — integration tests (require services)
- `@e2e` — end-to-end tests (complete system)
- `@slow` — slow tests (database, network)
- `@pending` — not yet implemented

### Option B: Separate Executables

Separate projects for each type:

```
specs/
├── UnitSpecs.dpr          # Only @unit
├── IntegrationSpecs.dpr   # Only @integration
└── E2ESpecs.dpr           # Only @e2e
```

**Advantages:**
- Clear dependency separation
- Different Delphi configurations
- Smaller executables

**Disadvantages:**
- Multiple projects to maintain
- Possible code duplication

---

*For the complete guide, please refer to the [Spanish version](TESTING-PATTERNS.es.md) while we complete the English translation.*
