# Step Bindings: Organizing Complex Steps

**🌍 Language: English | [Español](step-bindings.es.md)**

[← Back to Guide](../GUIDE.md)

---

As your specifications grow, you'll notice many steps repeat:

```pascal
// In Feature A:
.Given('an authenticated user', procedure(W: TWorldA) begin ... end)

// In Feature B:
.Given('an authenticated user', procedure(W: TWorldB) begin ... end)  // Duplicate!
```

**Step Bindings** solve this: define steps as class methods, using regex patterns:

```pascal
unit Auth.Steps.pas;

interface

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Binding;

type
  TAuthBindings = class
  public
    [Given('a user "([^"]+)" with password "([^"]+)"')]
    procedure SetupUser(World: TObject; Username, Password: string);
    
    [When('logs in')]
    procedure DoLogin(World: TObject);
    
    [ThenAttribute('login is successful')]
    procedure VerifyLoginSuccess(World: TObject);
    
    [ThenAttribute('login fails with "([^"]+)"')]
    procedure VerifyLoginError(World: TObject; ExpectedError: string);
  end;

implementation

procedure TAuthBindings.SetupUser(World: TObject; Username, Password: string);
begin
  var W := World as TAuthWorld;
  W.Username := Username;
  W.Password := Password;
end;

// ... rest of implementations
```

## Register and Use

```pascal
initialization
  Bindings.RegisterSteps<TAuthBindings>;
  
  Feature('Authentication')
  .UseWorld<TAuthWorld>
  
  .Scenario('Valid login')
    .Given('a user "admin" with password "secret123"')  // Uses the binding
    .When('logs in')
    .&Then('login is successful')
    
  .Scenario('Wrong password')
    .Given('a user "admin" with password "wrong"')
    .When('logs in')
    .&Then('login fails with "Invalid credentials"')
```

## Binding Characteristics

| Aspect | Description |
|--------|-------------|
| Patterns | Regex with capture groups for parameters |
| Types | `Integer`, `Int64`, `Float`, `string`, `Boolean` |
| First parameter | Always the World (use `TObject` and cast) |
| Priority | Inline lambda > Registered binding |
| Attributes | `[GivenAttribute]`, `[WhenAttribute]`, `[ThenAttribute]` |

> 💡 Bindings are ideal for common steps (authentication, data setup, etc.) used across multiple features.

---

[← DataTables](datatables.md) | [Next: Tags and Filtering →](tags-filtering.md)
