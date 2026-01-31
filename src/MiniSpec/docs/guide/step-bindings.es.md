# Step Bindings: Organizando Pasos Complejos

**🌍 Idioma: [English](step-bindings.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Cuando tus especificaciones crecen, notarás que muchos pasos se repiten:

```pascal
// En Feature A:
.Given('un usuario autenticado', procedure(W: TWorldA) begin ... end)

// En Feature B:
.Given('un usuario autenticado', procedure(W: TWorldB) begin ... end)  // ¡Duplicado!
```

Los **Step Bindings** resuelven esto: defines pasos como métodos de una clase, usando patrones regex:

```pascal
unit Auth.Steps.pas;

interface

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Binding;

type
  TAuthBindings = class
  public
    [Given('un usuario "([^"]+)" con password "([^"]+)"')]
    procedure SetupUser(World: TObject; Username, Password: string);
    
    [When('hace login')]
    procedure DoLogin(World: TObject);
    
    [ThenAttribute('el login es exitoso')]
    procedure VerifyLoginSuccess(World: TObject);
    
    [ThenAttribute('el login falla con "([^"]+)"')]
    procedure VerifyLoginError(World: TObject; ExpectedError: string);
  end;

implementation

procedure TAuthBindings.SetupUser(World: TObject; Username, Password: string);
begin
  var W := World as TAuthWorld;
  W.Username := Username;
  W.Password := Password;
end;

// ... resto de implementaciones
```

## Registrar y Usar

```pascal
initialization
  Bindings.RegisterSteps<TAuthBindings>;
  
  Feature('Autenticación')
  .UseWorld<TAuthWorld>
  
  .Scenario('Login válido')
    .Given('un usuario "admin" con password "secret123"')  // Usa el binding
    .When('hace login')
    .&Then('el login es exitoso')
    
  .Scenario('Password incorrecto')
    .Given('un usuario "admin" con password "wrong"')
    .When('hace login')
    .&Then('el login falla con "Credenciales inválidas"')
```

## Características de los Bindings

| Aspecto | Descripción |
|---------|-------------|
| Patrones | Regex con grupos de captura para parámetros |
| Tipos | `Integer`, `Int64`, `Float`, `string`, `Boolean` |
| Primer parámetro | Siempre el World (usa `TObject` y haz cast) |
| Prioridad | Lambda inline > Binding registrado |
| Atributos | `[GivenAttribute]`, `[WhenAttribute]`, `[ThenAttribute]` |

> 💡 Los bindings son ideales para pasos comunes (autenticación, setup de datos, etc.) que se usan en múltiples features.

---

[← DataTables](datatables.es.md) | [Siguiente: Tags y Filtrado →](tags-filtering.es.md)
