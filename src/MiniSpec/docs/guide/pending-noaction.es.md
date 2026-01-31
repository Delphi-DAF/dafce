# Pending y NoAction

**🌍 Idioma: [English](pending-noaction.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

MiniSpec proporciona dos formas de marcar steps que no tienen código ejecutable:

| Método | Semántica | Resultado | Uso típico |
|--------|-----------|-----------|------------|
| `.Pending` | "Pendiente de implementar" | Skip + detiene escenario | Work in progress |
| `.NoAction` | "Sin acción requerida" | Pass + continúa escenario | Steps puramente descriptivos |

## Pending

Indica que un step está pendiente de implementar. El escenario se marca como Skip y **no se ejecutan los steps siguientes**:

```pascal
.Scenario('Feature en desarrollo')
  .Given('una precondición', procedure(Ctx: TMyWorld) begin end)
  .When('algo pendiente de implementar')
    .Pending                          // Escenario Skip, no sigue
  .&Then('nunca se ejecuta', procedure(Ctx: TMyWorld)
    begin
      // Este código no se ejecuta
    end)
```

## NoAction

Indica que un step es puramente descriptivo y no requiere código. El step pasa automáticamente y **los steps siguientes sí se ejecutan**:

```pascal
.Scenario('Con steps descriptivos')
  .Given('el sistema está configurado')  // Descriptivo, el sistema ya está listo
    .NoAction
  .When('el usuario hace login', procedure(Ctx: TMyWorld)
    begin
      Ctx.DoLogin;  // Este código SÍ se ejecuta
    end)
  .&Then('accede al dashboard', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.IsLoggedIn).ToBeTrue;  // También se ejecuta
    end)
```

## Restricción

`.NoAction` solo es válido para steps Given y When. Los steps Then siempre deben contener verificaciones (assertions), por lo que usar `.NoAction` en Then lanzará una excepción.

```pascal
// ✓ Válido
.Given('contexto descriptivo').NoAction
.When('acción implícita').NoAction

// ✗ Error en tiempo de ejecución
.&Then('verificación').NoAction  // Lanza excepción
```

---

[← Vocabulario Gherkin](gherkin-vocabulary.es.md) | [Siguiente: Hooks →](hooks.es.md)
