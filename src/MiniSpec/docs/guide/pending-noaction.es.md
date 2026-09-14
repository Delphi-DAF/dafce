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

## Dónde cae

`.NoAction` sustituye el cuerpo del **último step declarado**, sea del tipo que sea:

```pascal
.Given('contexto descriptivo').NoAction
.When('acción implícita').NoAction
```

⚠️ **Nada impide ponerlo en un `Then`, y nada avisa.** El step pasa a ser un cuerpo que no hace
nada y que pasa, así que el escenario se queda con un `Then` que no verifica nada y sigue en verde
— que es peor que un fallo, porque parece cobertura.

```pascal
.&Then('verificación').NoAction  // compila, se ejecuta, no verifica nada
```

---

[← Vocabulario Gherkin](gherkin-vocabulary.es.md) | [Siguiente: Hooks →](hooks.es.md)
