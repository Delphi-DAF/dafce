# Assertions (Expect)

**🌍 Idioma: [English](assertions.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

MiniSpec proporciona una API fluida para verificaciones. Los métodos son autoexplicativos:

```pascal
// Valores
Expect(resultado).ToEqual(5);
Expect(valor).ToNotEqual(0);
Expect(numero).ToBeBetween(1, 10);

// Strings
Expect(mensaje).ToContain('error');
Expect(email).ToMatch('^[\w.-]+@[\w.-]+\.\w+$');

// Booleanos y nulos
Expect(activo).ToBeTrue;
Expect(objeto).ToNotBeNull;
```

## Verificando Excepciones

Un caso especial importante: verificar que el código lanza una excepción. MiniSpec **captura automáticamente** las excepciones en el step When, permitiendo verificarlas en el Then:

```pascal
.Scenario('División por cero')
  .Given('los números 10 y 0', procedure(World: TWorld)
    begin
      World.A := 10;
      World.B := 0;
    end)
  .When('se divide', procedure(World: TWorld)
    begin
      World.Calculator.Divide(World.A, World.B);  // Excepción capturada
    end)
  .&Then('lanza EDivByZero', procedure(World: TWorld)
    begin
      ExpectException(Raised).ToBe(EDivByZero);
    end);
```

## Métodos para Excepciones

| Método | Descripción |
|--------|-------------|
| `ExpectException(Raised).ToBeAny` | Se lanzó cualquier excepción |
| `ExpectException(Raised).ToBe(EMyException)` | Se lanzó tipo específico |
| `ExpectException(Raised).ToHaveMessage('texto')` | Mensaje contiene substring |
| `ExpectException(Raised).ToBeNone` | No se lanzó excepción |

> 💡 Consulta el código fuente de `Daf.MiniSpec.Expect.pas` para ver todos los métodos disponibles.

---

[← Tags y Filtrado](tags-filtering.es.md) | [Siguiente: Rules →](rules.es.md)
