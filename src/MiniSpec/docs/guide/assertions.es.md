# Assertions (Expect)

**🌍 Idioma: [English](assertions.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

MiniSpec proporciona una API fluida para verificaciones:

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

## Matchers de valor

Todos están en el registro que devuelve `Expect`. Comparan a través de un `Variant`, así que la
sobrecarga que llames no decide nada sobre la comparación.

| Matcher | Pasa cuando |
|---------|-------------|
| `ToEqual(V)` / `ToEqual<T>(V)` | El valor es igual a `V` |
| `ToNotEqual(V)` / `ToNotEqual<T>(V)` | No lo es |
| `ToBeGreaterThan(V)` `ToBeGreaterOrEqual(V)` | Comparación de orden |
| `ToBeLessThan(V)` `ToBeLessOrEqual(V)` | Lo mismo al revés |
| `ToBeBetween(Min, Max)` | `Min <= valor <= Max`, los dos extremos incluidos |
| `ToBeCloseTo(V, Tolerancia)` | `Abs(valor - V) <= Tolerancia`. `Tolerancia` vale `0.0001` si no se pasa |
| `ToBeWithinPercent(V, Porcentaje)` | Lo mismo, con la tolerancia como porcentaje de `V` |
| `ToBeTrue` / `ToBeFalse` | El valor es ese booleano |
| `ToBeNull` / `ToNotBeNull` | El variant es `Null`, o no lo es |
| `ToBeEmpty` | El variant es `Empty`, o su forma de cadena es `''` |
| `ToContain(S)` | La forma de cadena contiene `S`, **ignorando mayúsculas** |
| `ToStartWith(S)` / `ToEndWith(S)` | Empieza o acaba por `S`, también ignorando mayúsculas |
| `ToMatch(Patrón)` | `TRegEx.IsMatch` sobre la forma de cadena — una **expresión regular**, no un glob |
| `ToHaveCount(N)` | El valor es un **array** variant con `N` elementos. Cualquier otra cosa falla con `Expected array value` |

⚠️ **`ToBeEmpty` no imprime el valor que encontró.** Su fallo dice `Expected value to be empty` y
nada más, así que no puede nombrar lo que había. Cuando el fallo tiene que nombrar algo, afirma con
`ToEqual` contra un centinela.

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
| `ExpectException(Raised).ToBe<EMyException>` | Lo mismo, escrito como argumento de tipo |
| `ExpectException(Raised).ToHaveMessage('texto')` | Mensaje contiene substring |
| `ExpectException(Raised).ToBeNone` | No se lanzó excepción |
| `ExpectException(Raised).ToMatchMessage('re')` | El mensaje casa con una **expresión regular** |

`Raised` es la excepción que escapó del step **When**. `ExpectException` recibe también la
llamada misma, para una comprobación que no es el evento del escenario:

```pascal
ExpectException(
  procedure
  begin
    World.Calculator.Divide(10, 0);
  end).ToBe<EDivByZero>;
```

⚠️ `ToMatchMessage` está en `TExpectException`, no en `TExpect`: llamarlo sobre un valor da
`E2003 Undeclared identifier`. Y es una regex, así que `'*Between*2*1*'` no casa con nada donde
`'Between.*2.*1'` sí.

---

[← Tags y Filtrado](tags-filtering.es.md) | [Siguiente: Rules →](rules.es.md)
