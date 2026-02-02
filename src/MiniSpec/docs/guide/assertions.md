# Assertions (Expect)

**🌍 Language: English | [Español](assertions.es.md)**

[← Back to Guide](../GUIDE.md)

---

MiniSpec provides a fluent API for verifications. Methods are self-explanatory:

```pascal
// Values
Expect(result).ToEqual(5);
Expect(value).ToNotEqual(0);
Expect(number).ToBeBetween(1, 10);

// Strings
Expect(message).ToContain('error');
Expect(email).ToMatch('^[\w.-]+@[\w.-]+\.\w+$');

// Booleans and nulls
Expect(active).ToBeTrue;
Expect(obj).ToNotBeNull;
```

## Verifying Exceptions

An important special case: verifying that code throws an exception. MiniSpec **automatically captures** exceptions in the When step, allowing verification in Then:

```pascal
.Scenario('Division by zero')
  .Given('the numbers 10 and 0', procedure(World: TWorld)
    begin
      World.A := 10;
      World.B := 0;
    end)
  .When('dividing', procedure(World: TWorld)
    begin
      World.Calculator.Divide(World.A, World.B);  // Exception captured
    end)
  .&Then('raises EDivByZero', procedure(World: TWorld)
    begin
      Expect(Raised).ToBe(EDivByZero);
    end);
```

## Exception Methods

| Method | Description |
|--------|-------------|
| `Expect(Raised).ToBeAny` | Any exception was raised |
| `Expect(Raised).ToBe(EMyException)` | Specific type was raised |
| `Expect(Raised).ToHaveMessage('text')` | Message contains substring |
| `Expect(Raised).ToBeNone` | No exception was raised |

> 💡 Check the source code of `Daf.MiniSpec.Expect.pas` for all available methods.

---

[← Tags and Filtering](tags-filtering.md) | [Next: Rules →](rules.md)
