# Assertions (Expect)

**🌍 Language: English | [Español](assertions.es.md)**

[← Back to Guide](../GUIDE.md)

---

MiniSpec provides a fluent API for verifications:

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

## Value matchers

Every one of them is on the record `Expect` returns. They compare through a `Variant`, so the
overload you call decides nothing about the comparison.

| Matcher | Passes when |
|---------|-------------|
| `ToEqual(V)` / `ToEqual<T>(V)` | The value equals `V` |
| `ToNotEqual(V)` / `ToNotEqual<T>(V)` | It does not |
| `ToBeGreaterThan(V)` `ToBeGreaterOrEqual(V)` | Ordered comparison |
| `ToBeLessThan(V)` `ToBeLessOrEqual(V)` | The same, the other way |
| `ToBeBetween(Min, Max)` | `Min <= value <= Max`, both ends included |
| `ToBeCloseTo(V, Tolerance)` | `Abs(value - V) <= Tolerance`. `Tolerance` defaults to `0.0001` |
| `ToBeWithinPercent(V, Percent)` | The same, with the tolerance as a percentage of `V` |
| `ToBeTrue` / `ToBeFalse` | The value is that boolean |
| `ToBeNull` / `ToNotBeNull` | The variant is `Null`, or is not |
| `ToBeEmpty` | The variant is `Empty`, or its string form is `''` |
| `ToContain(S)` | The string form contains `S`, **ignoring case** |
| `ToStartWith(S)` / `ToEndWith(S)` | It starts or ends with `S`, also ignoring case |
| `ToMatch(Pattern)` | `TRegEx.IsMatch` over the string form — a **regular expression**, not a glob |
| `ToHaveCount(N)` | The value is a variant **array** with `N` elements. Anything else fails with `Expected array value` |

⚠️ **`ToBeEmpty` does not print the value it found.** Its failure says `Expected value to be empty`
and no more, so it cannot name what was there. When the failure has to name something, assert with
`ToEqual` against a sentinel instead.

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
      ExpectException(Raised).ToBe(EDivByZero);
    end);
```

## Exception Methods

| Method | Description |
|--------|-------------|
| `ExpectException(Raised).ToBeAny` | Any exception was raised |
| `ExpectException(Raised).ToBe(EMyException)` | Specific type was raised |
| `ExpectException(Raised).ToBe<EMyException>` | The same, written as a type argument |
| `ExpectException(Raised).ToHaveMessage('text')` | Message contains substring |
| `ExpectException(Raised).ToBeNone` | No exception was raised |
| `ExpectException(Raised).ToMatchMessage('re')` | Message matches a **regular expression** |

`Raised` is the exception that escaped the **When** step. `ExpectException` also takes the
call itself, for a check that is not the scenario's event:

```pascal
ExpectException(
  procedure
  begin
    World.Calculator.Divide(10, 0);
  end).ToBe<EDivByZero>;
```

⚠️ `ToMatchMessage` is on `TExpectException`, not on `TExpect`: calling it on a plain value gives
`E2003 Undeclared identifier`. And it is a regex, so `'*Between*2*1*'` matches nothing where
`'Between.*2.*1'` matches.

---

[← Tags and Filtering](tags-filtering.md) | [Next: Rules →](rules.md)
