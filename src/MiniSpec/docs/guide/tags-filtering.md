# Tags and Filtering

**🌍 Language: English | [Español](tags-filtering.es.md)**

[← Back to Guide](../GUIDE.md)

---

**Tags** allow categorizing and filtering scenarios. Add them in the description, preferably at the end:

```pascal
Feature('''
Calculator

  @math @core
''')

.Scenario('''
  Division by zero
  @error @edge-case
''')
```

## Running with Filters

```bash
# Only scenarios with @unit
MyApp.exe -f "@unit"

# Scenarios @unit but NOT @slow
MyApp.exe -f "@unit and ~@slow"

# By feature title
MyApp.exe -f "Feat:Calculator"

# By category
MyApp.exe -f "Cat:Login"

# Complex expressions
MyApp.exe -f "(Feat:Login or @auth) and ~@slow"
```

## Filter Syntax

| Expression | Meaning |
|------------|---------|
| `@tag` | Has the tag |
| `~@tag` | Does NOT have the tag |
| `Feat:text` | Feature title contains text |
| `Scen:text` | Scenario description contains text |
| `Rule:text` | Rule description contains text |
| `Cat:text` | Category contains text |
| `and`, `or` | Logical operators |
| `()` | Grouping |

> ⚠️ Tags **must** start with `@`. Writing `--filter unit` will error; use `--filter @unit`.

---

[← Step Bindings](step-bindings.md) | [Next: Assertions →](assertions.md)
