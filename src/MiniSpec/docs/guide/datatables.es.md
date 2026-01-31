# DataTables: Datos Estructurados

**🌍 Idioma: [English](datatables.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

A veces un paso necesita datos más complejos que simples valores. Las **DataTables** permiten pasar estructuras tabulares:

```pascal
.Scenario('Crear múltiples usuarios')
  .Given('los siguientes usuarios:', [
    ['nombre', 'email',            'rol'],
    ['Alice',  'alice@test.com',   'admin'],
    ['Bob',    'bob@test.com',     'user'],
    ['Carol',  'carol@test.com',   'user']
  ], procedure(World: TMyWorld)
    begin
      var Table := SpecContext.DataTable;
      
      for var I := 0 to Table.RowCount - 1 do
      begin
        var Row := Table.AsMap(I);  // Fila como diccionario
        World.Users.Add(
          Row['nombre'].AsString,
          Row['email'].AsString,
          Row['rol'].AsString
        );
      end;
    end)
  .When('cuento los usuarios', procedure(World: TMyWorld)
    begin
      World.Count := World.Users.Count;
    end)
  .&Then('tengo 3 usuarios', procedure(World: TMyWorld)
    begin
      Expect(World.Count).ToEqual(3);
    end)
```

## API de DataTable

| Propiedad/Método | Descripción |
|------------------|-------------|
| `RowCount` | Número de filas (sin contar headers) |
| `ColCount` | Número de columnas |
| `Headers` | Array con nombres de columnas |
| `Cell[row, col]` | Celda por índices |
| `Cell[row, 'name']` | Celda por nombre de columna |
| `AsMap(row)` | Fila como `TDictionary<string, TValue>` |
| `AsList` | Todas las filas como array de diccionarios |
| `Transpose` | Nueva tabla con filas/columnas intercambiadas |

---

[← Scenario Outline](scenario-outline.es.md) | [Siguiente: Step Bindings →](step-bindings.es.md)
