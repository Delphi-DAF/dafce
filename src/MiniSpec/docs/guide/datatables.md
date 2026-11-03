# DataTables: Structured Data

**🌍 Language: English | [Español](datatables.es.md)**

[← Back to Guide](../GUIDE.md)

---

Sometimes a step needs more complex data than simple values. **DataTables** allow passing tabular structures:

```pascal
.Scenario('Create multiple users')
  .Given('the following users:', [
    ['name',   'email',            'role'],
    ['Alice',  'alice@test.com',   'admin'],
    ['Bob',    'bob@test.com',     'user'],
    ['Carol',  'carol@test.com',   'user']
  ], procedure(World: TMyWorld)
    begin
      var Table := SpecContext.DataTable;
      
      for var I := 0 to Table.RowCount - 1 do
      begin
        var Row := Table.AsMap(I);  // Row as dictionary
        World.Input.Add(            // input data, not yet processed
          Row['name'].AsString,
          Row['email'].AsString,
          Row['role'].AsString
        );
      end;
    end)
  .When('I import them into the directory', procedure(World: TMyWorld)
    begin
      World.Directory.Import(World.Input);  // the SUT: the event happens here
    end)
  .&Then('the directory has 3 users', procedure(World: TMyWorld)
    begin
      Expect(World.Directory.Count).ToEqual(3);
    end)
```

## DataTable API

| Property/Method | Description |
|-----------------|-------------|
| `RowCount` | Number of rows (not counting headers) |
| `ColCount` | Number of columns |
| `Headers` | Array with column names |
| `Cell[row, col]` | Cell by indices |
| `Cell[row, 'name']` | Cell by column name |
| `AsMap(row)` | Row as `TDictionary<string, TValue>` |
| `AsList` | All rows as array of dictionaries |
| `Transpose` | New table with rows/columns swapped |

---

[← Scenario Outline](scenario-outline.md) | [Next: Step Bindings →](step-bindings.md)
