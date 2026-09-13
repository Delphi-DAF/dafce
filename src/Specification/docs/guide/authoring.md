# Authoring with operators

**🌍 Language: English | [Español](authoring.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Comparisons

`Prop` names a member; the overloaded operators build the node.

```pascal
Prop('Price') > 100
Prop('Price') >= 100
Prop('Price') < 100
Prop('Price') <= 100
Prop('Active') = True
Prop('Country') <> 'US'
```

Each of those is one comparison node carrying an operator value. `Prop('Price') > 100` reads back as greater-than, a member reference `Price`, and a value `100`:

```pascal
var C := (Prop('Price') > 100).Node as ICompareExpr;
C.Op                                   // TCompareOp.Greater
(C.Target as IMemberExpr).Name         // 'Price'
(C.Operands[0] as IValueExpr).Value    // 100
```

---

## Combining

```pascal
(Prop('Price') > 100) and (Prop('Active') = True)
(Prop('Country') = 'US') or (Prop('Country') = 'UK')
not (Prop('Active') = True)
```

Parentheses around each comparison are required by Delphi, because `=` and `>` return an expression here rather than a `Boolean`.

Delphi's own precedence binds `and` tighter than `or`, exactly as SQL does, so this needs no parentheses of its own:

```pascal
A and B or C and D      // (A and B) or (C and D)
```

The layer implements no precedence. Write explicit grouping when you mean something else, and the emitter will nest it.

---

## The breadth surface

Anything SQLCute already expresses is reached by emitting the clause that says it, so each of these is an operator value and **no new node type**:

```pascal
Prop('Name').Like('%son%')
Prop('Name').NotLike('%son%')
Prop('Name').StartsWith('And')
Prop('Name').EndsWith('son')
Prop('Name').Contains('nder')
Prop('Price').Between(10, 200)
Prop('Price').NotBetween(10, 200)
Prop('Price').IsIn([5, 120, 900])
Prop('Price').NotIn([5, 120])
Prop('Price').IsNull
Prop('Price').IsNotNull
```

The LIKE family is case-insensitive by default, matching SQLCute. Pass `True` for a case-sensitive match:

```pascal
Prop('Name').Like('%son%', True)
```

---

## Values

A value leaf carries a single `Variant`, which is the representation SQLCute clauses already hold.

A type with no direct `Variant` form is narrowed **where you write it**:

```pascal
Prop('Id') = SomeGuid                        // typed overload → GUIDToString
Prop('Id') = TExprValue.From(SomeGuid)       // the same conversion, explicitly
```

Narrowing at the edge is what makes both lanes see the same canonical value, which is where this design promises parity.

---

## NULL

NULL has one representation: Commons' `TNullable` on the way in, Variant's `Null` from the leaf onward. No node carries a null flag of its own.

```pascal
var Age: TNullable<Integer> := TNullable<Integer>.Null;
Prop('Age') = TExprValue.From<Integer>(Age)      // → Variant Null
```

To test for NULL, use the predicate rather than a comparison — as in SQL, comparing against NULL is *unknown*, not true:

```pascal
Prop('Age').IsNull
Prop('Age').IsNotNull
```

Both back-ends follow ANSI three-valued logic, so `not (Prop('Age') > 18)` excludes a NULL age exactly as a database does.

---

## Parameters

A named placeholder carrying its bound value:

```pascal
Prm('minPrice', 100)
Prop('Price') > 100                 // equivalent for execution
```

A param leaf is a value leaf with a name. The name is for you — for logging, for building a rule once and reading it back — not a second binding mechanism.
