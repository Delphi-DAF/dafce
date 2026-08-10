# The optional metamodel

**🌍 Language: English | [Español](metamodel.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Declaring one

A metamodel is a hand-written companion class exposing an entity's members as expression leaves:

```pascal
ProductMeta = class
public
  class var Price:  TPropExpr<TProduct>;
  class var Active: TPropExpr<TProduct>;
  class constructor Create;
end;

class constructor ProductMeta.Create;
begin
  Price  := TPropExpr<TProduct>.Create('Price');     // the PROPERTY name
  Active := TPropExpr<TProduct>.Create('Active');
end;
```

Then author against it:

```pascal
ProductMeta.Price > 100
ProductMeta.Active = True
ProductMeta.Price.Between(10, 200)
```

`class var` plus a `class constructor` rather than class functions: the call site is a field read, and initialization runs once per class. The entity itself is untouched — the metamodel is a separate type, so no persistence strings leak into the POCO.

---

## What it buys, precisely

| Gain | Real? |
|------|-------|
| Typo-safety | **Yes** — `ProductMeta.Pric` does not compile |
| Discoverability | **Yes** — code completion lists the members |
| Rename-safety | **No** — the field holds a string, so renaming `Price` still compiles |

Being honest about the third matters, because it is the one people assume they are buying. What converts a stale name from a late failure into an early loud one is **self-validation**: the leaf is generic over the entity, so it checks the name when the class initializes — which is the first time a test touches it.

```pascal
class constructor StaleMeta.Create;
begin
  Price := TPropExpr<TProduct>.Create('Pricee');   // raises EExprMemberError here
end;
```

Note the consequence of `class constructor`: it runs during unit initialization, so a stale metamodel fails at startup rather than mid-query. That is the intent. If you want to trigger it deliberately in a test, use a `class procedure` initializer instead — a raising class constructor would take the whole runner down before any scenario ran.

---

## It stays an option

This is the part that keeps the metamodel from becoming a tax:

- **The runtime path is always sufficient.** `TEntityExpr<T>.Prop` reaches every capability, and no feature of the model, of specifications, or of the repository requires a metamodel.
- **Both paths produce identical nodes.** A metamodel leaf comes from the same constructor the runtime path calls. Nothing downstream — emitter, specification, repository — can distinguish them.
- **The two forms mix freely.**

```pascal
var Runtime := TEntityExpr<TProduct>.Create(nil);
(ProductMeta.Price > 100) and (Runtime.Prop('Active') = True)      // composes normally
```

So adopting a metamodel for a hot entity, or dropping one later, is a local edit rather than a migration. If any capability were metamodel-only, the option would have become a requirement.

---

## When it is worth it

Roughly: for entities queried from many places, where a rename or a typo would otherwise be found by a failing integration test rather than by the compiler. For an entity queried once from one service, `Prop('Price')` inside a `TEntityExpr<T>` already validates the name, and the metamodel adds a file to keep in sync for little in return.

Generation is out of scope. If it ever arrives, its real payoff is not the names — it is attaching a typed accessor to the leaf, which the node already reserves room for.
