# The entity proxy and name resolution

**🌍 Language: English | [Español](entity-proxy.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Why a proxy at all

`Prop('Price')` names a member and asks nothing about it. That is fine for a rule written next to the entity, but it means a typo travels: the SQL back-end will happily send an unmapped column to the server, while the in-memory lane cannot resolve it. The failure then surfaces far from its cause, and on only one back-end.

`TEntityExpr<T>` closes that gap by validating the member against the entity's RTTI where the expression is authored:

```pascal
var P := TEntityExpr<TProduct>.Create(nil);

P.Prop('Price') > 100         // fine
P.Prop('price') > 100         // fine too — resolves to 'Price', as RTTI spells it
P.Prop('Pricee') > 100        // EExprMemberError: names the member and the type
```

Properties win over fields, matching the in-memory lane's own resolution order. A record works as well as a class — nothing here needs a base class.

---

## The leaf carries a property name

Not a column name. Three independent reasons:

- A column name is persistence vocabulary. A leaf holding one would be persistence-aware, and could not live in a package that must stay free of SQL.
- It would declare the mapping twice — once wherever the ORM describes it, once here — with no compiler link between the copies. Drift is then only a matter of time, and it drifts *silently*.
- The property name is the right currency for the in-memory lane, which needs no mapping at all. Only SQL needs the column, and it gets it at emit time, in one place, once.

---

## One strategy, two questions

`INameResolution` answers both naming questions, because they are the same kind of question:

```pascal
INameResolution = interface
  function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
  function TableFor(EntityType: PTypeInfo): string;
end;
```

The defaults are deliberately minimal:

| Question | Default |
|----------|---------|
| Which column for this member? | The member name, unchanged |
| Which table for this type? | The type name with a leading `T` removed |

The `T` is stripped only when an upper-case letter follows, so `TProduct` → `Product` while `Thing` is left alone. No pluralization and no case conversion: those are naming *policy*, and guessing them would collide with whatever the ORM decides while looking like a feature.

Supplying your own is one class:

```pascal
TMappedNames = class(TInterfacedObject, INameResolution)
public
  function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
  function TableFor(EntityType: PTypeInfo): string;
end;

function TMappedNames.ColumnFor(EntityType: PTypeInfo; const Member: string): string;
begin
  if SameText(Member, 'Price') then Result := 'price_eur' else Result := Member;
end;

function TMappedNames.TableFor(EntityType: PTypeInfo): string;
begin
  Result := 'products';
end;
```

Then either ask the proxy:

```pascal
var P := TEntityExpr<TProduct>.Create(TMappedNames.Create);
P.Column('Price');    // 'price_eur'
P.Table;              // 'products'
```

…or hand the strategy to the emission, which is where a column name is actually needed:

```pascal
TExprEmitter.Where(Q, MyExpr, TMappedNames.Create, TypeInfo(TProduct));
TExprEmitter.Apply<TProduct>(Q, Spec, TMappedNames.Create);
```

---

## What the strategy is not

It is a seam for names, not a mapping model. Keys, lock columns, attributes, inheritance mapping and instance-driven DML are all ORM metadata, and the ORM is not designed yet. Baking a mapping model in here would prejudge that design from the wrong layer.

So this layer commits to the seam: whatever the ORM turns out to be, it supplies its own `INameResolution` and nothing here changes.

---

## The reserved accessor

`IMemberExpr.Accessor` exists and is always `nil` today. The room is reserved because the real payoff of a generated metamodel would not be the names but attaching a pre-resolved accessor here, letting the in-memory lane skip the name normalization, dictionary lookup and boxing it performs per row and per column. Reserving a field now means that can arrive without reshaping the node.

Both authoring paths leave it `nil`, deliberately — if one populated it and the other did not, it would become the thing that tells a metamodel leaf apart from a runtime one.
