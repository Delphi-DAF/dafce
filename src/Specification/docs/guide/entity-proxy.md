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

Not a column name.

- A column name is persistence vocabulary, and this package stays free of SQL.

- The property name is the right currency for the in-memory lane, which needs no mapping at all. Only SQL needs the column, and it gets it at emit time, in one place, once.

---

## One strategy, two questions

`INameResolver` answers both naming questions, because they are the same kind of question:

```pascal
INameResolver = interface
  function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
  function TableFor(EntityType: PTypeInfo): string;
end;
```

The defaults are deliberately minimal:

| Question | Default |
|----------|---------|
| Which column for this member? | The member name, unchanged |
| Which table for this type? | The type name with a leading `T` removed |

The `T` is stripped only when an upper-case letter follows, so `TProduct` → `Product` while `Thing` is left alone. No pluralization and no case conversion.

Supplying your own is one class:

```pascal
TMappedResolver = class(TInterfacedObject, INameResolver)
public
  function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
  function TableFor(EntityType: PTypeInfo): string;
end;

function TMappedResolver.ColumnFor(EntityType: PTypeInfo; const Member: string): string;
begin
  if SameText(Member, 'Price') then Result := 'price_eur' else Result := Member;
end;

function TMappedResolver.TableFor(EntityType: PTypeInfo): string;
begin
  Result := 'products';
end;
```

Then either ask the proxy:

```pascal
var P := TEntityExpr<TProduct>.Create(TMappedResolver.Create);
P.Column('Price');    // 'price_eur'
P.Table;              // 'products'
```

…or hand the strategy to the emission, which is where a column name is actually needed:

```pascal
TExprEmitter.Where(Q, MyExpr, TMappedResolver.Create, TypeInfo(TProduct));
TExprEmitter.Apply<TProduct>(Q, Spec, TMappedResolver.Create);
```

---

## What the strategy is not

It is a seam for names, not a mapping model: keys, lock columns, attributes, inheritance mapping and instance-driven DML are absent.


---

## The reserved accessor

`IMemberExpr.Accessor` exists and is always `nil` today.

Both authoring paths leave it `nil`.
