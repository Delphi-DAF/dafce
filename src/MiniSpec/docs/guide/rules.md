# Rules: Grouping by Business Rule

**🌍 Language: English | [Español](rules.es.md)**

[← Back to Guide](../GUIDE.md)

---

When a Feature has multiple business rules, **Rules** help organize the scenarios that illustrate each rule:

```pascal
Feature('Discount System')
.UseWorld<TDiscountWorld>

.Rule('VIP customers get 20% discount')
  .Scenario('Normal purchase by VIP customer')
    .Given('a VIP customer', procedure(W: TDiscountWorld)
      begin
        W.Customer := TCustomer.Create(TCustomerType.VIP);
      end)
    .When('buying a $100 product', procedure(W: TDiscountWorld)
      begin
        W.Purchase := W.Customer.Buy(100);
      end)
    .&Then('the total is $80', procedure(W: TDiscountWorld)
      begin
        Expect(W.Purchase.Total).ToEqual(80);
      end)

.Rule('Purchases over $100 have free shipping')
  .Scenario('Purchase of $150')
    // ...
  .Scenario('Purchase of $50')
    // ...
```

## Rule Behavior

| Aspect | Description |
|--------|-------------|
| Grouping | Scenarios under a Rule share that business rule |
| Non-nestable | Each `.Rule()` automatically closes the previous one |
| Background | The Feature's Background applies to all scenarios, including those in Rules |
| Filtering | Use `Rule:text` to filter by rule description |

## EndRule: Scenarios without Rule

`.EndRule` is only needed when you want to add scenarios **without Rule** after a Rule:

```pascal
Feature('...')
  .Rule('A specific rule')
    .Scenario('Test inside the Rule')
    
  .EndRule  // Needed to exit the Rule
  
  .Scenario('General scenario without Rule')
```

---

[← Assertions](assertions.md) | [Next: Gherkin Vocabulary →](gherkin-vocabulary.md)
