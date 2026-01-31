# Rules: Agrupando por Regla de Negocio

**🌍 Idioma: [English](rules.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Cuando una Feature tiene múltiples reglas de negocio, las **Rules** ayudan a organizar los escenarios que ilustran cada regla:

```pascal
Feature('Sistema de Descuentos')
.UseWorld<TDiscountWorld>

.Rule('Los clientes VIP tienen 20% de descuento')
  .Scenario('Compra normal de cliente VIP')
    .Given('un cliente VIP', procedure(W: TDiscountWorld)
      begin
        W.Customer := TCustomer.Create(TCustomerType.VIP);
      end)
    .When('compra un producto de $100', procedure(W: TDiscountWorld)
      begin
        W.Purchase := W.Customer.Buy(100);
      end)
    .&Then('el total es $80', procedure(W: TDiscountWorld)
      begin
        Expect(W.Purchase.Total).ToEqual(80);
      end)

.Rule('Las compras mayores a $100 tienen envío gratis')
  .Scenario('Compra de $150')
    // ...
  .Scenario('Compra de $50')
    // ...
```

## Comportamiento de Rules

| Aspecto | Descripción |
|---------|-------------|
| Agrupación | Los escenarios bajo una Rule comparten esa regla de negocio |
| No anidables | Cada `.Rule()` cierra la anterior automáticamente |
| Background | El Background de la Feature aplica a todos los escenarios, incluidos los de Rules |
| Filtrado | Usa `Rule:texto` para filtrar por descripción de rule |

## EndRule: Escenarios sin Rule

`.EndRule` solo es necesario cuando quieres añadir escenarios **sin Rule** después de una Rule:

```pascal
Feature('...')
  .Rule('Una regla específica')
    .Scenario('Test dentro de la Rule')
    
  .EndRule  // Necesario para salir de la Rule
  
  .Scenario('Escenario general sin Rule')
```

---

[← Assertions](assertions.es.md) | [Siguiente: Vocabulario Gherkin →](gherkin-vocabulary.es.md)
