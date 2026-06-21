# MediatR VCL Sample

Demonstrates the four core MediatR concepts using a simple customer management form:

| Concept | What it shows |
|---------|--------------|
| **Command** | `TAddCustomerCommand`, `TRemoveCustomerCommand` — mutate state, no return value |
| **Query** | `TCustomerQuery` — read state with an optional filter predicate, returns `TCustomer.TList` |
| **Notification** | `TCustomerAddedEvent`, `TCustomerRemovedEvent` — broadcast to multiple handlers |
| **Pipeline Behavior** | `TLoggingBehavior`, `TValidationBehavior` — cross-cutting concerns, composable |

## File structure

| File | Role |
|------|------|
| `MediatRSample.AppServices.pas` | `IAppLog` / `TAppLog` — injects a log sink into behaviors and notification handlers |
| `MediatRSample.Customer.pas` | Domain model (`TCustomer`) and in-memory store (`ICustomerStore`) |
| `MediatRSample.Requests.pas` | All commands, queries, and notification types |
| `MediatRSample.Handlers.pas` | Command and query handlers |
| `MediatRSample.Notifications.pas` | Audit notification handlers — log only, no UI knowledge |
| `MediatRSample.Behaviors.pas` | `TLoggingBehavior` (outer) and `TValidationBehavior` (inner) |
| `MediatRSample.MainForm.pas` | VCL form + UI-refresh notification handlers |
| `MediatRSample.dpr` | DI wiring and application entry point |

## Pipeline flow

Every request goes through two behaviors before reaching its handler:

```
FMediator.Send(TAddCustomerCommand)
  └─ TLoggingBehavior.Handle          [Pipeline] >> TAddCustomerCommand
       └─ TValidationBehavior.Handle  (short-circuit if name is blank)
            └─ TAddCustomerCommandHandler.Handle
                 └─ Mediator.Publish(TCustomerAddedEvent)
                      ├─ TCustomerAddedAuditHandler   [Notification] CustomerAdded — #1 "Alice"
                      └─ TFormCustomerAddedHandler    MainForm.Reload  ← reactive UI update
       [Pipeline] << TAddCustomerCommand  (N ms)
```

The **Pipeline Log** panel in the form makes every step visible at runtime.

## Notification handlers — two separate concerns

`TCustomerAddedEvent` has two handlers:

| Handler | Unit | Responsibility |
|---------|------|----------------|
| `TCustomerAddedAuditHandler` | `Notifications.pas` | Logs to `IAppLog` — pure domain, no UI |
| `TFormCustomerAddedHandler` | `MainForm.pas` | Calls `MainForm.Reload` — UI refresh |

This demonstrates that a single notification can fan out to independent concerns.

## Validation behavior — short-circuit

`TValidationBehavior` inspects `TAddCustomerCommand` before calling `Next()`. If the name
is blank it logs the rejection and returns without ever reaching the handler:

```
[Pipeline] >> TAddCustomerCommand
[Validation] TAddCustomerCommand rejected — name is empty
[Pipeline] << TAddCustomerCommand  (0 ms)
```

No exception is raised. The outer `TLoggingBehavior` sees a normal return, not an error.

## UI walkthrough

1. **Add** — type a name, press Enter or click *Add*.
   The list refreshes reactively (via `TFormCustomerAddedHandler`), not by an explicit
   `LoadCustomers` call in the button handler.
2. **Remove** — select a row, click *Remove Selected*.
3. **Search** — type a substring in *Filter*, click *Search* (or clear with *All*).
   Each search dispatches a `TCustomerQuery` with a predicate — visible in the log.
4. **Pipeline Log** — every `Send` and `Publish` appears here with timing information.

## DI registration order

```pascal
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);   // outermost
MediatR.AddBehavior(ServiceCollection, TValidationBehavior); // inner

ServiceCollection.AddSingleton<IAppLog, TAppLog>;
ServiceCollection.AddSingleton<ICustomerStore, TCustomerStore>;
```

All handlers (command, query, notification) are discovered automatically via RTTI
from `_T.PackageOf<TMainForm>`. `{$STRONGLINKTYPES ON}` in the `.dpr` prevents the
linker from stripping unreferenced types.
