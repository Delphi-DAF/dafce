# MediatR VCL Sample

Demonstrates the four core MediatR concepts using a simple customer management form:

| Concept | What it shows |
|---------|--------------|
| **Command** | `TAddCustomerCommand`, `TRemoveCustomerCommand` — mutate state, no return value |
| **Query** | `TCustomerQuery` — read state with an optional filter predicate, returns `TCustomer.TList` |
| **Notification** | `TCustomerAddedEvent`, `TCustomerRemovedEvent` — broadcast to multiple handlers |
| **Pipeline Behavior** | `TLoggingBehavior`, `TValidationBehavior`, `TQueryResultBehavior` — one of each flavor: global, typed void, typed response |

## File structure

| File | Role |
|------|------|
| `MediatRSample.AppServices.pas` | `IAppLog` / `TAppLog` — injects a log sink into behaviors and notification handlers |
| `MediatRSample.Customer.pas` | Domain model (`TCustomer`) and in-memory store (`ICustomerStore`) |
| `MediatRSample.Requests.pas` | All commands, queries, and notification types |
| `MediatRSample.Handlers.pas` | Command and query handlers |
| `MediatRSample.Notifications.pas` | Audit notification handlers — log only, no UI knowledge |
| `MediatRSample.Behaviors.pas` | `TLoggingBehavior` (global), `TValidationBehavior` (typed void) and `TQueryResultBehavior` (typed response) |
| `MediatRSample.MainForm.pas` | VCL form + UI-refresh notification handlers |
| `MediatRSample.dpr` | DI wiring and application entry point |

## Pipeline flow

Only `TLoggingBehavior` sees every request: it is the global flavor. The other two are typed, and
the framework routes each one to its own request type — `TValidationBehavior` to
`TAddCustomerCommand`, `TQueryResultBehavior` to `TCustomerQuery`. Adding a customer therefore goes
through two:

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

`TValidationBehavior` inspects `TAddCustomerCommand` before calling `Next.Call`. If the name
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
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);    // outermost
MediatR.AddBehavior(ServiceCollection, TValidationBehavior);
MediatR.AddBehavior(ServiceCollection, TQueryResultBehavior); // innermost

ServiceCollection.AddSingleton<IAppLog, TAppLog>;
ServiceCollection.AddSingleton<ICustomerStore, TCustomerStore>;
```

Behaviors are registered one by one: `MediatR.AddTo(ServiceCollection, Package)` discovers
handlers and not behaviors, and the scan for those is `ServiceCollection.AddMediatRBehaviors(Package)`.
All handlers (command, query, notification) are discovered automatically via RTTI
from `_T.PackageOf<TMainForm>`. `{$STRONGLINKTYPES ON}` in the `.dpr` prevents the
linker from stripping unreferenced types.
