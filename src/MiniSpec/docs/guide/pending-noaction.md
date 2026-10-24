# Pending and NoAction

**🌍 Language: English | [Español](pending-noaction.es.md)**

[← Back to Guide](../GUIDE.md)

---

MiniSpec provides two ways to mark steps that have no executable code:

| Method | Semantics | Result | Typical use |
|--------|-----------|--------|-------------|
| `.Pending` | "Pending implementation" | Pending + stops scenario | Work in progress |
| `.NoAction` | "No action required" | Pass + continues scenario | Purely descriptive steps |

## Pending

Indicates a step is pending implementation. The scenario counts as Pending and **following steps are not executed**:

```pascal
.Scenario('Feature in development')
  .Given('a precondition', procedure(Ctx: TMyWorld) begin end)
  .When('something pending implementation')
    .Pending                          // Scenario Pending, doesn't continue
  .&Then('never executes', procedure(Ctx: TMyWorld)
    begin
      // This code does not execute
    end)
```

A step of the Background works the same way: a `.Pending` step, or one with no binding, leaves the
rest of the Background and the steps of the scenario unrun, and the scenario counts as Pending or
Undefined, as if the step were its own.

## NoAction

Marks a step as purely descriptive. It passes automatically and **the following steps DO execute**:

```pascal
.Scenario('With a descriptive context')
  .Given('the system is configured')  // the environment already provides it
    .NoAction
  .When('user logs in', procedure(Ctx: TMyWorld)
    begin
      Ctx.DoLogin;  // this code DOES execute
    end)
  .&Then('accesses dashboard', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.IsLoggedIn).ToBeTrue;  // also executes
    end)
```

## The last resort

A scenario states the behaviour of **one event**, split across the three phases: `Given` the state
the event needs, `When` the event, `Then` what can be observed afterwards. A step with no body says
that piece is missing, and the scenario stops saying what it validates.

It is not forbidden — the marker exists for a reason — but **its legitimate use is rare, and rarest
of all on a `When` and on a `Then`**, so it is justified when used rather than taken for granted.

**The rule that decides: a step with nothing to *run* almost always has something to *check*.** That
is its form as a check, and it is what leaves `.NoAction` for the genuine remainder.

| Phase | What to do when there is nothing to run |
|---|---|
| `Given` | assert that the environment is what you assume |
| `When` | for an **implicit action**, wait until it has happened and assert that it did |
| `Then` | nothing fits here: a scenario that observes nothing asserts nothing |

So before writing it, two questions: **is there anything to run here?** and **is there anything to
check here?** `.NoAction` is the answer when both are no.

### The implicit action

Some events are produced by the system rather than by the scenario: a background task that runs on
its own, a timer, something that already happened while a unit was initialising. The `When` names
the event even though the test did not cause it.

```pascal
.When('implicit action').NoAction
```

**Two questions before accepting it**, because most steps that look implicit are not:

1. **Can I trigger it?** Then trigger it in the `When`, and this is not the case.
2. **Can I wait for it?** Then the wait is the step's body:

```pascal
.When('the background task has run',
  procedure(W: TMyWorld)
  begin
    Expect(W.WaitForTask(5000)).ToBeTrue;
  end)
```

⚠️ **A `When` that waits removes the race.** Without it the `Then` may run before the task finishes,
and the scenario then passes or fails depending on how loaded the machine is — which is flakiness,
not a matter of form.

⚠️ **Everywhere else, a `When` with no action still prints `✓`**, so the report declares an event
that never happened, which reads as covered. If the scenario has no event at all, it has no
behaviour under test: **omitting the `When` instead of disabling it is the same situation with a
different label.** Where the event usually turns up is in the `Given`, which ran it too early, or
inside the `Then`, which runs it while asserting.

⚠️ **Nothing stops you from putting it on a `Then`, and nothing warns you.** The step becomes a body
that does nothing and passes, so the scenario keeps a `Then` that verifies nothing and stays green —
worse than a failure, because it looks like coverage.

```pascal
.&Then('verification').NoAction  // compiles, runs, verifies nothing
```

## The `Given` that has nothing to build

The legitimate case is real but narrow: the environment already holds the state, so there is nothing
to set up. **Even there, checking beats doing nothing** — assert that the environment is what you
assume:

```pascal
.Given('the system is configured',
  procedure(W: TMyWorld)
  begin
    Expect(W.Environment.IsReady).ToBeTrue;
  end)
```

A `Given` that asserts its starting point turns an environment failure into a red saying *the
environment was not what you assumed*, instead of a failing `Then` that sends you looking in the
wrong place.

## Where it lands

`.NoAction` replaces the body of the **last declared step**, whatever its kind:

```pascal
.Given('descriptive context').NoAction
```

---

[← Gherkin Vocabulary](gherkin-vocabulary.md) | [Next: Hooks →](hooks.md)
