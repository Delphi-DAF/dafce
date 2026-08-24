#!/usr/bin/env python3
"""Package-level invariants, checked against the .dpk files.

Delphi will not catch any of these. `requires` is transitive, so a package
compiles as long as what it uses arrives through *someone*, whether or not it
said so — and it keeps compiling when it declares things it never uses. Both
directions drift silently, and both matter once the graph becomes a package
manifest.

    python3 eng/check-packages.py            # report and exit 1 on any breach
    python3 eng/check-packages.py --list     # print the graph as it stands
    python3 eng/check-packages.py --loose    # units that no package distributes

Invariants
----------
1. Commons is the floor. `Daf.Commons` requires `rtl` and nothing else. It is the
   one package almost everything depends on, so anything it drags in is dragged in
   by the whole framework — that is how a stray `dbrtl` ended up declared by
   nineteen packages.

2. Contracts do not depend on implementations. An `.Abstractions` package may
   require other `.Abstractions` packages and Commons, never a concrete one.
   Whoever implements a DAF contract should not be handed DAF's implementation of
   anything else.

3. Declared equals used. Every DAF package a `.dpk` requires is used by one of its
   own units, and every package its units use is required. Unused entries are
   allowed only through INTENTIONAL below, with the reason written down — because a
   `requires` also drags the required package's `initialization`, and sometimes that
   is exactly the point.

4. Some stacks are out of bounds. FORBIDDEN below names them by unit prefix, and
   names who the rule is about: either the handful of packages it applies to, or
   everyone except the handful licensed to link it. This is the one invariant here
   that no behaviour test could ever stand in for: `Daf.Web` served its routes and
   bound its port perfectly well while dragging DataSnap and DBX behind it, because
   what was wrong was the dependency, and tests are blind to dependencies by
   construction.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import NamedTuple

ROOT = Path(__file__).resolve().parents[1]

# Requires that are deliberately not used by any unit. Keyed by (package, required),
# with the reason: removing one of these breaks something no compiler will report.
INTENTIONAL = {
    ("Daf.SQLCute.InMemory", "Daf.SQLCute"):
        "Daf.SQLCute.Core's initialization registers the factory TQuery.New delegates to, "
        "and no other required package brings it into the closure",
}

# The floor. Kept as a set so the message can name what appeared.
FLOOR = {"Daf.Commons": {"rtl"}}


class Forbidden(NamedTuple):
    """A stack that must not be linked, what would bring it, and who the rule is about.

    Scope goes one of two ways, and exactly one of `only` / `everyone_but` is set:

    `only` names the packages the rule applies to. That is the right shape when the
    rule is about them and not about DAF in general: what is intolerable in a
    lightweight HTTP server is ordinary in a data module.

    `everyone_but` names the packages licensed to link it, and the rule covers all
    the rest. That is the right shape when nobody should link a stack except the one
    package built around it — and it is not the same as enumerating the other twenty,
    because the enumeration says nothing about the twenty-first. Adding a package and
    forgetting to list it there would not fail, it would *permit*, which is the worst
    way for an invariant to break: silently, and in the direction of allowing.

    A stack arrives by two routes and the rule checks both. `prefixes` catches the
    implicit import — a unit named in a uses clause — which is how the DataSnap one
    got in. `packages` catches the declared route: a `requires` entry links the stack
    just as hard, and would sail past a check that only reads uses clauses. Leave
    `packages` empty and only the import route is checked.
    """

    prefixes: tuple[str, ...]
    reason: str
    only: frozenset[str] = frozenset()
    everyone_but: frozenset[str] = frozenset()
    packages: frozenset[str] = frozenset()

    def covers(self, pkg: str) -> bool:
        return pkg in self.only if self.only else pkg not in self.everyone_but


# Unlike the spec-vs-code check, this one is a gate rather than a report. "Does this
# package link DataSnap" has no second reading that could make the answer acceptable.
FORBIDDEN = (
    Forbidden(
        prefixes=("datasnap.", "data.dbx"),
        only=frozenset({"Daf.Web"}),
        reason="a module whose purpose is to serve HTTP has no business carrying session "
               "managers, data transports, metadata readers or an SQL scanner into a "
               "consumer's application",
    ),
    Forbidden(
        prefixes=("fmx.",),
        packages=frozenset({"fmx"}),
        everyone_but=frozenset({"Daf.FMX"}),
        reason="FireMonkey is what Daf.FMX exists to isolate, and a VCL application has no "
               "reason to carry it for using DAF's logging. The contract package is the "
               "extreme case, since everyone uses it, but the rule is not its own",
    ),
    # Not a dependency-isolation boundary: both packages require only rtl and Daf.Commons, so
    # neither drags anything the other lacks. The boundary is the guardrail itself, and `only`
    # rather than `everyone_but` because the rule is about this one package -- Daf.Linq is public,
    # and the back-end that runs a query over loaded collections requires it by design.
    Forbidden(
        prefixes=("daf.linq",),
        packages=frozenset({"daf.linq"}),
        only=frozenset({"Daf.Specification"}),
        reason="a closure never translates, and Daf.Specification bounds its predicate tree on "
               "purpose so that what cannot be translated is unrepresentable. Linked together, a "
               "closure reaches where a tree is expected and nothing fails: it is evaluated in "
               "memory, over a different row set than the caller asked for. That is the silent "
               "fallback EF Core withdrew in 3.0, so this is a boundary and not a preference -- "
               "remove the dependency, not the rule",
    ),
)


def parse():
    """Returns (requires, contains, owner) read from every src/*/*.dpk."""
    requires, contains, owner = {}, {}, {}
    for dpk in sorted(ROOT.glob("src/*/*.dpk")):
        pkg = dpk.stem
        text = dpk.read_text(encoding="utf-8-sig")

        block = re.search(r"^requires(.*?);", text, re.S | re.M)
        body = re.sub(r"//[^\n]*", "", block.group(1)) if block else ""
        requires[pkg] = [x.strip() for x in body.replace("\n", " ").split(",") if x.strip()]

        block = re.search(r"^contains(.*?);\s*$", text, re.S | re.M)
        units = re.findall(r"([\w.]+)\s+in\s+'", block.group(1)) if block else []
        contains[pkg] = units
        for unit in units:
            owner[unit.lower()] = pkg

    if not requires:
        sys.exit("no .dpk found under src/*/ — is the repo root right?")
    return requires, contains, owner


def is_daf(name: str) -> bool:
    return name.lower().startswith("daf")


def imported_units(pkg, contains):
    """Every unit this package's own units name in a uses clause, as (by, imported)
    pairs. Both sections: an implementation-only use is still a link."""
    pairs = []
    for unit in contains[pkg]:
        # Recursive on purpose: NNLog keeps its targets in src/NNLog/Targets/, and a
        # one-level glob skipped four units in silence — a checker with blind spots is
        # worse than none, because it reports success over what it never read.
        hits = list((ROOT / "src").rglob(f"{unit}.pas"))
        if not hits:
            # Never skip in silence: a unit we cannot read makes the verdict for this
            # package a guess, and the guess would read as a pass.
            print(f"warning: {pkg} contains {unit}, whose source was not found",
                  file=sys.stderr)
            continue
        text = hits[0].read_text(encoding="utf-8-sig")
        for clause in re.finditer(r"\buses\b(.*?);", text, re.S):
            for token in re.findall(r"[\w.]+", clause.group(1)):
                pairs.append((unit, token))
    return pairs


def direct_uses(pkg, imports, owner):
    """The packages this one's own units name in a uses clause."""
    used = set()
    for _, token in imports:
        holder = owner.get(token.lower())
        if holder and holder != pkg:
            used.add(holder)
    return used


def loose_units(contains):
    """Framework units that no .dpk contains, so no package distributes them.

    A loose unit is not a build failure: it compiles the day somebody names it by
    path, and it is absent from the build otherwise -- it does not break, it
    *disappears*. Since a package is the unit of installation, a source file that
    belongs to no package is a file nobody can install. Tests and samples are out of
    scope: they are compiled by their own projects, not distributed.

    The parse this needs is the same one the invariants above already do, which is why
    it lives here rather than as a shell one-liner somebody keeps in a notebook.
    """
    packaged = {u.lower() for units in contains.values() for u in units}
    loose = []
    for pas in sorted((ROOT / "src").rglob("*.pas")):
        parts = {p.lower() for p in pas.relative_to(ROOT / "src").parts[:-1]}
        if parts & {"test", "tests", "samples"}:
            continue
        if pas.stem.lower() not in packaged:
            loose.append(pas.relative_to(ROOT).as_posix())
    return len(packaged), loose


def main():
    requires, contains, owner = parse()
    canon = {p.lower(): p for p in requires}
    fold = lambda name: canon.get(name.lower(), name)

    if "--loose" in sys.argv:
        packaged, loose = loose_units(contains)
        if loose:
            print(f"{len(loose)} unit(s) in no .dpk:", file=sys.stderr)
            for u in loose:
                print(f"  - {u}", file=sys.stderr)
            return 1
        print(f"every framework unit is packaged ({packaged} units across {len(requires)} packages)")
        return 0

    if "--list" in sys.argv:
        for pkg in sorted(requires):
            deps = ", ".join(sorted(fold(r) for r in requires[pkg])) or "—"
            print(f"{pkg:<36} {deps}")
        return 0

    breaches = []

    for pkg, allowed in FLOOR.items():
        if pkg not in requires:
            continue
        extra = sorted(set(requires[pkg]) - allowed)
        if extra:
            breaches.append(
                f"{pkg} is the floor and must require only {', '.join(sorted(allowed))}. "
                f"It also requires: {', '.join(extra)}.")

    for pkg in sorted(requires):
        if not pkg.endswith(".Abstractions"):
            continue
        concrete = sorted(
            fold(r) for r in requires[pkg]
            if is_daf(r) and not fold(r).endswith(".Abstractions") and fold(r) != "Daf.Commons")
        if concrete:
            breaches.append(
                f"{pkg} is a contract package and requires the implementation "
                f"{', '.join(concrete)}. Contracts may require other contracts and Commons.")

    for pkg in sorted(requires):
        imports = imported_units(pkg, contains)

        for rule in FORBIDDEN:
            if not rule.covers(pkg):
                continue
            for required in requires[pkg]:
                if required.lower() in rule.packages:
                    breaches.append(
                        f"{pkg} requires {required}: {rule.reason}.")
            for unit, imported in imports:
                if imported.lower().startswith(rule.prefixes):
                    breaches.append(
                        f"{pkg} links {imported}, brought in by {unit}: {rule.reason}.")

        declared = {fold(r) for r in requires[pkg] if is_daf(r)}
        used = direct_uses(pkg, imports, owner)
        for dep in sorted(declared - used):
            if (pkg, dep) not in INTENTIONAL:
                breaches.append(
                    f"{pkg} requires {dep} and no unit of it uses it. Remove it, or add it to "
                    f"INTENTIONAL in this script with the reason.")
        for dep in sorted(used - declared):
            breaches.append(
                f"{pkg} uses {dep} without requiring it — it arrives transitively today, so it "
                f"compiles, and breaks the day that path changes. Declare it.")

    if breaches:
        print(f"{len(breaches)} package invariant breach(es):\n", file=sys.stderr)
        for breach in breaches:
            print(f"  - {breach}", file=sys.stderr)
        return 1

    print(f"package invariants hold across {len(requires)} packages")
    return 0


if __name__ == "__main__":
    sys.exit(main())
