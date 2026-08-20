#!/usr/bin/env python3
"""Package-level invariants, checked against the .dpk files.

Delphi will not catch any of these. `requires` is transitive, so a package
compiles as long as what it uses arrives through *someone*, whether or not it
said so — and it keeps compiling when it declares things it never uses. Both
directions drift silently, and both matter once the graph becomes a package
manifest.

    python3 eng/check-packages.py            # report and exit 1 on any breach
    python3 eng/check-packages.py --list     # print the graph as it stands

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

4. Some stacks are out of bounds for some packages. FORBIDDEN below names them by
   unit prefix, per package. This is the one invariant here that no behaviour test
   could ever stand in for: `Daf.Web` served its routes and bound its port perfectly
   well while dragging DataSnap and DBX behind it, because what was wrong was the
   dependency, and tests are blind to dependencies by construction.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

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

# Stacks a package must not link, by the prefix of the unit names that would bring
# them. Keyed by package, because this is not a rule about DAF in general: what is
# intolerable in a lightweight HTTP server is ordinary in a data module.
#
# Unlike the spec-vs-code check, this one is a gate rather than a report. "Does this
# package link DataSnap" has no second reading that could make the answer acceptable.
FORBIDDEN = {
    "Daf.Web": (
        ("datasnap.", "data.dbx"),
        "a module whose purpose is to serve HTTP has no business carrying session "
        "managers, data transports, metadata readers or an SQL scanner into a "
        "consumer's application",
    ),
}


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


def main():
    requires, contains, owner = parse()
    canon = {p.lower(): p for p in requires}
    fold = lambda name: canon.get(name.lower(), name)

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

        if pkg in FORBIDDEN:
            prefixes, reason = FORBIDDEN[pkg]
            for unit, imported in imports:
                if imported.lower().startswith(prefixes):
                    breaches.append(
                        f"{pkg} links {imported}, brought in by {unit}: {reason}.")

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
