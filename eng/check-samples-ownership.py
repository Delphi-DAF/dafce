#!/usr/bin/env python3
"""Check that every sample lives under the module that declares what the sample uses.

WHY THIS EXISTS
---------------
A sample gets filed under the module its author had in mind, not under the one that declares
what the sample consumes -- and nothing in the build can say so. `samples.optset` puts
`$(DAF_FW_LIB_DEBUG)` on the unit search path, which is the WHOLE framework, so a
misfiled sample compiles exactly as well as a correctly filed one. There is no compiler
error to wait for, no failing suite, no link error: this check is the only signal that
can exist.

And a misfiled sample does not just sit there being untidy. It counts as an absence in
the module that should own it and as a duplicate in the module that does, so every
coverage count over samples is wrong until the filing is right. Measured 2026-09-07:
"VCL has no sample" was false -- it is `TicTacToeApp`, filed under Application.

THE RULE, AND WHY IT IS DERIVED
-------------------------------
A sample belongs to the LOWEST module in the graph whose package declares, through its
transitive `requires`, every package the sample consumes units from.

The ordering between modules is not enumerated here and never should be: each `.dpk`
already declares it, and `docs/_gen/module-graph.py` draws it. So a module added
tomorrow brings its answer with it, the same way `check-edition-parity.py` derives its
scope and the commit scopes are derived from the directory names.

TWO EXEMPTIONS, BOTH FORCED BY THE MEASUREMENT
----------------------------------------------
1. A sample may use the IMPLEMENTATION of an abstraction its module requires.
   `Daf.MediatR` requires `Daf.DependencyInjection.Abstractions`, and `MediatRSample`
   uses `Daf.DependencyInjection` -- because a sample builds a real container. That is
   what a sample IS. Without this exemption every sample that composes services is a
   finding, which would make the check about nothing.

2. A sample's own spec runner may use `Daf.MiniSpec`. `AGENTS.md` already blesses the
   pattern: a sample may carry a `Specs/` subfolder and its own runner when its purpose
   is to demonstrate BDD in a real application. A runner is recognised by carrying at
   least one `*.Feat` unit of its own -- that is what makes a program a spec runner
   rather than a program that happens to link MiniSpec.

WHY THE UNIT OF MEASURE IS THE PROJECT AND NOT THE DIRECTORY
------------------------------------------------------------
`TicTacToeApp/` holds TWO programs -- the app and its spec runner -- so a sweep that
reads every `.pas` under the directory blames the app for the runner's `Daf.MiniSpec`
and the runner for the app's `Daf.VCL`. Each `.dpr` is followed through the units it
names, transitively, which is exactly the set the compiler links.

WHY THE PARSER STRIPS COMMENTS FIRST, AND WITH A SCANNER
--------------------------------------------------------
The `contains` clause of `Daf.Commons` carries a comment with dotted names inside it.
A tokenizer that does not remove comments maps those names to `Daf.Commons`, and
`System.SysUtils` ends up owned by a DAF package: the report then gives a number that
is credible and wrong -- the first probe of this rule said 8 findings for that reason.

Layered regexes do not settle it either, because which comment opens first decides what
the other one means: a `{` inside a `//` line is not a block comment, and a `//` inside
a `{ }` block is not a line comment. So this walks the text once, left to right, the way
the compiler does, and skips string literals on the way -- a `.dpk` quotes file paths.

A SAMPLE OF THE EDITION, AND WHY IT IS CHECKED THE OTHER WAY ROUND
------------------------------------------------------------------
`src/samples/<Name>/` is for a sample that combines modules with NO module above all of
them: `HostedService` uses `Daf.Cli` and `Daf.NNLog`, and nothing declares both, so
filing it under either is arbitrary. It has no host package, so the rule above has
nothing to compare against.

The rule there is the inverse, and it has a reachable zero from day one: a sample of the
edition is misfiled if some single module DOES cover everything it uses, because then it
belongs to that module. Without this half, `src/samples/` would be where a misfiled
sample goes to stop being reported.

A GATE, AND IT WAS A REPORT FOR EXACTLY ONE SWEEP
------------------------------------------------
The rule found 5 misfiled samples the first time it ran, and a gate born red is
wallpaper -- so this was written as a report, with the promotion rule "it becomes a gate
the day a sweep leaves it at zero", the same regime `argumenta` is under.

The sweep that moved those five left it at **zero in both editions**, so the promotion
happened in the same change rather than being carried as a pending item whose condition
had already fired. Both halves of the zero are reachable and both are defended here: no
module sample uses a package its module does not declare, and no edition sample is one
that a single module would cover. Exit code is 1 on any finding.

ZERO SAMPLES SWEPT IS A FAILURE
-------------------------------
Rename the samples directory and a check that only reports what it finds prints "no
findings" -- success and total blindness with the same output. Finding no `.dpr` under
either `src/*/samples/` or `src/samples/` exits 2.

Usage:
    python3 eng/check-samples-ownership.py                    # gate: rc=1 on any finding,
                                                              # rc=2 if it swept nothing
    python3 eng/check-samples-ownership.py --verbose          # plus the packages each sample uses
    python3 eng/check-samples-ownership.py --probe <sample> <module>
        Re-judge one sample as if it lived under <module>, without touching the tree.
        This is the negative half of the control: a check that cannot be made to go red
        by a case it should catch is not measuring anything. E.g.
            --probe Cli/samples/GreetTool CommandLine    -> must report a finding
            --probe Application/samples/QuickStart Cli   -> must come back clean
"""

from __future__ import annotations

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
SRC = ROOT / "src"


def strip_comments(text: str) -> str:
    """Pascal comments out, string literals preserved as-is, in one left-to-right pass.

    Whitespace replaces what is removed so that nothing downstream can join two tokens
    that were never adjacent.
    """
    out, i, n = [], 0, len(text)
    while i < n:
        c = text[i]
        if c == "'":
            j = i + 1
            while j < n and text[j] != "'":
                j += 1
            out.append(text[i:j + 1])
            i = j + 1
        elif text.startswith("//", i):
            j = text.find("\n", i)
            j = n if j < 0 else j
            out.append(" " * (j - i))
            i = j
        elif c == "{":
            j = text.find("}", i)
            j = n if j < 0 else j + 1
            out.append(" " * (j - i))
            i = j
        elif text.startswith("(*", i):
            j = text.find("*)", i)
            j = n if j < 0 else j + 2
            out.append(" " * (j - i))
            i = j
        else:
            out.append(c)
            i += 1
    return "".join(out)


def read(path: pathlib.Path) -> str:
    return path.read_bytes().decode("utf-8-sig", errors="replace")


def packages():
    """(requires, owner, module_of_package, package_of_module) read from every src/*/*.dpk."""
    requires, owner, of_module = {}, {}, {}
    for dpk in sorted(SRC.glob("*/*.dpk")):
        pkg = dpk.stem
        text = strip_comments(read(dpk))
        block = re.search(r"^\s*requires(.*?);", text, re.S | re.M | re.I)
        requires[pkg] = [x.strip() for x in (block.group(1) if block else "")
                         .replace("\n", " ").split(",") if x.strip()]
        block = re.search(r"^\s*contains(.*?);", text, re.S | re.M | re.I)
        for unit in re.findall(r"([\w.]+)\s+in\s+'", block.group(1) if block else ""):
            owner[unit.lower()] = pkg
        of_module[dpk.parent.name] = pkg
    return requires, owner, of_module


def closure(pkg: str, requires: dict) -> set[str]:
    """The package itself plus everything its `requires` reaches, folded to real casing."""
    canon = {p.lower(): p for p in requires}
    seen, stack = {pkg}, [pkg]
    while stack:
        for name in requires.get(stack.pop(), []):
            real = canon.get(name.lower())
            if real and real not in seen:
                seen.add(real)
                stack.append(real)
    return seen


def project_units(dpr: pathlib.Path, owner: dict):
    """(packages_used, own_units) for one program, following `in '...'` transitively.

    `packages_used` maps package -> one unit of it that the project names, so the report
    can say WHICH unit brought the package rather than only that it did.
    """
    used, own, seen, todo = {}, [], set(), [dpr]
    while todo:
        path = todo.pop()
        if path in seen or not path.is_file():
            continue
        seen.add(path)
        if path != dpr:
            own.append(path.stem)
        text = strip_comments(read(path))
        for clause in re.finditer(r"\buses\b(.*?);", text, re.S | re.I):
            for unit, rel in re.findall(r"([\w.]+)\s*(?:in\s*'([^']*)')?", clause.group(1)):
                if not unit:
                    continue
                holder = owner.get(unit.lower())
                if holder:
                    used.setdefault(holder, unit)
                if rel:
                    todo.append(path.parent / rel.replace("\\", "/"))
                else:
                    # A unit of the sample's own that the .dpr did not qualify with a path.
                    local = sorted(dpr.parent.rglob(f"{unit}.pas"))
                    if local:
                        todo.append(local[0])
    return used, own


def judge(dpr, host_pkg, requires, owner):
    """The packages this sample uses that its host does not declare, after the exemptions.

    Returns (findings, used, exempted) where findings maps package -> the unit that
    brought it, and `exempted` records what each exemption absorbed and why.
    """
    reach = closure(host_pkg, requires)
    used, own = project_units(dpr, owner)
    is_runner = any(u.lower().endswith(".feat") for u in own)

    findings, exempted = {}, {}
    for pkg, unit in sorted(used.items()):
        if pkg in reach:
            continue
        if f"{pkg}.Abstractions" in reach:
            exempted[pkg] = f"implementation of {pkg}.Abstractions, which {host_pkg} requires"
        elif pkg == "Daf.MiniSpec" and is_runner:
            exempted[pkg] = "Daf.MiniSpec in the sample's own spec runner"
        else:
            findings[pkg] = unit
    return findings, used, exempted


def candidates(dprs, requires, owner, of_module, exclude=None):
    """The modules that could host every program in `dprs`, lowest first.

    Answered by re-running the verdict against each module rather than by comparing
    package sets, so the exemptions count here too -- otherwise the check would refuse
    a destination it would then accept, and say nothing about why.

    "Lowest" is the size of the transitive closure: a module that reaches fewer packages
    is further down the graph. It is a ranking for a person to read, not the verdict --
    when two modules tie, only a person can say which one the sample is about.
    """
    fits = []
    for module, pkg in sorted(of_module.items()):
        if module == exclude:
            continue
        if all(not judge(dpr, pkg, requires, owner)[0] for dpr in dprs):
            fits.append((len(closure(pkg, requires)), module))
    return [m for _, m in sorted(fits)]


def main(argv):
    verbose = "--verbose" in argv
    requires, owner, of_module = packages()
    if not requires:
        print("no .dpk under src/*/ — is the repo root right?", file=sys.stderr)
        return 2

    if "--probe" in argv:
        rest = argv[argv.index("--probe") + 1:]
        if len(rest) < 2:
            print("--probe needs <sample-dir-relative-to-src> <module>", file=sys.stderr)
            return 2
        sample, module = rest[0].replace("\\", "/"), rest[1]
        dprs = sorted((SRC / sample).glob("*.dpr"))
        if not dprs:
            print(f"no .dpr under src/{sample}", file=sys.stderr)
            return 2
        if module not in of_module:
            print(f"{module} is not a module with a .dpk under src/", file=sys.stderr)
            return 2
        host = of_module[module]
        for dpr in dprs:
            findings, used, exempted = judge(dpr, host, requires, owner)
            where = f"src/{sample}/{dpr.name} as if under {module} ({host})"
            if findings:
                print(f"FINDING  {where}")
                for pkg, unit in findings.items():
                    print(f"           uses {pkg} (via {unit}), which {host} does not declare")
            else:
                print(f"clean    {where}")
            for pkg, why in exempted.items():
                print(f"           exempted {pkg}: {why}")
        return 0

    module_dprs = sorted(SRC.glob("*/samples/*/*.dpr"))
    edition_dprs = sorted(SRC.glob("samples/*/*.dpr"))
    if not module_dprs and not edition_dprs:
        print("no .dpr under src/*/samples/ or src/samples/ — nothing was swept, so this is"
              " not a clean report", file=sys.stderr)
        return 2

    misfiled = 0

    for dpr in module_dprs:
        module = dpr.relative_to(SRC).parts[0]
        host = of_module.get(module)
        rel = dpr.relative_to(ROOT).as_posix()
        if not host:
            misfiled += 1
            print(f"  {rel}: its module has no .dpk, so there is nothing to check it against")
            continue
        bad, used, exempted = judge(dpr, host, requires, owner)
        if verbose:
            print(f"  {dpr.relative_to(SRC)}  [{host}]  uses "
                  f"{', '.join(sorted(used)) or 'no DAF package'}")
        if not bad:
            continue
        misfiled += 1
        print(f"  {rel}")
        for pkg, unit in bad.items():
            print(f"      uses {pkg} (via {unit}), and {host} does not require it")
        fits = candidates([dpr], requires, owner, of_module, exclude=module)
        print(f"      belongs to: {fits[0] if fits else 'NO module declares all of it, so it'
                                  ' is a sample of the edition — src/samples/'}"
              + (f"  (also fits: {', '.join(fits[1:])})" if len(fits) > 1 else ""))

    # A sample of the edition is judged the other way round: it is misfiled if some single
    # module covers it. Judged per DIRECTORY and not per program, because a sample and its
    # own spec runner move together -- the app is what constrains where the pair can live.
    for sample_dir in sorted({d.parent for d in edition_dprs}):
        dprs = sorted(sample_dir.glob("*.dpr"))
        fits = candidates(dprs, requires, owner, of_module)
        if verbose:
            print(f"  {sample_dir.relative_to(SRC)}  [edition]  "
                  f"{len(dprs)} program(s), fits {fits or 'no single module'}")
        if fits:
            misfiled += 1
            print(f"  {sample_dir.relative_to(ROOT).as_posix()}")
            print(f"      every package it uses is declared by {fits[0]}, so it belongs to that"
                  f" module and not to the edition"
                  + (f" (also fits: {', '.join(fits[1:])})" if len(fits) > 1 else ""))

    swept = len(module_dprs) + len(edition_dprs)
    print(f"{swept} sample program(s) swept ({len(edition_dprs)} of the edition),"
          f" {misfiled} misfiled")
    return 1 if misfiled else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
