#!/usr/bin/env python3
"""Check that every program's project registers exactly the units its `.dpr` compiles.

WHY THIS EXISTS
---------------
A program lists its own units twice, and two different tools read the two lists. The
compiler reads the `uses` clause of the `.dpr`: every `X in 'X.pas'` there is compiled and
linked. The IDE reads the `DCCReference` items of the `.dproj`, and that is what its Project
Manager shows. Nothing makes the two agree, and when they disagree nothing turns red: a unit
named only in the `.dpr` compiles, an incremental make recompiles it when it changes, its
features run, and the suite passes.

Seven programs carried 22 such units -- spec features, step definitions, a sample's domain
unit -- the same ones in both editions. Every one compiled; none was in its project. There is
no compiler error to wait for and no failing suite: this check is the only signal that can
exist.

HOW IT WORKS
------------
The scope is every tracked `.dproj` whose `<MainSource>` is a `.dpr`, derived and never
enumerated: the project declares its own program, so nothing is paired by file name, and a
program added tomorrow is checked without anybody editing this file. A project whose main
source is a `.dpk` is a package and declares its units in `contains`; it is counted and
skipped.

For each program, the units of the `.dpr` that carry `in '...'` are compared with the
`Include` of every `DCCReference`, both ways. A path is compared after turning `\\` into `/`,
resolving `..`, and folding case, because Windows makes no difference between them. A
reference with a `<Form>` inside counts like any other. Three findings:

  compiled by the .dpr, not registered      the case this check was written for
  registered, not compiled by the .dpr      a reference left behind by a removed unit
  registered, no tracked file at that path  a reference to a file that is not there

A reference is one finding however many of the last two it breaks.

Order is not compared. The projects did not keep the `.dpr` order before this check existed,
and what the check guards is membership.

WHY THE `.dpr` IS SCANNED AND NOT MATCHED
-----------------------------------------
A comment can say `uses`: the header of `CatalogServiceSample.dpr` is a `{ }` block that
talks about its uses clause, and a regex over the raw text starts the clause there and
swallows the `{$APPTYPE CONSOLE}` below it. A `//` can also sit inside the clause, as in
`TicTacToeSpecs.dpr`. So the text is walked once, left to right, the way the compiler does
and the way `check-samples-ownership.py` does.

The walk also records where the compiler directives are. A `{$IFDEF}` inside the clause
makes the set of compiled units depend on the defines, and the check does not guess which
branch counts: it names the program as one it could not read.

WHY IT FAILS LOUDLY WHEN IT CHECKS NOTHING
------------------------------------------
Move the projects, break the `MainSource` match, or run it from the wrong root, and a check
that only reports what it found would print "0 findings" -- success and total blindness
spelling the same word. Checking zero programs is a failure, and so is a program it could
not read: the zero it prints has to mean every program was compared.

Usage:
    python3 eng/check-dproj-units.py     # rc=0 clean, 1 on any finding,
                                         # 2 if it checked nothing or could not read a program
"""

from __future__ import annotations

import pathlib
import posixpath
import re
import subprocess
import sys
import xml.etree.ElementTree as ET

ROOT = pathlib.Path(__file__).resolve().parents[1]


def scan(text: str) -> tuple[str, list[int]]:
    """Comments blanked out and string literals kept, plus where each directive starts.

    Whitespace replaces what is removed, so positions in the result are positions in the
    source and nothing downstream can join two tokens that were never adjacent.
    """
    out, directives, i, n = [], [], 0, len(text)
    while i < n:
        c = text[i]
        if c == "'":
            j = i + 1
            while j < n and text[j] != "'":
                j += 1
            out.append(text[i:j + 1])
            i = j + 1
            continue
        if text.startswith("//", i):
            j = text.find("\n", i)
            j = n if j < 0 else j
        elif c == "{":
            if text.startswith("{$", i):
                directives.append(i)
            j = text.find("}", i)
            j = n if j < 0 else j + 1
        elif text.startswith("(*", i):
            if text.startswith("(*$", i):
                directives.append(i)
            j = text.find("*)", i)
            j = n if j < 0 else j + 2
        else:
            out.append(c)
            i += 1
            continue
        out.append(" " * (j - i))
        i = j
    return "".join(out), directives


def norm(base: str, rel: str) -> str:
    return posixpath.normpath(posixpath.join(base, rel.replace("\\", "/"))).lower()


def read(path: pathlib.Path) -> str:
    return path.read_bytes().decode("utf-8-sig", errors="replace")


def tracked(*patterns: str) -> list[str]:
    out = subprocess.run(["git", "-C", str(ROOT), "ls-files", *patterns],
                         capture_output=True, text=True, check=True).stdout
    return [line for line in out.splitlines() if line]


def dpr_units(dpr: pathlib.Path) -> list[tuple[str, str]]:
    """(unit, path as written) for every `in '...'` unit of the program's uses clause.

    Raises ValueError when the clause carries a compiler directive.
    """
    text, directives = scan(read(dpr))
    clause = re.search(r"\buses\b(.*?);", text, re.S | re.I)
    if not clause:
        return []
    if any(clause.start(1) <= d < clause.end(1) for d in directives):
        raise ValueError("a compiler directive inside the uses clause: which units compile"
                         " depends on the defines")
    return re.findall(r"([\w.]+)\s+in\s*'([^']*)'", clause.group(1), re.I)


def local(tag: str) -> str:
    return tag.rsplit("}", 1)[-1]


def project(dproj: pathlib.Path) -> tuple[str | None, list[str]]:
    """(main source, DCCReference includes) of one project."""
    root = ET.fromstring(dproj.read_bytes())
    main = next((el.text.strip() for el in root.iter()
                 if local(el.tag) == "MainSource" and el.text
                 and el.text.strip().lower().endswith((".dpr", ".dpk"))), None)
    refs = [el.get("Include", "") for el in root.iter() if local(el.tag) == "DCCReference"]
    return main, refs


def main() -> int:
    files = {f.lower() for f in tracked()}
    checked, packages, unreadable, findings = 0, 0, [], []

    for rel in tracked("*.dproj"):
        dproj = ROOT / rel
        base = posixpath.dirname(rel)
        try:
            main_source, refs = project(dproj)
        except ET.ParseError as e:
            unreadable.append(f"  {rel}: not well-formed XML ({e})")
            continue
        if not main_source or main_source.lower().endswith(".dpk"):
            packages += 1
            continue
        dpr_rel = norm(base, main_source)
        dpr = ROOT / posixpath.join(base, main_source.replace("\\", "/"))
        if not dpr.is_file():
            unreadable.append(f"  {rel}: its main source {main_source} is not there")
            continue
        try:
            units = dpr_units(dpr)
        except ValueError as e:
            unreadable.append(f"  {rel}: {e}")
            continue
        checked += 1

        dpr_base = posixpath.dirname(dpr_rel)
        compiled = {norm(dpr_base, path): path for _, path in units}
        registered = {norm(base, inc): inc for inc in refs}
        for key, path in compiled.items():
            if key not in registered:
                findings.append(f"  {rel}: {path} -- compiled by the .dpr, not registered")
        for key, inc in registered.items():
            facts = ([] if key in compiled else ["not compiled by the .dpr"]) \
                + ([] if key in files else ["no tracked file at that path"])
            if facts:
                findings.append(f"  {rel}: {inc} -- registered, {', and '.join(facts)}")

    for line in unreadable + findings:
        print(line)

    if not checked and not unreadable:
        print("FAIL dproj units · checked 0 programs: the sweep is broken, not the tree",
              file=sys.stderr)
        return 2
    programs = len({f.split(":", 1)[0] for f in findings})
    verdict = "FAIL" if findings or unreadable else "ok"
    print(f"{verdict} dproj units · {checked} program(s) checked ({packages} package(s) skipped),"
          f" {len(findings)} finding(s) in {programs} program(s)"
          + (f", {len(unreadable)} program(s) could not be read" if unreadable else ""))
    if unreadable:
        return 2
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
