#!/usr/bin/env python3
"""Check that every `.Feat.pas` unit declares at least one feature.

WHY THIS EXISTS
---------------
The `.Feat` suffix is a promise: this unit holds a `Feature(...)`. Four units broke it and each
one broke it differently, which is why counting them was the only way to see it.

`SQLCute/test/SQLCute.String.Feat.pas` was **three bytes** -- a BOM and nothing else. It was born
empty in `ccd8b5f`, whose own message says it carries "LIKE and string-match WHERE scenarios (F3)"
and whose numstat for it reads `0 0`: the 133 lines went whole into its neighbour. No `.dpr` and no
`.dproj` referenced it, so nothing ever asked why it was there, and a later repackaging carried it
along to a new directory.

The other three -- `Doubles.Feat.pas` in `Application`, `DependencyInjection` and `web` -- held only
helper types. Same suffix, no feature, and the same name as MiniSpec's own `Doubles.Feat.pas`, which
**is** a feature because there the doubles are what gets specified. They are `[Module].Doubles.pas`
now.

Neither shape had a red waiting for it. A spec runner names its units one by one in the `.dpr`, so
a unit nobody names simply does not exist for the build; and a unit that is named but declares no
feature compiles and links in silence, adding nothing to the run. The suite stays green either way,
and the count of scenarios does not move -- so "this file holds specs" and "this file holds nothing"
print exactly the same thing.

HOW IT WORKS
------------
Every tracked `*.Feat.pas` must contain a `Feature(` call -- wherever it lives, module tests and a
sample's own runner alike. That is all: the suffix says what the unit is, and this makes the tree
agree with the suffix.

It does NOT check the reverse -- a unit with a feature that lacks the suffix -- because that has no
reachable zero: step definitions and helpers legitimately carry features' worth of code without
being features, and telling them apart needs reading.

WHY IT FAILS LOUDLY WHEN IT FINDS NOTHING
-----------------------------------------
Move the tests, rename the suffix, or break the glob, and a check that only reports what it found
would print "0 offenders" -- success and total blindness spelling the same word. Sweeping zero
units is a failure here, not a pass.
"""
import pathlib
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent


def tracked_feat_units():
    # The scope is EVERY tracked `.Feat.pas`, derived and never enumerated. A first version
    # globbed `src/*/test/` and silently skipped nine: the spec runners that live inside a
    # sample -- `TicTacToeApp/Specs/`, `CalculatorSpecs/` -- which carry the same promise.
    out = subprocess.run(["git", "-C", str(ROOT), "ls-files", "*.Feat.pas"],
                         capture_output=True, text=True, check=True).stdout
    return [ROOT / line for line in out.splitlines() if line]


def main():
    units = tracked_feat_units()
    if not units:
        print("FAIL feat units · swept 0 units: the sweep is broken, not the tree", file=sys.stderr)
        return 2

    offenders = []
    for p in units:
        text = p.read_bytes().decode("utf-8-sig", errors="replace")
        if "Feature(" not in text:
            offenders.append(p.relative_to(ROOT))

    for o in offenders:
        print(f"  {o}: no Feature( -- a `.Feat` unit with no feature")
    verdict = "FAIL" if offenders else "ok"
    print(f"{verdict} feat units · {len(units)} unit(s) swept, {len(offenders)} without a feature")
    return 1 if offenders else 0


if __name__ == "__main__":
    sys.exit(main())
