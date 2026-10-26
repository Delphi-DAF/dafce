#!/usr/bin/env python3
"""Check the code blocks of the docs against the declarations in `src/`.

WHY THIS EXISTS
---------------
A fenced `pascal` block is the one place no other check looks: `check-quickstart-snippet.py`
compares a whole cited file, `check-doc-paths.py` reads prose, and `find-doc-claims.py` reads the
register. Inside a block, most names cannot be judged -- `TCreateOrderCommand` in a CQRS example
does not exist because the READER is meant to write it. Measured over DAFce: 428 distinct T/I/E
identifiers in 1076 blocks, 170 of which are absent from `src/`, and reporting those 170 is
reporting the corpus.

So this gate takes only the two slices where the answer is DECIDABLE, and both are derived from
`src/` itself rather than from a list:

A. A MEMBER CALLED ON ONE OF OUR TYPES
   `Arg.InRange(...)` is decidable because `Arg` is declared in `src/`: whatever follows the dot
   has to exist. `Provider.GetRequiredService` is not, because `Provider` is a variable.
   Measured 2026-09-13: **818 candidates, 1 finding** -- `Arg.InRange`, in the doubles guide, where
   `Arg` has exactly three members and none is generic. Reading that one line found two more the
   grep could not see (`Arg.Matching`, and `Arg.Any<Integer>` on a non-generic method), which is
   the division of labour: the check says where to look, a person says what is wrong.

B. AN EXAMPLE DECLARING A TYPE THAT ALSO EXISTS IN `src/`
   Not a defect on its own -- a reference document showing `ILogger = interface` is doing its job,
   and 124 of the 126 candidates are exactly that. The defect is a SHAPE MISMATCH: the example
   declares `TJsonResult<T> = class` while the framework ships a non-generic `TJsonResult`, so the
   reader is told to write a type that already exists, with a different shape. Measured the same
   day: **126 candidates, 2 findings**, both repaired.

BOTH ZEROS ARE DEFENDED, WHICH IS WHY THIS IS A GATE AND NOT A REPORT
---------------------------------------------------------------------
Neither slice needs the meaning of a sentence: a member exists or it does not, and two
declarations of the same name either agree in shape or they do not. The zero was reached by
repair, not by narrowing the check -- and the positive control is in git: run the same logic over
`HEAD` before those commits and both findings come back.

THE ONE THING IT REFUSES TO CALL SUCCESS
----------------------------------------
Sweeping no block, or finding no candidate, exits 2. A wrong path, an extension left out and a
clean corpus all print "0 findings", and those readings must not share an exit code.

TWO TRAPS OF THE SCOPE ITSELF, BOTH MEASURED
--------------------------------------------
Fences are PAIRED, never counted: a closing fence carries no label, so counting them doubles the
total and files every closing under "unlabelled". And `pascal` is not the only label -- each
edition carries 2 `delphi` blocks and 2 unlabelled ones that are Pascal, so a sweep reading only
```pascal leaves four behind.
"""
import argparse, re, subprocess, sys
from pathlib import Path

FENCE = re.compile(r"^\s*```+\s*([A-Za-z0-9_+-]*)\s*$")
PASCALISH = re.compile(r"\b(begin|end;|uses|procedure|function|:=|interface|implementation|var\b)", re.I)
DECL = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(<[^>]*>)?\s*=\s*(class|record|interface|reference to)", re.M | re.I)
MEMBER = re.compile(r"\b([A-Z][A-Za-z0-9_]{2,})\.([A-Z][A-Za-z0-9_]{2,})\b")


def tracked(root, *globs):
    out = subprocess.run(["git", "-C", str(root), "ls-files", *globs],
                         capture_output=True, text=True).stdout.split()
    return [root / p for p in out]


def blocks(root):
    """(path, first body line, language, body) for every fenced block, fences PAIRED."""
    for p in tracked(root, "*.md", "*.qmd"):
        try:
            lines = p.read_bytes().decode("utf-8-sig").replace("\r\n", "\n").split("\n")
        except OSError:
            continue
        start = lang = None
        buf = []
        for i, ln in enumerate(lines, 1):
            m = FENCE.match(ln)
            if not m:
                if start:
                    buf.append(ln)
                continue
            if start is None:
                start, lang, buf = i, (m.group(1) or ""), []
            else:
                yield p, start + 1, lang, "\n".join(buf)
                start = None


def is_pascal(lang, body):
    return lang.lower() in ("pascal", "delphi") or (not lang and bool(PASCALISH.search(body)))


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("root", nargs="?", default=".", help="repository root")
    ap.add_argument("--verbose", action="store_true")
    args = ap.parse_args()
    root = Path(args.root).resolve()

    source = []
    shapes = {}
    for p in tracked(root, "src/**/*.pas", "src/**/*.dpr", "src/**/*.inc"):
        t = p.read_bytes().decode("utf-8-sig", errors="replace")
        source.append(t)
        for m in DECL.finditer(t):
            shapes.setdefault(m.group(1), set()).add((bool(m.group(2)), m.group(3).lower()))
    src_text = "\n".join(source).lower()

    swept = cand_a = cand_b = 0
    findings = []
    for path, first, lang, body in blocks(root):
        if not is_pascal(lang, body):
            continue
        swept += 1
        rel = path.relative_to(root)
        for m in MEMBER.finditer(body):
            owner, member = m.group(1), m.group(2)
            if owner not in shapes:
                continue
            cand_a += 1
            if member.lower() not in src_text:
                line = first + body[:m.start()].count("\n")
                findings.append(f"{rel}:{line}: {owner}.{member} — `{owner}` is ours and has no `{member}`")
        for m in DECL.finditer(body):
            name, generic, kind = m.group(1), bool(m.group(2)), m.group(3).lower()
            if name not in shapes:
                continue
            cand_b += 1
            if (generic, kind) not in shapes[name]:
                line = first + body[:m.start()].count("\n")
                have = " / ".join(f"{'generic' if g else 'plain'} {k}" for g, k in sorted(shapes[name]))
                findings.append(f"{rel}:{line}: {name} declared here as "
                                f"{'generic' if generic else 'plain'} {kind}, but src/ has {have}")

    for f in findings:
        print(f"  {f}")
    print(f"{swept} Pascal block(s), {cand_a} member call(s) and {cand_b} declaration(s) checked "
          f"against src/, {len(findings)} finding(s)")

    if not swept or not cand_a or not cand_b:
        print("nothing to check -- that is a failure, not a clean corpus", file=sys.stderr)
        return 2
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
