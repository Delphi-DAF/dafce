#!/usr/bin/env python3
"""Check that every `src/...` path a document cites is a path that exists.

WHY THIS EXISTS
---------------
Documents point at source files and sample folders in running prose, inside backticks. Nothing
read those: they are not Markdown links, so the site's link map never sees them; they are not
identifiers, so the claims sweep never sees them; and they are not quick-start markers, so the
snippet gate never sees them. A move leaves them behind silently.

It happened: filing every sample under the module that declares what it uses moved `PackageInit`
from `src/Hosting/samples/` to `src/Cli/samples/`, and `STARTUP-COMPOSITION.md` and its Spanish
pair kept pointing at the old place -- twice each, in both editions, on a page the site publishes.
Eight dead paths on the public web, and every check was green.

WHY THE SCOPE IS `src/` AND NOT ANY PATH-SHAPED TOKEN
-----------------------------------------------------
Measured before choosing: matching any backticked token with a `/` gives 34 non-resolving hits per
edition, and most are not paths at all -- `Pass/Fail/Skip`, `try/except`, `LIMIT/OFFSET`,
`OFFSET/FETCH`, `text/plain`. The rest are link *text* written relative to the document
(`SQLCute/samples/SQLCuteSample`), whose href does resolve. A gate born red is wallpaper, so the
scope is the tokens that start with `src/`: those are root-relative by construction, with nothing
to interpret. Zero is reachable there, and it is where this check starts.

`.private/` is out of scope: it is gitignored working material, and its plans name files that do
not exist yet on purpose -- seven such paths today, every one of them legitimate.

Usage:
    python3 eng/check-doc-paths.py            # gate: rc=1 on any dead path, rc=2 if it swept nothing
"""

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
CITED = re.compile(r'`(src/[A-Za-z0-9_.\-/]+)`')
SKIP = {'.private', 'out', '.quarto', 'node_modules', '.git'}


def documents():
    for pattern in ('*.md', '*.qmd'):
        for f in ROOT.rglob(pattern):
            if not SKIP & set(f.relative_to(ROOT).parts):
                yield f


def main() -> int:
    dead, cited, swept = [], 0, 0
    for f in documents():
        swept += 1
        text = f.read_text(encoding='utf-8-sig', errors='replace')
        for m in CITED.finditer(text):
            cited += 1
            if not (ROOT / m.group(1)).exists():
                dead.append((f.relative_to(ROOT), text[:m.start()].count('\n') + 1, m.group(1)))

    # Sweeping nothing and sweeping a clean corpus print the same count, so they must not share an
    # exit code -- a renamed extension or a moved script would otherwise read as a pass.
    if swept == 0 or cited == 0:
        print(f"[check-doc-paths] swept {swept} document(s) and found {cited} cited path(s): "
              f"that is not a clean corpus, it is a check that read nothing", file=sys.stderr)
        return 2

    for f, line, path in dead:
        print(f"  {f}:{line}  ->  {path}")
    print(f"[check-doc-paths] {len(dead)} dead path(s) of {cited} cited across {swept} document(s)")
    return 1 if dead else 0


if __name__ == '__main__':
    sys.exit(main())
