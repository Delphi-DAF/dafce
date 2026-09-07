#!/usr/bin/env python3
"""Check the front matter of every page the site publishes: its sidebar exists, its language matches.

WHY THIS EXISTS
---------------
Front matter says nothing about the product, and it decides whether the content is seen at all.
`en/docs/migration-2.0.qmd` declared `sidebar: docs-en`, an id `_quarto.yml` does not define, and
that page was published without the sidebar its three siblings carry -- from January to
2026-08-31, when a person reading the file found it. Quarto renders an undefined sidebar id in
silence: no warning, no non-zero exit, a page that looks finished and has lost its navigation.

Nothing else in the workspace can see it. `link-map.py` maps the links inside a document,
`check-site-links.py` walks the rendered result -- and a page with no sidebar has no dead link and
is reachable from the navbar, so both are right and both say nothing.

WHAT IT CHECKS, AND WHY BOTH ZEROS ARE REACHABLE
------------------------------------------------
  * `sidebar:` -- the id is defined in `_quarto.yml`, or it is the literal `false`. Measured
    2026-09-06: 87 pages, four ids in use (`docs`, `docs-es`, `modules`, `modules-es`), 0 failures.
  * `lang:`    -- a page under `es/` declares `lang: es`. The project has no `lang` of its own, so
    Quarto's default is English and an undeclared Spanish page is published as English: it changes
    hyphenation, what a screen reader announces, and Quarto's own interface strings. 39 of 43
    declared it; the four that did not were fixed the same day, so this starts at zero too.

WHAT IT DELIBERATELY DOES NOT CHECK
-----------------------------------
`title:`. On a module page the title comes from the `# H1` of the README the page includes, which
is correct and deliberate -- requiring it would report sixty pages that are not defective, and a
gate born red is wallpaper. English pages are not required to declare `lang` either, for the same
reason: the project default already is English, so demanding it would be sixty more.

WHY IT FAILS LOUDLY WHEN IT FINDS NOTHING
-----------------------------------------
No `.qmd` under the directory -- a rename, a wrong path, a `docs/` that moved -- reads exactly like
a site whose front matter is perfect. So sweeping zero pages is an error, not a pass.

Usage:
    python3 eng/check-frontmatter.py [docs-dir]      # default docs; rc=1 on any defect
"""

import pathlib
import re
import sys

RE_FRONT_MATTER = re.compile(r"\A---\n(.*?)\n---", re.S)
RE_SIDEBAR = re.compile(r"^sidebar:\s*(\S+)", re.M)
RE_LANG = re.compile(r"^lang:\s*(\S+)", re.M)
# `_quarto.yml` declares them as `- id: <name>` under `website.sidebar`.
RE_ID = re.compile(r"^\s*-\s*id:\s*(\S+)", re.M)


def main(argv):
    root = pathlib.Path(argv[0] if argv else "docs")
    quarto = root / "_quarto.yml"
    if not quarto.is_file():
        print(f"[check-frontmatter] sin {quarto}: no hay ids contra los que contrastar",
              file=sys.stderr)
        return 2
    ids = set(RE_ID.findall(quarto.read_text(encoding="utf-8")))
    if not ids:
        print(f"[check-frontmatter] {quarto} no declara ningun sidebar id: no barri nada",
              file=sys.stderr)
        return 2

    pages = sorted(root.rglob("*.qmd"))
    if not pages:
        print(f"[check-frontmatter] cero .qmd bajo {root}: eso es un fallo, no un sitio correcto",
              file=sys.stderr)
        return 2

    findings = []
    for p in pages:
        m = RE_FRONT_MATTER.match(p.read_text(encoding="utf-8-sig").replace("\r\n", "\n"))
        if not m:
            findings.append(f"{p}: sin front matter")
            continue
        fm = m.group(1)
        sb = RE_SIDEBAR.search(fm)
        if sb and sb.group(1) != "false" and sb.group(1) not in ids:
            findings.append(f"{p}: sidebar '{sb.group(1)}' no lo define _quarto.yml "
                            f"(hay {', '.join(sorted(ids))})")
        parts = p.relative_to(root).parts
        if parts and parts[0] == "es":
            lg = RE_LANG.search(fm)
            if not lg:
                findings.append(f"{p}: bajo es/ y sin `lang: es` — se publica como inglesa")
            elif lg.group(1) != "es":
                findings.append(f"{p}: bajo es/ y declara lang '{lg.group(1)}'")

    print(f"[check-frontmatter] {len(pages)} pagina(s), {len(ids)} sidebar id(s), "
          f"{len(findings)} defecto(s)")
    for f in findings:
        print(f"  {f}", file=sys.stderr)
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
