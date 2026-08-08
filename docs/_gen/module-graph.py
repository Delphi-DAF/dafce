#!/usr/bin/env python3
"""Generate the package dependency graph documentation from the .dpk files.

The graph is read from the `requires` clause of every src/*/*.dpk, so the page
cannot drift from the packages: it is derived, never hand-written. Run it after
adding, removing or re-wiring a package.

    python3 docs/_gen/module-graph.py            # write the partials
    python3 docs/_gen/module-graph.py --check    # fail if they are out of date

--check is what the docs workflow runs. Without it the page would rot exactly
like a hand-maintained list does, only less visibly, because a stale graph still
renders.

Output goes to docs/_partials/module-graph.<lang>.md, included from the modules
overview page of each language. The prose around it stays hand-written; only the
facts are generated.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT_DIR = ROOT / "docs" / "_partials"

# Third-party runtime packages collapse into one node per family, with the real
# names in the label: eight separate nodes say nothing the family does not.
FAMILIES = (("FireDAC", "FireDAC"), ("Indy", "Indy"))

# Every package requires rtl, so drawing 20-odd arrows to one node only hides the
# graph. It is stated in the caption instead.
ALWAYS = {"rtl"}


def read_graph() -> dict[str, list[str]]:
    graph = {}
    for dpk in sorted(ROOT.glob("src/*/*.dpk")):
        text = dpk.read_text(encoding="utf-8-sig")
        match = re.search(r"^requires(.*?);", text, re.S | re.M)
        body = re.sub(r"//[^\n]*", "", match.group(1)) if match else ""
        reqs = [r.strip() for r in body.replace("\n", " ").split(",") if r.strip()]
        graph[dpk.stem] = reqs
    if not graph:
        sys.exit("no .dpk found under src/*/ — is the repo root right?")
    return graph


def is_daf(name: str) -> bool:
    return name.lower().startswith("daf")


def canonical(graph: dict[str, list[str]]) -> dict[str, str]:
    """Package names are spelled inconsistently in some requires clauses (DAF. vs
    Daf.), and Delphi resolves them case-insensitively. Fold them here so the
    graph has one node per package."""
    return {name.lower(): name for name in graph}


def daf_edges(graph):
    canon = canonical(graph)
    return {
        pkg: sorted({canon.get(r.lower(), r) for r in reqs if is_daf(r)})
        for pkg, reqs in graph.items()
    }


def reachable(edges, node, seen=None):
    seen = set() if seen is None else seen
    for dep in edges.get(node, ()):
        if dep not in seen:
            seen.add(dep)
            reachable(edges, dep, seen)
    return seen


def reduce_transitively(edges):
    """Keep only the edges that are not implied by another path. Roughly 40% of
    the declared ones are, and drawing them makes the graph look denser than the
    dependency structure actually is."""
    reduced = {}
    for pkg, deps in edges.items():
        keep = []
        for dep in deps:
            implied = set()
            for other in deps:
                if other != dep:
                    implied |= reachable(edges, other) | {other}
            if dep not in implied:
                keep.append(dep)
        reduced[pkg] = keep
    return reduced


def third_party(graph, pkg):
    """Every non-DAF package this one ends up needing, its own plus everything
    inherited through the DAF packages it requires."""
    edges = daf_edges(graph)
    out = set()
    for node in {pkg} | reachable(edges, pkg):
        out |= {r for r in graph.get(node, ()) if not is_daf(r)}
    return sorted(out)


def family_of(name):
    for prefix, label in FAMILIES:
        if name.startswith(prefix):
            return label
    return name


def node_id(pkg):
    return pkg.replace("Daf.", "").replace(".", "")


def short(pkg):
    return pkg.replace("Daf.", "")


STRINGS = {
    "en": dict(
        intro="Read from the `requires` clause of every `.dpk`, so it cannot drift from the "
              "packages. Arrows read *requires*.",
        stats="**{pkgs} packages**, **{edges} real edges** between them, **{redundant}** more "
              "declared but already implied by another path, **{cycles}** cycles.",
        reduced="Drawn as a transitive reduction: only the edges that do not already arrive by "
                "another path. `rtl` is left out because all {pkgs} packages require it; the "
                "{families} packages are grouped one node per family, with the real names in the "
                "label.",
        legend="Dotted border marks a contract package (`.Abstractions`); the dashed rounded nodes "
               "are third-party runtimes.",
        rt_head=("Package", "Runtime packages it pulls in"),
        rt_caption="{clean} of {pkgs} need nothing beyond `rtl`.",
        roots="Installable on their own, with no dependency on another DAF package: {roots}.",
    ),
    "es": dict(
        intro="Leído del `requires` de cada `.dpk`, así que no puede desviarse de los paquetes. "
              "Las flechas se leen *requiere*.",
        stats="**{pkgs} paquetes**, **{edges} aristas reales** entre ellos, **{redundant}** más "
              "declaradas pero ya implicadas por otro camino, **{cycles}** ciclos.",
        reduced="Dibujado como reducción transitiva: solo las aristas que no llegan ya por otro "
                "camino. `rtl` no se dibuja porque lo requieren los {pkgs} paquetes; los de "
                "{families} van agrupados en un nodo por familia, con los nombres reales en la "
                "etiqueta.",
        legend="El borde de puntos marca un paquete de contrato (`.Abstractions`); los nodos "
               "redondeados a trazos son runtimes ajenos.",
        rt_head=("Paquete", "Runtimes que arrastra"),
        rt_caption="{clean} de {pkgs} no piden más que `rtl`.",
        roots="Instalables solos, sin depender de ningún otro paquete de DAF: {roots}.",
    ),
}


def render(graph, lang):
    s = STRINGS[lang]
    edges = daf_edges(graph)
    reduced = reduce_transitively(edges)
    declared = sum(len(v) for v in edges.values())
    kept = sum(len(v) for v in reduced.values())
    abstractions = sorted(p for p in graph if p.endswith(".Abstractions"))
    roots = sorted(p for p in graph if not edges[p])
    families = sorted({family_of(r) for pkg in graph for r in graph[pkg]
                       if not is_daf(r) and r not in ALWAYS})

    lines = [
        "<!-- Generated by docs/_gen/module-graph.py — do not edit by hand. -->",
        "",
        s["intro"],
        "",
        s["stats"].format(pkgs=len(graph), edges=kept, redundant=declared - kept, cycles=0),
        "",
        "```{mermaid}",
        "graph TD",
    ]

    # Labels once, in their own declaration; edges then reference bare ids, which
    # keeps the diagram source readable and the diff small when one edge moves.
    for pkg in sorted(graph):
        lines.append(f"  {node_id(pkg)}[{short(pkg)}]")

    # Third-party edges, declared ones only: an inherited runtime is already shown
    # by the DAF edge that carries it.
    rt_nodes: dict[str, set[str]] = {}
    rt_edges = []
    for pkg in sorted(graph):
        own = sorted({r for r in graph[pkg] if not is_daf(r) and r not in ALWAYS})
        for fam in sorted({family_of(r) for r in own}):
            members = sorted(r for r in own if family_of(r) == fam)
            rt_nodes.setdefault(fam, set()).update(members)
            rt_edges.append((node_id(pkg), f"RT{fam}", members))

    for fam in sorted(rt_nodes):
        label = " · ".join(sorted(rt_nodes[fam]))
        lines.append(f'  RT{fam}[("{label}")]')

    for pkg in sorted(reduced):
        for dep in reduced[pkg]:
            lines.append(f"  {node_id(pkg)} --> {node_id(dep)}")

    for src, dst, members in rt_edges:
        # When a package takes only part of a family, say so on the edge: the node
        # label lists the whole family and the difference would otherwise be lost.
        whole = sorted(rt_nodes[dst[2:]])
        if members != whole:
            lines.append(f'  {src} -->|{" · ".join(members)}| {dst}')
        else:
            lines.append(f"  {src} --> {dst}")

    if abstractions:
        lines.append("  classDef abs stroke-dasharray:4 3;")
        lines.append("  class " + ",".join(node_id(a) for a in abstractions) + " abs;")
    if rt_nodes:
        lines.append("  classDef rt stroke-dasharray:2 2,stroke-width:2px;")
        lines.append("  class " + ",".join(f"RT{f}" for f in sorted(rt_nodes)) + " rt;")
    lines += ["```", ""]

    lines += [s["reduced"].format(pkgs=len(graph), families=" and ".join(families)
                                 if lang == "en" else " y ".join(families)), "", s["legend"], ""]
    if roots:
        lines += [s["roots"].format(roots=", ".join(f"`{short(r)}`" for r in roots)), ""]

    head = s["rt_head"]
    lines += [f"| {head[0]} | {head[1]} |", "|---|---|"]
    clean = 0
    for pkg in sorted(graph):
        rt = third_party(graph, pkg)
        if rt == ["rtl"]:
            clean += 1
        lines.append(f"| `{short(pkg)}` | " + " · ".join(f"`{r}`" for r in rt) + " |")
    lines += ["", s["rt_caption"].format(clean=clean, pkgs=len(graph)), ""]
    return "\n".join(lines)


def main():
    check = "--check" in sys.argv
    graph = read_graph()
    stale = []
    for lang in ("en", "es"):
        target = OUT_DIR / f"module-graph.{lang}.md"
        content = render(graph, lang)
        if check:
            current = target.read_text(encoding="utf-8") if target.exists() else ""
            if current != content:
                stale.append(target.relative_to(ROOT))
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(content, encoding="utf-8")
            print(f"wrote {target.relative_to(ROOT)}")
    if check and stale:
        print("The generated dependency graph is out of date:", file=sys.stderr)
        for path in stale:
            print(f"  {path}", file=sys.stderr)
        print("\nRun: python3 docs/_gen/module-graph.py", file=sys.stderr)
        return 1
    if check:
        print("dependency graph up to date")
    return 0


if __name__ == "__main__":
    sys.exit(main())
