#!/usr/bin/env python3
"""Map every relative link inside the documents the site publishes to a target that works there.

WHY THIS EXISTS
---------------
The site publishes `src/**/*.md` through `{{< include >}}`, and a relative link inside such a
document is correct in the repository -- GitHub resolves it against the file's own directory -- and
meaningless in the site, where that file is not a page. Quarto resolves those links against the
temporary merged input it writes (`quarto-input<hash>.md`), so a link to a sibling README comes out
as `../../en/modules/quarto-input57a447753a049bf8.md.html`, and a link into another module's README
comes out as a `WARN: Unable to resolve link target` and a dead href. Measured on the rendered site
before this existed: **108 dead local links of 4311**, and the render still exited 0, so nothing
would ever have failed.

The two obvious fixes are both worse. An absolute `https://delphi-daf.github.io/...` URL in the
document makes a local render un-reviewable: the link leaves your build and lands on the published
site, so you read the old page while believing you checked the new one -- and a page that does not
exist yet 404s until the deploy. Hand-editing the links breaks the repository view, which is the
one most readers use.

HOW IT WORKS
------------
Each wrapper `.qmd` includes exactly one `.md` (asserted below), so the pair is a 1:1 map. For every
relative link inside those documents this script resolves the target against the *source* file's
directory and writes the replacement into `link-map.lua`, which the `include-links.lua` filter
applies at render time -- before Quarto's own link resolution, and keyed on `quarto.doc.input_file`,
the only place that still knows the real input.

A target that the site publishes becomes a project-root-relative `.qmd` path: Quarto rewrites those
correctly and keeps the anchor (measured -- `/en/modules/di.qmd#appmodule-pattern` renders as
`../en/modules/di.html#appmodule-pattern`). A target that exists in the repository but is not
published becomes a link to GitHub, which is where a reader can actually read it. Nothing is
invented: a target that does not exist at all is an error, because that is a link broken in the
repository too.

WHY IT FAILS LOUDLY
-------------------
`--check` compares the generated map with the one in git and fails on any difference, so a document
that grows a new link cannot reach the site without the map following it. And finding **no**
document with an include is an error rather than an empty map: a renamed wrapper directory would
otherwise turn this into a silent no-op that reports success.

Usage:
    python3 docs/_gen/link-map.py            # regenerate docs/_gen/link-map.lua
    python3 docs/_gen/link-map.py --check     # gate: rc=1 if the map is stale
"""

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent.parent
DOCS = ROOT / "docs"
MAP = DOCS / "_gen" / "link-map.lua"

# El sitio se construye desde main, asi que un enlace a GitHub apunta ahi.
REPO = "https://github.com/Delphi-DAF/dafce"
BRANCH = "main"

INCLUDE = re.compile(r"\{\{<\s*include\s+([^\s>]+)\s*>\}\}")
# Enlace markdown inline. El destino no lleva espacios ni parentesis: los que hay en estos
# documentos son rutas, y un destino con parentesis se veria en el informe como no encontrado.
LINK = re.compile(r"\]\(\s*([^)\s]+?)\s*\)")
RAW_HTML_LINK = re.compile(r"""<a\s[^>]*href=["']([^"']+)["']""", re.I)
EXTERNAL = re.compile(r"^(?:[a-z][a-z0-9+.-]*:|//|#|/)")


HEADING = re.compile(r"^(#{1,6})[ \t]+(.+?)[ \t]*#*[ \t]*$", re.M)
FENCE = re.compile(r"^(?:```|~~~)", re.M)


def slug_github(text: str) -> str:
    """El id que genera GitHub: sin puntuacion, los espacios a guion, el resto tal cual."""
    t = strip_inline(text).lower()
    t = re.sub(r"[^\w\s-]", "", t, flags=re.UNICODE)
    return re.sub(r"\s", "-", t.strip())


def slug_pandoc(text: str) -> str:
    """El id que genera pandoc (auto_identifiers), que es el que acaba en el sitio.

    Difiere del de GitHub en lo que conserva: el punto sobrevive -- 'Daf.MemUtils - Smart pointers'
    es daf.memutils-smart-pointers alli y dafmemutils--smart-pointers en GitHub -- y un id no puede
    empezar por algo que no sea letra, asi que '.NET x DAF' acaba en net-daf.
    """
    t = strip_inline(text).lower()
    t = re.sub(r"[^\w\s.-]", "", t, flags=re.UNICODE)
    t = re.sub(r"\s+", "-", t.strip())
    t = re.sub(r"-{2,}", "-", t)
    return re.sub(r"^[^a-z]+", "", t)


def strip_inline(text: str) -> str:
    """Quita el marcado inline que no llega al id: enlaces, codigo, enfasis."""
    text = re.sub(r"\[([^\]]*)\]\([^)]*\)", r"\1", text)
    text = text.replace("`", "").replace("*", "").replace("_", " ")
    return text


def anchors_of(text: str) -> dict:
    """{slug de GitHub: slug de pandoc} para cada encabezado, cuando difieren."""
    out, in_fence = {}, False
    for line in text.split("\n"):
        if FENCE.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        m = HEADING.match(line)
        if not m:
            continue
        gh, pd = slug_github(m.group(2)), slug_pandoc(m.group(2))
        if gh and pd and gh != pd:
            out[gh] = pd
    return out


def read(path: pathlib.Path) -> str:
    return path.read_bytes().decode("utf-8-sig", errors="replace")


def wrappers():
    """(qmd relative to docs, md relative to root) for every page that includes a document."""
    for qmd in sorted(DOCS.rglob("*.qmd")):
        for m in INCLUDE.finditer(read(qmd)):
            src = (qmd.parent / m.group(1)).resolve()
            yield qmd.relative_to(DOCS).as_posix(), src.relative_to(ROOT).as_posix()


def _add(links, qmd_rel, key, repl, findings):
    antes = links.setdefault(qmd_rel, {}).get(key)
    if antes is not None and antes != repl:
        findings.append(f"{qmd_rel}: '{key}' resuelve a {antes} y a {repl} segun el documento; "
                        f"el filtro no puede desempatarlo")
        return
    links[qmd_rel][key] = repl


def main(argv):
    check = "--check" in argv
    pages, published, findings = {}, {}, []

    for qmd_rel, md_rel in wrappers():
        pages.setdefault(qmd_rel, []).append(md_rel)
        if md_rel in published:
            findings.append(f"{md_rel} lo publican dos paginas: {published[md_rel]} y {qmd_rel}")
        published[md_rel] = qmd_rel

    if not pages:
        print("ningun .qmd bajo docs/ incluye un documento -- el marcador es "
              "'{{< include ... >}}'; si se renombro el directorio, este chequeo quedo ciego",
              file=sys.stderr)
        return 1

    # Una pagina puede incluir varios documentos -- sqlcute-guide.qmd junta once capitulos --,
    # asi que el mapa se clava en la PAGINA y no en el documento: es lo unico que el filtro sabe
    # de si mismo. Dos documentos de la misma pagina que resuelvan el mismo destino a sitios
    # distintos son un conflicto y se informa; el filtro no puede desempatarlos.
    # Las anclas del documento tambien cambian de forma al publicarse, y por la misma razon: el
    # fuente lleva el slug de GitHub y el sitio el de pandoc.
    anclas = {md_rel: anchors_of(read(ROOT / md_rel)) for md_rel in published}

    links = {}
    for md_rel, qmd_rel in sorted(published.items()):
        md = ROOT / md_rel
        text = read(md)
        targets = set(LINK.findall(text)) | set(RAW_HTML_LINK.findall(text))
        for raw in sorted(targets):
            if raw.startswith("#"):
                # Ancla de la propia pagina: solo hace falta traducir el slug.
                repl = anclas[md_rel].get(raw[1:])
                if repl:
                    _add(links, qmd_rel, raw, "#" + repl, findings)
                continue
            if EXTERNAL.match(raw):
                continue
            path, _, frag = raw.partition("#")
            if not path:
                continue
            try:
                target = (md.parent / path).resolve().relative_to(ROOT).as_posix()
            except ValueError:
                findings.append(f"{md_rel}: {raw} sale del repositorio")
                continue
            if target in published:
                repl = "/" + published[target]
                # El ancla del destino tambien se traduce, si el fuente la nombra.
                if frag:
                    repl += "#" + (anclas[target].get(frag) or frag)
            elif (ROOT / target).is_file():
                repl = f"{REPO}/blob/{BRANCH}/{target}"
            elif (ROOT / target).is_dir():
                repl = f"{REPO}/tree/{BRANCH}/{target}"
            else:
                findings.append(f"{md_rel}: {raw} no existe (roto tambien en el repositorio)")
                continue
            _add(links, qmd_rel, raw if frag else path, repl, findings)

    if findings:
        for f in findings:
            print(f"  {f}", file=sys.stderr)
        print(f"{len(findings)} problema(s) que el mapa no puede resolver", file=sys.stderr)
        return 1

    out = ["-- Generado por docs/_gen/link-map.py -- no editar a mano.",
           "-- Destino de cada enlace relativo de los documentos que el sitio publica.",
           "return {",
           "  -- cuantos documentos incluye cada pagina (informativo)",
           "  pages = {"]
    for qmd_rel in sorted(pages):
        out.append(f'    ["{qmd_rel}"] = {len(pages[qmd_rel])},')
    out += ["  },", "  links = {"]
    for qmd_rel in sorted(links):
        out.append(f'    ["{qmd_rel}"] = {{')
        for path in sorted(links[qmd_rel]):
            out.append(f'      ["{path}"] = "{links[qmd_rel][path]}",')
        out.append("    },")
    out += ["  },", "}", ""]
    content = "\n".join(out).encode("utf-8")

    n = sum(len(v) for v in links.values())
    if check:
        if not MAP.is_file():
            print(f"{MAP.relative_to(ROOT)} no existe -- ejecuta link-map.py", file=sys.stderr)
            return 1
        if MAP.read_bytes() != content:
            print(f"{MAP.relative_to(ROOT)} esta obsoleto -- ejecuta link-map.py", file=sys.stderr)
            return 1
        print(f"{len(pages)} pagina(s), {n} enlace(s) mapeado(s), mapa al dia")
        return 0

    MAP.write_bytes(content)
    print(f"{len(pages)} pagina(s), {n} enlace(s) mapeado(s) -> {MAP.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
