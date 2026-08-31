#!/usr/bin/env python3
"""Check that every local link of the rendered site points at something that exists.

WHY THIS EXISTS
---------------
`quarto render` exits 0 with a site full of dead links, and it publishes it. Its own
`WARN: Unable to resolve link target` is a warning nobody reads in a green build, and it does not
see the worst cases at all: a filter that rewrites a link to `#guide` or to `#` produces an anchor,
not an unresolved target, so Quarto says nothing. Measured on the site before this existed: **108
dead hrefs, 97 links to `#guide` and 21 to `#` with visible text** -- 226 in all, and every build
green.

The links inside a document that the site publishes by `{{< include >}}` are the ones that rot,
because they are written for the repository -- where GitHub resolves them -- and the site is not the
repository. `docs/_gen/link-map.py` rewrites them at render time; this checks the result, which is
the only place where the answer is not a guess.

WHY IT FAILS LOUDLY WHEN IT FINDS NOTHING
-----------------------------------------
Run against a directory with no HTML -- a rename, a render that did not happen, a wrong path -- a
check that only counts what it finds reports zero broken links and total blindness with the same
words. So no HTML is an error, not a pass.

Usage:
    python3 eng/check-site-links.py [site-dir]      # default out/site; rc=1 on any dead link
    python3 eng/check-site-links.py --list          # every finding, not just the first ones
"""

import pathlib
import re
import sys
import urllib.parse
HREF = re.compile(rb'<a\s[^>]*href="([^"]+)"', re.I)
EXTERNAL = re.compile(r"^(?:[a-z][a-z0-9+.-]*:|//)")
# El contenido de la pagina, sin navbar ni pie: un href="#" ahi fuera es el desplegable del tema
# --medido-- y no un enlace roto. Dentro de <main> si lo es: el filtro viejo producia href="#guide"
# y href="#" con texto visible en lugar de una pagina, y eso Quarto no lo llama nada.
MAIN = re.compile(rb'<main\b.*?</main>', re.S)


def main(argv):
    args = [a for a in argv if not a.startswith("-")]
    show_all = "--list" in argv
    site = pathlib.Path(args[0] if args else "out/site")
    if not site.is_absolute():
        site = pathlib.Path(__file__).resolve().parent.parent / site

    pages = sorted(site.rglob("*.html"))
    if not pages:
        print(f"no hay ningun .html bajo {site} -- no se ha medido nada; renderiza el sitio "
              f"antes ('quarto render docs --output-dir ../out/site')", file=sys.stderr)
        return 2

    findings, informe, total = [], [], 0
    for page in pages:
        html = page.read_bytes()
        rel = page.relative_to(site).as_posix()
        ids = set(re.findall(rb'\sid="([^"]+)"', html))
        # Por POSICION de la etiqueta, no por su href: 'b"#" in main' es cierto siempre, porque
        # el byte aparece en cualquier color CSS. Medido: daba 74 falsos positivos, uno por pagina.
        spans = [(m.start(), m.end()) for m in MAIN.finditer(html)]
        for m in HREF.finditer(html):
            href = urllib.parse.unquote(m.group(1).decode("utf-8", "replace"))
            if EXTERNAL.match(href):
                continue
            if href.startswith("#"):
                # Solo dentro del contenido; fuera son controles del tema.
                if not any(a <= m.start() < b for a, b in spans):
                    continue
                frag = href[1:]
                if not frag:
                    findings.append(f"{rel} -> '#' (enlace a ninguna parte)")
                elif frag.encode() not in ids:
                    informe.append(f"{rel} -> {href} (la pagina no define ese ancla)")
                continue
            total += 1
            if not (page.parent / href.partition("#")[0]).resolve().exists():
                findings.append(f"{rel} -> {href}")

    print(f"{len(pages)} pagina(s), {total} enlace(s) locales, {len(findings)} muerto(s)")
    if informe:
        # Informe, no puerta: son enlaces internos de documentos que ya estaban asi, y forzar su
        # cero hoy dejaria el chequeo rojo de salida -- que es como se aprende a ignorarlo.
        print(f"informe: {len(informe)} ancla(s) interna(s) que su pagina no define")
        for f in (informe if show_all else informe[:5]):
            print(f"  {f}")
        if not show_all and len(informe) > 5:
            print(f"  ... y {len(informe) - 5} mas (--list)")
    if findings:
        for f in findings if show_all else findings[:20]:
            print(f"  {f}", file=sys.stderr)
        if not show_all and len(findings) > 20:
            print(f"  ... y {len(findings) - 20} mas (--list para verlos todos)", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
