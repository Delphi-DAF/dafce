#!/usr/bin/env python3
"""Comprueba que CHANGELOG.md y CHANGELOG.es.md son el mismo documento en dos idiomas.

POR QUE EXISTE
--------------
El changelog se partio en dos ficheros el 2026-09-09 porque llevaba mezclados los dos
idiomas: 1.1.0-1.4.0 en ingles y 1.5.0, 1.6.0 y [Unreleased] en castellano. La division
resuelve la mezcla y crea una deuda nueva: cada entrada se escribe dos veces. Los README
de modulo tienen la misma deuda y ahi el riesgo se diluye; en un changelog, una entrada
que falta en un idioma es una feature que un lector no sabe que existe.

QUE COMPRUEBA, Y POR QUE ASI
----------------------------
1. Estructura: las mismas versiones, en el mismo orden; las mismas secciones dentro de
   cada version, en el mismo orden; y el mismo numero de entradas en cada seccion. Es lo
   que caza la entrada anadida a un idioma y olvidada en el otro, que es el modo de fallo
   que la division introduce.
2. Idioma del canonico: CHANGELOG.md es el fichero que GitHub ensena y el que acompana al
   README de la raiz, los dos en ingles. Un marcador de castellano ahi es el defecto que
   motivo la division, y la estructura NO lo ve.

Lo que NO comprueba, a proposito: que la traduccion sea fiel. Eso lo lee una persona. Y
tampoco la "englishness" del fichero espanol: nombres como Live Dashboard o Test Doubles
son identificadores publicados, asi que una lista de palabras inglesas daria falsos rojos
sin fin.

El umbral del chequeo 2 es 1 marcador, y se midio en las dos direcciones. Con umbral 3
—elegido a ojo contra los falsos positivos— una entrada traducida se cuela: "descartar las
entradas a proposito" da DOS marcadores y pasaba en verde. El positivo verdadero de este
chequeo es exactamente eso, una entrada, asi que el umbral tiene que ser 1. Medido el
2026-09-09: los dos ficheros canonicos dan CERO con esta lista, asi que el cero es
alcanzable y esto puede ser puerta.
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
EN, ES = ROOT / "CHANGELOG.md", ROOT / "CHANGELOG.es.md"

# Palabras funcion del castellano que no son identificadores ni nombres de producto.
ES_MARKERS = re.compile(
    r"\b(que|para|los|las|una|con|sin|desde|as[ií]|cuando|ahora|deja|pero|porque|"
    r"entradas|fichero|sigue|hace|tiene)\b", re.IGNORECASE)
UMBRAL_IDIOMA = 1


def perfil(path):
    """(version, (secciones...), (entradas por seccion...)) por cada version del fichero."""
    t = path.read_bytes().decode("utf-8-sig").replace("\r\n", "\n")
    out = []
    for m in re.finditer(r"^## (\[[^\]]+\]).*$", t, re.M):
        i = m.end()
        j = t.find("\n## ", i)
        cuerpo = t[i:j if j > 0 else len(t)]
        secs, cuentas = [], []
        for s in re.finditer(r"^### (\w+)$", cuerpo, re.M):
            k = s.end()
            n = cuerpo.find("\n### ", k)
            trozo = cuerpo[k:n if n > 0 else len(cuerpo)]
            secs.append(s.group(1))
            cuentas.append(len([l for l in trozo.splitlines() if l.strip().startswith("- ")]))
        out.append((m.group(1), tuple(secs), tuple(cuentas)))
    return out


def main():
    fallos = []
    for f in (EN, ES):
        if not f.exists():
            fallos.append(f"falta {f.name}")
    if fallos:
        print("FAIL changelog parity ·", "; ".join(fallos))
        return 1

    a, b = perfil(EN), perfil(ES)
    ver_a, ver_b = [v for v, _, _ in a], [v for v, _, _ in b]
    if ver_a != ver_b:
        fallos.append(f"versiones distintas: EN {ver_a} vs ES {ver_b}")
    else:
        for (v, sa, ca), (_, sb, cb) in zip(a, b):
            if sa != sb:
                fallos.append(f"{v}: secciones EN {'/'.join(sa)} vs ES {'/'.join(sb)}")
            elif ca != cb:
                d = [f"{s}: {x} vs {y}" for s, x, y in zip(sa, ca, cb) if x != y]
                fallos.append(f"{v}: entradas distintas — {', '.join(d)}")

    hits = ES_MARKERS.findall(EN.read_bytes().decode("utf-8-sig"))
    if len(hits) >= UMBRAL_IDIOMA:
        fallos.append(f"{EN.name} tiene {len(hits)} marcadores de castellano "
                      f"(p.ej. {', '.join(sorted(set(h.lower() for h in hits))[:5])})")

    n = len(ver_a)
    if fallos:
        print(f"FAIL changelog parity · {n} versiones")
        for f in fallos:
            print(f"  {f}")
        return 1
    print(f"ok   changelog parity · {n} versiones, "
          f"{sum(sum(c) for _, _, c in a)} entradas en cada idioma")
    return 0


if __name__ == "__main__":
    sys.exit(main())
