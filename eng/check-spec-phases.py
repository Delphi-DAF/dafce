#!/usr/bin/env python3
"""Cuenta escenarios cuyo `When` falta o esta desactivado con `.NoAction`.

POR QUE EXISTE
--------------
Un escenario valida el comportamiento de UN evento, y las tres fases son esa afirmacion
repartida: el `Given` deja el estado, el `When` ejecuta el evento, el `Then` observa lo que
quedo. Un escenario cuyo evento vive en otra fase sigue pudiendo pasar y fallar, asi que la
deriva no la delata ningun rojo. Lo que se pierde es concreto: con el evento en el `Given`, su
fallo se informa como `✗ Given ...` --que manda a buscar a la preparacion-- y el `Then` solo
puede comparar lo que el montaje dejo escrito; con el evento en el `Then`, o cada asercion lo
repite --y entonces son eventos distintos y no uno observado dos veces-- o solo la primera lo
ejecuta.

QUE CUENTA, Y QUE NO
--------------------
Cuenta ESCENARIOS, no apariciones de `.NoAction`: la marca cubre tambien el `Given` legitimo
--"nada registrado"-- y una puerta que los contase naceria roja por algo que esta bien.

Solo mira escenarios de una `Feature(` declarada en columna 0. Los de una `Feature(` indentada
son *fixtures*: features de juguete que un spec construye dentro de un paso para ejecutarlas
como sujeto, y forzarles un `When` cambiaria lo que el spec de fuera mide. Medido sobre las dos
ediciones: 1988 escenarios cuelgan de una feature en columna 0 y 20 de una anidada, sin nada en
medio. Y los pasos de un escenario se buscan anclados a SU indentacion: sin eso, cortar el
bloque al ver el `Feature(` anidado dejaba fuera el `When` que venia despues, y dos escenarios
correctos salian como hallazgo.

EL MARCADOR
-----------
Hay escenarios cuyo evento no se puede separar porque ES la ejecucion del propio paso --los de
`OutlinePlaceholderStep`, donde cada paso comprueba su propia descripcion ya sustituida--. Se
eximen con un comentario en el bloque que precede al `.Scenario(`:

    // [sin-When] la descripcion del propio paso es el sujeto: no hay evento que separar

La razon es obligatoria, y vive donde se lee. Un marcador sobre un escenario que SI tiene `When`
tambien se reporta: un marcador sin consecuencia se convierte en decorado.

QUE LEE
-------
Los `.Feat.pas` y los `.feature` que git versiona, y no un glob del disco: hasta el 2026-11-24
leia `src/*/test/*.Feat.pas`, y no veia ni la feature escrita a mano de un sample ni las specs
que viven mas abajo. Un `.Feat.pas` generado desde un `.feature` declara su `Feature(` con
sangria, asi que aqui cuenta como fixture: la fuente de verdad es el `.feature`,
y es lo que se lee. En Gherkin, el `When` es el paso con la palabra clave del idioma que declara
el fichero (`# language:`), `en` si no declara ninguno; un idioma sin palabras clave aqui es un
hallazgo, no un fichero que se salta. El marcador es un comentario de Gherkin, en el bloque que
precede al escenario:

    # [sin-When] la descripcion del propio paso es el sujeto: no hay evento que separar

LO QUE ESTA PUERTA NO PUEDE VER
-------------------------------
Que el `When` ejecute EL evento del escenario y no otra cosa. Eso pide leer la frase, y se queda
fuera como todo lo que pide leer.
"""
import re
import subprocess
import sys
from pathlib import Path

# Su razon de ser es demostrar que un paso desactivado se puede desactivar, asi que cada
# escenario suyo es un caso legitimo del que este chequeo habla.
EXCLUDED = {"MiniSpec/test/NoActionSteps.Feat.pas"}

SCENARIO = re.compile(r"^(\s*)\.Scenario\(")
FEATURE = re.compile(r"^(\s*)Feature\(")
SIBLING = re.compile(r"^(\s*)(\.Scenario\(|\.Rule\()")
WHEN = re.compile(r"^(\s*)\.When\(")
NOACTION = re.compile(r"\)\s*\.NoAction")
MARKER = re.compile(r"^\s*//\s*\[sin-When\]\s*(\S.*)$")


def scan(path: Path):
    """Devuelve (sin_when, marcador_sobrante), cada uno [(linea, descripcion, razon)]."""
    lines = path.read_bytes().decode("utf-8-sig", errors="replace").splitlines()
    missing, stale = [], []
    owner_is_toplevel = False
    i = 0
    while i < len(lines):
        feature = FEATURE.match(lines[i])
        if feature:
            owner_is_toplevel = len(feature.group(1)) == 0
            i += 1
            continue
        scenario = SCENARIO.match(lines[i])
        if not scenario or not owner_is_toplevel:
            i += 1
            continue
        start, indent = i, len(scenario.group(1))
        desc = lines[i].split("'")[1] if "'" in lines[i] else lines[i].strip()
        # El bloque de comentario que precede al escenario, no solo la linea de encima: una
        # razon suele necesitar dos lineas, y exigir que el marcador sea la ultima es una
        # trampa que solo se descubre cuando el escenario sale igualmente como hallazgo.
        reason = ""
        j = i - 1
        while j >= 0 and lines[j].lstrip().startswith("//"):
            marker = MARKER.match(lines[j])
            if marker:
                reason = marker.group(1).strip()
                break
            j -= 1
        i += 1
        has_when_with_body = False
        while i < len(lines):
            if lines[i].startswith("end."):
                break
            nested = FEATURE.match(lines[i])
            if nested and len(nested.group(1)) == 0:
                break
            sibling = SIBLING.match(lines[i])
            if sibling and len(sibling.group(1)) <= indent:
                break
            when = WHEN.match(lines[i])
            if when and len(when.group(1)) <= indent + 4:
                # `.When('x').NoAction` cabe en una linea; `.When('x',` continua en la
                # siguiente con su lambda. Se miran tres por si la llamada se parte.
                if not NOACTION.search(" ".join(lines[i:i + 3])):
                    has_when_with_body = True
            i += 1
        if has_when_with_body and reason:
            stale.append((start + 1, desc, reason))
        elif not has_when_with_body and not reason:
            missing.append((start + 1, desc, ""))
    return missing, stale


# Las palabras clave de Gherkin, por idioma: las mismas que acepta su compilador.
GHERKIN = {
    "en": {"scenario": ("Scenario Outline", "Scenario Template", "Scenario", "Example"),
           "section": ("Feature", "Rule", "Background", "Examples", "Scenarios"),
           "when": ("When",)},
    "es": {"scenario": ("Esquema del escenario", "Escenario", "Ejemplo"),
           "section": ("Caracter\u00edstica", "Necesidad del negocio", "Requisito", "Regla de negocio",
                       "Regla", "Antecedentes", "Contexto", "Ejemplos"),
           "when": ("Cuando",)},
}
LANGUAGE = re.compile(r"^\s*#\s*language:\s*(\S+)")
GHERKIN_MARKER = re.compile(r"^\s*#\s*\[sin-When\]\s*(\S.*)$")


def scan_feature(path: Path):
    """Lo mismo que scan, en Gherkin. Devuelve (sin_when, marcador_sobrante, idioma_desconocido)."""
    lines = path.read_bytes().decode("utf-8-sig", errors="replace").splitlines()
    language = "en"
    for line in lines:
        found = LANGUAGE.match(line)
        if found:
            language = found.group(1)
            break
    keys = GHERKIN.get(language)
    if keys is None:
        return [], [], language
    missing, stale = [], []
    scenario, has_when, docstring = None, False, False

    def close():
        if scenario is None:
            return
        line, desc, reason = scenario
        if has_when and reason:
            stale.append(scenario)
        elif not has_when and not reason:
            missing.append((line, desc, ""))

    for i, raw in enumerate(lines):
        text = raw.strip()
        if text.startswith('"""') or text.startswith("```"):
            docstring = not docstring
            continue
        if docstring or text.startswith("|") or text.startswith("#") or text.startswith("@"):
            continue
        keyword = text.split(":", 1)[0].strip() if ":" in text else None
        if keyword in keys["scenario"] or keyword in keys["section"]:
            close()
            scenario, has_when = None, False
            if keyword in keys["scenario"]:
                # El bloque de comentarios y tags que precede al escenario.
                reason, j = "", i - 1
                while j >= 0 and lines[j].strip()[:1] in ("#", "@"):
                    marker = GHERKIN_MARKER.match(lines[j])
                    if marker:
                        reason = marker.group(1).strip()
                        break
                    j -= 1
                scenario = (i + 1, text.split(":", 1)[1].strip(), reason)
            continue
        if scenario and any(text.startswith(word + " ") for word in keys["when"]):
            has_when = True
    close()
    return missing, stale, None


def tracked(root: Path, pattern: str):
    """Los ficheros que git versiona y casan con el patron. Sin git no hay alcance, y un
    alcance vacio leido como 'cero hallazgos' es justo el verde falso que esto evita."""
    listed = subprocess.run(["git", "-C", str(root), "ls-files", "-z", "--", pattern],
                            capture_output=True, text=True)
    if listed.returncode != 0:
        sys.exit(f"git ls-files {pattern}: {listed.stderr.strip()}")
    return sorted(p for p in listed.stdout.split("\0") if p)


def report(rel, missing, stale):
    for line, desc, _ in missing:
        print(f"{rel}:{line}: sin un When con cuerpo: {desc}")
    for line, desc, reason in stale:
        print(f"{rel}:{line}: marcador [sin-When] sobre un escenario que si lo tiene: {desc}")
        print(f"    dice: {reason}")
    return len(missing) + len(stale)


def main(argv):
    root = Path(argv[1]) if len(argv) > 1 and not argv[1].startswith("-") else Path(".")
    findings = 0
    for rel in tracked(root, "*.Feat.pas"):
        if rel.removeprefix("src/") in EXCLUDED:
            continue
        findings += report(rel, *scan(root / rel))
    for rel in tracked(root, "*.feature"):
        missing, stale, unknown = scan_feature(root / rel)
        if unknown:
            print(f"{rel}: idioma de Gherkin sin palabras clave aqui: {unknown}")
            findings += 1
        findings += report(rel, missing, stale)
    print(f"{findings} hallazgo(s)")
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
