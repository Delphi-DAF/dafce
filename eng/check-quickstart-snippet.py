#!/usr/bin/env python3
"""Check that every quick-start snippet in the docs is the sample the compiler reads.

WHY THIS EXISTS
---------------
The startup snippet in the docs had been wrong for months, in four different spellings, two of
which did not compile -- including the one on the public getting-started page, which is the first
thing anybody copies. Nobody noticed because nobody could: a snippet in a Markdown file is read by
no compiler, so "the docs describe an API that does not exist" and "the docs are fine" produce
exactly the same green. Specs prove the API exists; they say nothing about whether a document
describes it. This closes that gap by making the document quote a real, compiled file.

HOW IT WORKS
------------
A document cites a source file with a marker, and the fenced block right after it must be that
file, verbatim:

    <!-- quickstart: src/Cli/samples/QuickStart/QuickStart.dpr -->

    ```pascal
    ...the contents of that file...
    ```

The sample is in `DAFSamples.groupproj`, so it is built like any other. If the startup API changes, the sample stops compiling; if someone edits
a document instead, this stops matching. Either way something goes red.

WHY IT FAILS LOUDLY WHEN IT FINDS NOTHING
-----------------------------------------
Rename the marker and a check that only compares what it finds would report zero mismatches --
success and total blindness printing the same thing. So finding no citation at all is a failure,
not a pass, and so is a marker naming a file that does not exist.

WHAT IT DELIBERATELY DOES NOT DO
--------------------------------
It does not compile the fenced block, and it does not look at the other snippets in a document.
Extracting arbitrary fragments and compiling them needs a harness for code that is not a program;
this is the first rung of that ladder, not the whole thing. Partial snippets stay unchecked, and
that is a known gap rather than an oversight.

Line endings: the comparison is on normalized text, because the source is CRLF-with-BOM and the
document is CRLF, and a check that fails over invisible bytes gets ignored -- which is worse than
not having it.

Usage:
    python3 eng/check-quickstart-snippet.py            # gate: rc=1 on any mismatch
    python3 eng/check-quickstart-snippet.py --write     # rewrite the blocks from the sources
"""

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
MARKER = re.compile(r'<!--\s*quickstart:\s*(?P<path>[^\s>]+?)\s*-->')
FENCE = re.compile(r'^(?P<indent>[ \t]*)```(?P<lang>[A-Za-z0-9_+-]*)[ \t]*$')


def normalize(text: str) -> str:
    """Compare meaning, not bytes: no BOM, one line-ending, no trailing blank lines."""
    return text.lstrip('\ufeff').replace('\r\n', '\n').replace('\r', '\n').rstrip('\n')


def read(path: pathlib.Path) -> str:
    return path.read_bytes().decode('utf-8-sig', errors='replace')


# Build leftovers and the git directory. Everything else is fair game on purpose: scanning the
# worktree rather than `git ls-files` is what keeps a brand-new document with a citation from
# being invisible to this check until somebody remembers to stage it.
SKIP_DIRS = {'.git', '__history', 'out', '_site', '.quarto', 'node_modules'}


def docs():
    for path in sorted(ROOT.rglob('*')):
        if path.suffix not in ('.md', '.qmd') or not path.is_file():
            continue
        if SKIP_DIRS & set(path.relative_to(ROOT).parts):
            continue
        yield path


def citations(doc: pathlib.Path):
    """Yield (line_no, source_path_text, fence_start, fence_end, block) per marker in `doc`."""
    lines = read(doc).replace('\r\n', '\n').split('\n')
    for i, line in enumerate(lines):
        m = MARKER.search(line)
        if not m:
            continue
        start = next((j for j in range(i + 1, len(lines)) if FENCE.match(lines[j])), None)
        end = None
        if start is not None:
            closing = FENCE.match(lines[start]).group('indent') + '```'
            end = next((j for j in range(start + 1, len(lines))
                        if lines[j].rstrip() == closing), None)
        block = '\n'.join(lines[start + 1:end]) if end is not None else None
        yield i + 1, m.group('path'), start, end, block


def main(argv):
    write_mode = '--write' in argv
    findings, cited, rewritten = [], 0, 0

    for doc in docs():
        rel_doc = doc.relative_to(ROOT).as_posix()
        replacements = []
        for line_no, src_rel, start, end, block in citations(doc):
            cited += 1
            src = ROOT / src_rel
            if not src.is_file():
                findings.append(f'{rel_doc}:{line_no} cites {src_rel}, which does not exist')
                continue
            if block is None:
                findings.append(f'{rel_doc}:{line_no} has no fenced block after the marker '
                                f'(it should quote {src_rel})')
                continue
            want = normalize(read(src))
            if normalize(block) == want:
                continue
            if write_mode:
                replacements.append((start + 1, end, want.split('\n')))
            else:
                findings.append(
                    f'{rel_doc}:{line_no} does not match its source {src_rel}\n'
                    f'      the fenced block after that marker must be {src_rel} verbatim;\n'
                    f'      rerun with --write to copy it over')

        if write_mode and replacements:
            lines = read(doc).replace('\r\n', '\n').split('\n')
            for first, last, new in reversed(replacements):
                lines[first:last] = new
            doc.write_bytes(('\n'.join(lines)).replace('\n', '\r\n').encode('utf-8'))
            rewritten += 1

    if not cited:
        print('no quick-start citation found in any .md/.qmd under the repo -- the marker is'
              ' "<!-- quickstart: <path> -->"; if it was renamed, this check went blind',
              file=sys.stderr)
        return 1

    if write_mode:
        print(f'{cited} citation(s), {rewritten} document(s) rewritten')
        return 0

    for f in findings:
        print(f'  {f}')
    print(f'{cited} citation(s) checked, {len(findings)} mismatch(es)')
    return 1 if findings else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
