# Command Line

**🌍 Language: English | [Español](cli.es.md)**

[← Back to Guide](../GUIDE.md)

---

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help |
| `-f, --filter <expr>` | Filter scenarios (see [Tags and Filtering](tags-filtering.md)) |
| `-t, --tags` | List all tags with counts |
| `-q, --query <expr>` | Show matching scenarios (without executing) |
| `-r, --reporter <spec>` | Reporter with options (see [Reporters](reporters.md)) |
| `--pause` | Wait for key at end |
| `--dry-run` | List scenarios without executing |
| `--stacktrace` | Show full stack trace on errors |
| `--strict-pending` | Treat pending specs as failures (exit code 1) |
| `--allow-undefined` | Undefined steps do not set the exit code (still counted and reported) |

> 💡 `--stacktrace` requires a stack trace library (JclDebug, MadExcept, EurekaLog).

### What sets the exit code

A **failed** spec and an **undefined** step set it to 1; **pending** specs do not, unless you ask
with `--strict-pending`. An undefined step is one whose description matched no binding — an
unintentional omission, not work in progress — so it fails by default: otherwise renaming a step
definition would leave the suite green while that step never runs.

`--allow-undefined` is for a suite that marks a step undefined **on purpose**, which is the only way
to check that the steps after it are skipped. It releases the exit code and nothing else: the summary
line still counts the undefined steps and the reporter still marks them. A suite that needs it can
declare it itself with `MiniSpec.AllowUndefined(True)`, instead of relying on whoever launches it to
pass the flag.

## Examples

```bash
# Help
MyApp.exe -h

# Run only unit tests
MyApp.exe -f "@unit"

# See which scenarios match without executing
MyApp.exe -q "@integration"

# List all tags
MyApp.exe -t

# Run with JUnit output for CI
MyApp.exe -r junit:output=results.xml

# Real-time dashboard
MyApp.exe -r live:port=8080

# Multiple reporters simultaneously
MyApp.exe -r console -r junit:output=results.xml -r json:output=report.json
```

## Configuration File

If a `MiniSpec.ini` file does not exist in the executable directory, MiniSpec creates one with default settings on first run. On subsequent runs, options are loaded from this file.

Command line options **always take priority** over the file. See [Reporters](reporters.md) for full INI syntax.

```ini
[minispec]
reporters=console,live
filter=@unit
pause=true

[reporter.live]
port=8080
```

---

[← Reporters](reporters.md) | [Next: Nested Runs →](nested-runs.md)
