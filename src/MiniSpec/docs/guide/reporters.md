# Reporters

**🌍 Language: English | [Español](reporters.es.md)**

[← Back to Guide](../GUIDE.md)

---

Syntax: `-r <name>:<option1>=<value>,<option2>=<value>,...`

| Reporter | Options | Example |
|----------|---------|---------|
| `console` | *(none)* | `-r console` |
| `json` | `output=<file>` | `-r json:output=report.json` |
| `junit` | `output=<file>` | `-r junit:output=results.xml` |
| `gherkin` | `output=<dir>` | `-r gherkin:output=features/` |
| `live` | `port=<num>`, `wait=<ms>` | `-r live:port=8080,wait=5000` |

## Multiple Reporters

You can use multiple reporters in the same run by repeating the `-r` option:

```bash
# Console + JUnit for CI + JSON for analysis
MyApp.exe -r console -r junit:output=results.xml -r json:output=report.json
```

All reporters receive the same events and generate their output simultaneously.

## JUnit Reporter

Generates XML in JUnit format for CI/CD integration. Compatible with GitHub Actions, GitLab CI, Jenkins, Azure DevOps.

## Live Reporter

By default waits 3 seconds for browser connection. Use `wait=0` to disable.

## Configuration File

MiniSpec creates `MiniSpec.ini` in the executable directory if it doesn't exist:

```ini
[minispec]
reporters=console,live
filter=@unit
pause=true

[reporter.live]
port=8080
wait=3000

[reporter.junit]
output=results.xml
```

The `reporters` key accepts multiple reporter names separated by commas. Each reporter can have its own `[reporter.<name>]` section for options.

Reporters declared in `[reporter.X]` sections are automatically registered even if not listed in the `reporters` key. For example, the following is equivalent to `reporters=console,live`:

```ini
[minispec]
reporters=console

[reporter.live]
port=9000
```

> **Backward compatibility**: the old `reporter=` (singular) key is still read.

Command line options take priority over the file.

---

[← Dependency Injection](injection.md) | [Next: Command Line →](cli.md)
