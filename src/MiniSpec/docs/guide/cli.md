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

> 💡 `--stacktrace` requires a stack trace library (JclDebug, MadExcept, EurekaLog).

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
```

---

[← Reporters](reporters.md) | [Back to Guide →](../GUIDE.md)
