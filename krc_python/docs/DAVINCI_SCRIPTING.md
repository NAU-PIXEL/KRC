# Running Davinci in Scripts - Best Practices

## Problem: Davinci Hangs in Batch Mode

When running davinci scripts non-interactively (e.g., from Python subprocess or bash scripts), davinci can hang waiting for stdin input even when using the `-f` flag.

## Solution: Two Critical Requirements

**Per Davinci User Guide (https://davinci.mars.asu.edu/index.php?title=User_Guide):**

1. **End script with `return` or `quit()`**: "end your script with a return statement or the last line will not execute"
2. **Provide empty stdin**: Always pipe empty input when running in batch mode

### Bash
```bash
echo "" | davinci -f script.dv
```

### Python subprocess
```python
import subprocess

result = subprocess.run(
    ['davinci', '-f', 'script.dv'],
    input='',  # Empty stdin prevents hanging
    capture_output=True,
    text=True,
    timeout=60
)
```

## Complete Example: Running with Timeout Protection

```bash
#!/bin/bash
# Run davinci with timeout protection

echo "" | davinci -f my_script.dv > output.txt 2>&1 &
DV_PID=$!

# Wait up to 15 seconds
sleep 15

if ps -p $DV_PID > /dev/null 2>&1; then
  echo "ERROR: Davinci still running after 15 seconds (likely hung)"
  kill -9 $DV_PID
  exit 1
else
  echo "SUCCESS: Davinci completed"
  exit 0
fi
```

## Loading krc.dvrc

To use the KRC thermal model functions, load krc.dvrc using `source()`:

```davinci
# Load krc.dvrc to define krc function
source("/path/to/krc.dvrc")

# Now krc() is available
result = krc(lat=0., lon=0., body="Mars", ls=270, INERTIA=200)
```

**Common mistakes:**
- ❌ `load("/path/to/krc.dvrc")` - Tries to parse as XML
- ❌ `$include /path/to/krc.dvrc` - Wrong syntax for davinci
- ✅ `source("/path/to/krc.dvrc")` - Correct!

## Debugging Davinci Scripts

1. **Check if krc is defined:**
   ```bash
   davinci -c "help krc"
   ```
   If not found: "Function not found: krc" - need to source krc.dvrc

2. **Run interactively first:**
   ```bash
   davinci
   davinci> source("/path/to/krc.dvrc")
   davinci> result = krc(lat=0.)
   ```

3. **Check for parse errors:**
   ```bash
   davinci -f script.dv 2>&1 | grep -i error
   ```

## Environment Variables

Davinci checks for:
- `KRC_HOME` - Path to KRC installation (must contain `krc` executable and support data)
- `.dvrc` - User's davinci initialization file (in home directory)

## Complete Validation Script Pattern

```python
#!/usr/bin/env python3
import subprocess
from pathlib import Path
import tempfile

# Create working directory
workdir = Path(tempfile.mkdtemp(prefix="davinci_test_"))

# Create davinci script
script_content = f'''
source("/path/to/krc.dvrc")
result = krc(
    lat=0.,
    lon=0.,
    body="Mars",
    ls=270,
    INERTIA=200,
    ALBEDO=0.25,
    KEEP="T",
    workdir="{workdir}",
    v=1
)
'''

script_file = workdir / "test.dv"
with open(script_file, 'w') as f:
    f.write(script_content)

# Run davinci with empty stdin
result = subprocess.run(
    ['davinci', '-f', str(script_file)],
    input='',  # CRITICAL: prevents hanging
    capture_output=True,
    text=True,
    timeout=60
)

print(f"Return code: {result.returncode}")
print(f"STDOUT:\n{result.stdout}")
print(f"STDERR:\n{result.stderr}")

# Check for input file
inp_file = workdir / "krc.inp"
if inp_file.exists():
    print(f"✓ Input file created: {inp_file}")
else:
    print(f"✗ Input file not found")
```

## Key Takeaways

1. **Always use `input=''`** in Python subprocess.run() when calling davinci
2. **Always use `echo "" |`** in bash when calling davinci
3. **Use `source()`** (not `load()` or `$include`) to load krc.dvrc
4. **Set timeout** to detect hangs (60s is usually sufficient for input file generation)
5. **Use absolute paths** for workdir parameter to krc()
6. **Check return code** - davinci may exit 0 even if krc() failed

## Troubleshooting

| Symptom | Cause | Solution |
|---------|-------|----------|
| Process hangs indefinitely | No stdin provided | Add `input=''` or `echo "" \|` |
| "Function not found: krc" | krc.dvrc not loaded | Add `source("/path/to/krc.dvrc")` |
| "Unable to determine file type" | Used `load()` instead of `source()` | Change to `source()` |
| "Environment variable not found: include" | Used `$include` syntax | Change to `source()` with string path |
| Input file not created | KRC execution failed, check workdir | Add `v=1` to krc() call for verbose output |
| "KRC_HOME not set" | Environment not configured | Set KRC_HOME or pass to krc() |

---

**Last Updated:** 2025-10-09
**Tested With:** Davinci 2.x, KRC 3.6.5
