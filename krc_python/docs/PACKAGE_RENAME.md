# Package Rename: krc → pykrc

## Rationale

The package directory has been renamed from `krc` to `pykrc` to avoid confusion:

- **`krc`** = The Fortran thermal model executable
- **`pykrc`** = The Python interface package

This makes it clearer that this is the Python interface, not the core KRC model itself.

## Changes Made

### Directory Structure
```
Before:
krc_python/
├── krc/              # Package directory
│   ├── __init__.py
│   ├── core.py
│   └── ...

After:
krc_python/
├── pykrc/            # Package directory (renamed)
│   ├── __init__.py
│   ├── core.py
│   └── ...
```

### Import Statements

**Before:**
```python
from krc import krc
from krc.config import set_krc_home
from krc.materials import calculate_thermal_properties
```

**After:**
```python
from pykrc import krc
from pykrc.config import set_krc_home
from pykrc.materials import calculate_thermal_properties
```

Note: The main function is still called `krc()` - only the package name changed.

### Files Updated

All Python files and documentation:
- `pykrc/*.py` - All module imports updated
- `tests/*.py` - Test imports updated
- `examples/*.py` - Example imports updated
- `setup.py` - Package data reference updated
- `*.md` - Documentation code examples updated

## Usage

The public API remains the same - only the import changes:

```python
from pykrc import krc, set_krc_home

# Everything else works identically
result = krc(lat=0, lon=0, body="Mars", ls=270, INERTIA=200, ALBEDO=0.25)
```

## Installation

No change to installation process:

```bash
cd krc_python
pip install -e .
```

The package is still installed as `krc-python` (PyPI name), but imported as `pykrc`.

## Testing

All tests still work:

```bash
pytest tests/
```

The test files have been updated to use `from pykrc import ...`

## Migration Guide

If you have existing code using the old package name:

**Find and replace:**
```bash
# In your Python files
sed -i 's/from krc\./from pykrc./g' your_script.py
sed -i 's/import krc$/import pykrc/g' your_script.py
```

**Manual check:**
- Update any `from krc import krc` to `from pykrc import krc`
- Update any `import krc.module` to `import pykrc.module`

## Summary

| Aspect | Before | After |
|--------|--------|-------|
| Package directory | `krc/` | `pykrc/` |
| Import statement | `from krc import` | `from pykrc import` |
| Main function | `krc()` | `krc()` (unchanged) |
| PyPI package | `krc-python` | `krc-python` (unchanged) |
| Module names | `krc.core`, `krc.config`, etc. | `pykrc.core`, `pykrc.config`, etc. |

This rename improves clarity while maintaining the intuitive `krc()` function name for the main interface.
