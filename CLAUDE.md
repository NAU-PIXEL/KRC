# CLAUDE.md - AI Assistant Guidelines for KRC Project

**Last Updated:** 2025-10-19
**Purpose:** Maintain consistency and quality across AI assistant sessions working on the KRC Python project

---

## Project Overview

**KRC (Kieffer Regolith Cooling model)** is a 1D planetary thermal model written in Fortran. The `pykrc` Python package provides a modern interface to KRC while maintaining **exact parity** with the reference Davinci implementation.

**Primary Goal:** Achieve and maintain byte-exact compatibility with Davinci-generated KRC inputs and outputs.

---

## Critical Conventions

### 1. Davinci Parity is Sacred

- **ALL changes must maintain byte-exact compatibility** with Davinci KRC interface
- Reference Davinci source line numbers in comments (e.g., `# Per Davinci krc.dvrc line 231`)
- When in doubt, check the Davinci implementation first
- Test changes against reference outputs using `test_davinci_parity.py`
- Input file formatting must exactly match Fortran expectations (including quirks)

### 2. Code Style Standards

**Docstrings:**
- Use **NumPy-style docstrings** for all public functions
- Required sections: Parameters, Returns, Raises (if applicable)
- Optional sections: Notes, Examples, References
- Include units in parameter descriptions (e.g., "J m⁻² K⁻¹ s⁻½")
- Provide usage examples with `>>>` prompts

Example:
```python
def calculate_thermal_properties(inertia: float, density: float) -> Dict[str, float]:
    """
    Calculate thermal properties from inertia and density.

    Parameters
    ----------
    inertia : float
        Thermal inertia in SI units (J m⁻² K⁻¹ s⁻½)
    density : float
        Material density in kg/m³

    Returns
    -------
    Dict[str, float]
        Dictionary with keys 'conductivity' and 'heat_capacity'

    Notes
    -----
    Per Davinci krc.dvrc lines 604-608, this calculation follows
    the relationship: K = I² / (ρ * C)
    """
```

**Type Hints:**
- **Always use type hints** for function parameters and return values
- Use typing module for complex types: `Dict[str, Any]`, `Optional[float]`, `Union`, `Tuple`
- Return types must always be specified

**Naming Conventions:**
- Variables/functions: `snake_case`
- Classes: `PascalCase`
- Constants: `SCREAMING_SNAKE_CASE`
- **KRC parameters: UPPERCASE** (matching Fortran: `ALBEDO`, `INERTIA`, `N1`, `TDEEP`)
- Private functions/helpers: Leading underscore (`_helper_function`)

**Import Organization:**
```python
# Standard library
import os
from pathlib import Path
from typing import Dict, Any, Optional

# Third-party
import numpy as np
import pandas as pd

# Local
from pykrc.defaults import USER_DEFAULTS
from pykrc.materials import calculate_thermal_properties
```

### 3. Architecture Principles

**Module Organization:**
- **One responsibility per module** - don't mix concerns
- Key modules and their roles:
  - `core.py`: Main user-facing `krc()` function
  - `executor.py`: Fortran binary execution and input file generation
  - `materials.py`: Material property calculations
  - `numerical.py`: Numerical stability calculations (N1, N2, etc.)
  - `defaults.py`: **Single source of truth for ALL defaults**
  - `interface_validator.py`: Testing/validation framework

**Defaults Hierarchy:**
- ALL defaults must come from `defaults.py`
- Never hardcode default values in other modules
- Three-level parameter precedence:
  1. User-specified (highest priority)
  2. PORB-derived (body-specific defaults from orbital files)
  3. USER_DEFAULTS (pykrc system defaults)
  4. MASTER_INP_HEADER_DEFAULTS (Fortran placeholder values)

**Helper Functions:**
- Extract complex logic into private helper functions with leading underscore
- Helpers should have single, clear responsibility
- Example: `_extract_user_params()`, `_apply_default_parameters()`, `_write_changecards()`

**Error Handling:**
- Use custom `KRCValidationError` for validation failures
- Standard exceptions: `ValueError`, `RuntimeError`, `FileNotFoundError`
- Error messages must include:
  - What went wrong
  - Why it's a problem
  - Suggestion for how to fix it
- Example: `f"Unknown material: {material}. Available: {list(MATERIAL_DATABASE.keys())}"`

### 4. The Changecard System

**Understanding Changecards:**
- KRC input files use **punch card convention** from Fortran era
- Header contains master.inp defaults (placeholders)
- Actual runtime values specified via **changecards** at end of file
- Three changecard types:
  - Type 1: Float values (format: `1 <index> <value> '<name>' /`)
  - Type 2: Integer values (format: `2 <index> <value> '<name>' /`)
  - Type 3: Boolean values (format: `3 <index> T|F '<name>' /`)

**Changecard Logic:**
Write a changecard if ANY of these conditions are true:
1. Value differs from master.inp header default
2. Parameter in `porb_touched_params` set (PORB "touched" it)
3. Parameter explicitly set by user

**PORB-Touched Parameters:**
- When PORB (orbital parameter) files are used, they set certain parameters
- Even if PORB-derived value matches master.inp default, **still write changecard**
- Track in `porb_touched_params` set
- Examples: `EMISS`, `PORB`, `ALBEDO`, `INERTIA` when derived from orbital files

### 5. Fortran Interop Patterns

**Input File Formatting:**
- **Exact formatting required** - Fortran reads fixed-width columns
- Use pre-formatted strings from `MASTER_INP_FORMATS` dict in `defaults.py`
- G15.7 format for scientific notation floats
- Column alignment must match Davinci output exactly
- Booleans: "T"/"F" strings (not "True"/"False")

**Binary Output Parsing:**
- KRC outputs in custom bin52 format
- Handle both little-endian and big-endian
- Architecture string limited to 5 characters (workaround: truncate "x86_64" to "x86_6")
- Use `struct` module for unpacking binary data
- Parse metadata header for dimensions and data types

**Fortran Logical Values:**
- Fortran: "T" or "F"
- Python: `True` or `False`
- Always convert at I/O boundaries

---

## Development Workflow

### Git Management

**IMPORTANT: Claude does not perform git operations.**
- The user handles all git commands (commit, push, branch, merge, etc.)
- Claude can suggest git workflows or recommend when to commit
- Claude can help write commit messages or PR descriptions
- But the user must execute all git commands themselves

**Branch Naming Convention:**
- Pattern: `<lastname>-<feature>`
- Example: `haberle-python`
- Main branch for PRs: `main`

**Commit Message Style:**
- Use present tense describing what the commit does
- Include context about what's being debugged/implemented
- Multi-sentence descriptions are encouraged
- Indicate current state and next steps
- Example: "Debugging the input file generation logic and refactoring core.py"

### Testing Strategy

**Test File Organization:**
- Pattern: `test_*.py` in `krc_python/tests/`
- Specialized test suites:
  - `test_davinci_parity.py`: Byte-exact comparison with Davinci
  - `test_integration_parity.py`: End-to-end integration tests
  - `test_numerical.py`: Numerical algorithm validation
  - `test_materials.py`: Material property calculations

**Testing Philosophy and Success Criteria:**

**PRIMARY SUCCESS METRIC: Identical Input File Generation**
- The **single most important measure of success** is producing an identical KRC input file compared to Davinci
- Input files must match byte-for-byte (or line-for-line with identical formatting)
- This ensures Fortran KRC receives exactly the same instructions as Davinci would provide
- Test input file parity BEFORE testing outputs

**SECONDARY SUCCESS METRIC: Output Temperature Array Comparison**
- After input file parity is confirmed, validate output surface temperatures
- Temperature arrays must match in:
  - **Array size** (same number of elements/timesteps)
  - **Element-wise values** (nearly identical)
- Allow for minor variability due to precision/rounding differences
- Comparison approach:
  - Absolute element-wise comparison with small tolerance (e.g., 0.01 K)
  - Arrays should be "nearly identical" not just "similar trends"

**General Testing Rules:**
- Code is not "done" until it passes BOTH input file parity AND output comparison
- Test both unit functionality AND integration with Fortran
- Edge cases must be explicitly tested
- Use descriptive test names: `test_high_inertia_low_albedo()`

**Assertion Style:**
```python
# Direct assertions
assert condition, "Descriptive error message"

# Range checks
assert 25 <= N1 <= 100, f"N1 out of range: {N1}"

# Type checks
assert isinstance(result, dict)

# PRIMARY TEST: Input file comparison (exact match required)
with open('pykrc_input.txt') as f1, open('davinci_input.txt') as f2:
    pykrc_lines = f1.readlines()
    davinci_lines = f2.readlines()
    assert len(pykrc_lines) == len(davinci_lines), "Input files have different line counts"
    for i, (line1, line2) in enumerate(zip(pykrc_lines, davinci_lines)):
        assert line1 == line2, f"Input file mismatch at line {i+1}"

# SECONDARY TEST: Temperature array comparison (nearly identical with tolerance)
import numpy as np
pykrc_temps = result['surf']  # Surface temperature array
davinci_temps = reference['surf']
assert len(pykrc_temps) == len(davinci_temps), "Temperature arrays have different sizes"
assert np.allclose(pykrc_temps, davinci_temps, atol=0.01, rtol=1e-5), \
    f"Temperature arrays differ beyond tolerance (max diff: {np.max(np.abs(pykrc_temps - davinci_temps))} K)"
```

---

## Common Patterns

### Parameter Handling

```python
def process_parameters(user_params: Dict[str, Any]) -> Dict[str, Any]:
    """
    Process user parameters with proper precedence.

    1. Start with USER_DEFAULTS
    2. Apply PORB-derived values (if applicable)
    3. Override with user-specified values
    4. Track what PORB "touched" for changecard logic
    """
    master_params = USER_DEFAULTS.copy()
    porb_touched_params = set()

    # Apply PORB values if body specified
    if 'body' in user_params:
        porb_params, touched = get_porb_params(user_params['body'])
        master_params.update(porb_params)
        porb_touched_params.update(touched)

    # User params override everything
    master_params.update(user_params)

    return master_params, porb_touched_params
```

### Validation Pattern

```python
def validate_thermal_inertia(inertia: float) -> None:
    """
    Validate thermal inertia value.

    Raises
    ------
    KRCValidationError
        If inertia is outside valid range
    """
    if not 5 <= inertia <= 2500:
        raise KRCValidationError(
            f"Thermal inertia {inertia} outside valid range [5, 2500]. "
            f"Typical values: Mars dust ~50, Mars rock ~400, Moon ~50"
        )
```

### Helper Function Extraction

```python
def krc_complex_operation(params: Dict[str, Any]) -> Dict[str, Any]:
    """Public interface with complex logic."""
    # Extract to private helpers
    user_params = _extract_user_params(params)
    defaults = _apply_default_parameters(user_params)
    validated = _validate_all_parameters(defaults)
    return _execute_krc(validated)

def _extract_user_params(params: Dict[str, Any]) -> Dict[str, Any]:
    """Helper: Extract and normalize user parameters."""
    # Implementation
    pass
```

---

## Common Pitfalls to Avoid

### 1. Don't Break Davinci Parity
- **Never** change formatting without verifying against Davinci
- Test ALL changes with `test_davinci_parity.py`
- Even "improvements" can break Fortran parsing

### 2. Don't Hardcode Defaults
```python
# BAD
def calculate_something(albedo=0.15):
    ...

# GOOD
from pykrc.defaults import USER_DEFAULTS

def calculate_something(albedo: Optional[float] = None):
    if albedo is None:
        albedo = USER_DEFAULTS['ALBEDO']
    ...
```

### 3. Don't Forget Changecard Logic
```python
# BAD - Only writes if value differs from default
if value != MASTER_INP_HEADER_DEFAULTS[param]:
    write_changecard(param, value)

# GOOD - Checks PORB-touched and user-specified too
if (value != MASTER_INP_HEADER_DEFAULTS[param] or
    param in porb_touched_params or
    param in user_specified_params):
    write_changecard(param, value)
```

### 4. Don't Mix Module Responsibilities
```python
# BAD - Materials calculation in core.py
def krc():
    conductivity = inertia**2 / (density * heat_capacity)  # Should be in materials.py
    ...

# GOOD - Import from appropriate module
from pykrc.materials import calculate_conductivity

def krc():
    conductivity = calculate_conductivity(inertia, density, heat_capacity)
    ...
```

### 5. Don't Ignore Type Hints
```python
# BAD
def process_data(data):
    return data['result']

# GOOD
def process_data(data: Dict[str, Any]) -> float:
    return data['result']
```

---

## Documentation Standards

### Creating .md Documentation Files

**IMPORTANT: Claude does not create .md files without permission.**
- **Always ask before creating** any markdown documentation files
- Documentation files should only be created:
  - After large plans are executed and completed
  - At the user's explicit request or discretion
  - When summarizing significant work or analysis
- Claude can recommend creating documentation but must wait for user approval
- Exception: Adding to existing documentation is fine if requested

**When Claude might suggest documentation:**
- After completing major refactoring
- When analysis reveals important findings (like code cleanup analysis)
- To document architectural decisions
- To capture lessons learned from debugging sessions

### Code Documentation (in-code comments and docstrings)

**Always document:**
- All public functions and classes
- Complex algorithms or calculations
- Deviations from expected behavior
- Fortran interop quirks
- Parameter units and valid ranges

**Use inline comments for:**
- Non-obvious logic
- References to Davinci source lines
- Explanations of "why" not "what"
- Workarounds for Fortran limitations

### Documentation Examples

**Good inline comment:**
```python
# Per Davinci krc.dvrc line 231, always write 0 0 for these params
# regardless of actual values. This is a Fortran quirk.
f.write(f"{0:7.2f} {0:7.2f}\n")
```

**Bad inline comment:**
```python
# Write zeros to file
f.write(f"{0:7.2f} {0:7.2f}\n")
```

**Good parameter documentation:**
```python
Parameters
----------
thermal_inertia : float
    Thermal inertia in SI units (J m⁻² K⁻¹ s⁻½).
    Typical values: Mars dust ~50, Mars rock ~400, Moon ~50.
    Valid range: 5 to 2500.
```

**Bad parameter documentation:**
```python
Parameters
----------
thermal_inertia : float
    The thermal inertia value
```

---

## File Structure Reference

### Key Python Modules

```
krc_python/
├── pykrc/
│   ├── core.py              # Main krc() function (user interface)
│   ├── executor.py          # Fortran execution & input generation
│   ├── materials.py         # Material property calculations
│   ├── numerical.py         # N1, N2, TAUD calculations
│   ├── defaults.py          # ALL defaults (single source of truth)
│   ├── interface_validator.py  # Validation framework
│   ├── bin_parser.py        # Binary output parsing
│   ├── orbital.py           # PORB/orbital parameters
│   ├── layers.py            # Multi-layer regolith
│   ├── frost.py             # Frost/ice calculations
│   ├── validation.py        # Parameter validation
│   ├── ancillary.py         # Ancillary data (TES, MOLA, etc.)
│   └── data_loaders.py      # HDF5/data file loading
├── tests/
│   ├── test_davinci_parity.py      # Critical: Davinci comparison
│   ├── test_integration_parity.py  # End-to-end tests
│   ├── test_numerical.py           # Numerical algorithms
│   ├── test_materials.py           # Material properties
│   └── test_*.py                   # Other unit tests
├── docs/                    # Documentation (needs cleanup - see CODE_CLEANUP_ANALYSIS)
└── examples/                # Usage examples
```

---

## Backward Compatibility Policy

**IMPORTANT: We are NOT prioritizing backward compatibility at this time.**

- This is the **first version** of pykrc software
- The software **has not been shipped yet**
- Breaking changes are acceptable and expected during development
- Focus on correctness and Davinci parity over API stability
- Once we ship v1.0, backward compatibility will become important

**What this means for development:**
- Refactor freely to improve code quality
- Change APIs if needed to match testing philosophy
- Remove deprecated patterns without migration paths
- Prioritize doing things right over preserving old interfaces

## When to Ask for Clarification

Ask the user before:
1. **Breaking Davinci parity** - even if it seems like an improvement
2. **Major refactoring** - get buy-in on approach first
3. **Removing "unused" code** - it might be intended for future public API
4. **Modifying defaults** - defaults affect all users
5. **Changing test expectations** - tests encode important behaviors

Don't ask about:
1. Fixing obvious bugs (but do test thoroughly)
2. Adding docstrings to undocumented code
3. Extracting helper functions (if it improves readability)
4. Adding type hints (always good)
5. Following existing conventions (that's expected)

---

## Useful Commands

```bash
# Run tests
pytest krc_python/tests/

# Run specific test suite
pytest krc_python/tests/test_davinci_parity.py -v

# Run numerical tests
pytest krc_python/tests/test_numerical.py -v

# Check for broken imports (from cleanup analysis)
python -c "from pykrc import bin5_reader"  # This will fail - module doesn't exist
```

**Git commands (user executes these, not Claude):**
```bash
# Check status and recent changes
git status
git diff krc_python/pykrc/
git log --oneline -5

# Branch management
git checkout -b lastname-feature
git branch -a

# Committing (user does this manually)
git add <files>
git commit -m "Your message here"
git push origin <branch>
```

---

## Project-Specific Knowledge

### Bodies Supported
- Mars (most common, most tested)
- Moon
- Phobos
- Deimos
- Europa
- Generic/custom bodies (via orbital parameters)

### Material Database
Located in `defaults.py` as `MATERIAL_DATABASE`
- Pre-defined materials: "dust", "rock", "ice", etc.
- Each has: density, heat capacity, conductivity
- Used for k_style material models

### K-Style Options
Three approaches to material properties:
1. **"Mars"** - Mars-specific empirical relationships
2. **"Moon"** - Moon-specific relationships
3. **"Bulk"** - Simple bulk thermal properties

### Important Parameters (always UPPERCASE)
- `ALBEDO` - Surface albedo (0-1)
- `INERTIA` - Thermal inertia (SI units)
- `EMISS` - Infrared emissivity (0-1)
- `N1` - Number of model layers
- `N2` - Skin depth layers
- `TDEEP` - Deep temperature (K)
- `TAUD` - Optical depth (atmosphere)
- `PORB` - Orbital parameters flag

---

## Questions to Ask Yourself Before Coding

1. **Does this produce identical input files? (PRIMARY GOAL)**
   - Does my input file match Davinci's byte-for-byte or line-for-line?
   - Have I tested input file parity FIRST before checking outputs?
   - Will Fortran correctly parse my output formatting?
   - Have I verified all changecards are written correctly?

2. **Do the temperature outputs match? (SECONDARY GOAL)**
   - Are the temperature array sizes identical?
   - Are element-wise values nearly identical (within tolerance)?
   - Have I checked for precision/rounding issues?

3. **Does this maintain Davinci parity in general?**
   - Have I checked the Davinci source?
   - Have I tested with `test_davinci_parity.py`?
   - Are Davinci source line references in my comments?

4. **Are defaults handled correctly?**
   - Am I using `defaults.py`?
   - Is the three-level precedence correct?
   - Are changecards written for PORB-touched params?

5. **Is my code documented?**
   - NumPy-style docstring?
   - Type hints on all parameters?
   - Units specified?
   - Davinci references in comments?

6. **Is this the right module?**
   - Am I mixing concerns?
   - Should this be a helper function?
   - Is there an existing utility I should use?

7. **Have I tested edge cases?**
   - High/low parameter values?
   - Missing optional parameters?
   - Different bodies/materials?
   - Integration with Fortran?

---

## Summary: Golden Rules

1. **PRIMARY GOAL: Identical input file generation** (test this FIRST)
2. **SECONDARY GOAL: Nearly identical temperature array outputs** (test this SECOND)
3. **Davinci parity is non-negotiable**
4. **All defaults come from `defaults.py`**
5. **Use NumPy-style docstrings with type hints**
6. **KRC parameters are UPPERCASE**
7. **One responsibility per module**
8. **Extract helpers for complex logic**
9. **Reference Davinci source lines in comments**
10. **Changecard logic includes PORB-touched params**
11. **Claude does not create .md files or perform git operations without permission**
12. **When in doubt, check Davinci first**

---

**End of Guidelines**

For questions about this codebase, consult:
- This CLAUDE.md file
- [CODE_CLEANUP_ANALYSIS_2025-10-19.md](krc_python/CODE_CLEANUP_ANALYSIS_2025-10-19.md)
- Davinci source code (reference implementation)
- [docs/](krc_python/docs/) directory (but note: needs consolidation)
