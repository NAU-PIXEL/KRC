# Implementation Plan: Use Pre-Formatted PORB Strings

**Date:** 2025-10-14
**Goal:** Eliminate round-trip conversion by using pre-formatted PORB text directly from HDF files
**Benefit:** Guaranteed exact parity with davinci, eliminates G15.7 formatting complexity

---

## Executive Summary

**Current approach:**
```
HDF → parse text → numeric values → re-format → output
      (risky)                        (complex)
```

**New approach:**
```
HDF → store raw text → output verbatim
      (safe)           (simple)
```

**Fallback for custom bodies:**
```
Generic PORB → calculate numeric → format G15.7 → output
               (only when needed)
```

---

## 1. Architecture Changes

### 1.1 Data Flow Comparison

**Before:**
```python
# orbital.py
porb_params = []
for line in lines[1:7]:
    values = line.split()
    for val in values:
        porb_params.append(float(val))  # Parse to numeric
# Store: porb_params = [104.0, 0.1, ...]

# executor.py
for val in porb_params:
    formatted = format_g15_7(val)  # Re-format
    f.write(formatted)
```

**After:**
```python
# orbital.py
porb_text_lines = lines[1:7]  # Keep as text
# Store: porb_text_lines = [
#   "   104.0000      0.1000000...",
#   "  0.9340198E-01   1.523712...",
#   ...
# ]

# executor.py
for line in porb_text_lines:
    f.write(line + '\n')  # Write verbatim
```

### 1.2 Backward Compatibility

**Must support both:**
1. **Pre-formatted text** (from HDF cache) - preferred
2. **Numeric values** (from generic_porb or calculations) - fallback

**Strategy:**
```python
if hasattr(orbital_elem, 'porb_text_lines'):
    # Use pre-formatted text (davinci parity guaranteed)
    write_preformatted_porb(f, orbital_elem.porb_text_lines)
else:
    # Fallback to formatting numeric values (generic bodies)
    write_formatted_porb(f, orbital_elem.porb_params)
```

---

## 2. Code Changes

### 2.1 orbital.py: load_body_parameters()

**Location:** Lines 98-180

**Current code:**
```python
# Read the 'rot' field (pre-formatted PORB text)
if 'rot' in f:
    rot_text = f['rot'][0].decode('utf-8')
    lines = rot_text.strip().split('\n')

    if lines:
        # First line is the PORB header
        params['PORB_HEADER'] = lines[0]

        # Next 6 lines contain 30 float values (5 per line)
        porb_params = []
        for line in lines[1:7]:
            values = line.split()
            for val in values:
                try:
                    porb_params.append(float(val))
                except ValueError:
                    pass

        if len(porb_params) == 30:
            params['PORB_PARAMS'] = porb_params
```

**New code:**
```python
# Read the 'rot' field (pre-formatted PORB text)
if 'rot' in f:
    rot_text = f['rot'][0].decode('utf-8')
    lines = rot_text.strip().split('\n')

    if lines:
        # First line is the PORB header
        params['PORB_HEADER'] = lines[0]

        # Store pre-formatted text lines (6 lines)
        if len(lines) >= 7:
            params['PORB_TEXT_LINES'] = lines[1:7]

        # OPTIONAL: Also parse to numeric for compatibility
        # (Can be removed once fully migrated)
        porb_params = []
        for line in lines[1:7]:
            values = line.split()
            for val in values:
                try:
                    porb_params.append(float(val))
                except ValueError:
                    pass

        if len(porb_params) == 30:
            params['PORB_PARAMS'] = porb_params  # Keep for now
```

**Changes:**
- ✓ Add `params['PORB_TEXT_LINES'] = lines[1:7]`
- ✓ Keep `PORB_PARAMS` for backward compatibility (can remove later)

### 2.2 orbital.py: porb()

**Location:** Lines 183-262

**Current code:**
```python
# Extract PORB data (these don't go into OrbitalElements)
porb_header = params.pop('PORB_HEADER', None)
porb_params = params.pop('PORB_PARAMS', None)

# ...

# Store PORB data as attributes
if porb_header:
    orbital_elem.porb_header = porb_header
if porb_params:
    orbital_elem.porb_params = porb_params
```

**New code:**
```python
# Extract PORB data (these don't go into OrbitalElements)
porb_header = params.pop('PORB_HEADER', None)
porb_text_lines = params.pop('PORB_TEXT_LINES', None)
porb_params = params.pop('PORB_PARAMS', None)  # Keep for compatibility

# ...

# Store PORB data as attributes
if porb_header:
    orbital_elem.porb_header = porb_header
if porb_text_lines:
    orbital_elem.porb_text_lines = porb_text_lines  # NEW
if porb_params:
    orbital_elem.porb_params = porb_params  # Keep for fallback
```

**Changes:**
- ✓ Add `porb_text_lines` extraction
- ✓ Attach as `orbital_elem.porb_text_lines`

### 2.3 porb_handler.py: setup_orbital_parameters()

**Location:** Lines 220-225

**Current code:**
```python
# Add PORB data for output
if hasattr(body_params, 'porb_header'):
    porb_params['PORB_HEADER'] = body_params.porb_header
if hasattr(body_params, 'porb_params'):
    porb_params['PORB_PARAMS'] = body_params.porb_params
```

**New code:**
```python
# Add PORB data for output
if hasattr(body_params, 'porb_header'):
    porb_params['PORB_HEADER'] = body_params.porb_header
if hasattr(body_params, 'porb_text_lines'):
    porb_params['PORB_TEXT_LINES'] = body_params.porb_text_lines  # NEW (preferred)
if hasattr(body_params, 'porb_params'):
    porb_params['PORB_PARAMS'] = body_params.porb_params  # Keep for fallback
```

**Changes:**
- ✓ Pass through `PORB_TEXT_LINES`

### 2.4 executor.py: _write_input_file()

**Location:** Lines 217-252

**Current code:**
```python
# Write PORB parameters (30 values in 6 lines of 5G15.7 each)
# Format: 5 values per line, each in a 15-character field
# Fortran G format: uses E notation for |val| < 0.1 or |val| >= 1e8
# Otherwise uses F format with 7 significant figures
porb_params = params.get('PORB_PARAMS')
if porb_params is not None:
    for i in range(0, len(porb_params), 5):
        chunk = porb_params[i:i+5]
        formatted_vals = []
        for val in chunk:
            abs_val = abs(val)
            # [... formatting logic ...]
            formatted_vals.append(formatted)
        line = ''.join(formatted_vals) + '    '
        f.write(line + '\n')
```

**New code:**
```python
# Write PORB parameters
# Prefer pre-formatted text (exact davinci parity)
porb_text_lines = params.get('PORB_TEXT_LINES')
if porb_text_lines is not None:
    # Use pre-formatted text from HDF (guaranteed exact match)
    for line in porb_text_lines:
        f.write(line + '\n')
else:
    # Fallback: format numeric values (for generic/custom bodies)
    porb_params = params.get('PORB_PARAMS')
    if porb_params is not None:
        for i in range(0, len(porb_params), 5):
            chunk = porb_params[i:i+5]
            formatted_vals = []
            for val in chunk:
                formatted_vals.append(self._format_g15_7(val))
            line = ''.join(formatted_vals) + '    '
            f.write(line + '\n')
```

**Changes:**
- ✓ Check for `PORB_TEXT_LINES` first
- ✓ Write verbatim if available
- ✓ Fallback to formatting if not

### 2.5 executor.py: New Helper Method

**Add new method:**
```python
def _format_g15_7(self, val: float) -> str:
    """
    Format a value using Fortran G15.7 format.

    Only used for generic/custom bodies without pre-formatted PORB text.
    For cached bodies, pre-formatted text is used directly.

    G15.7 format:
    - 15 character field width
    - 7 significant figures
    - Auto-switches between F and E notation

    Args:
        val: Float value to format

    Returns:
        15-character formatted string
    """
    abs_val = abs(val)

    # E format: |val| < 0.1 or |val| >= 1e8
    if (abs_val < 0.1 and abs_val > 0) or abs_val >= 1e8:
        return f"{val:15.7E}"

    # F format: 0.1 <= |val| < 1e8 or val == 0
    else:
        # Determine decimal places for ~7 significant figures
        if abs_val < 1:
            decimals = 7
        elif abs_val < 10:
            decimals = 6
        elif abs_val < 100:
            decimals = 5
        elif abs_val < 1000:
            decimals = 4
        elif abs_val < 10000:
            decimals = 3
        elif abs_val < 100000:
            decimals = 2
        elif abs_val < 1000000:
            decimals = 1
        else:  # 1e6 <= abs_val < 1e8
            decimals = 0

        return f"{val:15.{decimals}f}"
```

**Changes:**
- ✓ Extract formatting logic to separate method
- ✓ Fix missing decimal cases (10k-100M ranges)
- ✓ Add clear documentation

---

## 3. Testing Strategy

### 3.1 Test Pre-Formatted Path

**Test:** Verify HDF text is written verbatim

```python
def test_porb_preformatted_verbatim():
    """Verify pre-formatted PORB text is written unchanged."""
    from pykrc.data_loaders import KRCDataLoader
    from pykrc.config import get_paths
    import h5py

    paths = get_paths()
    loader = KRCDataLoader(paths.support_dir)

    # Load Mars PORB
    porb_file = paths.support_dir / "porb_defaults" / "Mars.porb.hdf"

    # Read original text
    with h5py.File(porb_file, 'r') as f:
        rot_text = f['rot'][0].decode('utf-8')
        original_lines = rot_text.strip().split('\n')[1:7]

    # Run PyKRC
    from pykrc import krc
    result = krc(lat=0, lon=0, body="Mars", DELLS=1.0)

    # Read generated input file
    with open(result.input_file_path) as f:
        lines = f.readlines()

    # Extract PORB section (lines 36-41 in standard format)
    porb_section = [line.rstrip('\n') for line in lines[36:42]]

    # Compare character-by-character
    for i, (orig, generated) in enumerate(zip(original_lines, porb_section)):
        assert orig == generated, (
            f"Line {i+1} differs:\n"
            f"  Original:  '{orig}'\n"
            f"  Generated: '{generated}'"
        )
```

### 3.2 Test Fallback Path

**Test:** Verify G15.7 formatting for generic bodies

```python
def test_porb_generic_formatting():
    """Verify G15.7 formatting for generic bodies."""
    from pykrc.executor import KRCExecutor

    executor = KRCExecutor()

    # Test cases from master.inp
    test_cases = [
        (104.0,        "       104.0000"),
        (0.1,          "      0.1000000"),
        (0.8644665,    "      0.8644665"),
        (0.03226901,   " 0.3226901E-01"),
        (-1.281586,    "      -1.281586"),
        (0.09340198,   " 0.9340198E-01"),
        (1.523712,     "       1.523712"),
        (0.4090926,    "      0.4090926"),
        (0.0,          "       0.000000"),
        (686.9929,     "       686.9929"),
        (3397.977,     "       3397.977"),
        (24.62296,     "       24.62296"),
        # Large value tests
        (12345.67,     "    12345.670"),
        (123456.7,     "     123456.70"),
        (1234567.0,    "    1234567.0"),
        (12345678.0,   "     12345678"),
    ]

    for val, expected in test_cases:
        result = executor._format_g15_7(val)
        assert result == expected, (
            f"Value {val}: expected '{expected}', got '{result}'"
        )
        assert len(result) == 15, (
            f"Value {val}: width is {len(result)}, expected 15"
        )
```

### 3.3 Test All Cached Bodies

**Test:** Verify all HDF cache files work

```python
def test_all_cached_bodies():
    """Test PORB loading for all cached bodies."""
    from pykrc.config import get_paths
    from pykrc import krc

    paths = get_paths()
    porb_dir = paths.support_dir / "porb_defaults"

    # Get all cached bodies
    cached_bodies = [f.stem for f in porb_dir.glob("*.porb.hdf")]

    for body in cached_bodies:
        print(f"Testing {body}...")

        # This should not raise an error
        result = krc(lat=0, lon=0, body=body, DELLS=8.0)

        # Verify PORB section exists in input file
        with open(result.input_file_path) as f:
            lines = f.readlines()

        # Check PORB header (line 35)
        assert "RUNTIME" in lines[35]
        assert body in lines[35]

        # Check PORB values (lines 36-41)
        for i in range(36, 42):
            line = lines[i].rstrip('\n')
            assert len(line) == 79, f"{body} line {i}: wrong length {len(line)}"

        print(f"  ✓ {body} passed")
```

---

## 4. Migration Plan

### Phase 1: Add Pre-Formatted Support (Immediate)

**Changes:**
1. ✓ Modify `load_body_parameters()` to store `PORB_TEXT_LINES`
2. ✓ Modify `porb()` to attach `porb_text_lines` attribute
3. ✓ Modify `setup_orbital_parameters()` to pass through
4. ✓ Modify `_write_input_file()` to use pre-formatted text
5. ✓ Keep `PORB_PARAMS` for backward compatibility

**Testing:**
- Run all cached bodies tests
- Character-by-character comparison with davinci
- Verify Mars, Earth, Phobos at minimum

**Result:** Cached bodies get exact davinci parity

### Phase 2: Improve G15.7 Fallback (Soon)

**Changes:**
1. ✓ Extract `_format_g15_7()` method
2. ✓ Fix missing decimal cases
3. ✓ Add comprehensive tests

**Testing:**
- Test all edge cases
- Test generic_porb() bodies
- Compare with Fortran G format if possible

**Result:** Generic bodies also work correctly

### Phase 3: Cleanup (Later)

**Changes:**
1. ? Remove `PORB_PARAMS` numeric parsing (if not needed)
2. ? Remove old formatting code (if fully replaced)
3. ? Document final architecture

**Testing:**
- Full regression test suite
- Verify no broken functionality

**Result:** Clean, maintainable code

---

## 5. Benefits

### 5.1 Guaranteed Parity

**Before:** Risk of formatting differences
```
HDF text → parse → re-format → might differ
```

**After:** Exact match guaranteed
```
HDF text → copy → exact match ✓
```

### 5.2 Simpler Code

**Before:** Complex G15.7 logic
```python
# 35+ lines of formatting logic
if abs_val < 0.1:
    formatted = f"{val:15.7E}"
elif abs_val < 1:
    decimals = 7
elif abs_val < 10:
    decimals = 6
# ... etc
```

**After:** Simple text copy
```python
# 3 lines
for line in porb_text_lines:
    f.write(line + '\n')
```

### 5.3 Performance

**Before:** Parse + re-format (slower)
```python
for val_str in line.split():
    val = float(val_str)      # Parse
    formatted = format(val)   # Re-format
```

**After:** Direct write (faster)
```python
f.write(line + '\n')  # Just write
```

### 5.4 Robustness

**Before:** Multiple failure points
- Parsing errors
- Floating-point precision loss
- Format string bugs
- Python vs Fortran differences

**After:** Single source of truth
- Use exact text from HDF
- No conversion needed
- No rounding errors
- Perfect fidelity

---

## 6. Edge Cases

### 6.1 Generic Bodies (No HDF)

**Scenario:** User provides custom orbital parameters

```python
from pykrc import krc, generic_porb

custom_body = generic_porb(
    name="MyPlanet",
    rotation_period=2.5,
    obliquity=15.0,
    ...
)

result = krc(lat=0, lon=0, body=custom_body, ...)
```

**Handling:**
- `generic_porb()` calculates numeric PORB values
- No `PORB_TEXT_LINES` available
- Falls back to `_format_g15_7()`
- Still works, just not guaranteed exact davinci match

### 6.2 Modified Epoch

**Scenario:** User wants different epoch

```python
result = krc(lat=0, lon=0, body="Mars", epoch=0.20, ...)
```

**Current PyKRC:** Doesn't support custom epoch (uses HDF cache)

**Handling:**
- If epoch != 0.10 (default), would need to run porbmn
- PyKRC doesn't implement porbmn execution
- Could: Raise error for custom epochs
- Or: Document limitation

### 6.3 Corrupted HDF

**Scenario:** HDF file missing 'rot' field

**Handling:**
```python
if 'rot' in f:
    # Use pre-formatted
    params['PORB_TEXT_LINES'] = lines[1:7]
else:
    # Fallback: calculate numeric values
    # (Current code doesn't do this - would need implementation)
    raise ValueError(f"HDF file missing 'rot' field: {porb_file}")
```

---

## 7. Documentation Updates

### 7.1 Code Comments

**Add to `load_body_parameters()`:**
```python
def load_body_parameters(body_name: str, data_loader=None) -> Dict[str, Any]:
    """
    Load orbital parameters for a celestial body from HDF files.

    For cached bodies, returns pre-formatted PORB text from the HDF 'rot' field.
    This ensures exact parity with davinci's PORB output, eliminating the need
    for complex G15.7 formatting logic and avoiding floating-point conversion errors.

    Returns
    -------
    dict
        Dictionary with keys:
        - PORB_HEADER: str - First line of PORB output
        - PORB_TEXT_LINES: list[str] - 6 pre-formatted PORB value lines
        - PORB_PARAMS: list[float] - 30 numeric values (for compatibility)
        - rotation_period: float - Rotation period in days
        - orbital_period: float - Orbital period in days
        - krc_params: dict - KRC-specific parameters (PTOTAL, GRAV, etc.)
    """
```

### 7.2 User Documentation

**Add to README or usage guide:**

```markdown
## PORB Data Sources

PyKRC uses pre-computed orbital parameters from cached HDF files
for common bodies (Mars, Earth, Moon, etc.). These files contain
pre-formatted PORB text that is written directly to KRC input files,
ensuring exact parity with the davinci KRC interface.

For custom or generic bodies, PyKRC calculates PORB values and formats
them using Fortran G15.7 format emulation.

### Supported Bodies with Cached PORB

- Planets: Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto
- Satellites: Moon, Phobos, Deimos, Io, Europa, Ganymede, Callisto
- Asteroids: Bennu, Ceres, Vesta, Pallas, Juno, Dinkinesh
- Comets: 1P-Halley, 9P-Tempel_1, etc.

### Custom Bodies

For bodies not in the cache, use `generic_porb()` to define custom
orbital parameters. Note: PORB formatting may differ slightly from
davinci for custom bodies.
```

---

## 8. Implementation Checklist

### Code Changes
- [ ] Modify `orbital.py:load_body_parameters()` to store `PORB_TEXT_LINES`
- [ ] Modify `orbital.py:porb()` to attach `porb_text_lines` attribute
- [ ] Modify `porb_handler.py:setup_orbital_parameters()` to pass through
- [ ] Modify `executor.py:_write_input_file()` to prefer pre-formatted text
- [ ] Add `executor.py:_format_g15_7()` method with fixed decimal logic
- [ ] Update comments and docstrings

### Testing
- [ ] Test pre-formatted path works (Mars)
- [ ] Test character-by-character match with davinci
- [ ] Test all 30 cached bodies
- [ ] Test G15.7 formatting with edge cases
- [ ] Test generic body fallback
- [ ] Add regression tests

### Documentation
- [ ] Update code comments
- [ ] Update docstrings
- [ ] Update user documentation
- [ ] Document limitations (custom epoch, etc.)

### Validation
- [ ] Run full test suite
- [ ] Compare with davinci output
- [ ] Verify KRC runs successfully
- [ ] Check output matches davinci results

---

## 9. Summary

**Recommendation:** Implement this change immediately.

**Effort:** Low (2-3 hours of coding + testing)

**Risk:** Very low (backward compatible, fallback maintained)

**Benefit:** High (guaranteed parity, simpler code, more robust)

**Next Steps:**
1. Implement Phase 1 changes
2. Test with Mars
3. Test with all cached bodies
4. Commit and document

---

**END OF DOCUMENT**
