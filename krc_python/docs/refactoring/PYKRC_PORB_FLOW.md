# PyKRC PORB Data Flow Analysis

**Date:** 2025-10-14
**Purpose:** Document exactly how PyKRC obtains and uses PORB orbital parameters

---

## Executive Summary

**YES, PyKRC accesses the `porb_defaults/*.porb.hdf` files!**

PyKRC loads the **same HDF cache files** that davinci uses, but it:
1. **Reads the pre-formatted text** from `porb.rot`
2. **Parses it back to numeric values**
3. **Re-formats using Python G15.7 implementation**

**This is the critical validation point:** PyKRC must format the numeric values to produce **exactly the same text** that was originally in the HDF file.

---

## 1. Complete PORB Data Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                 User calls krc(body="Mars", ...)                │
└────────────────────────┬────────────────────────────────────────┘
                         │
                         ▼
           core.py: run_krc_model()
                         │
                         ▼
    porb_handler.py: setup_orbital_parameters()
                         │
                         ▼
         orbital.py: porb(body, data_loader)
                         │
                         ▼
    orbital.py: load_body_parameters(body_name, data_loader)
                         │
    ┌────────────────────┴────────────────────┐
    │                                         │
    ▼                                         ▼
HDF file exists?                      No HDF file
    │                                         │
    YES                                       ▼
    │                                  BODY_DEFAULTS
    ▼                                  (hardcoded)
Read from HDF:                               │
$SUPPORT_DIR/porb_defaults/                  │
    Mars.porb.hdf                             │
    │                                         │
    ├─ Read 'rot' field ────────────┐        │
    │  (pre-formatted text)          │        │
    │  Lines 0: PORB header          │        │
    │  Lines 1-6: 30 PORB values     │        │
    │                                │        │
    │  Parse back to floats: ────────┤        │
    │    [104.0, 0.1, 0.8644665,     │        │
    │     0.03226901, -1.281586,     │        │
    │     ... 30 values total]       │        │
    │                                │        │
    ├─ Read 'krc' group ────────────┤        │
    │    PTOTAL, GRAV, TAURAT,       │        │
    │    DUSTA, ARC2_G0, etc.        │        │
    │                                │        │
    ├─ Read 'rot_per' (hours) ──────┤        │
    │    Convert to days             │        │
    │                                │        │
    └─ Read 'period' (days) ────────┤        │
                                    │        │
                    ┌───────────────┴────────┘
                    │
                    ▼
        Return params dict:
         - PORB_HEADER: str (original text)
         - PORB_PARAMS: list[float] (30 numeric values)
         - rotation_period: float (days)
         - orbital_period: float (days)
         - krc_params: dict (PTOTAL, GRAV, etc.)
                    │
                    ▼
         orbital.py: porb()
         Creates OrbitalElements dataclass
         Attaches PORB data as attributes
                    │
                    ▼
    porb_handler.py: setup_orbital_parameters()
    Extracts PORB data, calculates N24/N5/JDISK
    Returns: (body_params, porb_params, porb_touched)
                    │
                    ▼
            core.py: run_krc_model()
            Merges into params dict
                    │
                    ▼
        executor.py: _write_input_file()
                    │
                    ▼
        Lines 221-252: Write PORB section
            │
            ├─ Line 199: Write PORB_HEADER verbatim
            │    "2013 Jul 24 11:28:09=RUNTIME..."
            │
            └─ Lines 223-252: Format PORB_PARAMS
                for val in PORB_PARAMS:
                    formatted = format_g15_7(val)
                    ↓
                **CRITICAL: Must produce exact same**
                **text as original 'rot' field!**
```

---

## 2. HDF File Structure

### 2.1 File Location

**Path:** `$SUPPORT_DIR/porb_defaults/<body>.porb.hdf`

**Example:** `krc_support/porb_defaults/Mars.porb.hdf`

### 2.2 HDF Structure (from h5py inspection)

```
Mars.porb.hdf
├── rot              # Pre-formatted text (7 lines concatenated)
│   Type: bytes array
│   Shape: (1, 1, 1) - scalar stored as 3D array
│   Content: "2013 Jul 24...\n   104.0000...\n..."
│
├── rot_per          # Rotation period in HOURS
│   Type: float64
│   Shape: (1, 1, 1)
│   Value: 24.66 (for Mars = 1.0275 days)
│
├── period           # Orbital period in DAYS
│   Type: float64
│   Shape: (1, 1, 1)
│   Value: 686.9929 (Mars year)
│
├── krc/             # KRC-specific parameters
│   ├── ARC2_G0      # Photometric parameter
│   ├── DELJUL       # Time step (days/degree Ls)
│   ├── DUSTA        # Dust particle size (μm)
│   ├── GRAV         # Surface gravity (m/s²)
│   ├── N24          # Output timesteps per day
│   ├── PERIOD       # Rotation period (Earth days)
│   ├── PTOTAL       # Atmospheric pressure (Pa)
│   └── TAURAT       # IR/visible opacity ratio
│
└── type/            # Body classification
    ├── body_type    # "Planet", "Satellite", etc.
    ├── id           # Numeric identifier
    ├── name         # Body name
    └── parent_body  # Parent (for satellites)
```

### 2.3 'rot' Field Details

**Storage format:**
- Stored as a **single concatenated string** with newlines
- Encoded as UTF-8 bytes in HDF
- 7 lines total when split by '\n'

**Example (Mars):**
```
Line 0: "2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars"
Line 1: "   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    "
Line 2: "  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    "
Line 3: "   5.544402       0.000000       0.000000       686.9929       3397.977    "
Line 4: "   24.62296       0.000000      -1.240317       0.000000       0.000000    "
Line 5: "   0.000000      0.3244965      0.8559126      0.4026359     -0.9458869    "
Line 6: "  0.2936298      0.1381285       0.000000     -0.4256703      0.9048783   "
```

---

## 3. PyKRC Loading Process

### 3.1 orbital.py: load_body_parameters()

**Location:** [orbital.py:98-180](orbital.py#L98-L180)

```python
def load_body_parameters(body_name: str, data_loader=None) -> Dict[str, Any]:
    """Load orbital parameters from HDF files."""
    import h5py

    params = {}

    # Try to load from porb_defaults directory
    if data_loader:
        porb_file = data_loader.support_dir / "porb_defaults" / f"{body_name}.porb.hdf"

        if porb_file.exists():
            with h5py.File(porb_file, 'r') as f:

                # Read the 'rot' field (pre-formatted PORB text)
                if 'rot' in f:
                    rot_text = f['rot'][0].decode('utf-8')  # Decode bytes to string
                    lines = rot_text.strip().split('\n')    # Split into lines

                    if lines:
                        # First line is the PORB header
                        params['PORB_HEADER'] = lines[0]

                        # Next 6 lines contain 30 float values (5 per line)
                        porb_params = []
                        for line in lines[1:7]:            # Lines 1-6
                            values = line.split()          # Split on whitespace
                            for val in values:
                                try:
                                    porb_params.append(float(val))  # Parse back to float
                                except ValueError:
                                    pass

                        if len(porb_params) == 30:
                            params['PORB_PARAMS'] = porb_params

                # Read KRC-specific parameters
                if 'krc' in f:
                    krc_group = f['krc']
                    for key in krc_group.keys():
                        dataset = krc_group[key]
                        val = dataset[0, 0, 0]             # Extract scalar from 3D array
                        params[key.upper()] = float(val)

                # Read rotation period (convert hours to days)
                if 'rot_per' in f:
                    rot_per_hours = float(f['rot_per'][0, 0, 0])
                    params['rotation_period'] = rot_per_hours / 24.0

                # Read orbital period
                if 'period' in f:
                    params['orbital_period'] = float(f['period'][0, 0, 0])

                return params

    # Fallback to hardcoded defaults if HDF not found
    if body_name in BODY_DEFAULTS:
        return BODY_DEFAULTS[body_name].copy()

    raise ValueError(f"Body '{body_name}' not found in database")
```

### 3.2 Key Steps

**Step 1: Read pre-formatted text**
```python
rot_text = f['rot'][0].decode('utf-8')
# Result: "2013 Jul 24...\n   104.0000...\n..."
```

**Step 2: Split into lines**
```python
lines = rot_text.strip().split('\n')
# Result: ['2013 Jul 24...', '   104.0000...', ...]
```

**Step 3: Extract header**
```python
params['PORB_HEADER'] = lines[0]
# Result: "2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars"
```

**Step 4: Parse values back to floats**
```python
for line in lines[1:7]:
    values = line.split()  # Split on whitespace
    for val in values:
        porb_params.append(float(val))  # "104.0000" → 104.0
```

**Result:** `porb_params = [104.0, 0.1, 0.8644665, 0.03226901, ...]`

---

## 4. The Critical Conversion

### 4.1 The Round-Trip

**Original (in HDF):**
```
"   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    "
```

**PyKRC reads and parses:**
```python
[104.0, 0.1, 0.8644665, 0.03226901, -1.281586]
```

**PyKRC must re-format to:**
```
"   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    "
```

**EXACT CHARACTER-BY-CHARACTER MATCH REQUIRED!**

### 4.2 Why This Is Challenging

**Precision loss during parsing:**
```python
original = " 0.3226901E-01"
parsed   = float("0.3226901E-01")  # 0.03226901
formatted = f"{parsed:15.7E}"       # Must produce " 0.3226901E-01"
```

**Potential issues:**
- Floating-point rounding in Python vs Fortran
- Different exponent formatting (E-01 vs E-1)
- Leading zeros in exponent (E-01 vs E-1)
- Trailing spaces
- Scientific notation threshold differences

---

## 5. Current PyKRC Formatting

### 5.1 executor.py Implementation

**Location:** [executor.py:217-252](executor.py#L217-L252)

```python
# Write PORB parameters (30 values in 6 lines of 5G15.7 each)
porb_params = params.get('PORB_PARAMS')
if porb_params is not None:
    for i in range(0, len(porb_params), 5):
        chunk = porb_params[i:i+5]
        formatted_vals = []
        for val in chunk:
            abs_val = abs(val)

            # Fortran G15.7: uses E notation for |val| < 0.1 or |val| >= 1e8
            if (abs_val < 0.1 and abs_val > 0) or abs_val >= 1e8:
                formatted = f"{val:15.7E}"
            else:
                # F format with variable decimals
                if abs_val >= 1:
                    if abs_val >= 1000:
                        decimals = 3
                    elif abs_val >= 100:
                        decimals = 4
                    elif abs_val >= 10:
                        decimals = 5
                    else:
                        decimals = 6
                else:
                    decimals = 7

                formatted = f"{val:{15}.{decimals}f}"
            formatted_vals.append(formatted)

        line = ''.join(formatted_vals) + '    '  # 4 trailing spaces
        f.write(line + '\n')
```

### 5.2 Missing Cases (from PORB_FORMATTING_ANALYSIS.md)

**Current code is incomplete for large values:**

```python
# MISSING:
if abs_val >= 10000000:    # 10M ≤ v < 100M
    decimals = 0
elif abs_val >= 1000000:   # 1M ≤ v < 10M
    decimals = 1
elif abs_val >= 100000:    # 100k ≤ v < 1M
    decimals = 2
elif abs_val >= 10000:     # 10k ≤ v < 100k
    decimals = 3
```

**For Mars, this doesn't matter** (no PORB values > 10k except 3397.977), but **for other bodies it could fail!**

---

## 6. Verification Strategy

### 6.1 Extract Original Text from HDF

```python
import h5py
from pathlib import Path

def extract_porb_text(body: str, support_dir: Path) -> list[str]:
    """Extract original PORB text from HDF file."""
    porb_file = support_dir / "porb_defaults" / f"{body}.porb.hdf"

    with h5py.File(porb_file, 'r') as f:
        rot_text = f['rot'][0].decode('utf-8')
        lines = rot_text.strip().split('\n')
        return lines[1:7]  # Lines with numeric values

# Get original
original_lines = extract_porb_text("Mars", support_dir)
```

### 6.2 Parse to Numeric

```python
def parse_porb_values(lines: list[str]) -> list[float]:
    """Parse PORB text back to numeric values."""
    values = []
    for line in lines:
        for val_str in line.split():
            values.append(float(val_str))
    return values

# Get numeric values
numeric_values = parse_porb_values(original_lines)
# Result: [104.0, 0.1, 0.8644665, 0.03226901, ...]
```

### 6.3 Re-format and Compare

```python
def format_porb_line(values: list[float]) -> str:
    """Format 5 PORB values using G15.7."""
    formatted = [format_g15_7(val) for val in values]
    return ''.join(formatted) + '    '

# Re-format
pykrc_lines = []
for i in range(0, 30, 5):
    chunk = numeric_values[i:i+5]
    pykrc_lines.append(format_porb_line(chunk))

# Compare character-by-character
for i, (orig, pykrc) in enumerate(zip(original_lines, pykrc_lines)):
    if orig != pykrc:
        print(f"Line {i+1} differs:")
        print(f"  Original: '{orig}'")
        print(f"  PyKRC:    '{pykrc}'")

        # Character-by-character comparison
        for j, (c1, c2) in enumerate(zip(orig, pykrc)):
            if c1 != c2:
                print(f"  Position {j}: '{c1}' != '{c2}'")
```

### 6.4 Test All Cached Bodies

```python
# Get list of all cached bodies
porb_defaults_dir = support_dir / "porb_defaults"
cached_bodies = [f.stem for f in porb_defaults_dir.glob("*.porb.hdf")]

for body in cached_bodies:
    print(f"\nTesting {body}...")
    original = extract_porb_text(body, support_dir)
    numeric = parse_porb_values(original)

    # Re-format
    pykrc_formatted = []
    for i in range(0, 30, 5):
        chunk = numeric[i:i+5]
        pykrc_formatted.append(format_porb_line(chunk))

    # Compare
    for i, (orig, pykrc) in enumerate(zip(original, pykrc_formatted)):
        assert orig == pykrc, f"{body} line {i+1} differs!"

    print(f"  ✓ All lines match!")
```

---

## 7. Known Issues and Risks

### 7.1 Floating-Point Precision

**Issue:** Python `float(string)` may parse differently than Fortran `READ()`

**Example:**
```fortran
! Fortran writes:
WRITE(*, '(G15.7)') 0.03226901
! Output: " 0.3226901E-01"

! Python reads:
val = float("0.3226901E-01")
# val = 0.032269009999999998 (not exactly 0.03226901)

! Python writes:
f"{val:15.7E}"
# May produce: " 0.3226901E-01" (correct)
# Or:          " 3.2269010E-02" (wrong exponent!)
```

**Mitigation:**
- Test with actual HDF values
- Use `decimal.Decimal` for exact parsing if needed
- Compare output character-by-character

### 7.2 Exponent Format Differences

**Python default:**
```python
f"{0.03226901:15.7E}"  # " 3.2269010E-02" (normalized)
```

**Fortran G format:**
```fortran
" 0.3226901E-01"       ! Leading zero, different exponent
```

**Current PyKRC code uses Python's default `E` format:**
```python
formatted = f"{val:15.7E}"  # May not match Fortran!
```

**Need to verify:** Does Python's `{:E}` format match Fortran's G format?

### 7.3 Trailing Spaces

**Fortran writes exactly 4 trailing spaces:**
```fortran
WRITE(*, '(5G15.7,4X)')  ! 4X = 4 spaces
```

**PyKRC adds 4 spaces:**
```python
line = ''.join(formatted_vals) + '    '  # Correct ✓
```

---

## 8. Recommendations

### 8.1 Immediate Actions

1. **Fix decimal place logic** for large values (10k-100M range)

2. **Test Python E format** vs Fortran G format:
   ```python
   test_val = 0.03226901
   print(f"{test_val:15.7E}")  # Check if matches " 0.3226901E-01"
   ```

3. **Implement verification test**:
   ```python
   # In tests/test_porb_formatting.py
   def test_porb_round_trip():
       """Verify PORB values format identically to HDF source."""
       # Load Mars HDF
       # Parse to numeric
       # Re-format
       # Compare character-by-character
   ```

4. **Document any discrepancies** found and decide:
   - Modify Python formatting to match Fortran
   - Or accept minor differences if functionally equivalent

### 8.2 Long-term Considerations

**Option A: Direct HDF pass-through (like davinci)**
- Store pre-formatted PORB text from HDF
- Write verbatim to input file
- **No re-formatting needed!**
- Guaranteed exact match

**Option B: Maintain current approach**
- Parse to numeric, re-format
- Requires exact G15.7 replication
- More flexible for custom PORB values
- Higher risk of formatting differences

**Recommendation:** Consider Option A for bodies with cached HDF, Option B only for custom/generic bodies.

---

## 9. Summary

### 9.1 PyKRC PORB Data Sources

**Primary:** `porb_defaults/*.porb.hdf` (same files davinci uses)
**Fallback:** Hardcoded `BODY_DEFAULTS` in orbital.py

### 9.2 Data Flow

1. Load HDF file
2. Read 'rot' field (pre-formatted text)
3. Parse text → numeric values (30 floats)
4. Store as `PORB_PARAMS` list
5. Later: Re-format using Python G15.7
6. Write to input file

### 9.3 Critical Validation Point

**The round-trip must be exact:**
```
HDF text → parse → numeric → format → output text
                                       ↓
                              Must equal HDF text!
```

### 9.4 Current Status

- ✓ HDF loading works
- ✓ Text parsing works
- ✓ Numeric extraction works
- ✗ **Formatting incomplete** (missing large value cases)
- ? **Formatting accuracy unknown** (needs testing)

### 9.5 Next Steps

1. Fix decimal place logic
2. Test with Mars HDF values
3. Verify character-by-character match
4. Test all 30 cached bodies
5. Document any necessary workarounds

---

**END OF DOCUMENT**
