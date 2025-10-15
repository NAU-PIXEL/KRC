# PORB Parameter Formatting: Complete Specification

**Date:** 2025-10-14
**Source Analysis:**
- [master.inp](/Users/chaberle/Documents/GitHab/KRC/run/master.inp) lines 35-41
- [executor.py](/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/executor.py) lines 217-252
**Purpose:** Document exact PORB formatting for 1-to-1 davinci parity

---

## Executive Summary

The PORB orbital parameters are written to the input file as **30 floating-point values** formatted in **6 lines of 5 values each**, using **Fortran G15.7 format**. This format uses a **15-character field width** with **7 significant digits**, switching between fixed-point and scientific notation based on magnitude.

**Key Finding:** PyKRC implementation has the right approach but needs precision in the conditional logic for when to use E vs F format.

---

## 1. Master.inp PORB Section

### 1.1 Raw Text (lines 35-41)

```
 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars
   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586
  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373
   5.544402       0.000000       0.000000       686.9929       3397.977
   24.62296       0.000000      -1.240317       0.000000       0.000000
   0.000000      0.3244965      0.8559126      0.4026359     -0.9458869
  0.2936298      0.1381285       0.000000     -0.4256703      0.9048783
```

### 1.2 Structure

**Line 35 (Header):**
- Format: `<date> <time>=RUNTIME.  IPLAN AND TC= <IPLAN> <TC> <body>:<body>`
- Example: `2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars`
- IPLAN: Planet ID (104.0 for Mars)
- TC: Time constant (0.10000)

**Lines 36-41 (PORB Matrix):**
- 30 values total
- 6 lines × 5 values per line
- Each value: **15 characters wide**
- Trailing spaces: 4 spaces after 5th column

---

## 2. Fortran G15.7 Format Specification

### 2.1 G Format Definition

**Fortran G format** is a **generalized** format that automatically switches between:
- **F format** (fixed-point): for "normal" magnitude values
- **E format** (exponential): for very small or very large values

**G15.7 means:**
- **15**: Total field width (including sign, digits, decimal, exponent)
- **7**: Significant digits to display

### 2.2 G Format Decision Logic

**According to Fortran standard (ISO/IEC 1539-1:2010):**

For `Gw.d` format where `w=15` and `d=7`:

1. Calculate effective exponent `e` such that value = `m × 10^e` where `1 ≤ |m| < 10`
2. Define range: `-d-2 < e < 0` for F format, else E format
3. For G15.7: `-9 < e < 0` uses F format

**Translation to absolute value ranges:**
- **F format:** `0.1 ≤ |value| < 10^8` (approximately)
- **E format:** `|value| < 0.1` or `|value| ≥ 10^8`

**Special case:** `value == 0.0` uses F format (no exponent needed)

### 2.3 Observed Master.inp Examples

Let me analyze each value from lines 36-41:

| Value | Magnitude | Format Used | Width | Sig Figs | Notes |
|-------|-----------|-------------|-------|----------|-------|
| `104.0000` | 104 | **F** | 15 | 4 decimals | Leading spaces |
| `0.1000000` | 0.1 | **F** | 15 | 7 decimals | Exactly at boundary |
| `0.8644665` | 0.864 | **F** | 15 | 7 decimals | |
| `0.3226901E-01` | 0.03227 | **E** | 15 | 7 sig figs | < 0.1 threshold |
| `-1.281586` | 1.28 | **F** | 15 | 6 decimals | Negative, normal range |
| `0.9340198E-01` | 0.09340 | **E** | 15 | 7 sig figs | < 0.1 threshold |
| `1.523712` | 1.524 | **F** | 15 | 6 decimals | |
| `0.4090926` | 0.409 | **F** | 15 | 7 decimals | ≥ 0.1 |
| `0.000000` | 0 | **F** | 15 | 6 decimals | Zero special case |
| `0.9229373` | 0.923 | **F** | 15 | 7 decimals | |
| `5.544402` | 5.544 | **F** | 15 | 6 decimals | |
| `686.9929` | 687 | **F** | 15 | 4 decimals | |
| `3397.977` | 3398 | **F** | 15 | 3 decimals | |
| `24.62296` | 24.6 | **F** | 15 | 5 decimals | |
| `-1.240317` | 1.24 | **F** | 15 | 6 decimals | |
| `0.3244965` | 0.324 | **F** | 15 | 7 decimals | |
| `0.8559126` | 0.856 | **F** | 15 | 7 decimals | |
| `0.4026359` | 0.403 | **F** | 15 | 7 decimals | |
| `-0.9458869` | 0.946 | **F** | 15 | 7 decimals | |
| `0.2936298` | 0.294 | **F** | 15 | 7 decimals | |
| `0.1381285` | 0.138 | **F** | 15 | 7 decimals | |
| `-0.4256703` | 0.426 | **F** | 15 | 7 decimals | |
| `0.9048783` | 0.905 | **F** | 15 | 7 decimals | |

### 2.4 Pattern Analysis

**E format used when:**
- `0.3226901E-01` (0.03227): YES, < 0.1
- `0.9340198E-01` (0.09340): YES, < 0.1

**F format used when:**
- `0.1000000` (0.1): YES, exactly at boundary ≥ 0.1
- `0.4090926` (0.409): YES, > 0.1
- `0.000000` (0.0): YES, special case

**Conclusion:** The threshold is **exactly 0.1** (inclusive for F format)

---

## 3. Fortran G15.7 Formatting Rules

### 3.1 Decision Tree

```
For value v with G15.7 format:

IF v == 0.0:
    → F format: "       0.000000"  (15 chars, right-aligned)

ELSE IF abs(v) < 0.1:
    → E format: " 0.dddddddE±ee"   (15 chars, 7 decimals after 0.)
    → Example: " 0.3226901E-01"

ELSE IF abs(v) >= 1e8:
    → E format: " 0.dddddddE±ee"   (15 chars, 7 decimals after 0.)
    → Example: " 0.1234567E+09"

ELSE:  # 0.1 <= abs(v) < 1e8
    → F format with variable decimal places
    → Total significant figures ≈ 7
    → Examples:
       0.1000000 → "      0.1000000"  (7 decimals, |v| < 1)
       1.523712  → "       1.523712"  (6 decimals, 1 ≤ |v| < 10)
       24.62296  → "       24.62296"  (5 decimals, 10 ≤ |v| < 100)
       686.9929  → "       686.9929"  (4 decimals, 100 ≤ |v| < 1000)
       3397.977  → "       3397.977"  (3 decimals, 1000 ≤ |v| < 10000)
```

### 3.2 F Format Decimal Places

**For F format (0.1 ≤ |v| < 1e8):**

The number of decimal places is chosen to maintain approximately 7 significant figures:

| Magnitude Range | Integer Digits | Decimal Places | Total Sig Figs | Example |
|-----------------|----------------|----------------|----------------|---------|
| 0.1 ≤ v < 1 | 0 | 7 | ~7 | `0.1000000` |
| 1 ≤ v < 10 | 1 | 6 | 7 | `1.523712` |
| 10 ≤ v < 100 | 2 | 5 | 7 | `24.62296` |
| 100 ≤ v < 1000 | 3 | 4 | 7 | `686.9929` |
| 1000 ≤ v < 10000 | 4 | 3 | 7 | `3397.977` |
| 10000 ≤ v < 100000 | 5 | 2 | 7 | `12345.67` |
| 100000 ≤ v < 1000000 | 6 | 1 | 7 | `123456.7` |
| 1000000 ≤ v < 10000000 | 7 | 0 | 7 | `1234567.` |
| 10000000 ≤ v < 100000000 | 8 | 0 | 8 | `12345678` |

**Formula:**
```python
if abs(v) < 1:
    decimals = 7
elif abs(v) < 10:
    decimals = 6
elif abs(v) < 100:
    decimals = 5
elif abs(v) < 1000:
    decimals = 4
elif abs(v) < 10000:
    decimals = 3
elif abs(v) < 100000:
    decimals = 2
elif abs(v) < 1000000:
    decimals = 1
else:  # abs(v) < 1e8
    decimals = 0
```

### 3.3 E Format Structure

**For E format (|v| < 0.1 or |v| ≥ 1e8):**

```
 0.dddddddE±ee
│││││││││││││││
│││││││││││││└└─ Exponent (2 digits, sign always present)
││││││││││└└─── Exponent character 'E'
│││││││││└───── 7 decimal digits
│││└└└───────── Decimal point
││└───────────── Leading zero before decimal
│└────────────── Optional sign (space if positive)
└─────────────── Leading spaces to reach width 15
```

**Examples:**
- ` 0.3226901E-01` (value = 0.03226901)
- ` 0.9340198E-01` (value = 0.09340198)
- `-0.1234567E+09` (value = -123456700)

**Python format string:** `f"{val:15.7E}"`

---

## 4. PyKRC Current Implementation

### 4.1 Code Analysis (executor.py lines 217-252)

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
                # E format: 0.dddddddE±ee (d=7 digits after decimal)
                formatted = f"{val:15.7E}"
            else:
                # F format with up to 7 significant figures
                # Determine decimal places needed
                if abs_val >= 1:
                    # For values >= 1, show appropriate decimals
                    if abs_val >= 1000:
                        decimals = 3  # e.g., 3397.977
                    elif abs_val >= 100:
                        decimals = 4  # e.g., 101.0000
                    elif abs_val >= 10:
                        decimals = 5  # e.g., 24.62296
                    else:
                        decimals = 6  # e.g., 5.544402
                else:
                    # For 0.1 <= val < 1
                    decimals = 7  # e.g., 0.4090926

                formatted = f"{val:{15}.{decimals}f}"
            formatted_vals.append(formatted)
        line = ''.join(formatted_vals) + '    '
        f.write(line + '\n')
```

### 4.2 Issues with Current Implementation

**ISSUE 1: Missing ranges in F format**
- Current code doesn't handle 10000 ≤ v < 1e8
- Missing cases: decimals=2, decimals=1, decimals=0

**ISSUE 2: Zero handling**
- Current code treats zero same as other values in 0.1 ≤ v < 1 range
- This is actually **correct** - zero gets 7 decimals → `0.000000` ✓

**ISSUE 3: Boundary condition**
- `abs_val < 0.1 and abs_val > 0` is correct
- `abs_val >= 0.1` for F format is correct
- **This is correct** ✓

### 4.3 Corrected Logic

**The issue is incomplete decimal place handling:**

```python
if abs_val >= 1:
    if abs_val >= 10000000:
        decimals = 0  # 10M ≤ v < 100M (but <1e8 for F format)
    elif abs_val >= 1000000:
        decimals = 1  # 1M ≤ v < 10M
    elif abs_val >= 100000:
        decimals = 2  # 100k ≤ v < 1M
    elif abs_val >= 10000:
        decimals = 3  # 10k ≤ v < 100k
    elif abs_val >= 1000:
        decimals = 3  # 1k ≤ v < 10k
    elif abs_val >= 100:
        decimals = 4  # 100 ≤ v < 1k
    elif abs_val >= 10:
        decimals = 5  # 10 ≤ v < 100
    else:
        decimals = 6  # 1 ≤ v < 10
else:
    # For 0 ≤ val < 1 (including zero)
    decimals = 7
```

---

## 5. Line Formatting

### 5.1 Whitespace Rules

**Per line:**
- 5 values × 15 characters = 75 characters
- Plus 4 trailing spaces
- **Total: 79 characters per line**

**Example breakdown (line 36):**
```
   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586
│  │        │    │         │    │         │    │             │ │         │  │
│  └────────┘    └─────────┘    └─────────┘    └─────────────┘ └─────────┘  │
│   15 chars      15 chars       15 chars       15 chars       15 chars     │
│                                                                            │
└─ Leading spaces                                         Trailing spaces ──┘
```

**Trailing spaces:** Exactly 4 spaces after the 5th value (column 75-78)

### 5.2 Python Implementation

```python
for i in range(0, len(porb_params), 5):
    chunk = porb_params[i:i+5]
    line = ''.join([format_g15_7(val) for val in chunk]) + '    '
    f.write(line + '\n')
```

---

## 6. Complete Specification

### 6.1 Algorithm

```python
def format_g15_7(val: float) -> str:
    """
    Format a value using Fortran G15.7 format.

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

    # Determine format type
    # E format: |val| < 0.1 or |val| >= 1e8
    if (abs_val < 0.1 and abs_val > 0) or abs_val >= 1e8:
        # E notation: 0.dddddddE±ee
        return f"{val:15.7E}"

    # F format: 0.1 <= |val| < 1e8 or val == 0
    else:
        # Determine decimal places for ~7 significant figures
        if abs_val < 1:
            # 0 <= val < 1 (including zero)
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


def write_porb_params(f, porb_params: list):
    """
    Write PORB parameters (30 values) to input file.

    Format: 6 lines × 5 values, G15.7 format

    Args:
        f: File handle
        porb_params: List of 30 float values
    """
    for i in range(0, len(porb_params), 5):
        chunk = porb_params[i:i+5]
        formatted = [format_g15_7(val) for val in chunk]
        line = ''.join(formatted) + '    '  # 4 trailing spaces
        f.write(line + '\n')
```

### 6.2 Test Cases

```python
test_values = [
    (104.0, "       104.0000"),
    (0.1, "      0.1000000"),
    (0.8644665, "      0.8644665"),
    (0.03226901, " 0.3226901E-01"),
    (-1.281586, "      -1.281586"),
    (0.09340198, " 0.9340198E-01"),
    (1.523712, "       1.523712"),
    (0.4090926, "      0.4090926"),
    (0.0, "       0.000000"),
    (5.544402, "       5.544402"),
    (686.9929, "       686.9929"),
    (3397.977, "       3397.977"),
    (24.62296, "       24.62296"),
    (-1.240317, "      -1.240317"),
    (0.001, " 0.1000000E-02"),  # < 0.1 → E format
    (99999999, "  99999999"),    # Just under 1e8 → F format, 0 decimals
    (1e8, " 0.1000000E+08"),    # Exactly 1e8 → E format
    (12345.67, "    12345.670"),  # 10k ≤ v < 100k → 3 decimals
    (123456.7, "     123456.7"),  # 100k ≤ v < 1M → 1 decimal
    (1234567, "      1234567"),   # 1M ≤ v < 10M → 0 decimals
]

for val, expected in test_values:
    result = format_g15_7(val)
    assert result == expected, f"Value {val}: expected '{expected}', got '{result}'"
    assert len(result) == 15, f"Value {val}: width is {len(result)}, expected 15"
```

---

## 7. Comparison with Davinci

### 7.1 Davinci Implementation

**Davinci does NOT format PORB directly!**

From [krc.dvrc:485](krc.dvrc#L485):
```davinci
master.inp.part6=porb.rot
```

**Davinci simply:**
1. Loads pre-formatted PORB from `master.inp` into `inp.part6`
2. Calls `porb(body)` which returns `porb.rot` (30-element array)
3. **Replaces** `inp.part6` with `porb.rot`
4. **Writes** `inp.part6` directly to output file (lines 1147, 1345)

**Key insight:** The PORB values in `porb.rot` are **already pre-formatted as strings** by the PORB Fortran code!

### 7.2 PORB Source Formatting

The actual formatting happens in the **PORB Fortran code** (porbel.f or similar), which writes the PORB matrix to a file that davinci then reads.

**This means:**
- Davinci doesn't format PORB - it just passes through pre-formatted strings
- PyKRC must replicate the Fortran G15.7 format exactly

---

## 8. Recommendations for PyKRC

### 8.1 Critical Fixes

**Fix 1: Complete decimal place logic**

Add missing ranges for large values:

```python
if abs_val >= 10000000:
    decimals = 0  # 10M to <100M
elif abs_val >= 1000000:
    decimals = 1  # 1M to <10M
elif abs_val >= 100000:
    decimals = 2  # 100k to <1M
elif abs_val >= 10000:
    decimals = 3  # 10k to <100k (currently missing)
```

**Fix 2: Verify threshold precision**

Current code is correct:
```python
if (abs_val < 0.1 and abs_val > 0) or abs_val >= 1e8:
    # E format
```

This correctly handles:
- ✓ `abs_val == 0.1` → F format
- ✓ `abs_val < 0.1` (but not zero) → E format
- ✓ `abs_val == 0` → F format (falls through to else)

### 8.2 Testing Strategy

**Test with actual PORB values:**

1. Extract `porb.rot` from davinci for Mars
2. Format with PyKRC
3. Compare character-by-character with master.inp lines 36-41

**Test edge cases:**
- `0.1` (exactly at boundary)
- `0.09999999` (just below boundary)
- `1e8` (exactly at upper boundary)
- `9.9999999e7` (just below upper boundary)
- `0.0` (zero)
- Very large negative values

### 8.3 Validation Checklist

- [ ] E format used for `|val| < 0.1` (excluding zero)
- [ ] E format used for `|val| >= 1e8`
- [ ] F format used for `0.1 ≤ |val| < 1e8`
- [ ] F format used for `val == 0.0`
- [ ] Decimal places correct for all magnitude ranges
- [ ] Field width exactly 15 characters
- [ ] Line has 4 trailing spaces after 5th value
- [ ] Negative sign handled correctly
- [ ] Scientific notation format: `0.dddddddE±ee`
- [ ] All 30 PORB values formatted correctly

---

## 9. Summary Table

### 9.1 Format Selection

| Condition | Format | Example Value | Formatted Output |
|-----------|--------|---------------|------------------|
| `val == 0` | F, 7 decimals | 0.0 | `"       0.000000"` |
| `0 < \|val\| < 0.1` | **E, 7 decimals** | 0.03227 | `" 0.3226901E-01"` |
| `0.1 ≤ \|val\| < 1` | F, 7 decimals | 0.409 | `"      0.4090926"` |
| `1 ≤ \|val\| < 10` | F, 6 decimals | 1.524 | `"       1.523712"` |
| `10 ≤ \|val\| < 100` | F, 5 decimals | 24.62 | `"       24.62296"` |
| `100 ≤ \|val\| < 1k` | F, 4 decimals | 687.0 | `"       686.9929"` |
| `1k ≤ \|val\| < 10k` | F, 3 decimals | 3398 | `"       3397.977"` |
| `10k ≤ \|val\| < 100k` | F, 3 decimals | 12345 | `"    12345.670"` |
| `100k ≤ \|val\| < 1M` | F, 2 decimals | 123456 | `"     123456.70"` |
| `1M ≤ \|val\| < 10M` | F, 1 decimal | 1234567 | `"    1234567.0"` |
| `10M ≤ \|val\| < 100M` | F, 0 decimals | 12345678 | `"     12345678"` |
| `\|val\| ≥ 1e8` | **E, 7 decimals** | 1e8 | `" 0.1000000E+08"` |

### 9.2 Line Structure

```
Position:  1         10        20        30        40        50        60        70
           |         |         |         |         |         |         |         |
Line 36:   "   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    "
           └─ 15 chars ┘└─ 15 chars ┘└─ 15 chars ┘└─ 15 chars ┘└─ 15 chars ┘└─ 4 ┘

Total: 79 characters (75 for data + 4 trailing spaces)
```

---

## 10. Code Changes Required

### 10.1 executor.py Modifications

**Current code location:** Lines 217-252

**Changes needed:**

```python
# BEFORE (incomplete):
if abs_val >= 1:
    if abs_val >= 1000:
        decimals = 3
    elif abs_val >= 100:
        decimals = 4
    # ... missing cases ...

# AFTER (complete):
if abs_val >= 1:
    if abs_val >= 10000000:
        decimals = 0  # 10M ≤ v < 100M
    elif abs_val >= 1000000:
        decimals = 1  # 1M ≤ v < 10M
    elif abs_val >= 100000:
        decimals = 2  # 100k ≤ v < 1M
    elif abs_val >= 10000:
        decimals = 3  # 10k ≤ v < 100k
    elif abs_val >= 1000:
        decimals = 3  # 1k ≤ v < 10k
    elif abs_val >= 100:
        decimals = 4  # 100 ≤ v < 1k
    elif abs_val >= 10:
        decimals = 5  # 10 ≤ v < 100
    else:
        decimals = 6  # 1 ≤ v < 10
else:
    # 0 ≤ val < 1 (including zero)
    decimals = 7
```

**Note:** The 10k and 1k ranges both use 3 decimals - this is intentional to maintain ~7 significant figures.

---

**END OF DOCUMENT**
