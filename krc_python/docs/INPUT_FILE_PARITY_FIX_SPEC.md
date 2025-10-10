# PyKRC Input File Parity Fix Specification

**Goal:** Achieve 100% 1-to-1 input file parity with Davinci krc.dvrc
**Date:** 2025-10-09
**Based on:** Architecture analysis from both Davinci and PyKRC agents

---

## Executive Summary

This document provides a complete specification for fixing PyKRC to generate **byte-for-byte identical** input files compared to Davinci krc.dvrc. All fixes are prioritized and include exact file locations, line numbers, and code changes.

### Critical Path Issues

1. **N4 Header Writing** - Causes KRC execution failure (DUMB,C=772 773)
2. **Changecard Filtering** - Parameters not written when they should be
3. **MASTER_INP_DEFAULTS** - Wrong baseline values used
4. **Parameter Header Values** - Incorrect values written to header section

---

## Priority 1: CRITICAL FIXES (Prevents Execution)

### Fix 1.1: N4 Header Writing

**Issue:** N4 in header always shows master.inp default (19), not actual number of latitudes.

**Location:** `pykrc/executor.py`, lines 252-260

**Current Code:**
```python
# INTEGER*4 block 1 (N1 through IC2)
int_params_1 = ['N1', 'N2', 'N3', 'N4', 'N5', 'N24', 'IIB', 'IC2']
int_values_1 = [
    params.get(p, self.MASTER_INP_DEFAULTS['INTEGER'].get(p, 0))
    for p in int_params_1
]
self._write_param_line(f, int_params_1, 8)
self._write_param_line(f, int_values_1, 8, is_float=False)
```

**Fix:**
```python
# INTEGER*4 block 1 (N1 through IC2)
int_params_1 = ['N1', 'N2', 'N3', 'N4', 'N5', 'N24', 'IIB', 'IC2']

# CRITICAL: N4 must be actual number of latitudes, not default
# This is written to header (not via changecard) per Davinci
n4_actual = len(params.get('Latitudes', [1.0]))  # Number of latitudes

int_values_1 = []
for p in int_params_1:
    if p == 'N4':
        int_values_1.append(n4_actual)  # Use actual count
    else:
        int_values_1.append(params.get(p, self.MASTER_INP_DEFAULTS['INTEGER'].get(p, 0)))

self._write_param_line(f, int_params_1, 8)
self._write_param_line(f, int_values_1, 8, is_float=False)
```

**Validation:** After fix, header line 20 should show `N4=1` when 1 latitude provided, not `N4=19`.

---

### Fix 1.2: KOLD/KEEP Header Values

**Issue:** KOLD and KEEP always written as 0, should reflect actual params.

**Location:** `pykrc/executor.py`, lines 229-231

**Current Code:**
```python
# Write KOLD, KEEP header (line 1)
kold = params.get('KOLD', 0)
keep = params.get('KEEP', 0)
f.write(f"{kold} {keep} / KOLD: season to start with;  KEEP: continue saving data in same disk file\n")
```

**Fix:**
```python
# Write KOLD, KEEP header (line 1)
# Convert KEEP from boolean/string to integer if needed
kold = params.get('KOLD', 0)
keep_val = params.get('KEEP', 0)

# Handle KEEP as boolean or string (from user input)
if isinstance(keep_val, str):
    keep = 1 if keep_val.upper() in ('T', 'TRUE', '1') else 0
elif isinstance(keep_val, bool):
    keep = 1 if keep_val else 0
else:
    keep = int(keep_val)

f.write(f"{kold} {keep} / KOLD: season to start with;  KEEP: continue saving data in same disk file\n")
```

**Validation:** When `KEEP='T'` passed, header should show `0 1 /` not `0 0 /`.

---

## Priority 2: CHANGECARD SYSTEM FIXES

### Fix 2.1: Changecard Filtering Logic

**Issue:** Changecards only written for params in `user_params` AND different from defaults. Should write for ALL params that differ from MASTER_INP_DEFAULTS, regardless of user_params.

**Location:** `pykrc/executor.py`, lines 400-503 (`_write_changecards`)

**Current Logic:**
```python
def _write_changecards(self, f, params, user_params=None):
    # ...
    # Only write if in user_params OR differs significantly from default
    if user_params is None or param_name in user_params:
        if should_write_changecard(param_value, default_value):
            # Write changecard
```

**Davinci Logic:**
- Write changecard if `HasValue(param)` (user provided it)
- OR if param differs from master.inp default
- No filtering based on "user_params" concept

**Fix Strategy:**

The `user_params` parameter was added to filter changecards for minimal output, but Davinci doesn't do this. For **1-to-1 parity**, we need to match Davinci's behavior exactly.

**Option A (Recommended): Match Davinci exactly**
```python
def _write_changecards(self, f, params, user_params=None):
    """
    Write changecards for parameters that differ from MASTER_INP_DEFAULTS.

    For 1-to-1 Davinci parity:
    - Write ALL parameters that differ from master.inp defaults
    - Do NOT filter by user_params (Davinci doesn't have this concept)
    - Order: Type 3 (bool), Type 2 (int), Type 1 (float), Type 8 (files)
    """
    # Ignore user_params filtering for parity
    # Write all parameters that differ from defaults

    # [Rest of implementation with user_params filtering removed]
```

**Option B (Compromise): Add parity_mode flag**
```python
def _write_changecards(self, f, params, user_params=None, parity_mode=True):
    """
    Write changecards for parameters.

    Args:
        parity_mode: If True, match Davinci exactly (ignore user_params filter)
                    If False, use PyKRC's optimized filtering
    """
    if parity_mode:
        # Write all params that differ from defaults
        effective_user_params = None
    else:
        # Use user_params filter for minimal output
        effective_user_params = user_params

    # [Rest with effective_user_params]
```

**Recommendation:** Use **Option A** for simplicity and guaranteed parity. The changecard filtering was premature optimization.

---

### Fix 2.2: Changecard Parameter Coverage

**Issue:** Some parameters that should have changecards don't.

**Location:** `pykrc/executor.py`, lines 410-450

**Missing Parameters:**

Based on Davinci analysis, ensure these parameters CAN have changecards:

**REAL*8 Parameters (Type 1, indices 1-64):**
- All 64 float parameters from master.inp
- Current PyKRC covers most but check: DUSTA, TAURAT, FANON, PhotoFunc

**INTEGER*4 Parameters (Type 2, indices 1-24, skip N4):**
- Current: N1, N2, N3, N5, N24, IIB, IC2, NRSET, JDISK, etc.
- Add if missing: JBARE, Notif, IDISK2, KPREF

**LOGICAL*4 Parameters (Type 3, indices 1-20):**
- Current: LPORB, LKEY, LKofT
- Add if missing: LP1-LP6, LVFA, LVFT, LSC, LZONE, LOCAL, LPTAVE

**Current Coverage Check:**
```python
# In _write_changecards, verify all these mappings exist:

REAL_PARAM_INDICES = {
    'ALBEDO': 1, 'EMISS': 2, 'INERTIA': 3, 'COND2': 4, 'DENS2': 5,
    'PERIOD': 6, 'SPEC_HEAT': 7, 'DENSITY': 8,
    # ... ensure all 64 are mapped
    'FANON': 13, 'TATM': 14, 'TDEEP': 15, 'SpHeat2': 16,
    'TAUD': 17, 'DUSTA': 18, 'TAURAT': 19,
    # etc.
}

INTEGER_PARAM_INDICES = {
    'N1': 1, 'N2': 2, 'N3': 3,
    # N4 is skipped (written to header)
    'N5': 5, 'N24': 6, 'IIB': 7, 'IC2': 8,
    'NRSET': 9, 'NMHA': 10, 'NRUN': 11, 'JDISK': 12,
    'IDOWN': 13, 'FlxP14': 14, 'TUN_Flx15': 15, 'KPREF': 16,
    'K4OUT': 17, 'JBARE': 18, 'Notif': 19, 'IDISK2': 20
}

LOGICAL_PARAM_INDICES = {
    'LP1': 1, 'LP2': 2, 'LP3': 3, 'LP4': 4, 'LP5': 5, 'LP6': 6,
    'LPGLOB': 7, 'LVFA': 8, 'LVFT': 9, 'LKofT': 10,
    'LPORB': 11, 'LKEY': 12, 'LSC': 13, 'LZONE': 14, 'LOCAL': 15,
    'Prt76': 16, 'LPTAVE': 17, 'Prt78': 18, 'Prt79': 19, 'L_ONE': 20
}
```

**Fix:** Verify mappings in `executor.py` match these exactly.

---

## Priority 3: MASTER_INP_DEFAULTS CORRECTIONS

### Fix 3.1: Update Default Values

**Issue:** Some defaults don't match what Davinci uses.

**Location:** `pykrc/executor.py`, lines 30-125 (MASTER_INP_DEFAULTS)

**Required Changes:**

Compare current MASTER_INP_DEFAULTS against actual master.inp:

**File to check:** `/Users/chaberle/Documents/GitHab/KRC/run/master.inp`

**Known Discrepancies:**

| Parameter | Current PyKRC | Should Be | Source |
|-----------|---------------|-----------|--------|
| RLAY | 1.15 (?) | 1.15 | master.inp line 11 |
| FLAY | 0.10 (?) | 0.10 | master.inp line 11 |
| FANON | 0.055 (?) | 0.06 | master.inp line 7 |
| TAURAT | 0.25 (?) | 0.25 | master.inp line 7 |

**Action Required:**

1. Read `/Users/chaberle/Documents/GitHab/KRC/run/master.inp`
2. Parse all 64 REAL*8, 24 INTEGER*4, 20 LOGICAL*4 values
3. Update `MASTER_INP_DEFAULTS` dict in `executor.py` to match EXACTLY
4. Document any intentional deviations

**Verification Script:**
```python
from pykrc.input_processor import parse_master_inp
from pykrc.config import get_paths

paths = get_paths()
master = parse_master_inp(paths.master_inp)

# Compare against MASTER_INP_DEFAULTS
from pykrc.executor import KRCExecutor
executor = KRCExecutor('/path/to/krc')

for param, value in master.items():
    defaults_val = executor.MASTER_INP_DEFAULTS.get(param_type, {}).get(param)
    if defaults_val != value:
        print(f"MISMATCH: {param} = {defaults_val} (PyKRC) vs {value} (master.inp)")
```

---

### Fix 3.2: Header Values Written

**Issue:** Header section uses MASTER_INP_DEFAULTS but should use actual parameter values when available.

**Current Behavior:**
- Header lines 3-18 written with MASTER_INP_DEFAULTS
- Changecards override values
- This is CORRECT per KRC format

**Davinci Behavior:**
- Same approach (header = defaults, changecards = overrides)

**Status:** ✅ No fix needed - this is correct behavior.

---

## Priority 4: PARAMETER VALUE CORRECTIONS

### Fix 4.1: Calculated vs User Parameters

**Issue:** Some parameters should prefer calculated values over master.inp defaults.

**Location:** `pykrc/core.py` (parameter preparation before calling executor)

**Examples:**

1. **DELJUL** - Should be calculated from DELLS and PERIOD
   ```python
   # In core.py, before passing to executor:
   if 'DELJUL' not in params and 'DELLS' in params:
       DELJUL = params['PERIOD'] * params['DELLS'] / 360.0
       params['DELJUL'] = DELJUL
   ```

2. **N24** - Should be calculated from PERIOD
   ```python
   # Based on Davinci krc.dvrc lines 462-465
   if 'N24' not in params:
       rot_per = params['PERIOD']
       n24 = int(np.floor((rot_per * 4) / 96) * 96)
       if n24 < 96:
           n24 = 96
       params['N24'] = n24
   ```

3. **Material Properties** - ConUp, ConLo, SphUp, SphLo calculated from INERTIA/Mat/Por
   - Current implementation appears correct
   - Verify against Davinci Mat_Prop() function

**Action:** Review `pykrc/core.py` parameter preparation to ensure all calculated params match Davinci.

---

## Priority 5: FORMATTING SPECIFICATIONS

### Fix 5.1: Float Precision in Headers

**Issue:** Header float precision may differ from Davinci.

**Location:** `pykrc/executor.py`, `_write_param_line()` method

**Davinci Format:**
- Header REAL*8: 10 characters wide, 2 decimal places: `%10.2f`
- Example: `      0.25      1.00    200.00`

**Current PyKRC:**
```python
def _write_param_line(self, f, values, per_line, is_float=True):
    if is_float:
        formatted = [f"{v:10.2f}" for v in values]
    else:
        formatted = [f"{v:10d}" for v in values]
```

**Status:** ✅ Appears correct - verify with test case.

---

### Fix 5.2: Changecard Float Precision

**Issue:** Changecard float formatting must match Davinci exactly.

**Location:** `pykrc/executor.py`, lines 475-485

**Davinci Format:**
```davinci
# From krc.dvrc lines 1474-1480
if (abs(value) < 1.0) {
    sprintf("%.3E", value)  # Scientific notation
} else {
    sprintf("%.4f", value)  # Fixed point
}
```

**Current PyKRC:**
```python
if abs(value) < 1.0:
    value_str = f"{value:.3E}"
else:
    value_str = f"{value:.4f}"
```

**Status:** ✅ Appears correct - verify formatting matches exactly.

---

### Fix 5.3: PORB Header Format

**Issue:** PORB header line may be hardcoded or use wrong format.

**Location:** `pykrc/executor.py`, lines 297-312

**Davinci Format:**
```
PORB:2014jun10 2024 Jun 27 13:04:12 IPLAN,TC= 101.0 0.10000 Mars:Mars
```

**Current PyKRC:**
```python
porb_header = params.get('PORB_HEADER',
    'PORB:2014jun10 2024 Jun 27 13:04:12 IPLAN,TC= 101.0 0.10000 Mars:Mars')
f.write(f"{porb_header}\n")
```

**Issue:** Uses default Mars header for all bodies.

**Fix:**
```python
# Should be generated dynamically based on body and timestamp
# Format: PORB:<version> <timestamp> IPLAN,TC= <iplan> <tc> <body>:<body>

porb_header = params.get('PORB_HEADER')
if not porb_header:
    # Generate from body params
    import datetime
    now = datetime.datetime.now()
    timestamp = now.strftime("%Y %b %d %H:%M:%S")
    iplan = params.get('IPLAN', 101.0)  # From PORB data
    tc = params.get('TC', 0.1)          # From PORB data
    body = params.get('body', 'Mars')
    porb_header = f"PORB:2014jun10 {timestamp} IPLAN,TC= {iplan:.1f} {tc:.5f} {body}:{body}"

f.write(f"{porb_header}\n")
```

---

## Priority 6: SPECIAL FEATURES

### Fix 6.1: Time-Varying Arrays (Type 8 Changecards)

**Issue:** ALBEDO/TAUD arrays not written to files.

**Davinci Behavior:**
- If ALBEDO or TAUD is an array, write to separate file
- Type 8 changecard points to file
- Format: N5 values, one per line

**Current PyKRC:**
- Arrays detected and stored
- NOT written as Type 8 changecards

**Location to Fix:** `pykrc/executor.py`, `_write_changecards()` after line 500

**Implementation:**
```python
# After existing changecard writing, add:

# Type 8: Time-varying arrays
# Check for ALBEDO array
if 'ALBEDO' in params and isinstance(params['ALBEDO'], (list, np.ndarray)):
    if len(params['ALBEDO']) > 1:
        # Write ALBEDO to file
        albedo_file = workdir / 'albedo.dat'
        with open(albedo_file, 'w') as af:
            for alb in params['ALBEDO']:
                af.write(f"{alb:.6f}\n")
        # Write Type 8 changecard
        f.write(f"8 1 0 './albedo.dat' /\n")

# Check for TAUD array
if 'TAUD' in params and isinstance(params['TAUD'], (list, np.ndarray)):
    if len(params['TAUD']) > 1:
        # Write TAUD to file
        taud_file = workdir / 'taud.dat'
        with open(taud_file, 'w') as tf:
            for tau in params['TAUD']:
                tf.write(f"{tau:.6f}\n")
        # Write Type 8 changecard
        f.write(f"8 17 0 './taud.dat' /\n")
```

---

### Fix 6.2: Eclipse Parameters (Type 8 Changecard)

**Davinci:** Eclipse params written as Type 14 changecard (not Type 8).

**Current PyKRC:** Not implemented.

**Priority:** Low (not commonly used, document as limitation).

---

### Fix 6.3: Planetary Flux Parameters

**Davinci:** PFlux params written as Type 15 changecard.

**Current PyKRC:** Not implemented.

**Priority:** Low (not commonly used, document as limitation).

---

## Implementation Order

### Phase 1: Critical Fixes (Blocks Execution)
1. Fix 1.1: N4 Header Writing ⚠️ **MUST FIX FIRST**
2. Fix 1.2: KOLD/KEEP Header Values
3. Fix 2.1: Changecard Filtering Logic

### Phase 2: Core Parity
4. Fix 3.1: MASTER_INP_DEFAULTS Corrections
5. Fix 2.2: Changecard Parameter Coverage
6. Fix 4.1: Calculated vs User Parameters

### Phase 3: Polish
7. Fix 5.1-5.3: Formatting Specifications
8. Fix 6.1: Time-Varying Arrays

### Phase 4: Advanced Features (Optional)
9. Fix 6.2-6.3: Eclipse/PFlux (document as future work)

---

## Validation Plan

After each fix:

1. **Generate test input file** with PyKRC
2. **Generate reference input file** with Davinci
3. **Compare line-by-line** using diff
4. **Run KRC** with both files to verify they execute
5. **Compare KRC output** to verify numerical equivalence

### Test Cases

**Test 1: Minimal Mars**
```python
krc(lat=0., lon=0., body="Mars", INERTIA=200, ALBEDO=0.25)
```

**Test 2: Multi-latitude**
```python
krc(lat=[0., 25., 50.], body="Mars", INERTIA=200)
```

**Test 3: Two-layer**
```python
krc(lat=0., body="Mars", thick=0.3, INERTIA=200, INERTIA2=1200)
```

**Test 4: Time-varying**
```python
krc(lat=0., body="Mars", ALBEDO=[0.2]*360, INERTIA=200)
```

---

## Success Criteria

✅ **100% Input File Parity** when:
1. All test cases produce byte-identical input files
2. KRC executes successfully with PyKRC-generated files
3. KRC output matches Davinci output numerically (within tolerance)
4. No DUMB errors or execution failures
5. Validation suite passes all tests

---

## Files to Modify

1. **`pykrc/executor.py`** - Primary changes (N4, changecards, formatting)
2. **`pykrc/core.py`** - Parameter preparation (N24, DELJUL calculations)
3. **`pykrc/interface_validator.py`** - Already fixed (return statement)
4. **Tests** - Add comprehensive input file comparison tests

---

## Estimated Effort

- **Phase 1 (Critical):** 2-3 hours
- **Phase 2 (Core Parity):** 4-6 hours
- **Phase 3 (Polish):** 2-3 hours
- **Phase 4 (Advanced):** 4-6 hours (optional)
- **Testing/Validation:** 3-4 hours

**Total:** 15-22 hours for complete 1-to-1 parity

---

**Next Step:** Begin Phase 1 implementation with Fix 1.1 (N4 Header Writing).
