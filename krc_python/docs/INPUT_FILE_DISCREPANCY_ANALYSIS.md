# PyKRC Input File Discrepancy Analysis
## Systematic Analysis of All 15 Integration Tests

**Date:** 2025-10-15
**Analyst:** Claude
**Purpose:** Identify root causes for input file differences between pykrc and davinci

---

## Executive Summary

Analysis of 15 integration tests reveals **7 distinct root causes** for input file discrepancies:

1. ✅ **Cosmetic only** (4 tests) - Trailing whitespace, path differences
2. 🔴 **TPREDICT logic** (5 tests) - Missing conditional defaults for GGT, N3, NRSET
3. 🔴 **k_style='Moon' polynomial fitting** (2 tests) - ConUp1/2 coefficients differ
4. 🔴 **k_style='Bulk' normalization** (1 test) - Wrong formula used
5. 🔴 **Two-layer IC2/FLAY calculation** (1 test) - N1 calculation differences
6. 🔴 **Phobos body defaults** (1 test) - Missing INERTIA default
7. ⚠️ **Missing tests** (2 tests) - Eclipse/PFlux not running

---

## Test-by-Test Analysis

### ✅ Group 1: Perfect Parity (4 tests)
**Only cosmetic differences - no functional impact**

#### Test 1: `porb_defaults_mars`
- **Parameters:** `lat=25.0, KEEP="T"`
- **Differences:**
  - Line 39: Trailing whitespace in PORB matrix
  - Line 98: Output path (`./krc.t52` vs `/tmp/.../outdata.bin.52`)
- **Root Cause:** Cosmetic only
- **Action Required:** None (acceptable)

#### Test 2: `user_param_precedence`
- **Parameters:** `lat=25.0, EMISS=0.95, KEEP="T"`
- **Differences:** Same as Test 1
- **Root Cause:** Cosmetic only
- **Action Required:** None

#### Test 3: `ptotal_forces_taud_zero`
- **Parameters:** `lat=0.0, PTOTAL=0.5, TAUD=0.1, KEEP="T"`
- **Differences:** Same as Test 1
- **Root Cause:** Cosmetic only
- **Action Required:** None

#### Test 4: `mars_with_ls_and_inertia`
- **Parameters:** `lat=12.0, ls=123.0, INERTIA=250.0, KEEP="T"`
- **Differences:** Same as Test 1
- **Root Cause:** Cosmetic only
- **Action Required:** None

---

### 🔴 Group 2: TPREDICT Logic Issues (5 tests)
**Missing conditional logic for GGT, N3, NRSET based on DELJUL**

The tests in this group all show the same pattern:
- PyKRC: `N3=1, NRSET=999, GGT=99.0`
- Davinci: `N3=15, NRSET=3, GGT=0.1`

#### Root Cause Analysis

**Davinci logic ([krc.dvrc:853-865](krc.dvrc#L853-L865)):**
```davinci
if(DELJUL<=3*PERIOD) {
    if(HasValue(TPREDICT)==0) TPREDICT="F"
}
if(HasValue(TPREDICT)==0) TPREDICT="T"

if(TPREDICT=="F") {
    if(HasValue(GGT)==0)   GGT=99.
    if(HasValue(N3)==0)    N3=1
    if(HasValue(NRSET)==0) NRSET=999
} else {
    if(HasValue(GGT)==0)   GGT=atof(key.part1.value[,maxpos(key.part1.key=="GGT")[2]])
    if(HasValue(N3)==0)    N3=atof(key.part2.value[,maxpos(key.part2.key=="N3")[2]])
    if(HasValue(NRSET)==0) NRSET=atof(key.part2.value[,maxpos(key.part2.key=="NRSET")[2]])
}
```

**Key insight:** When `DELJUL > 3*PERIOD`, davinci sets `TPREDICT="T"` (enable prediction), which then causes GGT, N3, NRSET to be loaded from `master.inp` defaults (0.1, 15, 3).

**PyKRC issue:** PyKRC appears to always use the `TPREDICT="F"` defaults, regardless of DELJUL.

#### Affected Tests

##### Test 5: `phobos_default`
- **Parameters:** `lat=12.0, body="Phobos", KEEP="T"`
- **DELJUL:** 1.9085 (calculated from DELLS=1.0)
- **PERIOD:** 0.3189 days (Phobos rotation)
- **Condition:** DELJUL (1.9085) > 3*PERIOD (0.957) → TPREDICT should be "T"
- **Differences:**
  - Line 48: `2 3 1 'N3' /` → `2 3 15 'N3' /`
  - Line 54: `2 9 999 'NRSET' /` → `2 9 3 'NRSET' /`
  - Line 77: `1 39 99.0000 'GGT' /` → `1 39 1.000E-01 'GGT' /`
  - Line 57: Missing `2 16 1 'KPREF' /` changecard (davinci doesn't write it)
  - Lines 61-67: Different material properties (see Group 4)
- **Action Required:** Fix TPREDICT conditional logic in [core.py](krc_python/pykrc/core.py)

##### Test 6: `dells_blocks_deljul`
- **Parameters:** `lat=25.0, DELLS=2.0, KEEP="T"`
- **DELJUL:** 3.8166 (calculated from DELLS=2.0)
- **PERIOD:** 1.026 days (Mars)
- **Condition:** DELJUL (3.8166) > 3*PERIOD (3.078) → TPREDICT should be "T"
- **Differences:**
  - Line 48: `2 3 1 'N3' /` → `2 3 15 'N3' /`
  - Line 54: `2 9 999 'NRSET' /` → `2 9 3 'NRSET' /`
  - Line 78: `1 39 99.0000 'GGT' /` → `1 39 1.000E-01 'GGT' /`
- **Action Required:** Same as Test 5

##### Test 7: `k_style_moon`
- **Parameters:** `lat=12.0, INERTIA=100.0, k_style="Moon", KEEP="T"`
- **Note:** Also has k_style issue (see Group 3)
- **DELJUL:** 1.9083 (default DELLS=1.0)
- **PERIOD:** 1.026 days (Mars)
- **Condition:** DELJUL (1.9083) < 3*PERIOD (3.078) → TPREDICT should be "F"
- **Differences:** None for GGT/N3/NRSET (PyKRC correctly uses "F" defaults)
- **Action Required:** None for TPREDICT logic

##### Test 8: `k_style_bulk`
- **Parameters:** `lat=12.0, INERTIA=100.0, k_style="Bulk", KEEP="T"`
- **Note:** Also has k_style issue (see Group 4)
- **DELJUL:** 1.9083 (default DELLS=1.0)
- **PERIOD:** 1.026 days (Mars)
- **Condition:** DELJUL (1.9083) < 3*PERIOD (3.078) → TPREDICT should be "F"
- **Differences:** None for GGT/N3/NRSET (PyKRC correct)
- **Action Required:** None for TPREDICT logic

##### Test 9: `two_layer_regolith`
- **Parameters:** `lat=0.0, thick=0.3, INERTIA=200.0, INERTIA2=1200.0, LKofT=False, KEEP="T"`
- **Note:** Also has two-layer issues (see Group 5)
- **DELJUL:** 1.9083 (default DELLS=1.0)
- **PERIOD:** 1.026 days (Mars)
- **Condition:** DELJUL (1.9083) < 3*PERIOD (3.078) → TPREDICT should be "F"
- **Differences:** None for GGT/N3/NRSET (PyKRC correct)
- **Action Required:** None for TPREDICT logic

---

### 🔴 Group 3: k_style='Moon' Polynomial Fitting (2 tests)
**ConUp1 and ConUp2 coefficients differ by orders of magnitude**

#### Test 10: `k_style_moon`
- **Parameters:** `lat=12.0, INERTIA=100.0, k_style="Moon", KEEP="T"`
- **Differences:**
  - Line 82: `1 50 -4.045E-19 'ConUp1' /` → `1 50 6.354E-10 'ConUp1' /`
  - Line 83: `1 51 2.515E-18 'ConUp2' /` → `1 51 2.135E-10 'ConUp2' /`
  - Line 85: `1 54 -4.045E-19 'ConLo1' /` → `1 54 6.354E-10 'ConLo1' /`
  - Line 86: `1 55 2.515E-18 'ConLo2' /` → `1 55 2.135E-10 'ConLo2' /`

#### Root Cause Analysis

**Davinci k_style='Moon' logic ([krc.dvrc:622-624](krc.dvrc#L622-L624)):**
```davinci
if(k_style == "Moon"){
  k_Table = COND*(1+2.7*((T_Tab-T_user)/350.)^3)  # Hayne et al. 2017
}
FIT = fit(y=k_Table, x=X, "cube", plot=0)
```

Where:
- `T_Tab = 80 + findgen(200)*2` (80K to 478K)
- `X = (T_Tab - 220) * 0.01`
- `T_user = 220` (default for Mars/generic)
- `COND = INERTIA^2/(DENSITY*SPEC_HEAT)` = 0.01476 (for INERTIA=100)

**Expected polynomial:**
```
k(X) = ConUp0 + ConUp1*X + ConUp2*X^2 + ConUp3*X^3
```

**PyKRC values:** ConUp1 ≈ -4e-19, ConUp2 ≈ 2.5e-18 (essentially zero)
**Davinci values:** ConUp1 ≈ 6.3e-10, ConUp2 ≈ 2.1e-10 (small but non-zero)

**Issue:** PyKRC's cubic fit is producing near-zero coefficients, suggesting either:
1. Numerical precision issues in the fit
2. Wrong X normalization
3. Different fit implementation

#### Test 11: `high_inertia_low_albedo`
- **Parameters:** `lat=0.0, INERTIA=1200.0, ALBEDO=0.05, KEEP="T"`
- **Differences:**
  - Lines 60-61: Tiny rounding difference in DENS2/DENSITY (1890.9091 vs 1890.9092)
- **Root Cause:** Floating point precision (acceptable)
- **Action Required:** None

---

### 🔴 Group 4: k_style='Bulk' Normalization (1 test)
**Completely different ConUp/ConLo coefficients**

#### Test 12: `k_style_bulk`
- **Parameters:** `lat=12.0, INERTIA=100.0, k_style="Bulk", KEEP="T"`
- **Differences:**
  - Line 46: `2 1 25 'N1' /` → `2 1 37 'N1' /`
  - Lines 82-89: All ConUp/ConLo coefficients completely different

#### Root Cause Analysis

**Davinci k_style='Bulk' logic ([krc.dvrc:628-630](krc.dvrc#L628-L630)):**
```davinci
if(k_style == "Bulk"){
  k_user  = Mat_Prop.k.Con0 + Mat_Prop.k.Con1*X_user + Mat_Prop.k.Con2*X_user^2 + Mat_Prop.k.Con3*X_user^3
  k_Table = (Mat_Prop.k.Con0 + Mat_Prop.k.Con1*X + Mat_Prop.k.Con2*X^2 + Mat_Prop.k.Con3*X^3) / k_user
}
FIT = fit(y=k_Table, x=X, "cube", plot=0)
```

**Key:** The bulk conductivity is **normalized by k_user** (conductivity at T_user).

**PyKRC appears to be missing this normalization**, leading to:
- PyKRC ConUp0 = 5.322 (absolute conductivity)
- Davinci ConUp0 = 1.0 (normalized to 1.0 at T_user)

**Action Required:** Fix k_style='Bulk' normalization in [materials.py](krc_python/pykrc/materials.py)

---

### 🔴 Group 5: Two-Layer Regolith (1 test)
**IC2, N1, FLAY, and LKofT changecard differences**

#### Test 13: `two_layer_regolith`
- **Parameters:** `lat=0.0, thick=0.3, INERTIA=200.0, INERTIA2=1200.0, LKofT=False, KEEP="T"`
- **Differences:**
  - Line 41: Missing `3 10 0 'LKofT' /` changecard in PyKRC
  - Line 44: `2 1 50 'N1' /` → `2 1 39 'N1' /`
  - Line 45: `2 2 864 'N2' /` → `2 2 1152 'N2' /`
  - Line 51: `2 8 999 'IC2' /` → `2 8 20 'IC2' /`
  - Line 58: Missing `1 3 200.0000 'INERTIA' /` changecard in PyKRC
  - Line 76: `1 34 1.000E-01 'FLAY' /` → `1 34 8.488E-02 'FLAY' /`

#### Root Cause Analysis

**IC2 issue:**
- IC2 should be the layer index where properties change (calculated from thick)
- PyKRC: 999 (no change)
- Davinci: 20 (layer 20)
- **Cause:** IC2 calculation in `krc_evalN1()` not running or incorrect

**FLAY issue:**
- FLAY is recalculated by `krc_evalN1()` to optimize layer spacing
- PyKRC: 0.1 (default)
- Davinci: 0.08488 (optimized)
- **Cause:** FLAY update from `krc_evalN1()` not being applied

**N1/N2 issue:**
- N1 and N2 are auto-calculated based on skin depth
- Differences suggest `krc_evalN1()` and `krc_evalN2()` returning different values
- **Cause:** Likely related to IC2/FLAY issue above

**LKofT changecard:**
- User explicitly set `LKofT=False`
- PyKRC not writing changecard (but davinci is)
- **Cause:** PyKRC changecard logic not recognizing user-set boolean False

**INERTIA changecard:**
- User explicitly set `INERTIA=200.0`
- PyKRC not writing changecard (but davinci is)
- **Cause:** Same as LKofT - explicit user values not triggering changecards

**Action Required:**
1. Fix IC2 calculation in N1 evaluation
2. Apply FLAY/RLAY updates from `krc_evalN1()`
3. Fix changecard logic for user-set parameters

---

### 🔴 Group 6: Phobos Body Defaults (1 test)
**Missing INERTIA default and material property differences**

#### Test 14: `phobos_default`
- **Parameters:** `lat=12.0, body="Phobos", KEEP="T"`
- **Differences (beyond TPREDICT issues):**
  - Line 61: Missing `1 1 6.700E-01 'ALBEDO' /` in PyKRC (davinci writes it)
  - Line 63: Missing `1 3 100.0000 'INERTIA' /` in PyKRC (davinci writes it)
  - Lines 64-65: COND2/DENS2 differ (different INERTIA defaults?)
  - Line 68: DENSITY differs
  - Line 75: TFROST differs (146.0 vs 0.0)
  - Lines 82-89: All ConUp/ConLo coefficients differ

#### Root Cause Analysis

**INERTIA default:**
- Phobos should default to INERTIA=100.0 (for non-Mars/Europa bodies)
- PyKRC appears to be using a different default or not setting it
- This cascades to all material properties (COND2, DENS2, DENSITY, ConUp/ConLo)

**TFROST default:**
- PyKRC: 146.0 (CO2 frost for Mars)
- Davinci: 0.0 (no frost for airless body)
- **Cause:** PyKRC applying Mars defaults to Phobos

**ALBEDO changecard:**
- Davinci writes ALBEDO=0.67 as changecard
- PyKRC doesn't write it
- **Cause:** PyKRC not recognizing body-specific defaults as needing changecards

**ConUp/ConLo differences:**
- Root cause is different INERTIA → different COND → different k(T) fit

**Action Required:**
1. Fix Phobos (generic body) INERTIA default to 100.0
2. Fix TFROST logic to not apply Mars CO2 frost to airless bodies
3. Ensure body-specific defaults trigger changecards

---

### ⚠️ Group 7: Missing/Failed Tests (2 tests)

#### Test 15: `type14_eclipse_daily`
- **Status:** Output files not found
- **Issue:** Eclipse functionality not implemented or not running
- **Action Required:** Investigate Eclipse changecard generation

#### Test 16: `type15_pflux`
- **Status:** Size mismatch (PyKRC=489,344, Davinci=1,459,712)
- **Issue:** PFlux functionality not generating correct output
- **Action Required:** Investigate PFlux changecard generation

#### Test 17: `user_defaults_europa`
- **Status:** Size mismatch (PyKRC=489,344, Davinci=1,459,712)
- **Issue:** Europa-specific configuration issue
- **Action Required:** Check Europa body setup and N5/JDISK calculations

---

## Root Cause Summary

| Root Cause | Affected Tests | Severity | Location |
|------------|----------------|----------|----------|
| **1. TPREDICT conditional logic** | phobos_default, dells_blocks_deljul | HIGH | [core.py:295-302](krc_python/pykrc/core.py#L295-L302) |
| **2. k_style='Moon' fit precision** | k_style_moon | MEDIUM | [materials.py](krc_python/pykrc/materials.py) k(T) fitting |
| **3. k_style='Bulk' normalization** | k_style_bulk | HIGH | [materials.py](krc_python/pykrc/materials.py) bulk conductivity |
| **4. IC2/FLAY from krc_evalN1()** | two_layer_regolith | HIGH | [core.py:667-678](krc_python/pykrc/core.py#L667-L678) |
| **5. User-set param changecards** | two_layer_regolith | MEDIUM | [executor.py:491-625](krc_python/pykrc/executor.py#L491-L625) |
| **6. Phobos body defaults** | phobos_default | HIGH | [core.py:533-600](krc_python/pykrc/core.py#L533-L600) |
| **7. Eclipse/PFlux generation** | type14_eclipse_daily, type15_pflux, user_defaults_europa | HIGH | [executor.py](krc_python/pykrc/executor.py) changecard gen |

---

## Recommended Fix Order

### Priority 1: Critical Logic Errors
1. **TPREDICT conditional** (affects 2 tests, simple fix)
2. **k_style='Bulk' normalization** (affects 1 test, medium complexity)
3. **Phobos body defaults** (affects 1 test, cascading issues)

### Priority 2: Two-Layer Support
4. **IC2/FLAY from krc_evalN1()** (affects 1 test, complex)
5. **User-set parameter changecards** (affects 1 test, simple fix)

### Priority 3: Numerical Precision
6. **k_style='Moon' fitting** (affects 1 test, low impact)

### Priority 4: Advanced Features
7. **Eclipse/PFlux** (affects 3 tests, requires investigation)

---

## Testing Strategy

After each fix:
1. Run affected integration test(s)
2. Compare input files byte-by-byte
3. Compare output arrays with tolerance=1e-6
4. Document any remaining acceptable differences

---

## Acceptable Differences

The following differences are **acceptable** and do not require fixes:
1. Trailing whitespace in PORB rotation matrix
2. Output file paths (relative vs absolute)
3. Floating point rounding in last decimal place (e.g., 1890.9091 vs 1890.9092)

---

## Davinci Reference Sections

Key davinci code sections for reference:
- **TPREDICT logic:** [krc.dvrc:853-865](krc.dvrc#L853-L865)
- **k_style fitting:** [krc.dvrc:622-664](krc.dvrc#L622-L664)
- **Material properties:** [krc.dvrc:602-648](krc.dvrc#L602-L648)
- **krc_evalN1() call:** [krc.dvrc:871-896](krc.dvrc#L871-L896)
- **Body defaults:** [krc.dvrc:533-600](krc.dvrc#L533-L600)
- **Changecard generation:** [krc.dvrc:1024-1133](krc.dvrc#L1024-L1133)

---

**End of Analysis**
