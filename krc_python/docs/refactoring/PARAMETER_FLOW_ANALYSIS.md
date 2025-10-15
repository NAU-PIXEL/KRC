# KRC Parameter Flow Analysis

## Overview

This document traces every parameter in the `krc()` function signature through the entire codebase to identify:
1. Where defaults are assigned
2. Where values are modified
3. Potential conflicts between `core.py` and `executor.py`
4. PORB-touched parameters
5. Master.inp defaults

**Critical Finding:** There are **CONFLICTS** between defaults set in `core.py` and `MASTER_INP_DEFAULTS` in `executor.py`.

---

## Default Assignment Layers

### Layer 1: Function Signature Defaults (core.py lines 34-167)
Parameters with defaults in the function signature itself.

### Layer 2: core.py Default Assignment (core.py lines 270-331)
Early parameter defaulting logic.

### Layer 3: PORB-Derived Defaults (core.py lines 390-484)
Parameters set when PORB is loaded, tracked in `porb_touched_params`.

### Layer 4: MASTER_INP_DEFAULTS (executor.py lines 36-168)
Hardcoded defaults used for input file header generation.

### Layer 5: Changecard Generation (executor.py lines 491-625)
Logic determining which parameters get changecards written.

---

## Parameter-by-Parameter Analysis

### Legend
- ✅ **Consistent**: Same default across all layers
- ⚠️ **CONFLICT**: Different defaults in different locations
- 🔵 **PORB-touched**: Set by PORB loader
- 🟢 **User-only**: No default, requires user input
- 🟡 **Calculated**: Computed from other parameters

---

## LOCATION & BODY

### `lat` 🟢 User-only
- **Signature:** `None`
- **core.py:** Validated as required (line 334)
- **Status:** Must be provided by user

### `lon` 🟢 User-only
- **Signature:** `None`
- **core.py:** Defaults to `0.0` if not provided (line 337)
- **Status:** Optional, defaults to equator

### `body` ✅ Consistent
- **Signature:** `"Mars"`
- **Used by:** PORB loader, ancillary data
- **Status:** Clean

### `ELEV` 🟡 Calculated
- **Signature:** `None`
- **core.py:** Loaded from ancillary data for Mars (lines 501-504), else `0.0` (line 523)
- **Status:** Auto-loaded for Mars, defaults to 0.0 otherwise

---

## TIME CONTROL

### `ls` 🟢 User-only
- **Signature:** `None`
- **Status:** Optional time specification

### `hour` 🟢 User-only
- **Signature:** `None`
- **Status:** Optional time specification

### `DELLS` ✅ Consistent
- **Signature:** `None`
- **core.py line 270:** `1.0`
- **Usage:** Seasonal time step (degrees Ls)
- **Status:** Clean

### `N5` 🟡 Calculated
- **Signature:** `None`
- **core.py lines 369-371:** Calculated from `DELLS`, `spinup_years`, `output_years`
- **master.inp:** `120`
- **Status:** Auto-calculated unless user overrides

### `JDISK` 🟡 Calculated
- **Signature:** `None`
- **core.py lines 373-374:** Calculated from `DELLS`, `spinup_years`
- **master.inp:** `81`
- **Status:** Auto-calculated unless user overrides

### `spinup_years` ✅ Consistent
- **Signature:** `None`
- **core.py line 272:** `2.0`
- **Status:** Clean

### `output_years` ✅ Consistent
- **Signature:** `None`
- **core.py line 274:** `1.0`
- **Status:** Clean

### `LKEY` ⚠️ **CONFLICT**
- **Signature:** `None`
- **core.py line 276:** `"T"` (True - use Ls for time)
- **master.inp line 159:** `False`
- **Type:** Boolean stored as string
- **CONFLICT:** core.py uses `"T"`, master.inp uses `False`
- **Resolution:** core.py should win (user-facing default), changecard will override master.inp

### `JD` 🟢 User-only
- **Signature:** `None`
- **Status:** Alternative to `ls` (Julian Date)

### `GD` 🟢 User-only
- **Signature:** `None`
- **Status:** Alternative to `ls` (Gregorian Date)

---

## MATERIAL PROPERTIES

### `INERTIA` 🟡 Calculated
- **Signature:** `None`
- **core.py lines 507-510:** Loaded from TES ancillary for Mars
- **core.py line 566:** Defaults to `200.0` if not loaded
- **master.inp:** `200.0`
- **Status:** Consistent fallback

### `k_style` ✅ Consistent
- **Signature:** `"Mars"`
- **Status:** Material conductivity model

### `Mat1` ✅ Consistent
- **Signature:** `"basalt"`
- **Status:** Upper layer material

### `Por1` 🟢 User-only
- **Signature:** `None`
- **Status:** Optional porosity override

### `T_user` ✅ Consistent
- **Signature:** `220.0`
- **Status:** Reference temperature for material properties

### `COND` 🟢 User-only
- **Signature:** `None`
- **Status:** Direct thermal conductivity (alternative to INERTIA)

### `DENSITY` ⚠️ **CONFLICT**
- **Signature:** `None`
- **master.inp:** `1600.0`
- **core.py:** Calculated from materials module
- **Status:** master.inp value only used in header, actual value from materials

### `SPEC_HEAT` ⚠️ **CONFLICT**
- **Signature:** `None`
- **master.inp:** `647.0`
- **core.py:** Calculated from materials module
- **Status:** master.inp value only used in header, actual value from materials

### `LKofT` ⚠️ **CONFLICT**
- **Signature:** `None`
- **core.py line 278:** `True` (enable T-dependent conductivity)
- **core.py line 479:** `True` (PORB-touched)
- **master.inp line 157:** `False`
- **CONFLICT:** core.py defaults to `True`, master.inp defaults to `False`
- **Resolution:** PORB-touched param, changecard will be written

---

## TWO-LAYER REGOLITH

### `thick` ✅ Consistent
- **Signature:** `None`
- **core.py line 280:** `0.0` (single layer)
- **Status:** Clean

### `INERTIA2` 🟡 Calculated
- **Signature:** `None`
- **core.py line 575:** Defaults to `INERTIA` (same as upper layer)
- **Status:** Only different if user specifies

### `Mat2` ✅ Consistent
- **Signature:** `"basalt"`
- **Status:** Lower layer material

### `Por2` 🟢 User-only
- **Signature:** `None`
- **Status:** Optional porosity override

### `IC2` ⚠️ **CONFLICT / PORB-touched**
- **Signature:** `None`
- **core.py line 459:** `999` (PORB-touched)
- **core.py line 678:** Recalculated if `thick != 0`
- **master.inp:** `999`
- **Status:** PORB-touched, changecard written even though value matches master.inp

### `FLAY` ⚠️ **MAJOR CONFLICT**
- **Signature:** `None`
- **core.py line 445:** `0.10` (PORB default)
- **master.inp:** `0.10`
- **OLD COMMENT in core.py:** "# Default: 2.0" (WRONG!)
- **Status:** ✅ Values actually match (0.10), but comment was misleading

### `RLAY` ⚠️ **CONFLICT**
- **Signature:** `None`
- **core.py line 448:** `1.15` (Davinci PORB default)
- **master.inp:** `1.15`
- **OLD COMMENT in core.py:** "# Default: 1.08" (WRONG!)
- **Status:** ✅ Values actually match (1.15), but comment was misleading

### `IIB` ⚠️ **CONFLICT**
- **Signature:** `None`
- **core.py line 454:** `-1` (temperature prediction mode - PORB default)
- **master.inp:** `0`
- **OLD COMMENT in core.py:** "# Default: 2" (WRONG!)
- **CONFLICT:** core.py uses `-1`, master.inp uses `0`
- **Status:** PORB-touched, changecard will override

### `LZONE` ⚠️ **CONFLICT**
- **Signature:** `None`
- **core.py line 483:** `False` (PORB-touched)
- **master.inp line 163:** `False`
- **Status:** PORB-touched, changecard written even though values match

---

## SURFACE PROPERTIES

### `ALBEDO` 🟡 Calculated
- **Signature:** `None`
- **core.py lines 496-499:** Loaded from TES ancillary for Mars
- **core.py line 516:** Defaults to `0.25` from master.inp
- **master.inp:** `0.25`
- **Status:** Can be time-varying array
- **Special:** Type 8 changecard if array

### `EMISS` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 421:** `1.0` (PORB-touched)
- **master.inp:** `1.00`
- **Status:** PORB-touched, changecard written even though values match

### `SLOPE` ⚠️ **PORB-touched CONFLICT**
- **Signature:** `None`
- **core.py line 433:** `0.0` (PORB-touched)
- **master.inp:** `0.0`
- **Status:** PORB-touched, changecard written even though values match

### `SLOAZI` ⚠️ **MAJOR CONFLICT**
- **Signature:** `None`
- **core.py line 436:** `0.0` (PORB-touched)
- **master.inp:** `90.0` ⚠️
- **CONFLICT:** core.py sets to `0.0`, master.inp has `90.0`
- **Resolution:** PORB-touched, changecard will override master.inp

---

## ATMOSPHERE

### `TAUD` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 427:** `0.3` (PORB-touched)
- **master.inp:** `0.3`
- **Status:** Can be time-varying array, PORB-touched

### `PTOTAL` 🔵 **PORB HDF**
- **Signature:** `None`
- **core.py lines 402-404:** Loaded from PORB HDF `krc_params` if available
- **master.inp:** `546.0`
- **Status:** PORB-sourced, changecard written

### `TATM` 🟢 User-only
- **Signature:** `None`
- **master.inp:** `200.0`
- **Status:** Optional atmospheric temperature

### `DUSTA` ⚠️ **CONFLICT**
- **Signature:** `None`
- **core.py lines 411-413:** Loaded from PORB HDF if available (PORB-touched)
- **core.py (old comment):** "# Default: 0.9"
- **master.inp:** `0.90`
- **Status:** PORB HDF override if available

### `TAURAT` ⚠️ **MAJOR CONFLICT**
- **Signature:** `None`
- **core.py lines 408-410:** Loaded from PORB HDF if available (PORB-touched)
- **core.py (old comment):** "# Default: 2.0" (WRONG!)
- **master.inp:** `0.25` ⚠️
- **CONFLICT:** Comment says 2.0, master.inp says 0.25
- **Status:** PORB HDF can override

### `FANON` ⚠️ **MAJOR CONFLICT**
- **Signature:** `None`
- **core.py line 287:** `0.055` (not PORB-related)
- **master.inp:** `0.055`
- **master.inp COMMENT (line 56):** "# NOTE: NOT 0.3!"
- **Status:** ✅ Actually consistent at 0.055

### `KPREF` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 462:** `1` (PORB-touched)
- **master.inp:** `1`
- **Status:** PORB-touched, changecard written

---

## FROST/CONDENSATION

### `LVFT` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 476:** `False` (PORB-touched)
- **master.inp line 156:** `False`
- **Status:** PORB-touched, changecard written even though values match

### `TFROST` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 439:** `146.0` (CO2 frost for Mars, PORB-touched)
- **master.inp:** `146.0`
- **Status:** PORB-touched, changecard written

### `CFROST` 🟢 User-only
- **Signature:** `None`
- **core.py lines 700-703:** Auto-calculated if `LVFT=True`
- **master.inp:** `589944.0`
- **Status:** Only used if frost enabled

### `AFROST` 🟢 User-only
- **Signature:** `None`
- **core.py lines 704-705:** Auto-calculated if `LVFT=True`
- **master.inp:** `0.65`
- **Status:** Only used if frost enabled

### `JBARE` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 465:** `0` (PORB-touched)
- **master.inp:** `0`
- **Status:** PORB-touched, changecard written

---

## NUMERICAL CONTROL

### `N1` 🟡 Calculated
- **Signature:** `None`
- **core.py lines 600-613:** Auto-calculated via `krc_evalN1()` if `auto_numerical=True`
- **core.py line 642:** Defaults to `50` from master.inp if not auto
- **master.inp:** `28`
- **Status:** Usually auto-calculated

### `N2` 🟡 Calculated
- **Signature:** `None`
- **core.py lines 627-636:** Auto-calculated via `krc_evalN2()` if `auto_numerical=True`
- **core.py line 644:** Defaults to `288` from master.inp if not auto
- **master.inp:** `1536`
- **Status:** Usually auto-calculated

### `N3` ⚠️ **COMPLEX**
- **Signature:** `None`
- **core.py line 288:** `1` (matches Davinci default)
- **core.py line 302:** Set to `1` if `TPREDICT=0.0`
- **core.py lines 667-668:** May be updated by convergence params
- **master.inp:** `15`
- **OLD COMMENT:** "# Default: 10" (WRONG!)
- **Status:** Complex logic depending on TPREDICT

### `NRSET` ⚠️ **COMPLEX**
- **Signature:** `None`
- **core.py line 291:** `0`
- **core.py line 302:** Set to `999` if `TPREDICT=0.0`
- **master.inp:** `3`
- **Status:** Depends on TPREDICT

### `GGT` ⚠️ **COMPLEX**
- **Signature:** `None`
- **core.py line 293:** `1.0`
- **core.py line 300:** Set to `99.0` if `TPREDICT=0.0`
- **master.inp:** `0.1`
- **Status:** Depends on TPREDICT

### `TPREDICT` ✅ Consistent
- **Signature:** `None`
- **core.py line 295:** `0.0` (disable prediction)
- **Status:** Clean

### `MAXN1` ✅ Consistent
- **Signature:** `None`
- **core.py line 304:** `100`
- **Status:** Upper bound for N1 calculation

### `MAXN2` ✅ Consistent
- **Signature:** `None`
- **core.py line 306:** `1000`
- **Status:** Upper bound for N2 calculation

### `auto_numerical` ✅ Consistent
- **Signature:** `None`
- **core.py line 308:** `True`
- **Status:** Enable auto N1/N2 calculation

---

## MODEL PARAMETERS

### `TDEEP` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 424:** `180.0` (PORB-touched - standard for Mars)
- **master.inp:** `180.0`
- **Status:** PORB-touched, changecard written even though values match

### `DJUL` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 430:** `0.1` (PORB default, NOT 0.0)
- **master.inp:** `-1222.69`
- **CONFLICT:** core.py uses `0.1`, master.inp uses `-1222.69`
- **Status:** PORB-touched, changecard will override

### `bodyforce` ✅ Consistent
- **Signature:** `None`
- **core.py line 311:** `0` (use cached PORB)
- **Status:** Clean

---

## OUTPUT CONTROL

### `TUN8` ✅ Consistent
- **Signature:** `None`
- **core.py line 313:** `0` (no depth profile output)
- **Status:** Clean

### `LMST` ✅ Consistent
- **Signature:** `None`
- **core.py line 315:** `"F"` (use LTST, not LMST)
- **Status:** Clean

### `WRITE` ✅ Consistent
- **Signature:** `None`
- **core.py line 317:** `"F"` (no detailed output)
- **Status:** Clean

### `KEEP` ✅ Consistent
- **Signature:** `None`
- **core.py line 319:** `"F"` (don't keep temp files)
- **Status:** Clean

---

## ADVANCED PHYSICS

### `PhotoFunc` ⚠️ **PORB-touched**
- **Signature:** `None`
- **core.py line 442:** `0.0` (Lambert photometric function, PORB-touched)
- **master.inp:** `0.0`
- **Status:** PORB-touched, changecard written

---

## ORBITAL PARAMETER OVERRIDES

### `GRAV` 🔵 **PORB HDF**
- **Signature:** `None`
- **core.py lines 405-407:** Loaded from PORB HDF if available
- **master.inp:** `3.727` (Mars)
- **Status:** PORB-sourced

### `DAU` 🟢 User-only
- **Signature:** `None`
- **master.inp:** `1.465` (Mars)
- **Status:** Optional override

### `SOLCON` 🟢 User-only
- **Signature:** `None`
- **master.inp:** `1368.0`
- **Status:** Optional override

### `SOLARDEC` 🟢 User-only
- **Signature:** `None`
- **master.inp:** `0.0`
- **Status:** Optional override

### `ARC2_G0` 🔵 **PORB HDF**
- **Signature:** `None`
- **core.py lines 414-416:** Loaded from PORB HDF if available
- **master.inp:** `0.5`
- **Status:** PORB-sourced

### `LsubS` 🟢 User-only
- **Signature:** `None`
- **master.inp:** `0.0`
- **Status:** Optional override

### `Atm_Cp` 🟢 User-only
- **Signature:** `None`
- **master.inp:** `735.9` (Mars)
- **Status:** Optional override

---

## ADVANCED COMPUTATIONAL

### `stability` 🟢 User-only
- **Signature:** `None`
- **Status:** Stability analysis flag

### `anc` 🟢 User-only
- **Signature:** `None`
- **Status:** Ancillary data dictionary

---

## ECLIPSE MODELING

### `Eclipse` ✅ Consistent
- **Signature:** `None`
- **core.py line 321:** `"F"` (disabled)
- **Status:** Clean

### `Eclipse_Style` ✅ Consistent
- **Signature:** `None`
- **core.py line 323:** `1.0` (daily)
- **Status:** Clean

### `Eclipser` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `Sun_Dis` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `Eclipser_Rad` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `Eclipsed_Rad` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `CM` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `Gamma` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `Date` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

### `Eclipse_line` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if Eclipse="T"

---

## PLANETARY FLUX

### `PFlux` ✅ Consistent
- **Signature:** `None`
- **core.py line 325:** `"F"` (disabled)
- **Status:** Clean

### `BT_Avg` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if PFlux="T"

### `BT_Min` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if PFlux="T"

### `BT_Max` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if PFlux="T"

### `Lon_Hr` ✅ Consistent
- **Signature:** `None`
- **core.py line 327:** `12.0`
- **Status:** Clean

### `IR` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if PFlux="T"

### `Vis` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if PFlux="T"

### `Emissivity` 🟢 User-only
- **Signature:** `None`
- **Status:** Only used if PFlux="T"

---

## EXECUTION OPTIONS

### `verbose` ✅ Consistent
- **Signature:** `None`
- **core.py line 329:** `False`
- **Status:** Clean

### `workdir` 🟢 User-only
- **Signature:** `None`
- **Status:** Optional working directory

### `keep_files` ✅ Consistent
- **Signature:** `None`
- **core.py line 331:** `False`
- **Status:** Clean

---

## INTERNAL PARAMETERS (Not in krc() signature)

### `K4OUT` 🔒 Internal
- **Set in:** core.py line 469, params dict line 950
- **Value:** `52` (bin52 output format)
- **master.inp:** `52`
- **Status:** Fixed, not user-configurable

### `TUN_Flx15` 🔒 Internal
- **Set in:** core.py line 470
- **Value:** `0`
- **master.inp:** `65`
- **CONFLICT:** core.py uses `0`, master.inp uses `65`
- **Status:** PORB-touched (line 472)

### `LPORB` 🔒 Internal
- **Set in:** Not in params dict
- **master.inp line 158:** `True`
- **Status:** Always True for pykrc

### `N24` 🟡 Calculated
- **Set in:** core.py lines 360-365 (from rotation period)
- **master.inp:** `48`
- **Status:** Calculated from PORB

### `DELJUL` 🟡 Calculated
- **Set in:** core.py lines 378-388 (from orbital period)
- **master.inp:** `17.174822`
- **Status:** Calculated from PORB

---

## CRITICAL CONFLICTS SUMMARY

### 🔴 **MAJOR CONFLICTS** (Different values, different behavior)

1. **SLOAZI**
   - core.py (PORB): `0.0`
   - master.inp: `90.0`
   - **Impact:** 90° difference in slope azimuth!
   - **Resolution:** Changecard overrides master.inp

2. **DJUL**
   - core.py (PORB): `0.1`
   - master.inp: `-1222.69`
   - **Impact:** Completely different Julian date offset
   - **Resolution:** Changecard overrides master.inp

3. **TUN_Flx15**
   - core.py: `0`
   - master.inp: `65`
   - **Impact:** Different flux tuning
   - **Resolution:** Changecard overrides master.inp

4. **IIB**
   - core.py (PORB): `-1` (temperature prediction mode)
   - master.inp: `0` (standard mode)
   - **Impact:** Different integration algorithm
   - **Resolution:** Changecard overrides master.inp

5. **N3, NRSET, GGT** (when TPREDICT=0.0)
   - core.py sets special values for stability
   - master.inp has different defaults
   - **Impact:** Numerical stability behavior
   - **Resolution:** Changecards override master.inp

### ⚠️ **MISLEADING COMMENTS** (Comments wrong, actual values OK)

1. **FLAY** - Comment said "Default: 2.0", actual: `0.10` ✅
2. **RLAY** - Comment said "Default: 1.08", actual: `1.15` ✅
3. **IIB** - Comment said "Default: 2", actual: `-1` ✅
4. **N3** - Comment said "Default: 10", actual: `1` ✅
5. **TAURAT** - Comment said "Default: 2.0", actual: `0.25` ✅

### ✅ **PORB-TOUCHED BUT MATCHING** (Changecard written even though values match)

These parameters are PORB-touched, so changecards are always written even when values match master.inp:

- EMISS: `1.0` (both)
- TDEEP: `180.0` (both)
- TAUD: `0.3` (both)
- SLOPE: `0.0` (both)
- TFROST: `146.0` (both)
- LVFT: `False` (both)
- LKofT: Conflict! core.py=`True`, master.inp=`False`
- LZONE: `False` (both)
- KPREF: `1` (both)
- JBARE: `0` (both)
- PhotoFunc: `0.0` (both)
- IC2: `999` (both)

---

## REFACTORING IMPLICATIONS

### Critical Decisions for Refactoring

1. **Where should defaults live?**
   - **Recommendation:** Create single source of truth in new `defaults.py` module
   - Keep MASTER_INP_DEFAULTS in executor.py for header generation only
   - All actual defaults should come from defaults.py

2. **PORB-touched parameter handling**
   - **Recommendation:** Extract to `_setup_porb_defaults()` helper
   - Return tuple: `(params_dict, porb_touched_set)`
   - Clear separation of PORB logic from user defaults

3. **Changecard logic**
   - **Current:** executor.py determines what to write based on comparison
   - **Keep as-is:** This logic is tightly coupled to Fortran input format
   - **Improve:** Better documentation of why PORB-touched params get changecards

4. **Material property defaults**
   - **Current:** Calculated in core.py, separate from MASTER_INP_DEFAULTS
   - **Keep as-is:** Material properties are computed, not static defaults
   - **Clarify:** Document that MASTER_INP_DEFAULTS are header-only

### Suggested Module Structure

```
pykrc/
  defaults.py          # NEW: Single source of truth for all defaults
  core.py              # Simplified, delegates to helpers
  executor.py          # Only contains MASTER_INP_DEFAULTS for header
  porb.py              # Existing PORB loader
  materials.py         # Existing material property calculator
  numerical.py         # Existing N1/N2 calculators
```

### defaults.py Structure

```python
# defaults.py
"""
Canonical defaults for all KRC parameters.

This is the single source of truth for default values.
MASTER_INP_DEFAULTS in executor.py should ONLY be used for
generating the input file header.
"""

# User-facing defaults (what krc() function uses)
USER_DEFAULTS = {
    # Time control
    'DELLS': 1.0,
    'spinup_years': 2.0,
    'output_years': 1.0,
    'LKEY': 'T',  # Use Ls (not Julian date)

    # Material
    'LKofT': True,  # Enable T-dependent properties
    'thick': 0.0,   # Single layer
    'k_style': 'Mars',
    'Mat1': 'basalt',
    'Mat2': 'basalt',
    'T_user': 220.0,

    # ... etc
}

# PORB-derived defaults (set when PORB is loaded)
PORB_DEFAULTS = {
    'EMISS': 1.0,
    'TDEEP': 180.0,
    'TAUD': 0.3,
    'DJUL': 0.1,      # NOT 0.0, NOT -1222.69 from master.inp
    'SLOPE': 0.0,
    'SLOAZI': 0.0,    # NOT 90.0 from master.inp
    'TFROST': 146.0,
    'PhotoFunc': 0.0,
    'FLAY': 0.10,
    'RLAY': 1.15,
    'IIB': -1,        # NOT 0 from master.inp
    'IC2': 999,
    'KPREF': 1,
    'JBARE': 0,
    'LVFT': False,
    'LKofT': True,    # NOT False from master.inp
    'LZONE': False,
    'K4OUT': 52,
    'TUN_Flx15': 0,   # NOT 65 from master.inp
}

# Parameters that should trigger changecards if set by PORB
PORB_TOUCHED_PARAMS = set(PORB_DEFAULTS.keys())
```

---

## TESTING STRATEGY

To validate refactoring doesn't break defaults:

1. **Snapshot all changecard outputs**
   ```python
   # For each test case, save:
   # - Which changecards are written
   # - What values they contain
   # - Ensure PORB-touched params always get changecards
   ```

2. **Validate against Davinci**
   ```bash
   # Compare input files between pykrc and davinci
   # Ensure changecard lists are identical
   ```

3. **Test default inheritance**
   ```python
   # Test that user params override PORB defaults
   # Test that PORB defaults override master.inp
   # Test that master.inp is only used for header
   ```

---

## ACTION ITEMS FOR REFACTORING

- [ ] Create `defaults.py` module with canonical defaults
- [ ] Extract `_apply_user_defaults()` helper
- [ ] Extract `_apply_porb_defaults()` helper
- [ ] Update misleading comments in core.py
- [ ] Add validation tests for default precedence
- [ ] Document why PORB-touched params get changecards
- [ ] Consider renaming `MASTER_INP_DEFAULTS` to `MASTER_INP_HEADER_VALUES`
- [ ] Add assertions that calculated defaults match expectations
