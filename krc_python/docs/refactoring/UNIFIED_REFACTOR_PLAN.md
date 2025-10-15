# Unified Refactoring Plan: core.py + executor.py

## Executive Summary

After analyzing both `core.py` and `executor.py`, we've discovered that **defaults are being set in multiple locations** with some conflicts. This unified plan addresses both modules simultaneously to eliminate duplication and establish a single source of truth.

**Key Findings:**
- ⚠️ 5 **MAJOR CONFLICTS** between core.py and executor.py defaults
- ⚠️ 5 **MISLEADING COMMENTS** that don't match actual values
- ✅ 13 **PORB-touched parameters** that need changecards even when matching master.inp
- 🎯 Current code ~1,710 lines (core: 963 + executor: 747)
- 🎯 Target: ~900-1,000 lines (core: 200 + executor: 400 + new helpers: 300-400)

---

## Problem: Multiple Sources of Truth

### Current Default Assignment Flow (BROKEN)

```
User Input (krc())
    ↓
core.py Early Defaults (lines 270-331)
    ↓
PORB Defaults (lines 390-484)  ← CONFLICTS with master.inp!
    ↓
Material Property Calculation
    ↓
Parameter Dict Building (lines 707-920)
    ↓
executor.run_krc(params)
    ↓
executor.create_input_file()
    ↓
    MASTER_INP_DEFAULTS (lines 36-168)  ← Different values!
    ↓
Changecard Generation (lines 491-625)
    ↓
Input File Written
```

### Problems

1. **SLOAZI**: core.py uses `0.0`, master.inp uses `90.0` → 90° error!
2. **DJUL**: core.py uses `0.1`, master.inp uses `-1222.69` → Wrong epoch!
3. **IIB**: core.py uses `-1`, master.inp uses `0` → Different algorithm!
4. **LKofT**: core.py uses `True`, master.inp uses `False` → Different physics!
5. **TUN_Flx15**: core.py uses `0`, master.inp uses `65` → Different tuning!

**Impact:** Changecards save the day (override master.inp), but the code is confusing and error-prone.

---

## Solution: Single Source of Truth Architecture

### New Module Structure

```
pykrc/
├── defaults.py          ← NEW: Canonical defaults
├── porb_handler.py      ← NEW: PORB-specific logic
├── core.py              ← REFACTORED: Orchestration only (~200 lines)
├── executor.py          ← REFACTORED: I/O only (~400 lines)
├── materials.py         ← EXISTING: No changes
├── numerical.py         ← EXISTING: No changes
└── validation.py        ← EXISTING: No changes
```

---

## Phase 0: Create New Modules (1 hour)

### 0.1 Create defaults.py

```python
"""
Canonical default values for all KRC parameters.

This is the SINGLE SOURCE OF TRUTH for defaults.
Other modules should import from here, not define their own defaults.

Three levels of defaults:
1. USER_DEFAULTS: Initial defaults when krc() is called
2. PORB_DEFAULTS: Values set when PORB is loaded (override user defaults)
3. MASTER_INP_HEADER_DEFAULTS: Values used ONLY for input file header
                                 (actual values come from changecards)
"""

from typing import Dict, Any, Set

# ============================================================================
# USER-FACING DEFAULTS (krc() function defaults)
# ============================================================================

USER_DEFAULTS: Dict[str, Any] = {
    # Time control
    'DELLS': 1.0,           # Seasonal step (degrees Ls)
    'spinup_years': 2.0,    # Spinup time
    'output_years': 1.0,    # Output time
    'LKEY': 'T',            # Use Ls (not Julian date)

    # Material properties
    'LKofT': True,          # Enable T-dependent conductivity
    'thick': 0.0,           # Single layer (not two-layer)
    'k_style': 'Mars',      # Conductivity model
    'Mat1': 'basalt',       # Upper layer material
    'Mat2': 'basalt',       # Lower layer material
    'T_user': 220.0,        # Reference temperature (K)
    'body': 'Mars',         # Default planet

    # Numerical control
    'N3': 1,                # Convergence iterations (Davinci default)
    'NRSET': 0,             # Reset counter
    'GGT': 1.0,             # Gradient timestep control
    'TPREDICT': 0.0,        # Temperature prediction (disabled)
    'MAXN1': 100,           # Max subsurface layers
    'MAXN2': 1000,          # Max timesteps/day
    'auto_numerical': True, # Auto-calculate N1/N2

    # Atmosphere
    'FANON': 0.055,         # Atmospheric anisotropy (NOT 0.3!)

    # Model control
    'bodyforce': 0,         # Use cached PORB (don't recalculate)
    'TUN8': 0,              # Depth profile output (disabled)
    'LMST': 'F',            # Use LTST (not LMST)
    'WRITE': 'F',           # No detailed output files
    'KEEP': 'F',            # Don't keep temp files

    # Eclipse/PFlux
    'Eclipse': 'F',         # Eclipse disabled
    'Eclipse_Style': 1.0,   # Daily eclipses
    'PFlux': 'F',           # Planetary flux disabled
    'Lon_Hr': 12.0,         # Longitude hour

    # Execution
    'verbose': False,       # Quiet mode
    'keep_files': False,    # Clean up temp files
}


# ============================================================================
# PORB-DERIVED DEFAULTS (set when PORB is loaded)
# ============================================================================
# These values are set whenever PORB is loaded, ensuring changecards are
# written even if they match master.inp header defaults.
# This matches Davinci krc.dvrc behavior.

PORB_DEFAULTS: Dict[str, Any] = {
    # Surface properties
    'EMISS': 1.0,           # Standard emissivity
    'SLOPE': 0.0,           # No slope
    'SLOAZI': 0.0,          # Slope azimuth (NOT 90.0 from master.inp!)

    # Thermal
    'TDEEP': 180.0,         # Deep temperature (K) for Mars
    'TFROST': 146.0,        # CO2 frost temperature (K)

    # Atmosphere
    'TAUD': 0.3,            # Dust optical depth
    'DJUL': 0.1,            # Julian date offset (NOT 0.0, NOT -1222.69!)

    # Physics
    'PhotoFunc': 0.0,       # Lambert photometric function

    # Layer properties
    'FLAY': 0.10,           # Layer spacing factor (Davinci default)
    'RLAY': 1.15,           # Layer thickness ratio (Davinci default)
    'IIB': -1,              # Temperature prediction mode (NOT 0!)
    'IC2': 999,             # Two-layer interface (recalculated if thick != 0)

    # Flags
    'LVFT': False,          # Frost disabled by default
    'LKofT': True,          # T-dependent properties enabled (NOT False!)
    'LZONE': False,         # No zone file
    'KPREF': 1,             # Pressure reference (Viking)
    'JBARE': 0,             # No forced bare ground

    # Internal
    'K4OUT': 52,            # bin52 output format
    'TUN_Flx15': 0,         # Flux tuning (NOT 65 from master.inp!)
}


# ============================================================================
# PORB-TOUCHED PARAMETERS
# ============================================================================
# These parameters should ALWAYS have changecards written when PORB is loaded,
# even if their values match master.inp header defaults.
# This ensures proper Davinci parity.

PORB_TOUCHED_PARAMS: Set[str] = {
    'EMISS', 'TDEEP', 'TAUD', 'DJUL', 'SLOPE', 'SLOAZI', 'TFROST',
    'PhotoFunc', 'FLAY', 'RLAY', 'IIB', 'IC2', 'KPREF', 'JBARE',
    'LVFT', 'LKofT', 'LZONE', 'K4OUT', 'TUN_Flx15'
}


# ============================================================================
# TPREDICT STABILITY MODE
# ============================================================================
# When TPREDICT=0.0, special stability values are set
# (Davinci krc.dvrc lines 841-847)

TPREDICT_STABILITY_OVERRIDES: Dict[str, Any] = {
    'GGT': 99.0,    # Disable gradient timestep control
    'N3': 1,        # Single iteration
    'NRSET': 999,   # Disable reset
}


# ============================================================================
# MASTER.INP HEADER DEFAULTS
# ============================================================================
# These values are used ONLY for writing the input file header.
# They match the master.inp file format from KRC Fortran.
#
# IMPORTANT: Actual parameter values come from changecards!
# These header values are just placeholders that KRC expects.
#
# This dict should ONLY be used in executor.py for header generation.

MASTER_INP_HEADER_DEFAULTS: Dict[str, Any] = {
    # Line 1: KOLD KEEP
    "KOLD": 0,
    "KEEP": 0,

    # Line 3-4: Material properties block 1
    "ALBEDO": 0.25,
    "EMISS": 1.00,
    "INERTIA": 200.0,
    "COND2": 2.77,
    "DENS2": 928.0,
    "PERIOD": 1.0275,
    "SPEC_HEAT": 647.0,
    "DENSITY": 1600.0,

    # Line 5-6: Atmosphere and thermal properties
    "CABR": 0.11,
    "AMW": 43.5,
    "SatPrA": 27.9546,
    "PTOTAL": 546.0,
    "FANON": 0.055,
    "TATM": 200.0,
    "TDEEP": 180.0,
    "SpHeat2": 1711.0,

    # Line 7-8: Atmospheric opacity and scattering
    "TAUD": 0.3,
    "DUSTA": 0.90,
    "TAURAT": 0.25,
    "TWILI": 0.0,
    "ARC2_G0": 0.5,
    "ARC3_Safe": 0.801,
    "SLOPE": 0.0,
    "SLOAZI": 90.0,    # Note: PORB overrides this to 0.0!

    # Line 9-10: Frost properties
    "TFROST": 146.0,
    "CFROST": 589944.0,
    "AFROST": 0.65,
    "FEMIS": 0.95,
    "AF1": 0.54,
    "AF2": 0.0009,
    "FROEXT": 50.0,
    "SatPrB": 3182.48,

    # Line 11-12: Layer and convergence
    "RLAY": 1.15,
    "FLAY": 0.10,
    "CONVF": 3.0,
    "DEPTH": 0.0,
    "DRSET": 0.0,
    "PhotoFunc": 0.0,
    "GGT": 0.1,
    "DTMAX": 0.1,

    # Line 13-14: Orbital and solar
    "DJUL": -1222.69,
    "DELJUL": 17.174822,
    "SOLARDEC": 0.0,
    "DAU": 1.465,
    "LsubS": 0.0,
    "SOLCON": 1368.0,
    "GRAV": 3.727,
    "Atm_Cp": 735.9,

    # Line 15-16: Conductivity polynomial coefficients (upper layer)
    "ConUp0": 0.038640,
    "ConUp1": -0.002145,
    "ConUp2": 0.002347,
    "ConUp3": -0.000750,
    "ConLo0": 2.766722,
    "ConLo1": -1.298966,
    "ConLo2": 0.629224,
    "ConLo3": -0.527291,

    # Line 17-18: Specific heat polynomial coefficients
    "SphUp0": 646.6275,
    "SphUp1": 246.6678,
    "SphUp2": -49.8216,
    "SphUp3": 7.9520,
    "SphLo0": 1710.648,
    "SphLo1": 721.8740,
    "SphLo2": 57.44873,
    "SphLo3": 24.37532,

    # Line 19-20: Grid parameters
    "N1": 28,
    "N2": 1536,
    "N3": 15,
    "N4": 19,
    "N5": 120,
    "N24": 48,
    "IIB": 0,          # Note: PORB overrides this to -1!
    "IC2": 999,

    # Line 21-22: Run control
    "NRSET": 3,
    "NMHA": 24,
    "NRUN": 0,
    "JDISK": 81,
    "IDOWN": 0,
    "FlxP14": 45,
    "TUN_Flx15": 65,   # Note: PORB overrides this to 0!
    "KPREF": 1,

    # Line 23-24: Output control
    "K4OUT": 52,
    "JBARE": 0,
    "Notif": 50,
    "IDISK2": 0,

    # Booleans (line 25-28)
    "LP1": False,
    "LP2": True,
    "LP3": False,
    "LP4": False,
    "LP5": False,
    "LP6": False,
    "LPGLOB": False,
    "LVFA": False,
    "LVFT": False,
    "LKofT": False,    # Note: PORB overrides this to True!
    "LPORB": True,
    "LKEY": False,
    "LSC": False,
    "LZONE": False,
    "LOCAL": True,
    "Prt76": False,
    "LPTAVE": False,
    "Prt78": False,
    "Prt79": False,
    "L_ONE": False,
}


def apply_tpredict_overrides(params: Dict[str, Any]) -> Dict[str, Any]:
    """
    Apply stability overrides if TPREDICT=0.0.

    When temperature prediction is disabled, special values are set
    for numerical stability (matches Davinci krc.dvrc lines 841-847).

    Parameters
    ----------
    params : dict
        Current parameter dictionary

    Returns
    -------
    dict
        Updated parameters with stability overrides if needed
    """
    if params.get('TPREDICT', 0.0) == 0.0:
        params.update(TPREDICT_STABILITY_OVERRIDES)
    return params


def get_porb_defaults() -> Dict[str, Any]:
    """
    Get PORB-derived defaults.

    Returns
    -------
    dict
        Copy of PORB defaults
    """
    return PORB_DEFAULTS.copy()


def get_porb_touched_params() -> Set[str]:
    """
    Get set of parameter names touched by PORB.

    These parameters should always have changecards written,
    even if their values match master.inp header defaults.

    Returns
    -------
    set
        Set of PORB-touched parameter names
    """
    return PORB_TOUCHED_PARAMS.copy()
```

### 0.2 Create porb_handler.py

```python
"""
PORB parameter handling.

Encapsulates all PORB-related logic:
- Loading orbital parameters
- Setting PORB-derived defaults
- Calculating N24, N5, JDISK, DELJUL
- Tracking PORB-touched parameters
"""

from typing import Dict, Any, Optional, Tuple, Set
import numpy as np

from .orbital import porb, OrbitalElements
from .data_loaders import KRCDataLoader
from .defaults import get_porb_defaults, get_porb_touched_params


def setup_orbital_parameters(
    body: str,
    data_loader: KRCDataLoader,
    DELLS: float,
    N5: Optional[int],
    JDISK: Optional[int],
    spinup_years: float,
    output_years: float,
    user_params: Dict[str, Any],
    verbose: bool
) -> Tuple[OrbitalElements, Dict[str, Any], Set[str]]:
    """
    Load orbital parameters and apply PORB-derived defaults.

    Parameters
    ----------
    body : str
        Celestial body name
    data_loader : KRCDataLoader
        Data loader instance
    DELLS : float
        Seasonal step size (degrees Ls)
    N5 : int or None
        Number of seasonal steps (calculated if None)
    JDISK : int or None
        Disk output start (calculated if None)
    spinup_years : float
        Spinup time (years)
    output_years : float
        Output time (years)
    user_params : dict
        Parameters explicitly set by user
    verbose : bool
        Print details

    Returns
    -------
    body_params : OrbitalElements
        Orbital/physical parameters for body
    porb_params : dict
        Parameters set by PORB (with calculated values)
    porb_touched : set
        Set of parameter names touched by PORB

    Notes
    -----
    Implements Davinci krc.dvrc behavior where PORB sets defaults
    that override master.inp, ensuring changecards are written.
    """
    if verbose:
        print(f"Loading orbital parameters for {body}...")

    # Load PORB data
    body_params = porb(body, data_loader=data_loader)

    # Calculate derived parameters
    rot_per = body_params.rotation_period
    n24_from_porb = int(np.floor((rot_per * 4) / 96) * 96)
    if n24_from_porb < 96:
        n24_from_porb = 96

    if N5 is None:
        total_years = spinup_years + output_years
        N5 = int(np.ceil(360.0 / DELLS * total_years))

    if JDISK is None:
        JDISK = int(np.ceil(360.0 / DELLS * spinup_years + 1))

    # Calculate DELJUL
    if hasattr(body_params, 'krc_params') and 'DELJUL' in body_params.krc_params:
        DELJUL_calc = body_params.krc_params['DELJUL']
    elif hasattr(body_params, 'orbital_period'):
        DELJUL_calc = body_params.orbital_period * DELLS / 360.0
    else:
        DELJUL_calc = rot_per * DELLS / 360.0
        if verbose:
            print(f"Warning: Using rotation period for DELJUL (may be inaccurate)")

    if verbose:
        print(f"Time control: DELLS={DELLS}°, N5={N5} seasons, JDISK={JDISK}")
        print(f"  Total run: {N5*DELLS/360:.1f} years, "
              f"Output: {(N5-JDISK)*DELLS/360:.1f} years")

    # Get PORB defaults
    porb_params = get_porb_defaults()
    porb_touched = get_porb_touched_params()

    # Override with PORB HDF data if available
    if hasattr(body_params, 'krc_params') and body_params.krc_params:
        krc_params = body_params.krc_params

        for param_name in ['PTOTAL', 'GRAV', 'TAURAT', 'DUSTA', 'ARC2_G0']:
            if param_name in krc_params and param_name not in user_params:
                porb_params[param_name] = krc_params[param_name]
                porb_touched.add(param_name)

    # Add calculated values
    porb_params['N24'] = n24_from_porb
    porb_params['N5'] = N5
    porb_params['JDISK'] = JDISK
    porb_params['DELJUL'] = DELJUL_calc
    porb_params['PERIOD'] = rot_per

    return body_params, porb_params, porb_touched
```

---

## Phase 1: Refactor core.py (3 hours)

See `CORE_REFACTOR.md` for detailed extraction plan, but with these changes:

1. Import from `defaults.py` instead of hardcoding
2. Use `porb_handler.py` for PORB logic
3. Remove all duplicate default assignments
4. Update misleading comments

### Key Changes

```python
# OLD (core.py lines 270-331):
if DELLS is None:
    DELLS = 1.0
if spinup_years is None:
    spinup_years = 2.0
# ... 60 more lines

# NEW:
from .defaults import USER_DEFAULTS, apply_tpredict_overrides
from .porb_handler import setup_orbital_parameters

defaults = {k: v for k, v in USER_DEFAULTS.items() if locals().get(k) is None}
locals().update(defaults)
apply_tpredict_overrides(locals())
```

---

## Phase 2: Refactor executor.py (2 hours)

### 2.1 Import from defaults.py

```python
# OLD (executor.py lines 36-168):
MASTER_INP_DEFAULTS = {
    "KOLD": 0,
    "KEEP": 0,
    # ... 130 more lines
}

# NEW (executor.py lines 1-20):
"""Execution engine for running KRC Fortran binary."""

import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
import platform

from .config import get_krc_executable, get_paths
from .defaults import (
    MASTER_INP_HEADER_DEFAULTS as MASTER_INP_DEFAULTS,
    PORB_TOUCHED_PARAMS
)

# Note: Renamed MASTER_INP_DEFAULTS → MASTER_INP_HEADER_DEFAULTS in defaults.py
# for clarity, but kept as MASTER_INP_DEFAULTS here for minimal code changes
```

### 2.2 Update changecard logic to use imported PORB_TOUCHED_PARAMS

```python
# OLD (executor.py line 509):
porb_touched = params.get('_porb_touched_params', set())

# NEW:
# PORB-touched params are now canonical in defaults.py
# Still allow override via params dict for flexibility
porb_touched = params.get('_porb_touched_params', PORB_TOUCHED_PARAMS.copy())
```

### 2.3 Add documentation about header vs. changecard values

```python
def create_input_file(...):
    """
    Create KRC input file from parameters.

    IMPORTANT: Input file structure
    - Header lines (lines 1-28): Use MASTER_INP_DEFAULTS
      These are just placeholders that match master.inp format
    - Changecards (after line 28): Override header values
      These contain the ACTUAL parameter values to use

    Why this design?
    - KRC Fortran expects specific header format from master.inp
    - Changecards modify parameters at runtime
    - PORB-touched params always get changecards for Davinci parity

    ... rest of docstring ...
    """
```

---

## Phase 3: Update Misleading Comments (15 min)

Fix all the wrong comments identified in PARAMETER_FLOW_ANALYSIS.md:

```python
# core.py OLD:
FLAY: Optional[float] = None,  # Default: 2.0
RLAY: Optional[float] = None,  # Default: 1.08
IIB: Optional[int] = None,  # Default: 2
N3: Optional[int] = None,  # Default: 10
TAURAT: Optional[float] = None,  # Default: 2.0

# NEW:
FLAY: Optional[float] = None,  # PORB default: 0.10 (layer spacing factor)
RLAY: Optional[float] = None,  # PORB default: 1.15 (layer thickness ratio)
IIB: Optional[int] = None,  # PORB default: -1 (temperature prediction mode)
N3: Optional[int] = None,  # Default: 1 (Davinci default)
TAURAT: Optional[float] = None,  # PORB default: 0.25 (optical depth ratio)
```

---

## Phase 4: Testing (2 hours)

### 4.1 Baseline Comparison Tests

```python
# tests/test_default_consistency.py
"""Test that defaults are consistent across modules."""

import pytest
from pykrc.defaults import (
    USER_DEFAULTS,
    PORB_DEFAULTS,
    MASTER_INP_HEADER_DEFAULTS,
    PORB_TOUCHED_PARAMS
)


def test_porb_touched_params_are_in_porb_defaults():
    """All PORB-touched params should have PORB defaults."""
    for param in PORB_TOUCHED_PARAMS:
        assert param in PORB_DEFAULTS, \
            f"{param} in PORB_TOUCHED_PARAMS but not in PORB_DEFAULTS"


def test_known_conflicts_are_documented():
    """Verify known conflicts between PORB and master.inp."""
    conflicts = {
        'SLOAZI': (PORB_DEFAULTS['SLOAZI'], MASTER_INP_HEADER_DEFAULTS['SLOAZI']),
        'DJUL': (PORB_DEFAULTS['DJUL'], MASTER_INP_HEADER_DEFAULTS['DJUL']),
        'IIB': (PORB_DEFAULTS['IIB'], MASTER_INP_HEADER_DEFAULTS['IIB']),
        'LKofT': (PORB_DEFAULTS['LKofT'], MASTER_INP_HEADER_DEFAULTS['LKofT']),
        'TUN_Flx15': (PORB_DEFAULTS['TUN_Flx15'], MASTER_INP_HEADER_DEFAULTS['TUN_Flx15']),
    }

    for param, (porb_val, master_val) in conflicts.items():
        # Verify conflict exists (values differ)
        assert porb_val != master_val, \
            f"{param}: Expected conflict but values match ({porb_val} == {master_val})"
        print(f"✓ Confirmed conflict: {param} (PORB={porb_val}, master.inp={master_val})")


def test_tpredict_stability_overrides():
    """Test TPREDICT=0.0 stability mode."""
    from pykrc.defaults import apply_tpredict_overrides

    params = {'TPREDICT': 0.0, 'GGT': 1.0, 'N3': 10, 'NRSET': 0}
    params = apply_tpredict_overrides(params)

    assert params['GGT'] == 99.0
    assert params['N3'] == 1
    assert params['NRSET'] == 999
```

### 4.2 Changecard Regression Tests

```python
# tests/test_changecard_generation.py
"""Test changecard generation matches expected output."""

import pytest
import tempfile
from pathlib import Path
from pykrc.executor import KRCExecutor
from pykrc.defaults import PORB_DEFAULTS


def test_porb_touched_params_always_get_changecards():
    """PORB-touched params should get changecards even if matching master.inp."""
    executor = KRCExecutor()

    with tempfile.TemporaryDirectory() as tmpdir:
        workdir = Path(tmpdir)

        # Create params with PORB defaults (matching master.inp)
        params = PORB_DEFAULTS.copy()
        params['Latitudes'] = [0.0]
        params['Elevations'] = [0.0]
        params['_porb_touched_params'] = set(PORB_DEFAULTS.keys())

        # Create input file
        input_file = executor.create_input_file(workdir, params)

        # Read and parse changecards
        with open(input_file) as f:
            content = f.read()

        # Verify changecards exist for PORB-touched params
        for param in ['EMISS', 'TDEEP', 'SLOPE', 'PhotoFunc']:
            assert f"'{param}'" in content, \
                f"Expected changecard for PORB-touched param {param}"


def test_conflict_params_use_porb_values():
    """Changecards should use PORB values, not master.inp values."""
    executor = KRCExecutor()

    with tempfile.TemporaryDirectory() as tmpdir:
        workdir = Path(tmpdir)

        params = PORB_DEFAULTS.copy()
        params['Latitudes'] = [0.0]
        params['Elevations'] = [0.0]
        params['_porb_touched_params'] = set(PORB_DEFAULTS.keys())

        input_file = executor.create_input_file(workdir, params)

        with open(input_file) as f:
            lines = f.readlines()

        # Find SLOAZI changecard (PORB=0.0, master.inp=90.0)
        sloazi_card = [l for l in lines if "'SLOAZI'" in l]
        assert len(sloazi_card) == 1
        assert "0.0" in sloazi_card[0] or "0.000" in sloazi_card[0]

        # Find IIB changecard (PORB=-1, master.inp=0)
        iib_card = [l for l in lines if "'IIB'" in l]
        assert len(iib_card) == 1
        assert "-1" in iib_card[0]
```

---

## Phase 5: Documentation (1 hour)

### 5.1 Update Module Docstrings

```python
# defaults.py (top of file)
"""
Canonical default values for KRC parameters.

Architecture
------------
Three levels of defaults:

1. USER_DEFAULTS
   - Initial values when krc() is called
   - User-facing behavior
   - Example: LKEY="T" (use Ls, not Julian date)

2. PORB_DEFAULTS
   - Values set when PORB is loaded
   - Override USER_DEFAULTS
   - Ensure Davinci parity
   - Example: SLOAZI=0.0 (PORB overrides master.inp 90.0)

3. MASTER_INP_HEADER_DEFAULTS
   - Used ONLY for input file header generation
   - Match master.inp format expected by Fortran
   - Actual values come from changecards!
   - Example: Header says IIB=0, changecard says IIB=-1

Default Precedence
------------------
User Input > PORB Defaults > USER_DEFAULTS > MASTER_INP_HEADER_DEFAULTS
           (highest priority)                 (lowest - header only)

Why Conflicts Exist
-------------------
PORB_DEFAULTS intentionally differ from MASTER_INP_HEADER_DEFAULTS because:
- MASTER_INP_HEADER: Generic defaults from original Fortran master.inp
- PORB_DEFAULTS: Body-specific, validated defaults from Davinci wrapper
- Changecards override header values, so conflicts are resolved correctly

Known Conflicts (by design):
- SLOAZI: PORB=0.0, master.inp=90.0
- DJUL: PORB=0.1, master.inp=-1222.69
- IIB: PORB=-1, master.inp=0
- LKofT: PORB=True, master.inp=False
- TUN_Flx15: PORB=0, master.inp=65

These conflicts are INTENTIONAL and ensure correct behavior.
"""
```

### 5.2 Add Architecture Diagram

Create `docs/ARCHITECTURE.md`:

```markdown
# KRC Python Architecture

## Parameter Flow Diagram

```
┌──────────────────────────────────────────────────────────────────┐
│                        User Calls krc()                          │
│                                                                  │
│  krc(lat=0, lon=0, body="Mars", ls=270, INERTIA=200, ...)      │
└────────────────────┬─────────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────┐
│                   core.py: Parameter Setup                        │
├──────────────────────────────────────────────────────────────────┤
│  1. Extract user_params (explicitly set by user)                 │
│  2. Apply USER_DEFAULTS (from defaults.py)                       │
│  3. Validate required parameters (lat, lon)                      │
│  4. Load PORB via porb_handler.setup_orbital_parameters()        │
│     ├─ Returns: body_params, porb_params, porb_touched          │
│     └─ porb_params override USER_DEFAULTS                        │
│  5. Load ancillary data (TES albedo, MOLA elevation)            │
│  6. Calculate material properties (from materials.py)            │
│  7. Calculate numerical params N1, N2 (from numerical.py)        │
│  8. Build complete params dict                                   │
└────────────────────┬─────────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────┐
│               executor.py: Create Input File                      │
├──────────────────────────────────────────────────────────────────┤
│  1. Write header with MASTER_INP_HEADER_DEFAULTS                 │
│     (lines 1-28: exact master.inp format)                        │
│  2. Write latitudes & elevations                                 │
│  3. Write PORB data block                                        │
│  4. Write changecards for:                                       │
│     ├─ All params != MASTER_INP_HEADER_DEFAULTS                 │
│     └─ All params in porb_touched (even if matching!)           │
│  5. Write output file specification                              │
└────────────────────┬─────────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────┐
│                    KRC Fortran Execution                         │
├──────────────────────────────────────────────────────────────────┤
│  1. Read header (uses MASTER_INP_HEADER_DEFAULTS)               │
│  2. Read changecards (OVERRIDES header values)                   │
│  3. Run thermal model                                            │
│  4. Write output to .t52 file                                    │
└────────────────────┬─────────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────┐
│                  bin_parser.py: Parse Output                     │
├──────────────────────────────────────────────────────────────────┤
│  1. Read binary output (.t52)                                    │
│  2. Extract temperature, time, orbital data                      │
│  3. Return structured dict                                       │
└────────────────────┬─────────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────┐
│                    Return to User                                │
│                                                                  │
│  result = {                                                      │
│      'surf': surface_temperature,                                │
│      'bol': bolometer_temperature,                               │
│      'time': time_array,                                         │
│      ...                                                         │
│  }                                                               │
└──────────────────────────────────────────────────────────────────┘
```

## Default Precedence

```
Highest Priority
    ↑
    │  User-specified parameters
    │  (passed to krc() function)
    │
    │  PORB-derived parameters
    │  (from PORB_DEFAULTS + PORB HDF data)
    │
    │  USER_DEFAULTS
    │  (from defaults.py)
    │
    │  MASTER_INP_HEADER_DEFAULTS
    │  (header placeholders only - overridden by changecards)
    ↓
Lowest Priority
```
```

---

## Timeline Summary

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| 0. Create new modules | 1 hour | defaults.py, porb_handler.py |
| 1. Refactor core.py | 3 hours | Simplified core.py (~200 lines) |
| 2. Refactor executor.py | 2 hours | Simplified executor.py (~400 lines) |
| 3. Update comments | 15 min | Correct inline comments |
| 4. Testing | 2 hours | Regression + unit tests |
| 5. Documentation | 1 hour | Module docs + architecture |
| **TOTAL** | **9.25 hours** | Production-ready refactor |

---

## Success Criteria

✅ All existing tests pass
✅ New tests for defaults.py
✅ Single source of truth for all defaults
✅ No duplicate default assignments
✅ Correct comments (no misleading info)
✅ PORB-touched params documented
✅ Changecard generation preserved
✅ Davinci parity maintained
✅ Code reduced by ~700 lines (40%)

---

## Migration Path

1. **Phase 0-1**: Create new modules, refactor core.py
2. **Test**: Verify all tests pass
3. **Phase 2**: Refactor executor.py
4. **Test**: Verify all tests pass
5. **Phase 3-5**: Cleanup, docs, final testing
6. **Deploy**: Merge to main

If any phase breaks tests, rollback and debug before continuing.
