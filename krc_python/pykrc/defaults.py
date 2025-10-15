"""
Canonical default values for all KRC parameters.

This is the SINGLE SOURCE OF TRUTH for defaults.
Other modules should import from here, not define their own defaults.

Three levels of defaults:
1. USER_DEFAULTS: Initial defaults when krc() is called
2. PORB_DEFAULTS: Values set when PORB is loaded (override user defaults)
3. MASTER_INP_HEADER_DEFAULTS: Values used ONLY for input file header
                                 (actual values come from changecards)

Architecture
------------
Default Precedence (highest to lowest):
  1. User-specified parameters (passed to krc())
  2. PORB-derived parameters (PORB_DEFAULTS + HDF data)
  3. USER_DEFAULTS (initial defaults)
  4. MASTER_INP_HEADER_DEFAULTS (header placeholders only)

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
