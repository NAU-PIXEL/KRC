"""Main KRC interface function."""

from pathlib import Path
from typing import Dict, Any, Optional, Union, List, Tuple, Set
import tempfile
import numpy as np

from pykrc.config import get_krc_home, get_paths
from pykrc.input_processor import parse_master_inp
from pykrc.data_loaders import KRCDataLoader
from pykrc.materials import calculate_thermal_properties
from pykrc.orbital import porb, OrbitalElements
from pykrc.executor import KRCExecutor
from pykrc.bin_parser import parse_bin52
from pykrc.layers import calculate_IC2, validate_two_layer_config
from pykrc.frost import get_frost_params_for_body, validate_frost_config
from pykrc.validation import validate_all_parameters, KRCValidationError
from pykrc.numerical import krc_evalN1, krc_evalN2, check_stability, calculate_convergence_params


def _extract_user_params(**local_vars):
    """
    Extract only parameters explicitly set by user (not None).

    Filters out None values and internal variables (starting with _).
    This allows us to distinguish between user-set params and defaults.
    """
    return {
        k: v for k, v in local_vars.items()
        if v is not None and not k.startswith('_') and k not in ['kwargs']
    }


def _extract_material_properties_for_numerics(
    using_direct_props: bool,
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: float,
    upper_props: Dict[str, Any]
) -> Tuple[float, float]:
    """
    Extract density and specific heat for numerical calculations.

    When using direct property specification, returns DENSITY and SPEC_HEAT directly.
    Otherwise derives density from INERTIA using: ρ = I²/(k·c)

    Parameters
    ----------
    using_direct_props : bool
        True if COND, DENSITY, SPEC_HEAT were directly specified
    DENSITY : float or None
        Direct density specification (kg/m³)
    SPEC_HEAT : float or None
        Direct specific heat specification (J/kg/K)
    INERTIA : float
        Thermal inertia (J m⁻² K⁻¹ s⁻½)
    upper_props : dict
        Calculated material properties with keys: 'SphUp0', 'ConUp0'

    Returns
    -------
    density : float
        Material density (kg/m³)
    specific_heat : float
        Specific heat (J/kg/K)

    Notes
    -----
    This helper appears 3 times in the original krc() function:
    - For N1 calculation (subsurface layers)
    - For N2 calculation (timesteps per day)
    - For stability check

    Examples
    --------
    >>> # Direct specification
    >>> dens, cp = _extract_material_properties_for_numerics(
    ...     True, 1600.0, 800.0, 200.0, {}
    ... )
    >>> dens, cp
    (1600.0, 800.0)

    >>> # Derived from INERTIA
    >>> props = {"SphUp0": 647.0, "ConUp0": 0.025}
    >>> dens, cp = _extract_material_properties_for_numerics(
    ...     False, None, None, 200.0, props
    ... )
    >>> # dens ≈ 2469.14 from I²/(k·c) = 200²/(0.025·647)
    >>> cp
    647.0
    """
    if using_direct_props:
        return DENSITY, SPEC_HEAT
    else:
        cp = upper_props["SphUp0"]
        k = upper_props["ConUp0"]
        # From I² = k·ρ·c, we get ρ = I²/(k·c)
        dens = (INERTIA**2) / (k * cp)
        return dens, cp


def _apply_default_parameters(
    DELLS: Optional[float],
    spinup_years: Optional[float],
    output_years: Optional[float],
    LKEY: Optional[str],
    LKofT: Optional[bool],
    thick: Optional[float],
    FANON: Optional[float],
    N3: Optional[int],
    NRSET: Optional[int],
    GGT: Optional[float],
    TPREDICT: Optional[float],
    MAXN1: Optional[int],
    MAXN2: Optional[int],
    auto_numerical: Optional[bool],
    bodyforce: Optional[int],
    TUN8: Optional[int],
    LMST: Optional[str],
    WRITE: Optional[str],
    KEEP: Optional[str],
    Eclipse: Optional[str],
    Eclipse_Style: Optional[float],
    PFlux: Optional[str],
    Lon_Hr: Optional[float],
    verbose: Optional[bool],
    keep_files: Optional[bool],
    lon: Optional[float]
) -> Dict[str, Any]:
    """
    Apply default values for parameters not explicitly set by user.

    This consolidates lines 338-406 of the original krc() function,
    implementing the default assignment logic in one place.

    Parameters
    ----------
    [All parameters that need defaults]

    Returns
    -------
    defaults : dict
        Dictionary mapping parameter names to their default values

    Notes
    -----
    PORB-related parameters (FLAY, RLAY, IIB, LZONE, EMISS, SLOPE, SLOAZI,
    DUSTA, TAURAT, KPREF, LVFT, JBARE, TDEEP, DJUL, PhotoFunc) are NOT
    set here - they will be set in PORB section to ensure changecards are
    written properly.

    TPREDICT=0.0 triggers stability overrides (GGT=99.0, N3=1, NRSET=999).
    """
    defaults = {}

    # Time control defaults
    defaults['DELLS'] = 1.0 if DELLS is None else DELLS
    defaults['spinup_years'] = 2.0 if spinup_years is None else spinup_years
    defaults['output_years'] = 1.0 if output_years is None else output_years
    defaults['LKEY'] = "T" if LKEY is None else LKEY

    # Material properties
    defaults['LKofT'] = True if LKofT is None else LKofT
    defaults['thick'] = 0.0 if thick is None else thick

    # Surface/atmospheric parameters (non-PORB)
    defaults['FANON'] = 0.055 if FANON is None else FANON

    # Numerical control
    defaults['N3'] = 1 if N3 is None else N3  # Match Davinci default
    defaults['NRSET'] = 0 if NRSET is None else NRSET
    defaults['GGT'] = 1.0 if GGT is None else GGT
    defaults['TPREDICT'] = 0.0 if TPREDICT is None else TPREDICT

    # Temperature prediction control logic (matches Davinci lines 841-847)
    # Short timesteps disable prediction for stability
    if defaults['TPREDICT'] == 0.0:  # TPREDICT="F" in Davinci
        defaults['GGT'] = 99.0
        defaults['N3'] = 1
        defaults['NRSET'] = 999

    defaults['MAXN1'] = 100 if MAXN1 is None else MAXN1
    defaults['MAXN2'] = 1000 if MAXN2 is None else MAXN2
    defaults['auto_numerical'] = True if auto_numerical is None else auto_numerical

    # Output control
    defaults['bodyforce'] = 0 if bodyforce is None else bodyforce
    defaults['TUN8'] = 0 if TUN8 is None else TUN8

    # Flags
    defaults['LMST'] = "F" if LMST is None else LMST
    defaults['WRITE'] = "F" if WRITE is None else WRITE
    defaults['KEEP'] = "F" if KEEP is None else KEEP
    defaults['Eclipse'] = "F" if Eclipse is None else Eclipse
    defaults['Eclipse_Style'] = 1.0 if Eclipse_Style is None else Eclipse_Style
    defaults['PFlux'] = "F" if PFlux is None else PFlux
    defaults['Lon_Hr'] = 12.0 if Lon_Hr is None else Lon_Hr

    # Runtime control
    defaults['verbose'] = False if verbose is None else verbose
    defaults['keep_files'] = False if keep_files is None else keep_files

    # Location (lon gets default if not set)
    defaults['lon'] = 0.0 if lon is None else lon

    return defaults


def _load_ancillary_data(
    body: str,
    lat: float,
    lon: float,
    ALBEDO: Optional[float],
    ELEV: Optional[Union[float, List[float]]],
    INERTIA: Optional[float],
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[float, Union[float, List[float]], Optional[float]]:
    """
    Load ancillary data (albedo, elevation, inertia) for Mars or apply defaults.

    This consolidates lines 633-668 of the original krc() function,
    implementing ancillary data lookup logic for Mars.

    Parameters
    ----------
    body : str
        Celestial body name
    lat : float
        Latitude (degrees)
    lon : float
        Longitude (degrees)
    ALBEDO : float, optional
        User-specified albedo (skips lookup)
    ELEV : float or list, optional
        User-specified elevation (skips lookup)
    INERTIA : float, optional
        User-specified thermal inertia (skips lookup)
    master_params : dict
        Master.inp defaults
    verbose : bool
        Print details

    Returns
    -------
    ALBEDO : float
        Surface albedo
    ELEV : float or list
        Surface elevation (km)
    INERTIA : float or None
        Thermal inertia (J m⁻² K⁻¹ s⁻½), None if not set

    Notes
    -----
    For Mars, tries to load TES albedo, MOLA elevation, and TES thermal inertia
    from ancillary data files. Falls back to master.inp defaults if unavailable.

    For non-Mars bodies, uses master.inp defaults.
    """
    if body == "Mars":
        # Import ancillary data functions
        try:
            from .ancillary import lookup_albedo, lookup_elevation, lookup_inertia

            # Use ancillary data lookups if not explicitly provided
            if ALBEDO is None:
                ALBEDO = lookup_albedo(lat, lon)
                if verbose:
                    print(f"Using TES albedo from ancillary data: {ALBEDO:.4f}")

            if ELEV is None:
                ELEV = lookup_elevation(lat, lon)
                if verbose:
                    print(f"Using MOLA elevation from ancillary data: {ELEV:.2f} km")

            # Load INERTIA from TES map if not explicitly provided (matches Davinci)
            if INERTIA is None:
                INERTIA = lookup_inertia(lat, lon)
                if verbose:
                    print(f"Using TES thermal inertia from ancillary data: {INERTIA:.1f}")
        except Exception as e:
            # Fall back to defaults if ancillary data unavailable
            if verbose:
                print(f"Warning: Could not load ancillary data ({e}), using defaults")
            if ALBEDO is None:
                ALBEDO = master_params.get("ALBEDO", 0.25)
            if ELEV is None:
                ELEV = 0.0
    else:
        # For non-Mars bodies, use defaults
        if ALBEDO is None:
            ALBEDO = master_params.get("ALBEDO", 0.25)
        if ELEV is None:
            ELEV = 0.0

    return ALBEDO, ELEV, INERTIA


def _calculate_material_properties(
    COND: Optional[float],
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: Optional[float],
    Mat1: str,
    Mat2: str,
    T_user: float,
    k_style: str,
    LKofT: bool,
    INERTIA2: Optional[float],
    thick: float,
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[bool, float, float, Dict[str, Any], Dict[str, Any]]:
    """
    Calculate thermal properties for upper and lower layers.

    This consolidates lines 760-814 of the original krc() function,
    implementing material property calculation logic.

    Parameters
    ----------
    COND : float, optional
        Direct thermal conductivity specification (W/m/K)
    DENSITY : float, optional
        Direct density specification (kg/m³)
    SPEC_HEAT : float, optional
        Direct specific heat specification (J/kg/K)
    INERTIA : float, optional
        Thermal inertia (J m⁻² K⁻¹ s⁻½)
    Mat1 : str
        Upper layer material name
    Mat2 : str
        Lower layer material name
    T_user : float
        Reference temperature (K)
    k_style : str
        Conductivity model ("Mars", "Moon", "Bulk")
    LKofT : bool
        Use temperature-dependent thermal properties
    INERTIA2 : float, optional
        Lower layer thermal inertia
    thick : float
        Layer thickness (m)
    master_params : dict
        Master.inp defaults
    verbose : bool
        Print details

    Returns
    -------
    using_direct_props : bool
        True if using direct COND/DENSITY/SPEC_HEAT specification
    INERTIA : float
        Thermal inertia (calculated if using direct props)
    INERTIA2 : float
        Lower layer thermal inertia (defaults to INERTIA)
    upper_props : dict
        Upper layer thermal property coefficients
    lower_props : dict
        Lower layer thermal property coefficients

    Notes
    -----
    Two modes of operation:
    1. Direct specification: COND, DENSITY, SPEC_HEAT all provided
       - Calculates INERTIA from I = sqrt(k·ρ·c)
       - Optionally generates T-dependent coefficients if LKofT=True
    2. INERTIA-based: Standard approach using material database
       - Calculates properties from Mat1, INERTIA, T_user, k_style
    """
    # Determine if using INERTIA or direct COND/DENSITY/SPEC_HEAT specification
    using_direct_props = (COND is not None and DENSITY is not None and SPEC_HEAT is not None)

    if using_direct_props:
        if verbose:
            print(f"Using direct material properties: COND={COND}, DENSITY={DENSITY}, SPEC_HEAT={SPEC_HEAT}")

        # Calculate implied INERTIA for reference
        INERTIA = np.sqrt(COND * DENSITY * SPEC_HEAT)

        upper_props = {
            "COND": COND,
            "DENSITY": DENSITY,
            "SPEC_HEAT": SPEC_HEAT,
        }

        # Generate T-dependent coefficients if requested
        if LKofT:
            # Use calculate_thermal_properties to get coefficients
            temp_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)
            upper_props.update({
                "ConUp0": temp_props["ConUp0"],
                "ConUp1": temp_props["ConUp1"],
                "ConUp2": temp_props["ConUp2"],
                "ConUp3": temp_props["ConUp3"],
                "SphUp0": temp_props["SphUp0"],
                "SphUp1": temp_props["SphUp1"],
                "SphUp2": temp_props["SphUp2"],
                "SphUp3": temp_props["SphUp3"],
            })
        else:
            # Constant properties (no T-dependence)
            upper_props.update({
                "ConUp0": COND, "ConUp1": 0.0, "ConUp2": 0.0, "ConUp3": 0.0,
                "SphUp0": SPEC_HEAT, "SphUp1": 0.0, "SphUp2": 0.0, "SphUp3": 0.0,
            })
    else:
        # Standard INERTIA-based approach
        if INERTIA is None:
            INERTIA = master_params.get("INERTIA", 200.0)

        if verbose:
            print(f"Calculating material properties for {Mat1} with INERTIA={INERTIA}...")

        upper_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)

    # Handle lower layer (for two-layer regolith)
    if INERTIA2 is None:
        INERTIA2 = INERTIA

    if verbose and thick != 0.0:
        print(f"Two-layer regolith: thick={thick}m, upper TI={INERTIA}, lower TI={INERTIA2}")

    lower_props = calculate_thermal_properties(Mat2, INERTIA2, T_user, k_style)

    return using_direct_props, INERTIA, INERTIA2, upper_props, lower_props


def _calculate_numerical_parameters(
    auto_numerical: bool,
    N1: Optional[int],
    N2: Optional[int],
    using_direct_props: bool,
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: float,
    upper_props: Dict[str, Any],
    RLAY: float,
    FLAY: float,
    DELJUL: float,
    N5: int,
    JDISK: int,
    MAXN1: int,
    rot_per: float,
    INERTIA2: float,
    thick: float,
    n24_from_porb: int,
    MAXN2: int,
    GGT: float,
    TPREDICT: float,
    N3: int,
    DELLS: float,
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[int, int, int]:
    """
    Calculate or validate numerical parameters (N1, N2, N3).

    This consolidates lines 946-1034 of the original krc() function,
    implementing numerical parameter auto-calculation and stability checking.

    Parameters
    ----------
    auto_numerical : bool
        Auto-calculate N1, N2 if not provided
    N1 : int, optional
        Number of subsurface layers (calculated if None)
    N2 : int, optional
        Number of timesteps per day (calculated if None)
    using_direct_props : bool
        Using direct COND/DENSITY/SPEC_HEAT specification
    DENSITY : float, optional
        Material density (kg/m³)
    SPEC_HEAT : float, optional
        Specific heat (J/kg/K)
    INERTIA : float
        Thermal inertia (J m⁻² K⁻¹ s⁻½)
    upper_props : dict
        Upper layer thermal property coefficients
    [... all other parameters ...]

    Returns
    -------
    N1 : int
        Number of subsurface layers
    N2 : int
        Number of timesteps per day
    N3 : int
        Convergence parameter

    Notes
    -----
    Auto-calculation uses krc_evalN1() and krc_evalN2() from numerical.py.
    Performs stability check using check_stability().
    Updates N3 if auto_numerical and convergence mode enabled (GGT=1.0, TPREDICT=0.0).
    """
    # Auto-calculate N1, N2 if not provided and auto_numerical is True
    if auto_numerical:
        if N1 is None:
            # Extract actual DENSITY and SPEC_HEAT for N1 calculation
            dens_for_N1, cp_for_N1 = _extract_material_properties_for_numerics(
                using_direct_props, DENSITY, SPEC_HEAT, INERTIA, upper_props
            )

            N1 = krc_evalN1(
                RLAY=RLAY,
                FLAY=FLAY,
                INERTIA=INERTIA,
                SPEC_HEAT=cp_for_N1,
                DENSITY=dens_for_N1,
                DELJUL=DELJUL,
                N5=N5,
                JDISK=JDISK,
                MAXN1=MAXN1,
                PERIOD=rot_per,
                INERTIA2=INERTIA2 if thick != 0.0 else None,
                verbose=verbose
            )
            if verbose:
                print(f"Auto-calculated N1={N1} layers")

        if N2 is None:
            # Extract actual DENSITY and SPEC_HEAT for N2 calculation
            dens_for_N2, cp_for_N2 = _extract_material_properties_for_numerics(
                using_direct_props, DENSITY, SPEC_HEAT, INERTIA, upper_props
            )

            N2 = krc_evalN2(
                FLAY=FLAY,
                INERTIA=INERTIA,
                DENSITY=dens_for_N2,
                SPEC_HEAT=cp_for_N2,
                PERIOD=rot_per,
                N24=n24_from_porb,
                MAXN2=MAXN2,
                verbose=verbose
            )
            if verbose:
                print(f"Auto-calculated N2={N2} timesteps/day")
    else:
        # Use master.inp defaults if not auto-calculating
        if N1 is None:
            N1 = master_params.get("N1", 50)
        if N2 is None:
            N2 = master_params.get("N2", 288)

    # Check numerical stability
    if N1 is not None and N2 is not None:
        # Get material properties for stability check
        dens_check, cp_check = _extract_material_properties_for_numerics(
            using_direct_props, DENSITY, SPEC_HEAT, INERTIA, upper_props
        )

        is_stable, stability_msg = check_stability(
            N1, N2, INERTIA, rot_per, FLAY, dens_check, cp_check, warn=True
        )
        if verbose:
            print(f"  {stability_msg}")

    # Auto-calculate convergence parameters if not provided
    if auto_numerical and (GGT == 1.0 and TPREDICT == 0.0):
        conv_params = calculate_convergence_params(N1, N2, DELLS, fast_mode=False)
        # Only override defaults if they weren't explicitly set
        if N3 == 10:
            N3 = conv_params["N3"]
        if verbose:
            print(f"Convergence parameters: N3={N3}, GGT={GGT}, TPREDICT={TPREDICT}")

    return N1, N2, N3


def krc(
    # ========== LOCATION & BODY ==========
    lat: Optional[Union[float, List[float]]] = None,
    lon: Optional[float] = None,
    body: str = "Mars",
    ELEV: Optional[Union[float, List[float]]] = None,

    # ========== TIME CONTROL ==========
    ls: Optional[float] = None,
    hour: Optional[float] = None,
    DELLS: Optional[float] = None,  # Default: 1.0
    N5: Optional[int] = None,
    JDISK: Optional[int] = None,
    spinup_years: Optional[float] = None,  # Default: 2.0
    output_years: Optional[float] = None,  # Default: 1.0
    LKEY: Optional[str] = None,  # Use Ls (solar longitude) for time input ("T") vs Julian Date ("F"), Default: "T"
    JD: Optional[float] = None,  # Julian Date (alternative to ls)
    GD: Optional[str] = None,  # Gregorian Date "YYYY-Mmm-DD" (alternative to ls)

    # ========== MATERIAL PROPERTIES ==========
    # Method 1: Thermal inertia (standard approach)
    INERTIA: Optional[float] = None,
    k_style: str = "Mars",
    Mat1: str = "basalt",
    Por1: Optional[float] = None,
    T_user: float = 220.0,

    # Method 2: Direct specification (alternative to INERTIA)
    COND: Optional[float] = None,
    DENSITY: Optional[float] = None,
    SPEC_HEAT: Optional[float] = None,
    LKofT: Optional[bool] = None,  # Default: True (use T-dependent conductivity)

    # ========== TWO-LAYER REGOLITH ==========
    thick: Optional[float] = None,  # Default: 0.0
    INERTIA2: Optional[float] = None,
    Mat2: str = "basalt",
    Por2: Optional[float] = None,
    IC2: Optional[int] = None,
    FLAY: Optional[float] = None,  # PORB default: 0.10 (layer spacing factor)
    RLAY: Optional[float] = None,  # PORB default: 1.15 (layer thickness ratio)
    IIB: Optional[int] = None,  # PORB default: -1 (temperature prediction mode)
    LZONE: Optional[str] = None,  # Use zone file for layer properties, Default: "F"

    # ========== SURFACE PROPERTIES ==========
    ALBEDO: Optional[Union[float, List[float]]] = None,  # Can be time-varying array
    EMISS: Optional[float] = None,  # Default: 1.0
    SLOPE: Optional[float] = None,  # Default: 0.0
    SLOAZI: Optional[float] = None,  # PORB default: 0.0 (slope azimuth, degrees E of N)

    # ========== ATMOSPHERE ==========
    TAUD: Optional[Union[float, List[float]]] = None,  # Can be time-varying array
    PTOTAL: Optional[float] = None,
    TATM: Optional[float] = None,
    DUSTA: Optional[float] = None,  # PORB default: 0.9 (dust single-scattering albedo)
    TAURAT: Optional[float] = None,  # PORB default: 0.25 (thermal/visible opacity ratio)
    FANON: Optional[float] = None,  # Default: 0.055 (atmospheric anisotropy factor, NOT 0.3!)
    KPREF: Optional[int] = None,  # Seasonal pressure model (1=Viking, 2=frost budget), Default: 1

    # ========== FROST/CONDENSATION ==========
    LVFT: Optional[bool] = None,  # Default: False
    TFROST: Optional[float] = None,
    CFROST: Optional[float] = None,
    AFROST: Optional[float] = None,
    JBARE: Optional[int] = None,  # Season number to force frost-free conditions, Default: 0

    # ========== NUMERICAL CONTROL ==========
    N1: Optional[int] = None,
    N2: Optional[int] = None,
    N3: Optional[int] = None,  # Default: 1 (Davinci default, matches TPREDICT=0.0 mode)
    NRSET: Optional[int] = None,  # Default: 0
    GGT: Optional[float] = None,  # Default: 1.0
    TPREDICT: Optional[float] = None,  # Default: 0.0
    MAXN1: Optional[int] = None,  # Default: 100
    MAXN2: Optional[int] = None,  # Default: 1000
    auto_numerical: Optional[bool] = None,  # Default: True

    # ========== MODEL PARAMETERS ==========
    TDEEP: Optional[float] = None,  # Default: 180.0
    DJUL: Optional[float] = None,  # Default: 0.0
    bodyforce: Optional[int] = None,  # Force PORB recalculation (0=cached, 1=recalculate), Default: 0

    # ========== OUTPUT CONTROL ==========
    TUN8: Optional[int] = None,  # Depth profile output (0=off, 101=all layers, N=every Nth layer), Default: 0
    LMST: Optional[str] = None,  # Output in Local Mean Solar Time ("T") vs LTST ("F"), Default: "F"
    WRITE: Optional[str] = None,  # Write detailed output files ("T" or "F"), Default: "F"
    KEEP: Optional[str] = None,  # Keep temporary files ("T" or "F"), Default: "F"

    # ========== ADVANCED PHYSICS ==========
    PhotoFunc: Optional[float] = None,  # Photometric function (0=Lambert, 0.6=Lunar-like), Default: 0.0

    # ========== ORBITAL PARAMETER OVERRIDES ==========
    # These override PORB values if specified
    GRAV: Optional[float] = None,  # Surface gravity (m/s²)
    DAU: Optional[float] = None,  # Distance from sun (AU)
    SOLCON: Optional[float] = None,  # Solar constant (W/m²)
    SOLARDEC: Optional[float] = None,  # Solar declination
    ARC2_G0: Optional[float] = None,  # Orbital parameter
    LsubS: Optional[float] = None,  # Latent heat of sublimation (J/kg)
    Atm_Cp: Optional[float] = None,  # Atmospheric heat capacity (J/kg/K)

    # ========== ADVANCED COMPUTATIONAL ==========
    stability: Optional[int] = None,  # Stability analysis flag
    anc: Optional[Dict] = None,  # Ancillary data dictionary

    # ========== ECLIPSE MODELING (Satellites) ==========
    Eclipse: Optional[str] = None,  # Enable eclipse calculation ("T" or "F"), Default: "F"
    Eclipse_Style: Optional[float] = None,  # 1.0=daily, 2.0=rare specified by Date, Default: 1.0
    Eclipser: Optional[str] = None,  # Name of eclipsing body
    Sun_Dis: Optional[float] = None,  # Sun distance (km, from PORB if not set)
    Eclipser_Rad: Optional[float] = None,  # Eclipser radius (km, from PORB if not set)
    Eclipsed_Rad: Optional[float] = None,  # Eclipsed body radius (km, from PORB if not set)
    CM: Optional[float] = None,  # Eclipse parameter
    Gamma: Optional[float] = None,  # Eclipse phase parameter
    Date: Optional[str] = None,  # Date for rare eclipse (YYYY-MM-DD)
    Eclipse_line: Optional[str] = None,  # Custom eclipse parameter string

    # ========== PLANETARY FLUX (Satellites) ==========
    PFlux: Optional[str] = None,  # Enable planetary thermal flux ("T" or "F"), Default: "F"
    BT_Avg: Optional[float] = None,  # Average planet brightness temperature (K)
    BT_Min: Optional[float] = None,  # Minimum planet brightness temperature (K)
    BT_Max: Optional[float] = None,  # Maximum planet brightness temperature (K)
    Lon_Hr: Optional[float] = None,  # Longitude hour on satellite surface (hours), Default: 12.0
    IR: Optional[float] = None,  # Planetary IR flux
    Vis: Optional[float] = None,  # Planetary visible flux
    Emissivity: Optional[float] = None,  # Planet emissivity

    # ========== EXECUTION OPTIONS ==========
    verbose: Optional[bool] = None,  # Default: False
    workdir: Optional[str] = None,
    keep_files: Optional[bool] = None,  # Default: False

    # ========== ADVANCED OPTIONS ==========
    **kwargs
) -> Dict[str, Any]:
    """
    Run KRC thermal model.

    Parameters
    ----------
    lat : float or list of float
        Latitude in degrees (-90 to 90). Can be a single value or list for multi-latitude runs.
    lon : float
        Longitude in degrees (0 to 360)
    body : str, optional
        Celestial body name (default "Mars")
    ls : float, optional
        Solar longitude in degrees (0 to 360)
    hour : float, optional
        Local hour (0 to 24)
    INERTIA : float, optional
        Thermal inertia in SI units (J m⁻² K⁻¹ s⁻½)
    ALBEDO : float, optional
        Surface albedo (0 to 1)
    EMISS : float, optional
        Surface emissivity (default 1.0)
    Mat1 : str, optional
        Upper layer material (default "basalt")
    Mat2 : str, optional
        Lower layer material (default "basalt")
    Por1 : float, optional
        Upper layer porosity (0 to 1)
    Por2 : float, optional
        Lower layer porosity (0 to 1)
    INERTIA2 : float, optional
        Lower layer thermal inertia
    TAUD : float, optional
        Dust optical depth
    PTOTAL : float, optional
        Total atmospheric pressure (Pa)
    TATM : float, optional
        Atmospheric temperature (K)
    TDEEP : float, optional
        Deep subsurface temperature (K)
    SLOPE : float, optional
        Surface slope in degrees
    SLOAZI : float, optional
        Slope azimuth in degrees
    k_style : str, optional
        Thermal conductivity model: "Mars", "Moon", or "Bulk"
    T_user : float, optional
        User reference temperature (K)
    ELEV : float or list of float, optional
        Surface elevation (km). Can be single value or list (must match length of lat if lat is list)
    TUN8 : int, optional
        Depth profile output control (0=off, 101=all layers, N=every Nth layer)
    PhotoFunc : float, optional
        Photometric function (0=Lambert, 0.6=Lunar-like, default 0.0)
    DUSTA : float, optional
        Dust absorptivity (default 0.9)
    TAURAT : float, optional
        Optical depth ratio vis/IR (default 2.0)
    FANON : float, optional
        Atmospheric anisotropy factor (default 0.3)
    verbose : bool, optional
        Print execution details
    workdir : str, optional
        Working directory (creates temp if None)
    keep_files : bool, optional
        Keep working directory after execution
    **kwargs
        Additional KRC parameters

    Returns
    -------
    dict
        Output structure containing:
        - surf: Surface temperature
        - bol: Bolometer temperature
        - time: Time axis
        - ls: Solar longitude
        - lat: Latitude
        - elev: Elevation
        - layer: Layer properties
        - anc: Ancillary data
        - porb: Orbital parameters

    Raises
    ------
    ValueError
        If required parameters are missing
    RuntimeError
        If KRC execution fails

    Examples
    --------
    >>> result = krc(lat=0, lon=0, body="Mars", ls=270, INERTIA=200, ALBEDO=0.25)
    >>> print(result['surf'])  # Surface temperature
    """
    # ========== EXTRACT USER-PROVIDED PARAMETERS ==========
    # Capture parameters explicitly set by user (not None)
    # This allows us to distinguish user-set values from defaults
    user_params = _extract_user_params(**locals())

    # ========== APPLY DEFAULTS FOR UNSET PARAMETERS ==========
    # Apply defaults using helper function
    defaults = _apply_default_parameters(
        DELLS, spinup_years, output_years, LKEY, LKofT, thick, FANON,
        N3, NRSET, GGT, TPREDICT, MAXN1, MAXN2, auto_numerical,
        bodyforce, TUN8, LMST, WRITE, KEEP, Eclipse, Eclipse_Style,
        PFlux, Lon_Hr, verbose, keep_files, lon
    )

    # Unpack defaults back to local variables
    DELLS = defaults['DELLS']
    spinup_years = defaults['spinup_years']
    output_years = defaults['output_years']
    LKEY = defaults['LKEY']
    LKofT = defaults['LKofT']
    thick = defaults['thick']
    FANON = defaults['FANON']
    N3 = defaults['N3']
    NRSET = defaults['NRSET']
    GGT = defaults['GGT']
    TPREDICT = defaults['TPREDICT']
    MAXN1 = defaults['MAXN1']
    MAXN2 = defaults['MAXN2']
    auto_numerical = defaults['auto_numerical']
    bodyforce = defaults['bodyforce']
    TUN8 = defaults['TUN8']
    LMST = defaults['LMST']
    WRITE = defaults['WRITE']
    KEEP = defaults['KEEP']
    Eclipse = defaults['Eclipse']
    Eclipse_Style = defaults['Eclipse_Style']
    PFlux = defaults['PFlux']
    Lon_Hr = defaults['Lon_Hr']
    verbose = defaults['verbose']
    keep_files = defaults['keep_files']
    lon = defaults['lon']

    # Validate required parameters
    if lat is None:
        raise ValueError("lat (latitude) is required")

    # Get KRC paths
    krc_home = get_krc_home()
    paths = get_paths()

    # Load master.inp defaults
    master_params = parse_master_inp(paths.master_inp)

    # Initialize data loader
    data_loader = KRCDataLoader(paths.support_dir)

    # Get orbital parameters
    if verbose:
        print(f"Loading orbital parameters for {body}...")

    body_params = porb(body, data_loader=data_loader)

    # Get rotation period for calculations
    rot_per = body_params.rotation_period

    # Set N24 from porb (davinci krc.dvrc lines 2755-2756)
    # N24 is number of output timesteps per day, calculated from rotation period
    # Minimum value is 96 (every 15 minutes)
    rot_per = body_params.rotation_period
    n24_from_porb = int(np.floor((rot_per * 4) / 96) * 96)
    if n24_from_porb < 96:
        n24_from_porb = 96

    # Set N5 and JDISK from user parameters or calculate from spinup/output years
    # User can now specify DELLS, N5, JDISK directly
    if N5 is None:
        total_years = spinup_years + output_years
        N5 = int(np.ceil(360.0 / DELLS * total_years))

    if JDISK is None:
        JDISK = int(np.ceil(360.0 / DELLS * spinup_years + 1))

    # Get DELJUL - complex precedence matching Davinci (krc.dvrc lines 347-355)
    # DELJUL is the time step in Julian days for each Ls degree
    #
    # Davinci logic:
    # if(HasValue(DELJUL)==0 && HasValue(DELLS)==0)  → DELJUL from PORB
    # if(HasValue(DELJUL)==0 && HasValue(DELLS)==1)  → DELJUL = PERIOD/360*DELLS (DELLS blocks PORB!)
    # if(HasValue(DELJUL)==1 && HasValue(DELLS)==0)  → User DELJUL kept
    # if(HasValue(DELJUL)==1 && HasValue(DELLS)==1)  → Error (both set)
    #
    # PyKRC: DELJUL is not user-facing, so only check if DELLS was user-set

    if 'DELLS' in user_params:
        # User set DELLS → calculate DELJUL from DELLS (blocks PORB value!)
        if hasattr(body_params, 'orbital_period'):
            DELJUL = body_params.orbital_period * DELLS / 360.0
        else:
            # Fallback: estimate from rotation period
            DELJUL = rot_per * DELLS / 360.0
            if verbose:
                print(f"Warning: Using rotation period for DELJUL (orbital_period not in PORB)")
    elif hasattr(body_params, 'krc_params') and 'DELJUL' in body_params.krc_params:
        # User did NOT set DELLS → use PORB DELJUL
        DELJUL = body_params.krc_params['DELJUL']
    elif hasattr(body_params, 'orbital_period'):
        # No PORB DELJUL → calculate from orbital period and default DELLS
        DELJUL = body_params.orbital_period * DELLS / 360.0
    else:
        # Final fallback
        DELJUL = rot_per * DELLS / 360.0
        if verbose:
            print(f"Warning: Using rotation period for DELJUL calculation (may be inaccurate)")

    # Set PORB-derived defaults (matching Davinci behavior)
    # These parameters are set when PORB is loaded, even if they match master.inp
    # This ensures changecards are written for all PORB-touched parameters

    # Track parameters set by PORB (for changecard generation)
    # These parameters should have changecards written even if they match master.inp defaults
    porb_touched_params = set()

    # Get parameters from PORB HDF krc_params dict if available
    if hasattr(body_params, 'krc_params') and body_params.krc_params:
        krc_params = body_params.krc_params

        if 'PTOTAL' in krc_params and PTOTAL is None:
            PTOTAL = krc_params['PTOTAL']
            porb_touched_params.add('PTOTAL')
        if 'GRAV' in krc_params and GRAV is None:
            GRAV = krc_params['GRAV']
            porb_touched_params.add('GRAV')
        if 'TAURAT' in krc_params and TAURAT is None:
            TAURAT = krc_params['TAURAT']
            porb_touched_params.add('TAURAT')
        if 'DUSTA' in krc_params and DUSTA is None:
            DUSTA = krc_params['DUSTA']
            porb_touched_params.add('DUSTA')
        if 'ARC2_G0' in krc_params and ARC2_G0 is None:
            ARC2_G0 = krc_params['ARC2_G0']
            porb_touched_params.add('ARC2_G0')

    # Set standard PORB-related defaults (Davinci krc.dvrc behavior)
    # These are set whenever PORB is loaded, ensuring changecards are written
    if EMISS is None:
        EMISS = 1.0  # Standard emissivity
        porb_touched_params.add('EMISS')
    if TDEEP is None:
        TDEEP = 180.0  # Standard deep temperature for Mars
        porb_touched_params.add('TDEEP')
    if TAUD is None:
        TAUD = 0.3  # Default atmospheric optical depth
        porb_touched_params.add('TAUD')
    if DJUL is None:
        DJUL = 0.1  # PORB default (not 0.0)
        porb_touched_params.add('DJUL')
    if SLOPE is None:
        SLOPE = 0.0
        porb_touched_params.add('SLOPE')
    if SLOAZI is None:
        SLOAZI = 0.0
        porb_touched_params.add('SLOAZI')
    if TFROST is None:
        TFROST = 146.0  # CO2 frost temperature for Mars
        porb_touched_params.add('TFROST')
    if PhotoFunc is None:
        PhotoFunc = 0.0
        porb_touched_params.add('PhotoFunc')
    if FLAY is None:
        FLAY = 0.10  # Davinci default (not 2.0)
        porb_touched_params.add('FLAY')
    if RLAY is None:
        RLAY = 1.15  # Davinci default (not 1.08)
        porb_touched_params.add('RLAY')

    # Set IIB to -1 when PORB is loaded (not 2)
    # IIB=-1 means temperature prediction mode (Davinci default with PORB)
    if IIB is None:
        IIB = -1
        porb_touched_params.add('IIB')

    # Set additional integer parameters (Davinci PORB defaults)
    if IC2 is None:
        IC2 = 999  # Will be recalculated later if thick != 0
        porb_touched_params.add('IC2')
    if KPREF is None:
        KPREF = 1  # Standard reference pressure
        porb_touched_params.add('KPREF')
    if JBARE is None:
        JBARE = 0  # No bare ground
        porb_touched_params.add('JBARE')

    # Internal parameters (not user-configurable)
    K4OUT = 52  # Standard output format (bin52)
    TUN_Flx15 = 0  # No tuning flux
    porb_touched_params.add('K4OUT')
    porb_touched_params.add('TUN_Flx15')

    # Set logical flags (Davinci PORB defaults)
    if LVFT is None:
        LVFT = False  # No frost by default (will be set True if needed)
        porb_touched_params.add('LVFT')
    if LKofT is None:
        LKofT = True  # Temperature-dependent thermal properties enabled
        porb_touched_params.add('LKofT')
    if LZONE is None:
        LZONE = False  # No zone control
        porb_touched_params.add('LZONE')

    # Physical constraint: No atmosphere → no dust (Davinci krc.dvrc line 547)
    if PTOTAL is not None and PTOTAL < 1.0:
        if verbose and TAUD != 0.0:
            print(f"Warning: PTOTAL={PTOTAL} < 1 Pa → forcing TAUD=0 (no atmosphere)")
        TAUD = 0.0
        porb_touched_params.add('TAUD')

    if verbose:
        print(f"Time control: DELLS={DELLS}°, N5={N5} seasons, JDISK={JDISK}")
        print(f"  Total run: {N5*DELLS/360:.1f} years, Output: {(N5-JDISK)*DELLS/360:.1f} years")

    # ========== LOAD ANCILLARY DATA ==========
    # Load albedo, elevation, and inertia from ancillary data (Mars) or defaults
    ALBEDO, ELEV, INERTIA = _load_ancillary_data(
        body, lat, lon, ALBEDO, ELEV, INERTIA, master_params, verbose
    )

    # ========== MATERIAL PROPERTY HANDLING ==========
    # Calculate thermal properties for upper and lower layers
    using_direct_props, INERTIA, INERTIA2, upper_props, lower_props = _calculate_material_properties(
        COND, DENSITY, SPEC_HEAT, INERTIA, Mat1, Mat2, T_user, k_style,
        LKofT, INERTIA2, thick, master_params, verbose
    )

    # ========== NUMERICAL PARAMETERS ==========
    # Calculate or validate numerical parameters (N1, N2, N3)
    N1, N2, N3 = _calculate_numerical_parameters(
        auto_numerical, N1, N2, using_direct_props, DENSITY, SPEC_HEAT,
        INERTIA, upper_props, RLAY, FLAY, DELJUL, N5, JDISK, MAXN1,
        rot_per, INERTIA2, thick, n24_from_porb, MAXN2, GGT, TPREDICT,
        N3, DELLS, master_params, verbose
    )

    # ========== TWO-LAYER CONFIGURATION ==========
    # Validate two-layer configuration
    validate_two_layer_config(thick, INERTIA, INERTIA2, Mat1, Mat2, Por1, Por2)

    # Calculate IC2 if not explicitly provided
    if IC2 is None:
        IC2 = calculate_IC2(thick, N1, FLAY, RLAY)
        if verbose and thick != 0.0:
            print(f"Calculated IC2={IC2} for thick={thick}m")

    # ========== FROST/CONDENSATION HANDLING ==========
    validate_frost_config(LVFT, PTOTAL, TFROST, body)

    if LVFT:
        # Auto-calculate frost parameters if not provided
        if TFROST is None or CFROST is None or AFROST is None:
            auto_frost = get_frost_params_for_body(body, PTOTAL if PTOTAL else 600.0)
            if auto_frost:
                if TFROST is None:
                    TFROST = auto_frost[0]
                if CFROST is None:
                    CFROST = auto_frost[1]
                if AFROST is None:
                    AFROST = auto_frost[2]
                if verbose:
                    print(f"Frost parameters for {body}: TFROST={TFROST:.1f}K, CFROST={CFROST:.2f}, AFROST={AFROST:.4f}")

        # Set frost defaults if still None
        if TFROST is None:
            TFROST = 148.0
        if CFROST is None:
            CFROST = 3182.48
        if AFROST is None:
            AFROST = 23.3494

    # Build full parameter set
    params = master_params.copy()

    # Override with user parameters
    # Handle ALBEDO (can be time-varying array)
    if isinstance(ALBEDO, (list, tuple, np.ndarray)):
        albedo_value = list(ALBEDO)
        albedo_is_array = True
    else:
        albedo_value = ALBEDO
        albedo_is_array = False

    params.update({
        # Surface properties
        "ALBEDO": albedo_value,
        "ALBEDO_is_array": albedo_is_array,
        "EMISS": EMISS,
        "INERTIA": INERTIA,
        "TDEEP": TDEEP,
        "SLOPE": SLOPE,
        "SLOAZI": SLOAZI,

        # Material properties - upper layer
        "SPEC_HEAT": upper_props["SPEC_HEAT"],
        "DENSITY": upper_props["DENSITY"],
        "COND": upper_props.get("COND", upper_props["SPEC_HEAT"] * upper_props["DENSITY"]),

        # Material properties - lower layer (for two-layer)
        "COND2": lower_props.get("COND", lower_props["SPEC_HEAT"] * lower_props["DENSITY"]),
        "DENS2": lower_props["DENSITY"],
        "SpHeat2": lower_props["SPEC_HEAT"],

        # T-dependent conductivity coefficients - upper layer
        "ConUp0": upper_props["ConUp0"],
        "ConUp1": upper_props["ConUp1"],
        "ConUp2": upper_props["ConUp2"],
        "ConUp3": upper_props["ConUp3"],

        # T-dependent conductivity coefficients - lower layer
        "ConLo0": lower_props["ConUp0"],
        "ConLo1": lower_props["ConUp1"],
        "ConLo2": lower_props["ConUp2"],
        "ConLo3": lower_props["ConUp3"],

        # T-dependent specific heat coefficients - upper layer
        "SphUp0": upper_props["SphUp0"],
        "SphUp1": upper_props["SphUp1"],
        "SphUp2": upper_props["SphUp2"],
        "SphUp3": upper_props["SphUp3"],

        # T-dependent specific heat coefficients - lower layer
        "SphLo0": lower_props["SphUp0"],
        "SphLo1": lower_props["SphUp1"],
        "SphLo2": lower_props["SphUp2"],
        "SphLo3": lower_props["SphUp3"],

        # Atmospheric parameters
        "DUSTA": DUSTA,
        "TAURAT": TAURAT,
        "FANON": FANON,
        "KPREF": KPREF,
        "PTOTAL": PTOTAL,

        # Two-layer regolith parameters
        "IC2": IC2,
        "IIB": IIB,
        "FLAY": FLAY,
        "RLAY": RLAY,
        "LZONE": LZONE,
        "LKofT": LKofT,

        # Time control
        "DELJUL": DELJUL,
        "DJUL": DJUL,
        "LKEY": LKEY,
        "bodyforce": bodyforce,

        # Numerical control
        "N1": N1,
        "N2": N2,
        "N3": N3,
        "NRSET": NRSET,
        "GGT": GGT,

        # Output control
        "TUN8": TUN8,
        "LMST": LMST,
        "WRITE": WRITE,
        "KEEP": KEEP,
        "K4OUT": K4OUT,
        "JBARE": JBARE,
        "TUN_Flx15": TUN_Flx15,

        # Advanced physics
        "PhotoFunc": PhotoFunc,

        # Eclipse modeling
        "Eclipse": Eclipse,
        "Eclipse_Style": Eclipse_Style,
        "Lon_Hr": Lon_Hr,
    })

    # Add optional parameters if they were set
    if GRAV is not None:
        params["GRAV"] = GRAV
    if ARC2_G0 is not None:
        params["ARC2_G0"] = ARC2_G0

    # Add frost parameters (LVFT always written as changecard)
    params["LVFT"] = LVFT
    if LVFT:
        params.update({
            "TFROST": TFROST,
            "CFROST": CFROST,
            "AFROST": AFROST,
        })

    # Add JD/GD alternative date specifications
    if JD is not None:
        params["JD"] = JD
    if GD is not None:
        params["GD"] = GD

    # Add orbital parameter overrides if specified
    if GRAV is not None:
        params["GRAV"] = GRAV
    if DAU is not None:
        params["DAU"] = DAU
    if SOLCON is not None:
        params["SOLCON"] = SOLCON
    if SOLARDEC is not None:
        params["SOLARDEC"] = SOLARDEC
    if ARC2_G0 is not None:
        params["ARC2_G0"] = ARC2_G0
    if LsubS is not None:
        params["LsubS"] = LsubS
    if Atm_Cp is not None:
        params["Atm_Cp"] = Atm_Cp

    # Add stability and anc if specified
    if stability is not None:
        params["stability"] = stability
    if anc is not None:
        params["anc"] = anc

    # Add eclipse parameters if enabled
    if Eclipse == "T":
        if Eclipser is not None:
            params["Eclipser"] = Eclipser
        if Sun_Dis is not None:
            params["Sun_Dis"] = Sun_Dis
        if Eclipser_Rad is not None:
            params["Eclipser_Rad"] = Eclipser_Rad
        if Eclipsed_Rad is not None:
            params["Eclipsed_Rad"] = Eclipsed_Rad
        if CM is not None:
            params["CM"] = CM
        if Gamma is not None:
            params["Gamma"] = Gamma
        if Date is not None:
            params["Date"] = Date
        if Eclipse_line is not None:
            params["Eclipse_line"] = Eclipse_line

    # Add planetary flux parameters if enabled
    if PFlux == "T":
        params["PFlux"] = True
        if BT_Avg is not None:
            params["BT_Avg"] = BT_Avg
        if BT_Min is not None:
            params["BT_Min"] = BT_Min
        if BT_Max is not None:
            params["BT_Max"] = BT_Max
        if IR is not None:
            params["IR"] = IR
        if Vis is not None:
            params["Vis"] = Vis
        if Emissivity is not None:
            params["Emissivity"] = Emissivity
    else:
        params["PFlux"] = False

    # Add atmospheric parameters if provided
    if PTOTAL is not None:
        params["PTOTAL"] = PTOTAL
    if TATM is not None:
        params["TATM"] = TATM

    # Handle TAUD (can be time-varying array)
    if TAUD is not None:
        if isinstance(TAUD, (list, tuple, np.ndarray)):
            params["TAUD"] = list(TAUD)
            params["TAUD_is_array"] = True
        else:
            params["TAUD"] = TAUD
            params["TAUD_is_array"] = False

    # Set body-specific parameters
    if hasattr(body_params, 'rotation_period'):
        params["PERIOD"] = body_params.rotation_period

    # Set N24, N5, and JDISK (now user-configurable)
    params["N24"] = n24_from_porb
    params["N5"] = N5
    params["JDISK"] = JDISK

    # Set PORB data if available
    if hasattr(body_params, 'porb_header'):
        params["PORB_HEADER"] = body_params.porb_header
    if hasattr(body_params, 'porb_text_lines'):
        params["PORB_TEXT_LINES"] = body_params.porb_text_lines  # Pre-formatted text (preferred)
    if hasattr(body_params, 'porb_params'):
        params["PORB_PARAMS"] = body_params.porb_params  # Numeric fallback

    # Override with any additional kwargs
    params.update(kwargs)

    # Set latitude/longitude (support single value or list)
    if isinstance(lat, (list, tuple, np.ndarray)):
        latitudes = list(lat)
        N4 = len(latitudes)
    else:
        latitudes = [lat]
        N4 = 1

    # Set elevations (support single value or list)
    if isinstance(ELEV, (list, tuple, np.ndarray)):
        elevations = list(ELEV)
        if len(elevations) != N4:
            raise ValueError(f"ELEV list length ({len(elevations)}) must match lat list length ({N4})")
    elif ELEV is not None:
        # Single elevation value - use for all latitudes
        elevations = [ELEV] * N4
    else:
        # Default elevation = 0
        elevations = [0.0] * N4

    params["Latitudes"] = latitudes
    params["Elevations"] = elevations
    params["N4"] = N4

    if verbose and N4 > 1:
        print(f"Multi-latitude run: {N4} latitudes from {min(latitudes):.1f}° to {max(latitudes):.1f}°")

    # Set output format
    params["K4OUT"] = 52  # bin52 output

    # Execute KRC
    executor = KRCExecutor(krc_home)

    if verbose:
        print("Running KRC...")

    work_path = Path(workdir) if workdir else None

    # Add PORB-touched params metadata to params dict
    params["_porb_touched_params"] = porb_touched_params

    result = executor.run_krc(
        params,
        workdir=work_path,
        verbose=verbose,
        user_params=user_params  # Pass user-set params for changecard filtering
    )

    if not result["success"]:
        raise RuntimeError(f"KRC execution failed: {result['stderr']}")

    # Parse output
    if verbose:
        print("Parsing output...")

    output = parse_bin52(
        result["output_file"],
        hour=hour,
        ls=ls
    )

    # Add metadata
    output["body"] = body
    output["porb"] = body_params
    output["alb"] = ALBEDO
    output["elev"] = ELEV
    output["lat"] = lat
    output["lon"] = lon
    output["workdir"] = result["workdir"]  # For accessing input file and other outputs

    # Clean up if requested
    if not keep_files and workdir is None:
        import shutil
        shutil.rmtree(result["workdir"])

    return output
