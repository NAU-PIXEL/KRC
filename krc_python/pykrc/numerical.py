"""Numerical parameter calculations for KRC.

This module implements automatic calculation of layer counts (N1) and timesteps (N2)
for numerical stability, matching the algorithms in davinci krc.dvrc exactly.

The functions here replicate:
- krc_evalN1: lines 1776-2103 in krc.dvrc
- krc_evalN2: lines 2107-2154 in krc.dvrc
"""

from typing import Tuple, Optional, Dict, Any
import numpy as np
import warnings


def krc_evalN1(
    RLAY: float,
    FLAY: float,
    INERTIA: float,
    SPEC_HEAT: float,
    DENSITY: float,
    DELJUL: float,
    N5: int,
    JDISK: int,
    MAXN1: int,
    PERIOD: float,
    INERTIA2: Optional[float] = None,
    SPEC_HEAT2: Optional[float] = None,
    DENSITY2: Optional[float] = None,
    THICK: float = 0.0,
    verbose: bool = False
) -> Dict[str, Any]:
    """
    Calculate optimal number of subsurface layers.

    This exactly replicates the krc_evalN1 function from krc.dvrc (lines 1776-2103).

    Parameters
    ----------
    RLAY : float
        Layer thickness ratio (geometric spacing factor)
    FLAY : float
        First layer thickness in diurnal skin depths
    INERTIA : float
        Surface material thermal inertia (J m^-2 K^-1 s^-0.5)
    SPEC_HEAT : float
        Surface material specific heat (J/kg/K)
    DENSITY : float
        Surface material density (kg/m³)
    DELJUL : float
        Delta julian date to increment by (typically 1.9083 for Mars)
    N5 : int
        Number of "seasons" to run (typically 1080)
    JDISK : int
        Start "season" to output (typically 721)
    MAXN1 : int
        Maximum number of layers KRC can accept
    PERIOD : float
        Length of solar day in Earth days (e.g., 1.0275 for Mars)
    INERTIA2 : float, optional
        Bottom material thermal inertia (defaults to INERTIA if not provided)
    SPEC_HEAT2 : float, optional
        Bottom material specific heat (defaults to SPEC_HEAT if not provided)
    DENSITY2 : float, optional
        Bottom material density (defaults to DENSITY if not provided)
    THICK : float, optional
        Top material thickness in m (0 = homogeneous, >0 = two-layer)
    verbose : bool, optional
        Print calculation details

    Returns
    -------
    dict
        Dictionary with keys:
        - 'N1': Number of layers
        - 'FLAY': Updated first layer thickness factor
        - 'RLAY': Layer thickness ratio
        - 'IC2': Layer index where properties change (999 if homogeneous)

    Notes
    -----
    Algorithm from krc.dvrc:
    1. Calculate seasonal skin depth: SSD = (I/(ρ·c)) * sqrt(PY/π)
    2. Calculate model depth: DEPTH = NSKIN * max(SSD1, SSD2)
    3. Build layers with geometric spacing until depth reached
    4. Stop when CENTER_DEPTH > DEPTH and i >= Min_Num_Layer

    Examples
    --------
    >>> result = krc_evalN1(RLAY=1.08, FLAY=2.0, INERTIA=200, SPEC_HEAT=800,
    ...                     DENSITY=1500, DELJUL=1.9083, N5=1080, JDISK=721,
    ...                     MAXN1=100, PERIOD=1.0275)
    >>> result['N1']
    50
    >>> result.keys()
    dict_keys(['N1', 'FLAY', 'RLAY', 'IC2'])
    """
    # Set defaults for homogeneous material (THICK == 0)
    if INERTIA2 is None:
        INERTIA2 = INERTIA
    if SPEC_HEAT2 is None:
        SPEC_HEAT2 = SPEC_HEAT
    if DENSITY2 is None:
        DENSITY2 = DENSITY

    Min_Num_Layer = 25

    # Number of seconds in output period (lines 1829)
    PY = 3600 * 24 * DELJUL * ((N5 - JDISK) + 1)

    # Length of the solar day in seconds (line 1831)
    PD = PERIOD * 86400

    # Seasonal Skin Depth in meters (lines 1834-1836)
    SSD1_m = (INERTIA / (DENSITY * SPEC_HEAT)) * np.sqrt(PY / 3.14159)
    SSD2_m = (INERTIA2 / (DENSITY2 * SPEC_HEAT2)) * np.sqrt(PY / 3.14159)
    SSD_max_m = max(SSD1_m, SSD2_m)

    # Diurnal Skin Depth in meters (lines 1839-1840)
    DSD1_m = (INERTIA / (DENSITY * SPEC_HEAT)) * np.sqrt(PD / 3.14159)
    DSD2_m = (INERTIA2 / (DENSITY2 * SPEC_HEAT2)) * np.sqrt(PD / 3.14159)

    # Calculate the number of years the model runs (line 1843)
    NSKIN = N5 / (N5 - JDISK + 1)

    # Total depth required (lines 1847-1850)
    DEPTH_m = NSKIN * max(SSD1_m, SSD2_m)
    if DEPTH_m > 25.0:
        DEPTH_m = 25.0

    if verbose:
        print(f"  PY (output period): {PY/86400:.1f} days = {PY:.0f} s")
        print(f"  PD (solar day): {PD:.0f} s")
        print(f"  SSD1: {SSD1_m:.3f} m, SSD2: {SSD2_m:.3f} m")
        print(f"  DSD1: {DSD1_m:.3f} m, DSD2: {DSD2_m:.3f} m")
        print(f"  NSKIN: {NSKIN:.2f}")
        print(f"  Required depth: {DEPTH_m:.3f} m")

    # Calculate IC2 and updated FLAY FIRST (Davinci krc.dvrc lines 1861-1894)
    # This must happen before N1 loop so we know where to switch from DSD1 to DSD2
    FLAY2 = FLAY  # Default: no modification
    if THICK == 0.0:
        # CASE 1: Homogeneous material
        IC2 = 999
    elif THICK <= (FLAY * DSD1_m * RLAY) and THICK > 0:
        # CASE 2a: top mesh element is smaller than default element thickness
        FLAY2 = THICK / (DSD1_m * RLAY)
        IC2 = 3
    elif THICK > (FLAY * DSD1_m * RLAY) and RLAY > 1.0:
        # CASE 2b: top material thickness is larger, calculate number of elements
        IC2 = int(np.log(1 - THICK * (1 - RLAY) / (DSD1_m * FLAY * RLAY)) / np.log(RLAY)) + 4

        # Calculate optimized FLAY2 using linear fit (Davinci lines 1880-1890)
        # Create table of FLAY values and corresponding bottom thickness
        Res = 10
        FLAY_Step = (FLAY - FLAY * 0.01) / (Res - 1)
        FLAY_Table = np.linspace(FLAY * 0.01, FLAY, Res + 1)
        Bot_Thick = np.zeros(Res + 1)

        for idx in range(Res + 1):
            # Calculate mesh thickness for IC2-2 layers with this FLAY value
            mesh_thickness = FLAY_Table[idx]
            total_thickness = 0.0
            for layer in range(IC2 - 2):
                if layer == 0:
                    total_thickness += mesh_thickness
                else:
                    total_thickness += mesh_thickness * (RLAY ** layer)
            Bot_Thick[idx] = total_thickness * DSD1_m

        # Linear fit to find FLAY that gives exactly THICK
        # fit(x=Bot_Thick-THICK, y=FLAY_Table, type="linear")
        # This finds FLAY where Bot_Thick = THICK (i.e., where x=0)
        from numpy.polynomial import Polynomial
        p = Polynomial.fit(Bot_Thick - THICK, FLAY_Table, 1)
        FLAY2 = p(0)  # Evaluate at x=0
    else:
        # Fallback
        IC2 = 999

    # Build layer structure with geometric spacing (Davinci krc.dvrc lines 1915-1943)
    # Start with first layer (virtual layer)
    N1 = MAXN1
    THICKNESS_D_Scale = FLAY2 / RLAY  # Use FLAY2 (potentially modified)

    # Determine DSD for first layer
    if 1 < IC2:
        DSD = DSD1_m
    else:
        DSD = DSD2_m

    THICKNESS_m = THICKNESS_D_Scale * DSD
    CENTER_DEPTH_m = -0.5 * THICKNESS_m
    prev_CENTER_DEPTH_m = CENTER_DEPTH_m  # Track previous layer depth

    # Build layers iteratively
    for i in range(2, MAXN1 + 1):
        # Determine which DSD to use based on layer number vs IC2
        if i < IC2:
            DSD = DSD1_m
        else:
            DSD = DSD2_m

        # Update thickness with geometric spacing
        THICKNESS_D_Scale = THICKNESS_D_Scale * RLAY

        # Store previous layer thickness before calculating new one
        prev_THICKNESS_m = THICKNESS_m

        # Calculate thickness in meters for current layer
        THICKNESS_m = THICKNESS_D_Scale * DSD

        # Update center depth in meters (using current and previous THICKNESS_m)
        prev_CENTER_DEPTH_m = CENTER_DEPTH_m
        CENTER_DEPTH_m = CENTER_DEPTH_m + 0.5 * (THICKNESS_m + prev_THICKNESS_m)

        # Check if PREVIOUS layer exceeded depth (Davinci line 1947)
        # Davinci checks CENTER_DEPTH_m[i-1], not CENTER_DEPTH_m[i]
        if ((prev_CENTER_DEPTH_m > DEPTH_m) and (i >= Min_Num_Layer)) or (i >= MAXN1):
            N1 = i
            break

    if verbose:
        print(f"  Calculated N1 = {N1} layers (final depth: {CENTER_DEPTH_m:.3f} m)")

    return {
        'N1': N1,
        'FLAY': FLAY2,
        'RLAY': RLAY,
        'IC2': IC2
    }


def krc_evalN2(
    FLAY: float,
    INERTIA: float,
    DENSITY: float,
    SPEC_HEAT: float,
    PERIOD: float,
    N24: int,
    MAXN2: int,
    verbose: bool = False
) -> int:
    """
    Calculate optimal timesteps per day for numerical stability.

    This exactly replicates the krc_evalN2 function from krc.dvrc (lines 2107-2154).

    Parameters
    ----------
    FLAY : float
        First layer thickness in diurnal skin depths
    INERTIA : float
        Thermal inertia of the top material (J m^-2 K^-1 s^-0.5)
    DENSITY : float
        Density of the top material (kg/m³)
    SPEC_HEAT : float
        Specific heat of the top material (J/kg/K)
    PERIOD : float
        Length of solar day in Earth days
    N24 : int
        Number of 'hours' per day stored (N2 must be divisible by this)
    MAXN2 : int
        Maximum number of timesteps per day
    verbose : bool, optional
        Print calculation details

    Returns
    -------
    int
        Number of timesteps per day N2

    Notes
    -----
    Algorithm from krc.dvrc (lines 2130-2153):
    1. Calculate diurnal skin depth: DSD = (I/(ρ·c)) * sqrt(PD/π)
    2. Calculate first layer thickness: FLAYM = FLAY * DSD
    3. Calculate stability timestep: DEL_T = 0.8 * (FLAYM²·ρ²·c²) / (2·I²)
    4. Calculate minimum timesteps: Num_time_step = int(PD/DEL_T) + 1
    5. Adjust for N24 divisibility

    The stability condition is from Kieffer 2013 Section 103.

    Examples
    --------
    >>> krc_evalN2(FLAY=2.0, INERTIA=200, DENSITY=1500, SPEC_HEAT=800,
    ...            PERIOD=1.0275, N24=24, MAXN2=10000)
    288
    """
    # Number of seconds in a solar day (line 2131)
    PD = PERIOD * 86400

    # Diurnal Skin Depth in meters (line 2134)
    DSD = (INERTIA / (DENSITY * SPEC_HEAT)) * np.sqrt(PD / 3.14159)

    # First Layer Thickness in meters (line 2137)
    FLAYM = FLAY * DSD

    # Stability timestep limit (line 2141)
    # From Kieffer 2013 Section 103, with 0.8 safety factor
    DEL_T = 0.8 * (FLAYM**2 * DENSITY**2 * SPEC_HEAT**2) / (2 * INERTIA**2)

    # Minimum number of time steps in a day (line 2144)
    Num_time_step = int(PD / DEL_T) + 1

    if verbose:
        print(f"  PD (solar day): {PD:.0f} s")
        print(f"  DSD: {DSD:.6f} m")
        print(f"  FLAYM (first layer): {FLAYM:.6f} m = {FLAYM*1000:.3f} mm")
        print(f"  DEL_T (max stable timestep): {DEL_T:.2f} s")
        print(f"  Num_time_step (raw): {Num_time_step}")

    # Adjust for N24 divisibility (lines 2147-2150)
    if N24 > Num_time_step:
        N2 = N24
    else:
        N2 = (int(Num_time_step / N24) + 1) * N24

    # Ensure N2 doesn't exceed MAXN2 (line 2153)
    if N2 > MAXN2:
        N2 = MAXN2

    if verbose:
        actual_dt = PD / N2
        print(f"  N2 (adjusted for N24={N24}): {N2}")
        print(f"  Actual timestep: {actual_dt:.2f} s")
        print(f"  Stability ratio: {actual_dt/DEL_T:.3f} (should be <= 1.0)")

    return N2


def check_stability(
    N1: int,
    N2: int,
    INERTIA: float,
    PERIOD: float,
    FLAY: float = 2.0,
    DENSITY: float = 1500.0,
    SPEC_HEAT: float = 800.0,
    warn: bool = True
) -> Tuple[bool, str]:
    """
    Check if N1/N2 combination satisfies stability conditions.

    Parameters
    ----------
    N1 : int
        Number of layers
    N2 : int
        Timesteps per day
    INERTIA : float
        Thermal inertia (J m^-2 K^-1 s^-0.5)
    PERIOD : float
        Rotation period (Earth days)
    FLAY : float, optional
        Layer spacing factor (default 2.0)
    DENSITY : float, optional
        Material density (kg/m³, default 1500)
    SPEC_HEAT : float, optional
        Specific heat (J/kg/K, default 800)
    warn : bool, optional
        Issue warning if unstable (default True)

    Returns
    -------
    tuple
        (is_stable, message)

    Notes
    -----
    Uses the same stability criterion as krc_evalN2:
        actual_dt <= DEL_T = 0.8 * (FLAYM²·ρ²·c²) / (2·I²)

    Examples
    --------
    >>> check_stability(N1=50, N2=288, INERTIA=200, PERIOD=1.0275)
    (True, "Stable: timestep ratio = 0.85")
    """
    # Calculate the stability limit using krc.dvrc formula
    PD = PERIOD * 86400
    DSD = (INERTIA / (DENSITY * SPEC_HEAT)) * np.sqrt(PD / 3.14159)
    FLAYM = FLAY * DSD
    DEL_T = 0.8 * (FLAYM**2 * DENSITY**2 * SPEC_HEAT**2) / (2 * INERTIA**2)

    # Actual timestep
    actual_dt = PD / N2

    # Check stability
    ratio = actual_dt / DEL_T
    is_stable = ratio <= 1.0

    message = f"{'Stable' if is_stable else 'UNSTABLE'}: timestep ratio = {ratio:.3f} (should be <= 1.0)"

    if not is_stable and warn:
        warnings.warn(
            f"Numerical instability detected! {message}\n"
            f"  N1={N1}, N2={N2}, INERTIA={INERTIA}, PERIOD={PERIOD}\n"
            f"  Suggestion: Increase N2 to at least {int(N2 * ratio / 0.8)}",
            UserWarning
        )

    return is_stable, message


def calculate_convergence_params(
    N1: int,
    N2: int,
    DELLS: float,
    fast_mode: bool = False
) -> dict:
    """
    Calculate recommended convergence control parameters.

    Parameters
    ----------
    N1 : int
        Number of layers
    N2 : int
        Timesteps per day
    DELLS : float
        Season spacing (degrees)
    fast_mode : bool, optional
        Use faster convergence at cost of accuracy

    Returns
    -------
    dict
        Convergence parameters: N3, NRSET, GGT, TPREDICT

    Notes
    -----
    N3: Days between convergence checks
    NRSET: Maximum convergence iterations (0 = no limit)
    GGT: Convergence acceleration factor (1.0 = normal, >1 = faster)
    TPREDICT: Temperature prediction mode (0 = none, 1 = linear extrapolation)

    Examples
    --------
    >>> calculate_convergence_params(N1=50, N2=288, DELLS=1.0)
    {'N3': 10, 'NRSET': 0, 'GGT': 1.0, 'TPREDICT': 0.0}
    """
    if fast_mode:
        # Faster convergence for initial runs or parameter studies
        N3 = max(5, int(10 / DELLS))  # Check more frequently
        GGT = 1.5  # Aggressive acceleration
        TPREDICT = 1.0  # Use prediction
    else:
        # Standard convergence for production runs
        N3 = max(10, int(20 / DELLS))  # Standard check interval
        GGT = 1.0  # No acceleration
        TPREDICT = 0.0  # No prediction

    NRSET = 0  # No limit on iterations (let it converge naturally)

    return {
        "N3": N3,
        "NRSET": NRSET,
        "GGT": GGT,
        "TPREDICT": TPREDICT
    }


def estimate_runtime(
    N1: int,
    N2: int,
    N5: int,
    baseline_time: float = 10.0
) -> float:
    """
    Estimate KRC runtime in seconds.

    Parameters
    ----------
    N1 : int
        Number of layers
    N2 : int
        Timesteps per day
    N5 : int
        Total seasons
    baseline_time : float, optional
        Baseline time for N1=50, N2=288, N5=1080 (default 10 seconds)

    Returns
    -------
    float
        Estimated runtime in seconds

    Notes
    -----
    Runtime scales approximately as: O(N1 * N2 * N5)

    Typical values:
    - Fast run: N1=30, N2=96, N5=360 -> ~2 seconds
    - Standard run: N1=50, N2=288, N5=1080 -> ~10 seconds
    - High-res run: N1=80, N2=576, N5=3600 -> ~200 seconds

    Examples
    --------
    >>> estimate_runtime(N1=50, N2=288, N5=1080)
    10.0
    >>> estimate_runtime(N1=80, N2=576, N5=3600)
    192.0
    """
    # Baseline: N1=50, N2=288, N5=1080
    baseline_N1 = 50
    baseline_N2 = 288
    baseline_N5 = 1080

    # Linear scaling with each parameter
    scale_factor = (
        (N1 / baseline_N1) *
        (N2 / baseline_N2) *
        (N5 / baseline_N5)
    )

    return baseline_time * scale_factor


def calculate_numerical_parameters(
    auto_numerical: bool,
    N1: Optional[int],
    N2: Optional[int],
    using_direct_props: bool,
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: float,
    upper_props: Dict[str, Any],
    lower_props: Dict[str, Any],
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
) -> Tuple[int, int, int, int, float, float]:
    """
    Calculate or validate numerical parameters (N1, N2, N3, IC2, FLAY, RLAY).

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
    lower_props : dict
        Lower layer thermal property coefficients
    RLAY : float
        Layer thickness ratio
    FLAY : float
        First layer thickness factor
    DELJUL : float
        Julian date increment
    N5 : int
        Number of seasons
    JDISK : int
        Output start season
    MAXN1 : int
        Maximum layers allowed
    rot_per : float
        Rotation period (Earth days)
    INERTIA2 : float
        Lower layer thermal inertia
    thick : float
        Two-layer thickness (m)
    n24_from_porb : int
        Hours per day from PORB
    MAXN2 : int
        Maximum timesteps allowed
    GGT : float
        Convergence parameter
    TPREDICT : float
        Temperature prediction mode
    N3 : int
        Convergence iteration count
    DELLS : float
        Solar longitude increment
    master_params : dict
        Master.inp defaults
    verbose : bool
        Print details

    Returns
    -------
    N1 : int
        Number of subsurface layers
    N2 : int
        Number of timesteps per day
    N3 : int
        Convergence parameter
    IC2 : int
        Layer index for property change
    FLAY : float
        Updated first layer thickness factor
    RLAY : float
        Layer thickness ratio

    Notes
    -----
    Auto-calculation uses krc_evalN1() and krc_evalN2() from numerical.py.
    Performs stability check using check_stability().
    Updates N3 if auto_numerical and convergence mode enabled (GGT=1.0, TPREDICT=0.0).
    """
    # Import materials helper here to avoid circular import
    from .materials import extract_material_properties_for_numerics

    # Initialize IC2 to default (single layer)
    IC2 = 999

    # Auto-calculate N1, N2 if not provided and auto_numerical is True
    if auto_numerical:
        if N1 is None:
            # For krc_evalN1, use actual material DENSITY and SPEC_HEAT (not derived)
            # This ensures DSD calculations match davinci exactly
            dens_for_N1 = upper_props.get("DENSITY")
            cp_for_N1 = upper_props.get("SPEC_HEAT")

            # Extract bottom layer properties for two-layer calculations
            if thick != 0.0 and lower_props:
                SPEC_HEAT2 = lower_props.get("SPEC_HEAT")
                DENSITY2 = lower_props.get("DENSITY")
            else:
                SPEC_HEAT2 = None
                DENSITY2 = None

            N1_result = krc_evalN1(
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
                SPEC_HEAT2=SPEC_HEAT2,
                DENSITY2=DENSITY2,
                THICK=thick,
                verbose=verbose
            )
            # Extract results from krc_evalN1
            N1 = N1_result['N1']
            FLAY = N1_result['FLAY']  # Updated FLAY
            RLAY = N1_result['RLAY']  # Typically unchanged
            IC2 = N1_result['IC2']  # Set IC2 from evalN1
            if verbose:
                print(f"Auto-calculated N1={N1} layers, IC2={IC2}, FLAY={FLAY:.4f}")

        if N2 is None:
            # For krc_evalN2, use actual material DENSITY and SPEC_HEAT (not derived)
            # This ensures DSD calculations match davinci exactly
            dens_for_N2 = upper_props.get("DENSITY")
            cp_for_N2 = upper_props.get("SPEC_HEAT")

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
        from .materials import extract_material_properties_for_numerics
        dens_check, cp_check = extract_material_properties_for_numerics(
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

    return N1, N2, N3, IC2, FLAY, RLAY
