"""Main KRC interface function."""

from pathlib import Path
from typing import Dict, Any, Optional, Union, List, Tuple, Set
import tempfile
import numpy as np

from pykrc.config import get_krc_home, get_paths
from pykrc.input_processor import parse_master_inp
from pykrc.data_loaders import KRCDataLoader
from pykrc.ancillary import load_ancillary_data
from pykrc.materials import calculate_material_properties
from pykrc.numerical import calculate_numerical_parameters
from pykrc.porb_handler import setup_orbital_parameters, OrbitalElements
from pykrc.executor import KRCExecutor
from pykrc.bin_parser import parse_bin52
from pykrc.layers import calculate_IC2, validate_two_layer_config
from pykrc.frost import get_frost_params_for_body, validate_frost_config
from pykrc.validation import validate_all_parameters, KRCValidationError
from pykrc.defaults import USER_DEFAULTS, TPREDICT_STABILITY_OVERRIDES


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

    # Time control defaults - use centralized USER_DEFAULTS
    defaults['DELLS'] = USER_DEFAULTS['DELLS'] if DELLS is None else DELLS
    defaults['spinup_years'] = USER_DEFAULTS['spinup_years'] if spinup_years is None else spinup_years
    defaults['output_years'] = USER_DEFAULTS['output_years'] if output_years is None else output_years
    defaults['LKEY'] = USER_DEFAULTS['LKEY'] if LKEY is None else LKEY

    # Material properties
    defaults['LKofT'] = USER_DEFAULTS['LKofT'] if LKofT is None else LKofT
    defaults['thick'] = USER_DEFAULTS['thick'] if thick is None else thick

    # Surface/atmospheric parameters (non-PORB)
    defaults['FANON'] = USER_DEFAULTS['FANON'] if FANON is None else FANON

    # Numerical control - TPREDICT will be set later based on DELJUL/PERIOD if None
    # (see lines 947-973 in krc() where TPREDICT is auto-determined)
    defaults['TPREDICT'] = TPREDICT  # Keep user value or None (will be set later if None)

    # Apply TPREDICT stability overrides only if user explicitly set TPREDICT=0.0
    # (If TPREDICT is None, it will be auto-determined later and overrides applied then)
    if TPREDICT == 0.0:
        defaults['GGT'] = TPREDICT_STABILITY_OVERRIDES['GGT'] if GGT is None else GGT
        defaults['N3'] = TPREDICT_STABILITY_OVERRIDES['N3'] if N3 is None else N3
        defaults['NRSET'] = TPREDICT_STABILITY_OVERRIDES['NRSET'] if NRSET is None else NRSET
    else:
        # Keep user values or None (will be set later based on TPREDICT auto-determination)
        defaults['GGT'] = GGT
        defaults['N3'] = N3
        defaults['NRSET'] = NRSET

    defaults['MAXN1'] = USER_DEFAULTS['MAXN1'] if MAXN1 is None else MAXN1
    defaults['MAXN2'] = USER_DEFAULTS['MAXN2'] if MAXN2 is None else MAXN2
    defaults['auto_numerical'] = USER_DEFAULTS['auto_numerical'] if auto_numerical is None else auto_numerical

    # Output control
    defaults['bodyforce'] = USER_DEFAULTS['bodyforce'] if bodyforce is None else bodyforce
    defaults['TUN8'] = USER_DEFAULTS['TUN8'] if TUN8 is None else TUN8

    # Flags
    defaults['LMST'] = USER_DEFAULTS['LMST'] if LMST is None else LMST
    defaults['WRITE'] = USER_DEFAULTS['WRITE'] if WRITE is None else WRITE
    defaults['KEEP'] = USER_DEFAULTS['KEEP'] if KEEP is None else KEEP
    defaults['Eclipse'] = USER_DEFAULTS['Eclipse'] if Eclipse is None else Eclipse
    defaults['Eclipse_Style'] = USER_DEFAULTS['Eclipse_Style'] if Eclipse_Style is None else Eclipse_Style
    defaults['PFlux'] = USER_DEFAULTS['PFlux'] if PFlux is None else PFlux
    defaults['Lon_Hr'] = USER_DEFAULTS['Lon_Hr'] if Lon_Hr is None else Lon_Hr

    # Runtime control
    defaults['verbose'] = USER_DEFAULTS['verbose'] if verbose is None else verbose
    defaults['keep_files'] = USER_DEFAULTS['keep_files'] if keep_files is None else keep_files

    # Location (lon gets default if not set) - not in USER_DEFAULTS, keep hardcoded
    defaults['lon'] = 0.0 if lon is None else lon

    return defaults








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

    # ========== SETUP ORBITAL PARAMETERS ==========
    # Use porb_handler module for clean PORB logic abstraction
    body_params, porb_params, porb_touched_params = setup_orbital_parameters(
        body=body,
        data_loader=data_loader,
        DELLS=DELLS,
        spinup_years=spinup_years,
        output_years=output_years,
        N5=N5,
        JDISK=JDISK,
        PTOTAL=PTOTAL,
        GRAV=GRAV,
        TAURAT=TAURAT,
        DUSTA=DUSTA,
        ARC2_G0=ARC2_G0,
        EMISS=EMISS,
        TDEEP=TDEEP,
        TAUD=TAUD,
        DJUL=DJUL,
        SLOPE=SLOPE,
        SLOAZI=SLOAZI,
        TFROST=TFROST,
        PhotoFunc=PhotoFunc,
        FLAY=FLAY,
        RLAY=RLAY,
        IIB=IIB,
        IC2=IC2,
        KPREF=KPREF,
        JBARE=JBARE,
        LVFT=LVFT,
        LKofT=LKofT,
        LZONE=LZONE,
        verbose=verbose
    )

    # Extract PORB-derived values
    rot_per = porb_params['PERIOD']
    n24_from_porb = porb_params['N24']
    N5 = porb_params['N5']
    JDISK = porb_params['JDISK']
    DELJUL = porb_params['DELJUL']
    K4OUT = porb_params['K4OUT']
    TUN_Flx15 = porb_params['TUN_Flx15']

    # Extract PORB-set parameters (only if they were set by PORB, not user)
    if 'PTOTAL' in porb_params and PTOTAL is None:
        PTOTAL = porb_params['PTOTAL']
    if 'GRAV' in porb_params and GRAV is None:
        GRAV = porb_params['GRAV']
    if 'TAURAT' in porb_params and TAURAT is None:
        TAURAT = porb_params['TAURAT']
    if 'DUSTA' in porb_params and DUSTA is None:
        DUSTA = porb_params['DUSTA']
    if 'ARC2_G0' in porb_params and ARC2_G0 is None:
        ARC2_G0 = porb_params['ARC2_G0']
    if 'EMISS' in porb_params and EMISS is None:
        EMISS = porb_params['EMISS']
    if 'TDEEP' in porb_params and TDEEP is None:
        TDEEP = porb_params['TDEEP']
    if 'TAUD' in porb_params and TAUD is None:
        TAUD = porb_params['TAUD']
    if 'DJUL' in porb_params and DJUL is None:
        DJUL = porb_params['DJUL']
    if 'SLOPE' in porb_params and SLOPE is None:
        SLOPE = porb_params['SLOPE']
    if 'SLOAZI' in porb_params and SLOAZI is None:
        SLOAZI = porb_params['SLOAZI']
    if 'TFROST' in porb_params and TFROST is None:
        TFROST = porb_params['TFROST']
    if 'PhotoFunc' in porb_params and PhotoFunc is None:
        PhotoFunc = porb_params['PhotoFunc']
    if 'FLAY' in porb_params and FLAY is None:
        FLAY = porb_params['FLAY']
    if 'RLAY' in porb_params and RLAY is None:
        RLAY = porb_params['RLAY']
    if 'IIB' in porb_params and IIB is None:
        IIB = porb_params['IIB']
    if 'KPREF' in porb_params and KPREF is None:
        KPREF = porb_params['KPREF']
    if 'JBARE' in porb_params and JBARE is None:
        JBARE = porb_params['JBARE']
    if 'LVFT' in porb_params and LVFT is None:
        LVFT = porb_params['LVFT']
    if 'LKofT' in porb_params and LKofT is None:
        LKofT = porb_params['LKofT']
    if 'LZONE' in porb_params and LZONE is None:
        LZONE = porb_params['LZONE']

    # Apply TPREDICT conditional logic (matches Davinci krc.dvrc lines 853-865)
    # This handles if we are using KRC's Temperature Predicting capabilities
    # for increased speed. For LOW DELJULs, the model may not reach stability
    # before the prediction happens, so we disable it.
    if TPREDICT is None:
        if DELJUL <= 3 * rot_per:
            # Short timesteps - disable prediction for stability
            TPREDICT = 0.0  # "F" in Davinci
            if verbose:
                print(f"TPREDICT auto-disabled: DELJUL ({DELJUL:.4f}) <= 3*PERIOD ({3*rot_per:.4f})")
        else:
            # Long timesteps - enable prediction for speed
            TPREDICT = 1.0  # "T" in Davinci (non-zero = enabled)
            if verbose:
                print(f"TPREDICT auto-enabled: DELJUL ({DELJUL:.4f}) > 3*PERIOD ({3*rot_per:.4f})")

    # Now set N3, NRSET, GGT based on TPREDICT mode
    if TPREDICT == 0.0:  # TPREDICT="F" - prediction disabled
        if GGT is None:
            GGT = 99.0  # Disable convergence acceleration
        if N3 is None:
            N3 = 1  # Single iteration per timestep
        if NRSET is None:
            NRSET = 999  # Reset temperature every timestep
    else:  # TPREDICT="T" or non-zero - prediction enabled
        if GGT is None:
            GGT = master_params.get('GGT', 0.1)  # Use master.inp default
        if N3 is None:
            N3 = master_params.get('N3', 15)  # Use master.inp default
        if NRSET is None:
            NRSET = master_params.get('NRSET', 3)  # Use master.inp default

    # ========== LOAD ANCILLARY DATA ==========
    # Load albedo, elevation, and inertia from ancillary data (Mars) or defaults
    ALBEDO, ELEV, INERTIA = load_ancillary_data(
        body, lat, lon, ALBEDO, ELEV, INERTIA, master_params, verbose
    )

    # ========== SET BODY-SPECIFIC k_style ==========
    # Davinci krc.dvrc lines 570, 583, 599: k_style depends on body
    # Only set k_style if user didn't explicitly provide it
    if 'k_style' not in user_params:
        # User didn't override - set based on body
        if body == "Mars":
            k_style = "Mars"  # sqrt(T) trend
        elif body == "Europa":
            k_style = "Moon"  # T^3 trend
        else:
            k_style = "Moon"  # T^3 trend for all other bodies (Phobos, Moon, etc.)
    # else: user explicitly set k_style, keep it

    # ========== MATERIAL PROPERTY HANDLING ==========
    # Calculate thermal properties for upper and lower layers
    using_direct_props, INERTIA, INERTIA2, upper_props, lower_props = calculate_material_properties(
        COND, DENSITY, SPEC_HEAT, INERTIA, Mat1, Mat2, T_user, k_style,
        LKofT, INERTIA2, thick, master_params, verbose
    )

    # ========== NUMERICAL PARAMETERS ==========
    # Calculate or validate numerical parameters (N1, N2, N3)
    N1, N2, N3, IC2, FLAY, RLAY = calculate_numerical_parameters(
        auto_numerical, N1, N2, using_direct_props, DENSITY, SPEC_HEAT,
        INERTIA, upper_props, lower_props, RLAY, FLAY, DELJUL, N5, JDISK, MAXN1,
        rot_per, INERTIA2, thick, n24_from_porb, MAXN2, GGT, TPREDICT,
        N3, DELLS, master_params, verbose
    )

    # ========== TWO-LAYER CONFIGURATION ==========
    # Validate two-layer configuration
    validate_two_layer_config(thick, INERTIA, INERTIA2, Mat1, Mat2, Por1, Por2)

    # IC2 is now set from krc_evalN1() if N1 was auto-calculated
    # If user provided N1, we still need to calculate IC2
    if IC2 is None:
        IC2 = calculate_IC2(thick, N1, FLAY, RLAY)
        if verbose and thick != 0.0:
            print(f"Calculated IC2={IC2} for thick={thick}m (user-provided N1)")

    # Ensure IC2, FLAY, RLAY have changecards written
    porb_touched_params.add('IC2')
    porb_touched_params.add('FLAY')
    porb_touched_params.add('RLAY')

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
