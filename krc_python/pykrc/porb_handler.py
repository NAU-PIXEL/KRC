"""
PORB (Planetary ORBit) parameter handling and orbital mechanics.

This module combines orbital mechanics and PORB parameter handling:
- Orbital elements and body database (OrbitalElements dataclass)
- Loading orbital parameters via porb()
- Setting PORB-derived defaults
- Calculating N24, N5, JDISK, DELJUL from orbital data
- Tracking PORB-touched parameters for changecard generation
- Extracting parameters from PORB HDF krc_params
- Utility functions for orbital calculations (future features)
"""

from typing import Dict, Any, Optional, Tuple, Set
from dataclasses import dataclass
import numpy as np
import h5py
from pathlib import Path

from .data_loaders import KRCDataLoader
from .defaults import get_porb_defaults, get_porb_touched_params


# ============================================================================
# Orbital Elements and Body Database
# ============================================================================

@dataclass
class OrbitalElements:
    """Orbital elements for a celestial body."""
    name: str
    body_type: str  # "Planet", "Satellite", "Asteroid", "Comet", "Generic", "Exoplanet"
    parent_body: Optional[str] = None

    # Orbital elements (Standish table format)
    epoch: float = 0.0
    eccentricity: float = 0.0
    semi_major_axis: float = 0.0  # AU
    perihelion: float = 0.0
    inclination: float = 0.0
    node: float = 0.0
    mean_anomaly: float = 0.0

    # Rotation parameters
    rotation_period: float = 1.0  # days
    pole_ra: float = 0.0  # degrees
    pole_dec: float = 0.0  # degrees
    meridian: float = 0.0  # degrees

    # Physical parameters
    radius: float = 1.0  # km
    mass: float = 1.0  # kg
    obliquity: float = 0.0  # degrees

    # Derived/calculated
    orbital_radius: Optional[float] = None  # km (for satellites)
    orbital_period: Optional[float] = None  # days


# Default body database (simplified - in real implementation, load from files)
BODY_DEFAULTS = {
    "Mars": {
        "body_type": "Planet",
        "rotation_period": 1.0275,
        "radius": 3397.0,
        "obliquity": 25.19,
        "semi_major_axis": 1.524,
    },
    "Phobos": {
        "body_type": "Satellite",
        "parent_body": "Mars",
        "rotation_period": 0.3189,
        "radius": 11.1,
        "orbital_radius": 9376.0,
    },
    "Earth": {
        "body_type": "Planet",
        "rotation_period": 1.0,
        "radius": 6371.0,
        "obliquity": 23.44,
        "semi_major_axis": 1.0,
    },
}


# ============================================================================
# Orbital Mechanics Functions
# ============================================================================

def identify_body_type(body_name: str) -> str:
    """
    Identify the type of celestial body.

    Parameters
    ----------
    body_name : str
        Name of the body

    Returns
    -------
    str
        Body type: "Planet", "Satellite", "Asteroid", "Comet", or "Unknown"
    """
    # Simple heuristics - in real implementation, query databases
    if body_name in ["Mercury", "Venus", "Earth", "Mars", "Jupiter",
                     "Saturn", "Uranus", "Neptune", "Pluto"]:
        return "Planet"

    if body_name in ["Moon", "Phobos", "Deimos", "Io", "Europa",
                     "Ganymede", "Callisto"]:
        return "Satellite"

    if body_name in ["Ceres", "Vesta", "Pallas", "Juno", "Bennu", "Dinkinesh"]:
        return "Asteroid"

    if "P-" in body_name or body_name.endswith("P"):
        return "Comet"

    return "Unknown"


def load_body_parameters(body_name: str, data_loader=None) -> Dict[str, Any]:
    """
    Load orbital parameters for a celestial body from HDF files.

    Parameters
    ----------
    body_name : str
        Name of the celestial body
    data_loader : KRCDataLoader, optional
        Data loader instance for accessing files

    Returns
    -------
    dict
        Dictionary of body parameters including PORB_HEADER and PORB_PARAMS

    Raises
    ------
    ValueError
        If body not found in database
    """
    params = {}

    # Try to load from porb_defaults directory
    if data_loader:
        porb_file = data_loader.support_dir / "porb_defaults" / f"{body_name}.porb.hdf"

        if porb_file.exists():
            with h5py.File(porb_file, 'r') as f:
                # Read the 'rot' field which contains the complete PORB text block
                if 'rot' in f:
                    rot_text = f['rot'][0].decode('utf-8')
                    # Split into lines
                    lines = rot_text.strip().split('\n')
                    if lines:
                        # First line is the PORB header
                        params['PORB_HEADER'] = lines[0]

                        # Store pre-formatted text lines (6 lines) for exact davinci parity
                        # This eliminates the need for G15.7 formatting and guarantees
                        # character-by-character match with davinci output
                        if len(lines) >= 7:
                            params['PORB_TEXT_LINES'] = lines[1:7]

                        # Also parse to numeric for backward compatibility
                        # (used as fallback for generic bodies without pre-formatted text)
                        porb_params = []
                        for line in lines[1:7]:
                            values = line.split()
                            for val in values:
                                try:
                                    porb_params.append(float(val))
                                except ValueError:
                                    pass

                        if len(porb_params) == 30:
                            params['PORB_PARAMS'] = porb_params

                # Read KRC-specific parameters
                if 'krc' in f:
                    krc_group = f['krc']
                    for key in krc_group.keys():
                        dataset = krc_group[key]
                        # Extract scalar value
                        val = dataset[0, 0, 0]
                        params[key.upper()] = float(val) if hasattr(val, 'dtype') else val

                # Read rotation period (stored in hours, convert to days)
                if 'rot_per' in f:
                    rot_per_hours = float(f['rot_per'][0, 0, 0])
                    params['rotation_period'] = rot_per_hours / 24.0  # Convert hours to days

                # Read orbital period
                if 'period' in f:
                    params['orbital_period'] = float(f['period'][0, 0, 0])

                # Read body type
                if 'type/body_type' in f:
                    params['body_type'] = f['type/body_type'][0].decode('utf-8')

                return params

    # Fallback to hardcoded defaults
    if body_name in BODY_DEFAULTS:
        return BODY_DEFAULTS[body_name].copy()

    raise ValueError(f"Body '{body_name}' not found in database")


def porb(
    body: str | Dict[str, Any],
    epoch: float = 0.10,
    force: bool = False,
    data_loader=None
) -> OrbitalElements:
    """
    Calculate orbital parameters and rotation matrix for a body.

    This is a simplified implementation. The full version would:
    1. Load orbital elements from Standish/Horizons data
    2. Run the PORB Fortran program to calculate rotation matrices
    3. Cache results in porb_defaults directory

    Parameters
    ----------
    body : str or dict
        Body name or structure with orbital parameters
    epoch : float, optional
        Fraction of century for start date (0.10 = 2010)
    force : bool, optional
        Force recalculation even if cached
    data_loader : KRCDataLoader, optional
        Data loader instance

    Returns
    -------
    OrbitalElements
        Orbital elements and rotation parameters

    Notes
    -----
    This is a placeholder implementation. Full implementation would:
    - Parse standish.tab, spinaxis.tab files
    - Run porbmn Fortran executable
    - Read rotation matrix from output
    """
    if isinstance(body, dict):
        # Generic body structure provided
        return OrbitalElements(
            name=body.get("name", "Generic"),
            body_type="Generic",
            **body
        )

    # Load from database
    body_type = identify_body_type(body)
    params = load_body_parameters(body, data_loader)

    # Extract PORB data (these don't go into OrbitalElements)
    porb_header = params.pop('PORB_HEADER', None)
    porb_text_lines = params.pop('PORB_TEXT_LINES', None)
    porb_params = params.pop('PORB_PARAMS', None)

    # Extract KRC-specific parameters (these also don't go into OrbitalElements)
    krc_params = {}
    krc_keys = ['ARC2_G0', 'DELJUL', 'DUSTA', 'GRAV', 'N24', 'PERIOD', 'PTOTAL', 'TAURAT']
    for key in krc_keys:
        if key in params:
            krc_params[key] = params.pop(key)

    # Don't pass body_type twice if it's already in params
    if 'body_type' in params:
        body_type = params.pop('body_type')

    # Create OrbitalElements with only the orbital parameters
    orbital_elem = OrbitalElements(
        name=body,
        body_type=body_type,
        **params
    )

    # Store PORB data as attributes for use in input generation
    if porb_header:
        orbital_elem.porb_header = porb_header
    if porb_text_lines:
        orbital_elem.porb_text_lines = porb_text_lines  # Pre-formatted text (preferred)
    if porb_params:
        orbital_elem.porb_params = porb_params  # Numeric values (fallback)
    if krc_params:
        orbital_elem.krc_params = krc_params

    return orbital_elem


def generic_porb(
    name: str,
    epoch: float = 2000.0,
    e: float = 0.0,
    a: float = 1.0,
    w: float = 0.0,
    i: float = 0.0,
    node: float = 0.0,
    peri: float = 0.0,
    m: float = 0.0,
    rot_per: float = 1.0,
    polera: float = 0.0,
    poledec: float = 90.0,
    merid: float = 0.0,
    period: Optional[float] = None
) -> OrbitalElements:
    """
    Create generic orbital elements from user-specified parameters.

    This function is a Davinci compatibility function for future features.

    Parameters
    ----------
    name : str
        Name of the body
    epoch : float
        Epoch (Julian date or year)
    e : float
        Eccentricity
    a : float
        Semi-major axis (AU)
    w : float
        Argument of perihelion (degrees)
    i : float
        Inclination (degrees)
    node : float
        Longitude of ascending node (degrees)
    peri : float
        Longitude of perihelion (degrees)
    m : float
        Mean anomaly (degrees)
    rot_per : float
        Rotation period (days)
    polera : float
        Pole right ascension (degrees)
    poledec : float
        Pole declination (degrees)
    merid : float
        Prime meridian (degrees)
    period : float, optional
        Orbital period (days)

    Returns
    -------
    OrbitalElements
        Generic orbital elements
    """
    # Calculate period from Kepler's third law if not provided
    if period is None:
        period = 365.25 * a**1.5  # Approximate for solar orbit

    return OrbitalElements(
        name=name,
        body_type="Generic",
        epoch=epoch,
        eccentricity=e,
        semi_major_axis=a,
        perihelion=peri,
        inclination=i,
        node=node,
        mean_anomaly=m,
        rotation_period=rot_per,
        pole_ra=polera,
        pole_dec=poledec,
        meridian=merid,
        orbital_period=period,
    )


def calculate_solar_longitude(julian_date: float, body: OrbitalElements) -> float:
    """
    Calculate solar longitude (Ls) for a given Julian date.

    This function is a Davinci compatibility function for future features.

    Parameters
    ----------
    julian_date : float
        Julian date
    body : OrbitalElements
        Orbital elements for the body

    Returns
    -------
    float
        Solar longitude in degrees (0-360)

    Notes
    -----
    Simplified calculation. Full implementation would use precise
    orbital mechanics.
    """
    # Placeholder - simplified calculation
    # Real implementation would account for eccentricity, perihelion, etc.
    if body.orbital_period:
        days_since_epoch = julian_date - body.epoch
        mean_motion = 360.0 / body.orbital_period
        Ls = (mean_motion * days_since_epoch) % 360.0
        return Ls
    else:
        return 0.0


def gd_to_jd(year: int, month: int, day: int, hour: float = 0.0) -> float:
    """
    Convert Gregorian date to Julian date.

    This function is a Davinci compatibility function for future features.

    Parameters
    ----------
    year : int
        Year
    month : int
        Month (1-12)
    day : int
        Day of month
    hour : float, optional
        Hour of day (0-24)

    Returns
    -------
    float
        Julian date
    """
    # Standard Julian date conversion
    a = (14 - month) // 12
    y = year + 4800 - a
    m = month + 12*a - 3

    jdn = day + (153*m + 2)//5 + 365*y + y//4 - y//100 + y//400 - 32045
    jd = jdn + (hour / 24.0) - 0.5

    return jd


def ls_to_date(ls: float, year: int, body: str = "Mars") -> tuple:
    """
    Convert solar longitude to calendar date.

    This function is a Davinci compatibility function for future features.

    Parameters
    ----------
    ls : float
        Solar longitude (degrees)
    year : int
        Earth year
    body : str
        Celestial body name

    Returns
    -------
    tuple
        (year, month, day, hour)

    Notes
    -----
    Simplified placeholder. Full implementation would use
    precise orbit calculations.
    """
    # Placeholder
    raise NotImplementedError("Ls to date conversion not yet implemented")


# ============================================================================
# PORB Parameter Setup
# ============================================================================


def setup_orbital_parameters(
    body: str,
    data_loader: KRCDataLoader,
    DELLS: float,
    spinup_years: float,
    output_years: float,
    # User-provided parameters (may override PORB defaults)
    N5: Optional[int] = None,
    JDISK: Optional[int] = None,
    PTOTAL: Optional[float] = None,
    GRAV: Optional[float] = None,
    TAURAT: Optional[float] = None,
    DUSTA: Optional[float] = None,
    ARC2_G0: Optional[float] = None,
    EMISS: Optional[float] = None,
    TDEEP: Optional[float] = None,
    TAUD: Optional[float] = None,
    DJUL: Optional[float] = None,
    SLOPE: Optional[float] = None,
    SLOAZI: Optional[float] = None,
    TFROST: Optional[float] = None,
    PhotoFunc: Optional[float] = None,
    FLAY: Optional[float] = None,
    RLAY: Optional[float] = None,
    IIB: Optional[int] = None,
    IC2: Optional[int] = None,
    KPREF: Optional[int] = None,
    JBARE: Optional[int] = None,
    LVFT: Optional[bool] = None,
    LKofT: Optional[bool] = None,
    LZONE: Optional[bool] = None,
    verbose: bool = False
) -> Tuple[OrbitalElements, Dict[str, Any], Set[str]]:
    """
    Load orbital parameters and apply PORB-derived defaults.

    This function implements Davinci krc.dvrc PORB loading behavior where certain
    parameters get default values when PORB is loaded, ensuring changecards are
    written even if they match master.inp defaults.

    Parameters
    ----------
    body : str
        Celestial body name (e.g., "Mars", "Moon", "Europa")
    data_loader : KRCDataLoader
        Data loader instance for accessing PORB files
    DELLS : float
        Seasonal step size (degrees Ls)
    spinup_years : float
        Spinup time (years)
    output_years : float
        Output time (years)
    N5 : int, optional
        Number of seasonal steps (calculated if None)
    JDISK : int, optional
        Disk output start season (calculated if None)
    [... all other PORB-touchable parameters ...]
    verbose : bool
        Print details during setup

    Returns
    -------
    body_params : OrbitalElements
        Orbital/physical parameters for the body
    porb_params : dict
        Parameters set by PORB (with calculated and default values)
    porb_touched : set
        Set of parameter names touched by PORB (for changecard generation)

    Notes
    -----
    PORB-touched parameters always get changecards written, even when they
    match master.inp header defaults. This ensures Davinci parity.

    The returned porb_params dict contains:
    - PORB HDF overrides (PTOTAL, GRAV, etc. from krc_params)
    - PORB standard defaults (EMISS, TDEEP, etc.)
    - Calculated values (N24, N5, JDISK, DELJUL, PERIOD)

    Examples
    --------
    >>> from pykrc.data_loaders import KRCDataLoader
    >>> from pykrc.config import get_paths
    >>> paths = get_paths()
    >>> loader = KRCDataLoader(paths.support_dir)
    >>> body_params, porb_params, touched = setup_orbital_parameters(
    ...     "Mars", loader, DELLS=1.0, spinup_years=2.0, output_years=1.0
    ... )
    >>> body_params.rotation_period
    1.0275
    >>> porb_params['N24']
    96
    >>> 'EMISS' in touched
    True
    """
    if verbose:
        print(f"Loading orbital parameters for {body}...")

    # Load PORB data
    body_params = porb(body, data_loader=data_loader)

    # Get rotation period for calculations
    rot_per = body_params.rotation_period

    # Calculate N24 from rotation period (Davinci krc.dvrc lines 2755-2756)
    # N24 = number of output timesteps per day
    # Minimum value is 96 (every 15 minutes)
    n24_from_porb = int(np.floor((rot_per * 4) / 96) * 96)
    if n24_from_porb < 96:
        n24_from_porb = 96

    # Calculate N5 and JDISK if not provided by user
    if N5 is None:
        total_years = spinup_years + output_years
        N5 = int(np.ceil(360.0 / DELLS * total_years))

    if JDISK is None:
        JDISK = int(np.ceil(360.0 / DELLS * spinup_years + 1))

    # Get DELJUL (time step in Julian days for each Ls degree)
    if hasattr(body_params, 'krc_params') and 'DELJUL' in body_params.krc_params:
        # Use pre-calculated value from PORB (more accurate)
        DELJUL_calc = body_params.krc_params['DELJUL']
    elif hasattr(body_params, 'orbital_period'):
        # Calculate from orbital period (not rotation period!)
        DELJUL_calc = body_params.orbital_period * DELLS / 360.0
    else:
        # Fallback: estimate from rotation period (will be wrong for planets!)
        DELJUL_calc = rot_per * DELLS / 360.0
        if verbose:
            print(f"Warning: Using rotation period for DELJUL calculation (may be inaccurate)")

    if verbose:
        print(f"Time control: DELLS={DELLS}°, N5={N5} seasons, JDISK={JDISK}")
        print(f"  Total run: {N5*DELLS/360:.1f} years, Output: {(N5-JDISK)*DELLS/360:.1f} years")

    # Get canonical PORB defaults and touched parameters
    porb_params = get_porb_defaults()
    porb_touched = get_porb_touched_params()

    # Override with PORB HDF krc_params if available and user didn't specify
    if hasattr(body_params, 'krc_params') and body_params.krc_params:
        krc_params = body_params.krc_params

        # Extract parameters from PORB HDF file
        hdf_params = {
            'PTOTAL': PTOTAL,
            'GRAV': GRAV,
            'TAURAT': TAURAT,
            'DUSTA': DUSTA,
            'ARC2_G0': ARC2_G0
        }

        for param_name, user_value in hdf_params.items():
            if param_name in krc_params and user_value is None:
                porb_params[param_name] = krc_params[param_name]
                porb_touched.add(param_name)

    # Apply PORB standard defaults (only if user didn't specify)
    # These get set even if they match master.inp to ensure changecards are written
    standard_defaults = {
        'EMISS': (EMISS, 1.0),
        'TDEEP': (TDEEP, 180.0),
        'TAUD': (TAUD, 0.3),
        'DJUL': (DJUL, 0.1),
        'SLOPE': (SLOPE, 0.0),
        'SLOAZI': (SLOAZI, 0.0),
        'TFROST': (TFROST, 146.0),
        'PhotoFunc': (PhotoFunc, 0.0),
        'FLAY': (FLAY, 0.10),
        'RLAY': (RLAY, 1.15),
        'IIB': (IIB, -1),
        'IC2': (IC2, 999),
        'KPREF': (KPREF, 1),
        'JBARE': (JBARE, 0),
        'LVFT': (LVFT, False),
        'LKofT': (LKofT, True),
        'LZONE': (LZONE, False),
    }

    for param_name, (user_value, default_value) in standard_defaults.items():
        if user_value is None:
            porb_params[param_name] = default_value
            porb_touched.add(param_name)
        else:
            porb_params[param_name] = user_value

    # Add internal parameters (always set by PORB)
    porb_params['K4OUT'] = 52
    porb_params['TUN_Flx15'] = 0
    porb_touched.add('K4OUT')
    porb_touched.add('TUN_Flx15')

    # Add calculated values
    porb_params['N24'] = n24_from_porb
    porb_params['N5'] = N5
    porb_params['JDISK'] = JDISK
    porb_params['DELJUL'] = DELJUL_calc
    porb_params['PERIOD'] = rot_per

    # Add PORB data for output
    if hasattr(body_params, 'porb_header'):
        porb_params['PORB_HEADER'] = body_params.porb_header
    if hasattr(body_params, 'porb_text_lines'):
        porb_params['PORB_TEXT_LINES'] = body_params.porb_text_lines  # Pre-formatted (preferred)
    if hasattr(body_params, 'porb_params'):
        porb_params['PORB_PARAMS'] = body_params.porb_params  # Numeric (fallback)

    return body_params, porb_params, porb_touched
