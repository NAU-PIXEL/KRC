"""
PORB (Planetary ORBit) parameter handling.

Encapsulates all PORB-related logic:
- Loading orbital parameters via porb()
- Setting PORB-derived defaults
- Calculating N24, N5, JDISK, DELJUL from orbital data
- Tracking PORB-touched parameters for changecard generation
- Extracting parameters from PORB HDF krc_params
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
