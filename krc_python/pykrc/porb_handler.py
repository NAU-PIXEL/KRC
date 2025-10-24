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

from typing import Dict, Any, Optional, Tuple, Set, Union
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
    "Moon": {
        "body_type": "Satellite",
        "parent_body": "Earth",
        "rotation_period": 27.32,  # Synchronous rotation (1 lunar day = ~27.32 Earth days)
        "radius": 1737.4,  # km
        "obliquity": 6.68,  # degrees
        "mass": 7.342e22,  # kg
        "orbital_radius": 384400.0,  # km from Earth
        "semi_major_axis": 1.0,  # AU (Earth's orbit)
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
                    # Split into lines - strip only outer newlines, preserve trailing spaces
                    # Per Davinci krc.dvrc, PORB text lines have exact formatting including trailing spaces
                    lines = rot_text.strip('\n').split('\n')
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


def _parse_standish_table(standish_path: Path, parent_body: str) -> Optional[list]:
    """
    Parse standish.tab to extract parent body orbital elements.

    Per Davinci krc.dvrc lines 2307-2316.

    Returns
    -------
    list or None
        Lines containing "C_END" header and parent body data (2 lines total)
    """
    with open(standish_path) as f:
        lines = [line.rstrip('\n') for line in f]

    # Find C_END marker
    c_end_idx = None
    for i, line in enumerate(lines):
        if line.startswith('C_END'):
            c_end_idx = i
            break

    if c_end_idx is None:
        return None

    # Find parent body after C_END
    for i in range(c_end_idx + 1, len(lines) - 1):
        if lines[i].strip().startswith(parent_body):
            # Return C_END + body line + rate line
            return ['C_END         a          e           I             L             w           Omega',
                    lines[i],
                    lines[i+1]]

    return None


def _parse_spinaxis_table(spinaxis_path: Path, body: str) -> Optional[list]:
    """
    Parse spinaxis.tab to extract body rotation parameters.

    Per Davinci krc.dvrc lines 2318-2328.

    Returns
    -------
    list or None
        Lines containing "C_END" header and body data (1 line total)
    """
    with open(spinaxis_path) as f:
        lines = [line.rstrip('\n') for line in f]

    # Find C_END marker
    c_end_idx = None
    for i, line in enumerate(lines):
        if line.startswith('C_END'):
            c_end_idx = i
            break

    if c_end_idx is None:
        return None

    # Find body after C_END
    for i in range(c_end_idx + 1, len(lines)):
        parts = lines[i].split()
        if parts and parts[0] == body:
            return ['C_END', lines[i]]

    return None


def _parse_planetary_params(params_path: Path, body: str) -> Dict[str, Any]:
    """
    Parse planetary_params3.csv to extract KRC-specific body parameters.

    Per Davinci krc.dvrc lines 2330-2351.

    Returns
    -------
    dict
        Dictionary with GRAV, PTOTAL, ARC2_G0, DUSTA, TAURAT, rotation_period, etc.
    """
    import csv

    with open(params_path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row['Name'] == body:
                # Per Davinci: rot_per is orbit_period (in hours), convert to days
                rot_per_hours = float(row['Orbit_Period'])

                return {
                    'GRAV': float(row['Gravity']),
                    'PTOTAL': float(row['PTOTAL']),
                    'ARC2_G0': float(row['ARC2_PHO']) if row['ARC2_PHO'] != '-999' else 0.5,
                    'DUSTA': float(row['DUSTA']) if row['DUSTA'] != '-999' else 0.9,
                    'TAURAT': float(row['TAURAT']) if row['TAURAT'] != '-999' else 0.22,
                    'rotation_period': rot_per_hours / 24.0,  # Convert hours to days
                    'orbital_period': float(row['Sideral_Period']) * 365.25636,  # Convert years to days
                    'radius': float(row['Radius']),
                }

    return {}


def _get_parent_body(spinaxis_path: Path, body: str) -> Optional[str]:
    """
    Get parent body from spinaxis.tab for satellites.

    Per Davinci krc.dvrc lines 2318-2328.

    Parameters
    ----------
    spinaxis_path : Path
        Path to spinaxis.tab file
    body : str
        Name of satellite body

    Returns
    -------
    str or None
        Parent body name (e.g., "Earth" for Moon, "Mars" for Phobos)
    """
    with open(spinaxis_path) as f:
        lines = [line.rstrip('\n') for line in f]

    # Find C_END marker
    c_end_idx = None
    for i, line in enumerate(lines):
        if line.startswith('C_END'):
            c_end_idx = i
            break

    if c_end_idx is None:
        return None

    # Find body after C_END
    for i in range(c_end_idx + 1, len(lines)):
        parts = lines[i].split('\t')
        if parts and parts[0] == body:
            # Parent body is in column 7 (for satellites)
            if len(parts) > 7:
                return parts[7].strip()
            return None

    return None


def _create_porb_run_file(
    run_path: Path,
    body: str,
    epoch: float,
    output_file: str
) -> None:
    """
    Create PORB .run file with operation parameters.

    Per Davinci krc.dvrc line 2357.

    Parameters
    ----------
    run_path : Path
        Path to write .run file
    body : str
        Body name
    epoch : float
        Epoch fraction (e.g., 0.10 for 2010)
    output_file : str
        Name of output .mat file

    Notes
    -----
    Format per PORB Fortran program:
      Line 1: T (terminal output)
      Line 2: 1 (initiate COMMON from orbital elements)
      Lines 3-4: 1 (sub-options)
      Line 5: epoch
      Line 6: 0
      Line 7: body name
      Line 8: 2 (save COMMON to file)
      Lines 9-10: 2 (binary format sub-options)
      Line 11: output filename
      Line 12: 0 (quit)
    """
    with open(run_path, 'w') as f:
        f.write("T\n")              # Terminal output
        f.write("1\n")              # Initiate COMMON from orbital elements
        f.write("1\n")              # Sub-option
        f.write("1\n")              # Sub-option
        f.write(f"{epoch}\n")       # Epoch
        f.write("0\n")              # Option
        f.write(f"{body}\n")        # Body name
        f.write("2\n")              # Save COMMON to file
        f.write("2\n")              # Binary format
        f.write("2\n")              # Sub-option
        f.write(f"{output_file}\n") # Output filename
        f.write("0\n")              # Quit


def _write_temp_files(
    workdir: Path,
    parent_body_lines: list,
    spin_lines: list
) -> None:
    """
    Write standish.tab and spinaxis.tab to working directory.

    Per Davinci krc.dvrc lines 2355-2356.

    Parameters
    ----------
    workdir : Path
        Temporary working directory
    parent_body_lines : list
        Output from _parse_standish_table (3 lines)
    spin_lines : list
        Output from _parse_spinaxis_table (2 lines)
    """
    # Write standish.tab
    with open(workdir / "standish.tab", 'w') as f:
        for line in parent_body_lines:
            f.write(line + '\n')

    # Write spinaxis.tab
    with open(workdir / "spinaxis.tab", 'w') as f:
        for line in spin_lines:
            f.write(line + '\n')


def _run_porb(
    workdir: Path,
    run_file: str,
    krc_home: Path
) -> Tuple[int, str, str]:
    """
    Execute PORB Fortran program.

    Per Davinci krc.dvrc lines 2746-2751.

    Parameters
    ----------
    workdir : Path
        Working directory containing input files
    run_file : str
        Name of .run file (e.g., "porb_Moon.run")
    krc_home : Path
        Path to KRC home directory (contains porbmn executable)

    Returns
    -------
    returncode : int
        Process return code (0 = success)
    stdout : str
        Standard output from porbmn
    stderr : str
        Standard error from porbmn

    Notes
    -----
    Davinci command:
      cd $workdir; $DV_KRC_HOME/src/porbmn < porb_<name>.run
    """
    import subprocess

    porbmn_exe = krc_home / "porbmn"

    if not porbmn_exe.exists():
        raise FileNotFoundError(
            f"PORB executable not found: {porbmn_exe}\n"
            f"Compile with: cd {krc_home} && make porbmn"
        )

    run_path = workdir / run_file

    with open(run_path) as f:
        result = subprocess.run(
            [str(porbmn_exe)],
            stdin=f,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=str(workdir),
            text=True,
            timeout=60
        )

    return result.returncode, result.stdout, result.stderr


def _parse_porb_output(mat_file: Path) -> Dict[str, Any]:
    """
    Parse PORB .mat output file.

    Per Davinci krc.dvrc line 2755.

    Parameters
    ----------
    mat_file : Path
        Path to <body>.mat file created by porbmn

    Returns
    -------
    dict
        Dictionary with keys:
          - porb_header: Header line
          - porb_text_lines: List of 6 formatted text lines
          - porb_params: List of 30 numeric values

    Notes
    -----
    Format:
      Line 1: PORB header (e.g., "PORB:2014jun10 2024 Jun 27...")
      Lines 2-7: Six lines of formatted numeric data (5 values per line)
        Values in G15.7 format, 30 values total
    """
    with open(mat_file) as f:
        lines = [line.rstrip('\n') for line in f]

    if len(lines) < 7:
        raise ValueError(f"Invalid PORB output: expected 7+ lines, got {len(lines)}")

    # First line is header
    porb_header = lines[0]

    # Next 6 lines are formatted numeric data
    porb_text_lines = lines[1:7]

    # Parse numeric values (5 per line, 30 total)
    porb_params = []
    for line in porb_text_lines:
        values = line.split()
        for val in values:
            try:
                porb_params.append(float(val))
            except ValueError:
                pass  # Skip non-numeric

    if len(porb_params) != 30:
        raise ValueError(f"Expected 30 PORB params, got {len(porb_params)}")

    return {
        'porb_header': porb_header,
        'porb_text_lines': porb_text_lines,
        'porb_params': porb_params
    }


def _calculate_derived_params(
    rot_per_hours: float,
    orbital_period: float
) -> Dict[str, Any]:
    """
    Calculate KRC-specific derived parameters.

    Per Davinci krc.dvrc lines 2764-2771.

    Parameters
    ----------
    rot_per_hours : float
        Rotation period in hours
    orbital_period : float
        Orbital period in days

    Returns
    -------
    dict
        Dictionary with N24, PERIOD, DELJUL

    Notes
    -----
    N24 calculation (line 2764-2765):
      - Calculate: (rot_per_hours * 4) / 96 -> floor -> * 96
      - Minimum value: 96
      - Ensures at least 96 timesteps per day

    PERIOD calculation (line 2768):
      - PERIOD = rot_per_hours / 24
      - Ratio of body rotation to Earth rotation

    DELJUL calculation (line 2771):
      - DELJUL = orbital_period / 360
      - Days per degree of solar longitude
    """
    import math

    # N24: timesteps per body day (minimum 96, multiples of 96)
    n24 = int(math.floor((rot_per_hours * 4) / 96) * 96)
    if n24 < 96:
        n24 = 96

    # PERIOD: body day / Earth day
    period = rot_per_hours / 24.0

    # DELJUL: days per degree Ls
    deljul = orbital_period / 360.0

    return {
        'N24': n24,
        'PERIOD': period,
        'DELJUL': deljul
    }


def _parse_smallbodies_hdf(hdf_path: Path, body: str) -> Optional[Dict[str, Any]]:
    """
    Parse small_bodies.hdf to extract orbital elements for an asteroid.

    Parameters
    ----------
    hdf_path : Path
        Path to small_bodies.hdf file
    body : str
        Body name (e.g., "Bennu", "Ceres")

    Returns
    -------
    dict or None
        Dictionary with orbital elements (a, e, i, period, rot_per, etc.)
        Returns None if body not found
    """
    with h5py.File(hdf_path, 'r') as f:
        # Parse names to find index
        names_bytes = f['name'][0]
        names_str = names_bytes.decode('utf-8')
        names = [n.strip() for n in names_str.split('\n') if n.strip()]

        if body not in names:
            return None

        idx = names.index(body)

        # Extract orbital elements at this index
        # Note: HDF structure is (1, N, 1) so we use [0, idx, 0]
        return {
            'a': float(f['a'][0, idx, 0]),              # Semi-major axis (AU)
            'e': float(f['e'][0, idx, 0]),              # Eccentricity
            'i': float(f['i'][0, idx, 0]),              # Inclination (degrees)
            'm': float(f['m'][0, idx, 0]),              # Mean anomaly (degrees)
            'node': float(f['node'][0, idx, 0]),        # Longitude of ascending node (degrees)
            'peri': float(f['peri'][0, idx, 0]),        # Longitude of perihelion (degrees)
            'period': float(f['period'][0, idx, 0]),    # Orbital period (days)
            'poledec': float(f['poledec'][0, idx, 0]),  # Pole declination (degrees)
            'polera': float(f['polera'][0, idx, 0]),    # Pole right ascension (degrees)
            'rot_per': float(f['rot_per'][0, idx, 0]),  # Rotation period (hours)
            'merid': float(f['merid'][0, idx, 0]),      # Prime meridian (degrees)
        }


def _parse_comets_hdf(hdf_path: Path, body: str) -> Optional[Dict[str, Any]]:
    """
    Parse comets.hdf to extract orbital elements for a comet.

    Parameters
    ----------
    hdf_path : Path
        Path to comets.hdf file
    body : str
        Body name (e.g., "1P-Halley", "67P-Churyumov-Gerasimenko")

    Returns
    -------
    dict or None
        Dictionary with orbital elements (q, e, i, period, rot_per, etc.)
        Returns None if body not found

    Notes
    -----
    Comets.hdf uses perihelion distance (q) instead of semi-major axis (a).
    Semi-major axis can be calculated from q and e: a = q / (1 - e)
    """
    with h5py.File(hdf_path, 'r') as f:
        # Parse names to find index
        names_bytes = f['name'][0]
        names_str = names_bytes.decode('utf-8')
        names = [n.strip() for n in names_str.split('\n') if n.strip()]

        if body not in names:
            return None

        idx = names.index(body)

        # Extract orbital elements at this index
        # Note: HDF structure is (1, N, 1) so we use [0, idx, 0]
        q = float(f['q'][0, idx, 0])         # Perihelion distance (AU)
        e = float(f['e'][0, idx, 0])         # Eccentricity

        # Calculate semi-major axis from perihelion distance and eccentricity
        # For elliptical orbits: a = q / (1 - e)
        if e < 1.0:  # Elliptical orbit
            a = q / (1.0 - e)
        else:  # Parabolic or hyperbolic orbit
            a = q  # Use perihelion distance as approximate

        return {
            'a': a,                                      # Semi-major axis (AU, calculated)
            'e': e,                                      # Eccentricity
            'i': float(f['i'][0, idx, 0]),              # Inclination (degrees)
            'm': 0.0,                                    # Mean anomaly (not in comets.hdf, use 0)
            'node': float(f['node'][0, idx, 0]),        # Longitude of ascending node (degrees)
            'peri': float(f['peri'][0, idx, 0]),        # Argument of perihelion (degrees)
            'period': float(f['period'][0, idx, 0]),    # Orbital period (days)
            'poledec': float(f['poledec'][0, idx, 0]),  # Pole declination (degrees)
            'polera': float(f['polera'][0, idx, 0]),    # Pole right ascension (degrees)
            'rot_per': float(f['rot_per'][0, idx, 0]),  # Rotation period (hours)
            'merid': 0.0,                                # Prime meridian (not in comets.hdf, use 0)
        }


def porb(
    body: str | Dict[str, Any],
    epoch: float = 0.10,
    force: bool = False,
    data_loader=None
) -> OrbitalElements:
    """
    Calculate orbital parameters and rotation matrix for a body.

    Implements Davinci krc.dvrc porb() function (lines 2214-2750).
    For Planet/Satellite bodies:
    1. Check for pre-computed .porb.hdf file (unless force=True)
    2. If not found or force=True, generate PORB data dynamically:
       - Load orbital elements from standish.tab
       - Load rotation parameters from spinaxis.tab
       - Load physical/atmospheric parameters from planetary_params3.csv
       - Call PORB Fortran program to compute rotation matrices
       - Parse output and return OrbitalElements

    Parameters
    ----------
    body : str or dict
        Body name or structure with orbital parameters
    epoch : float, optional
        Fraction of century for start date (0.10 = 2010)
    force : bool, optional
        Force recalculation even if cached (bodyforce in Davinci)
    data_loader : KRCDataLoader, optional
        Data loader instance

    Returns
    -------
    OrbitalElements
        Orbital elements and rotation parameters

    Notes
    -----
    Per Davinci krc.dvrc lines 2214-2750.
    """
    if isinstance(body, dict):
        # Generic body structure provided
        body_copy = body.copy()
        name = body_copy.pop("name", "Generic")
        return OrbitalElements(
            name=name,
            body_type="Generic",
            **body_copy
        )

    # Initialize data loader if not provided
    if data_loader is None:
        data_loader = KRCDataLoader()

    # Identify body type
    body_type = identify_body_type(body)

    # Try to load from HDF (unless force=True)
    if not force:
        try:
            params = load_body_parameters(body, data_loader)

            # Extract PORB data (these don't go into OrbitalElements)
            porb_header = params.pop('PORB_HEADER', None)
            porb_text_lines = params.pop('PORB_TEXT_LINES', None)
            porb_params = params.pop('PORB_PARAMS', None)

            # Extract KRC-specific parameters
            krc_params = {}
            krc_keys = [
                'ARC2_G0', 'DELJUL', 'DUSTA', 'GRAV', 'N24', 'PERIOD', 'PTOTAL',
                'TAUD', 'TAURAT', 'TFROST', 'ALBEDO', 'EMISS', 'INERTIA',
                'N1', 'N2', 'N3', 'N5', 'TDEEP', 'TBOT'
            ]
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
                orbital_elem.porb_text_lines = porb_text_lines
            if porb_params:
                orbital_elem.porb_params = porb_params
            if krc_params:
                orbital_elem.krc_params = krc_params

            return orbital_elem

        except (ValueError, FileNotFoundError):
            # HDF file not found, fall through to dynamic generation
            print(f"No cached PORB file for {body}, generating dynamically...")

    else:
        print(f"Forcing PORB generation for {body}")

    # Generate PORB data dynamically
    # Per Davinci krc.dvrc lines 2265-2780

    import tempfile
    import os
    import math

    # Handle asteroids and comets separately from planets/satellites
    if body_type in ["Asteroid", "Unknown"]:
        # Try to load from small_bodies.hdf first
        smallbodies_path = data_loader.support_dir / "small_bodies.hdf"
        if not smallbodies_path.exists():
            raise FileNotFoundError(f"small_bodies.hdf not found: {smallbodies_path}")

        hdf_data = _parse_smallbodies_hdf(smallbodies_path, body)

        # If not found in small_bodies.hdf, try comets.hdf (for "Unknown" types)
        if hdf_data is None and body_type == "Unknown":
            comets_path = data_loader.support_dir / "comets.hdf"
            if comets_path.exists():
                hdf_data = _parse_comets_hdf(comets_path, body)
                if hdf_data is not None:
                    body_type = "Comet"  # Update body type

        if hdf_data is None:
            raise ValueError(f"Body '{body}' not found in small_bodies.hdf or comets.hdf")

        # Calculate derived parameters
        rot_per_days = hdf_data['rot_per'] / 24.0  # Convert hours to days
        derived = _calculate_derived_params(hdf_data['rot_per'], hdf_data['period'])

        # Create OrbitalElements with minimal KRC params
        # Asteroids/Comets don't have full PORB matrices, so we provide basic orbital elements
        orbital_elem = OrbitalElements(
            name=body,
            body_type=body_type,  # Will be "Asteroid" or "Comet"
            semi_major_axis=hdf_data['a'],
            eccentricity=hdf_data['e'],
            inclination=hdf_data['i'],
            node=hdf_data['node'],
            perihelion=hdf_data['peri'],
            mean_anomaly=hdf_data['m'],
            rotation_period=rot_per_days,
            pole_ra=hdf_data['polera'],
            pole_dec=hdf_data['poledec'],
            meridian=hdf_data['merid'],
            orbital_period=hdf_data['period'],
        )

        # Attach minimal KRC params
        orbital_elem.krc_params = {
            'N24': derived['N24'],
            'PERIOD': derived['PERIOD'],
            'DELJUL': derived['DELJUL'],
        }

        return orbital_elem

    elif body_type == "Comet":
        # Try to load from comets.hdf
        comets_path = data_loader.support_dir / "comets.hdf"
        if not comets_path.exists():
            raise FileNotFoundError(f"comets.hdf not found: {comets_path}")

        hdf_data = _parse_comets_hdf(comets_path, body)
        if hdf_data is None:
            raise ValueError(f"Comet '{body}' not found in comets.hdf")

        # Calculate derived parameters
        rot_per_days = hdf_data['rot_per'] / 24.0  # Convert hours to days
        derived = _calculate_derived_params(hdf_data['rot_per'], hdf_data['period'])

        # Create OrbitalElements with minimal KRC params
        orbital_elem = OrbitalElements(
            name=body,
            body_type="Comet",
            semi_major_axis=hdf_data['a'],
            eccentricity=hdf_data['e'],
            inclination=hdf_data['i'],
            node=hdf_data['node'],
            perihelion=hdf_data['peri'],
            mean_anomaly=hdf_data['m'],
            rotation_period=rot_per_days,
            pole_ra=hdf_data['polera'],
            pole_dec=hdf_data['poledec'],
            meridian=hdf_data['merid'],
            orbital_period=hdf_data['period'],
        )

        # Attach minimal KRC params
        orbital_elem.krc_params = {
            'N24': derived['N24'],
            'PERIOD': derived['PERIOD'],
            'DELJUL': derived['DELJUL'],
        }

        return orbital_elem

    elif body_type not in ["Planet", "Satellite"]:
        raise ValueError(f"Dynamic PORB generation not supported for body type: {body_type}")

    # For Planet/Satellite: Use Fortran PORB program
    # Get paths to support files
    standish_path = data_loader.support_dir / "standish.tab"
    spinaxis_path = data_loader.support_dir / "spinaxis.tab"
    params_path = data_loader.support_dir / "planetary_params3.csv"

    # Determine parent body (for satellites, use parent planet; for planets, use self)
    if body_type == "Satellite":
        parent_body = _get_parent_body(spinaxis_path, body)
        if parent_body is None:
            raise ValueError(f"Could not find parent body for satellite {body} in spinaxis.tab")
    else:
        parent_body = body

    # Parse tables
    parent_lines = _parse_standish_table(standish_path, parent_body)
    spin_lines = _parse_spinaxis_table(spinaxis_path, body)
    body_params = _parse_planetary_params(params_path, body)

    if parent_lines is None:
        raise ValueError(f"Parent body {parent_body} not found in standish.tab")
    if spin_lines is None:
        raise ValueError(f"Body {body} not found in spinaxis.tab")
    if not body_params:
        raise ValueError(f"Body {body} not found in planetary_params3.csv")

    # Create temporary working directory
    with tempfile.TemporaryDirectory(prefix="porb_") as workdir:
        workdir = Path(workdir)

        # Write temp files
        _write_temp_files(workdir, parent_lines, spin_lines)
        _create_porb_run_file(
            workdir / f"porb_{body}.run",
            body,
            epoch,
            f"{body}.mat"
        )

        # Run PORB Fortran program
        krc_home = Path(os.environ.get('KRC_HOME', Path(__file__).parent.parent.parent))
        returncode, stdout, stderr = _run_porb(
            workdir,
            f"porb_{body}.run",
            krc_home
        )

        if returncode != 0:
            raise RuntimeError(f"PORB failed with return code {returncode}:\n{stderr}")

        # Parse output
        mat_file = workdir / f"{body}.mat"
        porb_data = _parse_porb_output(mat_file)

        # Calculate derived params
        derived = _calculate_derived_params(
            body_params['rotation_period'] * 24,  # Convert days to hours
            body_params['orbital_period']
        )

        # Combine all KRC params
        krc_params = {
            'GRAV': body_params['GRAV'],
            'PTOTAL': body_params['PTOTAL'],
            'ARC2_G0': body_params['ARC2_G0'],
            'DUSTA': body_params['DUSTA'],
            'TAURAT': body_params['TAURAT'],
            'N24': derived['N24'],
            'PERIOD': derived['PERIOD'],
            'DELJUL': derived['DELJUL'],
        }

        # Create OrbitalElements
        orbital_elem = OrbitalElements(
            name=body,
            body_type=body_type,
            rotation_period=body_params['rotation_period'],
            orbital_period=body_params['orbital_period'],
            radius=body_params['radius'],
        )

        # Attach PORB data
        orbital_elem.porb_header = porb_data['porb_header']
        orbital_elem.porb_text_lines = porb_data['porb_text_lines']
        orbital_elem.porb_params = porb_data['porb_params']
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


def gd_to_jd(gd_string: str, body: str = "Mars", data_loader: Optional[KRCDataLoader] = None) -> float:
    """
    Convert Gregorian date string to Julian date using body-specific lookup table.

    Per Davinci krc.dvrc GD2JD() function (lines 3265-3288).
    Uses pre-computed lookup tables from porb_defaults/<Body>_Ls_Date.ascii.

    Parameters
    ----------
    gd_string : str
        Gregorian date string in format "YYYY-Mmm-DD"
        (e.g., "1994-Feb-05", "2000-Jan-01")
        Mmm must be 3-letter month: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    body : str, optional
        Celestial body name (default: "Mars")
    data_loader : KRCDataLoader, optional
        Data loader for accessing lookup files

    Returns
    -------
    float
        Julian date corresponding to the Gregorian date

    Raises
    ------
    ValueError
        If date not found in lookup table or invalid format
    FileNotFoundError
        If lookup table file not found for body

    Notes
    -----
    Davinci command:
      JD = GD2JD("1994-Feb-05")  # Returns 2449388.5

    Lookup table format (Mars_Ls_Date.ascii):
      YYYY-Mmm-DD JD Ls
      1994-Feb-05 2449388.5 214.3334

    Valid date range: 1990-Jan-01 to 2040-Jan-01 (depending on body)

    Examples
    --------
    >>> gd_to_jd("1994-Feb-05", "Mars")
    2449388.5
    >>> gd_to_jd("2000-Feb-01", "Mars")
    2451575.5
    """
    # Initialize data loader if not provided
    if data_loader is None:
        data_loader = KRCDataLoader()

    # Construct path to lookup table
    lookup_file = data_loader.support_dir / "porb_defaults" / f"{body}_Ls_Date.ascii"

    if not lookup_file.exists():
        raise FileNotFoundError(
            f"Date lookup table not found for {body}: {lookup_file}\n"
            f"Expected format: <body>_Ls_Date.ascii in porb_defaults/"
        )

    # Read lookup table
    with open(lookup_file) as f:
        lines = f.readlines()

    # Search for matching date
    # Per Davinci krc.dvrc line 3279: grep(IN, DATE[,1,1]+"")
    # This searches for the date string in the file
    for line in lines:
        parts = line.strip().split()
        if len(parts) >= 2 and parts[0] == gd_string:
            try:
                jd = float(parts[1])

                # TODO: DAVINCI BUG - GD2JD() truncates to integer, dropping the .5
                # The lookup table contains JD values like 2449388.5 (noon on that day)
                # but Davinci's GD2JD() returns 2449388 (integer, missing the .5)
                # This causes a 0.5-day error in DJUL calculations.
                #
                # For Davinci parity, we match this buggy behavior by truncating.
                # This should be reported to Davinci/KRC developers.
                #
                # Evidence:
                #   Mars_Ls_Date.ascii: "1994-Feb-05 2449388.5 214.3334"
                #   Davinci GD2JD("1994-Feb-05") returns: 2449388
                #   Correct value should be: 2449388.5
                jd = int(jd)  # Truncate to match Davinci bug

                return jd
            except (ValueError, IndexError):
                raise ValueError(
                    f"Invalid Julian date in lookup table for {gd_string}: {line.strip()}"
                )

    # Date not found
    raise ValueError(
        f"Gregorian date '{gd_string}' not found in lookup table for {body}\n"
        f"Valid range: check {lookup_file}\n"
        f"Format must be: YYYY-Mmm-DD (e.g., '1994-Feb-05')\n"
        f"Months: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec"
    )


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
    IIB: Optional[Union[int, float]] = None,
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
    rot_per = body_params.rotation_period  # in days

    # Calculate N24 from rotation period (Davinci krc.dvrc lines 2764-2765)
    # N24 = number of output timesteps per day
    # Minimum value is 96 (every 15 minutes)
    # Per Davinci line 2764: uses rot_per in HOURS, not days
    rot_per_hours = rot_per * 24.0  # Convert days to hours
    n24_from_porb = int(np.floor((rot_per_hours * 4) / 96) * 96)
    if n24_from_porb < 96:
        n24_from_porb = 96

    # Calculate N5 and JDISK if not provided by user
    if N5 is None:
        total_years = spinup_years + output_years
        N5 = int(np.ceil(360.0 / DELLS * total_years))

    if JDISK is None:
        JDISK = int(np.ceil(360.0 / DELLS * spinup_years + 1))

    # Get DELJUL (time step in Julian days for each Ls degree)
    # Per Davinci krc.dvrc lines 348-352, when user sets DELLS, always recalculate
    # DELJUL from DELLS, don't use hardcoded PORB value
    if hasattr(body_params, 'orbital_period'):
        # Calculate from orbital period (not rotation period!)
        # This matches Davinci formula: DELJUL = PERIOD/360 * DELLS (line 822)
        DELJUL_calc = body_params.orbital_period * DELLS / 360.0
    elif hasattr(body_params, 'krc_params') and 'DELJUL' in body_params.krc_params:
        # Fallback: Use pre-calculated value from PORB if no orbital period
        # Note: This is only correct if DELLS=1.0!
        DELJUL_calc = body_params.krc_params['DELJUL']
        if verbose and DELLS != 1.0:
            print(f"Warning: Using PORB DELJUL={DELJUL_calc:.4f} with DELLS={DELLS} (may be inconsistent)")
    else:
        # Last resort: estimate from rotation period (will be wrong for planets!)
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
        # Note: TFROST is NOT set by PORB - it's body-specific (0 for Europa/generic, 146 default otherwise)
        'PhotoFunc': (PhotoFunc, 0.0),
        'FLAY': (FLAY, 0.10),
        'RLAY': (RLAY, 1.15),
        'IIB': (IIB, -1),
        'IC2': (IC2, 999),
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

    # Body-specific PORB defaults: KPREF and TFROST
    # Per Davinci krc.dvrc lines 367-369 (KPREF), 546/582 (TFROST)
    # Note: TFROST is NOT in PORB HDF files but has body-specific defaults
    # Davinci ALWAYS writes TFROST changecard (forced in executor.py like N4)
    if body == "Mars":
        # Mars-specific
        if KPREF is None:
            porb_params['KPREF'] = 1
            porb_touched.add('KPREF')
        else:
            porb_params['KPREF'] = KPREF

        if TFROST is None:
            porb_params['TFROST'] = 146.0  # CO2 frost point for Mars
        else:
            porb_params['TFROST'] = TFROST
    else:
        # All other bodies (Europa, Moon, Phobos, generic, etc.)
        if TFROST is None:
            porb_params['TFROST'] = 0.0  # No frost for airless/non-Mars bodies
        else:
            porb_params['TFROST'] = TFROST

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
