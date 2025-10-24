"""Two-layer regolith configuration and calculations.

Supports four modes for the thick parameter:
1. thick = 0: Uniform material properties
2. thick > 0: Two-layer regolith with sharp transition
3. thick < 0: Exponential density profile (H-parameter)
4. thick = 2D array: Zone table with layer-by-layer properties
"""

from typing import Optional, Dict, Any, Union, List
import numpy as np


def calculate_IC2(
    thick: float,
    N1: int,
    FLAY: float = 2.0,
    RLAY: float = 1.08
) -> int:
    """
    Calculate layer transition index IC2 from upper layer thickness.

    Matches krc.dvrc logic (lines 1240-1260).

    Parameters
    ----------
    thick : float
        Upper layer thickness in meters:
        - thick > 0: Two-layer regolith (upper/lower)
        - thick = 0: Uniform material
        - thick < 0: Exponential profile (not yet implemented)
    N1 : int
        Total number of subsurface layers
    FLAY : float, optional
        Layer spacing factor (default 2.0)
    RLAY : float, optional
        Minimum layer ratio (default 1.08)

    Returns
    -------
    int
        Layer index where transition occurs (1-indexed).
        Returns 999 for uniform material (thick=0).

    Notes
    -----
    Layer depths are calculated as:
    z[i] = z[i-1] * max(FLAY * (1 - (i-1)/N1), RLAY)

    Starting from z[0] = 0.001 m (1 mm top layer).

    Examples
    --------
    >>> calculate_IC2(0.0, 50)  # Uniform
    999
    >>> calculate_IC2(0.05, 50, FLAY=2.0)  # 5 cm upper layer
    15
    """
    # Uniform material
    if thick == 0.0:
        return 999

    # Exponential profile - IC2=999 per Davinci krc.dvrc line 1964
    if thick < 0.0:
        return 999

    # Two-layer: find layer index where cumulative depth exceeds thick
    z = [0.001]  # 1 mm top layer
    cumulative_depth = 0.001

    for i in range(1, N1):
        # Layer spacing formula from KRC
        ratio = max(FLAY * (1.0 - float(i - 1) / N1), RLAY)
        z_new = z[-1] * ratio
        z.append(z_new)
        cumulative_depth += z_new

        # Check if cumulative depth exceeds thick
        if cumulative_depth >= thick:
            return i + 1  # 1-indexed for Fortran (layer where transition occurs)

    # If thick exceeds total model depth, no transition within domain
    # Return N1 to indicate transition is at or below bottom of model
    return N1


def validate_two_layer_config(
    thick: float,
    INERTIA: float,
    INERTIA2: Optional[float],
    Mat1: str,
    Mat2: str,
    Por1: Optional[float],
    Por2: Optional[float]
) -> None:
    """
    Validate two-layer regolith configuration.

    Parameters
    ----------
    thick : float
        Upper layer thickness (m)
    INERTIA : float
        Upper layer thermal inertia
    INERTIA2 : float or None
        Lower layer thermal inertia
    Mat1 : str
        Upper layer material
    Mat2 : str
        Lower layer material
    Por1 : float or None
        Upper layer porosity
    Por2 : float or None
        Lower layer porosity

    Raises
    ------
    ValueError
        If two-layer configuration is invalid
    """
    if thick == 0.0:
        # Uniform material - no validation needed
        return

    if thick < 0.0:
        # Exponential profile requires INERTIA2 to define deep properties
        if INERTIA2 is None:
            raise ValueError(
                f"Exponential profile (thick={thick}) requires INERTIA2 "
                "to define deep layer properties"
            )
        # No additional validation needed - properties vary continuously
        return

    # Two-layer requires either different INERTIA or different material/porosity
    layers_differ = False

    if INERTIA2 is not None and INERTIA2 != INERTIA:
        layers_differ = True

    if Mat1 != Mat2:
        layers_differ = True

    if Por1 is not None and Por2 is not None and Por1 != Por2:
        layers_differ = True

    if not layers_differ:
        raise ValueError(
            f"Two-layer regolith (thick={thick}) requires different properties "
            "for upper and lower layers. Specify either:\n"
            "  - Different INERTIA2, or\n"
            "  - Different Mat2, or\n"
            "  - Different Por2"
        )


def calculate_exponential_profile(
    H: float,
    N1: int,
    FLAY: float,
    RLAY: float,
    DSD_m: float,
    INERTIA: float,
    INERTIA2: float,
    DENSITY: float,
    DENSITY2: float,
    SPEC_HEAT: float,
    SPEC_HEAT2: float,
    LKofT: bool = False
) -> Dict[str, Any]:
    """
    Calculate exponential density profile for thick < 0.

    Per Davinci krc.dvrc lines 1963-2035, creates exponentially-varying
    material properties following Hayne et al. formulation.

    Parameters
    ----------
    H : float
        E-folding scale in meters (abs(thick))
    N1 : int
        Number of subsurface layers
    FLAY : float
        Layer spacing factor
    RLAY : float
        Minimum layer ratio
    DSD_m : float
        Diurnal skin depth in meters
    INERTIA : float
        Surface thermal inertia
    INERTIA2 : float
        Deep thermal inertia
    DENSITY : float
        Surface density (kg/m³)
    DENSITY2 : float
        Deep density (kg/m³)
    SPEC_HEAT : float
        Surface specific heat (J/kg-K)
    SPEC_HEAT2 : float
        Deep specific heat (J/kg-K)
    LKofT : bool
        Temperature-dependent properties flag

    Returns
    -------
    Dict[str, Any]
        Dictionary with:
        - mode: 'exponential'
        - IC2: 999
        - LZONE: True
        - zone_data: Array of [thickness, density, conductivity, specific_heat]

    Notes
    -----
    Formula: PROP = PROP2 - (PROP2 - PROP1) * exp(-Z/H)
    where Z is depth (positive downward) and H is e-folding scale.
    """
    # Calculate conductivities from thermal inertias
    COND1 = INERTIA**2 / (DENSITY * SPEC_HEAT)
    COND2 = INERTIA2**2 / (DENSITY2 * SPEC_HEAT2)

    # Build layer mesh (per Davinci lines 1971-1998)
    thickness_m = []
    depth_center_m = []

    # Start with FLAY2 = 0.15 per Davinci line 1966
    FLAY2 = 0.15

    # Virtual layer (included in zone file for exponential profiles)
    virtual_thickness = FLAY2 * DSD_m / RLAY
    virtual_depth = -0.5 * virtual_thickness  # Center of virtual layer

    # Add virtual layer first
    thickness_m.append(virtual_thickness)
    depth_center_m.append(virtual_depth)

    # Build remaining layers
    current_thickness = virtual_thickness
    current_depth = virtual_depth

    for i in range(1, N1):  # Start from 1 to include all N1 layers
        # Update thickness for next layer
        current_thickness *= RLAY

        # Calculate center depth
        if i == 1:
            # First real layer after virtual
            current_depth = 0.5 * current_thickness
        else:
            # Subsequent layers
            current_depth += 0.5 * thickness_m[-1] + 0.5 * current_thickness

        thickness_m.append(current_thickness)
        depth_center_m.append(current_depth)  # Positive depth

    # Calculate exponential properties at each layer center
    # Per Davinci lines 2020-2022
    zone_data = []
    # Write N1-1 layers to zone file (excluding virtual layer)
    # Per Davinci line 2023: col_dim = dim(Col_1)[2]-1
    num_layers_to_write = min(len(thickness_m), N1 - 1)
    for i in range(num_layers_to_write):
        # For virtual layer (i==0), use surface (Z=0)
        # Per Davinci krc.dvrc line 1980, Z_T[1] = 0 for surface
        if i == 0:
            # Virtual layer uses surface properties (Z=0)
            Z = 0.0
        else:
            Z = depth_center_m[i]

        # Exponential interpolation between surface and deep values
        density = DENSITY2 - (DENSITY2 - DENSITY) * np.exp(-Z / H)
        conductivity = COND2 - (COND2 - COND1) * np.exp(-Z / H)
        specific_heat = SPEC_HEAT2 - (SPEC_HEAT2 - SPEC_HEAT) * np.exp(-Z / H)

        # Handle LKofT special case - per Davinci lines 2029-2034
        if LKofT and i < 2:
            # Top 2 layers use temperature-dependent properties
            conductivity = -1.0
            specific_heat = 3.0 if i == 0 else 4.0

        zone_data.append([thickness_m[i], density, conductivity, specific_heat])

    return {
        'mode': 'exponential',
        'IC2': 999,
        'LZONE': True,
        'zone_data': zone_data,
        'H': H
    }


def process_zone_table(thick_array: Union[List, np.ndarray]) -> Dict[str, Any]:
    """
    Process a zone table (thick as 2D array) for multi-layer configuration.

    Per Davinci krc.dvrc lines 2089-2109.

    Parameters
    ----------
    thick_array : array-like
        2D array of shape (N, 4) or (4, N) where N >= 3
        Columns: [thickness, density, conductivity, specific_heat]

    Returns
    -------
    Dict[str, Any]
        Dictionary with:
        - mode: 'table'
        - IC2: 999
        - LZONE: True
        - N1: 50 (default per Davinci line 2098)
        - zone_data: Formatted array for zone file

    Raises
    ------
    ValueError
        If array dimensions are incorrect
    """
    # Convert to numpy array
    thick_array = np.array(thick_array)

    # Handle both (N, 4) and (4, N) shapes
    if thick_array.shape[1] == 4:
        # Shape is (N, 4) - rows are layers
        zone_data = thick_array
    elif thick_array.shape[0] == 4:
        # Shape is (4, N) - transpose to get (N, 4)
        zone_data = thick_array.T
    else:
        raise ValueError(
            f"Zone table must have shape (N, 4) or (4, N), got {thick_array.shape}"
        )

    # Validate minimum layers
    if len(zone_data) < 3:
        raise ValueError(
            f"Zone table requires at least 3 layers, got {len(zone_data)}"
        )

    return {
        'mode': 'table',
        'IC2': 999,
        'LZONE': True,
        'N1': 50,  # Default per Davinci line 2098
        'zone_data': zone_data.tolist()
    }


def read_zone_file(zonefile: str) -> Dict[str, Any]:
    """
    Read and validate an external zone file for Mode 4 (multi-layer table).

    Per Davinci krc.dvrc, zone files have 4 columns:
    - Col 1: Layer thickness (m)
    - Col 2: Density (kg/m³)
    - Col 3: Conductivity (W/m-K) OR -1 for LKofT zones
    - Col 4: Specific heat (J/kg-K) OR zone index for LKofT

    Parameters
    ----------
    zonefile : str
        Path to zone file to read

    Returns
    -------
    Dict[str, Any]
        Dictionary with:
        - mode: 'external'
        - IC2: 999 (always for zone tables)
        - LZONE: True
        - N1: Number of layers from file
        - zone_data: Array of [thickness, density, conductivity, specific_heat]
        - zonefile: Original path to zone file

    Raises
    ------
    FileNotFoundError
        If zone file doesn't exist
    ValueError
        If zone file format is invalid
    """
    from pathlib import Path

    zonefile_path = Path(zonefile)

    # Check file exists
    if not zonefile_path.exists():
        raise FileNotFoundError(
            f"Zone file not found: {zonefile}\n"
            "Zone file must exist and be readable."
        )

    # Read and parse zone file
    zone_data = []
    with open(zonefile_path, 'r') as f:
        # Skip header line (C_END or similar)
        first_line = f.readline().strip()
        if not first_line.startswith('C'):
            # No header, rewind
            f.seek(0)

        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            parts = line.split()
            if len(parts) < 4:
                continue

            # Parse values
            try:
                thick = float(parts[0])
                density = float(parts[1])
                conductivity = float(parts[2])
                specific_heat = float(parts[3])
            except ValueError as e:
                raise ValueError(
                    f"Invalid zone file format at line: {line}\n"
                    f"Expected 4 numeric values, got: {parts}\n"
                    f"Error: {e}"
                )

            # Check for end marker (0 0 0 0)
            if thick == 0 and density == 0 and conductivity == 0 and specific_heat == 0:
                break

            zone_data.append([thick, density, conductivity, specific_heat])

    # Validate we got data
    if len(zone_data) == 0:
        raise ValueError(
            f"Zone file contains no valid layers: {zonefile}\n"
            "Zone file must contain at least one layer."
        )

    # Validate physical properties
    for i, layer in enumerate(zone_data):
        thick, density, cond, cp = layer

        if thick <= 0:
            raise ValueError(
                f"Layer {i+1}: thickness must be > 0, got {thick}"
            )
        if density <= 0:
            raise ValueError(
                f"Layer {i+1}: density must be > 0, got {density}"
            )
        # Conductivity can be -1 for LKofT mode, otherwise must be positive
        if cond != -1.0 and cond <= 0:
            raise ValueError(
                f"Layer {i+1}: conductivity must be > 0 or -1 (for LKofT), got {cond}"
            )
        # Specific heat can be a zone index (small integer) for LKofT, otherwise must be positive
        if cond != -1.0 and cp <= 0:
            raise ValueError(
                f"Layer {i+1}: specific heat must be > 0, got {cp}"
            )

    # Calculate N1 from zone file
    # Per Davinci krc.dvrc line 2023: Zone files have N1-1 layers (no virtual layer)
    # So N1 should equal the number of layers in the zone file
    N1 = len(zone_data)

    return {
        'mode': 'external',
        'IC2': 999,  # Always 999 for zone tables
        'LZONE': True,
        'N1': N1,
        'zone_data': zone_data,
        'zonefile': str(zonefile_path.absolute())
    }


def get_layer_info(
    thick: float,
    IC2: int,
    N1: int
) -> Dict[str, Any]:
    """
    Get information about layer configuration.

    Parameters
    ----------
    thick : float
        Upper layer thickness (m)
    IC2 : int
        Layer transition index
    N1 : int
        Total number of layers

    Returns
    -------
    dict
        Dictionary with layer configuration info:
        - layer_type: "uniform", "two-layer", or "exponential"
        - upper_layers: Number of layers in upper region
        - lower_layers: Number of layers in lower region
        - transition_depth: Depth of transition (m)
    """
    if thick == 0.0:
        return {
            "layer_type": "uniform",
            "upper_layers": 0,
            "lower_layers": N1,
            "transition_depth": None
        }

    if thick < 0.0:
        return {
            "layer_type": "exponential",
            "upper_layers": None,
            "lower_layers": None,
            "transition_depth": abs(thick)
        }

    # Two-layer
    upper_layers = IC2 - 1 if IC2 < 999 else N1
    lower_layers = N1 - upper_layers if IC2 < 999 else 0

    return {
        "layer_type": "two-layer",
        "upper_layers": upper_layers,
        "lower_layers": lower_layers,
        "transition_depth": thick
    }
