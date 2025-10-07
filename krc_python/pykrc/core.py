"""Main KRC interface function."""

from pathlib import Path
from typing import Dict, Any, Optional, Union
import tempfile

from pykrc.config import get_krc_home, get_paths
from pykrc.input_processor import parse_master_inp
from pykrc.data_loaders import KRCDataLoader
from pykrc.materials import calculate_thermal_properties
from pykrc.orbital import porb, OrbitalElements
from pykrc.executor import KRCExecutor
from pykrc.bin52_complete import parse_bin52


def krc(
    # Required parameters
    lat: Optional[float] = None,
    lon: Optional[float] = None,

    # Body and time
    body: str = "Mars",
    ls: Optional[float] = None,
    hour: Optional[float] = None,

    # Material properties
    INERTIA: Optional[float] = None,
    ALBEDO: Optional[float] = None,
    EMISS: float = 1.0,
    Mat1: str = "basalt",
    Mat2: str = "basalt",

    # Layer properties
    Por1: Optional[float] = None,
    Por2: Optional[float] = None,
    INERTIA2: Optional[float] = None,

    # Atmospheric parameters
    TAUD: Optional[float] = None,
    PTOTAL: Optional[float] = None,
    TATM: Optional[float] = None,

    # Model parameters
    TDEEP: float = 180.0,
    SLOPE: float = 0.0,
    SLOAZI: float = 90.0,

    # Conductivity model
    k_style: str = "Mars",
    T_user: float = 220.0,

    # Elevation
    ELEV: Optional[float] = None,

    # Execution options
    verbose: bool = False,
    workdir: Optional[str] = None,
    keep_files: bool = False,

    # Advanced options
    **kwargs
) -> Dict[str, Any]:
    """
    Run KRC thermal model.

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to 90)
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
    ELEV : float, optional
        Surface elevation (km)
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

    # Set defaults based on body
    if INERTIA is None:
        INERTIA = master_params.get("INERTIA", 200.0)

    if ALBEDO is None:
        ALBEDO = master_params.get("ALBEDO", 0.25)

    if INERTIA2 is None:
        INERTIA2 = INERTIA

    # Calculate material properties
    if verbose:
        print(f"Calculating material properties for {Mat1}...")

    upper_props = calculate_thermal_properties(
        Mat1, INERTIA, T_user, k_style
    )

    lower_props = calculate_thermal_properties(
        Mat2, INERTIA2, T_user, k_style
    )

    # Build full parameter set
    params = master_params.copy()

    # Override with user parameters
    params.update({
        "ALBEDO": ALBEDO,
        "EMISS": EMISS,
        "INERTIA": INERTIA,
        "TDEEP": TDEEP,
        "SLOPE": SLOPE,
        "SLOAZI": SLOAZI,

        # From material calculations
        "SPEC_HEAT": upper_props["SPEC_HEAT"],
        "DENSITY": upper_props["DENSITY"],
        "COND2": lower_props["COND"],
        "DENS2": lower_props["DENSITY"],
        "SpHeat2": lower_props["SPEC_HEAT"],

        "ConUp0": upper_props["ConUp0"],
        "ConUp1": upper_props["ConUp1"],
        "ConUp2": upper_props["ConUp2"],
        "ConUp3": upper_props["ConUp3"],

        "ConLo0": lower_props["ConUp0"],
        "ConLo1": lower_props["ConUp1"],
        "ConLo2": lower_props["ConUp2"],
        "ConLo3": lower_props["ConUp3"],

        "SphUp0": upper_props["SphUp0"],
        "SphUp1": upper_props["SphUp1"],
        "SphUp2": upper_props["SphUp2"],
        "SphUp3": upper_props["SphUp3"],

        "SphLo0": lower_props["SphUp0"],
        "SphLo1": lower_props["SphUp1"],
        "SphLo2": lower_props["SphUp2"],
        "SphLo3": lower_props["SphUp3"],
    })

    # Set body-specific parameters
    if hasattr(body_params, 'rotation_period'):
        params["PERIOD"] = body_params.rotation_period

    # Override with any additional kwargs
    params.update(kwargs)

    # Set latitude/longitude
    if lon is None:
        lon = 0.0

    params["Latitudes"] = [lat]
    params["Elevations"] = [ELEV if ELEV is not None else 0.0]

    # Set output format
    params["K4OUT"] = 52  # bin52 output

    # Execute KRC
    executor = KRCExecutor(krc_home)

    if verbose:
        print("Running KRC...")

    work_path = Path(workdir) if workdir else None

    result = executor.run_krc(
        params,
        workdir=work_path,
        verbose=verbose
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

    # Clean up if requested
    if not keep_files and workdir is None:
        import shutil
        shutil.rmtree(result["workdir"])

    return output
