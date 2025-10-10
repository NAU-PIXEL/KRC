"""Parameter validation for KRC inputs."""

from typing import Optional, Any, Dict
import numpy as np


class KRCValidationError(ValueError):
    """Exception raised for invalid KRC parameter combinations."""
    pass


def validate_location(
    lat: float,
    lon: float,
    ELEV: Optional[float] = None
) -> None:
    """
    Validate location parameters.

    Parameters
    ----------
    lat : float
        Latitude in degrees
    lon : float
        Longitude in degrees
    ELEV : float, optional
        Elevation in km

    Raises
    ------
    KRCValidationError
        If parameters are out of valid range
    """
    if not -90 <= lat <= 90:
        raise KRCValidationError(f"Latitude must be in [-90, 90], got {lat}")

    if not 0 <= lon <= 360:
        raise KRCValidationError(f"Longitude must be in [0, 360], got {lon}")

    if ELEV is not None:
        # Reasonable elevation bounds (Mars: -8 to +22 km)
        if not -10 <= ELEV <= 30:
            raise KRCValidationError(f"Elevation must be in [-10, 30] km, got {ELEV}")


def validate_material_properties(
    INERTIA: Optional[float] = None,
    COND: Optional[float] = None,
    DENSITY: Optional[float] = None,
    SPEC_HEAT: Optional[float] = None,
    ALBEDO: Optional[float] = None,
    EMISS: Optional[float] = None
) -> None:
    """
    Validate material property parameters.

    Parameters
    ----------
    INERTIA : float, optional
        Thermal inertia (J m^-2 K^-1 s^-0.5)
    COND : float, optional
        Thermal conductivity (W m^-1 K^-1)
    DENSITY : float, optional
        Density (kg m^-3)
    SPEC_HEAT : float, optional
        Specific heat (J kg^-1 K^-1)
    ALBEDO : float, optional
        Albedo (0-1)
    EMISS : float, optional
        Emissivity (0-1)

    Raises
    ------
    KRCValidationError
        If parameters are out of valid range or inconsistent
    """
    # Check specification method
    has_inertia = INERTIA is not None
    has_direct = COND is not None and DENSITY is not None and SPEC_HEAT is not None

    if not has_inertia and not has_direct:
        raise KRCValidationError(
            "Must specify either INERTIA or (COND, DENSITY, SPEC_HEAT)"
        )

    if has_direct:
        # Check that all three are provided
        if COND is None or DENSITY is None or SPEC_HEAT is None:
            raise KRCValidationError(
                "If specifying direct properties, must provide all of: COND, DENSITY, SPEC_HEAT"
            )

        # Validate ranges
        if not 0.001 <= COND <= 10.0:
            raise KRCValidationError(f"COND must be in [0.001, 10.0] W/m/K, got {COND}")

        if not 100 <= DENSITY <= 10000:
            raise KRCValidationError(f"DENSITY must be in [100, 10000] kg/m³, got {DENSITY}")

        if not 100 <= SPEC_HEAT <= 5000:
            raise KRCValidationError(f"SPEC_HEAT must be in [100, 5000] J/kg/K, got {SPEC_HEAT}")

        # Calculate implied INERTIA
        inertia_calc = np.sqrt(COND * DENSITY * SPEC_HEAT)

        # Validate it's reasonable
        if not 5 <= inertia_calc <= 5000:
            raise KRCValidationError(
                f"Implied thermal inertia {inertia_calc:.1f} is outside reasonable range [5, 5000]"
            )

    if INERTIA is not None:
        if not 5 <= INERTIA <= 5000:
            raise KRCValidationError(f"INERTIA must be in [5, 5000], got {INERTIA}")

    if ALBEDO is not None:
        if not 0.0 <= ALBEDO <= 1.0:
            raise KRCValidationError(f"ALBEDO must be in [0, 1], got {ALBEDO}")

    if EMISS is not None:
        if not 0.1 <= EMISS <= 1.0:
            raise KRCValidationError(f"EMISS must be in [0.1, 1], got {EMISS}")


def validate_time_parameters(
    DELLS: float,
    N5: Optional[int],
    JDISK: Optional[int],
    PERIOD: float
) -> None:
    """
    Validate time control parameters.

    Parameters
    ----------
    DELLS : float
        Season spacing (degrees Ls)
    N5 : int or None
        Total seasons
    JDISK : int or None
        Starting season for output
    PERIOD : float
        Rotation period (days)

    Raises
    ------
    KRCValidationError
        If time parameters are invalid
    """
    if not 0.001 <= DELLS <= 360:
        raise KRCValidationError(f"DELLS must be in [0.001, 360], got {DELLS}")

    if N5 is not None:
        if not 1 <= N5 <= 100000:
            raise KRCValidationError(f"N5 must be in [1, 100000], got {N5}")

    if JDISK is not None:
        if not 1 <= JDISK <= 100000:
            raise KRCValidationError(f"JDISK must be in [1, 100000], got {JDISK}")

    if N5 is not None and JDISK is not None:
        if JDISK >= N5:
            raise KRCValidationError(
                f"JDISK ({JDISK}) must be < N5 ({N5}) to have output seasons"
            )

    if not 0.001 <= PERIOD <= 1000:
        raise KRCValidationError(f"PERIOD must be in [0.001, 1000] days, got {PERIOD}")


def validate_numerical_parameters(
    N1: Optional[int],
    N2: Optional[int],
    N3: int,
    NRSET: int
) -> None:
    """
    Validate numerical control parameters.

    Parameters
    ----------
    N1 : int or None
        Number of layers
    N2 : int or None
        Timesteps per day
    N3 : int
        Days per convergence check
    NRSET : int
        Max convergence iterations

    Raises
    ------
    KRCValidationError
        If parameters are out of valid range
    """
    if N1 is not None:
        if not 10 <= N1 <= 200:
            raise KRCValidationError(f"N1 must be in [10, 200], got {N1}")

    if N2 is not None:
        if not 48 <= N2 <= 10000:
            raise KRCValidationError(f"N2 must be in [48, 10000], got {N2}")

    if not 1 <= N3 <= 1000:
        raise KRCValidationError(f"N3 must be in [1, 1000], got {N3}")

    if not 0 <= NRSET <= 1000:
        raise KRCValidationError(f"NRSET must be in [0, 1000], got {NRSET}")


def validate_atmospheric_parameters(
    TAUD: Optional[float],
    PTOTAL: Optional[float],
    TATM: Optional[float]
) -> None:
    """
    Validate atmospheric parameters.

    Parameters
    ----------
    TAUD : float or None
        Dust optical depth
    PTOTAL : float or None
        Total pressure (Pa)
    TATM : float or None
        Atmospheric temperature (K)

    Raises
    ------
    KRCValidationError
        If parameters are out of valid range
    """
    if TAUD is not None:
        if not 0.0 <= TAUD <= 10.0:
            raise KRCValidationError(f"TAUD must be in [0, 10], got {TAUD}")

    if PTOTAL is not None:
        if not 0.0 <= PTOTAL <= 1e7:
            raise KRCValidationError(f"PTOTAL must be in [0, 1e7] Pa, got {PTOTAL}")

    if TATM is not None:
        if not 10 <= TATM <= 1000:
            raise KRCValidationError(f"TATM must be in [10, 1000] K, got {TATM}")


def validate_all_parameters(params: Dict[str, Any]) -> None:
    """
    Run all parameter validation checks.

    Parameters
    ----------
    params : dict
        Dictionary of KRC parameters

    Raises
    ------
    KRCValidationError
        If any parameters are invalid
    """
    # Location
    validate_location(
        params.get("lat"),
        params.get("lon", 0.0),
        params.get("ELEV")
    )

    # Material properties
    validate_material_properties(
        INERTIA=params.get("INERTIA"),
        COND=params.get("COND"),
        DENSITY=params.get("DENSITY"),
        SPEC_HEAT=params.get("SPEC_HEAT"),
        ALBEDO=params.get("ALBEDO"),
        EMISS=params.get("EMISS")
    )

    # Time parameters
    if "PERIOD" in params:
        validate_time_parameters(
            params.get("DELLS", 1.0),
            params.get("N5"),
            params.get("JDISK"),
            params.get("PERIOD")
        )

    # Numerical parameters
    validate_numerical_parameters(
        params.get("N1"),
        params.get("N2"),
        params.get("N3", 10),
        params.get("NRSET", 0)
    )

    # Atmospheric parameters
    validate_atmospheric_parameters(
        params.get("TAUD"),
        params.get("PTOTAL"),
        params.get("TATM")
    )
