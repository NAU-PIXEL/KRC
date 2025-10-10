"""Frost and atmospheric condensation calculations."""

from typing import Optional, Tuple, Dict
import numpy as np


# Frost parameters for common condensable atmospheres
FROST_DATABASES = {
    "Mars_CO2": {
        "TFROST": 148.0,      # CO2 frost point at 600 Pa
        "CFROST": 590000.0,   # Latent heat of CO2 sublimation (J/kg)
        "AFROST": 0.65,       # Albedo of CO2 frost
        "description": "Mars CO2 frost (Tillman et al. 1993)"
    },
    "Titan_N2": {
        "TFROST": 80.0,       # N2 frost point
        "CFROST": 580.0,      # Approximate
        "AFROST": 16.0,       # Approximate
        "description": "Titan N2 frost"
    },
    "Pluto_N2": {
        "TFROST": 37.0,       # N2 frost on Pluto
        "CFROST": 580.0,
        "AFROST": 16.0,
        "description": "Pluto N2 frost"
    },
    "Triton_N2": {
        "TFROST": 38.0,       # N2 frost on Triton
        "CFROST": 580.0,
        "AFROST": 16.0,
        "description": "Triton N2 frost"
    }
}


def get_frost_params_for_body(
    body: str,
    PTOTAL: float
) -> Optional[Tuple[float, float, float]]:
    """
    Get frost parameters for a known body.

    Parameters
    ----------
    body : str
        Body name (e.g., "Mars", "Titan", "Pluto")
    PTOTAL : float
        Total atmospheric pressure (Pa)

    Returns
    -------
    tuple or None
        (TFROST, CFROST, AFROST) if body has frost, None otherwise

    Notes
    -----
    Returns standard frost parameters for known bodies.

    For Mars CO2:
    - TFROST ≈ 148 K (at ~600 Pa)
    - CFROST = 590000 J/kg (latent heat of CO2 sublimation)
    - AFROST = 0.65 (frost albedo)

    For more accurate pressure-dependent calculations, KRC uses the full
    Clausius-Clapeyron equation internally
    """
    body_upper = body.upper()

    # Mars CO2 frost
    if "MARS" in body_upper:
        if PTOTAL <= 0:
            return None

        # Mars CO2 frost parameters
        # Use standard values (full pressure-dependent calculation done in KRC)
        params = FROST_DATABASES["Mars_CO2"]
        TFROST = params["TFROST"]
        CFROST = params["CFROST"]
        AFROST = params["AFROST"]

        return (TFROST, CFROST, AFROST)

    # Titan N2 frost
    if "TITAN" in body_upper:
        params = FROST_DATABASES["Titan_N2"]
        return (params["TFROST"], params["CFROST"], params["AFROST"])

    # Pluto N2 frost
    if "PLUTO" in body_upper:
        params = FROST_DATABASES["Pluto_N2"]
        return (params["TFROST"], params["CFROST"], params["AFROST"])

    # Triton N2 frost
    if "TRITON" in body_upper:
        params = FROST_DATABASES["Triton_N2"]
        return (params["TFROST"], params["CFROST"], params["AFROST"])

    # No frost for this body
    return None


def validate_frost_config(
    LVFT: bool,
    PTOTAL: Optional[float],
    TFROST: Optional[float],
    body: str
) -> None:
    """
    Validate frost configuration.

    Parameters
    ----------
    LVFT : bool
        Enable variable frost temperature
    PTOTAL : float or None
        Atmospheric pressure (Pa)
    TFROST : float or None
        Frost point temperature (K)
    body : str
        Body name

    Raises
    ------
    ValueError
        If frost configuration is invalid
    """
    if not LVFT:
        return

    # Frost requires atmosphere
    if PTOTAL is None or PTOTAL <= 0:
        raise ValueError(
            "Frost modeling (LVFT=True) requires atmospheric pressure (PTOTAL > 0)"
        )

    # Check if we can auto-calculate frost parameters
    auto_params = get_frost_params_for_body(body, PTOTAL)

    if TFROST is None and auto_params is None:
        raise ValueError(
            f"Frost modeling (LVFT=True) requires TFROST for body '{body}'. "
            f"Auto-calculation is only available for: Mars, Titan, Pluto, Triton"
        )


def calculate_frost_albedo(
    ALBEDO: float,
    AFROST: Optional[float] = None
) -> float:
    """
    Calculate frost albedo.

    Parameters
    ----------
    ALBEDO : float
        Surface albedo (0-1)
    AFROST : float, optional
        Frost albedo override

    Returns
    -------
    float
        Albedo to use when frost is present

    Notes
    -----
    If AFROST is provided, use it directly.
    Otherwise, use typical frost albedo values:
    - CO2 frost: 0.65
    - H2O frost: 0.55
    """
    if AFROST is not None:
        # In KRC, AFROST parameter can mean:
        # - Clausius-Clapeyron A constant (23.3494 for Mars)
        # - Frost albedo (0-1 range)
        # Check range to disambiguate
        if 0 <= AFROST <= 1:
            return AFROST

    # Default frost albedo (CO2 ice)
    return 0.65


def get_frost_info(
    LVFT: bool,
    TFROST: Optional[float],
    CFROST: Optional[float],
    AFROST: Optional[float],
    body: str,
    PTOTAL: Optional[float]
) -> Dict[str, any]:
    """
    Get frost configuration information.

    Parameters
    ----------
    LVFT : bool
        Frost enabled
    TFROST : float or None
        Frost temperature
    CFROST : float or None
        Clausius-Clapeyron C
    AFROST : float or None
        Clausius-Clapeyron A or frost albedo
    body : str
        Body name
    PTOTAL : float or None
        Atmospheric pressure

    Returns
    -------
    dict
        Frost configuration info
    """
    if not LVFT:
        return {
            "enabled": False,
            "frost_type": None,
            "TFROST": None,
            "CFROST": None,
            "AFROST": None
        }

    # Determine frost type
    frost_type = "custom"
    if "MARS" in body.upper():
        frost_type = "CO2"
    elif "TITAN" in body.upper() or "PLUTO" in body.upper() or "TRITON" in body.upper():
        frost_type = "N2"

    return {
        "enabled": True,
        "frost_type": frost_type,
        "TFROST": TFROST,
        "CFROST": CFROST,
        "AFROST": AFROST,
        "PTOTAL": PTOTAL
    }
