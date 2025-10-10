"""Two-layer regolith configuration and calculations."""

from typing import Optional, Dict, Any
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

    # Exponential profile (not yet implemented)
    if thick < 0.0:
        raise NotImplementedError("Exponential profiles (thick < 0) not yet supported")

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
        raise NotImplementedError("Exponential profiles (thick < 0) not yet supported")

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
