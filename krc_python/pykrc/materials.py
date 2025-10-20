"""Material property calculations for KRC."""

from typing import Dict, Any, Optional, Tuple, List
import numpy as np
from dataclasses import dataclass


@dataclass
class MaterialCoefficients:
    """Coefficients for temperature-dependent material properties."""
    # Specific heat coefficients
    Sph0: float
    Sph1: float
    Sph2: float
    Sph3: float

    # Thermal conductivity coefficients
    Con0: float
    Con1: float
    Con2: float
    Con3: float

    # Density coefficients
    Dens0: float
    Dens1: float
    Dens2: float
    Dens3: float

    # Valid temperature range
    Cp_Tmin: float
    Cp_Tmax: float
    k_Tmin: float
    k_Tmax: float
    Dens_Tmin: float
    Dens_Tmax: float

    # References
    Cp_Ref: str = ""
    k_Ref: str = ""
    Dens_Ref: str = ""


# Material database
MATERIAL_DATABASE: Dict[str, MaterialCoefficients] = {
    "H2O": MaterialCoefficients(
        # Specific heat (Giauque & Stout 1936)
        Sph0=1704.57, Sph1=713.339, Sph2=110.694, Sph3=75.7506,
        Cp_Tmin=15.0, Cp_Tmax=273.0,
        Cp_Ref="Giauque & Stout (1936) J. Am. Chem. Soc. 58, 7, 1144-1150",

        # Thermal conductivity (Slack 1980)
        Con0=3.06445, Con1=-1.08693, Con2=-0.33438, Con3=-1.60453,
        k_Tmin=60.0, k_Tmax=270.0,
        k_Ref="Slack (1980) Phys. Rev. B 22, 3065",

        # Density (Röttger et al. 2012)
        Dens0=924.148, Dens1=-12.1406, Dens2=-3.71136, Dens3=0.036026,
        Dens_Tmin=10.0, Dens_Tmax=265.0,
        Dens_Ref="Röttger et al. (2012) Acta Crystallogr B. 68, 91",
    ),

    "basalt": MaterialCoefficients(
        # Specific heat (Hemingway et al. 1973)
        Sph0=609.906, Sph1=214.231, Sph2=-40.9437, Sph3=11.2575,
        Cp_Tmin=90.0, Cp_Tmax=350.0,
        Cp_Ref="Hemingway et al. (1973) LPSC 4, 2481",

        # Thermal conductivity (placeholder - needs reference)
        Con0=5.32202, Con1=-1.51737, Con2=0.587176, Con3=-0.126695,
        k_Tmin=10.0, k_Tmax=500.0,
        k_Ref="NO REFERENCE - VERIFY",

        # Density (Heuze 1983)
        Dens0=2600.0, Dens1=1e-5, Dens2=0.0, Dens3=0.0,
        Dens_Tmin=90.0, Dens_Tmax=350.0,
        Dens_Ref="Heuze (1983) Int. J. Rock Mech. Mining Sci. 20, 3-10",
    ),

    "CO2": MaterialCoefficients(
        # Specific heat (Solar System Ices)
        Sph0=1399.88, Sph1=652.64, Sph2=432.015, Sph3=192.398,
        Cp_Tmin=50.0, Cp_Tmax=230.0,
        Cp_Ref="Solar System Ices doi.org/10.1007/978-94-011-5252-5",

        # Thermal conductivity (placeholder)
        Con0=0.63, Con1=-0.05, Con2=0.0, Con3=0.0,
        k_Tmin=50.0, k_Tmax=230.0,
        k_Ref="Placeholder - needs reference",

        # Density (placeholder)
        Dens0=1600.0, Dens1=0.0, Dens2=0.0, Dens3=0.0,
        Dens_Tmin=50.0, Dens_Tmax=230.0,
        Dens_Ref="Placeholder - needs reference",
    ),
}


def get_material_properties(material: str) -> MaterialCoefficients:
    """
    Get material property coefficients.

    Parameters
    ----------
    material : str
        Material name (e.g., "basalt", "H2O", "CO2")

    Returns
    -------
    MaterialCoefficients
        Coefficients for the material

    Raises
    ------
    ValueError
        If material is not in database
    """
    if material not in MATERIAL_DATABASE:
        raise ValueError(
            f"Unknown material: {material}. "
            f"Available: {list(MATERIAL_DATABASE.keys())}"
        )
    return MATERIAL_DATABASE[material]


def calculate_specific_heat(T: float, coeffs: MaterialCoefficients) -> float:
    """
    Calculate specific heat at temperature T.

    Uses polynomial: Cp(T) = Sph0 + Sph1*X + Sph2*X^2 + Sph3*X^3
    where X = (T - 220) * 0.01

    Parameters
    ----------
    T : float
        Temperature in Kelvin
    coeffs : MaterialCoefficients
        Material coefficients

    Returns
    -------
    float
        Specific heat in J/(kg·K)
    """
    X = (T - 220.0) * 0.01
    Cp = coeffs.Sph0 + coeffs.Sph1*X + coeffs.Sph2*X**2 + coeffs.Sph3*X**3

    # Validate range
    if T < coeffs.Cp_Tmin or T > coeffs.Cp_Tmax:
        return np.nan

    return Cp


def calculate_thermal_conductivity(T: float, coeffs: MaterialCoefficients) -> float:
    """
    Calculate thermal conductivity at temperature T.

    Uses polynomial: k(T) = Con0 + Con1*X + Con2*X^2 + Con3*X^3
    where X = (T - 220) * 0.01

    Parameters
    ----------
    T : float
        Temperature in Kelvin
    coeffs : MaterialCoefficients
        Material coefficients

    Returns
    -------
    float
        Thermal conductivity in W/(m·K)
    """
    X = (T - 220.0) * 0.01
    k = coeffs.Con0 + coeffs.Con1*X + coeffs.Con2*X**2 + coeffs.Con3*X**3

    # Validate range
    if T < coeffs.k_Tmin or T > coeffs.k_Tmax:
        return np.nan

    return k


def calculate_density(T: float, coeffs: MaterialCoefficients) -> float:
    """
    Calculate density at temperature T.

    Uses polynomial: ρ(T) = Dens0 + Dens1*X + Dens2*X^2 + Dens3*X^3
    where X = (T - 220) * 0.01

    Parameters
    ----------
    T : float
        Temperature in Kelvin
    coeffs : MaterialCoefficients
        Material coefficients

    Returns
    -------
    float
        Density in kg/m³
    """
    X = (T - 220.0) * 0.01
    rho = coeffs.Dens0 + coeffs.Dens1*X + coeffs.Dens2*X**2 + coeffs.Dens3*X**3

    # Validate range
    if T < coeffs.Dens_Tmin or T > coeffs.Dens_Tmax:
        return np.nan

    return rho


def calculate_porosity(thermal_inertia: float, max_inertia: float = 2200.0) -> float:
    """
    Calculate porosity from thermal inertia.

    Formula: Por = 0.60 * (max_inertia - TI) / max_inertia

    Parameters
    ----------
    thermal_inertia : float
        Thermal inertia in SI units
    max_inertia : float, optional
        Maximum thermal inertia, default 2200

    Returns
    -------
    float
        Porosity (0-1)
    """
    return 0.60 * (max_inertia - thermal_inertia) / max_inertia


def calculate_thermal_properties(
    material: str,
    thermal_inertia: float,
    T_user: float = 220.0,
    k_style: str = "Mars"
) -> Dict[str, Any]:
    """
    Calculate thermal properties for a material.

    Parameters
    ----------
    material : str
        Material name
    thermal_inertia : float
        Thermal inertia in SI units (J m⁻² K⁻¹ s⁻½)
    T_user : float, optional
        User temperature in Kelvin, default 220
    k_style : str, optional
        Conductivity model: "Moon", "Mars", or "Bulk", default "Mars"

    Returns
    -------
    dict
        Dictionary containing:
        - SPEC_HEAT: Specific heat at T_user
        - DENSITY: Density at T_user
        - COND: Conductivity at T_user
        - Porosity: Calculated porosity
        - ConUp0-3: Conductivity polynomial coefficients
        - SphUp0-3: Specific heat polynomial coefficients
    """
    coeffs = get_material_properties(material)
    X_user = (T_user - 220.0) * 0.01

    # Calculate properties at user temperature
    SPEC_HEAT = calculate_specific_heat(T_user, coeffs)
    porosity = calculate_porosity(thermal_inertia)
    DENSITY = calculate_density(T_user, coeffs)
    DENSITY = (1 - porosity) * DENSITY  # Adjust for porosity
    COND = thermal_inertia**2 / (DENSITY * SPEC_HEAT)

    # Create temperature table for k(T) fitting
    # Davinci krc.dvrc lines 604-608: T_min=30, T_max=500, T_Step=10
    # T_NUM = int(1 + (T_max - T_min)/T_Step) = int(1 + 470/10) = 48 points
    T_min, T_max, T_step = 30.0, 500.0, 10.0
    T_tab = np.arange(T_min, T_max + T_step, T_step)  # 30, 40, 50, ..., 500 (48 points)
    X = (T_tab - 220.0) * 0.01

    # Calculate k(T) table based on style
    if k_style == "Moon":
        # Increases with T, T^3 trend (Hayne et al.)
        k_table = COND * (1 + 2.7 * ((T_tab - T_user) / 350.0)**3)
    elif k_style == "Mars":
        # Increases with T, sqrt(T) trend (Morgan et al.)
        k_table = COND * np.sqrt(T_tab / T_user)
    elif k_style == "Bulk":
        # Bulk conductivity - use material database polynomial normalized by k(T_user)
        # Davinci krc.dvrc lines 628-630
        X_user = (T_user - 220.0) * 0.01
        k_user = (coeffs.Con0 + coeffs.Con1*X_user +
                  coeffs.Con2*X_user**2 + coeffs.Con3*X_user**3)
        k_table = (coeffs.Con0 + coeffs.Con1*X +
                   coeffs.Con2*X**2 + coeffs.Con3*X**3) / k_user
    else:
        raise ValueError(f"Unknown k_style: {k_style}")

    # Fit cubic polynomial to k_table
    # Use numpy.polynomial.polynomial.polyfit which returns coefficients in ascending order
    # [c0, c1, c2, c3] to match davinci's fit() function behavior
    # Davinci: fit(y=k_Table, x=X, "cube") returns [a0, a1, a2, a3] for a0 + a1*x + a2*x² + a3*x³
    from numpy.polynomial import polynomial as P
    con_coeffs = P.polyfit(X, k_table, 3)
    ConUp0, ConUp1, ConUp2, ConUp3 = con_coeffs

    return {
        "INERTIA": thermal_inertia,
        "SPEC_HEAT": SPEC_HEAT,
        "DENSITY": DENSITY,
        "COND": COND,
        "Porosity": porosity,
        "ConUp0": ConUp0,
        "ConUp1": ConUp1,
        "ConUp2": ConUp2,
        "ConUp3": ConUp3,
        "SphUp0": coeffs.Sph0,
        "SphUp1": coeffs.Sph1,
        "SphUp2": coeffs.Sph2,
        "SphUp3": coeffs.Sph3,
        "composition": material,
    }


def extract_material_properties_for_numerics(
    using_direct_props: bool,
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: float,
    upper_props: Dict[str, Any]
) -> Tuple[float, float]:
    """
    Extract density and specific heat for numerical calculations.

    When using direct property specification, returns DENSITY and SPEC_HEAT directly.
    Otherwise derives density from INERTIA using: ρ = I²/(k·c)

    Parameters
    ----------
    using_direct_props : bool
        True if COND, DENSITY, SPEC_HEAT were directly specified
    DENSITY : float or None
        Direct density specification (kg/m³)
    SPEC_HEAT : float or None
        Direct specific heat specification (J/kg/K)
    INERTIA : float
        Thermal inertia (J m⁻² K⁻¹ s⁻½)
    upper_props : dict
        Calculated material properties with keys: 'SphUp0', 'ConUp0'

    Returns
    -------
    density : float
        Material density (kg/m³)
    specific_heat : float
        Specific heat (J/kg/K)

    Notes
    -----
    This helper appears 3 times in the original krc() function:
    - For N1 calculation (subsurface layers)
    - For N2 calculation (timesteps per day)
    - For stability check

    Examples
    --------
    >>> # Direct specification
    >>> dens, cp = extract_material_properties_for_numerics(
    ...     True, 1600.0, 800.0, 200.0, {}
    ... )
    >>> dens, cp
    (1600.0, 800.0)

    >>> # Derived from INERTIA
    >>> props = {"SphUp0": 647.0, "ConUp0": 0.025}
    >>> dens, cp = extract_material_properties_for_numerics(
    ...     False, None, None, 200.0, props
    ... )
    >>> # dens ≈ 2469.14 from I²/(k·c) = 200²/(0.025·647)
    >>> cp
    647.0
    """
    if using_direct_props:
        return DENSITY, SPEC_HEAT
    else:
        cp = upper_props["SphUp0"]
        k = upper_props["ConUp0"]
        # From I² = k·ρ·c, we get ρ = I²/(k·c)
        dens = (INERTIA**2) / (k * cp)
        return dens, cp


def calculate_material_properties(
    COND: Optional[float],
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: Optional[float],
    Mat1: str,
    Mat2: str,
    T_user: float,
    k_style: str,
    LKofT: bool,
    INERTIA2: Optional[float],
    thick: float,
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[bool, float, float, Dict[str, Any], Dict[str, Any]]:
    """
    Calculate thermal properties for upper and lower layers.

    This consolidates lines 760-814 of the original krc() function,
    implementing material property calculation logic.

    Parameters
    ----------
    COND : float, optional
        Direct thermal conductivity specification (W/m/K)
    DENSITY : float, optional
        Direct density specification (kg/m³)
    SPEC_HEAT : float, optional
        Direct specific heat specification (J/kg/K)
    INERTIA : float, optional
        Thermal inertia (J m⁻² K⁻¹ s⁻½)
    Mat1 : str
        Upper layer material name
    Mat2 : str
        Lower layer material name
    T_user : float
        Reference temperature (K)
    k_style : str
        Conductivity model ("Mars", "Moon", "Bulk")
    LKofT : bool
        Use temperature-dependent thermal properties
    INERTIA2 : float, optional
        Lower layer thermal inertia
    thick : float
        Layer thickness (m)
    master_params : dict
        Master.inp defaults
    verbose : bool
        Print details

    Returns
    -------
    using_direct_props : bool
        True if using direct COND/DENSITY/SPEC_HEAT specification
    INERTIA : float
        Thermal inertia (calculated if using direct props)
    INERTIA2 : float
        Lower layer thermal inertia (defaults to INERTIA)
    upper_props : dict
        Upper layer thermal property coefficients
    lower_props : dict
        Lower layer thermal property coefficients

    Notes
    -----
    Two modes of operation:
    1. Direct specification: COND, DENSITY, SPEC_HEAT all provided
       - Calculates INERTIA from I = sqrt(k·ρ·c)
       - Optionally generates T-dependent coefficients if LKofT=True
    2. INERTIA-based: Standard approach using material database
       - Calculates properties from Mat1, INERTIA, T_user, k_style
    """
    # Determine if using INERTIA or direct COND/DENSITY/SPEC_HEAT specification
    using_direct_props = (COND is not None and DENSITY is not None and SPEC_HEAT is not None)

    if using_direct_props:
        if verbose:
            print(f"Using direct material properties: COND={COND}, DENSITY={DENSITY}, SPEC_HEAT={SPEC_HEAT}")

        # Calculate implied INERTIA for reference
        INERTIA = np.sqrt(COND * DENSITY * SPEC_HEAT)

        upper_props = {
            "COND": COND,
            "DENSITY": DENSITY,
            "SPEC_HEAT": SPEC_HEAT,
        }

        # Generate T-dependent coefficients if requested
        if LKofT:
            # Use calculate_thermal_properties to get coefficients
            temp_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)
            upper_props.update({
                "ConUp0": temp_props["ConUp0"],
                "ConUp1": temp_props["ConUp1"],
                "ConUp2": temp_props["ConUp2"],
                "ConUp3": temp_props["ConUp3"],
                "SphUp0": temp_props["SphUp0"],
                "SphUp1": temp_props["SphUp1"],
                "SphUp2": temp_props["SphUp2"],
                "SphUp3": temp_props["SphUp3"],
            })
        else:
            # Constant properties (no T-dependence)
            upper_props.update({
                "ConUp0": COND, "ConUp1": 0.0, "ConUp2": 0.0, "ConUp3": 0.0,
                "SphUp0": SPEC_HEAT, "SphUp1": 0.0, "SphUp2": 0.0, "SphUp3": 0.0,
            })
    else:
        # Standard INERTIA-based approach
        if INERTIA is None:
            INERTIA = master_params.get("INERTIA", 200.0)

        if verbose:
            print(f"Calculating material properties for {Mat1} with INERTIA={INERTIA}...")

        upper_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)

    # Handle lower layer (for two-layer regolith)
    if INERTIA2 is None:
        INERTIA2 = INERTIA

    if verbose and thick != 0.0:
        print(f"Two-layer regolith: thick={thick}m, upper TI={INERTIA}, lower TI={INERTIA2}")

    lower_props = calculate_thermal_properties(Mat2, INERTIA2, T_user, k_style)

    return using_direct_props, INERTIA, INERTIA2, upper_props, lower_props
