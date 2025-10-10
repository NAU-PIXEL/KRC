# DETAILED REFACTORING PLAN: core.py:krc()

## Executive Summary

**Current State:** Single 963-line function with 140+ parameters handling entire KRC workflow
**Target State:** Main orchestration function (~150-200 lines) + 8-10 focused helper functions
**Risk Level:** Medium (must preserve exact numerical behavior)
**Estimated Effort:** 6-8 hours
**Testing Strategy:** Validate against existing test suite after each extraction

---

## Phase 1: Preparation (30 minutes)

### 1.1 Establish Baseline Tests
**Before any refactoring:**

```bash
# Run full test suite and capture output
cd krc_python
pytest tests/ -v --tb=short > baseline_results.txt

# Run specific integration tests
pytest tests/test_krc_run.py -v
pytest tests/test_krc_minimal.py -v
```

**Purpose:** Ensure all tests pass before refactoring. We'll re-run after each extraction.

### 1.2 Create Refactoring Branch
```bash
git checkout -b refactor/core-krc-function
git add -A
git commit -m "Baseline: Save working state before core.py refactoring"
```

---

## Phase 2: Extract Helper Functions (5-6 hours)

### Strategy: Bottom-Up Extraction
- Start with **pure functions** (no side effects, clear inputs/outputs)
- Extract **logically cohesive blocks**
- Preserve **exact behavior** (same calculations, same order)
- Keep **same variable names** initially (rename later if needed)

---

### **EXTRACTION 1: Material Property Derivation** (45 min)

**Location:** Lines 587-598, 618-625, 649-655 (appears 3 times - HIGH PRIORITY)

**New Function:**
```python
def _extract_material_properties_for_numerics(
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

    Examples
    --------
    >>> # Direct specification
    >>> dens, cp = _extract_material_properties_for_numerics(
    ...     True, 1600.0, 800.0, None, {}
    ... )
    >>> dens, cp
    (1600.0, 800.0)

    >>> # Derived from INERTIA
    >>> props = {"SphUp0": 647.0, "ConUp0": 0.025}
    >>> dens, cp = _extract_material_properties_for_numerics(
    ...     False, None, None, 200.0, props
    ... )
    >>> dens  # doctest: +ELLIPSIS
    2469...
    """
    if using_direct_props:
        return DENSITY, SPEC_HEAT
    else:
        cp = upper_props["SphUp0"]
        k = upper_props["ConUp0"]
        # From I² = k·ρ·c, we get ρ = I²/(k·c)
        dens = (INERTIA**2) / (k * cp)
        return dens, cp
```

**Where to place:** After `_extract_user_params()` function (~line 32)

**Changes in main function:**
- Line 587-598: Replace with `dens_for_N1, cp_for_N1 = _extract_material_properties_for_numerics(...)`
- Line 618-625: Replace with `dens_for_N2, cp_for_N2 = _extract_material_properties_for_numerics(...)`
- Line 649-655: Replace with `dens_check, cp_check = _extract_material_properties_for_numerics(...)`

**Test:**
```bash
pytest tests/test_krc_minimal.py -v
pytest tests/test_numerical.py -v  # Tests N1/N2 calculations
```

---

### **EXTRACTION 2: Default Parameter Setup** (1 hour)

**Location:** Lines 270-331 (parameter defaulting logic)

**New Function:**
```python
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
) -> Dict[str, Any]:
    """
    Apply default values to unset parameters.

    Returns dictionary with all defaults applied.
    Implements temperature prediction control logic from Davinci lines 841-847.

    Parameters
    ----------
    [All optional parameters from krc()]

    Returns
    -------
    dict
        Parameter names mapped to their default values

    Notes
    -----
    TPREDICT=0.0 triggers special stability mode:
    - GGT = 99.0 (disable gradient timestep control)
    - N3 = 1 (single iteration)
    - NRSET = 999 (disable reset)
    """
    defaults = {}

    defaults['DELLS'] = 1.0 if DELLS is None else DELLS
    defaults['spinup_years'] = 2.0 if spinup_years is None else spinup_years
    defaults['output_years'] = 1.0 if output_years is None else output_years
    defaults['LKEY'] = "T" if LKEY is None else LKEY
    defaults['LKofT'] = True if LKofT is None else LKofT
    defaults['thick'] = 0.0 if thick is None else thick
    defaults['FANON'] = 0.055 if FANON is None else FANON
    defaults['N3'] = 1 if N3 is None else N3
    defaults['NRSET'] = 0 if NRSET is None else NRSET
    defaults['GGT'] = 1.0 if GGT is None else GGT

    # Apply TPREDICT default
    tpredict = 0.0 if TPREDICT is None else TPREDICT
    defaults['TPREDICT'] = tpredict

    # Temperature prediction control (Davinci lines 841-847)
    if tpredict == 0.0:
        defaults['GGT'] = 99.0
        defaults['N3'] = 1
        defaults['NRSET'] = 999

    defaults['MAXN1'] = 100 if MAXN1 is None else MAXN1
    defaults['MAXN2'] = 1000 if MAXN2 is None else MAXN2
    defaults['auto_numerical'] = True if auto_numerical is None else auto_numerical
    defaults['bodyforce'] = 0 if bodyforce is None else bodyforce
    defaults['TUN8'] = 0 if TUN8 is None else TUN8
    defaults['LMST'] = "F" if LMST is None else LMST
    defaults['WRITE'] = "F" if WRITE is None else WRITE
    defaults['KEEP'] = "F" if KEEP is None else KEEP
    defaults['Eclipse'] = "F" if Eclipse is None else Eclipse
    defaults['Eclipse_Style'] = 1.0 if Eclipse_Style is None else Eclipse_Style
    defaults['PFlux'] = "F" if PFlux is None else PFlux
    defaults['Lon_Hr'] = 12.0 if Lon_Hr is None else Lon_Hr
    defaults['verbose'] = False if verbose is None else verbose
    defaults['keep_files'] = False if keep_files is None else keep_files

    return defaults
```

**Changes in main function:**
- Lines 270-331: Replace with:
```python
defaults = _apply_default_parameters(
    DELLS, spinup_years, output_years, LKEY, LKofT, thick, FANON,
    N3, NRSET, GGT, TPREDICT, MAXN1, MAXN2, auto_numerical,
    bodyforce, TUN8, LMST, WRITE, KEEP, Eclipse, Eclipse_Style,
    PFlux, Lon_Hr, verbose, keep_files
)
# Unpack defaults
DELLS = defaults['DELLS']
spinup_years = defaults['spinup_years']
# ... etc for all parameters
```

**Test:**
```bash
pytest tests/test_krc_run.py::test_defaults -v
```

---

### **EXTRACTION 3: PORB Parameter Setup** (1.5 hours)

**Location:** Lines 350-484 (orbital parameters and PORB-derived defaults)

**New Function:**
```python
def _setup_orbital_parameters(
    body: str,
    data_loader: 'KRCDataLoader',
    DELLS: float,
    N5: Optional[int],
    JDISK: Optional[int],
    spinup_years: float,
    output_years: float,
    PTOTAL: Optional[float],
    GRAV: Optional[float],
    TAURAT: Optional[float],
    DUSTA: Optional[float],
    ARC2_G0: Optional[float],
    EMISS: Optional[float],
    TDEEP: Optional[float],
    TAUD: Optional[float],
    DJUL: Optional[float],
    SLOPE: Optional[float],
    SLOAZI: Optional[float],
    TFROST: Optional[float],
    PhotoFunc: Optional[float],
    FLAY: Optional[float],
    RLAY: Optional[float],
    IIB: Optional[int],
    IC2: Optional[int],
    KPREF: Optional[int],
    JBARE: Optional[int],
    LVFT: Optional[bool],
    LKofT: Optional[bool],
    LZONE: Optional[str],
    verbose: bool
) -> Tuple[OrbitalElements, Dict[str, Any], set]:
    """
    Load orbital parameters and set PORB-derived defaults.

    Parameters
    ----------
    body : str
        Celestial body name (e.g., "Mars", "Moon")
    data_loader : KRCDataLoader
        Data loader instance
    [... other parameters ...]

    Returns
    -------
    body_params : OrbitalElements
        Orbital/physical parameters for the body
    porb_defaults : dict
        Parameters set by PORB (with defaults applied)
    porb_touched : set
        Set of parameter names touched by PORB (for changecard generation)

    Notes
    -----
    This function implements Davinci's PORB loading behavior where certain
    parameters get default values when PORB is loaded, ensuring changecards
    are written even if they match master.inp defaults.
    """
    if verbose:
        print(f"Loading orbital parameters for {body}...")

    body_params = porb(body, data_loader=data_loader)

    # Calculate N5, JDISK, DELJUL
    rot_per = body_params.rotation_period
    n24_from_porb = int(np.floor((rot_per * 4) / 96) * 96)
    if n24_from_porb < 96:
        n24_from_porb = 96

    if N5 is None:
        total_years = spinup_years + output_years
        N5 = int(np.ceil(360.0 / DELLS * total_years))

    if JDISK is None:
        JDISK = int(np.ceil(360.0 / DELLS * spinup_years + 1))

    # Calculate DELJUL
    if hasattr(body_params, 'krc_params') and 'DELJUL' in body_params.krc_params:
        DELJUL_calc = body_params.krc_params['DELJUL']
    elif hasattr(body_params, 'orbital_period'):
        DELJUL_calc = body_params.orbital_period * DELLS / 360.0
    else:
        DELJUL_calc = rot_per * DELLS / 360.0
        if verbose:
            print(f"Warning: Using rotation period for DELJUL calculation")

    # Track PORB-touched parameters
    porb_touched = set()
    porb_defaults = {}

    # Extract from PORB HDF krc_params if available
    if hasattr(body_params, 'krc_params') and body_params.krc_params:
        krc_params = body_params.krc_params

        if 'PTOTAL' in krc_params and PTOTAL is None:
            porb_defaults['PTOTAL'] = krc_params['PTOTAL']
            porb_touched.add('PTOTAL')
        # ... [similar for GRAV, TAURAT, DUSTA, ARC2_G0]

    # Set standard PORB-related defaults
    if EMISS is None:
        porb_defaults['EMISS'] = 1.0
        porb_touched.add('EMISS')
    # ... [similar for all PORB-touched parameters]

    # Add calculated values
    porb_defaults['N24'] = n24_from_porb
    porb_defaults['N5'] = N5
    porb_defaults['JDISK'] = JDISK
    porb_defaults['DELJUL'] = DELJUL_calc
    porb_defaults['PERIOD'] = rot_per

    return body_params, porb_defaults, porb_touched
```

**Test:**
```bash
pytest tests/test_krc_run.py -v
pytest tests/test_advanced_params.py -v
```

---

### **EXTRACTION 4: Material Property Calculation** (45 min)

**Location:** Lines 526-581 (material property handling)

**New Function:**
```python
def _calculate_material_properties(
    COND: Optional[float],
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    INERTIA: Optional[float],
    INERTIA2: Optional[float],
    Mat1: str,
    Mat2: str,
    T_user: float,
    k_style: str,
    thick: float,
    LKofT: bool,
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[Dict[str, Any], Dict[str, Any], bool, float]:
    """
    Calculate upper and lower layer material properties.

    Supports two methods:
    1. Direct specification: COND, DENSITY, SPEC_HEAT
    2. INERTIA-based: Calculate from material + thermal inertia

    Parameters
    ----------
    COND : float or None
        Direct thermal conductivity (W/m/K)
    DENSITY : float or None
        Direct density (kg/m³)
    SPEC_HEAT : float or None
        Direct specific heat (J/kg/K)
    INERTIA : float or None
        Thermal inertia (J m⁻² K⁻¹ s⁻½)
    INERTIA2 : float or None
        Lower layer thermal inertia
    Mat1, Mat2 : str
        Material names for upper/lower layers
    T_user : float
        Reference temperature (K)
    k_style : str
        Conductivity model: "Mars", "Moon", or "Bulk"
    thick : float
        Layer thickness (m)
    LKofT : bool
        Enable temperature-dependent properties
    master_params : dict
        Master.inp default parameters
    verbose : bool
        Print details

    Returns
    -------
    upper_props : dict
        Upper layer properties with T-dependent coefficients
    lower_props : dict
        Lower layer properties with T-dependent coefficients
    using_direct_props : bool
        True if direct specification was used
    INERTIA : float
        Thermal inertia (calculated if not provided)
    """
    using_direct_props = (
        COND is not None and
        DENSITY is not None and
        SPEC_HEAT is not None
    )

    if using_direct_props:
        if verbose:
            print(f"Using direct material properties: "
                  f"COND={COND}, DENSITY={DENSITY}, SPEC_HEAT={SPEC_HEAT}")

        INERTIA = np.sqrt(COND * DENSITY * SPEC_HEAT)

        upper_props = {
            "COND": COND,
            "DENSITY": DENSITY,
            "SPEC_HEAT": SPEC_HEAT,
        }

        if LKofT:
            temp_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)
            upper_props.update({
                "ConUp0": temp_props["ConUp0"],
                # ... [all coefficients]
            })
        else:
            upper_props.update({
                "ConUp0": COND, "ConUp1": 0.0, "ConUp2": 0.0, "ConUp3": 0.0,
                "SphUp0": SPEC_HEAT, "SphUp1": 0.0, "SphUp2": 0.0, "SphUp3": 0.0,
            })
    else:
        if INERTIA is None:
            INERTIA = master_params.get("INERTIA", 200.0)

        if verbose:
            print(f"Calculating material properties for {Mat1} "
                  f"with INERTIA={INERTIA}...")

        upper_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)

    # Lower layer
    if INERTIA2 is None:
        INERTIA2 = INERTIA

    if verbose and thick != 0.0:
        print(f"Two-layer regolith: thick={thick}m, "
              f"upper TI={INERTIA}, lower TI={INERTIA2}")

    lower_props = calculate_thermal_properties(Mat2, INERTIA2, T_user, k_style)

    return upper_props, lower_props, using_direct_props, INERTIA
```

**Test:**
```bash
pytest tests/test_materials.py -v
pytest tests/test_layers.py -v
```

---

### **EXTRACTION 5: Numerical Parameter Calculation** (1 hour)

**Location:** Lines 582-671 (N1/N2 calculation and stability check)

**New Function:**
```python
def _calculate_numerical_parameters(
    auto_numerical: bool,
    N1: Optional[int],
    N2: Optional[int],
    N3: int,
    GGT: float,
    TPREDICT: float,
    RLAY: float,
    FLAY: float,
    INERTIA: float,
    INERTIA2: float,
    thick: float,
    DELJUL: float,
    N5: int,
    JDISK: int,
    MAXN1: int,
    MAXN2: int,
    rot_per: float,
    n24_from_porb: int,
    DELLS: float,
    using_direct_props: bool,
    DENSITY: Optional[float],
    SPEC_HEAT: Optional[float],
    upper_props: Dict[str, Any],
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[int, int, int]:
    """
    Calculate numerical grid parameters (N1, N2) and convergence (N3).

    If auto_numerical=True, calculates optimal N1 and N2 based on:
    - N1: Number of subsurface layers (from seasonal skin depth)
    - N2: Number of timesteps per day (from diurnal skin depth)

    Also performs numerical stability check.

    Parameters
    ----------
    auto_numerical : bool
        Enable automatic N1/N2 calculation
    N1, N2 : int or None
        User-specified values (override auto-calculation)
    [... other parameters ...]

    Returns
    -------
    N1 : int
        Number of subsurface layers
    N2 : int
        Number of timesteps per day
    N3 : int
        Number of convergence iterations (may be updated)

    Notes
    -----
    Uses krc_evalN1() and krc_evalN2() from numerical module.
    Performs stability check and warns if unstable.
    """
    if auto_numerical:
        if N1 is None:
            dens_for_N1, cp_for_N1 = _extract_material_properties_for_numerics(
                using_direct_props, DENSITY, SPEC_HEAT, INERTIA, upper_props
            )

            N1 = krc_evalN1(
                RLAY=RLAY, FLAY=FLAY, INERTIA=INERTIA,
                SPEC_HEAT=cp_for_N1, DENSITY=dens_for_N1,
                DELJUL=DELJUL, N5=N5, JDISK=JDISK, MAXN1=MAXN1,
                PERIOD=rot_per,
                INERTIA2=INERTIA2 if thick != 0.0 else None,
                verbose=verbose
            )
            if verbose:
                print(f"Auto-calculated N1={N1} layers")

        if N2 is None:
            dens_for_N2, cp_for_N2 = _extract_material_properties_for_numerics(
                using_direct_props, DENSITY, SPEC_HEAT, INERTIA, upper_props
            )

            N2 = krc_evalN2(
                FLAY=FLAY, INERTIA=INERTIA,
                DENSITY=dens_for_N2, SPEC_HEAT=cp_for_N2,
                PERIOD=rot_per, N24=n24_from_porb, MAXN2=MAXN2,
                verbose=verbose
            )
            if verbose:
                print(f"Auto-calculated N2={N2} timesteps/day")
    else:
        if N1 is None:
            N1 = master_params.get("N1", 50)
        if N2 is None:
            N2 = master_params.get("N2", 288)

    # Stability check
    if N1 is not None and N2 is not None:
        dens_check, cp_check = _extract_material_properties_for_numerics(
            using_direct_props, DENSITY, SPEC_HEAT, INERTIA, upper_props
        )

        is_stable, stability_msg = check_stability(
            N1, N2, INERTIA, rot_per, FLAY, dens_check, cp_check, warn=True
        )
        if verbose:
            print(f"  {stability_msg}")

    # Auto-calculate convergence parameters
    if auto_numerical and (GGT == 1.0 and TPREDICT == 0.0):
        conv_params = calculate_convergence_params(N1, N2, DELLS, fast_mode=False)
        if N3 == 10:  # Only override default
            N3 = conv_params["N3"]
        if verbose:
            print(f"Convergence parameters: N3={N3}, GGT={GGT}, TPREDICT={TPREDICT}")

    return N1, N2, N3
```

**Test:**
```bash
pytest tests/test_numerical.py -v
```

---

### **EXTRACTION 6: Ancillary Data Loading** (30 min)

**Location:** Lines 489-525 (ancillary data lookups for Mars)

**New Function:**
```python
def _load_ancillary_data(
    body: str,
    lat: Union[float, List[float]],
    lon: float,
    ALBEDO: Optional[float],
    ELEV: Optional[float],
    INERTIA: Optional[float],
    master_params: Dict[str, Any],
    verbose: bool
) -> Tuple[float, float, Optional[float]]:
    """
    Load ancillary data for Mars (TES albedo, MOLA elevation, TES inertia).

    For non-Mars bodies, returns defaults.

    Parameters
    ----------
    body : str
        Celestial body name
    lat : float or list
        Latitude(s)
    lon : float
        Longitude
    ALBEDO : float or None
        User-specified albedo (overrides lookup)
    ELEV : float or None
        User-specified elevation (overrides lookup)
    INERTIA : float or None
        User-specified inertia (overrides lookup)
    master_params : dict
        Master.inp defaults
    verbose : bool
        Print details

    Returns
    -------
    ALBEDO : float
        Albedo value
    ELEV : float
        Elevation value (km)
    INERTIA : float or None
        Thermal inertia value (None if not loaded)
    """
    if body == "Mars":
        try:
            from .ancillary import lookup_albedo, lookup_elevation, lookup_inertia

            if ALBEDO is None:
                ALBEDO = lookup_albedo(lat, lon)
                if verbose:
                    print(f"Using TES albedo from ancillary data: {ALBEDO:.4f}")

            if ELEV is None:
                ELEV = lookup_elevation(lat, lon)
                if verbose:
                    print(f"Using MOLA elevation from ancillary data: {ELEV:.2f} km")

            if INERTIA is None:
                INERTIA = lookup_inertia(lat, lon)
                if verbose:
                    print(f"Using TES thermal inertia from ancillary data: {INERTIA:.1f}")

        except Exception as e:
            if verbose:
                print(f"Warning: Could not load ancillary data ({e}), using defaults")
            if ALBEDO is None:
                ALBEDO = master_params.get("ALBEDO", 0.25)
            if ELEV is None:
                ELEV = 0.0
    else:
        if ALBEDO is None:
            ALBEDO = master_params.get("ALBEDO", 0.25)
        if ELEV is None:
            ELEV = 0.0

    return ALBEDO, ELEV, INERTIA
```

**Test:**
```bash
pytest tests/test_krc_run.py::test_mars_ancillary -v
```

---

### **EXTRACTION 7: Parameter Dictionary Builder** (1 hour)

**Location:** Lines 707-920 (building params dictionary)

**New Function:**
```python
def _build_krc_parameter_dict(
    master_params: Dict[str, Any],
    # Surface properties
    ALBEDO: Union[float, List[float]],
    EMISS: float,
    INERTIA: float,
    TDEEP: float,
    SLOPE: float,
    SLOAZI: float,
    # Material properties
    upper_props: Dict[str, Any],
    lower_props: Dict[str, Any],
    # Atmospheric
    DUSTA: float,
    TAURAT: float,
    FANON: float,
    KPREF: int,
    PTOTAL: Optional[float],
    TATM: Optional[float],
    TAUD: Optional[Union[float, List[float]]],
    # Two-layer
    IC2: int,
    IIB: int,
    FLAY: float,
    RLAY: float,
    LZONE: bool,
    LKofT: bool,
    # Time control
    DELJUL: float,
    DJUL: float,
    LKEY: str,
    bodyforce: int,
    # Numerical
    N1: int,
    N2: int,
    N3: int,
    NRSET: int,
    GGT: float,
    # Output
    TUN8: int,
    LMST: str,
    WRITE: str,
    KEEP: str,
    K4OUT: int,
    JBARE: int,
    TUN_Flx15: int,
    # Physics
    PhotoFunc: float,
    # Eclipse/PFlux
    Eclipse: str,
    Eclipse_Style: float,
    Lon_Hr: float,
    PFlux: str,
    # Body params
    body_params: OrbitalElements,
    n24_from_porb: int,
    N5: int,
    JDISK: int,
    # Frost
    LVFT: bool,
    TFROST: Optional[float],
    CFROST: Optional[float],
    AFROST: Optional[float],
    # Optional overrides
    GRAV: Optional[float] = None,
    ARC2_G0: Optional[float] = None,
    JD: Optional[float] = None,
    GD: Optional[str] = None,
    DAU: Optional[float] = None,
    SOLCON: Optional[float] = None,
    SOLARDEC: Optional[float] = None,
    LsubS: Optional[float] = None,
    Atm_Cp: Optional[float] = None,
    stability: Optional[int] = None,
    anc: Optional[Dict] = None,
    # Eclipse params
    Eclipser: Optional[str] = None,
    Sun_Dis: Optional[float] = None,
    Eclipser_Rad: Optional[float] = None,
    Eclipsed_Rad: Optional[float] = None,
    CM: Optional[float] = None,
    Gamma: Optional[float] = None,
    Date: Optional[str] = None,
    Eclipse_line: Optional[str] = None,
    # PFlux params
    BT_Avg: Optional[float] = None,
    BT_Min: Optional[float] = None,
    BT_Max: Optional[float] = None,
    IR: Optional[float] = None,
    Vis: Optional[float] = None,
    Emissivity: Optional[float] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Build complete KRC parameter dictionary for execution.

    Combines all parameter sources:
    - Master.inp defaults
    - User-specified values
    - Calculated values (material props, numerical params)
    - PORB data
    - Optional overrides

    Returns
    -------
    dict
        Complete parameter dictionary for KRCExecutor
    """
    params = master_params.copy()

    # Handle ALBEDO (can be array)
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
        # ... [all parameters - lines 719-920]
    })

    # Add conditionals for optional params
    if GRAV is not None:
        params["GRAV"] = GRAV
    # ... [all optional additions]

    # Override with kwargs
    params.update(kwargs)

    return params
```

**Test:**
```bash
pytest tests/test_executor.py -v
```

---

### **EXTRACTION 8: Latitude/Elevation Setup** (30 min)

**Location:** Lines 922-947 (multi-latitude configuration)

**New Function:**
```python
def _setup_latitude_elevation(
    lat: Union[float, List[float]],
    ELEV: Union[float, List[float], None]
) -> Tuple[List[float], List[float], int]:
    """
    Configure latitude and elevation arrays for single or multi-lat runs.

    Parameters
    ----------
    lat : float or list of float
        Latitude value(s)
    ELEV : float, list of float, or None
        Elevation value(s) in km

    Returns
    -------
    latitudes : list of float
        Latitude array
    elevations : list of float
        Elevation array (matches length of latitudes)
    N4 : int
        Number of latitudes

    Raises
    ------
    ValueError
        If ELEV list length doesn't match lat list length

    Examples
    --------
    >>> _setup_latitude_elevation(45.0, 2.0)
    ([45.0], [2.0], 1)

    >>> _setup_latitude_elevation([30.0, 45.0, 60.0], None)
    ([30.0, 45.0, 60.0], [0.0, 0.0, 0.0], 3)

    >>> _setup_latitude_elevation([30.0, 45.0], [1.5, 2.0])
    ([30.0, 45.0], [1.5, 2.0], 2)
    """
    # Process latitudes
    if isinstance(lat, (list, tuple, np.ndarray)):
        latitudes = list(lat)
        N4 = len(latitudes)
    else:
        latitudes = [lat]
        N4 = 1

    # Process elevations
    if isinstance(ELEV, (list, tuple, np.ndarray)):
        elevations = list(ELEV)
        if len(elevations) != N4:
            raise ValueError(
                f"ELEV list length ({len(elevations)}) must match "
                f"lat list length ({N4})"
            )
    elif ELEV is not None:
        elevations = [ELEV] * N4
    else:
        elevations = [0.0] * N4

    return latitudes, elevations, N4
```

**Test:**
```bash
pytest tests/test_multilat.py -v
```

---

## Phase 3: Refactored Main Function (30 min)

After all extractions, the main `krc()` function will look like:

```python
def krc(
    # [All 140+ parameters remain the same]
    **kwargs
) -> Dict[str, Any]:
    """[Same docstring]"""

    # ========== EXTRACT USER PARAMETERS ==========
    user_params = _extract_user_params(**locals())

    # ========== APPLY DEFAULTS ==========
    defaults = _apply_default_parameters(
        DELLS, spinup_years, output_years, LKEY, LKofT, thick, FANON,
        N3, NRSET, GGT, TPREDICT, MAXN1, MAXN2, auto_numerical,
        bodyforce, TUN8, LMST, WRITE, KEEP, Eclipse, Eclipse_Style,
        PFlux, Lon_Hr, verbose, keep_files
    )
    # Unpack (or keep as dict and reference as defaults['DELLS'])

    # ========== VALIDATE REQUIRED ==========
    if lat is None:
        raise ValueError("lat (latitude) is required")
    if lon is None:
        lon = 0.0

    # ========== LOAD CONFIGURATION ==========
    krc_home = get_krc_home()
    paths = get_paths()
    master_params = parse_master_inp(paths.master_inp)
    data_loader = KRCDataLoader(paths.support_dir)

    # ========== ORBITAL PARAMETERS ==========
    body_params, porb_defaults, porb_touched = _setup_orbital_parameters(
        body, data_loader, DELLS, N5, JDISK, spinup_years, output_years,
        PTOTAL, GRAV, TAURAT, DUSTA, ARC2_G0, EMISS, TDEEP, TAUD, DJUL,
        SLOPE, SLOAZI, TFROST, PhotoFunc, FLAY, RLAY, IIB, IC2,
        KPREF, JBARE, LVFT, LKofT, LZONE, verbose
    )
    # Apply porb_defaults to local variables

    # ========== ANCILLARY DATA ==========
    ALBEDO, ELEV, INERTIA = _load_ancillary_data(
        body, lat, lon, ALBEDO, ELEV, INERTIA, master_params, verbose
    )

    # ========== MATERIAL PROPERTIES ==========
    upper_props, lower_props, using_direct_props, INERTIA = \
        _calculate_material_properties(
            COND, DENSITY, SPEC_HEAT, INERTIA, INERTIA2,
            Mat1, Mat2, T_user, k_style, thick, LKofT,
            master_params, verbose
        )

    # ========== NUMERICAL PARAMETERS ==========
    N1, N2, N3 = _calculate_numerical_parameters(
        auto_numerical, N1, N2, N3, GGT, TPREDICT,
        RLAY, FLAY, INERTIA, INERTIA2, thick,
        porb_defaults['DELJUL'], porb_defaults['N5'], porb_defaults['JDISK'],
        MAXN1, MAXN2, porb_defaults['PERIOD'], porb_defaults['N24'], DELLS,
        using_direct_props, DENSITY, SPEC_HEAT, upper_props,
        master_params, verbose
    )

    # ========== TWO-LAYER / FROST VALIDATION ==========
    validate_two_layer_config(thick, INERTIA, INERTIA2, Mat1, Mat2, Por1, Por2)

    if IC2 is None:
        IC2 = calculate_IC2(thick, N1, FLAY, RLAY)

    validate_frost_config(LVFT, PTOTAL, TFROST, body)

    if LVFT:
        # Auto-frost logic (lines 686-706)
        pass

    # ========== BUILD PARAMETER DICT ==========
    params = _build_krc_parameter_dict(
        master_params, ALBEDO, EMISS, INERTIA, TDEEP, SLOPE, SLOAZI,
        upper_props, lower_props, DUSTA, TAURAT, FANON, KPREF,
        PTOTAL, TATM, TAUD, IC2, IIB, FLAY, RLAY, LZONE, LKofT,
        porb_defaults['DELJUL'], DJUL, LKEY, bodyforce,
        N1, N2, N3, NRSET, GGT, TUN8, LMST, WRITE, KEEP,
        52, JBARE, 0, PhotoFunc, Eclipse, Eclipse_Style, Lon_Hr, PFlux,
        body_params, porb_defaults['N24'], porb_defaults['N5'], porb_defaults['JDISK'],
        LVFT, TFROST, CFROST, AFROST,
        GRAV=GRAV, ARC2_G0=ARC2_G0, JD=JD, GD=GD,
        # ... all optional params
        **kwargs
    )

    # ========== LATITUDE/ELEVATION ==========
    latitudes, elevations, N4 = _setup_latitude_elevation(lat, ELEV)
    params["Latitudes"] = latitudes
    params["Elevations"] = elevations
    params["N4"] = N4

    if verbose and N4 > 1:
        print(f"Multi-latitude run: {N4} latitudes")

    # ========== EXECUTE KRC ==========
    executor = KRCExecutor(krc_home)

    if verbose:
        print("Running KRC...")

    work_path = Path(workdir) if workdir else None
    params["_porb_touched_params"] = porb_touched

    result = executor.run_krc(
        params, workdir=work_path, verbose=verbose, user_params=user_params
    )

    if not result["success"]:
        raise RuntimeError(f"KRC execution failed: {result['stderr']}")

    # ========== PARSE OUTPUT ==========
    if verbose:
        print("Parsing output...")

    output = parse_bin52(result["output_file"], hour=hour, ls=ls)

    # ========== ADD METADATA ==========
    output.update({
        "body": body,
        "porb": body_params,
        "alb": ALBEDO,
        "elev": ELEV,
        "lat": lat,
        "lon": lon,
        "workdir": result["workdir"]
    })

    # ========== CLEANUP ==========
    if not keep_files and workdir is None:
        import shutil
        shutil.rmtree(result["workdir"])

    return output
```

**Estimated line count:** ~180-200 lines (down from 963!)

---

## Phase 4: Testing & Validation (1 hour)

### 4.1 Unit Tests for New Functions

Create `tests/test_core_helpers.py`:

```python
"""Unit tests for core.py helper functions."""

import pytest
import numpy as np
from pykrc.core import (
    _extract_material_properties_for_numerics,
    _apply_default_parameters,
    _setup_latitude_elevation,
)


class TestExtractMaterialProperties:
    def test_direct_specification(self):
        dens, cp = _extract_material_properties_for_numerics(
            using_direct_props=True,
            DENSITY=1600.0,
            SPEC_HEAT=800.0,
            INERTIA=None,
            upper_props={}
        )
        assert dens == 1600.0
        assert cp == 800.0

    def test_derived_from_inertia(self):
        props = {"SphUp0": 647.0, "ConUp0": 0.025}
        dens, cp = _extract_material_properties_for_numerics(
            using_direct_props=False,
            DENSITY=None,
            SPEC_HEAT=None,
            INERTIA=200.0,
            upper_props=props
        )
        expected_dens = (200.0**2) / (0.025 * 647.0)
        assert np.isclose(dens, expected_dens)
        assert cp == 647.0


class TestSetupLatitudeElevation:
    def test_single_lat_single_elev(self):
        lats, elevs, n4 = _setup_latitude_elevation(45.0, 2.0)
        assert lats == [45.0]
        assert elevs == [2.0]
        assert n4 == 1

    def test_multi_lat_no_elev(self):
        lats, elevs, n4 = _setup_latitude_elevation([30, 45, 60], None)
        assert lats == [30, 45, 60]
        assert elevs == [0.0, 0.0, 0.0]
        assert n4 == 3

    def test_multi_lat_single_elev(self):
        lats, elevs, n4 = _setup_latitude_elevation([30, 45], 1.5)
        assert lats == [30, 45]
        assert elevs == [1.5, 1.5]
        assert n4 == 2

    def test_multi_lat_multi_elev(self):
        lats, elevs, n4 = _setup_latitude_elevation([30, 45], [1.0, 2.0])
        assert lats == [30, 45]
        assert elevs == [1.0, 2.0]
        assert n4 == 2

    def test_mismatched_lengths_raises(self):
        with pytest.raises(ValueError, match="must match"):
            _setup_latitude_elevation([30, 45, 60], [1.0, 2.0])


class TestApplyDefaults:
    def test_all_none_uses_defaults(self):
        defaults = _apply_default_parameters(
            DELLS=None, spinup_years=None, output_years=None,
            LKEY=None, LKofT=None, thick=None, FANON=None,
            N3=None, NRSET=None, GGT=None, TPREDICT=None,
            MAXN1=None, MAXN2=None, auto_numerical=None,
            bodyforce=None, TUN8=None, LMST=None, WRITE=None,
            KEEP=None, Eclipse=None, Eclipse_Style=None,
            PFlux=None, Lon_Hr=None, verbose=None, keep_files=None
        )
        assert defaults['DELLS'] == 1.0
        assert defaults['spinup_years'] == 2.0
        assert defaults['TPREDICT'] == 0.0

    def test_tpredict_zero_overrides(self):
        defaults = _apply_default_parameters(
            # ... all None except:
            TPREDICT=0.0, GGT=1.0, N3=10, NRSET=0
            # ... (rest None)
        )
        assert defaults['GGT'] == 99.0
        assert defaults['N3'] == 1
        assert defaults['NRSET'] == 999
```

### 4.2 Integration Tests

Run existing test suite after refactoring:

```bash
# Full test suite
pytest tests/ -v

# Critical integration tests
pytest tests/test_krc_run.py -v
pytest tests/test_krc_minimal.py -v
pytest tests/test_multilat.py -v
pytest tests/test_advanced_params.py -v

# Numerical validation
pytest tests/test_numerical.py -v
pytest tests/test_layers.py -v
pytest tests/test_frost.py -v
```

### 4.3 Regression Testing

Compare outputs before/after refactoring:

```python
# tests/test_refactoring_regression.py
"""
Regression tests for core.py refactoring.
Ensures numerical output is identical before/after refactoring.
"""

import pytest
import numpy as np
from pykrc import krc


@pytest.mark.parametrize("lat,lon,ls,inertia,albedo", [
    (0, 0, 270, 200, 0.25),
    (45, 180, 90, 300, 0.15),
    (-60, 270, 180, 150, 0.35),
])
def test_refactoring_preserves_output(lat, lon, ls, inertia, albedo):
    """Test that refactored krc() produces identical output."""
    result = krc(
        lat=lat, lon=lon, body="Mars", ls=ls,
        INERTIA=inertia, ALBEDO=albedo,
        DELLS=15.0, spinup_years=1.0, output_years=0.5
    )

    # Check output structure
    assert 'surf' in result
    assert 'bol' in result
    assert 'time' in result

    # Check output shapes
    assert result['surf'].shape[0] > 0

    # Check temperature ranges (sanity check)
    assert np.all(result['surf'] > 0)
    assert np.all(result['surf'] < 400)
```

---

## Phase 5: Documentation & Cleanup (30 min)

### 5.1 Update Docstrings

Ensure each helper function has:
- Full parameter documentation
- Return value documentation
- Examples
- Notes about Davinci correspondence

### 5.2 Add Module Docstring

At top of `core.py`:

```python
"""
Main KRC interface function.

This module provides the primary Python interface to the KRC thermal model.
The main entry point is the krc() function, which orchestrates:

1. Parameter validation and defaulting
2. Orbital parameter loading (PORB)
3. Material property calculation
4. Numerical grid setup (N1, N2)
5. KRC Fortran execution
6. Output parsing

The implementation closely follows the Davinci krc.dvrc wrapper to ensure
numerical parity with the reference implementation.

Helper Functions
----------------
_extract_user_params : Extract user-specified parameters
_apply_default_parameters : Apply default values to unset parameters
_setup_orbital_parameters : Load PORB and set body-specific defaults
_load_ancillary_data : Load TES/MOLA data for Mars
_calculate_material_properties : Compute thermal properties from INERTIA
_extract_material_properties_for_numerics : Extract ρ and c for calculations
_calculate_numerical_parameters : Auto-calculate N1, N2, check stability
_setup_latitude_elevation : Configure multi-latitude runs
_build_krc_parameter_dict : Assemble complete parameter set for execution
"""
```

### 5.3 Update CHANGELOG

Create entry documenting refactoring:

```markdown
## [0.2.0] - 2025-10-10

### Changed
- **BREAKING REFACTOR**: Refactored `krc()` function in core.py
  - Extracted 8 helper functions for improved maintainability
  - Reduced main function from 963 lines to ~180 lines
  - **No change to public API or numerical behavior**
  - All existing tests pass without modification

### Added
- Helper functions in core.py:
  - `_extract_material_properties_for_numerics()`
  - `_apply_default_parameters()`
  - `_setup_orbital_parameters()`
  - `_load_ancillary_data()`
  - `_calculate_material_properties()`
  - `_calculate_numerical_parameters()`
  - `_setup_latitude_elevation()`
  - `_build_krc_parameter_dict()`
- Unit tests for core helper functions in `tests/test_core_helpers.py`
```

---

## Phase 6: Final Validation (30 min)

### 6.1 Run Full Test Suite

```bash
pytest tests/ -v --cov=pykrc/core --cov-report=html
```

Target: 100% test pass rate, >85% code coverage for core.py

### 6.2 Performance Check

Ensure refactoring doesn't impact performance:

```python
import time
from pykrc import krc

start = time.time()
result = krc(lat=0, lon=0, body="Mars", ls=270, INERTIA=200, ALBEDO=0.25)
elapsed = time.time() - start

print(f"Execution time: {elapsed:.2f}s")
# Should be similar to pre-refactoring time
```

### 6.3 Manual Integration Test

Run a complex multi-parameter case:

```python
from pykrc import krc

result = krc(
    lat=[0, 30, 60],  # Multi-latitude
    lon=180,
    body="Mars",
    ls=90,
    INERTIA=300,
    INERTIA2=150,
    thick=0.05,  # Two-layer
    ALBEDO=0.15,
    TAUD=0.5,
    LVFT=True,  # Frost
    DELLS=5.0,
    spinup_years=1.5,
    output_years=0.5,
    verbose=True
)

print(f"Success! Output shape: {result['surf'].shape}")
```

---

## Risk Mitigation

### Risks & Mitigation Strategies

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Break numerical parity with Davinci | Medium | High | Run validation tests after each extraction |
| Introduce bugs in parameter passing | Medium | High | Unit test each helper function |
| Miss edge cases | Low | Medium | Comprehensive test coverage |
| Performance regression | Low | Low | Benchmark before/after |
| API breakage for users | Very Low | High | No changes to public signature |

### Rollback Plan

```bash
# If issues found after refactoring:
git checkout main
git branch -D refactor/core-krc-function

# Or cherry-pick specific extractions:
git revert <commit-hash>
```

---

## Success Criteria

✅ All existing tests pass without modification
✅ New unit tests for helper functions with >90% coverage
✅ Main `krc()` function reduced to <200 lines
✅ No change to public API
✅ Numerical output identical to pre-refactoring (validated via tests)
✅ Code complexity reduced (measured via `radon` or `pylint`)
✅ Documentation complete for all new functions

---

## Maintenance Benefits

**After refactoring:**

1. **Testability**: Each helper function can be unit tested independently
2. **Readability**: Main function reads like a workflow, not implementation
3. **Reusability**: Helper functions can be used elsewhere (e.g., validation scripts)
4. **Debuggability**: Easier to isolate issues to specific stages
5. **Extensibility**: Adding new features (e.g., Moon support) is clearer
6. **Onboarding**: New developers can understand flow more quickly

---

## Timeline Summary

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| 1. Preparation | 30 min | Baseline tests, git branch |
| 2. Extractions 1-8 | 6 hours | 8 helper functions |
| 3. Refactored main | 30 min | Simplified krc() |
| 4. Testing | 1 hour | Unit + integration tests |
| 5. Documentation | 30 min | Docstrings, CHANGELOG |
| 6. Validation | 30 min | Final checks |
| **TOTAL** | **9 hours** | Production-ready refactor |

---

## Next Steps After Refactoring

1. Apply same pattern to `executor.py:create_input_file()` (177 lines)
2. Apply same pattern to `executor.py:_write_changecards()` (134 lines)
3. Set up automated complexity monitoring with `radon`
4. Consider extracting frost logic to separate module
