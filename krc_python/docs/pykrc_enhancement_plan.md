# PyKRC Enhancement Plan: Achieving Feature Parity with krc.dvrc

**Document Version:** 1.0
**Date:** 2025-10-08
**Goal:** Expand pykrc parameter handling to match krc.dvrc capabilities
**Current Coverage:** 37.6% (32/85 parameters)
**Target Coverage:** 95%+ (80+/85 parameters)

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Implementation Strategy](#implementation-strategy)
3. [Phase 1: Critical Parameters (2-3 weeks)](#phase-1-critical-parameters)
4. [Phase 2: Time & Numerical Control (2-3 weeks)](#phase-2-time--numerical-control)
5. [Phase 3: Advanced Features (1-2 months)](#phase-3-advanced-features)
6. [Phase 4: Testing & Validation (2-3 weeks)](#phase-4-testing--validation)
7. [Architecture Changes](#architecture-changes)
8. [API Design](#api-design)
9. [Backward Compatibility](#backward-compatibility)
10. [Testing Strategy](#testing-strategy)
11. [Risk Assessment](#risk-assessment)

---

## Executive Summary

### Current State
- **37.6% parameter coverage** (32/85 parameters)
- Suitable for simple, uniform material modeling
- Hardcoded time control (`DELLS=1.0`, `N5=1080`)
- No two-layer regolith support
- No frost/condensation modeling
- Missing numerical tuning capabilities

### Target State
- **95%+ parameter coverage** (80+/85 parameters)
- Full two-layer regolith support
- Complete frost modeling capabilities
- User-configurable time resolution
- Automatic N1/N2 calculation with user override
- Eclipse and planetary flux modeling
- Temperature-depth profile output

### Implementation Timeline
- **Phase 1:** 2-3 weeks (Critical parameters)
- **Phase 2:** 2-3 weeks (Time & numerical control)
- **Phase 3:** 1-2 months (Advanced features)
- **Phase 4:** 2-3 weeks (Testing & validation)
- **Total:** 3-4 months to production-ready

### Key Benefits
1. Full parity with IDL krc.dvrc interface
2. Support for Mars polar science (frost modeling)
3. Layered regolith studies
4. Custom time resolution studies
5. Satellite thermal modeling
6. Production science readiness

---

## Implementation Strategy

### Guiding Principles

1. **Backward Compatibility:** All existing code must continue to work
2. **Incremental Deployment:** Each phase produces working, testable code
3. **Davinci Alignment:** Match krc.dvrc behavior exactly where possible
4. **Validation First:** Test against davinci krc() outputs
5. **Documentation:** Every new parameter fully documented with examples

### Development Approach

```
┌─────────────────────────────────────────────────────────────┐
│                    PHASE 1: Foundation                      │
│  Two-layer, frost, direct material properties              │
│  └─> Enables: Layered regolith, Mars polar science         │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│              PHASE 2: Time & Numerics                       │
│  DELLS, N5, N1/N2 calculation, convergence control         │
│  └─> Enables: Custom resolution, numerical tuning          │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│              PHASE 3: Advanced Features                     │
│  Eclipse, PFlux, TUN8, PhotoFunc, time-varying params      │
│  └─> Enables: Satellite modeling, advanced output          │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│           PHASE 4: Testing & Documentation                  │
│  Comprehensive tests, examples, validation suite            │
│  └─> Production readiness                                   │
└─────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Critical Parameters

**Duration:** 2-3 weeks
**Priority:** HIGH
**Blockers:** None

### Objectives

Enable the most critical missing functionality:
1. Two-layer regolith modeling
2. Frost/condensation support
3. Direct material property specification
4. Advanced atmospheric parameters

### Parameters to Add (15 total)

#### 1.1 Two-Layer Regolith Support

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `thick` | float | Upper layer thickness (m); >0=two-layer, <0=exponential, 0=uniform | 0.0 | **CRITICAL** |
| `IC2` | int | Layer transition depth index | 999 | **CRITICAL** |
| `IIB` | int | Bottom boundary: 1=fixed T, 2=zero flux, 3=geo flux | 2 | HIGH |
| `FLAY` | float | Layer spacing factor | 2.0 | MEDIUM |
| `RLAY` | float | Minimum layer ratio | 1.08 | MEDIUM |

**Implementation Tasks:**
- [ ] Add parameters to `krc()` function signature
- [ ] Add `thick` ↔ `IC2` conversion logic (matching krc.dvrc lines 1240-1260)
- [ ] Implement two-layer material property handling
- [ ] Update input file writer to set IC2, COND2, DENS2, SpHeat2 properly
- [ ] Add validation: `thick > 0` requires `INERTIA2` or `Mat2 != Mat1`

**Code Changes:**
```python
# In core.py krc() function
def krc(
    # ... existing params ...

    # Two-layer parameters
    thick: float = 0.0,
    IC2: Optional[int] = None,
    IIB: int = 2,
    FLAY: float = 2.0,
    RLAY: float = 1.08,

    # ... rest
):
    # Calculate IC2 from thick if not explicitly provided
    if IC2 is None:
        IC2 = _calculate_IC2(thick, N1, FLAY, RLAY)

    # Validate two-layer configuration
    if thick != 0.0:
        if INERTIA2 is None and Mat2 == Mat1 and Por2 == Por1:
            raise ValueError("Two-layer regolith (thick != 0) requires INERTIA2 or different Mat2/Por2")
```

**Testing:**
- [ ] Test uniform material (thick=0) matches current behavior
- [ ] Test two-layer (thick=0.05) with different materials
- [ ] Test exponential profile (thick=-0.1)
- [ ] Validate against davinci krc.dvrc output

#### 1.2 Frost/Condensation Support

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `LVFT` | bool | Variable frost temperature (enable condensation) | False | **CRITICAL** |
| `TFROST` | float | Frost point temperature (K) | 148.0 | **CRITICAL** |
| `CFROST` | float | Clausius-Clapeyron constant C | 3182.48 | HIGH |
| `AFROST` | float | Clausius-Clapeyron constant A | 23.3494 | HIGH |
| `KPREF` | int | Pressure model: 0=const, 1=viking, 2=file | 0 | MEDIUM |

**Implementation Tasks:**
- [ ] Add frost parameters to `krc()` signature
- [ ] Pass `LVFT` flag to KRC input file
- [ ] Calculate frost parameters from body if not provided (Mars CO2, Titan N2, etc.)
- [ ] Add frost validation: LVFT requires PTOTAL > 0
- [ ] Update parser to handle frost output fields

**Code Changes:**
```python
# In core.py
def krc(
    # ... existing ...

    # Frost parameters
    LVFT: bool = False,
    TFROST: Optional[float] = None,
    CFROST: Optional[float] = None,
    AFROST: Optional[float] = None,
    KPREF: int = 0,
):
    # Auto-calculate frost parameters for known bodies
    if LVFT and TFROST is None:
        TFROST, CFROST, AFROST = _get_frost_params(body, PTOTAL)

    # Validate frost configuration
    if LVFT:
        if PTOTAL is None or PTOTAL <= 0:
            raise ValueError("LVFT=True requires PTOTAL > 0")
        if TFROST is None:
            raise ValueError("LVFT=True requires TFROST (or known body for auto-calculation)")
```

**Testing:**
- [ ] Test Mars CO2 frost at south pole (LVFT=True, lat=-85)
- [ ] Test seasonal frost appearance/disappearance
- [ ] Compare with davinci frost output
- [ ] Test Titan N2 frost

#### 1.3 Direct Material Property Specification

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `COND` | float | Thermal conductivity (W/m/K), alternative to INERTIA | None | HIGH |
| `DENSITY` | float | Bulk density (kg/m³), alternative to INERTIA | None | HIGH |
| `SPEC_HEAT` | float | Specific heat (J/kg/K), alternative to INERTIA | None | HIGH |
| `LKofT` | bool | Enable T-dependent thermal properties | True | HIGH |

**Implementation Tasks:**
- [ ] Add optional `COND`, `DENSITY`, `SPEC_HEAT` parameters
- [ ] Implement parameter precedence: explicit COND/DENSITY/SPEC_HEAT overrides INERTIA
- [ ] Add consistency check: INERTIA ≈ sqrt(COND × DENSITY × SPEC_HEAT)
- [ ] Add `LKofT` flag to control temperature-dependent properties
- [ ] Update material property calculation logic

**Code Changes:**
```python
def krc(
    # Material properties (alternative specification methods)
    INERTIA: Optional[float] = None,
    COND: Optional[float] = None,
    DENSITY: Optional[float] = None,
    SPEC_HEAT: Optional[float] = None,
    LKofT: bool = True,
    # ...
):
    # Determine material properties
    if COND is not None and DENSITY is not None and SPEC_HEAT is not None:
        # Direct specification
        upper_props = {
            "COND": COND,
            "DENSITY": DENSITY,
            "SPEC_HEAT": SPEC_HEAT,
        }
        # Calculate INERTIA for reference
        INERTIA = np.sqrt(COND * DENSITY * SPEC_HEAT)
    elif INERTIA is not None:
        # Calculate from INERTIA (existing behavior)
        upper_props = calculate_thermal_properties(Mat1, INERTIA, T_user, k_style)
    else:
        raise ValueError("Must specify either INERTIA or (COND, DENSITY, SPEC_HEAT)")

    # Generate polynomial coefficients only if LKofT=True
    if LKofT:
        upper_props.update(generate_kT_coefficients(Mat1, k_style, T_user))
```

#### 1.4 Advanced Atmospheric Parameters

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `DUSTA` | float | Dust absorptivity | 0.9 | MEDIUM |
| `TAURAT` | float | Optical depth ratio (vis/IR) | 2.0 | MEDIUM |
| `FANON` | float | Atmospheric anisotropy factor | 0.3 | MEDIUM |

**Implementation Tasks:**
- [ ] Add advanced atmospheric parameters
- [ ] Set body-specific defaults (Mars: DUSTA=0.9, TAURAT=2.0)
- [ ] Pass through to KRC input file

### Phase 1 Deliverables

- [ ] Updated `core.py` with 15 new parameters
- [ ] Two-layer regolith calculation module
- [ ] Frost parameter auto-calculation
- [ ] Material property precedence logic
- [ ] Unit tests for all new parameters
- [ ] Example notebooks:
  - `example_two_layer_regolith.ipynb`
  - `example_mars_polar_frost.ipynb`
  - `example_direct_properties.ipynb`

### Phase 1 Success Criteria

1. ✅ Two-layer regolith runs match davinci output
2. ✅ Mars polar frost modeling produces seasonal cycles
3. ✅ Direct COND/DENSITY/SPEC_HEAT specification works
4. ✅ All existing tests pass (backward compatibility)
5. ✅ Documentation complete for all new parameters

---

## Phase 2: Time & Numerical Control

**Duration:** 2-3 weeks
**Priority:** HIGH
**Blockers:** Phase 1 complete

### Objectives

1. Make time control user-configurable
2. Implement automatic N1/N2 calculation
3. Add convergence control parameters
4. Enable output filtering at execution time

### Parameters to Add (12 total)

#### 2.1 Time Resolution Control

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `DELLS` | float | Season spacing (degrees Ls) | 1.0 | **CRITICAL** |
| `N5` | int | Total seasons to run | Auto | **CRITICAL** |
| `JDISK` | int | Starting season for output | Auto | **CRITICAL** |
| `DELJUL` | float | Season spacing (days) | Auto | HIGH |
| `DJUL` | float | Starting Julian date | 0.0 | MEDIUM |
| `LKEY` | int | Time input: 0=Ls, 1=JD, 2=GD | 0 | MEDIUM |
| `JD` | float | Julian date (if LKEY=1) | None | LOW |
| `GD` | float | Gregorian date (if LKEY=2) | None | LOW |

**Implementation Tasks:**
- [ ] Make `DELLS` user-settable (remove hardcode)
- [ ] Auto-calculate `N5` and `JDISK` from `DELLS` and spinup time
- [ ] Allow user override of `N5`, `JDISK`
- [ ] Implement `DELJUL` calculation from `DELLS` and `PERIOD`
- [ ] Add `LKEY` support for JD/GD input modes
- [ ] Update time axis in output

**Code Changes:**
```python
def krc(
    # ... existing ...

    # Time control
    DELLS: float = 1.0,           # Degrees Ls per output
    N5: Optional[int] = None,      # Total seasons (auto: 3 years)
    JDISK: Optional[int] = None,   # Start output season (auto: 2 years)
    DELJUL: Optional[float] = None,  # Days per season (auto-calc)
    DJUL: float = 0.0,
    LKEY: int = 0,
    JD: Optional[float] = None,
    GD: Optional[float] = None,

    # Spinup control
    spinup_years: float = 2.0,
    output_years: float = 1.0,
):
    # Calculate DELJUL from DELLS and PERIOD
    if DELJUL is None:
        DELJUL = PERIOD * DELLS / 360.0

    # Auto-calculate N5 and JDISK
    if N5 is None:
        total_years = spinup_years + output_years
        N5 = int(np.ceil(360.0 / DELLS * total_years))

    if JDISK is None:
        JDISK = int(np.ceil(360.0 / DELLS * spinup_years + 1))

    # Validate time parameters
    if N5 <= JDISK:
        raise ValueError(f"N5 ({N5}) must be > JDISK ({JDISK})")
```

**Testing:**
- [ ] Test high-resolution (DELLS=0.1) output
- [ ] Test coarse resolution (DELLS=10.0)
- [ ] Test long runs (N5=10000, many years)
- [ ] Test short spinup (spinup_years=0.5)

#### 2.2 Automatic Layer/Timestep Calculation

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `N1` | int | Number of layers (auto-calculated or user override) | Auto | **CRITICAL** |
| `N2` | int | Timesteps per day (auto-calculated or user override) | Auto | **CRITICAL** |
| `N3` | int | Days per convergence check | 10 | HIGH |
| `NRSET` | int | Maximum convergence iterations | 0 | MEDIUM |

**Implementation Tasks:**
- [ ] Implement `krc_evalN1()` matching davinci (krc.dvrc lines 2769-2826)
- [ ] Implement `krc_evalN2()` matching davinci (krc.dvrc lines 2827-2866)
- [ ] Calculate N1 from INERTIA, PERIOD, DELLS (seasonal skin depth)
- [ ] Calculate N2 from PERIOD, N1 (Courant stability)
- [ ] Allow user override of N1, N2
- [ ] Warn if manual N1/N2 violates stability

**Code Changes:**
```python
# New file: pykrc/numerical.py

def krc_evalN1(INERTIA: float, PERIOD: float, DELLS: float,
               MAXN1: int = 100) -> int:
    """
    Calculate optimal number of layers.

    Based on seasonal skin depth: z_skin = sqrt(κ * P * DELLS/360)
    where κ = INERTIA² / (DENSITY * SPEC_HEAT)

    Matches krc.dvrc lines 2769-2826.
    """
    # Estimate thermal diffusivity
    kappa = (INERTIA ** 2) / (1200.0 * 800.0)  # Typical regolith values

    # Seasonal skin depth (meters)
    z_skin = np.sqrt(kappa * PERIOD * 86400.0 * DELLS / 360.0)

    # Total depth should be ~4× skin depth
    total_depth = 4.0 * z_skin

    # Layer spacing with FLAY=2.0, RLAY=1.08
    N1 = int(np.ceil(np.log(total_depth / 0.001) / np.log(1.5)))

    # Clamp to reasonable range
    N1 = max(20, min(N1, MAXN1))

    return N1


def krc_evalN2(PERIOD: float, N1: int, MAXN2: int = 1000) -> int:
    """
    Calculate optimal timesteps per day for numerical stability.

    Courant condition: dt <= dz² / (2 * κ)

    Matches krc.dvrc lines 2827-2866.
    """
    # Estimate smallest layer thickness (top layer)
    dz_min = 0.001  # meters

    # Estimate thermal diffusivity (worst case: high INERTIA)
    kappa_max = (2000.0 ** 2) / (1200.0 * 800.0)  # High TI regolith

    # Courant stability condition
    dt_max = (dz_min ** 2) / (2.0 * kappa_max)  # seconds

    # Timesteps per day
    N2 = int(np.ceil(86400.0 / dt_max))

    # Increase by safety factor
    N2 = int(N2 * 1.5)

    # Clamp to reasonable range
    N2 = max(96, min(N2, MAXN2))

    # Round to multiple of 96 for cleaner output
    N2 = ((N2 + 95) // 96) * 96

    return N2


# In core.py
def krc(
    # ... existing ...

    # Numerical control
    N1: Optional[int] = None,
    N2: Optional[int] = None,
    N3: int = 10,
    NRSET: int = 0,
    MAXN1: int = 100,
    MAXN2: int = 1000,
):
    # Auto-calculate N1, N2 if not provided
    if N1 is None:
        N1 = krc_evalN1(INERTIA, PERIOD, DELLS, MAXN1)
        if verbose:
            print(f"Auto-calculated N1={N1} layers")

    if N2 is None:
        N2 = krc_evalN2(PERIOD, N1, MAXN2)
        if verbose:
            print(f"Auto-calculated N2={N2} timesteps/day")

    # Stability warning
    if N1 is not None and N2 is not None:
        check_stability(N1, N2, INERTIA, PERIOD)
```

**Testing:**
- [ ] Test auto N1/N2 for Mars (INERTIA=200)
- [ ] Test auto N1/N2 for extreme INERTIA (50, 2000)
- [ ] Test manual N1/N2 override
- [ ] Verify stability warnings trigger correctly
- [ ] Compare auto N1/N2 with davinci values

#### 2.3 Convergence Control

| Parameter | Type | Description | Default | Priority |
|-----------|------|-------------|---------|----------|
| `GGT` | float | Convergence relaxation parameter | 1.0 | HIGH |
| `TPREDICT` | float | Temperature prediction mode | 0.0 | MEDIUM |

**Implementation Tasks:**
- [ ] Add convergence parameters to input file
- [ ] Set intelligent defaults based on DELLS and N1
- [ ] Document convergence tuning guidelines

### Phase 2 Deliverables

- [ ] `pykrc/numerical.py` module with N1/N2 calculation
- [ ] User-configurable time resolution
- [ ] Automatic stability checking
- [ ] Updated examples showing:
  - High-resolution runs (DELLS=0.1)
  - Custom N1/N2 tuning
  - Convergence optimization
- [ ] Performance benchmarks (various DELLS, N1, N2)

### Phase 2 Success Criteria

1. ✅ Auto N1/N2 matches davinci calculations within 10%
2. ✅ Custom DELLS runs produce correct time axes
3. ✅ Stability warnings prevent numerical crashes
4. ✅ High-resolution runs (DELLS=0.01) complete successfully
5. ✅ Documentation includes convergence tuning guide

---

## Phase 3: Advanced Features

**Duration:** 1-2 months
**Priority:** MEDIUM
**Blockers:** Phase 2 complete

### Objectives

1. Temperature-depth profile output
2. Eclipse modeling (satellites)
3. Planetary flux calculations
4. Photometric functions
5. Multiple latitude support
6. Time-varying parameters

### 3.1 Temperature-Depth Output

| Parameter | Type | Description | Priority |
|-----------|------|-------------|----------|
| `TUN8` | int | Depth profile output control (I15=TUN8) | **CRITICAL** |
| `depth_indices` | list[int] | Layer indices for output | HIGH |

**Implementation:**
- [ ] Add `TUN8` parameter (e.g., 101 for all layers)
- [ ] Parse additional depth data from bin52 output
- [ ] Add `result['depth']` and `result['temp_profile']` to output
- [ ] Create visualization helper for depth-time plots

**Testing:**
- [ ] Test TUN8=101 (all layers) output
- [ ] Test TUN8=15 (every 15th layer)
- [ ] Visualize diurnal temperature wave penetration

### 3.2 Eclipse Modeling

| Parameter | Type | Description | Priority |
|-----------|------|-------------|----------|
| `Eclipse` | bool | Enable eclipse calculation | HIGH |
| `Eclipse_Style` | int | 1=Phobos, 2=Deimos, 3=custom | HIGH |
| `Eclipser` | str | Name of eclipsing body | HIGH |
| Plus 15 more eclipse parameters... | | | MEDIUM |

**Implementation:**
- [ ] Add eclipse parameter group
- [ ] Integrate with KRC eclipse routines
- [ ] Add eclipse timing output
- [ ] Examples for Phobos, Deimos thermal modeling

### 3.3 Planetary Flux

| Parameter | Type | Description | Priority |
|-----------|------|-------------|----------|
| `PFlux` | bool | Enable planetary thermal flux | HIGH |
| `BT_Avg`, `BT_Min`, `BT_Max` | float | Planet brightness temps | HIGH |
| Plus 8 more PFlux parameters... | | | MEDIUM |

**Implementation:**
- [ ] Add planetary flux parameters
- [ ] Calculate view factors to parent planet
- [ ] Add thermal flux contribution to energy balance
- [ ] Example: Phobos with Mars thermal emission

### 3.4 Photometric Functions

| Parameter | Type | Description | Priority |
|-----------|------|-------------|----------|
| `PhotoFunc` | float | Non-Lambert photometry (0=Lambert, 0.6=Lunar) | MEDIUM |

**Implementation:**
- [ ] Add PhotoFunc parameter
- [ ] Pass to KRC for non-Lambertian albedo
- [ ] Example: Lunar highlands (PhotoFunc=0.6)

### 3.5 Multiple Latitudes

| Parameter | Type | Description | Priority |
|-----------|------|-------------|----------|
| `N4` | int | Number of latitudes | HIGH |
| `lat` | float or list[float] | Latitude(s) | HIGH |
| `ELEV` | float or list[float] | Elevation(s) | HIGH |

**Implementation:**
- [ ] Accept `lat` as scalar or list
- [ ] Auto-set `N4` from len(lat)
- [ ] Parse multi-latitude bin52 output
- [ ] Return list of results (one per latitude)

**Testing:**
- [ ] Test N4=10 latitudes [-90:10:90]
- [ ] Compare single-lat vs multi-lat outputs

### 3.6 Time-Varying Parameters

| Parameter | Type | Description | Priority |
|-----------|------|-------------|----------|
| `ALBEDO` | float or array | Constant or time-varying albedo | MEDIUM |
| `TAUD` | float or array | Constant or time-varying opacity | MEDIUM |

**Implementation:**
- [ ] Accept scalar or array for ALBEDO, TAUD
- [ ] Write time-series to KRC input format
- [ ] Example: Seasonal Mars dust cycle

### Phase 3 Deliverables

- [ ] TUN8 depth profile output
- [ ] Eclipse modeling system
- [ ] Planetary flux calculations
- [ ] Multi-latitude support
- [ ] Time-varying parameter arrays
- [ ] Advanced examples:
  - Phobos thermal model with eclipses
  - Mars seasonal dust cycle
  - Temperature-depth animations

### Phase 3 Success Criteria

1. ✅ TUN8 output matches davinci depth profiles
2. ✅ Eclipse timings match analytical calculations
3. ✅ Planetary flux contributions correct
4. ✅ Multi-latitude runs efficient (vectorized)
5. ✅ Time-varying parameters work correctly

---

## Phase 4: Testing & Validation

**Duration:** 2-3 weeks
**Priority:** CRITICAL
**Blockers:** Phases 1-3 complete

### 4.1 Unit Tests

Create comprehensive unit test suite:

```python
# tests/test_parameters.py
def test_two_layer_regolith():
    """Test two-layer configuration."""
    result = krc(lat=0, lon=0, body="Mars",
                 INERTIA=100, INERTIA2=300, thick=0.05)
    assert result['success']
    # Verify output shows layer transition

def test_frost_condensation():
    """Test Mars polar frost."""
    result = krc(lat=-85, lon=0, body="Mars", ls=270,
                 LVFT=True, INERTIA=100)
    assert 'frost' in result['anc']
    # Verify frost appears in southern winter

def test_auto_N1_N2():
    """Test automatic layer/timestep calculation."""
    result = krc(lat=0, lon=0, INERTIA=200)
    N1 = result['config']['N1']
    N2 = result['config']['N2']
    assert 20 <= N1 <= 100
    assert 96 <= N2 <= 1000

def test_custom_time_resolution():
    """Test custom DELLS."""
    result = krc(lat=0, lon=0, DELLS=0.1)
    # Verify output has 3600 time points (360°/0.1°)
    assert len(result['ls']) == 3600
```

### 4.2 Integration Tests

Compare with davinci krc.dvrc outputs:

```python
# tests/test_vs_davinci.py
def test_match_davinci_simple():
    """Compare simple run with davinci."""
    # Run pykrc
    py_result = krc(lat=0, lon=0, body="Mars",
                    INERTIA=200, ALBEDO=0.25, TAUD=0.3)

    # Load davinci reference
    dv_result = load_davinci_reference("mars_eq_i200_a025_t03.pkl")

    # Compare outputs
    assert np.allclose(py_result['surf'], dv_result['surf'], rtol=0.01)

def test_match_davinci_two_layer():
    """Compare two-layer with davinci."""
    # ... similar ...
```

### 4.3 Regression Tests

Ensure backward compatibility:

```python
# tests/test_regression.py
def test_existing_code_still_works():
    """All old examples must still work."""
    result = krc(lat=0, lon=0)  # Minimal call
    assert result['success']

    result = krc(lat=0, lon=0, INERTIA=200, ALBEDO=0.25)
    assert result['success']
```

### 4.4 Performance Tests

Benchmark execution time:

```python
# tests/test_performance.py
def test_speed_simple_run():
    """Simple run should complete in < 10 seconds."""
    import time
    start = time.time()
    result = krc(lat=0, lon=0)
    elapsed = time.time() - start
    assert elapsed < 10.0

def test_speed_high_resolution():
    """High-res run (DELLS=0.01) in < 60 seconds."""
    # ... test ...
```

### 4.5 Documentation

- [ ] Update all docstrings
- [ ] API reference for every parameter
- [ ] Tutorial notebooks:
  - Getting started
  - Two-layer regolith
  - Mars polar frost
  - Custom time resolution
  - Eclipse modeling
  - Multi-latitude runs
- [ ] Migration guide from old pykrc API
- [ ] Comparison with davinci krc.dvrc

### Phase 4 Deliverables

- [ ] 100+ unit tests (>90% coverage)
- [ ] 20+ integration tests vs davinci
- [ ] Performance benchmarks
- [ ] Complete API documentation
- [ ] 6+ tutorial notebooks
- [ ] CI/CD pipeline (GitHub Actions)

### Phase 4 Success Criteria

1. ✅ All tests pass
2. ✅ >90% code coverage
3. ✅ Output matches davinci within 1% RMS
4. ✅ Performance benchmarks documented
5. ✅ Documentation complete and reviewed

---

## Architecture Changes

### Current Architecture

```
krc() function in core.py
├─> parse_master_inp() → params dict
├─> porb() → orbital params
├─> calculate_thermal_properties() → material props
├─> KRCExecutor.run_krc() → execute
└─> parse_bin52() → output dict
```

### Enhanced Architecture

```
krc() function in core.py
├─> parse_master_inp() → base params
├─> porb() → orbital params
├─> validate_parameters() → check consistency ⭐NEW
│
├─> IF two-layer:
│   ├─> calculate_IC2() ⭐NEW
│   └─> two_layer_properties() ⭐NEW
│
├─> IF frost:
│   └─> calculate_frost_params() ⭐NEW
│
├─> IF N1/N2 = None:
│   ├─> krc_evalN1() ⭐NEW
│   └─> krc_evalN2() ⭐NEW
│
├─> calculate_thermal_properties()
├─> build_parameter_dict() ⭐ENHANCED
├─> KRCExecutor.create_input_file() ⭐ENHANCED
├─> KRCExecutor.run_krc()
│
└─> parse_bin52() ⭐ENHANCED
    ├─> IF TUN8: parse depth profiles ⭐NEW
    ├─> IF Eclipse: parse eclipse times ⭐NEW
    └─> IF N4 > 1: parse multi-lat ⭐NEW
```

### New Modules

```
pykrc/
├── core.py                  # Main krc() function [ENHANCED]
├── config.py                # Configuration [UNCHANGED]
├── input_processor.py       # Input parsing [ENHANCED]
├── materials.py             # Material properties [ENHANCED]
├── orbital.py               # Orbital mechanics [UNCHANGED]
├── executor.py              # KRC execution [ENHANCED]
├── bin_parser.py            # Output parsing [ENHANCED]
├── data_loaders.py          # Data loading [UNCHANGED]
│
├── numerical.py             # ⭐NEW: N1/N2 calculation
├── frost.py                 # ⭐NEW: Frost/condensation
├── layers.py                # ⭐NEW: Two-layer logic
├── validation.py            # ⭐NEW: Parameter validation
├── eclipse.py               # ⭐NEW: Eclipse modeling
└── planetary_flux.py        # ⭐NEW: Planetary thermal flux
```

---

## API Design

### Backward Compatibility

All existing code continues to work:

```python
# These must all still work EXACTLY as before
result = krc(lat=0, lon=0)
result = krc(lat=0, lon=0, body="Mars")
result = krc(lat=0, lon=0, INERTIA=200, ALBEDO=0.25)
```

### New Parameter Groups

Organize parameters into logical groups:

```python
def krc(
    # ========== LOCATION & BODY ==========
    lat: float | list[float],
    lon: float = 0.0,
    body: str = "Mars",
    ELEV: float | list[float] | None = None,

    # ========== TIME CONTROL ==========
    ls: Optional[float] = None,
    hour: Optional[float] = None,
    DELLS: float = 1.0,
    N5: Optional[int] = None,
    JDISK: Optional[int] = None,
    spinup_years: float = 2.0,
    output_years: float = 1.0,

    # ========== MATERIAL PROPERTIES ==========
    # Method 1: Thermal inertia
    INERTIA: Optional[float] = None,
    k_style: str = "Mars",
    Mat1: str = "basalt",
    Por1: Optional[float] = None,
    T_user: float = 220.0,

    # Method 2: Direct specification
    COND: Optional[float] = None,
    DENSITY: Optional[float] = None,
    SPEC_HEAT: Optional[float] = None,
    LKofT: bool = True,

    # ========== TWO-LAYER REGOLITH ==========
    thick: float = 0.0,
    INERTIA2: Optional[float] = None,
    Mat2: str = "basalt",
    Por2: Optional[float] = None,
    IC2: Optional[int] = None,

    # ========== SURFACE PROPERTIES ==========
    ALBEDO: float | np.ndarray = 0.25,
    EMISS: float = 1.0,
    SLOPE: float = 0.0,
    SLOAZI: float = 90.0,
    PhotoFunc: float = 0.0,

    # ========== ATMOSPHERE ==========
    TAUD: float | np.ndarray | None = None,
    PTOTAL: Optional[float] = None,
    TATM: Optional[float] = None,
    DUSTA: float = 0.9,
    TAURAT: float = 2.0,
    FANON: float = 0.3,

    # ========== FROST/CONDENSATION ==========
    LVFT: bool = False,
    TFROST: Optional[float] = None,
    CFROST: Optional[float] = None,
    AFROST: Optional[float] = None,
    KPREF: int = 0,

    # ========== NUMERICAL CONTROL ==========
    N1: Optional[int] = None,
    N2: Optional[int] = None,
    N3: int = 10,
    NRSET: int = 0,
    GGT: float = 1.0,
    FLAY: float = 2.0,
    RLAY: float = 1.08,
    IIB: int = 2,
    MAXN1: int = 100,
    MAXN2: int = 1000,

    # ========== OUTPUT CONTROL ==========
    TUN8: int = 0,
    LMST: bool = False,
    N4: Optional[int] = None,

    # ========== ECLIPSE MODELING ==========
    Eclipse: bool = False,
    Eclipse_Style: int = 1,
    Eclipser: str = "Phobos",
    # ... other eclipse params ...

    # ========== PLANETARY FLUX ==========
    PFlux: bool = False,
    BT_Avg: float = 210.0,
    # ... other pflux params ...

    # ========== EXECUTION OPTIONS ==========
    TDEEP: float = 180.0,
    verbose: bool = False,
    workdir: Optional[str] = None,
    keep_files: bool = False,

    # ========== ADVANCED ==========
    **kwargs
) -> Dict[str, Any]:
    """Run KRC thermal model with full parameter control."""
    pass
```

### Parameter Validation

```python
# In pykrc/validation.py

def validate_krc_parameters(params: dict) -> None:
    """Validate parameter consistency and bounds."""

    # Two-layer validation
    if params['thick'] != 0.0:
        if params['INERTIA2'] is None and params['Mat2'] == params['Mat1']:
            raise ValueError("Two-layer requires INERTIA2 or different Mat2")

    # Frost validation
    if params['LVFT']:
        if params['PTOTAL'] is None or params['PTOTAL'] <= 0:
            raise ValueError("LVFT requires PTOTAL > 0")

    # Time validation
    if params['N5'] <= params['JDISK']:
        raise ValueError("N5 must be > JDISK")

    # Numerical stability
    if params['N1'] is not None and params['N2'] is not None:
        check_stability(params['N1'], params['N2'], params['INERTIA'])

    # Bounds checking
    if not -90 <= params['lat'] <= 90:
        raise ValueError("lat must be in [-90, 90]")

    # ... many more checks ...
```

---

## Backward Compatibility

### Strategy

1. **All existing calls work unchanged**
2. **New parameters have sensible defaults**
3. **Deprecation warnings for future API changes**
4. **Version checking for serialized outputs**

### Migration Path

For users upgrading from old pykrc:

```python
# OLD API (still works)
result = krc(lat=0, lon=0, body="Mars", INERTIA=200)

# NEW API (more options)
result = krc(lat=0, lon=0, body="Mars", INERTIA=200,
             DELLS=0.1,  # Higher resolution
             thick=0.05,  # Two-layer
             LVFT=True)   # Frost modeling
```

### Output Structure Preservation

```python
# Existing output fields preserved
result['surf']  # Still exists
result['bol']   # Still exists
result['time']  # Still exists

# New optional fields
result['depth']  # Only if TUN8 > 0
result['frost']  # Only if LVFT=True
result['eclipse']  # Only if Eclipse=True
```

---

## Testing Strategy

### Test Pyramid

```
                  ┌─────────────┐
                  │   Manual    │  5%  (Visual inspection, plots)
                  └─────────────┘
              ┌───────────────────┐
              │   Integration     │  15% (vs davinci)
              └───────────────────┘
          ┌─────────────────────────┐
          │      Functional         │  30% (Feature tests)
          └─────────────────────────┘
      ┌───────────────────────────────┐
      │          Unit                 │  50% (Parameter validation)
      └───────────────────────────────┘
```

### Continuous Integration

```yaml
# .github/workflows/test.yml
name: Test PyKRC

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.10'

      - name: Install dependencies
        run: |
          pip install -e .
          pip install pytest pytest-cov

      - name: Compile KRC
        run: |
          cd src
          make

      - name: Run unit tests
        run: pytest tests/unit/ -v --cov=pykrc

      - name: Run integration tests
        run: pytest tests/integration/ -v

      - name: Check coverage
        run: pytest --cov=pykrc --cov-report=html --cov-fail-under=90
```

---

## Risk Assessment

### High Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing code | HIGH | Comprehensive regression tests |
| Numerical instability with new params | HIGH | Auto N1/N2 calculation, stability checks |
| Inconsistent with davinci | HIGH | Extensive comparison tests |

### Medium Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| Performance degradation | MEDIUM | Benchmarking, profiling |
| Parameter explosion (too many options) | MEDIUM | Logical grouping, good defaults |
| Documentation lag | MEDIUM | Write docs alongside code |

### Low Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| Platform compatibility | LOW | CI testing on Linux/Mac/Windows |
| Dependency version conflicts | LOW | Pin versions in setup.py |

---

## Success Metrics

### Quantitative

- ✅ **95%+ parameter coverage** (80+/85 parameters)
- ✅ **>90% test coverage** (pytest-cov)
- ✅ **<1% RMS difference** vs davinci outputs
- ✅ **<10% performance overhead** vs current pykrc
- ✅ **100% backward compatibility** (all old examples work)

### Qualitative

- ✅ **Production-ready:** Used by science teams for publications
- ✅ **Well-documented:** New users can start in <30 minutes
- ✅ **Maintainable:** Other developers can contribute
- ✅ **Extensible:** Easy to add new parameters in future

---

## Timeline Summary

| Phase | Duration | End Date (from 2025-10-08) |
|-------|----------|----------------------------|
| Phase 1: Critical Parameters | 2-3 weeks | ~2025-10-29 |
| Phase 2: Time & Numerics | 2-3 weeks | ~2025-11-19 |
| Phase 3: Advanced Features | 1-2 months | ~2026-01-19 |
| Phase 4: Testing & Validation | 2-3 weeks | ~2026-02-09 |
| **TOTAL** | **3-4 months** | **~2026-02-09** |

---

## Next Steps

### Immediate Actions (This Week)

1. **Review this plan** with team/stakeholders
2. **Create GitHub issues** for each phase/task
3. **Set up project board** (Kanban: To Do, In Progress, Done)
4. **Create feature branch** (`feature/parameter-expansion`)
5. **Write first tests** (test-driven development)

### Phase 1 Kickoff (Next Week)

1. **Implement thick parameter** (simplest critical feature)
2. **Add IC2 calculation** from thick
3. **Write tests** for two-layer regolith
4. **Update documentation** for thick parameter
5. **Get feedback** from early users

---

## Conclusion

This plan provides a **systematic, phased approach** to achieving feature parity between pykrc and krc.dvrc. By prioritizing critical missing parameters (two-layer, frost) in Phase 1, we deliver immediate value while laying groundwork for more advanced features.

The **3-4 month timeline** is realistic given:
- Incremental development (phases can be deployed independently)
- Extensive existing codebase to build on
- Clear reference implementation (krc.dvrc)
- Comprehensive testing strategy

Upon completion, pykrc will be a **production-ready Python interface** to KRC, suitable for scientific publications and capable of matching all davinci krc.dvrc functionality.

---

**END OF PLAN**
