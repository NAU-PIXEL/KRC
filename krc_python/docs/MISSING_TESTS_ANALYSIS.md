# Missing Tests Analysis for PyKRC Integration Suite

**Date:** 2025-10-23
**Source:** Analysis of `krc_davinci/test_KRC.dv`
**Current Coverage:** 58 tests, 63.8% pass rate

---

## Executive Summary

After analyzing the Davinci KRC test file (`krc_davinci/test_KRC.dv`), we identified **11 major test categories** that are either missing or underrepresented in our current PyKRC integration test suite. This document prioritizes these gaps and provides implementation guidance.

---

## Current Test Coverage (What We Have) ✓

Our existing 58 tests cover:
- Basic Mars runs (lat/lon/ls)
- PORB tests (Mars, Phobos, Europa, Bennu, Ceres)
- Date conversions (GD, JD, DJUL, Ls)
- Material properties (k_style, basalt)
- Two-layer regolith (thick > 0)
- Eclipse modes
- PFlux (planetary flux)
- T→TI inversion (point mode) for various bodies
- lbound (lower boundary conditions)
- N1 variations (20 to 999 layers)

---

## Priority 1: HIGH PRIORITY (Core Functionality)

These tests exercise fundamental capabilities that users will rely on frequently.

### 1. Direct Material Property Override
**Lines in test_KRC.dv:** 738-763
**Current Status:** ❌ Not tested

**Description:**
Users should be able to directly specify thermal conductivity and heat capacity polynomial coefficients, bypassing the material database.

**Test Case:**
```python
def test_direct_material_property_override(self, validator, tolerance, keep_files):
    """Test direct specification of ConUp/SphUp polynomial coefficients."""
    result = validator.compare_run(
        pykrc_params={
            "lat": 2.0,
            "body": "Europa",
            "LKofT": "F",
            "DENSITY": 940.0,
            "ConUp0": 0.111249,
            "ConUp1": 0.003732,
            "ConUp2": 0.0390182,
            "ConUp3": -0.031618,
            "SphUp0": 1720.67,
            "SphUp1": 694.32,
            "SphUp2": 28.09,
            "SphUp3": 34.23,
            "KEEP": "T"
        },
        davinci_cmd='krc(lat=2.,body="Europa",LKofT="F",DENSITY=940.,...)',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="direct_material_override"
    )
```

**Why Important:**
- Advanced users need precise control over material properties
- Critical for matching lab measurements or mission data
- Tests that user-specified coefficients correctly override defaults

**Implementation Priority:** 🔴 HIGH
**Estimated Effort:** 2-3 hours

---

### 2. Time-Varying Opacity (TAUD Tables)
**Lines in test_KRC.dv:** 660-663
**Current Status:** ❌ Not tested

**Description:**
Atmospheric opacity should be able to vary with season (Ls) to model dust storms and seasonal atmospheric changes.

**Test Case:**
```python
def test_taud_seasonal_variation(self, validator, tolerance, keep_files):
    """Test TAUD varying with Ls for seasonal dust storms."""
    # TAUD table: [Ls_values, TAU_values] pairs
    result = validator.compare_run(
        pykrc_params={
            "lat": 0.0,
            "TAUD": [[1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330],
                     [0.3, 0.4, 0.6, 0.9, 0.81, 0.83, 1.0, 0.6, 0.4, 0.2, 0.3, 0.5]],
            "KEEP": "T"
        },
        davinci_cmd='krc(lat=0.,TAUD=TAUD,KEEP="T")',  # Requires TAUD array setup
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="taud_seasonal"
    )
```

**Why Important:**
- Essential for Mars regional/global circulation model (GCM) coupling
- Models dust storm effects on surface temperatures
- Common use case for Mars atmospheric studies

**Implementation Priority:** 🔴 HIGH
**Estimated Effort:** 4-5 hours (requires array input handling)

---

### 3. High-Inertia Edge Cases (>1400)
**Lines in test_KRC.dv:** 167-171, 185-189
**Current Status:** ⚠️ Partially tested (we test up to 250)

**Description:**
Document and test known systematic errors in T→TI inversion at very high thermal inertia values (>1400 J m⁻² K⁻¹ s⁻½).

**Test Cases Needed:**
```python
def test_high_ti_inversion_mars_1400(self):
    """Test T→TI at INERTIA=1400 (expect ~1% error)."""
    # Per Davinci test_KRC.dv line 167: Expected TI=1397 (1% off)

def test_high_ti_inversion_mars_2100(self):
    """Test T→TI at INERTIA=2100 (expect ~1% error)."""
    # Per Davinci test_KRC.dv line 170: Expected TI=2072 (1% off)
```

**Why Important:**
- Documents known limitations for users
- Validates that errors are consistent with Davinci
- High-TI materials (bedrock) are scientifically important

**Implementation Priority:** 🔴 HIGH
**Estimated Effort:** 2 hours

---

## Priority 2: MEDIUM PRIORITY (Extended Features)

These tests cover important but less frequently used features.

### 4. Custom IR/VIS Flux Tables
**Lines in test_KRC.dv:** 637-656
**Current Status:** ⚠️ Basic PFlux tested, not time-varying arrays

**Description:**
Allow users to specify time-varying infrared and visible flux arrays for planetary heating calculations.

**Test Case:**
```python
def test_custom_flux_tables_europa(self, validator, tolerance, keep_files):
    """Test custom IR/VIS flux arrays for Europa."""
    # LTST array (24 hours)
    # IR flux array [W/m²] at each hour
    # VIS flux array [W/m²] at each hour
    result = validator.compare_run(
        pykrc_params={
            "lat": 0.0,
            "INERTIA": 70.0,
            "body": "Europa",
            "ALBEDO": 0.55,
            "PFlux": "T",
            "IR": [[0,1,2,...,23], [IR_values]],    # 2xN array
            "Vis": [[0,1,2,...,23], [VIS_values]],  # 2xN array
            "KEEP": "T"
        },
        davinci_cmd='krc(lat=0.,INERTIA=70.,body="Europa",...,IR=IR,Vis=VIS)',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="custom_flux_europa"
    )
```

**Why Important:**
- Advanced PFlux modeling for satellites
- Tests Jupiter/Saturn heating effects on moons
- Enables custom heating scenarios

**Implementation Priority:** 🟡 MEDIUM
**Estimated Effort:** 5-6 hours (complex array handling)

---

### 5. Atmospheric Parameters (Non-Mars)
**Lines in test_KRC.dv:** 731-735
**Current Status:** ⚠️ Mars atmosphere tested, not other bodies

**Description:**
Test atmospheric parameters (TAURAT, DUSTA, ARC2_G0) for non-Mars bodies like Titan, Triton, or Pluto.

**Test Case:**
```python
def test_atmosphere_bennu_synthetic(self, validator, tolerance, keep_files):
    """Test adding synthetic atmosphere to Bennu (for validation)."""
    result = validator.compare_run(
        pykrc_params={
            "lat": 0.0,
            "body": "Bennu",
            "INERTIA": 200.0,
            "PTOTAL": 1.01,      # Triggers atmosphere (>1.0)
            "TAURAT": 0.21,
            "DUSTA": 0.99,
            "TAUD": 0.01,
            "LKofT": "F",
            "KEEP": "T"
        },
        davinci_cmd='krc(lat=0.,body="Bennu",PTOTAL=1.01,TAURAT=0.21,...)',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="atmosphere_bennu"
    )
```

**Why Important:**
- Validates atmosphere code works for any body
- Useful for exoplanet/theoretical studies
- Tests PTOTAL > 1.0 triggering logic

**Implementation Priority:** 🟡 MEDIUM
**Estimated Effort:** 3 hours

---

### 6. Additional Solar System Bodies
**Lines in test_KRC.dv:** 666-676
**Current Status:** ❌ Not tested (if PORB files exist)

**Description:**
Test other solar system satellites if PORB files are available.

**Bodies to Test (if PORB files exist):**
- Proteus (Neptune satellite)
- Titania (Uranus satellite)
- Triton (Neptune satellite) - marked with `###ISSUE` in test file
- Phoebe (Saturn satellite)

**Test Case (Example):**
```python
def test_proteus_neptune_satellite(self, validator, tolerance, keep_files):
    """Test Proteus (Neptune satellite) if PORB available."""
    result = validator.compare_run(
        pykrc_params={
            "lat": 12.0,
            "lon": 9.34,
            "body": "Proteus",
            "INERTIA": 5.0,
            "ALBEDO": 0.096,
            "LKofT": "F",
            "KEEP": "T"
        },
        davinci_cmd='krc(body="Proteus",lat=12.,lon=9.34,INERTIA=5.,ALBEDO=0.096,LKofT="F")',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="proteus_default"
    )
```

**Why Important:**
- Expands body coverage
- Validates outer solar system support
- Useful for mission planning (Uranus/Neptune missions)

**Implementation Priority:** 🟡 MEDIUM (conditional on PORB file availability)
**Estimated Effort:** 2 hours per body

---

## Priority 3: LOW PRIORITY (Advanced/Rarely Used)

These features are important but less commonly used.

### 7. Exoplanet/Custom Body Support
**Lines in test_KRC.dv:** 13-23, 870-884
**Current Status:** ⚠️ Generic PORB tested but skipped

**Description:**
Test custom orbital parameters via `generic_porb()` and exoplanet wrapper via `exo_porb()`.

**Test Case:**
```python
def test_generic_porb_custom_orbit(self, validator, tolerance, keep_files):
    """Test custom orbital parameters via generic_porb."""
    # Create custom body with specific orbital elements
    porb_params = {
        "e": 0.5,           # Eccentricity
        "a": 1.2,           # Semi-major axis (AU)
        "rot_per": 365.256, # Rotation period (hours)
        "period": 23.9345,  # Orbital period (hours)
        "Obliq": 45.0,      # Obliquity (degrees)
        # ... other orbital elements
    }
    # Test requires PyKRC support for generic_porb structure
```

**Why Important:**
- Enables exoplanet thermal modeling
- Useful for theoretical studies
- Tests custom orbital mechanics

**Implementation Priority:** 🟢 LOW
**Estimated Effort:** 8-10 hours (requires generic_porb interface)

---

### 8. Multi-Material Mesh (Complex THICK)
**Lines in test_KRC.dv:** 716-723
**Current Status:** ⚠️ Simple two-layer tested, not complex meshes

**Description:**
Test complex multi-layer structures with varying properties per layer.

**Test Case:**
```python
def test_multi_material_mesh_europa(self, validator, tolerance, keep_files):
    """Test 5-layer mesh with varying density, conductivity, heat capacity."""
    mesh_thick = [0.02, 0.04, 0.10, 0.50, 1.00]  # Layer thicknesses (m)
    mesh_dens  = [45., 90., 450., 800., 905.]    # Densities (kg/m³)
    mesh_cond  = [0.1, 0.4, 1.0, 2.0, 5.35]      # Conductivities
    mesh_cp    = [870., 870., 870., 870., 870.]  # Heat capacities

    # THICK = combined 2D array [thick, dens, cond, cp]
    result = validator.compare_run(
        pykrc_params={
            "body": "Europa",
            "lat": 12.0,
            "thick": [mesh_thick, mesh_dens, mesh_cond, mesh_cp],
            "TUN8": 101,
            "KEEP": "T"
        },
        davinci_cmd='krc(body="Europa",lat=12.,thick=THICK,TUN8=101,KEEP="T")',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="multi_material_mesh"
    )
```

**Why Important:**
- Models complex subsurface stratigraphy
- Useful for layered ice deposits
- Tests advanced material property handling

**Implementation Priority:** 🟢 LOW
**Estimated Effort:** 6-8 hours

---

### 9. LZONE (Zone Tables)
**Lines in test_KRC.dv:** 915-920
**Current Status:** ❌ Not tested

**Description:**
Test zone-based material property tables (LZONE="T" with multi-column tables).

**Test Case:**
```python
def test_lzone_multi_region(self, validator, tolerance, keep_files):
    """Test LZONE zone tables for multi-region modeling."""
    # Zone table with thickness, density, and polynomial coefficients per zone
    zone_table = [
        [0.02, 1500., 0.05, 0.001, 0.0001, 0.0001, 657.9, 650.0, 0.00001, -0.000001],
        [0.06, 1700., 0.02, 0.002, 0.0002, 0.0002, 630.0, 650.0, 0.00002, -0.000002],
        [0.20, 1900., 0.70, 0.003, 0.0003, 0.0003, 400.0, 650.0, 0.00003, -0.000003],
    ]
    # Columns: thick, dens, C0-C3 (conductivity), Cp0-Cp3 (heat capacity)

    result = validator.compare_run(
        pykrc_params={
            "lat": 12.0,
            "LZONE": "T",
            "thick": zone_table,
            "ls": 12.0,
            "hour": 4.0,
            "TUN8": 101,
            "KEEP": "T"
        },
        davinci_cmd='krc(lat=12.,LZONE="T",thick=TABLE,ls=12.,hour=4.,TUN8=101,KEEP="T")',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="lzone_zones"
    )
```

**Why Important:**
- Advanced multi-region modeling
- Useful for complex geological structures
- Tests zone-based property assignment

**Implementation Priority:** 🟢 LOW
**Estimated Effort:** 8-10 hours

---

### 10. Lunar H-Parameter (Exponential Profiles)
**Lines in test_KRC.dv:** 887-896
**Current Status:** ❌ PyKRC crashes ("not yet supported")

**Description:**
Test exponential density profiles (thick < 0) for Moon regolith modeling.

**Test Case:**
```python
def test_lunar_h_parameter_profile(self, validator, tolerance, keep_files):
    """Test exponential density profile (H-parameter) for Moon."""
    result = validator.compare_run(
        pykrc_params={
            "body": "Moon",
            "GD": "2009-Apr-20",
            "ALBEDO": 0.12,
            "thick": -0.05,        # Negative = exponential profile
            "INERTIA": 60.0,
            "INERTIA2": 85.0,
            "lat": 0.0,
            "SLOPE": 30.0,
            "SLOAZI": 23.0,
            "KEEP": "T"
        },
        davinci_cmd='krc(body="Moon",GD="2009-Apr-20",thick=-0.05,INERTIA=60,...)',
        tolerance=tolerance,
        keep_files=keep_files,
        test_name="lunar_h_parameter"
    )
```

**Why Important:**
- Critical for Moon thermal modeling
- Models density increasing with depth
- Currently crashes in PyKRC (needs implementation)

**Implementation Priority:** 🟢 LOW (requires feature implementation)
**Estimated Effort:** 10-15 hours (new feature)

---

### 11. Extreme Parameter Validation
**Lines in test_KRC.dv:** Various edge cases throughout file
**Current Status:** ⚠️ Some tested (N1=999), many missing

**Description:**
Test parameter validation at extreme values to ensure graceful handling.

**Test Cases Needed:**
- Very low INERTIA (<10)
- Very high INERTIA (>2500)
- Extreme SLOPE (>45°)
- Near-zero ALBEDO
- ALBEDO near 1.0
- Extreme latitudes (89°, -89°)
- Very small timesteps (N24 > 500)

**Why Important:**
- Ensures robust error handling
- Prevents silent failures
- Documents valid parameter ranges

**Implementation Priority:** 🟢 LOW
**Estimated Effort:** 4-6 hours

---

## Implementation Roadmap

### Phase 1: Core Functionality (Weeks 1-2)
- [ ] Direct material property override (Test #1)
- [ ] Time-varying TAUD tables (Test #2)
- [ ] High-inertia edge cases (Test #3)

**Expected Impact:** +5-7% test coverage, validates critical features

### Phase 2: Extended Features (Weeks 3-4)
- [ ] Custom IR/VIS flux tables (Test #4)
- [ ] Atmospheric parameters (Test #5)
- [ ] Additional solar system bodies (Test #6)

**Expected Impact:** +3-5% test coverage, expands capabilities

### Phase 3: Advanced Features (Weeks 5-8)
- [ ] Exoplanet/custom bodies (Test #7)
- [ ] Multi-material mesh (Test #8)
- [ ] LZONE zone tables (Test #9)
- [ ] Lunar H-parameter (Test #10)
- [ ] Extreme parameter validation (Test #11)

**Expected Impact:** +5-8% test coverage, completes comprehensive validation

---

## Appendix: Test File Organization

### Suggested New Test Classes

```python
class TestAdvancedMaterialPropertiesIntegration:
    """Direct coefficient override and complex material models."""
    # Tests #1, #8

class TestAtmosphericModelingIntegration:
    """Time-varying opacity and atmospheric parameters."""
    # Tests #2, #5

class TestPlanetaryFluxIntegration:
    """Advanced PFlux with custom IR/VIS tables."""
    # Tests #4

class TestExtendedBodyCoverageIntegration:
    """Additional solar system bodies and exoplanets."""
    # Tests #6, #7

class TestAdvancedGeometryIntegration:
    """LZONE, H-parameters, complex mesh structures."""
    # Tests #9, #10

class TestParameterValidationIntegration:
    """Edge cases and extreme parameter values."""
    # Tests #3, #11
```

---

## Conclusion

This analysis identified **~40 new test cases** across 11 major categories. Implementing Phase 1 (HIGH PRIORITY) tests would provide immediate value by:

1. Validating core features users need (direct material control, seasonal opacity)
2. Documenting known limitations (high-TI errors)
3. Increasing test coverage by 5-7%
4. Building confidence in PyKRC's Davinci parity

**Recommendation:** Start with HIGH PRIORITY tests, then reassess based on user feedback and development priorities.

---

**Document Version:** 1.0
**Author:** Analysis by Claude (Anthropic)
**Contact:** See CLAUDE.md for project context
