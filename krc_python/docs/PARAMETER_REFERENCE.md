# KRC Parameter Reference

**Last Updated:** 2025-10-20
**Purpose:** Comprehensive reference for all KRC parameters, their resolution order, transformations, and input file generation

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Parameter Resolution and Precedence](#parameter-resolution-and-precedence)
3. [Parameter Categories](#parameter-categories)
4. [Material Property Calculations](#material-property-calculations)
5. [PORB Parameter Handling](#porb-parameter-handling)
6. [Input File Generation and Changecards](#input-file-generation-and-changecards)
7. [Complete Parameter Tables](#complete-parameter-tables)
8. [Critical Differences: Davinci vs PyKRC](#critical-differences-davinci-vs-pykrc)

---

## Executive Summary

KRC accepts **140+ parameters** across 6 categories, which undergo a complex transformation pipeline before being written to the Fortran input file. This document traces every parameter through the complete flow:

**User Input** → **Resolution** → **Transformation** → **Fortran Input File**

### Key Concepts

1. **Parameter Precedence**: User → PORB → Material → Master.inp → Hardcoded
2. **Automatic Calculations**: Material properties, N1/N2, derived values
3. **Changecard System**: Fortran punch-card convention for runtime parameter modification
4. **PORB-Touched Parameters**: Special tracking for orbital-derived values

### Primary vs Secondary Success Metrics

**PRIMARY**: Input file generation must match Davinci byte-for-byte
**SECONDARY**: Temperature output arrays must be nearly identical

---

## Parameter Resolution and Precedence

### Resolution Pipeline

```
┌─────────────────────────────────────────────────────────────┐
│                     USER INPUTS                             │
│  krc(lat=30, INERTIA=250, body="Mars", ...)                │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              INITIALIZATION                                 │
│  • Load master.inp defaults                                 │
│  • Load PORB for body                                       │
│  • Set davinci defaults                                     │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│         PARAMETER RESOLUTION                                │
│  Priority: User > PORB > Material > Master.inp              │
│  • Check HasValue() for each parameter                      │
│  • Apply PORB defaults if not user-set                      │
│  • Calculate derived parameters                             │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│      MATERIAL CALCULATIONS                                  │
│  • Determine Mat1/Mat2 properties                           │
│  • Calculate INERTIA from COND/DENSITY/SPEC_HEAT           │
│  • Fit k(T) and Cp(T) polynomials                           │
│  • Apply porosity corrections                               │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│       INPUT FILE GENERATION                                 │
│  • Write master.inp header                                  │
│  • Write parameter arrays (REAL*8, INTEGER*4, LOGICAL*4)    │
│  • Generate changecards (Type 1/2/3/8/14/15)                │
└─────────────────────────────────────────────────────────────┘
```

### Three-Level Parameter Precedence

For each parameter, resolution follows this order (highest priority first):

1. **User Explicit**: User directly specifies in function call
2. **PORB-Derived**: Calculated by PORB for specific body
3. **Material-Derived**: Calculated from material properties
4. **Master.inp Defaults**: Placeholder values from template
5. **Hardcoded Defaults**: Fallback values in code

### Resolution Examples

**INERTIA Resolution**:
```python
# Priority order:
# 1. User explicit: krc(INERTIA=250)
# 2. TES map (if Mars): krc_INERTIA[lat, lon]
# 3. Material calc: sqrt(COND * DENSITY * SPEC_HEAT)
# 4. Body default: 100 (Europa), 200 (generic)
```

**PERIOD Resolution**:
```python
# Priority order:
# 1. User explicit: krc(PERIOD=1.0)
# 2. PORB-derived: porb(body).krc.PERIOD
# 3. Master.inp: 1.0275 (Mars default)
```

**DELJUL Resolution**:
```python
# Complex logic:
# 1. User DELLS: DELJUL = PERIOD/360 × DELLS
# 2. User DELJUL: Use as-is
# 3. PORB DELJUL: porb(body).krc.DELJUL
# NOTE: User DELLS blocks PORB DELJUL injection
```

---

## Parameter Categories

### 1. Material Properties (18 parameters)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| ALBEDO | REAL*8 | 0.25 (Mars map) | - | Surface bolometric albedo |
| EMISS | REAL*8 | 1.0 | - | Thermal emissivity |
| INERTIA | REAL*8 | 200 (Mars map) | J m⁻² K⁻¹ s⁻½ | Upper layer thermal inertia |
| INERTIA2 | REAL*8 | INERTIA | J m⁻² K⁻¹ s⁻½ | Lower layer thermal inertia |
| COND | REAL*8 | Calculated | W m⁻¹ K⁻¹ | Upper layer conductivity |
| COND2 | REAL*8 | Calculated | W m⁻¹ K⁻¹ | Lower layer conductivity |
| DENSITY | REAL*8 | Calculated | kg m⁻³ | Upper layer density |
| DENS2 | REAL*8 | Calculated | kg m⁻³ | Lower layer density |
| SPEC_HEAT | REAL*8 | Calculated | J kg⁻¹ K⁻¹ | Upper layer specific heat |
| SpHeat2 | REAL*8 | Calculated | J kg⁻¹ K⁻¹ | Lower layer specific heat |
| Mat1 | STRING | "basalt" | - | Upper layer material |
| Mat2 | STRING | Mat1 | - | Lower layer material |
| Por1 | REAL*8 | Calc from I | - | Upper layer porosity |
| Por2 | REAL*8 | Por1 | - | Lower layer porosity |
| thick | REAL*8 | 0 | m | Upper layer thickness |
| T_user | REAL*8 | 220 | K | Reference temperature |
| k_style | STRING | "Mars" | - | k(T) formulation |

### 2. Atmospheric Parameters (11 parameters)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| PTOTAL | REAL*8 | PORB | Pa | Atmospheric pressure |
| TAUD | REAL*8 | 0.30 | - | Visible optical depth |
| TATM | REAL*8 | PORB | K | Atmospheric temperature |
| DUSTA | REAL*8 | PORB | μm | Dust particle size |
| TAURAT | REAL*8 | PORB | - | IR/vis opacity ratio |
| FANON | REAL*8 | 0.055 | - | Non-condensable fraction |
| LVFT | LOGICAL*4 | F | - | Variable frost temperature |
| TFROST | REAL*8 | 0 or calc | K | Frost point temperature |
| CFROST | REAL*8 | Calc | J kg⁻¹ | Latent heat of frost |
| AFROST | REAL*8 | Calc | - | Frost albedo |
| KPREF | INTEGER*4 | 1 | - | Pressure model (1 or 2) |

### 3. Thermal Model Parameters (14 parameters)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| N1 | INTEGER*4 | krc_evalN1() | - | Number of layers |
| N2 | INTEGER*4 | krc_evalN2() | - | Timesteps per day |
| N3 | INTEGER*4 | 15 | - | Max iteration days |
| FLAY | REAL*8 | 0.10 | skin depths | First layer thickness |
| RLAY | REAL*8 | 1.15 | - | Layer growth ratio |
| TDEEP | REAL*8 | 180 | K | Deep/initial temperature |
| IIB | INTEGER*4 | -1 | - | Bottom BC type |
| IC2 | INTEGER*4 | 999 or calc | - | Layer property change |
| CONVF | REAL*8 | 3.0 | - | Stability factor |
| DTMAX | REAL*8 | 0.1 | K | RMS layer T change |
| DDT | REAL*8 | 0.002 | K | 2nd difference limit |
| GGT | REAL*8 | 0.1 or 99 | K | Surface iteration |
| NRSET | INTEGER*4 | 999 or 3 | - | Days before reset |
| LKofT | LOGICAL*4 | T | - | Use k(T), Cp(T) |

### 4. Time and Season Control (9 parameters)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| N4 | INTEGER*4 | 1 | - | Number of latitudes |
| N5 | INTEGER*4 | 120 | - | Number of seasons |
| N24 | INTEGER*4 | 48 or PORB | - | Output hours per day |
| PERIOD | REAL*8 | PORB | days | Rotation period |
| DELJUL | REAL*8 | PORB or calc | days | Time step (Julian days) |
| DELLS | REAL*8 | User only | deg | Time step (Ls degrees) |
| DJUL | REAL*8 | 0.1 or calc | JD | Starting date |
| JDISK | INTEGER*4 | 81 or calc | - | First output season |
| LKEY | LOGICAL*4 | T | - | Date mode (Ls vs JD) |

### 5. Location and Geometry (7 parameters)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| lat | REAL*8 | 0 | deg | Latitude |
| lon | REAL*8 | 0 | deg E | Longitude |
| ELEV | REAL*8 | 0 or map | km | Surface elevation |
| SLOPE | REAL*8 | 0 | deg | Surface slope |
| SLOAZI | REAL*8 | 0 | deg | Slope azimuth |
| LPORB | LOGICAL*4 | T | - | Use orbital calculations |
| LOCAL | LOGICAL*4 | T | - | Use local scaling |

### 6. Orbital Parameters (30 values)

Provided as 30-element PORB matrix when LPORB=T:
- Planet ID (IPLAN)
- Time constant (TC)
- Orbital elements (eccentricity, obliquity, etc.)
- Planetary constants (radius, mass, etc.)
- Rotation matrices

### 7. Output Control (10 parameters)

| Parameter | Type | Default | Units | Description |
|-----------|------|---------|-------|-------------|
| K4OUT | INTEGER*4 | 52 | - | Output format (0-56) |
| LP1-LP6 | LOGICAL*4 | F | - | Print control flags |
| LPGLOB | LOGICAL*4 | F | - | Print global parameters |
| LPTAVE | LOGICAL*4 | F | - | Print averages |
| TUN8 | INTEGER*4 | 0 | - | Special output mode |
| KEEP | INTEGER*4 | 0 | - | Continue disk file |

### 8. k(T) and Cp(T) Coefficients (16 parameters)

Temperature-dependent polynomial coefficients (calculated):
- ConUp0-3: Upper layer k(T) = ConUp0 + ConUp1×X + ConUp2×X² + ConUp3×X³
- ConLo0-3: Lower layer k(T)
- SphUp0-3: Upper layer Cp(T)
- SphLo0-3: Lower layer Cp(T)

Where X = (T - 220) × 0.01 (normalized temperature)

---

## Material Property Calculations

### Calculation Order (Critical!)

PyKRC **must** follow this exact sequence from Davinci:

```python
# 1. Get material baseline properties
Mat_Prop = get_material_properties(Mat1)

# 2. Calculate specific heat at T_user
X_user = (T_user - 220.0) * 0.01
SPEC_HEAT = (Mat_Prop['SphUp0'] +
             Mat_Prop['SphUp1'] * X_user +
             Mat_Prop['SphUp2'] * X_user**2 +
             Mat_Prop['SphUp3'] * X_user**3)

# 3. Calculate porosity from INERTIA (if not user-set)
if not user_set_Por1:
    Por1 = 0.60 * (2200.0 - INERTIA) / 2200.0

# 4. Calculate density with porosity and T correction
X_Dens = (T_user - 220.0) * 0.01
D0 = Mat_Prop['Dens0']
D1 = Mat_Prop['Dens1']
D2 = Mat_Prop['Dens2']
D3 = Mat_Prop['Dens3']
grain_density = D0 + D1*X_Dens + D2*X_Dens**2 + D3*X_Dens**3
DENSITY = (1 - Por1) * grain_density

# 5. Calculate conductivity
COND = INERTIA**2 / (DENSITY * SPEC_HEAT)

# 6. Generate temperature table (80K to 478K)
T_Tab = np.arange(80, 478, 2.0)
X = (T_Tab - 220.0) * 0.01

# 7. Calculate k_Table using k_style
if k_style == "Moon":
    # Hayne et al. 2017
    k_Table = COND * (1 + 2.7 * ((T_Tab - T_user) / 350.0)**3)
elif k_style == "Mars":
    # Morgan et al. 2018
    k_Table = COND * np.sqrt(T_Tab / T_user)
elif k_style == "Bulk":
    # Use bulk conductivity polynomial
    k_Table = (Mat_Prop['ConB0'] + Mat_Prop['ConB1'] * X +
               Mat_Prop['ConB2'] * X**2 + Mat_Prop['ConB3'] * X**3)

# 8. Fit cubic polynomial to k_Table
ConUp0, ConUp1, ConUp2, ConUp3 = fit_cubic(X, k_Table)

# 9. Repeat for lower layer with Mat2, Por2, INERTIA2
```

### k_style Formulations

**"Moon"**: Hayne et al. 2017
```python
k(T) = k_user * (1 + 2.7 * ((T - T_user) / 350)**3)
```

**"Mars"**: Morgan et al. 2018
```python
k(T) = k_user * sqrt(T / T_user)
```

**"Bulk"**: Material-specific polynomial
```python
k(T) = ConB0 + ConB1*X + ConB2*X² + ConB3*X³
```

### Two-Layer Configuration

Triggered by `thick != 0`:

- **thick > 0**: Two-layer regolith, thick = top layer thickness (meters)
- **thick < 0**: Exponential property trend, thick = H parameter
- **thick = 0**: Uniform material

IC2 calculation:
```python
if thick != 0:
    IC2 = calculate_layer_index_from_depth(thick, FLAY, RLAY, N1)
else:
    IC2 = 999  # No layer change
```

**Constraint**: IC2 must be ≥ 3 or 999 (layer 1 is atmosphere, layer 2 is first surface)

---

## PORB Parameter Handling

### PORB Structure

When `body` is specified, PORB calculations provide:

```python
porb = porb(body, force=bodyforce)

# porb.krc dictionary contains:
{
    'PERIOD': 1.0275,      # Rotation period (days)
    'N24': 96,             # Timesteps per day
    'DELJUL': 1.9083,      # Time step (days)
    'PTOTAL': 546.0,       # Atmospheric pressure (Pa)
    'DUSTA': 1.5,          # Dust size (μm)
    'GRAV': 3.727,         # Gravity (m/s²)
    'TAURAT': 0.25,        # IR/vis opacity ratio
    'ARC2_G0': ...,        # Photometric parameter
    'SOLCON': 1368.0,      # Solar constant (W/m²)
    'Atm_Cp': 735.0,       # Atmospheric Cp (J/kg/K)
    # ... additional parameters
}

# porb.rot: 30-element orbital parameter matrix
```

### PORB Injection Loop

**Critical**: Davinci injects ALL `porb.krc` parameters unless:
1. User explicitly provided the parameter
2. Parameter is DELJUL and user set DELLS

```python
# Davinci logic (lines 469-484)
for param_name in porb.krc.keys():
    if not user_has_value(param_name):
        if param_name == 'DELJUL' and user_has_value('DELLS'):
            # Skip - will be calculated from DELLS
            continue
        else:
            # Inject PORB value
            params[param_name] = porb.krc[param_name]
```

**PyKRC must implement same logic!**

### PORB-Touched Parameters

Even if PORB-derived value matches master.inp default, **changecard must be written**:

```python
porb_touched_params = set()

# Track which parameters PORB "touched"
for param in porb.krc.keys():
    if param not in user_params:
        porb_touched_params.add(param)

# Later, during changecard generation:
if (value != master_inp_default or
    param in porb_touched_params or
    param in user_specified_params):
    write_changecard(param, value)
```

---

## Input File Generation and Changecards

### Input File Structure

```
Line 1:      0 0 / KOLD KEEP (always "0 0" in header)
Line 2:      Title (80 characters)
Lines 3-10:  REAL*8 params (8 per line, 64 total)
Lines 11-13: INTEGER*4 params (8 per line, 20 total)
Lines 14-15: LOGICAL*4 params (10 per line, 20 total)
Line 16+:    Latitudes (10 per line, N4 values)
Line 17+:    Elevations (10 per line, N4 values)
Lines 18+:   PORB matrix (5 per line, 30 values) IF LPORB=T
Lines 19+:   Changecards
Final:       0/ (two lines)
```

### Parameter Formatting

**REAL*8** (10-character fields):
```python
header = "    " + "".join(f"{name:>10}" for name in param_names)
values = "    " + "".join(f"{val:>10.2f}" for val in param_values)
```

**INTEGER*4** (10-character fields):
```python
header = "".join(f"{name:>10}" for name in param_names)
values = "".join(f"{val:>10d}" for val in param_values)
```

**LOGICAL*4** (7-character fields):
```python
header = "".join(f"{name:>7}" for name in param_names)
values = "".join(f"{'T' if val else 'F':>7}" for val in param_values)
```

**Latitudes/Elevations** (7-character fields):
```python
values = "".join(f"{val:>7.2f}" for val in values)  # 10 per line
```

**PORB matrix** (15-character fields, G15.7 format):
```python
for i in range(0, 30, 5):
    chunk = porb_params[i:i+5]
    line = ''.join(f"{val:15.7f}" for val in chunk)
```

### Changecard System

**Historical context**: Punch card legacy - allows runtime parameter modification without recompiling.

**Format**: `<TYPE> <INDEX> <VALUE> '<NAME>' /`

**Types**:
- **Type 1**: REAL*8 (part1 parameters, indices 1-64)
- **Type 2**: INTEGER*4 (part2 parameters, indices 1-20)
- **Type 3**: LOGICAL*4 (part3 parameters, indices 1-20)
- **Type 8**: File operations (subtypes for input/output files)
- **Type 14**: Eclipse parameters
- **Type 15**: Planetary flux parameters

### Changecard Generation Order

```python
# 1. Type 3 (LOGICAL) - all 20 parameters checked
for i, param in enumerate(bool_params, start=1):
    if has_value(param):
        write(f"3 {i} {1 if param else 0} '{param_name}' /")

# 2. Type 2 (INTEGER) - all 20 parameters checked, EXCEPT N4
for i, param in enumerate(int_params, start=1):
    if has_value(param) and param_name != "N4":
        write(f"2 {i} {param} '{param_name}' /")

# 3. Type 1 (REAL) - all 64 parameters checked
for i, param in enumerate(float_params, start=1):
    if has_value(param):
        if -1 < param < 1:
            # Small values: scientific notation
            write(f"1 {i} {param:.3E} '{param_name}' /")
        else:
            # Large values: fixed point
            write(f"1 {i} {param:.4f} '{param_name}' /")

# 4. Type 8 (Files) - output file ALWAYS written
write(f"8 5 0 '{output_path}' /")

# 5. Terminator
write("0/\n0/\n")
```

### Changecard Logic: When to Write

A changecard is written if **ANY** of these conditions are true:

1. Value differs from master.inp header default
2. Parameter in `porb_touched_params` set
3. Parameter explicitly set by user

**Example**:
```python
def should_write_changecard(param, value, default, porb_touched, user_set):
    return (value != default or
            param in porb_touched or
            param in user_set)
```

### N4 Special Handling

**Critical**: N4 is **NOT** written via changecard!

Instead, N4 is written directly into the master.inp header structure:
```python
# Find N4 position in INTEGER section
n4_line_index = find_line_with("N4")
# Overwrite value directly in the 10-character field
inp_lines[n4_line_index] = replace_n4_value(inp_lines[n4_line_index], N4)
```

**Reason**: KRC checks N4 before processing changecards to validate lat/elev array sizes.

### Time-Varying Parameters

**ALBEDO** and **TAUD** can be time-varying (2D arrays):

```python
if isinstance(ALBEDO, np.ndarray) and ALBEDO.ndim == 2:
    # Write to file
    with open(workdir / 'albfile.tab', 'w') as f:
        f.write("# Ls vs ALBEDO\n")
        for ls, alb in ALBEDO:
            f.write(f"{ls:.2f}\t{alb:.2f}\n")

    # Use Type 8 changecard
    write("8 22 0 'albfile.tab' /")

# Similar for TAUD (Type 8 subtype 23)
```

### Special Changecards

**Eclipse (Type 14)**:
```
14 <style> <sun_dis> <eclipser_rad> <cm> <eclipsed_rad> <per_mut> <bias> <date> <ecl_cent_hr> 3 0. 0. / Eclipse
```

**Planetary Flux (Type 15)**:
```
15 <IR_flux> <IR_half_amp> <IR_phase_lag> <Vis_flux> <Vis_half_amp> <Vis_phase_lag> <Lon_Hr> / Forcing from Planet
```

---

## Complete Parameter Tables

### REAL*8 Parameters (Part1, 64 total)

**Order** (index 1-64):

| Index | Parameter | Default | Units | Notes |
|-------|-----------|---------|-------|-------|
| 1 | ALBEDO | 0.25 | - | Surface albedo |
| 2 | EMISS | 1.0 | - | Emissivity |
| 3 | INERTIA | 200 | J m⁻² K⁻¹ s⁻½ | Thermal inertia |
| 4 | COND2 | Calc | W m⁻¹ K⁻¹ | Lower layer conductivity |
| 5 | DENS2 | Calc | kg m⁻³ | Lower layer density |
| 6 | PERIOD | PORB | days | Rotation period |
| 7 | SPEC_HEAT | Calc | J kg⁻¹ K⁻¹ | Specific heat |
| 8 | DENSITY | Calc | kg m⁻³ | Density |
| 9-16 | CABR, AMW, SatPrA, PTOTAL, FANON, TATM, TDEEP, SpHeat2 | Various | Various | Atm/thermal |
| 17-24 | TAUD, DUSTA, TAURAT, TWILI, ARC2_G0, ARC3_Safe, SLOPE, SLOAZI | Various | Various | Opacity/geometry |
| 25-32 | TFROST, CFROST, AFROST, FEMIS, AF1, AF2, FROEXT, SatPrB | Various | Various | Frost |
| 33-40 | RLAY, FLAY, CONVF, DEPTH, DRSET, PhotoFunc, GGT, DTMAX | Various | Various | Thermal solution |
| 41-48 | DJUL, DELJUL, SOLARDEC, DAU, LsubS, SOLCON, GRAV, Atm_Cp | PORB | Various | Orbital/atm |
| 49-56 | ConUp0-3, ConLo0-3 | Fitted | W m⁻¹ K⁻¹ | k(T) coefficients |
| 57-64 | SphUp0-3, SphLo0-3 | From Mat | J kg⁻¹ K⁻¹ | Cp(T) coefficients |

### INTEGER*4 Parameters (Part2, 20 total)

**Order** (index 1-20):

| Index | Parameter | Default | Notes |
|-------|-----------|---------|-------|
| 1 | N1 | krc_evalN1() | Number of layers |
| 2 | N2 | krc_evalN2() | Timesteps per day |
| 3 | N3 | 15 | Iteration days |
| 4 | N4 | 1 | Number of latitudes (NOT in changecards) |
| 5 | N5 | 120 | Number of seasons |
| 6 | N24 | 48 | Output hours per day |
| 7 | IIB | -1 | Bottom boundary type |
| 8 | IC2 | 999 | Layer change index |
| 9-16 | NRSET, NMHA, NRUN, JDISK, IDOWN, FlxP14, TUN_Flx15, KPREF | Various | Control flags |
| 17-20 | K4OUT, JBARE, Notif, IDISK2 | Various | Output control |

### LOGICAL*4 Parameters (Part3, 20 total)

**Order** (index 1-20):

| Index | Parameter | Default | Notes |
|-------|-----------|---------|-------|
| 1-7 | LP1-6, LPGLOB | F | Print flags |
| 8 | LVFA | F | Variable frost albedo |
| 9 | LVFT | F | Variable frost temp |
| 10 | LKofT | T | Use k(T), Cp(T) |
| 11 | LPORB | T | Use orbital calculations |
| 12 | LKEY | T | Date mode (Ls vs JD) |
| 13 | LSC | F | (unused) |
| 14 | LZONE | Calc | Zone file flag |
| 15-20 | LOCAL, Prt76, LPTAVE, Prt78, Prt79, L_ONE | Various | Control flags |

---

## Critical Differences: Davinci vs PyKRC

### Default Value Discrepancies

| Parameter | Davinci Default | PyKRC Default | Impact | Solution |
|-----------|----------------|---------------|--------|----------|
| **RLAY** | 1.15 (master.inp) | 1.08 | **HIGH** - Changes layer spacing | Use 1.15 |
| **FLAY** | 0.10 (master.inp) | 2.0 | **HIGH** - Changes first layer depth | Use 0.10 |
| **FANON** | 0.055 (master.inp) | 0.3 | **MEDIUM** - Non-condensable fraction | Use 0.055 |
| **TAURAT** | 0.25 (master.inp) | 2.0 | **MEDIUM** - IR/vis opacity ratio | Use 0.25 |
| **N3** | 15 (master.inp) | 1 | **LOW** - Iteration convergence | Use 15 |

### Material Calculation Order

**Critical**: PyKRC must follow Davinci's exact calculation sequence:

1. Get Mat_Prop(Mat1)
2. Calculate SPEC_HEAT at T_user
3. Calculate Por1 from INERTIA (if not user-set)
4. Calculate DENSITY from Mat_Prop, Por1, T_user
5. Calculate COND = INERTIA²/(DENSITY×SPEC_HEAT)
6. Generate T_Tab (80K to 478K)
7. Calculate k_Table using k_style
8. Fit cubic polynomial → ConUp0-3

**Any deviation breaks input file parity!**

### PORB Injection

**Davinci**: Injects ALL `porb.krc` parameters unless user set or DELLS blocks DELJUL

**PyKRC must**:
- Implement same injection loop
- Track `porb_touched_params`
- Write changecards for all PORB-touched parameters

### Changecard Precision

**Float changecards** use magnitude-dependent precision:

```python
if -1 < value < 1:
    format_string = f"1 {i} {value:.3E} '{name}' /"  # Scientific
else:
    format_string = f"1 {i} {value:.4f} '{name}' /"  # Fixed
```

**Examples**:
- `ALBEDO=0.25` → `1 1 2.500E-01 'ALBEDO' /`
- `INERTIA=250` → `1 3 250.0000 'INERTIA' /`

### Header Line Format

**Always write** `0 0 /` in header line 1, regardless of actual KEEP value:

```python
f.write("0 0 / KOLD: season to start with;  KEEP: continue saving\n")
```

Actual KEEP value is set via changecard.

### Input File Parity Checklist

- [ ] RLAY/FLAY defaults match Davinci (1.15/0.10)
- [ ] FANON/TAURAT use master.inp values (0.055/0.25) not common values
- [ ] N4 written to header, NOT changecard
- [ ] Changecard precision matches (%.3E vs %.4f based on magnitude)
- [ ] Float changecards written for all values ≠ master.inp OR PORB-touched OR user-set
- [ ] Type 8 changecard for output file ALWAYS written
- [ ] PORB parameter injection matches Davinci logic
- [ ] DELLS/DELJUL mutual exclusion enforced
- [ ] IC2 constraint (≥3 or 999) validated
- [ ] Time-varying ALBEDO/TAUD write to file + Type 8 changecard
- [ ] PORB matrix uses 15-char fields (G15.7 format)
- [ ] Latitude/elevation use 7-char fields (F7.2 format)
- [ ] Material calculation order exactly matches Davinci

---

## Validation Checklist for PyKRC

### Pre-Execution Validation

```python
# Check dimensional limits
assert N1 <= 30, "N1 exceeds MAXN1"
assert N2 <= 1536, "N2 exceeds MAXN2"
assert N4 <= 37, "N4 exceeds MAXN4"
assert N5 <= 161, "N5 exceeds MAXN5"

# Check stability
assert CONVF == 0 or CONVF >= 0.8, "CONVF < 0.8 unstable"

# Check N2 is even and N24 divides N2
assert N2 % 2 == 0, "N2 must be even"
assert N2 % N24 == 0, "N24 must divide N2"

# Check IC2 constraint
assert IC2 == 999 or IC2 >= 3, "IC2 must be ≥3 or 999"

# Check DELLS/DELJUL mutual exclusion
assert not (has_DELLS and has_DELJUL), "Cannot set both DELLS and DELJUL"

# Check DELJUL >= PERIOD
assert DELJUL >= PERIOD, "DELJUL must be ≥ PERIOD"
```

### Post-Execution Validation

```python
# Check input file was created
assert inp_file.exists(), "Input file not created"

# Compare with Davinci reference (PRIMARY METRIC)
with open(pykrc_inp) as f1, open(davinci_inp) as f2:
    pykrc_lines = f1.readlines()
    davinci_lines = f2.readlines()
    assert len(pykrc_lines) == len(davinci_lines)
    for i, (line1, line2) in enumerate(zip(pykrc_lines, davinci_lines)):
        assert line1 == line2, f"Mismatch at line {i+1}"

# Check output file was created
assert out_file.exists(), "Output file not created"

# Parse output and check arrays (SECONDARY METRIC)
result = parse_bin52(out_file)
assert len(result['surf']) > 0, "No temperature data"
assert np.allclose(result['surf'], reference['surf'], atol=0.01), \
    "Temperature arrays differ beyond tolerance"
```

---

## References

- **Davinci Source**: krc.dvrc (reference implementation)
- **KRC Fortran**: krc8.f, tseas8.f, tlats8.f, tday8.f
- **Master.inp**: Default parameter template
- **PORB System**: Orbital parameter calculations
- **Material Database**: Mat_Prop() function in materials.py
- **Kieffer et al., 2013**: "A reanalysis of Mars atmospheric temperature..." JGR Planets

---

**End of Document**
