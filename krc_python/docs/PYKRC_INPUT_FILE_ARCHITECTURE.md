# PyKRC Input File Generation Architecture - Technical Analysis

**Generated:** 2025-10-09
**Purpose:** Comprehensive technical mapping of PyKRC's input file generation system for comparison with Davinci implementation

---

## Table of Contents

1. [Overview](#overview)
2. [KRCExecutor.create_input_file() Implementation](#krcexecutorcreate_input_file-implementation)
3. [Changecard Generation (_write_changecards)](#changecard-generation-_write_changecards)
4. [Parameter Formatting Functions](#parameter-formatting-functions)
5. [MASTER_INP_DEFAULTS Handling](#master_inp_defaults-handling)
6. [Parameter Ordering and Structure](#parameter-ordering-and-structure)
7. [Special Cases Handling](#special-cases-handling)
8. [Parameter Reference Table](#parameter-reference-table)
9. [Issues and Discrepancies](#issues-and-discrepancies)

---

## Overview

PyKRC generates KRC Fortran input files through the `KRCExecutor` class in `/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/executor.py`. The system uses a **changecard-based approach** where:

1. A **base template** is written using `MASTER_INP_DEFAULTS` values
2. **Changecards** override specific parameters that differ from defaults
3. The file follows the strict format expected by KRC Fortran binary (v3.5.6+)

### Key Design Principles

- **Minimal file writing**: Only header uses defaults; changecards handle variations
- **Type-indexed changecards**: Type 1 (float), Type 2 (int), Type 3 (bool), Type 8 (file)
- **Order matters**: Parameter index in changecard corresponds to position in master.inp
- **PORB orbital data**: Separate section for planetary orbital parameters

---

## KRCExecutor.create_input_file() Implementation

**Location:** `executor.py`, lines 200-326

### Function Signature

```python
def create_input_file(
    self,
    workdir: Path,
    params: Dict[str, Any],
    filename: str = "krc.inp",
    user_params: Optional[Dict[str, Any]] = None
) -> Path
```

### Execution Flow

```
1. Open file for writing
   ↓
2. Write KOLD/KEEP header (line 229-231)
   - Always uses defaults (0, 0)
   - Actual values in changecards if different
   ↓
3. Write parameter blocks using MASTER_INP_DEFAULTS (lines 236-291)
   - 8 REAL*8 blocks (64 parameters)
   - 3 INTEGER*4 blocks (24 parameters)
   - 2 LOGICAL*4 blocks (20 parameters)
   ↓
4. Write latitudes array (lines 293-294)
   - Format: 10F7.2 (10 values per line, 7 chars wide, 2 decimals)
   ↓
5. Write elevations array (lines 295-296)
   - Format: 10F7.2
   ↓
6. Write PORB section if LPORB=T (lines 297-312)
   - Header line with timestamp
   - 30 orbital parameters in 6 lines (5G15.7 per line)
   ↓
7. Write changecards (line 316)
   - Only for parameters that differ from defaults
   - Filtered by user_params if provided
   ↓
8. Write disk file specification (lines 319-321)
   - Type 8 changecard with output filename
   ↓
9. Write termination (lines 323-324)
   - Two "0/" lines to end input
```

### Code Flow Diagram

```
create_input_file()
├── Write header (KOLD, KEEP, version line)
├── _write_param_line() × 8  [REAL*8 blocks]
│   ├── Write parameter names (10-char fields)
│   └── Write values (10-char fields, 2 decimals)
├── _write_param_line() × 3  [INTEGER*4 blocks]
│   ├── Write parameter names
│   └── Write values (10-char fields)
├── _write_logical_flags()   [LOGICAL*4 blocks]
│   ├── Write LP* flags (7-char fields)
│   └── Write LPORB* flags (7-char fields)
├── _write_latitudes()
├── _write_elevations()
├── Write PORB header + params (if LPORB=T)
├── _write_changecards()     [Overrides]
│   ├── Loop boolean params (Type 3)
│   ├── Loop integer params (Type 2)
│   └── Loop float params (Type 1)
└── Write disk file + termination
```

---

## Changecard Generation (_write_changecards)

**Location:** `executor.py`, lines 400-503

### Purpose

Changecards allow runtime parameter modification following KRC's punch card convention. They override values in the header section.

### Changecard Format

```
<type> <index> <value> '<parameter_name>' /
```

- **Type 1**: Floating point (REAL*8)
- **Type 2**: Integer (INTEGER*4)
- **Type 3**: Boolean/Logical (LOGICAL*4)
- **Type 8**: File path

### Implementation Details

#### Parameter Lists (lines 424-446)

Three ordered lists define the changecard index for each parameter:

```python
# Floating point parameters (Type 1) - 64 parameters
float_params = [
    # Block 1: Material properties (8 params)
    "ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2", "PERIOD", "SPEC_HEAT", "DENSITY",
    # Block 2: Atmosphere (8 params)
    "CABR", "AMW", "SatPrA", "PTOTAL", "FANON", "TATM", "TDEEP", "SpHeat2",
    # Block 3: Opacity/slope (8 params)
    "TAUD", "DUSTA", "TAURAT", "TWILI", "ARC2_G0", "ARC3_Safe", "SLOPE", "SLOAZI",
    # Block 4: Frost (8 params)
    "TFROST", "CFROST", "AFROST", "FEMIS", "AF1", "AF2", "FROEXT", "SatPrB",
    # Block 5: Layers/convergence (8 params)
    "RLAY", "FLAY", "CONVF", "DEPTH", "DRSET", "PhotoFunc", "GGT", "DTMAX",
    # Block 6: Orbital/solar (8 params)
    "DJUL", "DELJUL", "SOLARDEC", "DAU", "LsubS", "SOLCON", "GRAV", "Atm_Cp",
    # Block 7: Conductivity coefficients (8 params)
    "ConUp0", "ConUp1", "ConUp2", "ConUp3", "ConLo0", "ConLo1", "ConLo2", "ConLo3",
    # Block 8: Specific heat coefficients (8 params)
    "SphUp0", "SphUp1", "SphUp2", "SphUp3", "SphLo0", "SphLo1", "SphLo2", "SphLo3"
]

# Integer parameters (Type 2) - 24 parameters
int_params = [
    # Block 9: Grid parameters (8 params)
    "N1", "N2", "N3", "N4", "N5", "N24", "IIB", "IC2",
    # Block 10: Run control (8 params)
    "NRSET", "NMHA", "NRUN", "JDISK", "IDOWN", "FlxP14", "TUN_Flx15", "KPREF",
    # Block 11: Output control (4 params)
    "K4OUT", "JBARE", "Notif", "IDISK2"
]

# Boolean parameters (Type 3) - 20 parameters
bool_params = [
    # LP flags (10 params)
    "LP1", "LP2", "LP3", "LP4", "LP5", "LP6", "LPGLOB", "LVFA", "LVFT", "LKofT",
    # LPORB flags (10 params)
    "LPORB", "LKEY", "LSC", "LZONE", "LOCAL", "Prt76", "LPTAVE", "Prt78", "Prt79", "L_ONE"
]
```

### Writing Logic

#### Boolean Changecards (lines 448-465)

```python
for i, param_name in enumerate(bool_params, start=1):
    if param_name in params:
        val = params[param_name]
        # Convert to boolean
        if isinstance(val, bool):
            bool_val = val
        elif isinstance(val, str):
            bool_val = val.upper() == 'T'
        else:
            bool_val = bool(val)

        # Only write if different from master.inp default
        master_default = MASTER_INP_DEFAULTS.get(param_name, False)
        if bool_val != master_default:
            int_val = 1 if bool_val else 0
            f.write(f"3 {i} {int_val} '{param_name}' /\n")
```

**Example output:**
```
3 4 1 'LP4' /
```
Means: Type 3 (boolean), index 4 (LP4), value 1 (True)

#### Integer Changecards (lines 467-481)

```python
for i, param_name in enumerate(int_params, start=1):
    if param_name in params:
        # Skip N4 (davinci line 1037)
        if param_name == "N4":
            continue
        val = int(params[param_name])
        # Only write if different from master.inp default
        if param_name in MASTER_INP_DEFAULTS:
            if val != MASTER_INP_DEFAULTS[param_name]:
                f.write(f"2 {i} {val} '{param_name}' /\n")
        else:
            f.write(f"2 {i} {val} '{param_name}' /\n")
```

**Special case:** N4 is skipped because it's set directly in the header (multi-latitude handling)

**Example output:**
```
2 1 50 'N1' /
2 2 288 'N2' /
```

#### Float Changecards (lines 483-503)

```python
for i, param_name in enumerate(float_params, start=1):
    if param_name in params:
        val = float(params[param_name])
        should_write = False
        if param_name in MASTER_INP_DEFAULTS:
            # Compare with small tolerance for floating point
            if abs(val - MASTER_INP_DEFAULTS[param_name]) > 1e-6:
                should_write = True
        else:
            should_write = True

        if should_write:
            # Use scientific notation for small values
            if -1 < val < 1 and val != 0:
                f.write(f"1 {i} {val:.3E} '{param_name}' /\n")
            else:
                f.write(f"1 {i} {val:.4f} '{param_name}' /\n")
```

**Formatting rules:**
- Small values (-1 < val < 1, val ≠ 0): Scientific notation with 3 decimal places (e.g., `3.000E-01`)
- Other values: Fixed-point with 4 decimal places (e.g., `250.0000`)

**Example output:**
```
1 1 3.000E-01 'ALBEDO' /
1 3 250.0000 'INERTIA' /
```

### Filtering Mechanism

The `user_params` argument controls which parameters get changecards:

```python
# If user_params not provided, write all params (backward compatibility)
if user_params is None:
    user_params = params
```

**Purpose:** Distinguish between:
- Parameters **explicitly set by user** → write changecard
- Parameters **calculated internally** → may not need changecard if same as default

---

## Parameter Formatting Functions

### _write_param_line()

**Location:** `executor.py`, lines 328-353

**Purpose:** Write header blocks of 8 parameters each

```python
def _write_param_line(
    self,
    f,
    params: Dict[str, Any],
    param_names: List[str],
    is_int: bool = False,
    end_marker: bool = False
):
```

#### Format Specification

**Header line:**
- 10-character fields, right-aligned
- Parameter names in order

**Value line:**
- 10-character fields, right-aligned
- Integers: `f"{int(val):>10}"`
- Floats: `f"{float(val):>10.2f}"` (2 decimal places)
- End marker if `end_marker=True`: adds `"                                       end"`

**Example:**
```
    ALBEDO     EMISS   INERTIA     COND2     DENS2    PERIOD SPEC_HEAT   DENSITY
      0.25      1.00    200.00      2.77    928.00      1.03    647.00   1600.00
```

### _write_logical_flags()

**Location:** `executor.py`, lines 355-376

**Purpose:** Write boolean parameter blocks

```python
def _write_logical_flags(self, f, params: Dict[str, Any]):
```

#### Format Specification

**Two blocks:**
1. LP flags (LP1-LKofT): 10 parameters
2. LPORB flags (LPORB-L_ONE): 10 parameters

**Field width:** 7 characters, right-aligned

**Value format:**
- True → `"T"`
- False → `"F"`

**Example:**
```
    LP1    LP2    LP3    LP4    LP5    LP6 LPGLOB   LVFA   LVFT  LKofT
      F      T      F      F      F      F      F      F      F      F
  LPORB   LKEY    LSC  LZONE  LOCAL  Prt76 LPTAVE  Prt78  Prt79  L_ONE
      T      F      F      F      T      F      F      F      F      F
```

### _write_latitudes()

**Location:** `executor.py`, lines 378-387

**Purpose:** Write latitude array

```python
def _write_latitudes(self, f, params: Dict[str, Any]):
```

#### Format Specification

**Header:** `"Latitudes: in 10F7.2  _____7 _____7 _____7\n"`

**Values:**
- 10 values per line
- Format: `f"{lat:>7.2f}"` (7 chars, 2 decimals, right-aligned)
- Separated by single space

**Example:**
```
Latitudes: in 10F7.2  _____7 _____7 _____7
 -87.50 -80.00 -70.00 -60.00 -50.00 -40.00 -30.00 -20.00 -10.00   0.00
  10.00  20.00  30.00  40.00  50.00  60.00  70.00  80.00  87.50
```

### _write_elevations()

**Location:** `executor.py`, lines 389-398

**Purpose:** Write elevation array

```python
def _write_elevations(self, f, params: Dict[str, Any]):
```

#### Format Specification

**Header:** `"_____7 _____7 _____7 Elevations: in 10F7.2\n"`

**Values:**
- Same format as latitudes: 10 per line, `f"{elev:>7.2f}"`

**Example:**
```
_____7 _____7 _____7 Elevations: in 10F7.2
   3.51   2.01   1.39   1.22   0.38   0.48   1.17   1.67   1.26   0.17
```

---

## MASTER_INP_DEFAULTS Handling

**Location:** `executor.py`, lines 15-147

### Purpose

Defines the **baseline values** that appear in the input file header. These are the Version 356 master.inp defaults.

### Data Structure

```python
MASTER_INP_DEFAULTS = {
    # Line 1: KOLD KEEP
    "KOLD": 0,
    "KEEP": 0,

    # Lines 3-4: Material properties block 1
    "ALBEDO": 0.25,
    "EMISS": 1.00,
    "INERTIA": 200.0,
    "COND2": 2.77,
    "DENS2": 928.0,
    "PERIOD": 1.0275,
    "SPEC_HEAT": 647.0,
    "DENSITY": 1600.0,

    # ... (continues for all 108 parameters)
}
```

### Key Observations

#### Critical Value Differences from Old Defaults

**FANON:**
```python
"FANON": 0.055,  # NOTE: NOT 0.3!
```
Comment indicates this differs from an older default.

**TAURAT:**
```python
"TAURAT": 0.25,  # NOTE: NOT 2.0!
```
Another critical difference noted.

### Loading and Merging

The defaults are used in two ways:

1. **Header generation** (lines 236-291):
```python
self._write_param_line(f, MASTER_INP_DEFAULTS, [
    "ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2",
    "PERIOD", "SPEC_HEAT", "DENSITY"
])
```

2. **Changecard filtering** (lines 462, 476, 490):
```python
master_default = MASTER_INP_DEFAULTS.get(param_name, False)
if bool_val != master_default:
    # Write changecard
```

### Integration with core.py

**Location:** `core.py`, lines 362-363

```python
# Load master.inp defaults
master_params = parse_master_inp(paths.master_inp)
```

The `parse_master_inp()` function reads the actual master.inp file to extract defaults, which are then merged with user parameters:

```python
# Build full parameter set
params = master_params.copy()

# Override with user parameters
params.update({
    "ALBEDO": albedo_value,
    "INERTIA": INERTIA,
    # ...
})
```

---

## Parameter Ordering and Structure

### REAL*8 Parameters (64 total)

**Order in output file (lines 3-18):**

| Block | Lines | Parameters | Count |
|-------|-------|------------|-------|
| 1 | 3-4 | ALBEDO → DENSITY | 8 |
| 2 | 5-6 | CABR → SpHeat2 | 8 |
| 3 | 7-8 | TAUD → SLOAZI | 8 |
| 4 | 9-10 | TFROST → SatPrB | 8 |
| 5 | 11-12 | RLAY → DTMAX | 8 |
| 6 | 13-14 | DJUL → Atm_Cp | 8 |
| 7 | 15-16 | ConUp0 → ConLo3 | 8 |
| 8 | 17-18 | SphUp0 → SphLo3 | 8 |

### INTEGER*4 Parameters (24 total)

**Order in output file (lines 19-24):**

| Block | Lines | Parameters | Count |
|-------|-------|------------|-------|
| 9 | 19-20 | N1 → IC2 | 8 |
| 10 | 21-22 | NRSET → KPREF | 8 |
| 11 | 23-24 | K4OUT → IDISK2 | 4 (+end marker) |

### LOGICAL*4 Parameters (20 total)

**Order in output file (lines 25-28):**

| Block | Lines | Parameters | Count |
|-------|-------|------------|-------|
| 12 | 25-26 | LP1 → LKofT | 10 |
| 13 | 27-28 | LPORB → L_ONE | 10 |

### Latitude/Elevation Arrays

**Location in file:** Lines 29-34 (variable length)

**Format:**
- Latitudes: After boolean flags, header `"Latitudes: in 10F7.2"`
- Elevations: After latitudes, header `"_____7 _____7 _____7 Elevations: in 10F7.2"`

**Number of values:** Determined by `N4` parameter (number of latitudes)

### PORB Section

**Location:** Lines 33-41 (if LPORB=T)

**Structure:**
```
<PORB_HEADER>
<5 float values in G15.7 format>
<5 float values in G15.7 format>
<5 float values in G15.7 format>
<5 float values in G15.7 format>
<5 float values in G15.7 format>
<5 float values in G15.7 format>
```

**Total:** 30 orbital parameters

**Code (lines 297-312):**
```python
if params.get('LPORB', True):
    # Write PORB header line
    porb_header = params.get('PORB_HEADER',
        ' 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars')
    f.write(porb_header + '\n')

    # Write PORB parameters (30 values in 6 lines of 5G15.7 each)
    porb_params = params.get('PORB_PARAMS')
    if porb_params is not None:
        for i in range(0, len(porb_params), 5):
            chunk = porb_params[i:i+5]
            # G15.7 format: 15 characters wide, 7 decimal places
            line = ''.join(f"{val:15.7f}" for val in chunk)
            f.write(line + '\n')
```

**Format:** `G15.7` = 15 characters wide, 7 decimal places (Fortran G format)

---

## Special Cases Handling

### Multi-Latitude Runs

**N4 Calculation:**

In `core.py` (lines 795-800):
```python
# Set latitude/longitude (support single value or list)
if isinstance(lat, (list, tuple, np.ndarray)):
    latitudes = list(lat)
    N4 = len(latitudes)
else:
    latitudes = [lat]
    N4 = 1
```

**N4 Header Update:**

N4 is written directly to the header, NOT via changecard. From Davinci (krc.dvrc lines 1019-1022):
```davinci
#set N4 because KRC performs the check of # of elevations/latitudes prior to setting the change cards.
N4part2posy=maxpos(inp.part2==grep(inp.part2,"N4")[,1])[2]
N4part2posx=strstr(inp.part2[,N4part2posy],"N4")+1
inp.part2[N4part2posx-9:N4part2posx,N4part2posy+1]=sprintf("%10s",N4+"")
```

**PyKRC equivalent:** N4 value is written in the header (line 20 of input file), and skipped in changecards (executor.py line 472):
```python
if param_name == "N4":
    continue
```

### Time-Varying Arrays

**ALBEDO and TAUD** can be time-varying arrays.

**Detection in core.py (lines 591-596):**
```python
# Handle ALBEDO (can be time-varying array)
if isinstance(ALBEDO, (list, tuple, np.ndarray)):
    albedo_value = list(ALBEDO)
    albedo_is_array = True
else:
    albedo_value = ALBEDO
    albedo_is_array = False
```

**Changecard handling in Davinci (krc.dvrc lines 1055-1074):**
```davinci
# if ALBEDO is an array, do a special thing
if (max(dim(ALBEDO))>1 && key.part1.key[,i]=="ALBEDO") {
    # Write array to albfile.tab
    write(var, workdir+"/albfile.tab", type=ascii, force=1)
    params=cat(params, sprintf("8 22 0 '%s/albfile.tab' /", workdir), y)
}
```

**PyKRC STATUS:** Not yet implemented in changecard writer. Arrays are stored in params but not written as Type 8 file changecards.

### Eclipse/Planetary Flux Parameters

**Eclipse parameters** (from core.py lines 724-741):
```python
if Eclipse == "T":
    if Eclipser is not None:
        params["Eclipser"] = Eclipser
    if Sun_Dis is not None:
        params["Sun_Dis"] = Sun_Dis
    # ... etc
```

**Planetary flux parameters** (lines 744-757):
```python
if PFlux == "T":
    params["PFlux"] = True
    if BT_Avg is not None:
        params["BT_Avg"] = BT_Avg
    # ... etc
```

**PyKRC STATUS:** Parameters stored but not written to input file. May require custom changecard types or special formatting.

### Two-Layer Configurations

**IC2 calculation** in `core.py` (lines 556-559):
```python
# Calculate IC2 if not explicitly provided
if IC2 is None:
    IC2 = calculate_IC2(thick, N1, FLAY, RLAY)
```

**IC2 in input file:**
- Written to header (line 20)
- Written as changecard if different from default (999)

**Validation** (line 553):
```python
# Validate two-layer configuration
validate_two_layer_config(thick, INERTIA, INERTIA2, Mat1, Mat2, Por1, Por2)
```

**Special handling:** If `thick > 0`, upper and lower material properties differ:
- Upper: ConUp0-ConUp3, SphUp0-SphUp3
- Lower: ConLo0-ConLo3, SphLo0-SphLo3

---

## Parameter Reference Table

### Complete Parameter Mapping

| Parameter | Type | Default | Index | Block | Description |
|-----------|------|---------|-------|-------|-------------|
| **Material Properties** |
| ALBEDO | float | 0.25 | 1 | 1 | Surface albedo |
| EMISS | float | 1.00 | 2 | 1 | Surface emissivity |
| INERTIA | float | 200.0 | 3 | 1 | Thermal inertia (SI) |
| COND2 | float | 2.77 | 4 | 1 | Lower layer conductivity |
| DENS2 | float | 928.0 | 5 | 1 | Lower layer density |
| PERIOD | float | 1.0275 | 6 | 1 | Rotation period (days) |
| SPEC_HEAT | float | 647.0 | 7 | 1 | Upper layer specific heat |
| DENSITY | float | 1600.0 | 8 | 1 | Upper layer density |
| **Atmosphere** |
| CABR | float | 0.11 | 9 | 2 | C/A ratio |
| AMW | float | 43.5 | 10 | 2 | Atmospheric molecular weight |
| SatPrA | float | 27.9546 | 11 | 2 | Saturation pressure A |
| PTOTAL | float | 546.0 | 12 | 2 | Total pressure (Pa) |
| FANON | float | 0.055 | 13 | 2 | Atmospheric anisotropy |
| TATM | float | 200.0 | 14 | 2 | Atmospheric temperature |
| TDEEP | float | 180.0 | 15 | 2 | Deep temperature |
| SpHeat2 | float | 1711.0 | 16 | 2 | Lower layer specific heat |
| **Opacity/Slope** |
| TAUD | float | 0.3 | 17 | 3 | Dust optical depth |
| DUSTA | float | 0.90 | 18 | 3 | Dust absorptivity |
| TAURAT | float | 0.25 | 19 | 3 | Tau ratio vis/IR |
| TWILI | float | 0.0 | 20 | 3 | Twilight parameter |
| ARC2_G0 | float | 0.5 | 21 | 3 | Orbital parameter |
| ARC3_Safe | float | 0.801 | 22 | 3 | Safety parameter |
| SLOPE | float | 0.0 | 23 | 3 | Surface slope (deg) |
| SLOAZI | float | 90.0 | 24 | 3 | Slope azimuth (deg) |
| **Frost** |
| TFROST | float | 146.0 | 25 | 4 | Frost temperature |
| CFROST | float | 589944.0 | 26 | 4 | Frost heat capacity |
| AFROST | float | 0.65 | 27 | 4 | Frost albedo |
| FEMIS | float | 0.95 | 28 | 4 | Frost emissivity |
| AF1 | float | 0.54 | 29 | 4 | Frost parameter 1 |
| AF2 | float | 0.0009 | 30 | 4 | Frost parameter 2 |
| FROEXT | float | 50.0 | 31 | 4 | Frost extinction |
| SatPrB | float | 3182.48 | 32 | 4 | Saturation pressure B |
| **Layers** |
| RLAY | float | 1.15 | 33 | 5 | Layer ratio |
| FLAY | float | 0.10 | 34 | 5 | First layer fraction |
| CONVF | float | 3.0 | 35 | 5 | Convergence factor |
| DEPTH | float | 0.0 | 36 | 5 | Depth parameter |
| DRSET | float | 0.0 | 37 | 5 | Reset parameter |
| PhotoFunc | float | 0.0 | 38 | 5 | Photometric function |
| GGT | float | 0.1 | 39 | 5 | Convergence parameter |
| DTMAX | float | 0.1 | 40 | 5 | Max time step |
| **Orbital** |
| DJUL | float | -1222.69 | 41 | 6 | Julian date offset |
| DELJUL | float | 17.174822 | 42 | 6 | Delta Julian date |
| SOLARDEC | float | 0.0 | 43 | 6 | Solar declination |
| DAU | float | 1.465 | 44 | 6 | Distance (AU) |
| LsubS | float | 0.0 | 45 | 6 | Latent heat sublimation |
| SOLCON | float | 1368.0 | 46 | 6 | Solar constant |
| GRAV | float | 3.727 | 47 | 6 | Gravity (m/s²) |
| Atm_Cp | float | 735.9 | 48 | 6 | Atmospheric heat capacity |
| **Conductivity Coefficients** |
| ConUp0 | float | 0.038640 | 49 | 7 | Upper layer K(T) c₀ |
| ConUp1 | float | -0.002145 | 50 | 7 | Upper layer K(T) c₁ |
| ConUp2 | float | 0.002347 | 51 | 7 | Upper layer K(T) c₂ |
| ConUp3 | float | -0.000750 | 52 | 7 | Upper layer K(T) c₃ |
| ConLo0 | float | 2.766722 | 53 | 7 | Lower layer K(T) c₀ |
| ConLo1 | float | -1.298966 | 54 | 7 | Lower layer K(T) c₁ |
| ConLo2 | float | 0.629224 | 55 | 7 | Lower layer K(T) c₂ |
| ConLo3 | float | -0.527291 | 56 | 7 | Lower layer K(T) c₃ |
| **Specific Heat Coefficients** |
| SphUp0 | float | 646.6275 | 57 | 8 | Upper layer Cp(T) c₀ |
| SphUp1 | float | 246.6678 | 58 | 8 | Upper layer Cp(T) c₁ |
| SphUp2 | float | -49.8216 | 59 | 8 | Upper layer Cp(T) c₂ |
| SphUp3 | float | 7.9520 | 60 | 8 | Upper layer Cp(T) c₃ |
| SphLo0 | float | 1710.648 | 61 | 8 | Lower layer Cp(T) c₀ |
| SphLo1 | float | 721.8740 | 62 | 8 | Lower layer Cp(T) c₁ |
| SphLo2 | float | 57.44873 | 63 | 8 | Lower layer Cp(T) c₂ |
| SphLo3 | float | 24.37532 | 64 | 8 | Lower layer Cp(T) c₃ |
| **Grid Parameters** |
| N1 | int | 28 | 1 | 9 | Number of layers |
| N2 | int | 1536 | 2 | 9 | Timesteps per day |
| N3 | int | 15 | 3 | 9 | Convergence days |
| N4 | int | 19 | 4 | 9 | Number of latitudes |
| N5 | int | 120 | 5 | 9 | Number of seasons |
| N24 | int | 48 | 6 | 9 | Output times per day |
| IIB | int | 0 | 7 | 9 | Bottom boundary |
| IC2 | int | 999 | 8 | 9 | Two-layer interface |
| **Run Control** |
| NRSET | int | 3 | 9 | 10 | Reset parameter |
| NMHA | int | 24 | 10 | 10 | Hours parameter |
| NRUN | int | 0 | 11 | 10 | Run number |
| JDISK | int | 81 | 12 | 10 | Disk output start |
| IDOWN | int | 0 | 13 | 10 | Downwelling parameter |
| FlxP14 | int | 45 | 14 | 10 | Flux parameter |
| TUN_Flx15 | int | 65 | 15 | 10 | Tuning flux |
| KPREF | int | 1 | 16 | 10 | Pressure reference |
| **Output Control** |
| K4OUT | int | 52 | 17 | 11 | Output format |
| JBARE | int | 0 | 18 | 11 | Bare season |
| Notif | int | 50 | 19 | 11 | Notification |
| IDISK2 | int | 0 | 20 | 11 | Disk 2 parameter |
| **Boolean Flags** |
| LP1 | bool | False | 1 | 12 | Print flag 1 |
| LP2 | bool | True | 2 | 12 | Print parameters |
| LP3 | bool | False | 3 | 12 | Print flag 3 |
| LP4 | bool | False | 4 | 12 | Print flag 4 |
| LP5 | bool | False | 5 | 12 | Print flag 5 |
| LP6 | bool | False | 6 | 12 | Print flag 6 |
| LPGLOB | bool | False | 7 | 12 | Global print |
| LVFA | bool | False | 8 | 12 | Variable frost albedo |
| LVFT | bool | False | 9 | 12 | Variable frost temp |
| LKofT | bool | False | 10 | 12 | K(T) dependency |
| LPORB | bool | True | 11 | 13 | Use PORB |
| LKEY | bool | False | 12 | 13 | Use Ls input |
| LSC | bool | False | 13 | 13 | Self-consistent |
| LZONE | bool | False | 14 | 13 | Zone file |
| LOCAL | bool | True | 15 | 13 | Local time |
| Prt76 | bool | False | 16 | 13 | Print 76 |
| LPTAVE | bool | False | 17 | 13 | Print average |
| Prt78 | bool | False | 18 | 13 | Print 78 |
| Prt79 | bool | False | 19 | 13 | Print 79 |
| L_ONE | bool | False | 20 | 13 | One-point mode |

---

## Issues and Discrepancies

### 1. Missing PORB Parameter Handling

**Issue:** PORB_PARAMS are written to file but no validation or generation if missing.

**Location:** `executor.py`, lines 304-312

**Current behavior:**
```python
porb_params = params.get('PORB_PARAMS')
if porb_params is not None:
    for i in range(0, len(porb_params), 5):
        # Write params
```

**Problem:** If `PORB_PARAMS` is None, nothing is written. KRC may fail.

**Expected behavior (from Davinci):** PORB should be generated via `porb()` function if not provided.

### 2. N4 Changecard Skipping

**Issue:** N4 is skipped in changecards but not updated in header directly.

**Location:** `executor.py`, line 472

```python
if param_name == "N4":
    continue
```

**PyKRC approach:** N4 is written to header via `MASTER_INP_DEFAULTS`, then overridden if user provides different value. But the header write uses the default (19), not the calculated value from `len(latitudes)`.

**Fix needed:** Update header writing to use actual N4 value from params, not always MASTER_INP_DEFAULTS.

### 3. Time-Varying Arrays Not Implemented

**Issue:** ALBEDO and TAUD arrays are stored but not written as Type 8 file changecards.

**Location:** Missing in `_write_changecards()`

**Expected behavior (from Davinci krc.dvrc lines 1055-1074):**
```davinci
if (max(dim(ALBEDO))>1 && key.part1.key[,i]=="ALBEDO") {
    write(var, workdir+"/albfile.tab", type=ascii, force=1)
    params=cat(params, sprintf("8 22 0 '%s/albfile.tab' /", workdir), y)
}
```

**Fix needed:** Implement array file writing and Type 8 changecards for:
- ALBEDO → `8 22 0 'path/albfile.tab' /`
- TAUD → `8 23 0 'path/taufile.tab' /`

### 4. Eclipse/Planetary Flux Parameters

**Issue:** Parameters are stored but not written to input file.

**Location:** `core.py` lines 724-757 store params, but no writing logic in executor.

**Expected behavior:** Custom changecard lines for eclipse/flux parameters.

**Status:** Unclear if these need special Type 8 changecards or custom format.

### 5. Missing PORB Header Default

**Issue:** Default PORB header is hardcoded in executor.

**Location:** `executor.py`, lines 300-301

```python
porb_header = params.get('PORB_HEADER',
    ' 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars')
```

**Problem:** This is specific to Mars. Other bodies need different headers.

**Expected behavior:** PORB header should come from `porb()` function in orbital.py.

### 6. Float Precision Issues

**Issue:** Float parameters formatted with only 2 decimal places in header.

**Location:** `executor.py`, line 348

```python
values.append(f"{float(val):>10.2f}")
```

**Example:** `FANON: 0.055` becomes `0.06` (rounded)

**Impact:** Small values lose precision in header, though changecards have correct precision.

**Not a bug:** Header values don't matter if changecard is written. But could cause confusion.

### 7. Boolean Conversion Complexity

**Issue:** Multiple conversion paths for booleans.

**Location:** `executor.py`, lines 452-459

```python
if isinstance(val, bool):
    bool_val = val
elif isinstance(val, str):
    bool_val = val.upper() == 'T'
else:
    bool_val = bool(val)
```

**Problem:** Inconsistent handling. Some code uses `"T"/"F"` strings, some uses Python bools.

**Impact:** Could lead to incorrect conversions if mixed types passed.

### 8. Changecard Order

**Issue:** Changecards written in order: booleans → integers → floats.

**Location:** `executor.py`, lines 448-503

**PyKRC order:**
1. Type 3 (boolean)
2. Type 2 (integer)
3. Type 1 (float)

**Davinci order (krc.dvrc lines 1026-1089):**
1. Type 3 (boolean)
2. Type 2 (integer)
3. Type 1 (float)

**Status:** ✓ Matches Davinci

### 9. Disk File Specification Timing

**Issue:** Disk file changecard (Type 8, index 5) must come AFTER other changecards.

**Location:** `executor.py`, lines 319-321

**Code:**
```python
# Disk file specification (change card type 8) - must come AFTER changecards
k4out = params.get('K4OUT', 52)
output_filename = f"./{filename.replace('.inp', f'.t{k4out}')}"
f.write(f"8 5 0 '{output_filename}' /\n")
```

**Comment confirms correct ordering:** "must come AFTER changecards"

**Status:** ✓ Correct

### 10. Missing Validation

**Issue:** No validation that all required parameters are present.

**Location:** No validation before writing file.

**Potential problems:**
- Missing Latitudes → crash
- Missing Elevations → crash
- LPORB=T but no PORB_PARAMS → silent failure

**Fix needed:** Add validation in `create_input_file()` before writing.

---

## Example Generated File

**Test case:** Single latitude, ALBEDO=0.30, INERTIA=250, N1=50, LP4=True

```
  1: 0 0 / KOLD: season to start with;  KEEP: continue saving data in same disk file
  2: Version 356 default values.  19 latitudes with mean Mars zonal elevations
  3:     ALBEDO     EMISS   INERTIA     COND2     DENS2    PERIOD SPEC_HEAT   DENSITY
  4:       0.25      1.00    200.00      2.77    928.00      1.03    647.00   1600.00
  5:       CABR       AMW    SatPrA    PTOTAL     FANON      TATM     TDEEP   SpHeat2
  6:       0.11     43.50     27.95    546.00      0.06    200.00    180.00   1711.00
  7:       TAUD     DUSTA    TAURAT     TWILI   ARC2_G0 ARC3_Safe     SLOPE    SLOAZI
  8:       0.30      0.90      0.25      0.00      0.50      0.80      0.00     90.00
  9:     TFROST    CFROST    AFROST     FEMIS       AF1       AF2    FROEXT    SatPrB
 10:     146.00 589944.00      0.65      0.95      0.54      0.00     50.00   3182.48
 11:       RLAY      FLAY     CONVF     DEPTH     DRSET PhotoFunc       GGT     DTMAX
 12:       1.15      0.10      3.00      0.00      0.00      0.00      0.10      0.10
 13:       DJUL    DELJUL  SOLARDEC       DAU     LsubS    SOLCON      GRAV    Atm_Cp
 14:   -1222.69     17.17      0.00      1.47      0.00   1368.00      3.73    735.90
 15:     ConUp0    ConUp1    ConUp2    ConUp3    ConLo0    ConLo1    ConLo2    ConLo3
 16:       0.04     -0.00      0.00     -0.00      2.77     -1.30      0.63     -0.53
 17:     SphUp0    SphUp1    SphUp2    SphUp3    SphLo0    SphLo1    SphLo2    SphLo3
 18:     646.63    246.67    -49.82      7.95   1710.65    721.87     57.45     24.38
 19:         N1        N2        N3        N4        N5       N24       IIB       IC2
 20:         28      1536        15        19       120        48         0       999
 21:      NRSET      NMHA      NRUN     JDISK     IDOWN    FlxP14 TUN_Flx15     KPREF
 22:          3        24         0        81         0        45        65         1
 23:      K4OUT     JBARE     Notif    IDISK2
 24:         52         0        50         0                                       end
 25:     LP1    LP2    LP3    LP4    LP5    LP6 LPGLOB   LVFA   LVFT  LKofT
 26:       F      T      F      F      F      F      F      F      F      F
 27:   LPORB   LKEY    LSC  LZONE  LOCAL  Prt76 LPTAVE  Prt78  Prt79  L_ONE
 28:       T      F      F      F      T      F      F      F      F      F
 29: Latitudes: in 10F7.2  _____7 _____7 _____7
 30:    0.00
 31: _____7 _____7 _____7 Elevations: in 10F7.2
 32:    0.00
 33:  2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars
 34: 3 4 1 'LP4' /
 35: 2 1 50 'N1' /
 36: 2 2 288 'N2' /
 37: 1 1 3.000E-01 'ALBEDO' /
 38: 1 3 250.0000 'INERTIA' /
 39: 8 5 0 './krc.t52' /
 40: 0/
 41: 0/
```

**Analysis:**
- Lines 1-28: Header with MASTER_INP_DEFAULTS
- Lines 29-32: Single latitude/elevation (N4=1)
- Line 33: PORB header (Mars default)
- Lines 34-38: Changecards (4 parameters modified)
- Lines 39-41: Disk file + termination

**Changecards breakdown:**
- `3 4 1 'LP4' /` → Boolean LP4 (index 4) set to 1 (True)
- `2 1 50 'N1' /` → Integer N1 (index 1) set to 50
- `2 2 288 'N2' /` → Integer N2 (index 2) set to 288
- `1 1 3.000E-01 'ALBEDO' /` → Float ALBEDO (index 1) set to 0.3 (scientific notation)
- `1 3 250.0000 'INERTIA' /` → Float INERTIA (index 3) set to 250.0

---

## Comparison with Davinci Implementation

### Davinci Key Structures

**From krc.dvrc lines 1594-1616:**

```davinci
# Floating point variables (#1 in change cards)
key.part1 = krc_get_key_value(inp.part1, print=usage)

# Integer variables (#2 in change cards)
key.part2 = krc_get_key_value(inp.part2, print=usage)

# Boolean variables (#3 in change cards)
key.part3 = krc_get_key_value(inp.part3, print=usage)
```

The `key.partX.key` arrays contain parameter names in order, matching PyKRC's `float_params`, `int_params`, `bool_params`.

### Changecard Generation Flow (Davinci)

**From krc.dvrc lines 1026-1089:**

```davinci
# Start with part 3: the boolean values
for(i=1; i<=length(key.part3.key); i+=1) {
    if(HasValue(param)) {
        params = cat(params, sprintf("3 %i %i '%s' /", i, atob(val), name), axis=y)
    }
}

# Then part 2: the integer values
for(i=1; i<=length(key.part2.key); i+=1) {
    if(HasValue(param) && name != "N4") {
        params = cat(params, sprintf("2 %i %i '%s' /", i, val, name), axis=y)
    }
}

# Then part 1: the floating point values
for(i=1; i<=length(key.part1.key); i+=1) {
    if(HasValue(param)) {
        if(val < 1 && val > -1) {
            params = cat(params, sprintf("1 %i %.3E '%s' /", i, val, name), axis=y)
        } else {
            params = cat(params, sprintf("1 %i %.4f '%s' /", i, val, name), axis=y)
        }
    }
}
```

**Matches PyKRC:** ✓ Same order, same formatting

### Differences

1. **Davinci** checks `HasValue()` (is variable defined in scope)
   **PyKRC** checks `if param_name in params` (is key in dict)

2. **Davinci** always writes changecards for any `HasValue()` parameter
   **PyKRC** only writes if different from `MASTER_INP_DEFAULTS` OR if no default exists

3. **Davinci** uses `atob()` to convert "T"/"F" to 1/0
   **PyKRC** has more complex boolean conversion logic

---

## Recommendations

### Priority Fixes

1. **Fix N4 header writing** to use actual value from params, not always default
2. **Implement time-varying array support** (ALBEDO/TAUD Type 8 changecards)
3. **Add input validation** before writing file
4. **Fix PORB header generation** for non-Mars bodies

### Enhancement Opportunities

1. Add support for eclipse/flux parameters
2. Improve boolean type handling consistency
3. Add more detailed logging of what changecards are written
4. Create unit tests comparing generated files with Davinci output

---

## Conclusion

PyKRC's input file generation follows the KRC Fortran format correctly for basic use cases. The changecard system matches Davinci's implementation closely. However, several advanced features (time-varying arrays, eclipse parameters) are not yet fully implemented, and there are minor precision/validation issues to address.

The architecture is sound and extensible. Most issues can be fixed by:
1. Adding missing Type 8 changecard logic for arrays/files
2. Improving validation at file creation time
3. Ensuring N4 and PORB parameters are correctly propagated from calculated values

---

**Document Version:** 1.0
**Last Updated:** 2025-10-09
**Author:** Analysis generated for KRC Python interface development
