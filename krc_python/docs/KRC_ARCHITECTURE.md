# KRC Architecture and Runtime Environment

**Last Updated:** 2025-10-20
**Purpose:** Comprehensive technical reference for KRC thermal model architecture, execution requirements, and I/O specifications

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Program Architecture](#program-architecture)
3. [Execution Requirements](#execution-requirements)
4. [Input System](#input-system)
5. [Output System](#output-system)
6. [Convergence and Runtime](#convergence-and-runtime)
7. [Common Pitfalls](#common-pitfalls)
8. [Python Interface Guidelines](#python-interface-guidelines)
9. [Appendix: Binary Format Specifications](#appendix-binary-format-specifications)

---

## Executive Summary

**KRC (Kieffer Rapid Calculation)** is a Fortran-based planetary surface thermal model that uses explicit forward finite differences to solve the 1D heat equation.

### Key Characteristics

- **Interactive execution**: Expects two filenames via stdin (without extensions)
- **Long runtime**: Full seasonal runs take minutes to hours to reach thermal equilibrium
- **Complex input**: ~96 real parameters, ~40 integer parameters, ~20 logical flags
- **Multiple output modes**: 6 binary output formats (bin5, direct-access Fortran)
- **Nested architecture**: KRC → TSEAS → TLATS → TDAY hierarchy
- **Two execution modes**: Full seasonal model or "one-point" single calculation

### Critical Requirements

1. **Required data files**: `standish.tab`, `spinaxis.tab`, `PORBCM.mat` must be in working directory
2. **Stdin input**: Must provide two filenames (input and output base names)
3. **Precise formatting**: Input files must follow exact Fortran format specifications
4. **Stability constraints**: `CONVF >= 0.8` for numerical stability
5. **Generous timeouts**: 5-30 minutes for full runs

---

## Program Architecture

### Call Hierarchy

```
KRC (krc8.f)
├── Reads input parameters from .inp file
├── Loops over "seasons" (J5 index)
│   └── TSEAS (tseas.f)
│       ├── Loops over latitudes (J4 index)
│       │   └── TLATS (tlats.f)
│       │       ├── Calculates insolation and atmospheric conditions
│       │       ├── Loops over days (J3 index)
│       │       │   └── TDAY (tday.f)
│       │       │       ├── Loops over times of day (J2 index)
│       │       │       ├── Loops over layers (depth)
│       │       │       ├── Solves 1D heat equation
│       │       │       └── Convergence prediction (TAU=X²/2κ)
│       │       └── Checks convergence (DTMAX, DDT, GGT)
│       └── Global integrations (if N4 > 8)
└── Disk output (TDISK)
```

### Common Blocks (Shared Memory)

KRC uses Fortran COMMON blocks for shared data:

- **KRCCOM**: Main parameters (~96 reals, ~40 ints, ~20 logicals, TITLE, DAYTIM)
- **LATCOM**: Latitude-specific arrays (temperatures, frost, heat flow)
- **DAYCOM**: Diurnal arrays (hourly temperatures, insolation)
- **HATCOM**: "Catch-all" for new items (added 2004)
- **UNITS**: I/O unit numbers
- **FILCOM**: Filenames
- **PORBCM**: Orbital parameters (from PORB program)

### Key Dimensions (from krccom.inc)

```fortran
PARAMETER (MAXN1 = 30)       ! max layers
PARAMETER (MAXN2 = 384*4)    ! max times per day (1536)
PARAMETER (MAXN3 = 100)      ! max days to iterate
PARAMETER (MAXN4 = 37)       ! max latitudes
PARAMETER (MAXN5 = 161)      ! max seasons
PARAMETER (KOMMON = 10000000) ! storage for binary file
```

---

## Execution Requirements

### Interactive Input Method

KRC expects **two filenames via stdin** (without extensions):

```bash
echo -e "krc\nkrc" | ./krc
```

This provides:
- **Line 1**: Input filename → KRC adds `.inp` extension
- **Line 2**: Output filename → KRC adds `.prt` extension

Files must exist in the **current working directory**.

### Required Data Files

When `LPORB=T` (orbital calculations enabled), these files **must** be present in the working directory:

| File | Purpose | Size | Required |
|------|---------|------|----------|
| `standish.tab` | Planetary ephemeris data (JPL) | ~2.7KB | Yes |
| `spinaxis.tab` | Spin axis orientation | ~1.8KB | Yes |
| `PORBCM.mat` | Binary orbital common block | ~8.4KB | Yes |

**Critical**: Without these files, KRC will:
- Hang waiting for keyboard input
- Crash silently
- Produce incorrect orbital calculations

### Working Directory Setup

**Recommended approach**:

1. Create temporary working directory
2. Copy all three required files to working directory
3. Generate `.inp` file in working directory
4. Execute KRC from working directory (using `cwd=` parameter)
5. Read output files from working directory
6. Clean up temporary directory (unless debugging)

---

## Input System

### Input File Structure

```
Line 1:      KOLD KEEP [IDB1-6]       (I*, optional debug flags)
Line 2:      Title (80 characters)    (20A4)
Lines 3-10:  Real parameters          (8F10.0) - 8 values per line, 64 total
Lines 11-13: Integer parameters       (8I10)   - 8 values per line, 20 total
Lines 14-15: Logical flags            (10L7)   - 10 values per line, 20 total
Line 16+:    Latitudes                (10F7.2) - N4 values
Line 17+:    Elevations               (10F7.2) - N4 values
Lines 18+:   PORB orbital data        (5G15.7) - IF LPORB=T, 30 values
Lines 19+:   Change cards             (various formats)
Final:       0/ (end case), 0/ (end run)
```

### Parameter Types

**Real Parameters (64 total, 8F10.0 format)**:
- Physical properties: ALBEDO, EMISS, INERTIA, COND2, DENS2, PERIOD, SPEC_HEAT, DENSITY
- Atmospheric: CABR, AMW, PTOTAL, FANON, TATM, TDEEP
- Opacity/geometry: TAUD, DUSTA, SLOPE, SLOAZI
- Frost: TFROST, CFROST, AFROST, FEMIS, AF1, AF2, FROEXT
- Thermal solution: RLAY, FLAY, CONVF, DEPTH, DRSET, DDT, GGT, DTMAX
- Orbital: DJUL, DELJUL, SOLARDEC, DAU, LsubS, SOLCON, GRAV, AtmCp
- T-dependent coefficients: ConUp0-3, ConLo0-3, SphUp0-3, SphLo0-3

**Integer Parameters (20 total, 8I10 format)**:
- N1: Number of layers
- N2: Number of times per day
- N3: Maximum days to iterate
- N4: Number of latitudes
- N5: Number of seasons
- N24: Output timesteps per day
- IIB: Bottom boundary condition
- IC2: Layer where properties change
- NRSET: Days before reset
- JDISK: Season to begin disk output
- K4OUT: Disk output format

**Logical Parameters (20 total, 10L7 format)**:
- LP1-LP6: Print control flags
- LPGLOB: Print global parameters
- LVFA: Variable frost albedo
- LVFT: Variable frost temperature
- LKofT: Temperature-dependent K and Cp
- LPORB: Call PORB1 for orbital calculations
- LOCAL: Use layer-specific scaling

### Critical Formatting Rules

**Format precision is critical!** From KRC documentation:

> "Parameter values are listed below their titles, which are in many cases identical to the code name, and last character of the title is above the last location in the field. Thus, integer values MUST be aligned."

**Format specifications**:
- Real: `F10.0` format (10 characters wide, right-aligned)
- Integer: `I10` format (10 characters wide, right-aligned)
- Logical: `L7` format (7 characters, 'T' or 'F')
- Latitude/Elevation: `F7.2` (7 characters, 2 decimals)

**Example**:
```fortran
    ALBEDO     EMISS   INERTIA     COND2     DENS2    PERIOD SPEC_HEAT   DENSITY
      0.25      1.00     200.0      2.77     928.0    1.0275      647.     1600.
```

**Alignment errors cause**:
- Silent read failures
- Wrong parameter values
- Crashes in numerical routines

### Change Cards

Change cards modify parameters between runs:

```
<TYPE> <INDEX> <VALUE> '<COMMENT>' /
```

**Types**:
- Type 0: End of changes
- Type 1: Real parameter
- Type 2: Integer parameter
- Type 3: Logical parameter (0=F, 1=T)
- Type 4: New latitude cards
- Type 5: New elevation cards
- Type 7: New title
- Type 8: New disk filename (subtype 5 for output: `8 5 0 'path' /`)
- Type 10: Switch to one-point mode

### One-Point Mode

Special mode for fast single-point calculations without full seasonal runs.

**Activation**: Use change card in main input:
```
10 1 0.1 'oneA.one' / Switch to one-point mode
```

**One-point file format** (`oneA.one`):
```
Title Line, Text transferred to output
11    Ls   Lat Hour  Elev  Alb Inerti Opac Slop Azim
11 100.0  22.3 13.5  -3.1 0.20  200.0 0.30  0.0  90. Viking Lander 1
11  90.0  47.7 13.5  -3.0 0.20  100.0 0.20  0.0   0. Viking Lander 2
```

Each `11` line produces one output line with surface and planck temperatures.

---

## Output System

### Print File (.prt)

Human-readable ASCII output controlled by LP1-LP6 and LPGLOB flags:

- **LP1**: Program description
- **LP2**: All parameters and changes
- **LP3**: Hourly conditions on last day
- **LP4**: Daily convergence summary
- **LP5**: Latitude summary
- **LP6**: TMIN/TMAX vs latitude and layer
- **LPGLOB**: Global parameters each season

### Binary Output Formats (K4OUT parameter)

#### Direct-Access Fortran Files (K4OUT ≤ 50)

- **K4OUT = -1**: KRCCOM, then TSF & TPF
- **K4OUT = 0**: KRCCOM + LATCOM each season (standard for databases)
- **K4OUT = 1-49**: KRCCOM + DAYCOM for latitude [K4OUT] each season

#### Bin5 Files (K4OUT > 50)

All bin5 files include a "prefix" with KRCCOM and allow multiple cases.

- **Type 51**: `(N24, 2, N4, seasons, cases)`
  - 2 items: TSF (surface temp), TPF (planck temp)
  - Prefix: DJU5, SUBS, PZREF, TAUD, SUMF

- **Type 52**: `(N24, 7, N4, seasons, cases)` — **Recommended**
  - 7 items: TSF, TPF, TAF, DOWNVIS, DOWNIR, TIN (packed layer temps), FROST4 (packed frost/heat)
  - Most comprehensive output format

- **Type 54**: `(seasons, 5, N4, cases)`
  - 5 items: TSF@1am, TSF@13h, HEATMM, FROST4, TTB4

- **Type 55**: `(seasons, 9, cases)` — One latitude only
  - 9 items: Various temperature snapshots, heat flow, frost budget

- **Type 56**: `(vectors, N4, seasons, cases)`
  - All hours TSF/TPF, all layer temps at midnight, frost, heat

---

## Convergence and Runtime

### Convergence Approach

KRC runs multiple seasons to reach thermal equilibrium:

1. **Initial seasons**: Layers reset after NRSET days to speed convergence
2. **Subsequent seasons**: Temperature prediction using TAU = X²/(2κ) time constant
3. **Convergence tests**:
   - **DDT** < 0.002: RMS 2nd differences
   - **GGT** < 0.1: Surface boundary iteration
   - **DTMAX** < 0.1: RMS layer temperature changes per day

### Runtime Estimates

**Approximate formula**:
```
Runtime ≈ N1 × N2 × N3 × N4 × N5 × (10-50 µs per iteration)
```

**Examples**:
- **Default run**: 28 × 1536 × 15 × 19 × 120 = 1.5 billion iterations ≈ **5-30 minutes**
- **Fast test**: 10 × 96 × 3 × 1 × 3 = 8,640 iterations ≈ **0.1-1 seconds**
- **One-point mode**: Seconds per calculation

**Spin-up period**: First 3 seasons (NRSET=3) are most expensive

### Stability Requirements

**Critical constraint**:
```
CONVF >= 0.8 for numerical stability
```

**Recommended**: `CONVF = 2.0` to `4.0` for stable calculations

**If CONVF < 0.8**:
- Error: "Parameter error in TDAY(1): Instability anticipated"
- Numerical blowup
- Temperature divergence
- SIGKILL from system

### Fast Testing Parameters

To avoid iteration waiting for convergence:
```python
GGT = 99.0      # Disable surface iteration convergence check
NRSET = 999     # Disable layer resets
N3 = 1          # Turn off prediction
```

### Timeout Recommendations

For Python subprocess execution:
- **Fast tests**: 60 seconds
- **Production runs**: 300-1800 seconds (5-30 minutes)
- **One-point mode**: 60 seconds

---

## Common Pitfalls

### 1. Missing stdin input
**Symptom**: KRC hangs indefinitely
**Cause**: Not providing filenames via stdin
**Solution**: Use `subprocess.run(..., input="krc\nkrc\n")`

### 2. Missing data files
**Symptom**: KRC crashes or hangs
**Cause**: standish.tab, spinaxis.tab, or PORBCM.mat not in working directory
**Solution**: Copy all three files before execution

### 3. Timeout too short
**Symptom**: Process killed before completion
**Cause**: Default runs take 5-30 minutes
**Solution**: Set timeout ≥ 300 seconds for production runs

### 4. Unstable parameters
**Symptom**: SIGKILL or numerical errors
**Cause**: CONVF < 0.8
**Solution**: Ensure CONVF ≥ 2.0

### 5. Wrong architecture byte order
**Symptom**: Garbage data in binary output
**Cause**: Bin5 files may need byte-swapping on different platforms
**Solution**: Use bin5_reader which handles automatic byte swapping

### 6. Fortran vs NumPy ordering
**Symptom**: Incorrect array dimensions
**Cause**: Bin5 data is Fortran column-major
**Solution**: Transpose arrays appropriately

### 7. Change card syntax errors
**Symptom**: KRC crashes on input
**Cause**: Incorrect format or missing quotes
**Solution**: Follow exact format: `TYPE INDEX VALUE 'NAME' /`

### 8. LPORB without orbital data
**Symptom**: Crash or incorrect results
**Cause**: LPORB=T but no PORB cards in input
**Solution**: Always provide 30 PORB values when LPORB=T

### 9. Misaligned input parameters
**Symptom**: Strange results or crashes
**Cause**: Integer/float parameters not right-aligned in 10-char fields
**Solution**: Use proper format strings: `f"{value:>10.2f}"`

### 10. Output file not created
**Symptom**: No .t52 file after successful run
**Causes**:
- K4OUT = 0 (no disk output)
- JDISK > N5 (output starts after run ends)
- Wrong working directory
**Solution**: Set K4OUT=52, JDISK=1 or 3, verify CWD

---

## Python Interface Guidelines

### Minimal Test Configuration

For fast testing (< 1 second):

```python
fast_test_params = {
    'N1': 10,        # 10 layers
    'N2': 96,        # 4 times per hour
    'N3': 3,         # 3 days iteration
    'N4': 1,         # Single latitude
    'N5': 3,         # 3 seasons
    'NRSET': 1,      # Reset after 1 day
    'N24': 24,       # 24 hours output
    'K4OUT': 52,     # bin5 type 52
    'JDISK': 1,      # Start output immediately
    'CONVF': 3.0,    # Very stable
    'LPORB': True,   # Use orbital calculations
}
```

### Standard Production Configuration

For science-quality results (5-10 minutes):

```python
standard_params = {
    'N1': 20,        # 20 layers (adequate depth)
    'N2': 384,       # 16 times per hour (standard)
    'N3': 15,        # 15 days iteration
    'N4': 19,        # 19 latitudes (10° spacing)
    'N5': 120,       # Full Mars year
    'NRSET': 3,      # 3-season spin-up
    'N24': 48,       # High time resolution
    'K4OUT': 52,     # Full output
    'JDISK': 81,     # Save last Martian year
    'CONVF': 3.0,    # Stable
}
```

### Execution Pattern

```python
import subprocess
from pathlib import Path
import tempfile
import shutil

def execute_krc(params, timeout=300):
    """Execute KRC with proper setup."""
    # 1. Create working directory
    workdir = Path(tempfile.mkdtemp(prefix="krc_"))

    # 2. Copy required data files
    krc_home = Path(os.environ.get("KRC_HOME", "."))
    for fname in ["standish.tab", "spinaxis.tab", "PORBCM.mat"]:
        src = krc_home / "run" / fname
        shutil.copy(src, workdir / fname)

    # 3. Generate input file
    generate_inp_file(workdir / "krc.inp", params)

    # 4. Execute KRC with stdin
    result = subprocess.run(
        ["./krc"],
        input="krc\nkrc\n",
        capture_output=True,
        text=True,
        cwd=str(workdir),
        timeout=timeout
    )

    # 5. Check for output
    if not (workdir / "krc.t52").exists():
        raise RuntimeError(f"KRC failed: {result.stderr}")

    # 6. Parse output
    output = parse_bin52(workdir / "krc.t52")

    # 7. Cleanup
    shutil.rmtree(workdir)

    return output
```

### Validation Checks

```python
def validate_params(params):
    """Validate parameters before execution."""
    # Check dimensional limits
    assert params.get('N1', 0) <= 30, "N1 exceeds MAXN1=30"
    assert params.get('N2', 0) <= 1536, "N2 exceeds MAXN2=1536"
    assert params.get('N4', 0) <= 37, "N4 exceeds MAXN4=37"
    assert params.get('N5', 0) <= 161, "N5 exceeds MAXN5=161"

    # Check stability
    convf = params.get('CONVF', 3.0)
    assert convf == 0 or convf >= 0.8, f"CONVF={convf} < 0.8 will cause instability"

    # Check N2 is even
    assert params.get('N2', 384) % 2 == 0, "N2 must be even"

    # Check N24 divides N2
    n2 = params.get('N2', 384)
    n24 = params.get('N24', 24)
    assert n2 % n24 == 0, f"N24={n24} must divide N2={n2}"
```

---

## Appendix: Binary Format Specifications

### Bin5 Format Overview

The bin5 format is a binary file format used by Davinci and KRC for storing multidimensional arrays. Bin52 is a specific variant used by KRC for output data.

### Bin5 Header Format

Variable-length ASCII text ending with a marker, padded to 512-byte blocks:

```
<ndim> <dim1> <dim2> ... <dimN> <word_type> <nel> [>> <text>] C_END<arch>
```

**Components**:
- `ndim`: Number of dimensions (integer)
- `dim1, dim2, ...`: Size of each dimension (integers)
- `word_type`: Data type code (see table below)
- `nel`: Total number of elements (product of all dimensions)
- `text`: Optional description text (after `>>`)
- `arch`: 5-character architecture identifier (`x86  ` or `sun  `)
- `C_END`: End marker (arch is immediately before this)

### Word Types

| Code | Type | Size (bytes) | NumPy dtype |
|------|------|--------------|-------------|
| 1 | BYTE | 1 | uint8 |
| 2 | INTEGER | 2 | int16 |
| 3 | LONG | 4 | int32 |
| 4 | FLOAT | 4 | float32 |
| 5 | DOUBLE | 8 | float64 |
| 12 | UINTEGER | 2 | uint16 |
| 13 | ULONG | 4 | uint32 |
| 14 | LONGLONG | 8 | int64 |
| 15 | ULONGLONG | 8 | uint64 |

### Bin52 Structure (KRC Output)

1. **512-byte header** with version string (e.g., "KRCv3.5.6")
2. **4 metadata values** (float64 or float32):
   - NWKRC: Number of words in KRCCOM
   - IDX: Dimension index with extra values
   - NDX: Number of extra seasons
   - NSOUT: Number of output seasons

3. **KRCCOM block** - KRC parameters:

   Version 3 format:
   ```
   - fd[96]: float64 array (64 input + 32 calculated parameters)
   - lats[37]: float64 array (latitudes)
   - elevs[37]: float64 array (elevations)
   - id[40]: uint32 array (integer parameters)
   - ld[20]: uint32 array (boolean flags)
   - title: 80-char string
   - runtime: 24-char string
   ```

4. **Data arrays** organized as:
   ```
   data[hour][field][lat, season, case]
   ```

   Fields (for Type 52):
   1. Surface temperature (tsurf)
   2. Bolometer temperature (tbol)
   3. Atmospheric temperature (tatm)
   4. Downwelling visible flux (down_vis)
   5. Downwelling IR flux (down_ir)
   6. Packed layer temperatures (TIN)
   7. Packed frost/heat/albedo (FROST4)

### Byte Order Handling

The reader automatically detects and handles byte swapping:

1. Read architecture field from header (`x86` or `sun`)
2. Check system byte order
3. Swap if:
   - Big-endian system reading x86 data, OR
   - Little-endian system reading sun data

### Python Usage

```python
# Basic bin5 reading
from pykrc.bin5_reader import load_bin5
data = load_bin5('file.bin5')

# With metadata
from pykrc.bin5_reader import load_bin5_with_metadata
data, header = load_bin5_with_metadata('file.bin5')
print(f"Dimensions: {header.dims}")
print(f"Architecture: {header.arch}")

# Bin52 parsing
from pykrc.bin52_complete import parse_bin52
result = parse_bin52('outdata.bin.52')
surf_temp = result['surf']  # [hours, lats, seasons]
```

### KRCCOM Parameter Indices

Key indices in KRCCOM arrays (0-indexed):

**fd (float array)**:
- [2]: INERTIA - Thermal inertia
- [3]: COND2 - Layer 2 conductivity
- [4]: DENS2 - Layer 2 density
- [5]: PERIOD - Orbital period
- [6]: SPEC_HEAT - Specific heat
- [7]: DENSITY - Density
- [32]: RLAY - Layer ratio
- [33]: FLAY - First layer size

**id (integer array)**:
- [0]: N1 - Number of layers
- [1]: N2 - Calculations per day
- [2]: N3 - Number of latitudes
- [5]: N24 - Output timesteps per day
- [7]: IC2 - Layer 2 start index

**ld (boolean array)**:
- [14]: LOCAL - Local time flag

---

## References

- **KRC Fortran Source**: krc8.f, tseas.f, tlats.f, tday.f
- **KRC Documentation**: 2012je004164ds01-11.txt (architecture, parameters, formats)
- **Kieffer et al., 2013**: "A reanalysis of Mars atmospheric temperature..." JGR Planets
- **Davinci Reference**: krc.dvrc (Davinci implementation)
- **Binary Format**: ff_bin5.c, parser.h (C implementation)

---

**End of Document**
