# KRC Architecture and Runtime Requirements Review

**Date**: 2025-10-07
**Purpose**: Comprehensive review of KRC thermal model architecture, input requirements, and execution requirements for Python interface development

## 1. Executive Summary

KRC (Kieffer Rapid Calculation) is a Fortran-based planetary surface thermal model that uses explicit forward finite differences to solve the heat equation. Key characteristics:

- **Interactive execution**: Expects two filenames via stdin (no extensions)
- **Long runtime**: Runs multiple seasons to thermal equilibrium (minutes to hours)
- **Complex input**: ~96 real parameters, ~40 integer parameters, ~20 logical flags
- **Multiple output modes**: 6 binary output formats (bin5, direct access Fortran)
- **Nested architecture**: KRC → TSEAS → TLATS → TDAY hierarchy
- **Two execution modes**: Full seasonal model or "one-point" single calculation

## 2. Program Architecture

### 2.1 Call Hierarchy

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
│       │       │       └── Convergence prediction (TAU=X²/2)
│       │       └── Checks convergence (DTMAX, DDT, GGT)
│       └── Global integrations (if N4 > 8)
└── Disk output (TDISK)
```

### 2.2 Common Blocks (Shared Memory)

- **KRCCOM**: Main parameters (~96 reals, ~40 ints, ~20 logicals, TITLE, DAYTIM)
- **LATCOM**: Latitude-specific arrays (temperatures, frost, heat flow)
- **DAYCOM**: Diurnal arrays (hourly temperatures, insolation)
- **HATCOM**: "Catch-all" for new items (added 2004)
- **UNITS**: I/O unit numbers
- **FILCOM**: Filenames
- **PORBCM**: Orbital parameters (from PORB program)

### 2.3 Key Dimensions (from krccom.inc)

```fortran
PARAMETER (MAXN1 = 30)       ! max layers
PARAMETER (MAXN2 = 384*4)    ! max times per day
PARAMETER (MAXN3 = 100)      ! max days to iterate
PARAMETER (MAXN4 = 37)       ! max latitudes
PARAMETER (MAXN5 = 161)      ! max seasons
PARAMETER (KOMMON = 10000000) ! storage for binary file
```

## 3. Input System

### 3.1 Execution Method

KRC expects **two filenames via stdin** (without extensions):
```bash
echo -e "krc\nkrc" | ./krc
```

This provides:
- Line 1: Input filename → adds `.inp` extension
- Line 2: Output filename → adds `.prt` extension

### 3.2 Input File Structure

1. **KOLD/KEEP line**: Controls continuation from disk files
   ```
   0 0 / KOLD: season to start, KEEP: continue saving
   ```

2. **Title line**: 80-character description

3. **Real parameters** (8F10.0 format): 8 values per line, 96 total
   - Physical properties (ALBEDO, EMISS, INERTIA, COND2, DENS2, PERIOD, SpecHeat, DENSITY)
   - Atmospheric properties (CABR, AMW, PTOTAL, FANON, TATM, TDEEP)
   - Opacity and geometry (TAUD, DUSTA, SLOPE, SLOAZI)
   - Frost properties (TFROST, CFROST, AFROST, FEMIS, AF1, AF2, FROEXT)
   - Thermal solution (RLAY, FLAY, CONVF, DEPTH, DRSET, DDT, GGT, DTMAX)
   - Orbital parameters (DJUL, DELJUL, SDEC, DAU, SUBS, SOLCON, GRAV, AtmCp)
   - Temperature-dependent K(T) and Cp(T) coefficients (ConUp0-3, ConLo0-3, SphUp0-3, SphLo0-3)

4. **Integer parameters** (8I10 format): 8 values per line, 40 total
   - N1: Number of layers
   - N2: Number of times per day
   - N3: Maximum days to iterate
   - N4: Number of latitudes
   - N5: Number of seasons
   - N24: Number of hours per day stored
   - IB: Bottom boundary condition (0=insulating, 1=constant, 2=reset)
   - IC: First layer of changed properties
   - NRSET: Days before reset
   - NMHA: Hour angles for printout
   - NRUN: Run number
   - JDISK: Season to begin disk output
   - K4OUT: Disk output format (-=TSF/TPF, 0=LATCOM, 1-49=DAYCOM, 51-56=bin5 types)

5. **Logical parameters** (10L7 format): 10 values per line, 20 total
   - LP1-LP6: Print control flags
   - LPGLOB: Print global parameters
   - LVFA: Variable frost albedo
   - LVFT: Variable frost temperature
   - LKOFT: Temperature-dependent K and Cp
   - LPORB: Call PORB1 for orbital calculations
   - LOCAL: Use layer-specific scaling
   - LD16, LD18, LD19: Debug output flags

6. **Latitudes** (10F7.2): N4 values in degrees (-90 to +90)

7. **Elevations** (10F7.2): N4 values in km

8. **Orbital parameters** (if LPORB=T): Direct paste from PORB program output
   - RUNTIME line with IPLAN and TC
   - 39 orbital parameter values (9 per line)

9. **Change cards**: Modify parameters between runs
   ```
   TYPE INDEX VALUE 'Comment'
   ```
   - Type 0: End of changes
   - Type 1: Real parameter
   - Type 2: Integer parameter
   - Type 3: Logical parameter
   - Type 4: New latitude cards
   - Type 5: New elevation cards
   - Type 7: New title
   - Type 8: New disk filename
   - Type 10: Switch to one-point mode

### 3.3 One-Point Mode

Special mode for single-point calculations without full seasonal runs.

**Main input file**: Sets up base parameters, then uses change card:
```
10 1 0.1 'oneA.one' / Switch to one-point mode
```

**One-point input file** (e.g., `oneA.one`):
```
Title Line, Text transferred to output
11    Ls   Lat Hour  Elev  Alb Inerti Opac Slop Azim
11 100.0  22.3 13.5  -3.1 0.20  200.0 0.30  0.0  90. Viking Lander 1
11  90.0  47.7 13.5  -3.0 0.20  100.0 0.20  0.0   0. Viking Lander 2
...
```

Each `11` line produces one output line with surface and planck temperatures.

## 4. Required Data Files

KRC requires these auxiliary files in the working directory:

1. **standish.tab**: Planetary ephemeris data (from PORB system)
2. **spinaxis.tab**: Spin axis orientation data
3. **PORBCM.mat**: Binary orbital common block data

These are located in `/krc_support/` directory.

## 5. Output System

### 5.1 Print File (.prt)

Human-readable output controlled by LP1-LP6 and LPGLOB flags:
- Program description (LP1)
- All parameters and changes (LP2)
- Hourly conditions on last day (LP3)
- Daily convergence summary (LP4)
- Latitude summary (LP5)
- TMIN/TMAX vs latitude and layer (LP6)
- Global parameters each season (LPGLOB)

### 5.2 Binary Output (K4OUT parameter)

Six main binary output formats:

**Direct-access Fortran files** (K4OUT ≤ 50):
- K4OUT = -1: KRCCOM, then TSF & TPF
- K4OUT = 0: KRCCOM + LATCOM each season (standard for databases)
- K4OUT = 1-49: KRCCOM + DAYCOM for latitude [K4OUT] each season

**Bin5 files** (K4OUT > 50):
All bin5 files include a "prefix" with KRCCOM and allow multiple cases.

- **Type 51**: (N24, 2, N4, seasons, cases)
  - 2 items: TSF (surface temp), TPF (planck temp)
  - Prefix has season vectors: DJU5, SUBS, PZREF, TAUD, SUMF

- **Type 52**: (N24, 7, N4, seasons, cases) — **Most comprehensive**
  - 7 items: TSF, TPF, TAF, DOWNVIS, DOWNIR, packed layer temps (TIN), packed frost/heat (FROST4/AFRO4/HEATMM/TAX)
  - Same prefix as Type 51
  - **Recommended for full analysis**

- **Type 54**: (seasons, 5, N4, cases)
  - 5 items: TSF@1am, TSF@13h, HEATMM, FROST4, TTB4
  - Prefix has DJU5

- **Type 55**: (seasons, 9, cases) — One latitude only
  - 9 items: Tsur@1am/3am/1pm, spare, Tplan@1am/1pm, heat flow, frost budget, T_bottom
  - Prefix has DJU5
  - Cannot use continuation runs

- **Type 56**: (vectors, N4, seasons, cases)
  - All hours of TSF and TPF, all layers T4 at midnight, plus FROST4, HEATMM, TTA4
  - Same prefix as Type 51

### 5.3 Bin5 File Format

See [2012je004164ds11.txt](KRC_docs/2012je004164ds11.txt) for full specification.

**Header** (512-byte aligned):
```
ndim dim1 dim2 ... dimN wordtype nel lbl_len <<...>> header_text arch C_END
```

- `ndim`: Number of dimensions
- `dim1...dimN`: Dimension sizes (most-rapidly-varying first = Fortran order)
- `wordtype`: 1=byte, 2=int16, 3=int32, 4=float32, 5=float64, etc.
- `nel`: Total elements (product of dimensions)
- `lbl_len`: Header length in bytes (multiple of 512)
- `arch`: 5-character architecture string (e.g., "x86  ") **immediately before C_END**
- `C_END`: End marker

**Data**: Binary array in Fortran column-major order

## 6. Convergence and Runtime

### 6.1 Convergence Approach

KRC runs multiple seasons to reach thermal equilibrium:

1. **Initial season**: Layers reset after NRSET days to speed convergence
2. **Subsequent seasons**: Temperature prediction using TAU = X²/(2κ) time constant
3. **Convergence tests**:
   - DDT: RMS 2nd differences < threshold (default 0.002)
   - GGT: Surface boundary iteration < threshold (default 0.1)
   - DTMAX: RMS layer T changes per day < threshold (default 0.1)

### 6.2 Typical Runtimes

- **Full seasonal run** (N5=120 seasons, N4=19 lats, N1=20 layers, N2=384 times): ~5-30 minutes
- **One-point mode**: Seconds per calculation
- **Spin-up period**: First 3 seasons (NRSET=3) most expensive

### 6.3 Timeout Considerations

For Python interface:
- Set generous timeouts (≥5 minutes for full runs)
- Monitor convergence in print file
- Consider one-point mode for testing

## 7. Key Parameters for Testing

### 7.1 Minimal Test Configuration

```
N1  = 13     (13 layers, fast convergence)
N2  = 384    (standard diurnal resolution)
N3  = 10     (10 days convergence per season)
N4  = 1      (single latitude)
N5  = 3      (3 seasons for spin-up)
N24 = 24     (hourly output)
K4OUT = 0    (no disk output for testing)
```

### 7.2 Standard Mars Parameters

```fortran
ALBEDO  = 0.25        ! Surface albedo
INERTIA = 200.0       ! Thermal inertia [J/m²/K/s^(1/2)]
PERIOD  = 1.0275      ! Mars solar day length [Earth days]
PTOTAL  = 546.0       ! Mean pressure [Pa]
TAUD    = 0.3         ! Visible opacity
GRAV    = 3.727       ! Mars gravity [m/s²]
SOLCON  = 1368.0      ! Solar constant [W/m²]
```

### 7.3 One-Point Test

For rapid testing without full seasonal runs:
```
Ls   = 100.0    ! Northern summer
Lat  = 0.0      ! Equator
Hour = 13.5     ! Early afternoon
Elev = 0.0      ! Mean elevation
Alb  = 0.25     ! Standard albedo
Inerti = 200.0  ! Standard inertia
Opac = 0.3      ! Standard opacity
Slop = 0.0      ! Flat surface
Azim = 0.0      ! North-facing
```

## 8. Python Interface Requirements

### 8.1 Input File Generation

Python interface should:
1. Accept parameters as Python dict or kwargs
2. Validate parameter ranges and types
3. Generate properly formatted .inp file
4. Handle change cards for parameter variations
5. Support both full seasonal and one-point modes

### 8.2 Execution Management

1. **Working directory setup**:
   - Copy required data files (standish.tab, spinaxis.tab, PORBCM.mat)
   - Generate unique temp directory or use specified directory
   - Write input files with proper names

2. **Process execution**:
   ```python
   echo_input = f"{input_base}\n{output_base}\n"
   proc = subprocess.Popen(
       ["./krc"],
       stdin=subprocess.PIPE,
       stdout=subprocess.PIPE,
       stderr=subprocess.PIPE,
       cwd=working_dir
   )
   stdout, stderr = proc.communicate(
       input=echo_input.encode(),
       timeout=timeout_seconds
   )
   ```

3. **Output reading**:
   - Parse .prt file for convergence info
   - Read bin5 files with existing `bin5_reader.py`
   - Extract KRCCOM from bin5 prefix
   - Return as structured dict or dataclass

### 8.3 Output Parsing

For bin5 Type 52 (recommended):
```python
data = read_bin5("output.t52")
# Shape: (nhours, 7, nlat, nseasons, ncases)

# Extract arrays
tsf = data[:, 0, :, :, :]      # Surface temperature
tpf = data[:, 1, :, :, :]      # Planck temperature
taf = data[:, 2, :, :, :]      # Atmospheric temperature
downvis = data[:, 3, :, :, :]  # Downwelling visible
downir = data[:, 4, :, :, :]   # Downwelling IR
tin_packed = data[:, 5, :, :, :]   # Packed layer temps
frost_packed = data[:, 6, :, :, :] # Packed frost/heat

# Extract KRCCOM from header
# (First 4 words are dimensions, next NWKRC words are KRCCOM)
```

### 8.4 Validation

1. **Pre-execution validation**:
   - Check N1 ≤ MAXN1 (30)
   - Check N2 ≤ MAXN2 (1536)
   - Check N4 ≤ MAXN4 (37)
   - Check N5 ≤ MAXN5 (161)
   - Validate CONVF ≥ 0.8 for stability

2. **Post-execution validation**:
   - Check return code
   - Parse .prt for error messages
   - Validate output file existence
   - Check for "UNSTABLE" warnings

## 9. Common Pitfalls

1. **Missing stdin input**: KRC hangs waiting for filenames
2. **Missing data files**: standish.tab, spinaxis.tab, PORBCM.mat required
3. **Timeout too short**: Full runs can take 5-30 minutes
4. **Unstable parameters**: CONVF < 0.8 causes numerical instability
5. **Wrong architecture**: Bin5 files may need byte-swapping on different platforms
6. **Fortran vs NumPy ordering**: Bin5 data is Fortran column-major
7. **Change card syntax**: Must follow exact format or KRC crashes
8. **LPORB without orbital data**: If LPORB=T, must provide PORB cards

## 10. Example Workflows

### 10.1 Full Seasonal Run

```python
from pykrc import krc

params = {
    'albedo': 0.25,
    'inertia': 200.0,
    'n1': 20,        # layers
    'n2': 384,       # times per day
    'n3': 15,        # days to iterate
    'n4': 19,        # latitudes
    'n5': 120,       # seasons
    'k4out': 52,     # bin5 type 52 output
    'lats': [-87.5, -80, -70, -60, -50, -40, -30, -20, -10, 0,
             10, 20, 30, 40, 50, 60, 70, 80, 87.5],
    'elevs': [3.51, 2.01, 1.39, 1.22, 0.38, 0.48, 1.17, 1.67, 1.26, 0.17,
              -0.94, -1.28, -1.99, -2.51, -3.52, -4.08, -4.51, -4.38, -2.57],
}

result = krc.run(params, timeout=1800)  # 30 min timeout
print(f"Surface temps: {result['tsf'].shape}")
```

### 10.2 One-Point Mode

```python
from pykrc import krc

# One-point calculations
cases = [
    {'ls': 100, 'lat': 22.3, 'hour': 13.5, 'inertia': 100},
    {'ls': 100, 'lat': 22.3, 'hour': 13.5, 'inertia': 200},
    {'ls': 100, 'lat': 22.3, 'hour': 13.5, 'inertia': 400},
]

results = krc.run_onepoint(cases, timeout=60)
for r in results:
    print(f"Ls={r['ls']}, I={r['inertia']}: Tsurf={r['tsur']:.2f}K")
```

## 11. References

- **2012je004164ds01.txt**: Architecture documentation
- **2012je004164ds07.txt**: Complete parameter descriptions (Helplist)
- **2012je004164ds08.txt**: Sample input files
- **2012je004164ds10.txt**: One-point mode examples
- **2012je004164ds11.txt**: Bin5 format specification
- **Kieffer_2013.pdf**: Scientific methods paper
- **krc8.f**: Main program source
- **tseas.f, tlats.f, tday.f**: Core calculation routines

## 12. Next Steps for Python Interface

1. **Create parameter dataclass** with validation
2. **Implement input file generator** with proper formatting
3. **Create working directory manager** with data file copying
4. **Implement process executor** with proper stdin handling
5. **Create output parser** for .prt files
6. **Extend bin5_reader** to extract KRCCOM from prefix
7. **Create result dataclass** with named arrays
8. **Write comprehensive tests** for all modes
9. **Add examples** for common use cases
10. **Document parameter interactions** and sensitivities
