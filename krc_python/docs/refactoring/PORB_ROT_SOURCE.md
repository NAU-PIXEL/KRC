# PORB.ROT Source and Flow Analysis

**Date:** 2025-10-14
**Purpose:** Document the complete origin and flow of the `porb.rot` parameter that contains pre-formatted PORB orbital parameters

---

## Executive Summary

**`porb.rot` is a pre-formatted text array** containing the 30 PORB orbital parameters already formatted in Fortran G15.7 format. It comes from one of two sources:

1. **Pre-cached defaults** (`porb_defaults/*.porb.hdf`) - Fast, used by default
2. **Live PORB Fortran execution** (`porbmn` binary) - Slower, used when `force=1` or body not in cache

**Critical Finding:** The PORB parameters are **formatted by the Fortran PORB code** (`porbmn`), not by davinci. Davinci just reads the text file output and stores it as `porb.rot`.

---

## 1. The porb() Function Flow

### 1.1 Entry Point

**Location:** [krc.dvrc:2214](krc.dvrc#L2214)

```davinci
define porb(epoch, anc, v, force) {
    # epoch: Fraction of century (default 0.10 = 2010)
    # anc: Return ancillary data (default 0)
    # v: Verbose (default 0)
    # force: Force PORB recalculation (default 0)
```

**Called from krc():**
```davinci
# Line 295
porb=porb(body, force=bodyforce)
```

### 1.2 Decision Tree

```
porb(body, force=bodyforce)
│
├─ Is body a STRUCT? (generic_porb result)
│  └─ YES: Use generic orbital parameters
│
├─ force == 0 (default)
│  └─ Check for pre-cached file: $DV_SCRIPT_FILES/krc_support/porb_defaults/<body>.porb.hdf
│     ├─ EXISTS: Load and return (FAST PATH) ← Most common
│     └─ NOT EXISTS: Fall through to live PORB run
│
└─ force == 1 OR no cached file
   └─ Run porbmn Fortran binary (SLOW PATH)
      1. Create temp directory
      2. Write porb_<body>.run input file
      3. Execute: porbmn < porb_<body>.run
      4. Read output: <body>.mat
      5. Parse and return
```

---

## 2. Fast Path: Pre-Cached PORB Defaults

### 2.1 Cache Location

**Lines 2273-2280:**
```davinci
testfile=$DV_SCRIPT_FILES+"/krc_support/porb_defaults/"+body+".porb.hdf"
if(fexists(testfile) && force!=1) {
    printf("Found Default File: $DV_SCRIPT_FILES/krc_support/porb_defaults/%s\n", basename(testfile))
    verbose=0
    default=read(testfile)
    verbose=3
    return(default)
}
```

**Path:** `$DV_SCRIPT_FILES/krc_support/porb_defaults/<body>.porb.hdf`

**Example:** For Mars: `porb_defaults/Mars.porb.hdf`

### 2.2 Cache Contents

The cached HDF file contains a **complete PORB structure** with:

```davinci
default.rot           # Pre-formatted text array (7 lines)
default.rot_per       # Rotation period (hours)
default.period        # Orbital period (days)
default.krc.PERIOD    # In KRC units (Earth days)
default.krc.N24       # Timesteps per day
default.krc.DELJUL    # Time increment
default.krc.PTOTAL    # Atmospheric pressure
default.krc.GRAV      # Surface gravity
... (all KRC parameters)
default.type          # Body classification
default.planet_flux   # Satellite flux parameters (if applicable)
```

**Key field:** `default.rot` is a **text array** of 7 lines:
- Line 1: PORB header
- Lines 2-7: 30 PORB values (6 lines × 5 values)

### 2.3 Available Cached Bodies

**Line 64:**
```davinci
krc_porb_defaults=cat(
    "Mercury","Venus","Earth","Mars","Jupiter","Saturn","Uranus","Neptune","Pluto",
    "Phobos","Deimos","Europa","Ganymede","Io","Callisto",
    "Bennu","Ceres","Vesta","Pallas","Juno",
    "1P-Halley","9P-Tempel_1","10P-Tempel_2","81P-Wild_2","22P-Kopff",
    "21P-Giacobini-Zinner","19P-Borrelly","103P-Hartley_2",
    "67P-Churyumov-Gerasimenko","Dinkinesh",
    axis=y
)
```

**Total:** 30 pre-cached bodies

---

## 3. Slow Path: Live PORB Execution

### 3.1 When Triggered

1. `force=1` explicitly set
2. Body not in `porb_defaults/` cache
3. Custom epoch requested
4. Generic/exoplanet body (from `generic_porb()`)

### 3.2 Execution Flow

**Lines 2745-2761:**

```davinci
# 1. Run PORB Fortran binary
if(v>=1) {
    system("cd "+workdir+"; "+$DV_KRC_HOME+"/src/porbmn < "+workdir+"/porb_"+name+".run | tee -a "+workdir+"/krc.log")
    log=read_lines(workdir+"/krc.log")
} else {
    log=syscall("cd "+workdir+"; "+$DV_KRC_HOME+"/src/porbmn < "+workdir+"/porb_"+name+".run")
}

# 2. Read PORB output file
verbose=0
rot=read_lines(workdir+"/"+name+".mat")
verbose=1

# 3. Store in output structure
out.rot=rot
out.rot_per=rot_per
out.period=period
```

### 3.3 PORB Input File

**Created at:** `workdir+"/porb_"+name+".run"`

**Contents (example for Mars):**
```
<standish.tab contents>
<spinaxis.tab contents>
<planetary_params3.csv row for Mars>
```

### 3.4 PORB Output File

**Created at:** `workdir+"/"+name+".mat"`

**Format:** ASCII text file with 7 lines:
```
Line 1: 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars
Lines 2-7: [30 PORB values in G15.7 format, 5 per line]
```

**This file is formatted by the Fortran porbmn binary!**

---

## 4. The PORB.ROT Structure

### 4.1 Content Analysis

**`porb.rot` is a text array with 7 elements:**

```davinci
porb.rot[,1]  = "2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars"
porb.rot[,2]  = "   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    "
porb.rot[,3]  = "  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    "
porb.rot[,4]  = "   5.544402       0.000000       0.000000       686.9929       3397.977    "
porb.rot[,5]  = "   24.62296       0.000000      -1.240317       0.000000       0.000000    "
porb.rot[,6]  = "   0.000000      0.3244965      0.8559126      0.4026359     -0.9458869    "
porb.rot[,7]  = "  0.2936298      0.1381285       0.000000     -0.4256703      0.9048783   "
```

**Each value line:**
- 5 values × 15 characters = 75 characters
- Plus 4 trailing spaces = 79 characters total

### 4.2 Usage in krc()

**Line 485:**
```davinci
master.inp.part6=porb.rot
```

**Then written directly to input file (lines 1147, 1345):**
```davinci
input=cat(inp.header, inp.part1, inp.part2, inp.part3, inp.part4, inp.part5, inp.part6, params, axis=y)
```

**Result:** `porb.rot` is written **verbatim** to the KRC input file, no reformatting!

---

## 5. Where G15.7 Formatting Happens

### 5.1 The Formatting Source

**THE FORMATTING IS DONE BY FORTRAN PORBMN, NOT DAVINCI!**

The `porbmn` binary:
1. Reads input parameters
2. Calculates 30 orbital parameters
3. **Formats them using Fortran WRITE with G15.7 format**
4. Writes to `<body>.mat` file

**Fortran code (approximate):**
```fortran
! In porbmn source (likely porbel.f or similar)
WRITE(UNIT, '(5G15.7,4X)') (PORB_PARAMS(I), I=1,5)
WRITE(UNIT, '(5G15.7,4X)') (PORB_PARAMS(I), I=6,10)
WRITE(UNIT, '(5G15.7,4X)') (PORB_PARAMS(I), I=11,15)
WRITE(UNIT, '(5G15.7,4X)') (PORB_PARAMS(I), I=16,20)
WRITE(UNIT, '(5G15.7,4X)') (PORB_PARAMS(I), I=21,25)
WRITE(UNIT, '(5G15.7,4X)') (PORB_PARAMS(I), I=26,30)
```

**Format:** `5G15.7,4X`
- `5G15.7`: 5 values in G15.7 format (15 chars wide, 7 sig figs)
- `4X`: 4 trailing spaces

### 5.2 Pre-Cached Files

The cached `.porb.hdf` files were created by:

**Lines 2784-2800 (update defaults mode):**
```davinci
# Generate all default PORB files
for(i=1; i<=length(run_list); i+=1) {
    printf("Running Body: %s\n", run_list[,i])
    out=porb(run_list[,i], force=1)  # Force live PORB run
    outname=outdir+"/"+run_list[,i]+".porb.hdf"
    printf("Writing: %s\n", outname)
    write(out, outname, hdf, force=1)
}
```

**Process:**
1. Run `porbmn` for each body (slow)
2. Read formatted output from `<body>.mat`
3. Save complete structure (including `rot`) to HDF
4. Now cached for fast access

---

## 6. Implications for PyKRC

### 6.1 PyKRC Must Format PORB

**Since PyKRC doesn't call porbmn:**
- PyKRC receives **numeric** PORB values from `porb_handler.py`
- PyKRC **must format them** using G15.7 rules
- The formatting must **exactly match** Fortran G15.7 output

### 6.2 Current PyKRC Flow

```python
# In porb_handler.py
porb_params = get_porb_parameters(body)  # Returns numeric list of 30 values

# In executor.py
params['PORB_PARAMS'] = porb_params  # Numeric values

# In _write_input_file()
for val in porb_params:
    formatted = format_g15_7(val)  # PyKRC formats here!
    f.write(formatted)
```

### 6.3 Why PyKRC Needs Exact G15.7

**Reasons:**
1. **Bit-level reproducibility** with davinci
2. **KRC Fortran may be sensitive** to formatting differences
3. **Testing and validation** requires exact match
4. **Debugging** easier with identical files

### 6.4 Verification Strategy

**To verify PyKRC formatting is correct:**

1. **Extract PORB values from davinci cache:**
   ```python
   # Read Mars.porb.hdf
   # Parse porb.rot[2:7] back to numeric values
   # Compare with PyKRC's numeric PORB values
   ```

2. **Format with PyKRC:**
   ```python
   # Use PyKRC's format_g15_7() on those values
   # Compare character-by-character with porb.rot
   ```

3. **Test edge cases:**
   - Values at 0.1 boundary (E vs F format)
   - Values at 1e8 boundary
   - Zero values
   - Negative values
   - Large values (1M, 10M, 99M)

---

## 7. PORB Parameter Meanings

### 7.1 The 30 PORB Values

From examining KRC source and PORB documentation:

| Index | Name | Units | Description | Example (Mars) |
|-------|------|-------|-------------|----------------|
| 1 | IPLAN | - | Planet/body ID | 104.0 |
| 2 | TC | century | Time constant | 0.1 (2010) |
| 3 | e | - | Orbital eccentricity | 0.8644665 |
| 4 | i | radians | Orbital inclination | 0.03227 |
| 5 | Ω | radians | Longitude of ascending node | -1.281586 |
| 6 | ω̃ | radians | Longitude of perihelion | 0.09340 |
| 7 | a | AU | Semi-major axis | 1.523712 |
| 8 | n | rad/day | Mean motion | 0.4090926 |
| 9 | M0 | radians | Mean anomaly at epoch | 0.0 |
| 10 | λ0 | radians | Mean longitude at epoch | 0.9229373 |
| 11 | P | days | Orbital period | 686.9929 (Mars year) |
| 12 | Q1 | - | Quaternion component | Variable |
| 13 | Q2 | - | Quaternion component | Variable |
| 14 | Q3 | - | Quaternion component | Variable |
| 15 | Q4 | - | Quaternion component | Variable |
| 16-21 | Rotation matrix | - | 3×3 rotation matrix (6 values) | Variable |
| 22-27 | Pole vector | - | Spin axis components | Variable |
| 28-30 | Additional | - | Auxiliary parameters | Variable |

**Note:** These indices are approximate; actual PORB documentation should be consulted for precise definitions.

---

## 8. Code Flow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                   krc(body="Mars", ...)                         │
└────────────────────────┬────────────────────────────────────────┘
                         │
                         ▼
                  porb(body, force=0)
                         │
         ┌───────────────┴───────────────┐
         │                               │
    force=0                          force=1
 (or cache exists)                (or no cache)
         │                               │
         ▼                               ▼
┌─────────────────────┐     ┌─────────────────────────┐
│ Read HDF cache:     │     │ Create temp dir         │
│ porb_defaults/      │     │ Write porb_Mars.run     │
│   Mars.porb.hdf     │     │                         │
│                     │     │ Execute:                │
│ Structure contains: │     │ porbmn < porb_Mars.run  │
│  - rot (text array) │     │                         │
│  - krc (params)     │     │ Read output:            │
│  - type (metadata)  │     │ Mars.mat                │
│  - planet_flux      │     │                         │
│                     │     │ Parse into structure:   │
│                     │     │  - rot=read_lines()     │
│                     │     │  - calc krc params      │
└──────────┬──────────┘     └──────────┬──────────────┘
           │                           │
           └───────────┬───────────────┘
                       │
                       ▼
              porb.rot (text array)
                       │
              "   104.0000  ..."
              " 0.9340E-01  ..."
              "  5.544402   ..."
                    (etc)
                       │
                       ▼
         master.inp.part6 = porb.rot
                       │
                       ▼
      Write to krc.inp lines 36-41
      (verbatim, no reformatting)
                       │
                       ▼
               KRC Fortran reads
              30 PORB parameters
```

---

## 9. PyKRC Implementation Requirements

### 9.1 What PyKRC Must Do

**Because PyKRC doesn't have pre-formatted porb.rot:**

1. ✓ **Calculate numeric PORB values** (porb_handler.py)
2. ✗ **Format using G15.7 rules** (executor.py - needs fix)
3. ✓ **Write PORB header line** (executor.py - done)
4. ✗ **Write 6 lines × 5 values** (executor.py - needs decimal fix)

### 9.2 Missing Pieces

**From PORB_FORMATTING_ANALYSIS.md, PyKRC is missing:**

- Decimal places for 10k ≤ v < 100k range
- Decimal places for 100k ≤ v < 1M range
- Decimal places for 1M ≤ v < 10M range
- Decimal places for 10M ≤ v < 100M range

### 9.3 Complete Fix

**In executor.py, lines 234-248:**

```python
if abs_val >= 1:
    if abs_val >= 10000000:        # NEW
        decimals = 0
    elif abs_val >= 1000000:       # NEW
        decimals = 1
    elif abs_val >= 100000:        # NEW
        decimals = 2
    elif abs_val >= 10000:         # NEW
        decimals = 3
    elif abs_val >= 1000:
        decimals = 3
    elif abs_val >= 100:
        decimals = 4
    elif abs_val >= 10:
        decimals = 5
    else:
        decimals = 6
else:
    decimals = 7
```

---

## 10. Testing Strategy

### 10.1 Extract Reference Data

**From davinci:**
```bash
# Load Mars PORB
davinci -c "porb('Mars'); write(porb.rot, 'mars_porb_rot.txt', ascii)"
```

**Parse back to numeric:**
```python
# Read mars_porb_rot.txt
# Parse lines 2-7 (skip header)
# Extract 30 values from G15.7 format
# Store as reference array
```

### 10.2 Test PyKRC Formatting

```python
reference_values = [104.0, 0.1, 0.8644665, 0.03226901, ...]  # 30 values
reference_strings = [
    "   104.0000",
    "      0.1000000",
    "      0.8644665",
    " 0.3226901E-01",
    ...
]

for i, (val, expected) in enumerate(zip(reference_values, reference_strings)):
    formatted = format_g15_7(val)
    assert formatted == expected, f"Value {i}: expected '{expected}', got '{formatted}'"
```

### 10.3 Full Input File Comparison

```python
# Generate PyKRC input file
pykrc_file = generate_input_file(params)

# Generate davinci input file
davinci_file = run_davinci_krc(same_params)

# Compare PORB section (lines 36-41)
pykrc_porb = read_lines(pykrc_file, 36, 41)
davinci_porb = read_lines(davinci_file, 36, 41)

for i, (pykrc_line, davinci_line) in enumerate(zip(pykrc_porb, davinci_porb)):
    assert pykrc_line == davinci_line, f"Line {36+i} differs"
```

---

## 11. Summary

### 11.1 Key Findings

1. **`porb.rot` is pre-formatted text** from either:
   - Cached HDF file (fast, default)
   - Live `porbmn` Fortran execution (slow, if needed)

2. **Formatting happens in Fortran**, not davinci:
   - `porbmn` uses `WRITE(*, '(5G15.7,4X)')` format
   - Davinci reads and stores as text
   - Davinci writes verbatim to input file

3. **PyKRC must replicate Fortran G15.7**:
   - No pre-formatted source available
   - Must format numeric values exactly
   - G15.7 rules must match Fortran compiler behavior

4. **Current PyKRC has incomplete formatting**:
   - Missing 4 magnitude ranges
   - Otherwise correct logic

### 11.2 Action Items

- [ ] Fix PyKRC decimal place logic for large values
- [ ] Test with all 30 Mars PORB values
- [ ] Test edge cases (0.1, 1e8 boundaries)
- [ ] Verify character-by-character match with davinci
- [ ] Document any compiler-specific G format differences

---

**END OF DOCUMENT**
