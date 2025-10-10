# Davinci KRC Input File Generation Architecture

**Analysis Date:** 2025-10-09
**Source File:** `/Users/chaberle/Documents/GitHab/KRC/krc_davinci/krc.dvrc`
**Purpose:** Comprehensive technical map for PyKRC comparison and validation

---

## Table of Contents

1. [Parameter Transformation Pipeline](#1-parameter-transformation-pipeline)
2. [Changecard Generation Logic](#2-changecard-generation-logic)
3. [Input File Formatting Rules](#3-input-file-formatting-rules)
4. [PORB Parameter Handling](#4-porb-parameter-handling)
5. [Parameter Ordering](#5-parameter-ordering)
6. [Special Cases](#6-special-cases)
7. [Summary Tables](#7-summary-tables)
8. [Edge Cases and Gotchas](#8-edge-cases-and-gotchas)

---

## 1. Parameter Transformation Pipeline

### 1.1 Load Master.inp Defaults

**Function:** `krc_process_input()` (lines 1560-1716)

**Process:**
1. Read `master.inp` file from `$DV_KRC_HOME/run/master.inp`
2. Parse file into 6 sections:
   - `inp.header`: Lines 1-2 (KOLD/KEEP and description)
   - `inp.part1`: REAL*8 floating point parameters (64 parameters, 8 per line)
   - `inp.part2`: INTEGER*4 parameters (20 parameters)
   - `inp.part3`: LOGICAL*4 boolean flags (20 parameters)
   - `inp.part4`: Latitude array (up to N4 values)
   - `inp.part5`: Elevation array (up to N4 values)
   - `inp.part6`: PORB orbital parameters (30 values in 6 lines)

3. Extract key-value pairs using `krc_get_key_value()` (lines 1720-1774)

**Key Code:**
```davinci
master=read_lines($DV_KRC_HOME+"/run/master.inp")
# Extract sections based on grep for keyword markers
start1=maxpos(master==grep(master,"ALBEDO")[,1])[2]
end1=maxpos(master==grep(master,"N1")[,1])[2]-1
inp.part1=master[,start1:end1]
```

### 1.2 PORB Parameter Loading

**Function:** `porb()` (lines 294-295, 416-480)

**Priority Order:**
1. Pre-computed PORB defaults from `porb_master.hdf` (for speed)
2. Force recalculation if `bodyforce=1`
3. PORB provides:
   - `rotation_period` (PERIOD)
   - `orbital_parameters` (30-element matrix)
   - Body-specific defaults (N24, DELJUL, PTOTAL, GRAV, etc.)

**Key Code:**
```davinci
porb=porb(body,force=bodyforce)
# Replace master.inp orbital elements with PORB values
master.inp.part6=porb.rot
```

### 1.3 Parameter Resolution Order

**Function:** Main `krc()` function (lines 85-1557)

**Precedence (highest to lowest):**
1. **User-provided parameters** (explicitly passed to `krc()`)
2. **PORB-derived values** (lines 469-480)
3. **Material-derived values** (lines 524-711)
4. **Master.inp defaults** (parsed structure)

**Example Resolution Flow:**
```davinci
# Line 341: Set from PORB if not user-provided
if(HasValue(PERIOD)==0)     PERIOD=porb.krc.PERIOD

# Lines 550-563: Set from ancillary maps if not provided
if(HasValue(INERTIA)==0) {
    global(krc_INERTIA)
    INERTIA=krc_INERTIA[xy[1],xy[2]]  # From TES map
}

# Line 772: Fall back to master.inp
if(HasValue(FLAY)==0)   FLAY=atof(key.part1.value[,maxpos(key.part1.key=="FLAY")[2]])
```

### 1.4 Material Property Calculation

**Function:** Material property section (lines 524-711)

**Process:**
1. Set defaults based on body type (Mars, Europa, or generic)
2. Calculate thermal properties from:
   - `INERTIA` + `Mat1`/`Mat2` → derive COND, DENSITY, SPEC_HEAT
   - OR direct specification of COND/DENSITY/SPEC_HEAT
3. Calculate T-dependent polynomial coefficients (`ConUp0-3`, `SphUp0-3`)
4. Apply porosity correction to density

**Key Formula:**
```davinci
# Lines 602-619: Upper layer properties
X_user = (T_user-220.)*0.01
SPEC_HEAT = SphUp0 + SphUp1*X_user + SphUp2*X_user^2 + SphUp3*X_user^3
Por1 = 0.60 * (2200. - INERTIA)/2200.
DENSITY = (1 - Por1) * Mat_Prop.Dens.Dens0 + ...
COND = INERTIA^2/(DENSITY*SPEC_HEAT)

# Lines 621-636: Fit k(T) polynomial
if(k_style == "Moon"){
    k_Table = COND*(1+2.7*((T_Tab-T_user)/350.)^3)  # Hayne et al.
}
if(k_style == "Mars"){
    k_Table = COND*sqrt(T_Tab/T_user)  # Morgan et al.
}
FIT = fit(y=k_Table, x=X, "cube", plot=0)
ConUp0 = FIT[1]  # Through ConUp3 = FIT[4]
```

---

## 2. Changecard Generation Logic

### 2.1 What Are Changecards?

**Historical Context:** Punch card legacy - allows runtime parameter modification without recompiling.

**Format:**
```
<type> <index> <value> '<parameter_name>' /
```

**Types:**
- **Type 1:** REAL*8 floating point (part1 parameters)
- **Type 2:** INTEGER*4 integer (part2 parameters)
- **Type 3:** LOGICAL*4 boolean (part3 parameters)
- **Type 8:** CHARACTER file path (special file operations)

### 2.2 Changecard Generation Algorithm

**Function:** Changecard writing section (lines 1024-1089)

**Process:**

#### Step 1: Boolean Parameters (Type 3)
```davinci
# Lines 1026-1033
for(i=1; i<=length(key.part3.key); i+=1) {
    if(eval(sprintf("HasValue(%s)==1", key.part3.key[,i]))){
        params=cat(params, sprintf("3 %i %i '%s' /", i, atob(eval(key.part3.key[,i])), key.part3.key[,i]), axis=y)
    }
}
```

**Format:** `3 <index> <0_or_1> '<param_name>' /`
- Index: 1-20 (position in part3 array)
- Value: 0 (False) or 1 (True)
- Example: `3 9 1 'LVFT' /` (enable frost, 9th boolean parameter)

#### Step 2: Integer Parameters (Type 2)
```davinci
# Lines 1036-1045
for(i=1; i<=length(key.part2.key); i+=1) {
    if(eval(sprintf("HasValue(%s)==1", key.part2.key[,i])) && key.part2.key[,i]!="N4"){
        params=cat(params, sprintf("2 %i %i '%s' /", i, eval(key.part2.key[,i]), key.part2.key[,i]), axis=y)
    }
}
```

**Format:** `2 <index> <integer_value> '<param_name>' /`
- Index: 1-20 (position in part2 array)
- **Exception:** N4 is skipped (handled specially in file header, line 1022)
- Example: `2 1 29 'N1' /` (29 layers)

#### Step 3: Float Parameters (Type 1)
```davinci
# Lines 1047-1089
for(i=1; i<=length(key.part1.key); i+=1) {
    if(eval(sprintf("HasValue(%s)==1", key.part1.key[,i]))){
        # Special handling for arrays (ALBEDO, TAUD)
        if (max(dim(ALBEDO))>1 && key.part1.key[,i]=="ALBEDO") {
            # Write to file and use Type 8 changecard
        } else {
            # Regular scalar value
            if(eval(key.part1.key[,i])<1 && eval(key.part1.key[,i])>-1) {
                params=cat(params, sprintf("1 %i %.3E '%s' /", i, eval(key.part1.key[,i]), key.part1.key[,i]), axis=y)
            } else {
                params=cat(params, sprintf("1 %i %.4f '%s' /", i, eval(key.part1.key[,i]), key.part1.key[,i]), axis=y)
            }
        }
    }
}
```

**Format:** `1 <index> <float_value> '<param_name>' /`
- Index: 1-64 (position in part1 array)
- Precision:
  - If `-1 < value < 1`: Use scientific notation `%.3E` (3 decimal places)
  - Otherwise: Use fixed point `%.4f` (4 decimal places)
- Examples:
  - `1 3 250.0000 'INERTIA' /`
  - `1 1 2.500E-01 'ALBEDO' /` (for ALBEDO=0.25)

### 2.3 Filtering Logic: When Are Changecards Written?

**Critical Rule:** A changecard is written if and only if:

1. **User explicitly set the parameter** (`HasValue(param)==1`)
2. **OR** parameter was calculated internally (e.g., from PORB or materials)

**NOT written if:**
- Parameter uses master.inp default value
- Parameter is undefined

**Example:**
```davinci
# If user sets INERTIA=250, write changecard:
1 3 250.0000 'INERTIA' /

# If user doesn't set INERTIA, master.inp value (200.0) is used - no changecard

# If PORB sets PERIOD=1.0275, always write changecard:
1 6 1.0275 'PERIOD' /
```

### 2.4 Special Type 8 Changecards (File Operations)

**Format:** `8 <subtype> <flag> '<filepath>' /`

**Subtypes:**
- **8 3 0:** Input far-field file (line 1108)
- **8 5 0:** Output file path (line 1092) - **REQUIRED**
- **8 21 0:** Output far-field file (line 1102)
- **8 22 0:** Time-varying albedo file (line 1062)
- **8 23 0:** Time-varying tau file (line 1073)
- **8 25 0:** Zone file for layer properties (line 894)

**Example:**
```davinci
# Line 1092: Output file specification (ALWAYS written)
params=cat(params, sprintf("8 5 0 '%s/outdata.bin.%s' /", workdir, K4OUT+""), y)

# Output: 8 5 0 '/tmp/krc_12345/outdata.bin.52' /
```

### 2.5 Changecard Order

**Critical:** Changecards are written in strict order:

1. **Type 3** (Booleans) - all 20 parameters checked
2. **Type 2** (Integers) - all 20 parameters checked (except N4)
3. **Type 1** (Floats) - all 64 parameters checked
4. **Type 8** (Files) - written as needed
5. **Terminator:** `0/` (two lines, lines 1132-1133)

---

## 3. Input File Formatting Rules

### 3.1 Header Format (Lines 1-2)

```
0 0 / KOLD: season to start with;  KEEP: continue saving data in same disk file
Version 356 default values.  19 latitudes with mean Mars zonal elevations
```

**Rules:**
- Line 1: Two integers (KOLD, KEEP) followed by comment
- Always write `0 0` regardless of actual values (line 231)
- Actual KEEP value handled via changecards

### 3.2 REAL*8 Parameter Lines (8 per line)

**Function:** `_write_param_line()` (executor.py lines 328-353)

**Format:**
```
    ALBEDO     EMISS   INERTIA     COND2     DENS2    PERIOD SPEC_HEAT   DENSITY
      0.25      1.00     200.0      2.77     928.0    1.0275      647.     1600.
```

**Rules:**
- Header line: 10 characters per field, right-aligned
- Value line: 10 characters per field, right-aligned
- Precision: 2 decimal places (`.2f`)
- Total: 8 parameters per line × 8 lines = 64 REAL*8 parameters

**Field Width Specification:**
```davinci
# Lines 338-339, 348: 10-character fields
header = "".join(f"{name:>10}" for name in param_names)
values.append(f"{float(val):>10.2f}")
```

### 3.3 INTEGER*4 Parameter Lines

**Format:**
```
        N1        N2        N3        N4        N5       N24       IIB       IC2
        28      1536        15        19       120        48         0       999
```

**Rules:**
- Header: 10 characters per field, right-aligned
- Value: 10 characters per field, right-aligned, integer format
- Last line has `end` marker at position 73
- Total: 20 INTEGER parameters across 3 lines (8+8+4)

**Special N4 Handling:**
```davinci
# Lines 1020-1022: N4 is written directly to header, not via changecard
N4part2posy=maxpos(inp.part2==grep(inp.part2,"N4")[,1])[2]
N4part2posx=strstr(inp.part2[,N4part2posy],"N4")+1
inp.part2[N4part2posx-9:N4part2posx,N4part2posy+1]=sprintf("%10s",N4+"")
```

### 3.4 LOGICAL*4 Parameter Lines

**Format:**
```
    LP1    LP2    LP3    LP4    LP5    LP6 LPGLOB   LVFA   LVFT  LKofT
      F      T      F      F      F      F      F      F      F      F
  LPORB   LKEY    LSC  LZONE  LOCAL  Prt76 LPTAVE  Prt78  Prt79  L_ONE
      T      F      F      F      T      F      F      F      F      F
```

**Rules:**
- Header: 7 characters per field, right-aligned
- Value: 7 characters per field, right-aligned, 'T' or 'F'
- Two lines: 10 parameters each
- Total: 20 LOGICAL parameters

**Field Width:**
```python
# executor.py lines 362-376
f.write("".join(f"{name:>7}" for name in lp_names) + "\n")
lp_vals = "".join(f"{'T' if params.get(name, False) else 'F':>7}" for name in lp_names)
```

### 3.5 Latitude and Elevation Arrays

**Format:**
```
Latitudes: in 10F7.2  _____7 _____7 _____7 _____7 _____7 _____7 _____7
 -87.50 -80.00 -70.00 -60.00 -50.00 -40.00 -30.00 -20.00 -10.00   0.00
  10.00  20.00  30.00  40.00  50.00  60.00  70.00  80.00  87.50  -0.00
 _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7
   3.51   2.01   1.39   1.22   0.38   0.48   1.17   1.67   1.26   0.17
  -0.94  -1.28  -1.99  -2.51  -3.52  -4.08  -4.51  -4.38  -2.57  -0.00
```

**Rules:**
- Format: 10 values per line (F7.2 format)
- Each value: 7 characters wide, 2 decimal places
- Space-separated
- Number of values = N4

**Code:**
```davinci
# Lines 713, 716: Direct write to inp structure
inp.part4[,2]=sprintf("%7.2f", lat)
inp.part5[,2]=sprintf("%7.2f", ELEV)
```

---

## 4. PORB Parameter Handling

### 4.1 PORB Structure

**Source:** Pre-computed in `porb_master.hdf` or calculated on-the-fly

**Contents:**
- `porb.rot`: 30-element orbital parameter matrix
- `porb.krc`: Dictionary of KRC-specific parameters
- `porb.type`: Body classification info

### 4.2 PORB Header Line

**Format:**
```
2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars
```

**Rules:**
- Date/time stamp
- IPLAN code (planet identifier)
- TC value (time constant)
- Body name

### 4.3 PORB Parameter Matrix (30 values)

**Format:** 6 lines × 5 values per line, G15.7 format (15 chars wide, 7 decimals)

```
   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586
  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373
   5.544402       0.000000       0.000000       686.9929       3397.977
   24.62296       0.000000      -1.240317       0.000000       0.000000
   0.000000      0.3244965      0.8559126      0.4026359     -0.9458869
  0.2936298      0.1381285       0.000000     -0.4256703      0.9048783
```

**Code:**
```python
# executor.py lines 307-312
for i in range(0, len(porb_params), 5):
    chunk = porb_params[i:i+5]
    line = ''.join(f"{val:15.7f}" for val in chunk)
    f.write(line + '\n')
```

### 4.4 PORB Parameter Injection

**Process:**
```davinci
# Lines 469-480: Inject PORB values if not user-provided
porbkeys=get_struct_key(porb.krc)
for(i=1; i<=length(porb.krc); i+=1) {
    if(eval("HasValue("+porbkeys[,i]+")")==0) {
        if(HasValue(DELLS) && porbkeys[,i]=="DELJUL") {
            # Skip DELJUL if user set DELLS
        } else {
            # Set from PORB
            eval(sprintf("%s=porb.krc.%s", porbkeys[,i], porbkeys[,i]))
        }
    }
}

# Line 484: Replace orbital matrix
master.inp.part6=porb.rot
```

### 4.5 PORB-Provided KRC Parameters

**Common PORB parameters:**
- PERIOD (rotation period in days)
- DELJUL (time step in Julian days)
- N24 (timesteps per day)
- PTOTAL (atmospheric pressure)
- GRAV (surface gravity)
- SOLCON (solar constant)
- DAU (distance from sun)
- Various atmospheric parameters

---

## 5. Parameter Ordering

### 5.1 REAL*8 Parameters (Type 1, 64 total)

**Order (index 1-64):**

```python
# Lines 424-433 in executor.py
float_params = [
    # Line 1 (indices 1-8):
    "ALBEDO", "EMISS", "INERTIA", "COND2", "DENS2", "PERIOD", "SPEC_HEAT", "DENSITY",

    # Line 2 (indices 9-16):
    "CABR", "AMW", "SatPrA", "PTOTAL", "FANON", "TATM", "TDEEP", "SpHeat2",

    # Line 3 (indices 17-24):
    "TAUD", "DUSTA", "TAURAT", "TWILI", "ARC2_G0", "ARC3_Safe", "SLOPE", "SLOAZI",

    # Line 4 (indices 25-32):
    "TFROST", "CFROST", "AFROST", "FEMIS", "AF1", "AF2", "FROEXT", "SatPrB",

    # Line 5 (indices 33-40):
    "RLAY", "FLAY", "CONVF", "DEPTH", "DRSET", "PhotoFunc", "GGT", "DTMAX",

    # Line 6 (indices 41-48):
    "DJUL", "DELJUL", "SOLARDEC", "DAU", "LsubS", "SOLCON", "GRAV", "Atm_Cp",

    # Line 7 (indices 49-56):
    "ConUp0", "ConUp1", "ConUp2", "ConUp3", "ConLo0", "ConLo1", "ConLo2", "ConLo3",

    # Line 8 (indices 57-64):
    "SphUp0", "SphUp1", "SphUp2", "SphUp3", "SphLo0", "SphLo1", "SphLo2", "SphLo3"
]
```

### 5.2 INTEGER*4 Parameters (Type 2, 20 total)

**Order (index 1-20):**

```python
# Lines 436-440 in executor.py
int_params = [
    # Line 1 (indices 1-8):
    "N1", "N2", "N3", "N4", "N5", "N24", "IIB", "IC2",

    # Line 2 (indices 9-16):
    "NRSET", "NMHA", "NRUN", "JDISK", "IDOWN", "FlxP14", "TUN_Flx15", "KPREF",

    # Line 3 (indices 17-20):
    "K4OUT", "JBARE", "Notif", "IDISK2"
]
```

### 5.3 LOGICAL*4 Parameters (Type 3, 20 total)

**Order (index 1-20):**

```python
# Lines 443-446 in executor.py
bool_params = [
    # Line 1 (indices 1-10):
    "LP1", "LP2", "LP3", "LP4", "LP5", "LP6", "LPGLOB", "LVFA", "LVFT", "LKofT",

    # Line 2 (indices 11-20):
    "LPORB", "LKEY", "LSC", "LZONE", "LOCAL", "Prt76", "LPTAVE", "Prt78", "Prt79", "L_ONE"
]
```

---

## 6. Special Cases

### 6.1 Time-Varying ALBEDO

**Trigger:** `dim(ALBEDO) > 1` (2D array)

**Process:**
```davinci
# Lines 1055-1063
if (max(dim(ALBEDO))>1 && key.part1.key[,i]=="ALBEDO") {
    global(krc_var_header)
    var=krc_var_header
    for(j=1; j<=dim(ALBEDO)[2]; j+=1) {
        var=cat(var, sprintf("%.2f\t%.2f", ALBEDO[1,j], ALBEDO[2,j]), axis=y)
    }
    write(var, workdir+"/albfile.tab", type=ascii, force=1)
    params=cat(params, sprintf("8 22 0 '%s/albfile.tab' /", workdir), y)
}
```

**File Format (albfile.tab):**
```
# Header lines from krc_var_header
0.00    0.25
90.00   0.30
180.00  0.25
270.00  0.20
```
- Column 1: Ls (solar longitude)
- Column 2: ALBEDO value

**Changecard:** `8 22 0 '/path/to/albfile.tab' /`

### 6.2 Time-Varying TAUD

**Same structure as ALBEDO, different changecard type:**

```davinci
# Lines 1066-1074
params=cat(params, sprintf("8 23 0 '%s/taufile.tab' /", workdir), y)
```

**Changecard:** `8 23 0 '/path/to/taufile.tab' /`

### 6.3 Multi-Latitude Runs (N4 > 1)

**Rules:**
- N4 sets number of latitude/elevation pairs
- Latitudes written in part4 (10 per line, F7.2)
- Elevations written in part5 (10 per line, F7.2)
- Must have exactly N4 values in each array

**Example (N4=3):**
```
Latitudes: in 10F7.2  _____7 _____7 _____7
 -30.00   0.00  30.00
 _____7 _____7 _____7 Elevations: in 10F7.2
  -2.00   0.00   1.50
```

### 6.4 Eclipse Modeling

**Function:** Eclipse parameter generation (lines 451-463)

**Format:**
```davinci
Eclipse_line = sprintf("14 %.0i %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f 3 0. 0. / Eclipse",
    Eclipse_Style, Sun_Dis, Eclipser_Rad, CM, Eclipsed_Rad, Per_Mut, Bias, Date, Ecl_Cent_Hr)
```

**Changecard:** `14 <style> <sun_dis> <eclipser_rad> <cm> <eclipsed_rad> <per_mut> <bias> <date> <ecl_cent_hr> 3 0. 0. / Eclipse`

**Parameters:**
- Eclipse_Style: 1.0 (daily) or 2.0 (rare)
- Sun_Dis: Distance from sun (AU)
- Eclipser_Rad: Eclipsing body radius (km)
- CM: Center-of-mass orbit radius (km)
- Eclipsed_Rad: Eclipsed body radius (km)
- Per_Mut: Mutual period (days)
- Bias: Eclipse bias
- Date: J2000 date for rare eclipse
- Ecl_Cent_Hr: Eclipse central hour

### 6.5 Planetary Flux (Satellite Thermal Forcing)

**Function:** `krc_planetary_flux_porb()` (lines 3135-3166)

**Format:**
```davinci
line = sprintf("15 %.2f %.2f %.2f %.2f %.2f %.2f %.2f / Forcing from Planet on Satellite",
    IR_Flux, IR_Half_Amp, IR_Phase_Lag, Vis_Flux, Vis_Half_Amp, Vis_Phase_Lag, Lon_Hr)
```

**Changecard:** `15 <IR_flux> <IR_half_amp> <IR_phase_lag> <Vis_flux> <Vis_half_amp> <Vis_phase_lag> <Lon_Hr> /`

**Calculations:**
```davinci
# Lines 3151-3162
Radiance = 5.56E-8 * float(Data_Plan.BT_Avg)^4
IR_Flux = (Radiance * Data_Plan.Radius^2/Data_Sat.Orb_Radius^2)
Vis_Flux_Peak = 2*(pi*1361*Data_Plan.Radius^2/(Data_Plan.Dis_AU^2))/(2*pi*Data_Sat.Orb_Radius^2)*Data_Plan.Geom_alb
Vis_Flux = 0.5 * Vis_Flux_Peak
```

### 6.6 Two-Layer Configuration

**Triggered by:** `thick != 0`

**Parameters:**
- `thick > 0`: Two-layer regolith, thick = top layer thickness (m)
- `thick < 0`: Exponential trend, thick = H parameter
- `thick = 0`: Uniform material

**IC2 Calculation:**
```davinci
# Lines 868, 1304: IC2 from krc_evalN1
IC2 = N1struct.req.IC2
# Default: IC2=999 (no layer change)
# Two-layer: IC2 = layer index where material changes
```

**Changecard:**
```
2 8 5 'IC2' /   # Layer 5 is transition
```

### 6.7 Zone Files (LZONE="T")

**Function:** Layer property specification (lines 891-896)

**Process:**
```davinci
if(LZONE=="T") {
    write(N1struct.Zone.inp, workdir+"/zonefile.tab", type=ascii, force=1)
    params=cat(params, sprintf("8 25 0 'zonefile.tab' /"), y)
}
```

**File Format:** ASCII table with layer properties
**Changecard:** `8 25 0 'zonefile.tab' /`

---

## 7. Summary Tables

### 7.1 Parameter Type Summary

| Type | Count | Format | Width | Precision | Example |
|------|-------|--------|-------|-----------|---------|
| REAL*8 | 64 | Float | 10 chars | 2 decimals | `200.00` |
| INTEGER*4 | 20 | Integer | 10 chars | N/A | `29` |
| LOGICAL*4 | 20 | T/F | 7 chars | N/A | `T` |
| Latitude | N4 | Float | 7 chars | 2 decimals | `-87.50` |
| Elevation | N4 | Float | 7 chars | 2 decimals | `3.51` |
| PORB | 30 | G-format | 15 chars | 7 decimals | `0.8644665` |

### 7.2 Changecard Type Summary

| Type | Purpose | Format | Index Range | Example |
|------|---------|--------|-------------|---------|
| 1 | Float override | `1 <i> <val> '<name>' /` | 1-64 | `1 3 250.0000 'INERTIA' /` |
| 2 | Integer override | `2 <i> <val> '<name>' /` | 1-20 | `2 1 29 'N1' /` |
| 3 | Boolean override | `3 <i> <0_1> '<name>' /` | 1-20 | `3 9 1 'LVFT' /` |
| 8 | File operation | `8 <sub> 0 '<path>' /` | Subtype-specific | `8 5 0 './out.t52' /` |
| 14 | Eclipse | `14 <params...> /` | N/A | `14 1 1.5 ... /` |
| 15 | Planetary flux | `15 <fluxes...> /` | N/A | `15 120.5 ... /` |

### 7.3 Critical Default Values

**From master.inp (MASTER_INP_DEFAULTS):**

| Parameter | Default | Units | Notes |
|-----------|---------|-------|-------|
| ALBEDO | 0.25 | - | Often overridden from TES map |
| INERTIA | 200.0 | J m⁻² K⁻¹ s⁻½ | Often overridden from TES map |
| EMISS | 1.00 | - | Perfect emitter |
| PERIOD | 1.0275 | days | Mars default, overridden by PORB |
| PTOTAL | 546.0 | Pa | Mars default, overridden by PORB |
| TDEEP | 180.0 | K | Bottom boundary temperature |
| RLAY | 1.15 | - | **Davinci uses 1.15, PyKRC default 1.08** |
| FLAY | 0.10 | skin depths | **Davinci uses 0.10, PyKRC default 2.0** |
| N1 | 28 | - | Usually auto-calculated |
| N2 | 1536 | - | Usually auto-calculated |
| N3 | 15 | - | Convergence iterations |
| N24 | 48 | - | Timesteps per day (overridden by PORB) |
| JDISK | 81 | seasons | Start output season |
| N5 | 120 | seasons | Total seasons |
| IC2 | 999 | - | No layer change |
| FANON | 0.055 | - | **Master.inp, NOT 0.3** |
| TAURAT | 0.25 | - | **Master.inp, NOT 2.0** |

**Critical Discrepancy:** FANON and TAURAT defaults differ between master.inp and typical PyKRC values!

---

## 8. Edge Cases and Gotchas

### 8.1 FANON/TAURAT Mismatch

**Issue:** Master.inp defaults differ from common usage

**master.inp values:**
- FANON = 0.055 (line 35 in executor.py)
- TAURAT = 0.25 (line 43 in executor.py)

**PyKRC typical values:**
- FANON = 0.3 (atmospheric anisotropy)
- TAURAT = 2.0 (optical depth ratio)

**Resolution:** Always write changecards for these if user/PORB provides different values.

### 8.2 N4 Special Handling

**Issue:** N4 is NOT handled via changecards (lines 1020-1022, 472)

**Reason:** KRC checks N4 before processing changecards to validate lat/elev arrays

**Solution:** N4 is written directly into input file header (line 1022):
```davinci
inp.part2[N4part2posx-9:N4part2posx,N4part2posy+1]=sprintf("%10s",N4+"")
```

### 8.3 DELLS vs DELJUL

**Issue:** User can specify either DELLS (Ls delta) or DELJUL (JD delta)

**Resolution (lines 741-745, 800-822):**
```davinci
if(HasValue(DELLS) && HasValue(DELJUL)) {
    printf("Please do not set both DELLS and DELJUL\n")
    return(null)
}

if(HasValue(DELLS)) {
    DELJUL = 1.9083 * DELLS  # Convert Ls to JD
}
```

**Constraint:** `DELJUL >= PERIOD` (line 835)

### 8.4 RLAY/FLAY Defaults

**Critical Discrepancy:**

| Source | RLAY | FLAY | Notes |
|--------|------|------|-------|
| master.inp | 1.15 | 0.10 | Original KRC defaults |
| Davinci default | 1.15 | 0.10 | Uses master.inp |
| PyKRC krc() | 1.08 | 2.0 | **Different!** |
| krc_evalN1 | Calculated | Calculated | Often overrides |

**Impact:** Layer spacing calculations differ unless user explicitly sets these.

### 8.5 Changecard Precision

**Float changecards use different precision based on magnitude:**
```davinci
# Lines 1078-1084
if(eval(key.part1.key[,i])<1 && eval(key.part1.key[,i])>-1) {
    params=cat(params, sprintf("1 %i %.3E '%s' /", i, ...), axis=y)  # Scientific
} else {
    params=cat(params, sprintf("1 %i %.4f '%s' /", i, ...), axis=y)  # Fixed
}
```

**Examples:**
- `ALBEDO=0.25` → `1 1 2.500E-01 'ALBEDO' /`
- `INERTIA=250` → `1 3 250.0000 'INERTIA' /`
- `ConUp0=0.03864` → `1 49 3.864E-02 'ConUp0' /`

### 8.6 KEEP Parameter

**Issue:** Header always writes `0 0` (line 231)

**Actual KEEP:** Set via changecard if user requests

**Rationale:** Prevents accidental file continuation

### 8.7 PORB vs User Parameters

**Precedence:**
1. User explicit
2. PORB (if not user-set)
3. Master.inp

**Exception:** DELJUL skipped if user set DELLS (lines 473-474):
```davinci
if(HasValue(DELLS) && porbkeys[,i]=="DELJUL") {
    # Skip - calculated from DELLS instead
}
```

### 8.8 File Path Handling

**Absolute vs Relative:**
- Davinci uses absolute paths with temp directories
- Output file can be relative: `./krc.t52`
- Support files must be in workdir or absolute

**Workdir structure:**
```
/tmp/krc_12345/
├── krc              # Symlink to executable
├── krc.inp          # Input file
├── standish.tab     # PORB data
├── spinaxis.tab     # PORB data
├── PORBCM.mat       # PORB data
├── zonefile.tab     # If LZONE=T
├── albfile.tab      # If time-varying ALBEDO
└── taufile.tab      # If time-varying TAUD
```

### 8.9 Temperature Prediction Control

**Logic (lines 841-856):**
```davinci
if(DELJUL <= 3*PERIOD) {
    if(HasValue(TPREDICT)==0) TPREDICT="F"
}
if(TPREDICT=="F") {
    GGT=99.
    N3=1
    NRSET=999
}
```

**Impact:** Short timesteps disable prediction for stability.

### 8.10 IC2 Constraints

**Rule:** IC2 must be >= 3 or 999 (lines 493-500)

**Reason:** Layer 1 is atmosphere, layer 2 is first surface layer

**Enforcement:**
```davinci
if(IC2!=999 && IC2<3) {
    printf("IC2 must be either 999 or >=3\n")
    return(null)
}
```

---

## Appendix A: Key Function Reference

| Function | Lines | Purpose |
|----------|-------|---------|
| `krc()` | 85-1557 | Main interface function |
| `krc_process_input()` | 1560-1716 | Parse master.inp |
| `krc_get_key_value()` | 1720-1774 | Extract key-value pairs |
| `krc_evalN1()` | 1776+ | Calculate layer count |
| `krc_evalN2()` | (external) | Calculate timestep count |
| `porb()` | 294-295 | Load orbital parameters |
| `generic_porb()` | 2992-3082 | Custom orbital elements |
| `krc_cond_gas()` | 3084-3131 | Frost parameter lookup |
| `krc_planetary_flux_porb()` | 3135-3166 | Calculate planetary flux |
| `krc_eclipse()` | 451-463 | Generate eclipse parameters |
| `atob()` | 2971-2990 | Convert T/F to 0/1 |

---

## Appendix B: PyKRC Comparison Checklist

**Critical items to verify:**

- [ ] RLAY/FLAY defaults match davinci (1.15/0.10 vs 1.08/2.0)
- [ ] FANON/TAURAT use master.inp values (0.055/0.25) not common values
- [ ] N4 written to header, not changecard
- [ ] Changecard precision matches (%.3E vs %.4f)
- [ ] Float changecards only for values != master.inp
- [ ] Type 8 changecard order (files before terminator)
- [ ] PORB parameter injection happens before changecard generation
- [ ] DELLS/DELJUL mutual exclusion enforced
- [ ] IC2 constraint (>=3 or 999) validated
- [ ] Time-varying ALBEDO/TAUD write to file + Type 8 changecard
- [ ] Eclipse/PFlux changecards use correct format
- [ ] PORB matrix uses G15.7 format
- [ ] Latitude/elevation use F7.2 format (not F10.2)
- [ ] Zone file written when LZONE="T"
- [ ] Temperature prediction auto-disabled for short DELJUL

---

## Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2025-10-09 | 1.0 | Initial comprehensive analysis |

---

**End of Document**
