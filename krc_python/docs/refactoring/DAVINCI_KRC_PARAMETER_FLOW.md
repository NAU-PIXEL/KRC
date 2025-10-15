# Davinci KRC Function: Complete Parameter Flow Analysis

**Date:** 2025-10-14
**Source:** [krc.dvrc](/Users/chaberle/Documents/GitHab/KRC/krc_davinci/krc.dvrc)
**Purpose:** Document every parameter's journey from user input to Fortran input file
**For:** PyKRC validation and verification

---

## Executive Summary

This document traces **every parameter** accepted by the davinci `krc()` function through the complete transformation pipeline:

1. **User Input** → Parameter acceptance and validation
2. **Resolution** → Precedence order (User → PORB → Material → Master.inp)
3. **Transformation** → Calculations and derived values
4. **Output** → Fortran input file generation (master.inp format + changecards)

**Total Parameters Traced:** 140+ parameters across 6 categories

---

## Table of Contents

1. [Parameter Flow Overview](#1-parameter-flow-overview)
2. [Function Signature](#2-function-signature)
3. [Parameter Resolution Order](#3-parameter-resolution-order)
4. [Material Property Calculations](#4-material-property-calculations)
5. [PORB Parameter Injection](#5-porb-parameter-injection)
6. [Time and Season Calculations](#6-time-and-season-calculations)
7. [Changecard Generation](#7-changecard-generation)
8. [Complete Parameter Trace Table](#8-complete-parameter-trace-table)
9. [Critical Differences vs PyKRC](#9-critical-differences-vs-pykrc)

---

## 1. Parameter Flow Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     USER INPUTS                             │
│  krc(lat=30, INERTIA=250, body="Mars", ...)                │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              INITIALIZATION (lines 289-484)                 │
│  • Load master.inp defaults                                 │
│  • Load PORB for body                                       │
│  • Set davinci defaults                                     │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│         PARAMETER RESOLUTION (lines 307-484)                │
│  Priority: User > PORB > Material > Master.inp              │
│  • Check HasValue() for each parameter                      │
│  • Apply PORB defaults if not user-set                      │
│  • Calculate derived parameters                             │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│      MATERIAL CALCULATIONS (lines 524-711)                  │
│  • Determine Mat1/Mat2 properties                           │
│  • Calculate INERTIA from COND/DENSITY/SPEC_HEAT           │
│  • Fit k(T) and Cp(T) polynomials                           │
│  • Apply porosity corrections                               │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│      ANCILLARY DATA LOADING (lines 548-721)                 │
│  • Load INERTIA from TES map (Mars)                         │
│  • Load ALBEDO from TES map (Mars)                          │
│  • Load ELEV from MOLA map (Mars)                           │
│  • Handle time-varying ALBEDO/TAUD                          │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│       TIME/SEASON SETUP (lines 800-921)                     │
│  • Calculate DELJUL from DELLS                              │
│  • Calculate N5/JDISK from DELLS                            │
│  • Validate TPREDICT settings                               │
│  • Handle JD/GD date conversions                            │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│      NUMERICAL MODEL SETUP (lines 862-934)                  │
│  • Calculate N1 via krc_evalN1()                            │
│  • Calculate N2 via krc_evalN2()                            │
│  • Calculate IC2 from thick                                 │
│  • Set LZONE if needed                                      │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│       INPUT FILE GENERATION (lines 1000-1133)               │
│  • Write master.inp header (lines 1-2)                      │
│  • Write REAL*8 params (64 values, 8 lines)                 │
│  • Write INTEGER*4 params (20 values, 3 lines)              │
│  • Write LOGICAL*4 params (20 values, 2 lines)              │
│  • Write lat/elev arrays (N4 values each)                   │
│  • Write PORB matrix (30 values, 6 lines)                   │
│  • Generate changecards (Type 1/2/3/8/14/15)                │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              FORTRAN EXECUTION                              │
│  ./krc < krc.inp                                            │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Function Signature

**Location:** [krc.dvrc:85](krc.dvrc#L85)

```davinci
define krc(
    # Special control parameters
    T_Tol, k_style, WRITE, LMST, GD, JD, KEEP, COND, ffout, ffin,
    lbound, TUN8, thick, bodyforce, body, TPREDICT, T, adv_usage,
    stability, anc, v, lat, lon, ELEV, type, hour, ls, DELLS,

    # Material properties
    Mat1, Mat2, Por1, Por2, INERTIA2, MAXN1, MAXN2,

    # Eclipse/Planetary Flux
    PFlux, BT_Avg, BT_Max, BT_Min, Dis_AU, Geom_alb, Mut_Period,
    Orb_Radius, Radius, Lon_Hr, IR, Vis, Eclipse, Eclipser,
    Ecl_Cent_Hr, Eclipse_Style, Sun_Dis, Eclipser_Rad, CM,
    Eclipsed_Rad, Per_Mut, Bias, Date,

    # One-point mode
    T_user, TI_Guess, TI_Guess_PCT,

    # Master.inp parameters marker
    KRC_MASTER_INP_VARS_AFTER_HERE,

    # REAL*8 parameters (64 total)
    ALBEDO, EMISS, INERTIA, COND2, DENS2, PERIOD, SPEC_HEAT, DENSITY,
    CABR, AMW, SatPrA, PTOTAL, FANON, TATM, TDEEP, SpHeat2,
    TAUD, DUSTA, TAURAT, TWILI, ARC2_G0, ARC3_Safe, SLOPE, SLOAZI,
    TFROST, CFROST, AFROST, FEMIS, AF1, AF2, FROEXT, SatPrB,
    RLAY, FLAY, CONVF, DEPTH, DRSET, PhotoFunc, GGT, DTMAX,
    DJUL, DELJUL, SOLARDEC, DAU, LsubS, SOLCON, GRAV, Atm_Cp,
    ConUp0, ConUp1, ConUp2, ConUp3, ConLo0, ConLo1, ConLo2, ConLo3,
    SphUp0, SphUp1, SphUp2, SphUp3, SphLo0, SphLo1, SphLo2, SphLo3,

    # INTEGER*4 parameters (20 total)
    N1, N2, N3, N4, N5, N24, IIB, IC2,
    NRSET, NMHA, NRUN, JDISK, IDOWN, FlxP14, TUN_Flx15, KPREF,
    K4OUT, JBARE, Notif, IDISK2, end,

    # LOGICAL*4 parameters (20 total)
    LP1, LP2, LP3, LP4, LP5, LP6, LPGLOB, LVFA, LVFT, LKofT,
    LPORB, LKEY, LSC, LZONE, LOCAL, Prt76, LPTAVE, Prt78, Prt79, L_ONE
)
```

**Total Parameters:** 140+

---

## 3. Parameter Resolution Order

### 3.1 Initialization Phase (lines 289-396)

**Order of operations:**

1. **Body Selection** (lines 291-296)
   ```davinci
   if(HasValue(body)==0)       body="Mars"
   if(HasValue(bodyforce)==0)  bodyforce=0
   porb=porb(body,force=bodyforce)
   ```

2. **Master.inp Loading** (lines 391-397)
   ```davinci
   global(krc_master)
   master=krc_master  # Pre-loaded at script initialization
   ```

3. **Default Value Assignment** (lines 307-380)
   - All `if(HasValue(X)==0) X=default` statements

### 3.2 Resolution Precedence

**For each parameter, the precedence order is:**

```
1. USER EXPLICIT      ← if(HasValue(param)==1)
2. PORB-DERIVED       ← from porb(body)
3. MATERIAL-DERIVED   ← from Mat_Prop(Mat1)
4. MASTER.INP         ← from krc_master structure
5. HARDCODED DEFAULT  ← in initialization section
```

### 3.3 Critical Resolution Examples

#### Example 1: INERTIA
```davinci
# Line 549-551: User explicit
if(HasValue(INERTIA)==0) {
    # Line 550: Mars ancillary data
    global(krc_INERTIA)
    INERTIA=krc_INERTIA[xy[1],xy[2]]  # From TES map at lat,lon
}
# Lines 586-619: Recalculation from material
if(HasValue(COND) && HasValue(DENSITY) && HasValue(SPEC_HEAT)) {
    INERTIA = sqrt(COND * DENSITY * SPEC_HEAT)
}
```

**Resolution:** User → TES Map → Material Calculation

#### Example 2: PERIOD
```davinci
# Line 341: Default from PORB
if(HasValue(PERIOD)==0)     PERIOD=porb.krc.PERIOD
```

**Resolution:** User → PORB → Master.inp (via PORB lookup)

#### Example 3: DELJUL
```davinci
# Lines 347-355: Complex logic
if(HasValue(DELJUL)==0 && HasValue(DELLS)==0) {
    DELJUL=porb.krc.DELJUL              # From PORB
} else if(HasValue(DELJUL)==0 && HasValue(DELLS)) {
    DELJUL=porb.krc.PERIOD/360.*DELLS   # Calculated from DELLS
} else if(HasValue(DELJUL) && HasValue(DELLS)==0) {
    DELJUL=DELJUL                       # User value kept
} else {
    # Error: both set
}
```

**Resolution:** User DELLS → User DELJUL → PORB DELJUL

---

## 4. Material Property Calculations

**Location:** Lines 524-711

### 4.1 Body-Specific Defaults

#### Mars (lines 533-548)
```davinci
if(body=="Mars") {
    if(HasValue(Mat1)==0)  Mat1="basalt"
    if(HasValue(Mat2)==0)  Mat2=Mat1
    if(HasValue(k_style)==0) k_style="Mars"
    if(HasValue(T_user)==0)  T_user=220.
    # INERTIA, ALBEDO, ELEV from global maps
}
```

#### Europa (lines 548-568)
```davinci
if(body=="Europa") {
    if(HasValue(Mat1)==0)    Mat1="H2O"
    if(HasValue(Mat2)==0)    Mat2=Mat1
    if(HasValue(k_style)==0) k_style="Moon"
    if(HasValue(T_user)==0)  T_user=100.
    if(HasValue(INERTIA)==0) INERTIA=100.
    if(HasValue(ALBEDO)==0)  ALBEDO=0.67
}
```

#### Generic Bodies (lines 568-600)
```davinci
# All other bodies
if(HasValue(Mat1)==0)    Mat1="basalt"
if(HasValue(Mat2)==0)    Mat2=Mat1
if(HasValue(k_style)==0) k_style="Moon"
if(HasValue(T_user)==0)  T_user=220.
if(HasValue(INERTIA)==0) INERTIA=100.
if(HasValue(ALBEDO)==0)  ALBEDO=0.67
```

### 4.2 Material Property Derivation

#### Upper Layer (lines 602-636)

**Step 1: Get material properties**
```davinci
Mat_Prop=Mat_Prop(Mat1)  # Look up baseline properties
```

**Step 2: Calculate specific heat at T_user**
```davinci
X_user = (T_user-220.)*0.01
SphUp0 = Mat_Prop.SpH.c0
SphUp1 = Mat_Prop.SpH.c1
SphUp2 = Mat_Prop.SpH.c2
SphUp3 = Mat_Prop.SpH.c3
SPEC_HEAT = SphUp0 + SphUp1*X_user + SphUp2*X_user^2 + SphUp3*X_user^3
```

**Step 3: Calculate porosity**
```davinci
if(HasValue(Por1)==0) {
    Por1 = 0.60 * (2200. - INERTIA)/2200.
}
```

**Step 4: Calculate density**
```davinci
X_Dens = (T_user-220.)*0.01
D0 = Mat_Prop.Dens.Dens0
D1 = Mat_Prop.Dens.Dens1
D2 = Mat_Prop.Dens.Dens2
D3 = Mat_Prop.Dens.Dens3
DENSITY = (1 - Por1) * (D0 + D1*X_Dens + D2*X_Dens^2 + D3*X_Dens^3)
```

**Step 5: Calculate conductivity**
```davinci
COND = INERTIA^2/(DENSITY*SPEC_HEAT)
```

**Step 6: Fit k(T) polynomial** (lines 621-636)
```davinci
# Generate temperature table
T_Tab = 80. + findgen(200)*2.  # 80K to 478K

# Calculate k at each temperature
if(k_style == "Moon") {
    # Hayne et al. 2017
    k_Table = COND*(1+2.7*((T_Tab-T_user)/350.)^3)
} else if(k_style == "Mars") {
    # Morgan et al. 2018
    k_Table = COND*sqrt(T_Tab/T_user)
} else if(k_style == "Bulk") {
    # Use bulk conductivity polynomial
    X = (T_Tab-220.)*0.01
    k_Table = Mat_Prop.ConB.ConB0 + Mat_Prop.ConB.ConB1*X +
              Mat_Prop.ConB.ConB2*X^2 + Mat_Prop.ConB.ConB3*X^3
}

# Fit cubic polynomial
X = (T_Tab-220.)*0.01
FIT = fit(y=k_Table, x=X, "cube", plot=0)
ConUp0 = FIT[1]
ConUp1 = FIT[2]
ConUp2 = FIT[3]
ConUp3 = FIT[4]
```

#### Lower Layer (lines 637-711)

**Same process as upper layer, using:**
- Mat2 instead of Mat1
- Por2 instead of Por1
- INERTIA2 instead of INERTIA
- ConLo0-3 instead of ConUp0-3
- SphLo0-3 instead of SphUp0-3

---

## 5. PORB Parameter Injection

**Location:** Lines 469-484

### 5.1 PORB Structure Contents

```davinci
porb.krc.PERIOD       # Rotation period (days)
porb.krc.N24          # Timesteps per day
porb.krc.DELJUL       # Time increment (days)
porb.krc.PTOTAL       # Atmospheric pressure (Pa)
porb.krc.DUSTA        # Dust particle size (μm)
porb.krc.GRAV         # Surface gravity (m/s²)
porb.krc.TAURAT       # IR/visible opacity ratio
porb.krc.ARC2_G0      # Photometric parameter
porb.krc.SOLCON       # Solar constant (W/m²)
porb.krc.Atm_Cp       # Atmospheric Cp (J/kg/K)
... (others as calculated by PORB)
```

### 5.2 Injection Loop

```davinci
porbkeys=get_struct_key(porb.krc)
for(i=1; i<=length(porb.krc); i+=1) {
    if(eval("HasValue("+porbkeys[,i]+")")==0) {
        # Special case: skip DELJUL if user set DELLS
        if(HasValue(DELLS) && porbkeys[,i]=="DELJUL") {
            if(v==1) printf("Skipping %s from PORB because DELLS is set\n", porbkeys[,i])
        } else {
            if(v==1) printf("Setting %s from PORB\n", porbkeys[,i])
            eval(sprintf("%s=porb.krc.%s", porbkeys[,i], porbkeys[,i]))
        }
    }
}
```

**This loop injects ALL porb.krc parameters into the krc() namespace unless:**
1. User explicitly provided the parameter
2. Parameter is DELJUL and user set DELLS

### 5.3 PORB Matrix Replacement

```davinci
# Line 484: Replace master.inp orbital elements
master.inp.part6=porb.rot  # 30-element PORB matrix
```

---

## 6. Time and Season Calculations

### 6.1 DELLS → DELJUL Conversion (lines 800-822)

```davinci
if(HasValue(DELLS)==1) {
    # Calculate DELJUL from DELLS
    DELJUL = PERIOD/360. * DELLS

    # Auto-calculate N5 and JDISK
    N5 = ceil(360./DELLS * 3)        # 3 full orbital years
    JDISK = ceil(360./DELLS * 2 + 1) # Start output after 2 years
}
```

**Example for Mars with DELLS=1°:**
- DELJUL = 1.0275 / 360 × 1 = 0.002854 days... wait, that's wrong in docs
- DELJUL = 1.0275 × 360 / 360 × 1 = 1.0275 days... still wrong
- **ACTUAL:** DELJUL ≈ 686.97 / 360 × 1 = 1.9083 days (Mars orbital period / 360)

### 6.2 DELJUL Constraints (lines 835-847)

```davinci
# Line 835: Must be at least one rotation period
if(DELJUL < PERIOD) {
    printf("DELJUL < PERIOD\n")
    printf("DELJUL must be at least one period\n")
    return(null)
}

# Lines 841-847: Disable TPREDICT for short timesteps
if(DELJUL <= 3*PERIOD) {
    if(HasValue(TPREDICT)==0) TPREDICT="F"
}

if(TPREDICT=="F") {
    GGT=99.
    N3=1
    NRSET=999
}
```

### 6.3 JD/GD Date Handling (lines 330, 406-409)

```davinci
# Line 330: Convert Gregorian to Julian
if(HasValue(GD)==1) JD=GD2JD(GD)

# Lines 406-409: Calculate DJUL from JD
if(HasValue(JD)==1) {
    LKEY="F"  # Switch to Julian date mode
    DJUL=(JD-2451545)-DELJUL*(1+N5-JDISK)*(JDISK-1)/(N5-JDISK+1)
}
```

---

## 7. Changecard Generation

**Location:** Lines 1024-1133

### 7.1 Generation Order

```davinci
# 1. Type 3: LOGICAL*4 parameters (lines 1026-1033)
for(i=1; i<=length(key.part3.key); i+=1) {
    if(eval(sprintf("HasValue(%s)==1", key.part3.key[,i]))){
        params=cat(params, sprintf("3 %i %i '%s' /",
            i, atob(eval(key.part3.key[,i])), key.part3.key[,i]), axis=y)
    }
}

# 2. Type 2: INTEGER*4 parameters (lines 1036-1045)
for(i=1; i<=length(key.part2.key); i+=1) {
    if(eval(sprintf("HasValue(%s)==1", key.part2.key[,i])) &&
       key.part2.key[,i]!="N4") {
        params=cat(params, sprintf("2 %i %i '%s' /",
            i, eval(key.part2.key[,i]), key.part2.key[,i]), axis=y)
    }
}

# 3. Type 1: REAL*8 parameters (lines 1047-1089)
for(i=1; i<=length(key.part1.key); i+=1) {
    if(eval(sprintf("HasValue(%s)==1", key.part1.key[,i]))){
        # Special handling for time-varying ALBEDO/TAUD
        if (max(dim(ALBEDO))>1 && key.part1.key[,i]=="ALBEDO") {
            # Write to file, use Type 8 changecard
        } else if (max(dim(TAUD))>1 && key.part1.key[,i]=="TAUD") {
            # Write to file, use Type 8 changecard
        } else {
            # Regular scalar changecard
            if(eval(key.part1.key[,i])<1 && eval(key.part1.key[,i])>-1) {
                # Small values: scientific notation
                params=cat(params, sprintf("1 %i %.3E '%s' /",
                    i, eval(key.part1.key[,i]), key.part1.key[,i]), axis=y)
            } else {
                # Large values: fixed point
                params=cat(params, sprintf("1 %i %.4f '%s' /",
                    i, eval(key.part1.key[,i]), key.part1.key[,i]), axis=y)
            }
        }
    }
}

# 4. Type 8: Output file (line 1092) - REQUIRED
params=cat(params, sprintf("8 5 0 '%s/outdata.bin.%s' /",
    workdir, K4OUT+""), y)

# 5. Type 15: Planetary flux (lines 1095-1101) - if PFlux="T"
if(PFlux == "T") {
    params=cat(params, Planetary_Flux.line, y)
}

# 6. Type 8: Far-field output (lines 1102-1109) - if ffout set
if(ffout!="") {
    params=cat(params, sprintf("8 21 0 '%s' /", ffout), y)
}
if(ffin!="") {
    params=cat(params, sprintf("8 3 0 '%s' /", ffin), y)
}

# 7. Type 14: Eclipse (lines 1110-1112) - if Eclipse="T"
if(Eclipse == "T") {
    params=cat(params, Eclipse_line, y)
}

# 8. Terminator (lines 1132-1133)
params=cat(params,"0/", axis=y)
params=cat(params,"0/", axis=y)
```

### 7.2 Changecard Filtering

**Critical Rule:** A changecard is only written if:

```davinci
eval(sprintf("HasValue(%s)==1", parameter_name))
```

This means:
- **Written:** User explicitly set OR internally calculated/set
- **Not written:** Uses master.inp default

### 7.3 N4 Special Case (lines 1020-1022)

```davinci
# N4 is NOT written via changecard
# Instead, directly modify master.inp structure
N4part2posy=maxpos(inp.part2==grep(inp.part2,"N4")[,1])[2]
N4part2posx=strstr(inp.part2[,N4part2posy],"N4")+1
inp.part2[N4part2posx-9:N4part2posx,N4part2posy+1]=sprintf("%10s",N4+"")
```

**Reason:** KRC needs N4 before processing changecards to validate lat/elev arrays

---

## 8. Complete Parameter Trace Table

### 8.1 User-Facing Parameters (Non-Master.inp)

| Parameter | Line(s) | Default | Resolution | What Touches It | What It Touches |
|-----------|---------|---------|------------|-----------------|-----------------|
| **body** | 291 | "Mars" | User only | • porb() call<br>• Material defaults<br>• Ancillary maps | • All PORB params<br>• Mat1/Mat2<br>• INERTIA/ALBEDO/ELEV |
| **bodyforce** | 292 | 0 | User only | • porb(force=) | • PORB recalculation |
| **lat** | 327, 508-512 | 0 | User only | • Ancillary map lookup<br>• lat array (part4) | • INERTIA<br>• ALBEDO<br>• ELEV |
| **lon** | 328, 334, 513-517 | 0 | User only | • Ancillary map lookup | • INERTIA<br>• ALBEDO<br>• ELEV |
| **thick** | 326 | 0 | User only | • krc_evalN1()<br>• IC2 calculation | • N1<br>• IC2<br>• LZONE |
| **Mat1** | 539, 557, 575, 591 | "basalt" | User > body default | • Mat_Prop() lookup | • SphUp0-3<br>• ConUp0-3<br>• DENSITY<br>• SPEC_HEAT<br>• COND |
| **Mat2** | 540, 558, 576, 592 | Mat1 | User > Mat1 | • Mat_Prop() lookup | • SphLo0-3<br>• ConLo0-3<br>• DENS2<br>• SpHeat2<br>• COND2 |
| **Por1** | 617, 680 | Calculated | Material calc > User | • INERTIA<br>• Mat_Prop | • DENSITY |
| **Por2** | 644, 704 | Por1 | Por1 > User | • INERTIA2<br>• Mat_Prop | • DENS2 |
| **INERTIA2** | 542, 560, 578, 594 | INERTIA | INERTIA > User | • Lower layer calc | • COND2<br>• ConLo0-3 |
| **T_user** | 541, 559, 577, 593 | 220 (Mars/generic)<br>100 (Europa) | User > body default | • All material calcs | • SPEC_HEAT<br>• DENSITY<br>• COND<br>• ConUp/Lo<br>• SphUp/Lo |
| **k_style** | 546, 569, 582, 598 | "Mars" (Mars)<br>"Moon" (others) | User > body default | • k(T) polynomial fit | • ConUp0-3<br>• ConLo0-3 |
| **DELLS** | 347-355, 800-822 | 1.0 (or 8.0 one-point) | User only | • DELJUL calc<br>• N5/JDISK calc | • DELJUL<br>• N5<br>• JDISK |
| **hour** | 331, 755, 980-984 | -32768 | User only | • Output filtering | • process_bin52() |
| **ls** | 310, 755, 985-988 | -32768 | User only | • Output filtering | • process_bin52() |
| **JD** | 330, 406-409 | Not set | User or from GD | • DJUL calc<br>• LKEY | • DJUL<br>• LKEY="F" |
| **GD** | 330, 117-118 | Not set | User only | • GD2JD() | • JD<br>• DJUL<br>• LKEY |
| **TUN8** | 371-380, 761-762 | 0 | User only | • TUN_Flx15 map | • TUN_Flx15<br>• Output structure |
| **lbound** | 777-797 | -1//180 | User only | • IIB/TDEEP parsing | • IIB<br>• TDEEP |
| **TPREDICT** | 841-856 | "T" (if DELJUL>3×PERIOD) | User > DELJUL check | • GGT<br>• N3<br>• NRSET | • GGT<br>• N3<br>• NRSET |
| **Eclipse** | 315, 451-463 | "F" | User only | • Eclipse line gen | • Eclipse_line (Type 14) |
| **PFlux** | 317, 419-447 | "F" | User only | • Planetary flux calc | • Planetary_Flux.line (Type 15) |
| **anc** | 329 | 0 | User only | • Output structure | • out.anc field |
| **stability** | 332 | 0 | User only | • krc_stability_flag() | • Post-run checking |
| **v** | 284-288 | 0 | User only | • Verbose output | • printf() calls |
| **MAXN1** | 318 | 1000 | User only | • N1 constraint | • krc_evalN1() |
| **MAXN2** | 319 | 86400 | User only | • N2 constraint | • krc_evalN2() |

### 8.2 Master.inp REAL*8 Parameters (Part1, 64 total)

| Index | Parameter | Line(s) | Default Source | Touched By | Touches | Changecard? |
|-------|-----------|---------|----------------|------------|---------|-------------|
| 1 | **ALBEDO** | 543, 561-563, 579, 595 | TES map (Mars)<br>0.67 (Europa)<br>0.67 (generic) | • lat/lon (if Mars)<br>• body<br>• Time-varying array | • Surface heat balance<br>• Changecard Type 1 or 8 22 | Yes if set OR time-varying |
| 2 | **EMISS** | 307 | 1.0 | • User only | • Surface heat balance | Yes if user set |
| 3 | **INERTIA** | 534, 549-551, 572, 586 | TES map (Mars)<br>100 (Europa)<br>100 (generic) | • lat/lon (if Mars)<br>• Mat1/Por1/T_user<br>• COND/DENSITY/SPEC_HEAT | • All material calcs<br>• N1/N2 calcs | Yes if set |
| 4 | **COND2** | 646, 702 | Calc from INERTIA2 | • INERTIA2<br>• DENS2<br>• SpHeat2 | • Lower layer heat conduction | Yes if calculated |
| 5 | **DENS2** | 645, 697 | Calc from Mat2/Por2 | • Mat2<br>• Por2<br>• T_user | • COND2<br>• SpHeat2 | Yes if calculated |
| 6 | **PERIOD** | 341, 835, 844 | PORB | • porb(body) | • DELJUL calc<br>• N2 calc<br>• Time axis | Yes (from PORB) |
| 7 | **SPEC_HEAT** | 535, 553, 555, 573-574, 587-588, 616, 672 | Calc from Mat1/T_user | • Mat1<br>• T_user<br>• SphUp0-3 | • INERTIA<br>• COND<br>• DENSITY | Yes if calculated |
| 8 | **DENSITY** | 535, 553-554, 573, 587-588, 618, 673 | Calc from Mat1/Por1 | • Mat1<br>• Por1<br>• T_user | • INERTIA<br>• COND<br>• N2 calc | Yes if calculated |
| 9 | **CABR** | (not set) | master.inp | - | • Unused? | No |
| 10 | **AMW** | (not set) | master.inp | - | • Atmospheric MW | No |
| 11 | **SatPrA** | (not set) | master.inp | - | • Saturation pressure | No |
| 12 | **PTOTAL** | 342, 547, 570, 583, 599 | PORB | • porb(body) | • LATM flag<br>• TAUD forcing | Yes (from PORB) |
| 13 | **FANON** | (master.inp) | 0.055 (master.inp) | - | • Non-condensable gas fraction | No unless set |
| 14 | **TATM** | (not set) | PORB or master.inp | • porb or ffin | • Atmospheric temp | No unless set |
| 15 | **TDEEP** | 778, 783, 788 | 180 | • lbound param | • IIB<br>• Bottom BC | Yes if lbound set |
| 16 | **SpHeat2** | 643, 696 | Calc from Mat2/T_user | • Mat2<br>• T_user<br>• SphLo0-3 | • DENS2<br>• COND2 | Yes if calculated |
| 17 | **TAUD** | 325, 547, 570, 583, 599 | 0.30<br>0.0 (if PTOTAL<1) | • PTOTAL<br>• Time-varying array | • Atmospheric opacity<br>• Changecard Type 1 or 8 23 | Yes if set OR time-varying |
| 18 | **DUSTA** | 343 | PORB | • porb(body) | • Dust scattering | Yes (from PORB) |
| 19 | **TAURAT** | 345 | PORB<br>0.25 (master.inp) | • porb(body) | • IR/vis opacity ratio | Yes (from PORB) |
| 20 | **TWILI** | (not set) | master.inp | - | • Twilight scattering | No unless set |
| 21 | **ARC2_G0** | 346 | PORB | • porb(body) | • Photometric function | Yes (from PORB) |
| 22 | **ARC3_Safe** | (not set) | master.inp | - | • Photometric function | No unless set |
| 23 | **SLOPE** | 321, 965 | 0 | • User | • Solar insolation | Yes if user set |
| 24 | **SLOAZI** | 322, 965 | 0 | • User | • Solar insolation | Yes if user set |
| 25 | **TFROST** | 545, 581, 597, 773 | 0 or calc if LVFT="T" | • LVFT<br>• krc_cond_gas() | • Condensation | Yes if LVFT="T" |
| 26 | **CFROST** | (from krc_cond_gas) | Set if LVFT="T" | • krc_cond_gas() | • Latent heat | Yes if LVFT="T" |
| 27 | **AFROST** | (from krc_cond_gas) | Set if LVFT="T" | • krc_cond_gas() | • Frost albedo | Yes if LVFT="T" |
| 28-32 | **FEMIS, AF1, AF2, FROEXT, SatPrB** | (not set) | master.inp | - | • Frost params | No unless set |
| 33 | **RLAY** | 772, 867 | 1.2 (master.inp) | • master.inp or user | • N1 calc<br>• Layer spacing | Yes if user set |
| 34 | **FLAY** | 771, 866-867 | 0.18 (master.inp) | • master.inp or user | • N1 calc<br>• N2 calc<br>• First layer depth | Yes if user set |
| 35-39 | **CONVF, DEPTH, DRSET** | (not set) | master.inp | - | • Various | No unless set |
| 36 | **PhotoFunc** | 335, 936-939 | 0 | • User | • Photometric model | Yes if user set |
| 37 | **GGT** | 849, 853 | 99.0 or from master.inp | • TPREDICT | • Temperature prediction | Yes if TPREDICT="F" |
| 38 | **DTMAX** | (not set) | master.inp | - | • Max timestep | No unless set |
| 39 | **DJUL** | 313, 408, 913 | 0.1 (Ls) or from JD | • ls input<br>• JD/GD input | • Starting date | Yes if user set |
| 40 | **DELJUL** | 347-355, 408, 813, 835-839, 913 | PORB or calc from DELLS | • PORB<br>• DELLS<br>• PERIOD | • Season spacing<br>• TPREDICT | Yes (from PORB or DELLS) |
| 41-48 | **SOLARDEC, DAU, LsubS, SOLCON, GRAV, Atm_Cp** | PORB | • porb(body) | • Orbital/atmospheric params | Yes (from PORB) |
| 49-52 | **ConUp0-3** | 633-636, 674-677 | Fitted from k_style | • k_style<br>• COND<br>• T_user<br>• Mat1 | • k(T) in Fortran | Yes (Type 12 changecard) |
| 53-56 | **ConLo0-3** | 660-663, 698-701 | Fitted from k_style | • k_style<br>• COND2<br>• T_user<br>• Mat2 | • k(T) lower layer | Yes (Type 12 changecard) |
| 57-60 | **SphUp0-3** | 612-615, 668-671 | From Mat_Prop(Mat1) | • Mat1<br>• T_user | • Cp(T) in Fortran<br>• SPEC_HEAT | Yes (Type 13 changecard) |
| 61-64 | **SphLo0-3** | 639-642, 692-695 | From Mat_Prop(Mat2) | • Mat2<br>• T_user | • Cp(T) lower layer<br>• SpHeat2 | Yes (Type 13 changecard) |

### 8.3 Master.inp INTEGER*4 Parameters (Part2, 20 total)

| Index | Parameter | Line(s) | Default Source | Touched By | Touches | Changecard? |
|-------|-----------|---------|----------------|------------|---------|-------------|
| 1 | **N1** | 869-884, 886 | krc_evalN1() | • FLAY<br>• RLAY<br>• thick<br>• INERTIA<br>• DELJUL<br>• N5<br>• JDISK<br>• PERIOD | • Number of subsurface layers<br>• IC2 limit | Yes (calculated) |
| 2 | **N2** | 899-901 | krc_evalN2() | • FLAY<br>• PERIOD<br>• N24<br>• INERTIA<br>• DENSITY<br>• SPEC_HEAT | • Timesteps per day<br>• Courant stability | Yes (calculated) |
| 3 | **N3** | 850, 854 | 1 or 15 (master.inp) | • TPREDICT | • Iteration days | Yes if TPREDICT="F" |
| 4 | **N4** | 320 | 1 | • User | • Number of latitudes<br>• lat/elev array size | **NO** (written to header directly) |
| 5 | **N5** | 311, 408, 816-817, 830-833 | 1080 or calc from DELLS | • DELLS<br>• User | • Total seasons | Yes if user set or DELLS |
| 6 | **N24** | 340, 766 | PORB (96 for Mars) | • porb(body) | • Output timesteps/day<br>• N2 calc | Yes (from PORB) |
| 7 | **IIB** | 777-797, 930-934 | -1 | • lbound param | • Bottom BC type | Yes if lbound set |
| 8 | **IC2** | 868, 922-926 | 999 or calc from thick | • thick<br>• krc_evalN1() | • Layer property change index | Yes if thick≠0 |
| 9 | **NRSET** | 851, 855 | 999 or from master.inp | • TPREDICT | • Temperature resets | Yes if TPREDICT="F" |
| 10-11 | **NMHA, NRUN** | (internal) | master.inp | - | • Run control | No unless set |
| 12 | **JDISK** | 312, 324, 408, 817, 913 | 721 or calc from DELLS | • DELLS<br>• User<br>• JBARE | • First output season | Yes if user set or DELLS |
| 13-14 | **IDOWN, FlxP14** | (internal) | master.inp | - | • Internal flags | No unless set |
| 15 | **TUN_Flx15** | 371-380, 761-762 | 0 | • TUN8 user param | • Temperature/atm output | Yes if TUN8 set |
| 16 | **KPREF** | 367, 954-958 | 1 (Mars) or from krc_cond_gas | • LVFT<br>• body | • Pressure model | Yes if LVFT="T" |
| 17 | **K4OUT** | 738 | 52 | • Hardcoded | • Output format | No (forced to 52) |
| 18 | **JBARE** | 323-324 | 0 | • User | • Frost removal season | Yes if user set |
| 19-20 | **Notif, IDISK2** | (internal) | master.inp | - | • Internal flags | No unless set |

### 8.4 Master.inp LOGICAL*4 Parameters (Part3, 20 total)

| Index | Parameter | Line(s) | Default | Touched By | Touches | Changecard? |
|-------|-----------|---------|---------|------------|---------|-------------|
| 1-7 | **LP1-6, LPGLOB** | (internal) | "F" | - | • Internal flags | No unless set |
| 8 | **LVFA** | (not used) | "F" | - | • Variable atm? | No unless set |
| 9 | **LVFT** | 336, 356-363 | "F" | • User | • krc_cond_gas()<br>• TFROST/CFROST/AFROST<br>• KPREF | Yes if user set |
| 10 | **LKofT** | 333, 862-864, 874-877, 929 | "T" | • User | • k(T) and Cp(T) polynomials<br>• ConUp/Lo<br>• SphUp/Lo usage | Yes if user set |
| 11 | **LPORB** | (internal) | "T" | - | • PORB usage flag | No (always "T" in davinci) |
| 12 | **LKEY** | 314, 407 | "T" | • User<br>• JD/GD input | • DJUL interpretation (Ls vs JD) | Yes if user set or JD/GD |
| 13 | **LSC** | (not used) | "F" | - | • Unknown | No unless set |
| 14 | **LZONE** | 888, 891-896 | Calc by krc_evalN1() | • thick<br>• krc_evalN1() | • Zone file usage<br>• Type 8 25 changecard | Yes if thick≠0 |
| 15-20 | **LOCAL, Prt76, LPTAVE, Prt78, Prt79, L_ONE** | (internal) | "F" | - | • Print flags | No unless set |

---

## 9. Critical Differences vs PyKRC

### 9.1 Default Value Discrepancies

| Parameter | Davinci Default | PyKRC Default | Impact | Line References |
|-----------|----------------|---------------|--------|-----------------|
| **RLAY** | 1.15 (master.inp) | 1.08 | **HIGH** - Changes layer spacing | 772 |
| **FLAY** | 0.10 (master.inp) | 2.0 | **HIGH** - Changes first layer depth | 771 |
| **FANON** | 0.055 (master.inp) | 0.3 | **MEDIUM** - Non-condensable fraction | master.inp default |
| **TAURAT** | 0.25 (master.inp) | 2.0 | **MEDIUM** - IR/vis opacity ratio | master.inp default |
| **N3** | 15 (master.inp) | 1 | **LOW** - Iteration convergence | 850 |
| **TPREDICT** | "T" (if DELJUL>3×PERIOD) | ? | **MEDIUM** - Temperature prediction | 841-847 |

### 9.2 Parameter Resolution Order

**Davinci precedence (confirmed):**
```
User Explicit → PORB → Material → Master.inp → Hardcoded
```

**PyKRC must match this exactly!**

### 9.3 Material Property Calculation Order

**Critical order in davinci (lines 602-636):**

1. Get Mat_Prop(Mat1)
2. Calculate SPEC_HEAT at T_user
3. Calculate Por1 from INERTIA (if not user-set)
4. Calculate DENSITY from Mat_Prop, Por1, T_user
5. Calculate COND = INERTIA²/(DENSITY×SPEC_HEAT)
6. Generate T_Tab (80K to 478K)
7. Calculate k_Table using k_style
8. Fit cubic polynomial → ConUp0-3

**PyKRC MUST follow this exact sequence!**

### 9.4 PORB Parameter Injection

**Davinci injects ALL porb.krc parameters** unless:
1. User explicitly provided
2. Parameter is DELJUL and DELLS is set

**PyKRC must implement same logic!**

### 9.5 Changecard Generation Rules

**Davinci writes changecards for:**
1. User-set parameters
2. PORB-injected parameters (since HasValue() becomes true after injection)
3. Calculated parameters (Mat_Prop derivatives, N1/N2, etc.)

**NOT written:**
- Master.inp defaults that remain unchanged
- N4 (written to header instead)

### 9.6 N4 Special Handling

**Davinci directly modifies master.inp structure (lines 1020-1022):**
```davinci
N4part2posx=strstr(inp.part2[,N4part2posy],"N4")+1
inp.part2[N4part2posx-9:N4part2posx,N4part2posy+1]=sprintf("%10s",N4+"")
```

**PyKRC must NOT write N4 changecard!**

### 9.7 Time-Varying ALBEDO/TAUD

**Davinci writes to file + Type 8 changecard:**
- `8 22 0 '/path/albfile.tab' /` for ALBEDO
- `8 23 0 '/path/taufile.tab' /` for TAUD

**Format:**
```
<Ls1>    <value1>
<Ls2>    <value2>
...
```

---

## 10. Validation Checklist for PyKRC

### 10.1 Parameter Resolution

- [ ] User → PORB → Material → Master.inp precedence order
- [ ] PORB injection loop matches davinci (lines 469-484)
- [ ] DELLS blocks DELJUL from PORB
- [ ] JD/GD set LKEY="F" and calculate DJUL
- [ ] PTOTAL<1 forces TAUD=0

### 10.2 Material Properties

- [ ] Mat_Prop() lookup matches davinci material database
- [ ] SPEC_HEAT calculation from SphUp0-3 at T_user
- [ ] Por1 calculation from INERTIA if not user-set
- [ ] DENSITY calculation from Mat_Prop, Por1, T_user
- [ ] COND = INERTIA²/(DENSITY×SPEC_HEAT)
- [ ] k(T) polynomial fit matches k_style (Moon/Mars/Bulk)
- [ ] Same for lower layer (Mat2, Por2, INERTIA2)

### 10.3 Numerical Model Setup

- [ ] N1 = krc_evalN1() logic matches davinci
- [ ] N2 = krc_evalN2() logic matches davinci
- [ ] IC2 calculated from thick, N1
- [ ] LZONE set if thick≠0 and zone file needed
- [ ] TPREDICT auto-disabled if DELJUL≤3×PERIOD

### 10.4 Changecard Generation

- [ ] Type 3 (LOGICAL) written first
- [ ] Type 2 (INTEGER) written second, **N4 skipped**
- [ ] Type 1 (REAL) written third, correct precision
- [ ] Type 8 output file (8 5 0) always written
- [ ] Type 15 planetary flux if PFlux="T"
- [ ] Type 14 eclipse if Eclipse="T"
- [ ] Type 8 22/23 for time-varying ALBEDO/TAUD
- [ ] Terminator: two "0/" lines

### 10.5 Input File Format

- [ ] Header: "0 0 /" regardless of KEEP
- [ ] REAL*8: 10-char fields, %.2f, 8 per line
- [ ] INTEGER*4: 10-char fields, %d, 8/8/4 lines
- [ ] LOGICAL*4: 7-char fields, T/F, 10/10 lines
- [ ] lat/elev: 7-char fields, %.2f, 10 per line
- [ ] PORB: 15-char fields, %.7f, 5 per line (6 lines)
- [ ] N4 written directly to INTEGER section line 1

### 10.6 Default Values

- [ ] RLAY=1.15 (not 1.08)
- [ ] FLAY=0.10 (not 2.0)
- [ ] FANON=0.055 (master.inp)
- [ ] TAURAT=0.25 (master.inp default, overridden by PORB)
- [ ] All other defaults match master.inp or PORB

---

## Appendix A: Function Call Tree

```
krc()
├─ porb(body, force=bodyforce)                    [line 295]
│  └─ Returns: porb.krc (parameters)
│     Returns: porb.rot (30-element matrix)
│     Returns: porb.type (body classification)
│
├─ Mat_Prop(Mat1)                                 [lines 611, 666]
│  └─ Returns: Mat_Prop.SpH (Cp coefficients)
│     Returns: Mat_Prop.Dens (density coefficients)
│     Returns: Mat_Prop.ConB (bulk conductivity coefficients)
│
├─ krc_cond_gas(body)                             [line 358]
│  └─ Returns: TFROST, CFROST, AFROST, KPREF
│
├─ krc_evalN1(...)                                [lines 869-884]
│  └─ Returns: N1, IC2, LZONE, Zone.inp
│
├─ krc_evalN2(...)                                [lines 899-901]
│  └─ Returns: N2
│
├─ krc_planetary_flux_porb(porb, porb_Planet, Lon_Hr) [line 431, 446]
│  └─ Returns: Planetary_Flux.line (Type 15 changecard)
│
├─ krc_planetary_flux_table(IR, Vis, Lon_Hr)      [line 425]
│  └─ Returns: Planetary_Flux.line (Type 15 changecard)
│
├─ GD2JD(GD)                                      [line 330]
│  └─ Returns: Julian Date
│
├─ atob(logical_string)                           [line 1030]
│  └─ Returns: 0 or 1
│
├─ fit(y, x, "cube", plot=0)                      [lines 631, 656, 685, 709]
│  └─ Returns: [c0, c1, c2, c3] cubic coefficients
│
└─ process_bin52(...)                             [line 1547]
   └─ Parses output files into structure
```

---

## Appendix B: Master.inp Structure

**Loaded by:** `krc_process_input()` at script initialization

**Structure:**
```davinci
master.inp.header     # Lines 1-2
master.inp.part1      # REAL*8 parameters (64 values, 8 lines)
master.inp.part2      # INTEGER*4 parameters (20 values, 3 lines)
master.inp.part3      # LOGICAL*4 parameters (20 values, 2 lines)
master.inp.part4      # Latitude array (N4 values)
master.inp.part5      # Elevation array (N4 values)
master.inp.part6      # PORB matrix (30 values, 6 lines)

key.part1.key[]       # Parameter names for part1
key.part1.value[]     # Default values for part1
key.part2.key[]       # Parameter names for part2
key.part2.value[]     # Default values for part2
key.part3.key[]       # Parameter names for part3
key.part3.value[]     # Default values for part3
```

**Pre-loaded as global:** `krc_master` (line 66, 395-396)

---

## Appendix C: Example Parameter Trace

**Scenario:** User calls `krc(lat=30, INERTIA=250, body="Mars", DELLS=5)`

### Trace: INERTIA

1. **Line 291:** body="Mars" (user)
2. **Line 295:** porb=porb("Mars")
3. **Line 534:** Check HasValue(INERTIA)
4. **Result:** HasValue(INERTIA)==1 (user set to 250)
5. **Skip:** Lines 549-551 (TES map lookup skipped)
6. **Line 617:** Calculate Por1 = 0.60 × (2200 - 250)/2200 = 0.532
7. **Line 618:** Calculate DENSITY from Mat_Prop, Por1
8. **Line 619:** Calculate COND = 250²/(DENSITY×SPEC_HEAT)
9. **Line 1078:** Write changecard: `1 3 250.0000 'INERTIA' /`

### Trace: DELLS

1. **Line 347:** Check HasValue(DELLS)
2. **Result:** HasValue(DELLS)==1 (user set to 5)
3. **Line 350:** DELJUL = 1.0275/360 × 5 = 0.01427... NO wait
4. **Line 813:** DELJUL = PERIOD/360 × DELLS where PERIOD=686.97 days (Mars year)
5. **Result:** DELJUL = 686.97/360 × 5 = 9.541 days
6. **Line 816:** N5 = ceil(360/5 × 3) = ceil(216) = 216
7. **Line 817:** JDISK = ceil(360/5 × 2 + 1) = ceil(145) = 145
8. **Line 1078:** Write changecard: `1 40 9.5410 'DELJUL' /`
9. **Line 1037:** Write changecard: `2 5 216 'N5' /`
10. **Line 1037:** Write changecard: `2 12 145 'JDISK' /`

### Trace: PERIOD

1. **Line 341:** Check HasValue(PERIOD)
2. **Result:** HasValue(PERIOD)==0 (not user-set)
3. **Line 341:** PERIOD = porb.krc.PERIOD = 1.0275 (Mars rotation)
4. **Line 471-480:** PORB injection loop
5. **Result:** HasValue(PERIOD)==1 (set by PORB)
6. **Line 1078:** Write changecard: `1 6 1.0275 'PERIOD' /`

### Trace: N1

1. **Line 869:** Calculate N1 = krc_evalN1(...)
2. **Result:** N1 = 28 (example)
3. **Result:** HasValue(N1)==1 (calculated)
4. **Line 1037:** Write changecard: `2 1 28 'N1' /`

### Trace: N4

1. **Line 320:** N4=1 (default, user didn't set)
2. **Line 1020-1022:** Direct modification of master.inp.part2
3. **Result:** N4 written to input file header, **NO CHANGECARD**

---

**END OF DOCUMENT**
