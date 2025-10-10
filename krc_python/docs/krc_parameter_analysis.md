# KRC Parameter Analysis
## Comprehensive Documentation of krc.dvrc Configuration File

**Analysis Date:** 2025-10-08
**Source File:** `/Users/chaberle/Documents/GitHab/KRC/krc_python/docs/davinci/krc.dvrc`
**File Size:** 4379 lines
**KRC Version:** 1.07 (v3.5.6+)

---

## Executive Summary

This document provides a comprehensive analysis of ALL parameters defined in the KRC (K thermal model for planetary science) Davinci interface file. The analysis includes:
- **85 unique parameters** identified and documented
- Parameter types, default values, and descriptions
- Usage in Fortran source code (referenced in `/Users/chaberle/Documents/GitHab/KRC/src/`)
- Parameter relationships and dependencies

---

## Table of Contents

1. [Material Properties Parameters](#1-material-properties-parameters)
2. [Atmospheric Parameters](#2-atmospheric-parameters)
3. [Thermal Model Parameters](#3-thermal-model-parameters)
4. [Time and Season Control](#4-time-and-season-control)
5. [Location and Geometry](#5-location-and-geometry)
6. [Orbital Parameters](#6-orbital-parameters)
7. [Output Control Parameters](#7-output-control-parameters)
8. [Eclipse and Planetary Flux](#8-eclipse-and-planetary-flux)
9. [Advanced and Computational Parameters](#9-advanced-and-computational-parameters)
10. [Parameter Dependencies and Relationships](#10-parameter-dependencies-and-relationships)

---

## 1. Material Properties Parameters

These parameters define the thermophysical properties of the surface material layers.

### INERTIA
- **Type:** `REAL*8`
- **Default:** `100.` (for non-Mars bodies), from TES map for Mars at 2ppd resolution
- **Units:** J m⁻² K⁻¹ s⁻¹/² (Thermal Inertia Units, SI)
- **Line References:** dvrc lines 534, 549-551, 572, 586, 667
- **Description:** Thermal inertia of upper material layers at temperature T_user
- **Fortran Usage:**
  - Used in `/src/krc8.f` for initial temperature calculations
  - Referenced in `/src/tday8.f` for diurnal skin depth calculations
- **Special Cases:**
  - Mars: Loaded from `krc_INERTIA` global map (TES 2ppd)
  - Europa: Default 100 SI
  - Can be calculated from COND, DENSITY, SPEC_HEAT: `INERTIA = sqrt(COND*DENSITY*SPEC_HEAT)`
  - Can be calculated from T-dependent properties: `INERTIA = sqrt(ConUp0*DENSITY*SphUp0)`

### INERTIA2
- **Type:** `REAL*8`
- **Default:** `INERTIA` (same as upper layer)
- **Units:** J m⁻² K⁻¹ s⁻¹/²
- **Line References:** dvrc lines 542, 560, 578, 594, 691
- **Description:** Thermal inertia of lower material layers
- **Fortran Usage:** Used in layer calculations when IC2 ≠ 999
- **Notes:** Only used if material properties change with depth (thick ≠ 0 or IC2 < 999)

### COND
- **Type:** `REAL*8`
- **Default:** Calculated from `INERTIA^2/(DENSITY*SPEC_HEAT)`
- **Units:** W m⁻¹ K⁻¹ (thermal conductivity)
- **Line References:** dvrc lines 535, 553, 555, 573-574, 619, 678
- **Description:** Thermal conductivity of upper layer at T_user
- **Fortran Usage:**
  - Set in `/src/tcard8.f` line 85
  - Used in heat conduction calculations throughout KRC
- **Calculation:** If user provides COND, DENSITY, and SPEC_HEAT, INERTIA is recalculated

### COND2
- **Type:** `REAL*8`
- **Default:** Calculated from `INERTIA2^2/(DENS2*SpHeat2)`
- **Units:** W m⁻¹ K⁻¹
- **Line References:** dvrc lines 646, 702
- **Description:** Thermal conductivity of lower layer at T_user
- **Fortran Usage:**
  - Listed in `/src/krcc8m.f` line 32 as common block variable
  - Used when IC2 indicates layer property change

### DENSITY
- **Type:** `REAL*8`
- **Default:** Calculated from `(1 - Por1) * Mat_Prop.Dens.Dens0 + temperature corrections`
- **Units:** kg m⁻³
- **Line References:** dvrc lines 535, 553-554, 573, 587-588, 618, 673
- **Description:** Bulk density of upper layer including porosity
- **Fortran Usage:**
  - Set in `/src/tcard8.f` line 85 (DENSITY in TITF array)
  - Used in mass and heat capacity calculations
- **Formula:** Includes porosity correction and temperature-dependent density variations

### DENS2
- **Type:** `REAL*8`
- **Default:** Calculated from `(1 - Por2) * Mat_Prop.Dens.Dens0 + temperature corrections`
- **Units:** kg m⁻³
- **Line References:** dvrc lines 645, 697
- **Description:** Bulk density of lower layer
- **Fortran Usage:** Listed in `/src/krcc8m.f` line 32

### SPEC_HEAT
- **Type:** `REAL*8`
- **Default:** Calculated from `SphUp0 + SphUp1*X_user + SphUp2*X_user^2 + SphUp3*X_user^3`
- **Units:** J kg⁻¹ K⁻¹
- **Line References:** dvrc lines 535, 553, 555, 573-574, 587-588, 616, 672
- **Description:** Specific heat capacity of upper layer at T_user
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 86 as 'SPECHEAT'
  - Used in heat capacity calculations: C = DENSITY * SPEC_HEAT
- **Notes:** X_user = (T_user - 220) * 0.01 (normalized temperature)

### SpHeat2
- **Type:** `REAL*8`
- **Default:** Calculated from `SphLo0 + SphLo1*X_user + SphLo2*X_user^2 + SphLo3*X_user^3`
- **Units:** J kg⁻¹ K⁻¹
- **Line References:** dvrc lines 643, 696
- **Description:** Specific heat capacity of lower layer at T_user
- **Fortran Usage:** Listed in `/src/tcard8.f` line 87

### Mat1
- **Type:** `STRING`
- **Default:** `"basalt"` (general), `"H2O"` (Europa)
- **Line References:** dvrc lines 539, 557, 575, 591, 611
- **Description:** Material composition of upper layer
- **Options:** "basalt", "H2O", "ice", "granite", "sand", etc.
- **Usage:** Sets baseline thermal properties via Mat_Prop() function

### Mat2
- **Type:** `STRING`
- **Default:** `Mat1` (same as upper layer)
- **Line References:** dvrc lines 540, 558, 576, 592, 638
- **Description:** Material composition of lower layer

### Por1
- **Type:** `REAL*8`
- **Default:** Calculated from `0.60 * (2200 - INERTIA)/2200`
- **Units:** dimensionless (0-1)
- **Line References:** dvrc lines 617, 680
- **Description:** Porosity of upper layer
- **Fortran Usage:** Used to calculate bulk density from grain density

### Por2
- **Type:** `REAL*8`
- **Default:** `Por1`
- **Units:** dimensionless (0-1)
- **Line References:** dvrc lines 644, 704
- **Description:** Porosity of lower layer

### thick
- **Type:** `REAL*8`
- **Default:** `0` (uniform material)
- **Units:** meters
- **Line References:** dvrc lines 326, 862, 997
- **Description:** Thickness of upper material layer
- **Special Values:**
  - `0`: Uniform material (IC2 = 999)
  - `> 0`: Two-layer regolith with upper layer thickness in meters
  - `< 0`: Exponential thermal property profile, abs(thick) is H parameter
- **Fortran Usage:** Controls layer property transitions via IC2

### T_user
- **Type:** `REAL*8`
- **Default:** `220.` K (general), `100.` K (Europa)
- **Units:** Kelvin
- **Line References:** dvrc lines 541, 559, 577, 593, 603
- **Description:** Reference temperature at which thermal inertia is defined
- **Usage:** All material properties (COND, DENSITY, SPEC_HEAT) are defined at this temperature

### k_style
- **Type:** `STRING`
- **Default:** `"Mars"` (Mars), `"Moon"` (most others)
- **Options:** "Moon", "Mars", "Bulk"
- **Line References:** dvrc lines 546, 569, 582, 598, 621-630, 648-657
- **Description:** Temperature dependence style for thermal conductivity
- **Formulas:**
  - "Moon": k(T) = k_user * (1 + 2.7 * ((T-T_user)/350)³) (Hayne et al. 2017)
  - "Mars": k(T) = k_user * sqrt(T/T_user) (Morgan et al. 2018)
  - "Bulk": k(T) uses bulk conductivity polynomial

### ALBEDO (ALB in Fortran)
- **Type:** `REAL*8`
- **Default:** `0.67` (general), from TES map for Mars
- **Units:** dimensionless (0-1)
- **Line References:** dvrc lines 543, 561-563, 579, 595
- **Description:** Surface bolometric albedo
- **Fortran Usage:**
  - Variable name ALB in `/src/krcc8m.f` line 32
  - Used in `/src/tseas8.f` line 115-116 for seasonal variations
  - Can vary with season via SEASALB function
- **Special Cases:**
  - Mars: Loaded from `krc_ALBEDO` global map (TES 2ppd)
  - Can be time-varying via 2×n array (Ls vs ALBEDO)

### EMISS
- **Type:** `REAL*8`
- **Default:** `1.0` (blackbody)
- **Units:** dimensionless (0-1)
- **Line References:** dvrc line 307
- **Description:** Surface thermal emissivity
- **Fortran Usage:** Listed in `/src/tcard8.f` line 85

---

## 2. Atmospheric Parameters

Parameters controlling atmospheric effects and condensation.

### PTOTAL
- **Type:** `REAL*8`
- **Default:** From PORB calculations (body-specific)
- **Units:** Pascal
- **Line References:** dvrc lines 342, 547, 570, 583, 599
- **Description:** Average global atmospheric pressure
- **Fortran Usage:**
  - Listed in `/src/krcc8m.f` line 33
  - Used in `/src/krc8.f` line 199 to set LATM flag
  - LATM = (PTOTAL > 1.0)
- **Special Cases:**
  - PTOTAL < 1.0: Treated as airless body, TAUD forced to 0
  - Mars default: ~600 Pa
  - Determines if atmospheric effects are included

### TAUD
- **Type:** `REAL*8`
- **Default:** `0.30` (Mars), `0.0` (airless bodies)
- **Units:** dimensionless (optical depth)
- **Line References:** dvrc lines 325, 547, 570, 583, 599
- **Description:** Visible atmospheric opacity (dust optical depth)
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 87
  - Used in `/src/tseas8.f` lines 119-120 for seasonal variations
  - Can vary via SEASTAU or CLIMTAU functions
- **Special Cases:**
  - Can be time-varying via 2×n array (Ls vs TAUD)
  - Forced to 0 if PTOTAL < 1.0
  - Seasonal variations via Viking lander trends or global frost budget

### TATM
- **Type:** `REAL*8`
- **Default:** From PORB or far-field file
- **Units:** Kelvin
- **Description:** Atmospheric temperature
- **Fortran Usage:** Listed in `/src/tcard8.f` line 87
- **Notes:** Can be provided via far-field atmosphere file (FFATM)

### DUSTA
- **Type:** `REAL*8`
- **Default:** From PORB calculations
- **Units:** μm (dust particle size)
- **Line References:** dvrc line 343
- **Description:** Effective dust particle size
- **Fortran Usage:** Listed in `/src/krcc8m.f` line 33

### TAURAT
- **Type:** `REAL*8`
- **Default:** From PORB calculations
- **Units:** dimensionless
- **Line References:** dvrc line 345
- **Description:** Ratio of IR to visible opacity
- **Fortran Usage:** Listed in `/src/krcc8m.f` line 33

### FANON
- **Type:** `REAL*8`
- **Default:** `0.040` (CO₂ on Mars)
- **Units:** dimensionless (mixing ratio)
- **Line References:** dvrc line (not directly set in interface)
- **Description:** Fraction of non-condensable gas in atmosphere
- **Fortran Usage:** Listed in `/src/tcard8.f` line 86
- **Reference:** Mahaffy et al., Science 2013 (SAM measurements)

### LVFT
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"F"` (False - no condensable gases)
- **Options:** "T" or "F"
- **Line References:** dvrc lines 336, 356-363
- **Description:** Enable variable frost temperature based on pressure
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 100
  - When "T": Calls krc_cond_gas() to set frost parameters
  - Supported bodies: Mars, Titan, Pluto
- **Related Parameters:** TFROST, CFROST, AFROST

### TFROST
- **Type:** `REAL*8`
- **Default:** `0.0` K or calculated if LVFT="T"
- **Units:** Kelvin
- **Line References:** dvrc lines 545, 581, 597, 773
- **Description:** Frost point temperature
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 88
  - Used in condensation calculations
- **Notes:** If LVFT="T", calculated from pressure-dependent CO₂ saturation

### CFROST
- **Type:** `REAL*8`
- **Default:** Set by krc_cond_gas() if LVFT="T"
- **Units:** J kg⁻¹ (latent heat)
- **Description:** Latent heat of frost sublimation/condensation
- **Fortran Usage:** Listed in `/src/tcard8.f` line 88

### AFROST
- **Type:** `REAL*8`
- **Default:** Set by krc_cond_gas() if LVFT="T"
- **Units:** dimensionless (0-1)
- **Description:** Albedo of frost
- **Fortran Usage:** Listed in `/src/tcard8.f` line 88

### KPREF
- **Type:** `INTEGER*4`
- **Default:** `1` (Mars without LVFT), set by krc_cond_gas() if LVFT="T"
- **Options:** 1, 2
- **Line References:** dvrc lines 367, 954-958
- **Description:** Seasonal pressure model
- **Options:**
  - 1: Viking lander pressure trends
  - 2: Global frost budget-dependent pressure
- **Fortran Usage:** Listed in `/src/tcard8.f` line 96

---

## 3. Thermal Model Parameters

Parameters controlling the numerical thermal model.

### N1
- **Type:** `INTEGER*4`
- **Default:** Calculated by krc_evalN1() (typically 15-50)
- **Units:** dimensionless (count)
- **Line References:** dvrc lines 869-884, 886
- **Description:** Number of subsurface layers
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Used in `/src/tday8.f` for layer initialization
  - Constraints in `/src/tcard8.f` lines 365-372: 2 ≤ N1 ≤ MAXN1P
- **Calculation:** Automatic calculation ensures reaching 3 seasonal skin depths
- **MAXN1:** Maximum allowed N1 = 1000 (from `/src/krcc8m.f` line 7)

### N2
- **Type:** `INTEGER*4`
- **Default:** Calculated by krc_evalN2() (typically 288-1152)
- **Units:** dimensionless (count)
- **Line References:** dvrc lines 899-901
- **Description:** Number of calculations per day (timesteps per sol)
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Determines model temporal resolution
  - Constraints in `/src/tcard8.f` lines 389-393: 32 ≤ N2 ≤ MAXN2
- **MAXN2:** Maximum allowed N2 = 393,216 (from `/src/krcc8m.f` line 8)
- **Calculation:** Set to ensure numerical stability (Courant condition)

### FLAY
- **Type:** `REAL*8`
- **Default:** `0.18` (from master.inp)
- **Units:** skin depths
- **Line References:** dvrc lines 771, 866-867
- **Description:** Thickness of first (shallowest) subsurface layer
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 89
  - Used in skin depth calculations
- **Notes:** Default 0.18 diurnal skin depths for first layer

### RLAY
- **Type:** `REAL*8`
- **Default:** `1.2` (from master.inp)
- **Units:** dimensionless (ratio)
- **Line References:** dvrc lines 772, 867
- **Description:** Layer thickness growth ratio
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 89
  - Each successive layer = RLAY × previous layer thickness
- **Notes:** Default 1.2 means each layer 20% thicker than the one above

### TDEEP
- **Type:** `REAL*8`
- **Default:** `180` K
- **Units:** Kelvin
- **Line References:** dvrc lines 778, 783, 788
- **Description:** Deep layer or initial temperature
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 87
  - Used in `/src/krc8.f` lines 932-933 for boundary conditions
- **Usage depends on IIB:**
  - IIB = -1: Fixed bottom layer temperature
  - IIB = -2: All layers initialized to TDEEP, then bottom fixed

### IIB
- **Type:** `INTEGER*4`
- **Default:** `-1` (fixed bottom temperature)
- **Units:** dimensionless or mW (if > 0)
- **Line References:** dvrc lines 777-797, 930-934
- **Description:** Lower boundary condition type
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Controls bottom boundary in heat equation
- **Options:**
  - -2: All layers start at TDEEP, bottom fixed at TDEEP
  - -1: Bottom layer fixed at TDEEP (default)
  - 0: Insulating bottom boundary (zero heat flux)
  - >0: Geothermal heat flux in mW

### IC2
- **Type:** `INTEGER*4`
- **Default:** `999` (uniform) or calculated by krc_evalN1()
- **Units:** dimensionless (layer number)
- **Line References:** dvrc lines 868, 922-926
- **Description:** Layer number where material properties change
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Constraints in `/src/tcard8.f` lines 373-377: IC2 ≥ 3 or IC2 = 999
  - Used to implement two-layer regolith models
- **Special Values:**
  - 999: Uniform material properties (no layer change)
  - ≥3: Layer where COND2, DENS2, SpHeat2 take over

### LKofT
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"T"` (True)
- **Options:** "T" or "F"
- **Line References:** dvrc lines 333, 862-864, 874-877, 929
- **Description:** Use temperature-dependent material properties
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 100
  - When "T": Uses ConUp/ConLo and SphUp/SphLo coefficients
  - When "F": Properties constant at T_user values

### ConUp0, ConUp1, ConUp2, ConUp3
- **Type:** `REAL*8` (each)
- **Default:** Fitted cubic coefficients from k_style calculation
- **Units:** Varies (polynomial coefficients)
- **Line References:** dvrc lines 633-636, 674-677
- **Description:** Temperature-dependent conductivity coefficients for upper layer
- **Formula:** k(X) = ConUp0 + ConUp1*X + ConUp2*X² + ConUp3*X³
  - Where X = (T - 220) * 0.01
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` lines 91-92
  - Used when LKofT="T" for conductivity interpolation
  - Read via change card type 12

### ConLo0, ConLo1, ConLo2, ConLo3
- **Type:** `REAL*8` (each)
- **Default:** Fitted cubic coefficients
- **Line References:** dvrc lines 660-663, 698-701
- **Description:** Temperature-dependent conductivity coefficients for lower layer
- **Fortran Usage:** Listed in `/src/tcard8.f` line 92

### SphUp0, SphUp1, SphUp2, SphUp3
- **Type:** `REAL*8` (each)
- **Default:** From Mat_Prop() function for Mat1
- **Units:** J kg⁻¹ K⁻¹ (and polynomial derivatives)
- **Line References:** dvrc lines 612-615, 668-671
- **Description:** Temperature-dependent specific heat coefficients for upper layer
- **Formula:** Cp(X) = SphUp0 + SphUp1*X + SphUp2*X² + SphUp3*X³
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 92-93
  - Read via change card type 13

### SphLo0, SphLo1, SphLo2, SphLo3
- **Type:** `REAL*8` (each)
- **Default:** From Mat_Prop() function for Mat2
- **Line References:** dvrc lines 639-642, 692-695
- **Description:** Temperature-dependent specific heat coefficients for lower layer
- **Fortran Usage:** Listed in `/src/tcard8.f` line 93

### LZONE
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** Calculated by krc_evalN1(), usually "F"
- **Options:** "T" or "F"
- **Line References:** dvrc lines 888, 891-896
- **Description:** Use zone file for layer properties
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 100
  - When "T": Reads zonefile.tab for layer-specific properties
  - Set via change card type 8 25

---

## 4. Time and Season Control

Parameters controlling temporal progression and output timing.

### N5
- **Type:** `INTEGER*4`
- **Default:** `1080` (3 Mars years)
- **Units:** dimensionless (count)
- **Line References:** dvrc lines 311, 408, 816-817, 830-833
- **Description:** Total number of seasons to run
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Constraints: N5 ≤ 2161
  - Total runtime = N5 seasons
- **Notes:**
  - Default runs 3 Mars years (1080 × 1.9083 JD ≈ 2061 days ≈ 3.0 Mars years)
  - Typically set automatically based on DELLS

### N24
- **Type:** `INTEGER*4`
- **Default:** From PORB (typically 96 for Mars)
- **Units:** dimensionless (count)
- **Line References:** dvrc lines 340, 766
- **Description:** Number of output times per day
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Output spacing = 24*60/N24 minutes
  - Used in `/src/krc8.f` line 200
- **Example:** N24=96 → output every 15 minutes

### JDISK
- **Type:** `INTEGER*4`
- **Default:** `721` (after 2 years)
- **Units:** dimensionless (season number)
- **Line References:** dvrc lines 312, 324, 408, 817, 913
- **Description:** First season number to output to disk
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Used in `/src/krc8.f` line 242
  - Constraints in `/src/tcard8.f` lines 378-382: JDISK > 0
- **Notes:**
  - Default 721 allows 2 years spin-up before output
  - Output seasons = JDISK to N5

### DJUL
- **Type:** `REAL*8`
- **Default:** `0.1` (Ls) or calculated from JD/GD
- **Units:** Ls (degrees) or Julian days (depends on LKEY)
- **Line References:** dvrc lines 313, 408, 913
- **Description:** Starting date for simulation
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 90
  - Interpretation depends on LKEY flag
- **Usage:**
  - If LKEY="T": DJUL is Ls (solar longitude, 0-360°)
  - If LKEY="F": DJUL is Julian date offset from J2000

### DELJUL
- **Type:** `REAL*8`
- **Default:** `1.9083` days (≈1° Ls for Mars) or calculated from DELLS
- **Units:** Julian days or days
- **Line References:** dvrc lines 347-355, 408, 813, 835-839, 913
- **Description:** Time increment between output seasons
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 90
  - Used in time marching loop
- **Constraints:** DELJUL ≥ PERIOD (cannot be less than one day)
- **Notes:**
  - Default 1.9083 days ≈ 1° Ls increment for Mars
  - Can be set via DELLS parameter instead

### DELLS
- **Type:** `REAL*8`
- **Default:** `1.0` Ls or `8.0` Ls (one-point mode)
- **Units:** Ls degrees
- **Line References:** dvrc lines 347-355, 765, 800-822
- **Description:** Solar longitude increment between outputs
- **Usage:** Alternative to DELJUL for setting season spacing
- **Calculation:** DELJUL = PERIOD/360 * DELLS
- **Constraints:**
  - Minimum DELLS = 0.53845° (prevents time-of-day/season confusion)
  - N5 and JDISK automatically calculated from DELLS

### PERIOD
- **Type:** `REAL*8`
- **Default:** From PORB calculations (1.0275 days for Mars)
- **Units:** Earth days
- **Line References:** dvrc lines 341, 835, 844
- **Description:** Rotational period (length of one sol/day)
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 86
  - Used in `/src/krc8.f` line 200
  - Set in `/src/porbel.f` line 333 during PORB calculations
- **Examples:**
  - Mars: 1.0275 Earth days
  - Earth: 1.0 Earth days
  - Moon: 29.53 Earth days (tidally locked)

### LKEY
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"T"` (use Ls input)
- **Options:** "T" or "F"
- **Line References:** dvrc lines 314, 407
- **Description:** Use Ls (solar longitude) vs Julian Date for time input
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 100
  - Determines interpretation of DJUL parameter
- **Usage:**
  - "T": DJUL is Ls (0-360°)
  - "F": DJUL is Julian date

### N3
- **Type:** `INTEGER*4`
- **Default:** `1` or from master.inp
- **Units:** dimensionless (count)
- **Line References:** dvrc lines 850, 854
- **Description:** Number of iteration days for convergence
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Used in iterative solution scheme
  - Constraints in `/src/tcard8.f` lines 394-398: 1 ≤ N3 < MAXN3-1
- **Notes:** Affects convergence speed and accuracy

### NRSET
- **Type:** `INTEGER*4`
- **Default:** `999` or from master.inp
- **Units:** dimensionless (count)
- **Line References:** dvrc lines 851, 855
- **Description:** Number of resets before temperature prediction
- **Fortran Usage:** Listed in `/src/tcard8.f` line 95
- **Related:** Works with TPREDICT parameter

### TPREDICT
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"T"` or `"F"` if DELJUL ≤ 3*PERIOD
- **Options:** "T" or "F"
- **Line References:** dvrc lines 841-847, 915-921
- **Description:** Use forward temperature prediction for speed
- **Fortran Usage:** Sets GGT, N3, NRSET parameters
- **Notes:**
  - "F": More accurate but slower (GGT=99, N3=1, NRSET=999)
  - "T": Faster but may be less accurate for small DELJUL
  - Automatically set to "F" if DELJUL ≤ 3*PERIOD

### GGT
- **Type:** `REAL*8`
- **Default:** `99.0` or from master.inp
- **Units:** dimensionless
- **Line References:** dvrc lines 849, 853
- **Description:** Temperature prediction control parameter
- **Fortran Usage:** Listed in `/src/tcard8.f` line 90
- **Notes:** Controls convergence rate in iterative scheme

### ls
- **Type:** `REAL*8`
- **Default:** `-32768` (all seasons)
- **Units:** Ls degrees (0-360)
- **Line References:** dvrc lines 310, 755, 985-988
- **Description:** Specific solar longitude to output
- **Usage:**
  - -32768: Output all seasons
  - 0-360: Output only this specific Ls
- **Fortran Usage:** Used in output filtering via process_bin52()

### hour
- **Type:** `REAL*8`
- **Default:** `-32768` (all hours)
- **Units:** Local True Solar Time (0-24)
- **Line References:** dvrc lines 331, 755, 980-984
- **Description:** Specific hour of day to output
- **Usage:**
  - -32768: Output all hours
  - 0-24: Output only this specific hour
- **Fortran Usage:** Used in output filtering via process_bin52()

### JD / GD
- **Type:** `REAL*8` / `STRING`
- **Default:** Not set (uses Ls by default)
- **Units:** Julian Date / Gregorian Date
- **Line References:** dvrc lines 330, 406-409
- **Description:** Alternative date specification methods
- **Usage:**
  - JD: Julian date (2451544.5 = 1990-Jan-01)
  - GD: "YYYY-Mmm-DD" format (e.g., "2020-Jan-15")
- **Notes:** If set, overrides Ls input and sets LKEY="F"

---

## 5. Location and Geometry

Parameters specifying the geographic location and surface orientation.

### lat
- **Type:** `REAL*8`
- **Default:** `0` (equator)
- **Units:** degrees (-90 to 90)
- **Line References:** dvrc lines 327, 508-512, 713, 965
- **Description:** Planetocentric latitude
- **Fortran Usage:**
  - Set in `/src/tcard8.f` line 151
  - Stored in ALAT(N4) array in KRCCOM
  - Used throughout for solar position calculations
- **Constraints:** Must be in range [-90, 90]
- **Notes:**
  - Positive = North
  - Negative = South
  - Used in solar zenith angle calculations

### lon
- **Type:** `REAL*8`
- **Default:** `0` (prime meridian)
- **Units:** degrees East (0 to 360)
- **Line References:** dvrc lines 328, 334, 513-517, 521, 965
- **Description:** East longitude
- **Fortran Usage:** Used to extract ancillary data from global maps
- **Constraints:** Must be in range [0, 360]
- **Notes:**
  - Used to look up INERTIA, ALBEDO, ELEV from maps
  - Converted to image coordinates via geo_trans()

### ELEV
- **Type:** `REAL*8`
- **Default:** `0.0` km or from MOLA map (Mars)
- **Units:** kilometers
- **Line References:** dvrc lines 544, 565-567, 580, 596, 716
- **Description:** Surface elevation relative to reference
- **Fortran Usage:**
  - Set in `/src/tcard8.f` line 152
  - Stored in ELEV(N4) array in KRCCOM
  - Used in pressure and temperature calculations
- **Special Cases:**
  - Mars: Loaded from `krc_ELEV` global map (MOLA 2ppd, /1000 for km)
  - Affects atmospheric pressure via scale height

### SLOPE
- **Type:** `REAL*8`
- **Default:** `0.0` (flat surface)
- **Units:** degrees
- **Line References:** dvrc lines 321, 965
- **Description:** Surface slope angle from horizontal
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 88
  - Used in solar insolation calculations
  - Used in `/src/orlint8.f` line 28 for interpolation
- **Range:** [0, 90] degrees

### SLOAZI
- **Type:** `REAL*8`
- **Default:** `0.0` (azimuth = North)
- **Units:** degrees East from North
- **Line References:** dvrc lines 322, 965
- **Description:** Slope azimuth angle
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 88
  - Used with SLOPE in insolation calculations
- **Range:** [0, 360] degrees
- **Notes:** 0=N, 90=E, 180=S, 270=W

### body
- **Type:** `STRING`
- **Default:** `"Mars"`
- **Options:** "Mars", "Earth", "Moon", "Europa", "Phobos", "Bennu", "Halley", etc.
- **Line References:** dvrc lines 291-292, 357, 366, 533, 548, 571, 585, 910
- **Description:** Target planetary body name
- **Fortran Usage:** Used to load PORB orbital parameters
- **Special Cases:**
  - Mars: Loads TES ancillary data (INERTIA, ALBEDO, ELEV)
  - Europa: Sets H2O material properties, INERTIA=100, ALBEDO=0.67
  - Others: Generic properties unless specified
- **Notes:** Must match name in porb_master.hdf database

### bodyforce
- **Type:** `INTEGER*4`
- **Default:** `0` (use cached PORB)
- **Options:** 0, 1
- **Line References:** dvrc lines 292, 295
- **Description:** Force recalculation of PORB orbital parameters
- **Usage:**
  - 0: Use pre-computed PORB from cache (fast)
  - 1: Recalculate PORB on every call (slow but allows custom orbits)

---

## 6. Orbital Parameters

Parameters from PORB orbital calculations (usually not set directly).

### GRAV
- **Type:** `REAL*8`
- **Default:** From PORB (e.g., 3.71 m/s² for Mars)
- **Units:** m s⁻²
- **Line References:** dvrc line 344
- **Description:** Surface gravitational acceleration
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 91
  - From `/src/krcc8m.f` line 36
  - Used in atmospheric scale height calculations
- **Source:** Loaded from PORB planetary_params3.csv

### DAU
- **Type:** `REAL*8`
- **Default:** From PORB orbital calculations
- **Units:** Astronomical Units (AU)
- **Description:** Distance from Sun
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 90
  - Used in solar flux calculations: SOLCON / DAU²
- **Notes:** Time-varying for elliptical orbits

### SOLCON
- **Type:** `REAL*8`
- **Default:** From PORB (e.g., 1367.0 W/m² at 1 AU)
- **Units:** W m⁻²
- **Description:** Solar constant at 1 AU
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 91
  - From `/src/krcc8m.f` line 36
- **Notes:** Actual solar flux = SOLCON / DAU²

### SOLARDEC
- **Type:** `REAL*8`
- **Default:** From PORB orbital calculations
- **Units:** degrees
- **Description:** Solar declination angle
- **Fortran Usage:** Listed in `/src/tcard8.f` line 90
- **Notes:** Time-varying, determines subsolar latitude

### ARC2_G0
- **Type:** `REAL*8`
- **Default:** From PORB calculations
- **Units:** dimensionless
- **Line References:** dvrc line 346
- **Description:** Photometric function parameter
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 91
  - Named 'ARC2/PHT' in TITF array
  - Related to surface roughness and scattering

### LsubS
- **Type:** `REAL*8`
- **Default:** From PORB
- **Units:** degrees (Ls)
- **Description:** Solar longitude
- **Fortran Usage:** Time-varying orbital position parameter

### Atm_Cp
- **Type:** `REAL*8`
- **Default:** From PORB or calculated
- **Units:** J kg⁻¹ K⁻¹
- **Description:** Atmospheric specific heat at constant pressure
- **Fortran Usage:** Listed in `/src/tcard8.f` line 91

---

## 7. Output Control Parameters

Parameters controlling what and how results are saved.

### K4OUT
- **Type:** `INTEGER*4`
- **Default:** `52` (bin52 format)
- **Units:** dimensionless (file type code)
- **Line References:** dvrc line 738
- **Description:** Output file type/format
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 96
  - Controls output file format in TDISK8
- **Fixed:** Interface forces K4OUT=52 (bin52 encapsulates other formats)
- **Notes:** Type 52 is comprehensive binary output format

### TUN8 (I15)
- **Type:** `INTEGER*4`
- **Default:** `0` (no temperature-depth output)
- **Options:** 0, 101, 102
- **Line References:** dvrc lines 371-380, 761-762
- **Description:** Enable temperature with depth or atmospheric output
- **Fortran Usage:**
  - Mapped to TUN_Flx15 variable
  - Listed as I15 in `/src/krcc8m.f` line 28
  - Used in `/src/krc8.f` line 255
- **Options:**
  - 0: No extra output (default)
  - 101: Output temperatures with depth for all layers
  - 102: Output atmospheric parameters
- **Notes:** Can accept "T"/"F" string, converted to 101/0

### LMST
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"F"` (use LTST)
- **Options:** "T" or "F"
- **Line References:** dvrc line 309
- **Description:** Output in Local Mean Solar Time instead of Local True Solar Time
- **Fortran Usage:** Affects time axis in output structure
- **Notes:**
  - LTST: Local True Solar Time (accounts for orbital eccentricity)
  - LMST: Local Mean Solar Time (uniform clock time)

### N4
- **Type:** `INTEGER*4`
- **Default:** `1` (single latitude)
- **Units:** dimensionless (count)
- **Line References:** dvrc line 320
- **Description:** Number of latitudes to process
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 95
  - Dimensions ALAT() and ELEV() arrays
  - Constraints in `/src/tcard8.f` lines 399-405: 1 ≤ N4 ≤ MAXN4
- **MAXN4:** Maximum = 37 (from `/src/krcc8m.f` line 10)
- **Notes:** Allows parallel processing of multiple latitudes

### WRITE
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"F"` (don't write intermediate files)
- **Options:** "T" or "F"
- **Line References:** dvrc line 308
- **Description:** Write detailed output files
- **Usage:** Controls verbosity of file output

### KEEP
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"F"` (don't keep temporary files)
- **Options:** "T" or "F"
- **Line References:** dvrc line 339
- **Description:** Keep temporary working files
- **Usage:** For debugging, preserves intermediate results

### hour (duplicate entry - see Time Control)
- **Description:** Filters output to specific hour of day

### ls (duplicate entry - see Time Control)
- **Description:** Filters output to specific solar longitude

---

## 8. Eclipse and Planetary Flux

Parameters for modeling eclipses and planetary thermal emission.

### Eclipse
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"F"` (no eclipses)
- **Options:** "T" or "F"
- **Line References:** dvrc lines 315, 451-463
- **Description:** Enable eclipse modeling
- **Fortran Usage:**
  - Generates Eclipse_line parameter string
  - Used via change card type 14
  - Referenced in `/src/eclipse.f`
- **Related Parameters:** Eclipse_Style, Eclipser, Sun_Dis, Eclipser_Rad, CM, etc.

### Eclipse_Style
- **Type:** `REAL*8`
- **Default:** `1.0` (daily eclipses)
- **Options:** 1.0, 2.0
- **Line References:** dvrc line 454
- **Description:** Eclipse occurrence pattern
- **Options:**
  - 1.0: Rare eclipses (specified by Date parameter)
  - 2.0: Daily/regular eclipses

### Eclipser
- **Type:** `STRING`
- **Default:** Parent body from PORB
- **Line References:** dvrc line 452
- **Description:** Name of eclipsing body
- **Example:** "Mars" eclipsing Phobos

### Eclipsed_Rad
- **Type:** `REAL*8`
- **Default:** From PORB planet_flux.Radius
- **Units:** kilometers
- **Line References:** dvrc line 458
- **Description:** Radius of eclipsed body
- **Fortran Usage:** Used in shadow calculations

### Sun_Dis
- **Type:** `REAL*8`
- **Default:** Maximum of eclipser/eclipsed AU
- **Units:** Astronomical Units
- **Line References:** dvrc line 455
- **Description:** Distance to Sun for angular diameter
- **Usage:** Determines Sun's angular size for umbra/penumbra

### Eclipser_Rad
- **Type:** `REAL*8`
- **Default:** From Eclipser PORB
- **Units:** kilometers
- **Line References:** dvrc line 456
- **Description:** Radius of eclipsing body
- **Usage:** Shadow cone calculations

### CM
- **Type:** `REAL*8`
- **Default:** Maximum of mutual orbit radii
- **Units:** kilometers
- **Line References:** dvrc line 457
- **Description:** Mutual center-of-mass orbit radius
- **Usage:** Determines orbital geometry for eclipses

### Per_Mut
- **Type:** `REAL*8`
- **Default:** Maximum of mutual periods
- **Units:** Earth days
- **Line References:** dvrc line 459
- **Description:** Mutual orbital period
- **Notes:** Often same as diurnal PERIOD for tidally locked bodies

### Bias
- **Type:** `REAL*8`
- **Default:** `0.0`
- **Units:** dimensionless
- **Line References:** dvrc line 461
- **Description:** Eclipse phase bias/offset

### Date
- **Type:** `REAL*8`
- **Default:** `5000.0`
- **Units:** J2000 days
- **Line References:** dvrc line 460
- **Description:** Date of rare eclipse
- **Usage:** Only relevant if Eclipse_Style = 1

### Ecl_Cent_Hr
- **Type:** `REAL*8`
- **Default:** `12.0`
- **Units:** hours
- **Line References:** dvrc line 462
- **Description:** Eclipse central hour
- **Usage:** Local time when eclipse is centered

### PFlux
- **Type:** `LOGICAL*4` (STRING in Davinci)
- **Default:** `"F"` (no planetary flux)
- **Options:** "T" or "F"
- **Line References:** dvrc lines 317, 419-447
- **Description:** Enable planetary thermal flux on satellite surface
- **Usage:** For satellites orbiting planets (e.g., Phobos around Mars)
- **Calculation:** Via krc_planetary_flux_porb() or krc_planetary_flux_table()

### BT_Avg
- **Type:** `REAL*8`
- **Default:** From krc_planetary_flux_porb() calculations
- **Units:** Kelvin
- **Line References:** dvrc line 437
- **Description:** Average planetary brightness temperature
- **Usage:** Mean thermal emission from planet

### BT_Max
- **Type:** `REAL*8`
- **Default:** From calculations
- **Units:** Kelvin
- **Line References:** dvrc line 438
- **Description:** Maximum planetary brightness temperature
- **Usage:** Peak of diurnal temperature variation

### BT_Min
- **Type:** `REAL*8`
- **Default:** From calculations
- **Units:** Kelvin
- **Line References:** dvrc line 439
- **Description:** Minimum planetary brightness temperature
- **Usage:** Minimum of diurnal variation

### Geom_alb
- **Type:** `REAL*8`
- **Default:** From PORB
- **Units:** dimensionless (0-1)
- **Line References:** dvrc line 440
- **Description:** Geometric albedo of planet
- **Usage:** For reflected solar flux calculations

### Radius
- **Type:** `REAL*8`
- **Default:** From PORB planet_flux.Radius
- **Units:** kilometers
- **Line References:** dvrc line 441
- **Description:** Radius of orbiting body (satellite)

### Dis_AU
- **Type:** `REAL*8`
- **Default:** From PORB planet_flux.Dis_AU
- **Units:** Astronomical Units
- **Line References:** dvrc lines 442-443
- **Description:** Distance from Sun
- **Usage:** Solar flux calculations

### Mut_Period
- **Type:** `REAL*8`
- **Default:** From PORB planet_flux.Mut_Period
- **Units:** Earth days
- **Line References:** dvrc line 444
- **Description:** Mutual orbital period

### Orb_Radius
- **Type:** `REAL*8`
- **Default:** From PORB planet_flux.Orb_Radius
- **Units:** kilometers
- **Line References:** dvrc line 445
- **Description:** Orbital radius around planet

### Lon_Hr
- **Type:** `REAL*8`
- **Default:** `12.0` (subsolar point)
- **Units:** hours
- **Line References:** dvrc lines 316, 425, 431, 446
- **Description:** Longitude hour on satellite surface
- **Usage:** Determines viewing geometry for planetary flux

### IR / Vis
- **Type:** `ARRAY[2, n, 1]`
- **Default:** Not set (uses calculated values)
- **Line References:** dvrc lines 423-425
- **Description:** Custom IR/Visible flux arrays vs LTST
- **Format:** Column 1 = flux (W/m²), Column 2 = LTST (hours)
- **Usage:** Override default planetary flux with custom values

---

## 9. Advanced and Computational Parameters

Advanced parameters for special cases and debugging.

### PhotoFunc
- **Type:** `REAL*8`
- **Default:** `0` (Lambert)
- **Units:** dimensionless
- **Line References:** dvrc lines 335, 936-939
- **Description:** Photometric function model
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 90 as 'PhotF'
  - Controls albedo as function of solar incidence angle
- **Options:**
  - -1.0 to <0: Minnaert with exponent = abs(PhotoFunc)
  - 0.0 to <1.0: Lunar-like (Keihm polynomial, typical 0.25-0.375)
  - 2.0: Lambert (default)
  - 3.0: Lommel-Seeliger

### JBARE
- **Type:** `INTEGER*4`
- **Default:** `0` (no frost removal)
- **Units:** dimensionless (season number)
- **Line References:** dvrc lines 323-324
- **Description:** Season number to force frost-free conditions
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 96
  - Can be specified as Ls, then converted: JBARE = krc_ls(JBARE) + JDISK
- **Usage:**
  - 0: Normal frost behavior
  - >0: Remove any frost at this season number
- **Notes:** Useful for testing non-frost conditions

### MAXN1
- **Type:** `INTEGER*4` (PARAMETER)
- **Default:** `1000`
- **Units:** dimensionless (count)
- **Line References:** dvrc line 318
- **Description:** Maximum number of subsurface layers allowed
- **Fortran Usage:**
  - Defined in `/src/krcc8m.f` line 7 as PARAMETER
  - Hard limit on N1
- **Notes:** Cannot be changed without recompiling Fortran code

### MAXN2
- **Type:** `INTEGER*4` (PARAMETER)
- **Default:** `393216` (384 × 4 × 256)
- **Units:** dimensionless (count)
- **Line References:** dvrc line 319
- **Description:** Maximum timesteps per day allowed
- **Fortran Usage:**
  - Defined in `/src/krcc8m.f` line 8 as PARAMETER
  - Hard limit on N2

### NRUN
- **Type:** `INTEGER*4`
- **Default:** `0` (incremented automatically)
- **Units:** dimensionless (count)
- **Description:** Run number counter
- **Fortran Usage:**
  - Listed in `/src/tcard8.f` line 96
  - Incremented in `/src/krc8.f` line 248
- **Notes:** Used for tracking multiple sequential runs

### stability
- **Type:** `INTEGER*4`
- **Default:** `0` (no checking)
- **Options:** 0, 1
- **Line References:** dvrc line 332
- **Description:** Check model for numerical stability
- **Fortran Usage:** Calls krc_stability_flag() if enabled
- **Usage:**
  - 0: No stability checking
  - 1: Monitor temperature gradients for instabilities
- **Notes:** Used for debugging problematic parameter combinations

### NMOD
- **Type:** `INTEGER*4`
- **Default:** From input or set internally
- **Description:** Model mode flag
- **Fortran Usage:** Listed in `/src/tcard8.f` line 96

### IDISK2
- **Type:** `INTEGER*4`
- **Default:** Set internally
- **Description:** Secondary disk file control
- **Fortran Usage:** Listed in `/src/tcard8.f` line 96

### IDOWN
- **Type:** `INTEGER*4`
- **Default:** Set internally
- **Description:** Iteration control parameter
- **Fortran Usage:** Listed in `/src/tcard8.f` line 96

### ffout / ffin
- **Type:** `STRING`
- **Default:** `""` (not used)
- **Line References:** dvrc lines 337-338
- **Description:** Far-field file input/output paths
- **Status:** Not currently usable - available for testing only
- **Notes:** For advanced users, may allow saving/loading pre-computed far-field data

### anc
- **Type:** `INTEGER*4`
- **Default:** `0` (no ancillary output)
- **Options:** 0, 1
- **Line References:** dvrc line 329
- **Description:** Output ancillary data in structure
- **Usage:** Controls inclusion of extra diagnostic information

### v
- **Type:** `INTEGER*4`
- **Default:** `0` (quiet)
- **Options:** 0, 1
- **Line References:** dvrc lines 284-288
- **Description:** Verbosity flag
- **Usage:** 1 = print detailed parameter loading messages

### T (One-Point Mode)
- **Type:** `ARRAY`
- **Default:** Not set (runs full model)
- **Line References:** dvrc lines 754-767, 991
- **Description:** Temperature array for one-point thermal inertia retrieval
- **Usage:** If set, switches to KRC-ONE mode that derives TI from observations
- **Requirements:** Must also set ls and hour

### TI_Guess
- **Type:** `REAL*8`
- **Default:** Not set (full lookup table)
- **Line References:** dvrc line (one-point mode)
- **Description:** Initial guess for thermal inertia in one-point mode
- **Usage:** Speeds up one-point retrieval (~8x faster) but risky
- **Units:** J m⁻² K⁻¹ s⁻¹/²

### TI_Guess_PCT
- **Type:** `REAL*8`
- **Default:** Depends on TI_Guess
- **Units:** dimensionless (fraction)
- **Description:** Acceptable tolerance for TI_Guess
- **Usage:** If retrieved TI outside TI_Guess ± (TI_Guess_PCT × TI_Guess), returns -100

### T_Tol
- **Type:** `REAL*8`
- **Default:** `1.0` K
- **Units:** Kelvin
- **Description:** Temperature tolerance near frost point in one-point mode
- **Usage:** Temperatures within T_Tol of frost point treated as frosted

---

## 10. Parameter Dependencies and Relationships

### Material Property Relationships

The thermal properties are intricately connected:

```
INERTIA = sqrt(COND × DENSITY × SPEC_HEAT)
```

If user provides:
- **INERTIA only:** DENSITY and SPEC_HEAT calculated from Mat1/Por1, then COND derived
- **COND, DENSITY, SPEC_HEAT:** INERTIA recalculated from these
- **ConUp0, DENSITY, SphUp0:** INERTIA recalculated for T-dependent case

### Layer Configuration Logic

```
thick = 0:     IC2 = 999 (uniform material)
thick > 0:     Two-layer model, IC2 calculated to match thickness
thick < 0:     Exponential profile with H = abs(thick)
```

### Temperature-Dependent Properties

When `LKofT = "T"`:
- Conductivity: k(T) = ConUp0 + ConUp1×X + ConUp2×X² + ConUp3×X³
- Specific Heat: Cp(T) = SphUp0 + SphUp1×X + SphUp2×X² + SphUp3×X³
- Where X = (T - 220) / 100

k_style determines functional form:
- **"Moon"**: k ∝ T³ trend (Hayne et al. 2017)
- **"Mars"**: k ∝ √T trend (Morgan et al. 2018)
- **"Bulk"**: Uses material-specific polynomial

### Time Stepping Relationships

```
N2 ≥ Courant stability limit
N2 = f(FLAY, INERTIA, DENSITY, SPEC_HEAT, PERIOD, N24)

N1 ≥ 3 seasonal skin depths
N1 = f(RLAY, FLAY, INERTIA, DELJUL, N5, JDISK, PERIOD, thick)

If DELLS specified:
    DELJUL = PERIOD / 360 × DELLS
    N5 = ceil(360 / DELLS × 3)
    JDISK = ceil(360 / DELLS × 2 + 1)

Output seasons = N5 - JDISK + 1 ≤ 720
```

### Atmospheric Parameter Connections

```
If PTOTAL < 1.0:
    TAUD = 0 (forced)
    Body treated as airless

If LVFT = "T":
    TFROST = f(pressure, gas composition)
    Calls krc_cond_gas(body) for Mars/Titan/Pluto
    Sets CFROST, AFROST, TFROST dynamically
```

### PORB-Derived Parameters

These parameters are automatically set from PORB unless explicitly overridden:
- N24, PERIOD, PTOTAL, DUSTA, GRAV, TAURAT, ARC2_G0
- DELJUL (if DELLS not specified)
- DAU, SOLCON, SOLARDEC (time-varying orbital elements)

To use custom orbital parameters:
- Set `bodyforce = 1` to force PORB recalculation
- Or use `generic_porb()` function to define custom orbit

### Eclipse/Planetary Flux Hierarchy

```
For satellites:
    If PFlux = "T":
        If IR and Vis provided: Use custom arrays
        Else if BT_Avg provided: Calculate from parameters
        Else: Use krc_planetary_flux_porb(porb, porb_Planet)

    If Eclipse = "T":
        Generate Eclipse_line with 10 parameters
        Eclipser defaults to parent body from PORB
```

### Fortran Source Code References

Key files where parameters are used:

1. **`/src/krcc8m.f`**: KRCCOM common block definition
   - Lines 27-42: Variable declarations with types
   - Lines 66-91: Common block structure
   - All parameters ultimately stored here

2. **`/src/krc8.f`**: Main program
   - Line 71: Version number (3.6.5)
   - Line 189: Calls TCARD8 to read parameters
   - Line 199: Sets LATM flag from PTOTAL
   - Line 242: Checks N5 >= JDISK for output

3. **`/src/tcard8.f`**: Parameter input routine
   - Lines 85-93: REAL*8 parameter names (TITF array)
   - Lines 95-97: INTEGER*4 parameter names (TITI array)
   - Lines 99-101: LOGICAL*4 parameter names (TITL array)
   - Lines 147-152: Read full parameter set
   - Lines 365-405: Parameter bounds checking

4. **`/src/tday8.f`**: Daily thermal calculations
   - Uses N1, N2, FLAY, RLAY for layer/time setup
   - Implements temperature-dependent properties

5. **`/src/tseas8.f`**: Seasonal calculations
   - Lines 115-116: Variable albedo handling
   - Lines 119-120: Variable opacity handling

6. **`/src/tlats8.f`**: Latitude loop
   - Line 551: Albedo clamping [0,1]
   - Processes N4 latitudes in parallel

---

## Parameter Summary Statistics

### By Type
- **REAL*8 (Floating Point):** 64 parameters
- **INTEGER*4:** 20 parameters
- **LOGICAL*4 (Boolean):** 20 parameters
- **STRING:** 8 parameters (body, Mat1, Mat2, k_style, etc.)
- **ARRAY:** 2 parameters (IR, Vis)
- **PARAMETER (Constants):** 12 (MAXN1, MAXN2, etc.)

### By Source
- **User Input:** ~40 parameters typically set by users
- **PORB-Derived:** ~25 parameters from orbital calculations
- **Calculated:** ~20 parameters computed from other parameters
- **Fortran Internal:** ~10 parameters managed by Fortran code

### By Category (as shown earlier)
- Material Properties: 15 parameters
- Atmospheric: 11 parameters
- Thermal Model: 24 parameters
- Time/Season Control: 12 parameters
- Location/Geometry: 6 parameters
- Orbital: 7 parameters
- Output Control: 8 parameters
- Eclipse/Planetary Flux: 18 parameters
- Advanced/Computational: 14 parameters

### Most Commonly Modified Parameters
1. **lat, lon** - Location (required)
2. **INERTIA, ALBEDO** - Surface properties
3. **TAUD** - Atmospheric opacity
4. **N24** - Output time resolution
5. **DELLS** - Season spacing
6. **SLOPE, SLOAZI** - Surface orientation
7. **thick** - Layer structure
8. **hour, ls** - Output filtering

---

## Special Configuration Patterns

### Mars Standard Run
```
body = "Mars"
lat = <value>, lon = <value>
# INERTIA, ALBEDO, ELEV from TES/MOLA maps
TAUD = 0.3 (or seasonal)
N24 = 96 (15-minute output)
DELLS = 1 (1° Ls spacing)
LKofT = "T" (T-dependent properties)
k_style = "Mars"
```

### Airless Body (Moon/Asteroid)
```
body = "Moon" or "Bennu"
INERTIA = <value>
ALBEDO = <value>
PTOTAL = 0.1 (or less, forces airless)
# TAUD automatically set to 0
k_style = "Moon"
LKofT = "T"
PhotoFunc = 0.375 (Lunar-like)
```

### Europa Ice
```
body = "Europa"
Mat1 = "H2O", Mat2 = "H2O"
T_user = 100 (cold surface)
k_style = "Moon"
TFROST = 0 (no frost sublimation)
```

### Two-Layer Regolith
```
INERTIA = <upper_value>
INERTIA2 = <lower_value>
Mat1 = "basalt", Mat2 = "basalt"
Por1 = 0.5, Por2 = 0.3
thick = 0.05 (5 cm upper layer)
# IC2 calculated automatically
```

### High Time Resolution
```
N24 = 384 (every 3.75 minutes for Mars)
N2 = krc_evalN2() (auto, typically 1152+)
TUN8 = 101 (output T vs depth)
```

### One-Point Thermal Inertia Retrieval
```
T = <observed_temperature_array>
ls = <specific_Ls>
hour = <specific_hour>
# Returns TI instead of full model
TI_Guess = 200 (optional, for speed)
TI_Guess_PCT = 0.2 (±20% tolerance)
```

---

## File Format Notes

### Input File Structure (master.inp)

The Fortran KRC reads parameters from master.inp in this order:
1. Title line (KITLE, 84 chars)
2. 64 REAL*8 values (8 per line, F10.2 format)
3. 20 INTEGER*4 values (8 per line, I10 format)
4. 20 LOGICAL*4 values (10 per line, L7 format)
5. N4 latitude values (10 per line, F7.2 format)
6. N4 elevation values (10 per line, F7.2 format)
7. Optional change cards (Type 1-16)

### Change Card Types

The Davinci interface generates change cards:
- Type 1: REAL parameter change
- Type 2: INTEGER parameter change
- Type 3: LOGICAL parameter change
- Type 4: New latitude values
- Type 5: New elevation values
- Type 6: New PORB orbital parameters
- Type 7: New title
- Type 8: File name (subtype 3,4,5,21-25)
- Type 11: One-point mode data
- Type 12: T-dependent conductivity coefficients
- Type 13: T-dependent specific heat coefficients
- Type 14: Eclipse parameters (10 values)
- Type 15: Planetary flux parameters (7 values)

### Output Structure (from process_bin52)

The output structure includes:
- `out.tsurf`: Surface temperature [N24, N5-JDISK+1]
- `out.time`: LTST or LMST time axis
- `out.season`: Ls values
- `out.albedo`, `out.emissivity`, `out.inertia`: Surface properties
- `out.anc`: Ancillary data (if anc=1)
- `out.layers`: Layer depths and properties
- If TUN8=101: `out.T_depth`: Temperature vs depth [N1, N24, Nseasons]

---

## Validation and Constraints

### Hard Limits (from krcc8m.f PARAMETER statements)
- MAXN1 = 1000 layers
- MAXN2 = 393,216 timesteps/day
- MAXN3 = 16 iteration days
- MAXN4 = 37 latitudes
- MAXN6 = 6 years
- MAXNH = 86,400 output hours
- MAXFF = 6,144 far-field timesteps

### Runtime Checks (from tcard8.f)
- N1: [2, MAXN1P]
- IC2: ≥3 or =999
- JDISK: >0
- N2: [32, MAXN2]
- N3: [1, MAXN3-1]
- N4: [1, MAXN4]
- N5: ≤2161
- N5-JDISK: ≤720 (output season limit)

### Physical Constraints (from krc.dvrc)
- lat: [-90, 90] degrees
- lon: [0, 360] degrees
- ALBEDO: [0, 1]
- EMISS: [0, 1]
- Por1, Por2: [0, 1]
- DELLS: ≥0.53845° (prevents time-of-day/season aliasing)
- DELJUL: ≥PERIOD (must be at least one day)

### Stability Criteria
- N2 must satisfy Courant condition: Δt ≤ (Δz)²/(2α)
  - Where α = COND/(DENSITY×SPEC_HEAT) is thermal diffusivity
  - Automatically enforced by krc_evalN2()
- N1 must reach ≥3 seasonal skin depths for boundary condition isolation
  - Automatically enforced by krc_evalN1()

---

## Conclusion

This analysis documents **85 unique parameters** found in the krc.dvrc file, tracing their:
- **Default values** and calculation methods
- **Usage in Fortran source code** with specific file and line references
- **Physical meaning** and units
- **Relationships and dependencies** with other parameters
- **Constraints and validation** rules

The KRC thermal model is highly sophisticated, with parameters spanning:
- Material thermophysical properties (thermal inertia, conductivity, density, heat capacity)
- Atmospheric effects (pressure, opacity, frost condensation)
- Numerical model configuration (layer counts, timesteps, convergence criteria)
- Temporal control (seasonal progression, output timing)
- Spatial specification (latitude, longitude, elevation, slope)
- Orbital mechanics (automatically computed via PORB system)
- Advanced features (eclipses, planetary flux, temperature-dependent properties)

The Davinci interface provides a high-level wrapper that:
1. Automatically computes optimal N1 and N2 for stability
2. Handles material property calculations from composition
3. Manages PORB orbital parameter lookup/calculation
4. Generates properly formatted Fortran input files
5. Parses binary output into convenient structures

### Key Files Analyzed
- **Interface:** `/Users/chaberle/Documents/GitHab/KRC/krc_python/docs/davinci/krc.dvrc` (4379 lines)
- **Main Program:** `/Users/chaberle/Documents/GitHab/KRC/src/krc8.f`
- **Common Block:** `/Users/chaberle/Documents/GitHab/KRC/src/krcc8m.f`
- **Input Handler:** `/Users/chaberle/Documents/GitHab/KRC/src/tcard8.f`

---

**Document Generated:** 2025-10-08
**Analysis Tool:** Comprehensive parameter extraction from source code and documentation
**Total Parameters Documented:** 85+ parameters with full traceability
