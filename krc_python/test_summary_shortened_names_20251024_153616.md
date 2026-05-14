# PyKRC Integration Test Summary

**Generated:** 2025-10-24 15:42:22

**Total Tests:** 72
**Passed:** 56 ✓
**Failed:** 16 ✗
**Success Rate:** 77.8%

## Test Results Summary

| # | Test Name | Status | Input Files | Output Temps | Max Temp Diff (K) |
|---|-----------|--------|-------------|--------------|-------------------|
| 1 | PORB defaults (Mars) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 2 | User defaults (Europa) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 3 | Basalt INERTIA=250 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 4 | k_style=Moon | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 5 | k_style=Bulk | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 6 | DELLS blocks DELJUL | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 7 | User overrides PORB | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 8 | PTOTAL<1 forces TAUD=0 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 9 | TPREDICT=False stability | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 10 | Two-layer regolith | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 11 | Eclipse Style 1.0 (daily) | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 12 | PFlux (Type 15) | ✗ FAIL | ✗ Different | ✗ Different | 1.2803 |
| 13 | Mars Ls + INERTIA | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 14 | Phobos default | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 15 | High INERTIA + low ALBEDO | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 16 | Gregorian Date (GD) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0001 |
| 17 | Julian Date (JD) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0001 |
| 18 | Delta Julian Date (DJUL) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 19 | Ls (solar longitude) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 20 | JD to Ls conversion | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 21 | GD to Ls conversion | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 22 | Europa lbound=0 (insulating) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 23 | Europa lbound=-1//98 (fixed temp) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 24 | Europa lbound=-2//100 (all layers) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 25 | Europa lbound=0.5 (low heat flow) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 26 | Europa lbound=50 (high heat flow) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 27 | Mars lbound with two-layer | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 28 | Mars T→TI (LKofT=F, TI~13) | ✓ PASS | ✓ Identical | N/A | N/A |
| 29 | Mars T→TI (LKofT=F, TI~100) | ✓ PASS | ✓ Identical | N/A | N/A |
| 30 | Mars T→TI (LKofT=T, TI~100) | ✓ PASS | ✓ Identical | N/A | N/A |
| 31 | Europa T→TI (LKofT=F) | ✗ FAIL | ✗ Different | N/A | N/A |
| 32 | Europa T→TI (LKofT=T) | ✗ FAIL | ✗ Different | N/A | N/A |
| 33 | Phobos T→TI (LKofT=F, TI~100) | ✓ PASS | ✓ Identical | N/A | N/A |
| 34 | Bennu T→TI (LKofT=F, TI~100) | ✗ FAIL | ✗ Different | N/A | N/A |
| 35 | Mars T→TI cross-LKofT (T→F) | ✓ PASS | ✓ Identical | N/A | N/A |
| 36 | Moon default | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 37 | Moon low INERTIA=55 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 38 | Bennu default | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 39 | Ceres default | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 40 | 1P-Halley comet | ✗ FAIL | ✓ Identical | ✗ Different | 1.1436 |
| 41 | Phobos with PFlux | ✗ FAIL | ✗ Different | ✗ Different | 18.5310 |
| 42 | Phobos without PFlux | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 43 | Phobos with PFlux Lon_Hr=12 | ✗ FAIL | ✗ Different | ✗ Different | 25.8579 |
| 44 | Jupiter with N1=40 | ✗ FAIL | ✓ Identical | ✗ Different | 0.0567 |
| 45 | PORB Mars | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 46 | PORB Phobos | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 47 | PORB Europa | ✓ PASS | ✓ Identical | ✓ Identical | 0.0006 |
| 48 | PORB Bennu | ✓ PASS | ✓ Identical | ✓ Identical | 0.0011 |
| 49 | PORB Ceres | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 50 | Europa N1=20 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 51 | Europa N1=30 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 52 | Bennu N1=25 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 53 | Bennu N1=33 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 54 | Europa N1=39 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 55 | Mars N1=50 custom FLAY/RLAY | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 56 | Europa N1=250 (high) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0003 |
| 57 | Mars N1=999 (extreme) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 58 | Mars N1=20 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 59 | Mode 1: Standard constant properties | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 60 | Mode 2: Two-layer (thick=0.5) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 61 | Mode 3: Exponential profile (thick=-0.20) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 62 | Mode 3: Steep exponential (thick=-0.05) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 63 | Mode 3: Very high inertia contrast on Bennu (50→600, H=0.02m, N2=30000) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 64 | Mode 4: Basic zone table | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 65 | Mode 4: Complex zone table | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 66 | Thick with dust over rock | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 67 | Europa ice layers (thick=2.0) | ✗ FAIL | ✗ Different | ✗ Different | 0.0644 |
| 68 | Thick with lbound heat flow | ✗ FAIL | ✗ Different | ✓ Identical | 0.0043 |
| 69 | Exponential with N1=50 | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 70 | Very thin layer (thick=0.01) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 71 | Very deep interface (thick=10.0) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 72 | lzone with external file | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |

## Failed Tests - Detailed Analysis

### Eclipse Style 1.0 (daily)

**Failure Reason:** Eclipse Style 1.0 (daily): PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_eclipse/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 24 15:36:50           0 =IQ   errorfile = eLog20251024T153650.305                 
    .inp and .prt will be added to your input names
    Defaults:  input = krc , output = input
   ?* Input file name or / for default =krc
   ?* Print file name or / for default =krc
   TCARD notice: N4,MAXN4=           1          37
    RBUF= 3 9 0 'LVFT' /
    RBUF= 3 10 1 'LKofT' /
    RBUF= 3 12 1 'LKEY' /
    RBUF= 3 14 0 'LZONE' /
    RBUF= 2 1 32 'N1' /
    RBUF= 2 2 864 'N2' /
    RBUF= 2 3 15 'N3' /
    RBUF= 2 5 1080 'N5' /
    RBUF= 2 6 288 'N24' /
    RBUF= 2 7 -1 'IIB' /
    RBUF= 2 8 999 'IC2' /
    RBUF= 2 9 3 'NRSET' /
    RBUF= 2 12 721 'JDISK' /
    RBUF= 2 15 0 'TUN_Flx15' /
    RBUF= 2 17 52 'K4OUT' /
    RBUF= 2 18 0 'JBARE' /
    RBUF= 1 1 6.700E-01 'ALBEDO' /
    RBUF= 1 2 1.0000 'EMISS' /
    RBUF= 1 3 45.0000 'INERTIA' /
    RBUF= 1 4 6.000E-03 'COND2' /
    RBUF= 1 5 384.7783 'DENS2' /
    RBUF= 1 6 3.5500 'PERIOD' /
    RBUF= 1 7 877.0655 'SPEC_HEAT' /
    RBUF= 1 8 384.7783 'DENSITY' /
    RBUF= 1 12 0.000E+00 'PTOTAL' /
    RBUF= 1 15 180.0000 'TDEEP' /
    RBUF= 1 16 877.0655 'SpHeat2' /
    RBUF= 1 17 0.000E+00 'TAUD' /
    RBUF= 1 18 -999.0000 'DUSTA' /
    RBUF= 1 19 -999.0000 'TAURAT' /
    RBUF= 1 21 0.000E+00 'ARC2_G0' /
    RBUF= 1 23 0.000E+00 'SLOPE' /
    RBUF= 1 24 0.000E+00 'SLOAZI' /
    RBUF= 1 25 0.000E+00 'TFROST' /
    RBUF= 1 33 1.1500 'RLAY' /
    RBUF= 1 34 1.000E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 1.000E-01 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 12.0358 'DELJUL' /
    RBUF= 1 47 1.3150 'GRAV' /
    RBUF= 1 49 6.653E-03 'ConUp0' /
    RBUF= 1 50 1.632E-03 'ConUp1' /
    RBUF= 1 51 1.360E-03 'ConUp2' /
    RBUF= 1 52 3.779E-04 'ConUp3' /
    RBUF= 1 53 6.653E-03 'ConLo0' /
    RBUF= 1 54 1.632E-03 'ConLo1' /
    RBUF= 1 55 1.360E-03 'ConLo2' /
    RBUF= 1 56 3.779E-04 'ConLo3' /
    RBUF= 1 57 1704.5700 'SphUp0' /
    RBUF= 1 58 713.3390 'SphUp1' /
    RBUF= 1 59 110.6940 'SphUp2' /
    RBUF= 1 60 75.7506 'SphUp3' /
    RBUF= 1 61 1704.5700 'SphLo0' /
    RBUF= 1 62 713.3390 'SphLo1' /
    RBUF= 1 63 110.6940 'SphLo2' /
    RBUF= 1 64 75.7506 'SphLo3' /
    RBUF= 8 5 0 './krc.t52' /
   TCARD@280           5
  TCARD280  5  2  0  0 ./krc.t52
    RBUF= 14 1.0 'Jupiter' 0.0000 0.0000 0.0000 0.0000 /
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.000
   Elog= eLog20251024T153650.305                 
  
  STDERR: 
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_eclipse/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 24 15:36:50           0 =IQ   errorfile = eLog20251024T153650.305                 
  .inp and .prt will be added to your input names
  Defaults:  input = krc , output = input
 ?* Input file name or / for default =krc
 ?* Print file name or / for default =krc
 TCARD notice: N4,MAXN4=           1          37
  RBUF= 3 9 0 'LVFT' /
  RBUF= 3 10 1 'LKofT' /
  RBUF= 3 12 1 'LKEY' /
  RBUF= 3 14 0 'LZONE' /
  RBUF= 2 1 32 'N1' /
  RBUF= 2 2 864 'N2' /
  RBUF= 2 3 15 'N3' /
  RBUF= 2 5 1080 'N5' /
  RBUF= 2 6 288 'N24' /
  RBUF= 2 7 -1 'IIB' /
  RBUF= 2 8 999 'IC2' /
  RBUF= 2 9 3 'NRSET' /
  RBUF= 2 12 721 'JDISK' /
  RBUF= 2 15 0 'TUN_Flx15' /
  RBUF= 2 17 52 'K4OUT' /
  RBUF= 2 18 0 'JBARE' /
  RBUF= 1 1 6.700E-01 'ALBEDO' /
  RBUF= 1 2 1.0000 'EMISS' /
  RBUF= 1 3 45.0000 'INERTIA' /
  RBUF= 1 4 6.000E-03 'COND2' /
  RBUF= 1 5 384.7783 'DENS2' /
  RBUF= 1 6 3.5500 'PERIOD' /
  RBUF= 1 7 877.0655 'SPEC_HEAT' /
  RBUF= 1 8 384.7783 'DENSITY' /
  RBUF= 1 12 0.000E+00 'PTOTAL' /
  RBUF= 1 15 180.0000 'TDEEP' /
  RBUF= 1 16 877.0655 'SpHeat2' /
  RBUF= 1 17 0.000E+00 'TAUD' /
  RBUF= 1 18 -999.0000 'DUSTA' /
  RBUF= 1 19 -999.0000 'TAURAT' /
  RBUF= 1 21 0.000E+00 'ARC2_G0' /
  RBUF= 1 23 0.000E+00 'SLOPE' /
  RBUF= 1 24 0.000E+00 'SLOAZI' /
  RBUF= 1 25 0.000E+00 'TFROST' /
  RBUF= 1 33 1.1500 'RLAY' /
  RBUF= 1 34 1.000E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 1.000E-01 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 12.0358 'DELJUL' /
  RBUF= 1 47 1.3150 'GRAV' /
  RBUF= 1 49 6.653E-03 'ConUp0' /
  RBUF= 1 50 1.632E-03 'ConUp1' /
  RBUF= 1 51 1.360E-03 'ConUp2' /
  RBUF= 1 52 3.779E-04 'ConUp3' /
  RBUF= 1 53 6.653E-03 'ConLo0' /
  RBUF= 1 54 1.632E-03 'ConLo1' /
  RBUF= 1 55 1.360E-03 'ConLo2' /
  RBUF= 1 56 3.779E-04 'ConLo3' /
  RBUF= 1 57 1704.5700 'SphUp0' /
  RBUF= 1 58 713.3390 'SphUp1' /
  RBUF= 1 59 110.6940 'SphUp2' /
  RBUF= 1 60 75.7506 'SphUp3' /
  RBUF= 1 61 1704.5700 'SphLo0' /
  RBUF= 1 62 713.3390 'SphLo1' /
  RBUF= 1 63 110.6940 'SphLo2' /
  RBUF= 1 64 75.7506 'SphLo3' /
  RBUF= 8 5 0 './krc.t52' /
 TCARD@280           5
TCARD280  5  2  0  0 ./krc.t52
  RBUF= 14 1.0 'Jupiter' 0.0000 0.0000 0.0000 0.0000 /
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.000
 Elog= eLog20251024T153650.305                 

STDERR: 
```

**Temperature Array Status:** Not compared (reason unknown)

### PFlux (Type 15)

**Failure Reason:** PFlux (Type 15): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 99
First few differences:
--- /tmp/krc_integration_test_type15_pflux/pykrc/krc.inp
+++ /tmp/krc_integration_test_type15_pflux/davinci/krc.inp
@@ -56,14 +56,14 @@
 1 1 5.500E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

 1 3 50.0000 'INERTIA' /

-1 4 7.384E-03 'COND2' /

-1 5 386.0510 'DENS2' /

+1 4 7.282E-03 'COND2' /

+1 5 391.4233 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 386.0510 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

+1 8 391.4233 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP'; PFlux (Type 15): TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 1.2802679389037763 K
Max rel diff: 0.015482755960895751
Mean abs diff: 0.182556348785937 K

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 99

**First Differences:**
```diff
--- /tmp/krc_integration_test_type15_pflux/pykrc/krc.inp
+++ /tmp/krc_integration_test_type15_pflux/davinci/krc.inp
@@ -56,14 +56,14 @@
 1 1 5.500E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

 1 3 50.0000 'INERTIA' /

-1 4 7.384E-03 'COND2' /

-1 5 386.0510 'DENS2' /

+1 4 7.282E-03 'COND2' /

+1 5 391.4233 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 386.0510 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

+1 8 391.4233 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -78,22 +78,22 @@
 1 41 1.000E-01 'DJUL' /

 1 42 12.0358 'DELJUL' /

 1 47 1.3150 'GRAV' /

-1 49 8.187E-03 'ConUp0' /

-1 50 2.009E-03 'ConUp1' /

-1 51 1.674E-03 'ConUp2' /

-1 52 4.650E-04 'ConUp3' /

-1 53 8.187E-03 'ConLo0' /

-1 54 2.009E-03 'ConLo1' /

-1 55 1.674E-03 'ConLo2' /

-1 56 4.650E-04 'ConLo3' /

-1 57 1704.5700 'SphUp0' /

+1 49 8.075E
```

**Tolerance Tiers Applied:**
- cubic_coefficients: 2 parameters
- derived_properties: 5 parameters
- linear_coefficients: 4 parameters
- primary_coefficients: 2 parameters
- quadratic_coefficients: 2 parameters

### Europa T→TI (LKofT=F)

**Failure Reason:** Europa T→TI (LKofT=F): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 578 vs 578
First few differences:
--- /tmp/krc_integration_test_europa_ti_f/pykrc/krc.inp
+++ /tmp/krc_integration_test_europa_ti_f/davinci/krc.inp
@@ -43,7 +43,7 @@
 3 14 0 'LZONE' /

 2 3 15 'N3' /

 2 5 135 'N5' /

-2 6 96 'N24' /

+2 6 288 'N24' /

 2 7 -1 'IIB' /

 2 9 3 'NRSET' /

 2 12 91 'JDISK' /

@@ -52,13 +52,13 @@
 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 4 2.859E-02 'COND2' /

-1 5 398.7780 'DENS2' /

+1 4 2.822E-02 'COND2' /

+1 5 404.0254 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.06

**Input File Differences:**
- PyKRC lines: 578
- Davinci lines: 578

**First Differences:**
```diff
--- /tmp/krc_integration_test_europa_ti_f/pykrc/krc.inp
+++ /tmp/krc_integration_test_europa_ti_f/davinci/krc.inp
@@ -43,7 +43,7 @@
 3 14 0 'LZONE' /

 2 3 15 'N3' /

 2 5 135 'N5' /

-2 6 96 'N24' /

+2 6 288 'N24' /

 2 7 -1 'IIB' /

 2 9 3 'NRSET' /

 2 12 91 'JDISK' /

@@ -52,13 +52,13 @@
 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 4 2.859E-02 'COND2' /

-1 5 398.7780 'DENS2' /

+1 4 2.822E-02 'COND2' /

+1 5 404.0254 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

+1 7 877.0656 'SPEC_HEAT' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -69,510 +69,510 @@
 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41 1.000E-01 'DJUL' /

-1 42 96.2866 'DELJUL' /

+1 42 15.2664 'DELJUL' /

 1 47 1.3150 'GRAV' /

-1 53 3.170E-02 'ConLo0' /

-1 54 7.778E-03 'ConLo1' /

-1 55 6.482E-03 'ConLo2' /

```

**Temperature Array Status:** Not compared (reason unknown)

### Europa T→TI (LKofT=T)

**Failure Reason:** Europa T→TI (LKofT=T): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 578 vs 578
First few differences:
--- /tmp/krc_integration_test_europa_ti_t/pykrc/krc.inp
+++ /tmp/krc_integration_test_europa_ti_t/davinci/krc.inp
@@ -43,7 +43,7 @@
 3 14 0 'LZONE' /

 2 3 15 'N3' /

 2 5 135 'N5' /

-2 6 96 'N24' /

+2 6 288 'N24' /

 2 7 -1 'IIB' /

 2 9 3 'NRSET' /

 2 12 91 'JDISK' /

@@ -52,13 +52,13 @@
 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 4 2.859E-02 'COND2' /

-1 5 398.7780 'DENS2' /

+1 4 2.822E-02 'COND2' /

+1 5 404.0254 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.06

**Input File Differences:**
- PyKRC lines: 578
- Davinci lines: 578

**First Differences:**
```diff
--- /tmp/krc_integration_test_europa_ti_t/pykrc/krc.inp
+++ /tmp/krc_integration_test_europa_ti_t/davinci/krc.inp
@@ -43,7 +43,7 @@
 3 14 0 'LZONE' /

 2 3 15 'N3' /

 2 5 135 'N5' /

-2 6 96 'N24' /

+2 6 288 'N24' /

 2 7 -1 'IIB' /

 2 9 3 'NRSET' /

 2 12 91 'JDISK' /

@@ -52,13 +52,13 @@
 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 4 2.859E-02 'COND2' /

-1 5 398.7780 'DENS2' /

+1 4 2.822E-02 'COND2' /

+1 5 404.0254 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

+1 7 877.0656 'SPEC_HEAT' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -69,510 +69,510 @@
 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41 1.000E-01 'DJUL' /

-1 42 96.2866 'DELJUL' /

+1 42 15.2664 'DELJUL' /

 1 47 1.3150 'GRAV' /

-1 53 3.170E-02 'ConLo0' /

-1 54 7.778E-03 'ConLo1' /

-1 55 6.482E-03 'ConLo2' /

```

**Temperature Array Status:** Not compared (reason unknown)

### Bennu T→TI (LKofT=F, TI~100)

**Failure Reason:** Bennu T→TI (LKofT=F, TI~100): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 577 vs 577
First few differences:
--- /tmp/krc_integration_test_bennu_ti/pykrc/krc.inp
+++ /tmp/krc_integration_test_bennu_ti/davinci/krc.inp
@@ -68,11 +68,11 @@
 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41 1.000E-01 'DJUL' /

-1 42 9.7111 'DELJUL' /

+1 42 15.2664 'DELJUL' /

 1 47 0.000E+00 'GRAV' /

 1 53 1.476E-02 'ConLo0' /

-1 54 -4.045E-19 'ConLo1' /

-1 55 2.515E-18 'ConLo2' /

+1 54 6.354E-10 'ConLo1' /

+1 55 2.135E-10 'ConLo2' /

 1 56 9.294E-04 'ConLo3' /

 1 57 609.9060 'SphUp0' /

 1 58 214.2310 'S

**Input File Differences:**
- PyKRC lines: 577
- Davinci lines: 577

**First Differences:**
```diff
--- /tmp/krc_integration_test_bennu_ti/pykrc/krc.inp
+++ /tmp/krc_integration_test_bennu_ti/davinci/krc.inp
@@ -68,11 +68,11 @@
 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41 1.000E-01 'DJUL' /

-1 42 9.7111 'DELJUL' /

+1 42 15.2664 'DELJUL' /

 1 47 0.000E+00 'GRAV' /

 1 53 1.476E-02 'ConLo0' /

-1 54 -4.045E-19 'ConLo1' /

-1 55 2.515E-18 'ConLo2' /

+1 54 6.354E-10 'ConLo1' /

+1 55 2.135E-10 'ConLo2' /

 1 56 9.294E-04 'ConLo3' /

 1 57 609.9060 'SphUp0' /

 1 58 214.2310 'SphUp1' /

@@ -83,28 +83,28 @@
 1 63 -40.9437 'SphLo2' /

 1 64 11.2575 'SphLo3' /

 1 3 10.0000 'INERTIA' /

-1 8 1047.0909 'DENSITY' /

-2 2 864 'N2' /

-1 49 0.000E+00 'ConUp0' /

-1 50 0.000E+00 'ConUp1' /

-1 51 0.000E+00 'ConUp2' /

-1 52 0.000E+00 'ConUp3' /

-1 34 1.000E-01 'FLAY' /

-1 33 1.1500 'RLAY' /

-2 8 999 'IC2' /

-2 1 42 'N1' /

+1 8 1047.0908 'DENSITY' /

+2 2 864 'N2' /

+1 49 0.000E+00 'ConUp0' /

+1 50 0.000E+00 'ConUp1' /

+1 51 0.000E+00 'ConUp2' /

+1 52 0.000E+00 'ConUp
```

**Tolerance Tiers Applied:**
- datetime: 1 parameters

**Temperature Array Status:** Not compared (reason unknown)

### 1P-Halley comet

**Failure Reason:** 1P-Halley comet: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 1.1436345220979334 K
Max rel diff: 0.01933461900541214
Mean abs diff: 0.03028436857390397 K

**Tolerance Tiers Applied:**
- linear_coefficients: 2 parameters
- quadratic_coefficients: 2 parameters

### Phobos with PFlux

**Failure Reason:** Phobos with PFlux: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 99
First few differences:
--- /tmp/krc_integration_test_phobos_with_pflux/pykrc/krc.inp
+++ /tmp/krc_integration_test_phobos_with_pflux/davinci/krc.inp
@@ -79,12 +79,12 @@
 1 42 1.9085 'DELJUL' /

 1 47 5.700E-03 'GRAV' /

 1 49 1.476E-02 'ConUp0' /

-1 50 -4.045E-19 'ConUp1' /

-1 51 2.515E-18 'ConUp2' /

+1 50 6.354E-10 'ConUp1' /

+1 51 2.135E-10 'ConUp2' /

 1 52 9.294E-04 'ConUp3' /

 1 53 1.476E-02 'ConLo0' /

-1 54 -4.045E-19 'ConLo1' /

-1 55 2.515E-18 'ConLo2' /

+1 54 6.354E-10 'ConLo1' /

+1 55 2.135E-10 'ConL; Phobos with PFlux: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 18.531007027383396 K
Max rel diff: 0.11696617049941078
Mean abs diff: 2.7562296144138783 K

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 99

**First Differences:**
```diff
--- /tmp/krc_integration_test_phobos_with_pflux/pykrc/krc.inp
+++ /tmp/krc_integration_test_phobos_with_pflux/davinci/krc.inp
@@ -79,12 +79,12 @@
 1 42 1.9085 'DELJUL' /

 1 47 5.700E-03 'GRAV' /

 1 49 1.476E-02 'ConUp0' /

-1 50 -4.045E-19 'ConUp1' /

-1 51 2.515E-18 'ConUp2' /

+1 50 6.354E-10 'ConUp1' /

+1 51 2.135E-10 'ConUp2' /

 1 52 9.294E-04 'ConUp3' /

 1 53 1.476E-02 'ConLo0' /

-1 54 -4.045E-19 'ConLo1' /

-1 55 2.515E-18 'ConLo2' /

+1 54 6.354E-10 'ConLo1' /

+1 55 2.135E-10 'ConLo2' /

 1 56 9.294E-04 'ConLo3' /

 1 57 609.9060 'SphUp0' /

 1 58 214.2310 'SphUp1' /

@@ -94,6 +94,6 @@
 1 62 214.2310 'SphLo1' /

 1 63 -40.9437 'SphLo2' /

 1 64 11.2575 'SphLo3' /

-15 0.0000 0.0000 0.0000 0.000000 0.0000 0.000000 0.0000 0.0000 12.0000 0.0000 0.0000 /

+15 17.02 6.00 0.00 5.74 5.74 0.00 12.00 / Forcing from Planet on Satellite

 0/

 0/

```

**Tolerance Tiers Applied:**
- linear_coefficients: 2 parameters
- quadratic_coefficients: 2 parameters

### Phobos with PFlux Lon_Hr=12

**Failure Reason:** Phobos with PFlux Lon_Hr=12: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 99
First few differences:
--- /tmp/krc_integration_test_phobos_flux_comparison/pykrc/krc.inp
+++ /tmp/krc_integration_test_phobos_flux_comparison/davinci/krc.inp
@@ -57,10 +57,10 @@
 1 2 1.0000 'EMISS' /

 1 3 50.0000 'INERTIA' /

 1 4 3.811E-03 'COND2' /

-1 5 1075.4545 'DENS2' /

+1 5 1075.4546 'DENS2' /

 1 6 3.189E-01 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1075.4545 'DENSITY' /

+1 8 1075.4546 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -79,12 +79,12 @@
 1 ; Phobos with PFlux Lon_Hr=12: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 25.857940556156976 K
Max rel diff: 0.17582106165871592
Mean abs diff: 3.168055238622978 K

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 99

**First Differences:**
```diff
--- /tmp/krc_integration_test_phobos_flux_comparison/pykrc/krc.inp
+++ /tmp/krc_integration_test_phobos_flux_comparison/davinci/krc.inp
@@ -57,10 +57,10 @@
 1 2 1.0000 'EMISS' /

 1 3 50.0000 'INERTIA' /

 1 4 3.811E-03 'COND2' /

-1 5 1075.4545 'DENS2' /

+1 5 1075.4546 'DENS2' /

 1 6 3.189E-01 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1075.4545 'DENSITY' /

+1 8 1075.4546 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -79,12 +79,12 @@
 1 42 1.9085 'DELJUL' /

 1 47 5.700E-03 'GRAV' /

 1 49 3.811E-03 'ConUp0' /

-1 50 -1.689E-20 'ConUp1' /

-1 51 6.112E-19 'ConUp2' /

+1 50 4.194E-10 'ConUp1' /

+1 51 1.205E-10 'ConUp2' /

 1 52 2.400E-04 'ConUp3' /

 1 53 3.811E-03 'ConLo0' /

-1 54 -1.689E-20 'ConLo1' /

-1 55 6.112E-19 'ConLo2' /

+1 54 4.194E-10 'ConLo1' /

+1 55 1.205E-10 'ConLo2' /

 1 56 2.400E-04 'ConLo3' /

 1 57 609.9060 'SphUp0' /

 1 58 214.2310 'SphUp1' /

@@ -94,6 +94,6 @@
 1 62 214.2310 'SphLo1' /

 1 63 -40.9437 
```

**Tolerance Tiers Applied:**
- derived_properties: 2 parameters
- linear_coefficients: 2 parameters
- quadratic_coefficients: 2 parameters

### Jupiter with N1=40

**Failure Reason:** Jupiter with N1=40: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 0.05667863139993301 K
Max rel diff: 0.0005863085526219916
Mean abs diff: 0.00371120232702321 K

**Tolerance Tiers Applied:**
- derived_properties: 2 parameters
- linear_coefficients: 2 parameters
- quadratic_coefficients: 2 parameters

### Mode 4: Basic zone table

**Failure Reason:** Mode 4: Basic zone table: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_basic/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 24 15:41:55           0 =IQ   errorfile = eLog20251024T154155.730                 
    .inp and .prt will be added to your input names
    Defaults:  input = krc , output = input
   ?* Input file name or / for default =krc
   ?* Print file name or / for default =krc
   TCARD notice: N4,MAXN4=           1          37
    RBUF= 8 25 0 'zonefile.tab' /
   TCARD@280          25
   LZONE,I= T          12
  TCARD280 25  2  0  0 zonefile.tab
    RBUF= 3 9 0 'LVFT' /
    RBUF= 3 10 1 'LKofT' /
    RBUF= 3 12 1 'LKEY' /
    RBUF= 3 14 1 'LZONE' /
    RBUF= 2 1 37 'N1' /
    RBUF= 2 2 864 'N2' /
    RBUF= 2 3 1 'N3' /
    RBUF= 2 5 1080 'N5' /
    RBUF= 2 6 96 'N24' /
    RBUF= 2 7 -1 'IIB' /
    RBUF= 2 8 999 'IC2' /
    RBUF= 2 9 999 'NRSET' /
    RBUF= 2 12 721 'JDISK' /
    RBUF= 2 15 0 'TUN_Flx15' /
    RBUF= 2 16 1 'KPREF' /
    RBUF= 2 17 52 'K4OUT' /
    RBUF= 2 18 0 'JBARE' /
    RBUF= 1 1 2.364E-01 'ALBEDO' /
    RBUF= 1 2 1.0000 'EMISS' /
    RBUF= 1 3 52.0000 'INERTIA' /
    RBUF= 1 4 4.117E-03 'COND2' /
    RBUF= 1 5 1076.8727 'DENS2' /
    RBUF= 1 6 1.0260 'PERIOD' /
    RBUF= 1 7 609.9060 'SPEC_HEAT' /
    RBUF= 1 8 1076.8727 'DENSITY' /
    RBUF= 1 12 546.0000 'PTOTAL' /
    RBUF= 1 15 180.0000 'TDEEP' /
    RBUF= 1 16 609.9060 'SpHeat2' /
    RBUF= 1 17 3.000E-01 'TAUD' /
    RBUF= 1 18 9.000E-01 'DUSTA' /
    RBUF= 1 19 2.200E-01 'TAURAT' /
    RBUF= 1 21 5.000E-01 'ARC2_G0' /
    RBUF= 1 23 0.000E+00 'SLOPE' /
    RBUF= 1 24 0.000E+00 'SLOAZI' /
    RBUF= 1 25 146.0000 'TFROST' /
    RBUF= 1 33 1.1500 'RLAY' /
    RBUF= 1 34 1.000E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 99.0000 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 1.9083 'DELJUL' /
    RBUF= 1 47 3.7110 'GRAV' /
    RBUF= 1 49 4.139E-03 'ConUp0' /
    RBUF= 1 50 9.500E-04 'ConUp1' /
    RBUF= 1 51 -1.475E-04 'ConUp2' /
    RBUF= 1 52 2.740E-05 'ConUp3' /
    RBUF= 1 53 4.139E-03 'ConLo0' /
    RBUF= 1 54 9.500E-04 'ConLo1' /
    RBUF= 1 55 -1.475E-04 'ConLo2' /
    RBUF= 1 56 2.740E-05 'ConLo3' /
    RBUF= 1 57 609.9060 'SphUp0' /
    RBUF= 1 58 214.2310 'SphUp1' /
    RBUF= 1 59 -40.9437 'SphUp2' /
    RBUF= 1 60 11.2575 'SphUp3' /
    RBUF= 1 61 609.9060 'SphLo0' /
    RBUF= 1 62 214.2310 'SphLo1' /
    RBUF= 1 63 -40.9437 'SphLo2' /
    RBUF= 1 64 11.2575 'SphLo3' /
    RBUF= 8 5 0 './krc.t52' /
   TCARD@280           5
  TCARD280  5  2  0  0 ./krc.t52
    RBUF= 0/
   IBD1:6           0           0           0           0           0           0
   FZONE,NZ,JPRT=zonefile.tab        1000           7
   return NZ=           37
   K,IH,LALCON=           1           0 T
   K,IH,LALCON=           2           0 T
   K,IH,LALCON=           3           0 T
   K,IH,LALCON=           4           0 T
   K,IH,LALCON=           5           0 T
   K,IH,LALCON=           6           0 T
   K,IH,LALCON=           7           0 T
   K,IH,LALCON=           8           0 T
   K,IH,LALCON=           9           0 T
   K,IH,LALCON=          10           0 T
   K,IH,LALCON=          11           0 T
   K,IH,LALCON=          12           0 T
   K,IH,LALCON=          13           0 T
   K,IH,LALCON=          14           0 T
   K,IH,LALCON=          15           0 T
   K,IH,LALCON=          16           0 T
   K,IH,LALCON=          17           0 T
   K,IH,LALCON=          18           0 T
   K,IH,LALCON=          19           0 T
   Case  1  DTIME: total, user, system=    0.0007    0.0003    0.0004
   Case  1  DTIME: total, user, system=    0.0007    0.0003    0.0004
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251024T154155.730                 
  
  STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG
  
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_basic/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 24 15:41:55           0 =IQ   errorfile = eLog20251024T154155.730                 
  .inp and .prt will be added to your input names
  Defaults:  input = krc , output = input
 ?* Input file name or / for default =krc
 ?* Print file name or / for default =krc
 TCARD notice: N4,MAXN4=           1          37
  RBUF= 8 25 0 'zonefile.tab' /
 TCARD@280          25
 LZONE,I= T          12
TCARD280 25  2  0  0 zonefile.tab
  RBUF= 3 9 0 'LVFT' /
  RBUF= 3 10 1 'LKofT' /
  RBUF= 3 12 1 'LKEY' /
  RBUF= 3 14 1 'LZONE' /
  RBUF= 2 1 37 'N1' /
  RBUF= 2 2 864 'N2' /
  RBUF= 2 3 1 'N3' /
  RBUF= 2 5 1080 'N5' /
  RBUF= 2 6 96 'N24' /
  RBUF= 2 7 -1 'IIB' /
  RBUF= 2 8 999 'IC2' /
  RBUF= 2 9 999 'NRSET' /
  RBUF= 2 12 721 'JDISK' /
  RBUF= 2 15 0 'TUN_Flx15' /
  RBUF= 2 16 1 'KPREF' /
  RBUF= 2 17 52 'K4OUT' /
  RBUF= 2 18 0 'JBARE' /
  RBUF= 1 1 2.364E-01 'ALBEDO' /
  RBUF= 1 2 1.0000 'EMISS' /
  RBUF= 1 3 52.0000 'INERTIA' /
  RBUF= 1 4 4.117E-03 'COND2' /
  RBUF= 1 5 1076.8727 'DENS2' /
  RBUF= 1 6 1.0260 'PERIOD' /
  RBUF= 1 7 609.9060 'SPEC_HEAT' /
  RBUF= 1 8 1076.8727 'DENSITY' /
  RBUF= 1 12 546.0000 'PTOTAL' /
  RBUF= 1 15 180.0000 'TDEEP' /
  RBUF= 1 16 609.9060 'SpHeat2' /
  RBUF= 1 17 3.000E-01 'TAUD' /
  RBUF= 1 18 9.000E-01 'DUSTA' /
  RBUF= 1 19 2.200E-01 'TAURAT' /
  RBUF= 1 21 5.000E-01 'ARC2_G0' /
  RBUF= 1 23 0.000E+00 'SLOPE' /
  RBUF= 1 24 0.000E+00 'SLOAZI' /
  RBUF= 1 25 146.0000 'TFROST' /
  RBUF= 1 33 1.1500 'RLAY' /
  RBUF= 1 34 1.000E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 99.0000 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 1.9083 'DELJUL' /
  RBUF= 1 47 3.7110 'GRAV' /
  RBUF= 1 49 4.139E-03 'ConUp0' /
  RBUF= 1 50 9.500E-04 'ConUp1' /
  RBUF= 1 51 -1.475E-04 'ConUp2' /
  RBUF= 1 52 2.740E-05 'ConUp3' /
  RBUF= 1 53 4.139E-03 'ConLo0' /
  RBUF= 1 54 9.500E-04 'ConLo1' /
  RBUF= 1 55 -1.475E-04 'ConLo2' /
  RBUF= 1 56 2.740E-05 'ConLo3' /
  RBUF= 1 57 609.9060 'SphUp0' /
  RBUF= 1 58 214.2310 'SphUp1' /
  RBUF= 1 59 -40.9437 'SphUp2' /
  RBUF= 1 60 11.2575 'SphUp3' /
  RBUF= 1 61 609.9060 'SphLo0' /
  RBUF= 1 62 214.2310 'SphLo1' /
  RBUF= 1 63 -40.9437 'SphLo2' /
  RBUF= 1 64 11.2575 'SphLo3' /
  RBUF= 8 5 0 './krc.t52' /
 TCARD@280           5
TCARD280  5  2  0  0 ./krc.t52
  RBUF= 0/
 IBD1:6           0           0           0           0           0           0
 FZONE,NZ,JPRT=zonefile.tab        1000           7
 return NZ=           37
 K,IH,LALCON=           1           0 T
 K,IH,LALCON=           2           0 T
 K,IH,LALCON=           3           0 T
 K,IH,LALCON=           4           0 T
 K,IH,LALCON=           5           0 T
 K,IH,LALCON=           6           0 T
 K,IH,LALCON=           7           0 T
 K,IH,LALCON=           8           0 T
 K,IH,LALCON=           9           0 T
 K,IH,LALCON=          10           0 T
 K,IH,LALCON=          11           0 T
 K,IH,LALCON=          12           0 T
 K,IH,LALCON=          13           0 T
 K,IH,LALCON=          14           0 T
 K,IH,LALCON=          15           0 T
 K,IH,LALCON=          16           0 T
 K,IH,LALCON=          17           0 T
 K,IH,LALCON=          18           0 T
 K,IH,LALCON=          19           0 T
 Case  1  DTIME: total, user, system=    0.0007    0.0003    0.0004
 Case  1  DTIME: total, user, system=    0.0007    0.0003    0.0004
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251024T154155.730                 

STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG

```

**Temperature Array Status:** Not compared (reason unknown)

### Mode 4: Complex zone table

**Failure Reason:** Mode 4: Complex zone table: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_complex/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 24 15:41:57           0 =IQ   errorfile = eLog20251024T154157.150                 
    .inp and .prt will be added to your input names
    Defaults:  input = krc , output = input
   ?* Input file name or / for default =krc
   ?* Print file name or / for default =krc
   TCARD notice: N4,MAXN4=           1          37
    RBUF= 8 25 0 'zonefile.tab' /
   TCARD@280          25
   LZONE,I= T          12
  TCARD280 25  2  0  0 zonefile.tab
    RBUF= 3 9 0 'LVFT' /
    RBUF= 3 10 1 'LKofT' /
    RBUF= 3 12 1 'LKEY' /
    RBUF= 3 14 1 'LZONE' /
    RBUF= 2 1 37 'N1' /
    RBUF= 2 2 864 'N2' /
    RBUF= 2 3 1 'N3' /
    RBUF= 2 5 1080 'N5' /
    RBUF= 2 6 96 'N24' /
    RBUF= 2 7 -1 'IIB' /
    RBUF= 2 8 999 'IC2' /
    RBUF= 2 9 999 'NRSET' /
    RBUF= 2 12 721 'JDISK' /
    RBUF= 2 15 0 'TUN_Flx15' /
    RBUF= 2 16 1 'KPREF' /
    RBUF= 2 17 52 'K4OUT' /
    RBUF= 2 18 0 'JBARE' /
    RBUF= 1 1 1.500E-01 'ALBEDO' /
    RBUF= 1 2 1.0000 'EMISS' /
    RBUF= 1 3 150.0000 'INERTIA' /
    RBUF= 1 4 3.218E-02 'COND2' /
    RBUF= 1 5 1146.3636 'DENS2' /
    RBUF= 1 6 1.0260 'PERIOD' /
    RBUF= 1 7 609.9060 'SPEC_HEAT' /
    RBUF= 1 8 1146.3636 'DENSITY' /
    RBUF= 1 12 546.0000 'PTOTAL' /
    RBUF= 1 15 180.0000 'TDEEP' /
    RBUF= 1 16 609.9060 'SpHeat2' /
    RBUF= 1 17 3.000E-01 'TAUD' /
    RBUF= 1 18 9.000E-01 'DUSTA' /
    RBUF= 1 19 2.200E-01 'TAURAT' /
    RBUF= 1 21 5.000E-01 'ARC2_G0' /
    RBUF= 1 23 0.000E+00 'SLOPE' /
    RBUF= 1 24 0.000E+00 'SLOAZI' /
    RBUF= 1 25 146.0000 'TFROST' /
    RBUF= 1 33 1.1500 'RLAY' /
    RBUF= 1 34 1.000E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 99.0000 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 1.9083 'DELJUL' /
    RBUF= 1 47 3.7110 'GRAV' /
    RBUF= 1 49 3.235E-02 'ConUp0' /
    RBUF= 1 50 7.425E-03 'ConUp1' /
    RBUF= 1 51 -1.153E-03 'ConUp2' /
    RBUF= 1 52 2.141E-04 'ConUp3' /
    RBUF= 1 53 3.235E-02 'ConLo0' /
    RBUF= 1 54 7.425E-03 'ConLo1' /
    RBUF= 1 55 -1.153E-03 'ConLo2' /
    RBUF= 1 56 2.141E-04 'ConLo3' /
    RBUF= 1 57 609.9060 'SphUp0' /
    RBUF= 1 58 214.2310 'SphUp1' /
    RBUF= 1 59 -40.9437 'SphUp2' /
    RBUF= 1 60 11.2575 'SphUp3' /
    RBUF= 1 61 609.9060 'SphLo0' /
    RBUF= 1 62 214.2310 'SphLo1' /
    RBUF= 1 63 -40.9437 'SphLo2' /
    RBUF= 1 64 11.2575 'SphLo3' /
    RBUF= 8 5 0 './krc.t52' /
   TCARD@280           5
  TCARD280  5  2  0  0 ./krc.t52
    RBUF= 0/
   IBD1:6           0           0           0           0           0           0
   FZONE,NZ,JPRT=zonefile.tab        1000           7
   return NZ=           37
   K,IH,LALCON=           1           0 T
   K,IH,LALCON=           2           0 T
   K,IH,LALCON=           3           0 T
   K,IH,LALCON=           4           0 T
   K,IH,LALCON=           5           0 T
   K,IH,LALCON=           6           0 T
   K,IH,LALCON=           7           0 T
   K,IH,LALCON=           8           0 T
   K,IH,LALCON=           9           0 T
   K,IH,LALCON=          10           0 T
   K,IH,LALCON=          11           0 T
   K,IH,LALCON=          12           0 T
   K,IH,LALCON=          13           0 T
   K,IH,LALCON=          14           0 T
   K,IH,LALCON=          15           0 T
   K,IH,LALCON=          16           0 T
   K,IH,LALCON=          17           0 T
   K,IH,LALCON=          18           0 T
   K,IH,LALCON=          19           0 T
   Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
   Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251024T154157.150                 
  
  STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG
  
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_complex/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 24 15:41:57           0 =IQ   errorfile = eLog20251024T154157.150                 
  .inp and .prt will be added to your input names
  Defaults:  input = krc , output = input
 ?* Input file name or / for default =krc
 ?* Print file name or / for default =krc
 TCARD notice: N4,MAXN4=           1          37
  RBUF= 8 25 0 'zonefile.tab' /
 TCARD@280          25
 LZONE,I= T          12
TCARD280 25  2  0  0 zonefile.tab
  RBUF= 3 9 0 'LVFT' /
  RBUF= 3 10 1 'LKofT' /
  RBUF= 3 12 1 'LKEY' /
  RBUF= 3 14 1 'LZONE' /
  RBUF= 2 1 37 'N1' /
  RBUF= 2 2 864 'N2' /
  RBUF= 2 3 1 'N3' /
  RBUF= 2 5 1080 'N5' /
  RBUF= 2 6 96 'N24' /
  RBUF= 2 7 -1 'IIB' /
  RBUF= 2 8 999 'IC2' /
  RBUF= 2 9 999 'NRSET' /
  RBUF= 2 12 721 'JDISK' /
  RBUF= 2 15 0 'TUN_Flx15' /
  RBUF= 2 16 1 'KPREF' /
  RBUF= 2 17 52 'K4OUT' /
  RBUF= 2 18 0 'JBARE' /
  RBUF= 1 1 1.500E-01 'ALBEDO' /
  RBUF= 1 2 1.0000 'EMISS' /
  RBUF= 1 3 150.0000 'INERTIA' /
  RBUF= 1 4 3.218E-02 'COND2' /
  RBUF= 1 5 1146.3636 'DENS2' /
  RBUF= 1 6 1.0260 'PERIOD' /
  RBUF= 1 7 609.9060 'SPEC_HEAT' /
  RBUF= 1 8 1146.3636 'DENSITY' /
  RBUF= 1 12 546.0000 'PTOTAL' /
  RBUF= 1 15 180.0000 'TDEEP' /
  RBUF= 1 16 609.9060 'SpHeat2' /
  RBUF= 1 17 3.000E-01 'TAUD' /
  RBUF= 1 18 9.000E-01 'DUSTA' /
  RBUF= 1 19 2.200E-01 'TAURAT' /
  RBUF= 1 21 5.000E-01 'ARC2_G0' /
  RBUF= 1 23 0.000E+00 'SLOPE' /
  RBUF= 1 24 0.000E+00 'SLOAZI' /
  RBUF= 1 25 146.0000 'TFROST' /
  RBUF= 1 33 1.1500 'RLAY' /
  RBUF= 1 34 1.000E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 99.0000 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 1.9083 'DELJUL' /
  RBUF= 1 47 3.7110 'GRAV' /
  RBUF= 1 49 3.235E-02 'ConUp0' /
  RBUF= 1 50 7.425E-03 'ConUp1' /
  RBUF= 1 51 -1.153E-03 'ConUp2' /
  RBUF= 1 52 2.141E-04 'ConUp3' /
  RBUF= 1 53 3.235E-02 'ConLo0' /
  RBUF= 1 54 7.425E-03 'ConLo1' /
  RBUF= 1 55 -1.153E-03 'ConLo2' /
  RBUF= 1 56 2.141E-04 'ConLo3' /
  RBUF= 1 57 609.9060 'SphUp0' /
  RBUF= 1 58 214.2310 'SphUp1' /
  RBUF= 1 59 -40.9437 'SphUp2' /
  RBUF= 1 60 11.2575 'SphUp3' /
  RBUF= 1 61 609.9060 'SphLo0' /
  RBUF= 1 62 214.2310 'SphLo1' /
  RBUF= 1 63 -40.9437 'SphLo2' /
  RBUF= 1 64 11.2575 'SphLo3' /
  RBUF= 8 5 0 './krc.t52' /
 TCARD@280           5
TCARD280  5  2  0  0 ./krc.t52
  RBUF= 0/
 IBD1:6           0           0           0           0           0           0
 FZONE,NZ,JPRT=zonefile.tab        1000           7
 return NZ=           37
 K,IH,LALCON=           1           0 T
 K,IH,LALCON=           2           0 T
 K,IH,LALCON=           3           0 T
 K,IH,LALCON=           4           0 T
 K,IH,LALCON=           5           0 T
 K,IH,LALCON=           6           0 T
 K,IH,LALCON=           7           0 T
 K,IH,LALCON=           8           0 T
 K,IH,LALCON=           9           0 T
 K,IH,LALCON=          10           0 T
 K,IH,LALCON=          11           0 T
 K,IH,LALCON=          12           0 T
 K,IH,LALCON=          13           0 T
 K,IH,LALCON=          14           0 T
 K,IH,LALCON=          15           0 T
 K,IH,LALCON=          16           0 T
 K,IH,LALCON=          17           0 T
 K,IH,LALCON=          18           0 T
 K,IH,LALCON=          19           0 T
 Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
 Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251024T154157.150                 

STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG

```

**Temperature Array Status:** Not compared (reason unknown)

### Thick with dust over rock

**Failure Reason:** Thick with dust over rock: PyKRC failed - Two-layer regolith (thick=0.1) requires different properties for upper and lower layers. Specify either:
    - Different INERTIA2, or
    - Different Mat2, or
    - Different Por2
assert False

**PyKRC Error:**
```
Two-layer regolith (thick=0.1) requires different properties for upper and lower layers. Specify either:
  - Different INERTIA2, or
  - Different Mat2, or
  - Different Por2
```

**Temperature Array Status:** Not compared (reason unknown)

### Europa ice layers (thick=2.0)

**Failure Reason:** Europa ice layers (thick=2.0): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_thick_eur_ice/pykrc/krc.inp
+++ /tmp/krc_integration_test_thick_eur_ice/davinci/krc.inp
@@ -56,14 +56,14 @@
 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

 1 3 100.0000 'INERTIA' /

-1 4 5.1685 'COND2' /

-1 5 882.4023 'DENS2' /

+1 4 5.1655 'COND2' /

+1 5 882.9020 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 398.7780 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

+1 8 404.0254 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /
; Europa ice layers (thick=2.0): TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 0.06438677182690355 K
Max rel diff: 0.0007279064219401823
Mean abs diff: 0.010519470304123955 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_thick_eur_ice/pykrc/krc.inp
+++ /tmp/krc_integration_test_thick_eur_ice/davinci/krc.inp
@@ -56,14 +56,14 @@
 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

 1 3 100.0000 'INERTIA' /

-1 4 5.1685 'COND2' /

-1 5 882.4023 'DENS2' /

+1 4 5.1655 'COND2' /

+1 5 882.9020 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 398.7780 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

+1 8 404.0254 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -72,25 +72,25 @@
 1 24 0.000E+00 'SLOAZI' /

 1 25 0.000E+00 'TFROST' /

 1 33 1.1500 'RLAY' /

-1 34 9.111E-02 'FLAY' /

+1 34 9.231E-02 'FLAY' /

 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41 1.000E-01 'DJUL' /

 1 42 12.0358 'DELJUL' /

 1 47 1.3150 'GRAV' /

-1 49 3.170E-02 'ConUp0' /

-1 50 7.778E-03 'ConUp1' /

-1 51 6.482E-03 'ConUp2'
```

**Tolerance Tiers Applied:**
- derived_properties: 5 parameters

### Thick with lbound heat flow

**Failure Reason:** Thick with lbound heat flow: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 97 vs 97
First few differences:
--- /tmp/krc_integration_test_thick_lb/pykrc/krc.inp
+++ /tmp/krc_integration_test_thick_lb/davinci/krc.inp
@@ -56,13 +56,13 @@
 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

 1 3 100.0000 'INERTIA' /

-1 4 5.694E-01 'COND2' /

-1 5 500.5936 'DENS2' /

+1 4 5.646E-01 'COND2' /

+1 5 504.8415 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 398.7780 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

+1 8 404.0254 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

-1 16 877.0655 'SpHeat2' /

+

**Input File Differences:**
- PyKRC lines: 97
- Davinci lines: 97

**First Differences:**
```diff
--- /tmp/krc_integration_test_thick_lb/pykrc/krc.inp
+++ /tmp/krc_integration_test_thick_lb/davinci/krc.inp
@@ -56,13 +56,13 @@
 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

 1 3 100.0000 'INERTIA' /

-1 4 5.694E-01 'COND2' /

-1 5 500.5936 'DENS2' /

+1 4 5.646E-01 'COND2' /

+1 5 504.8415 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 398.7780 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

+1 8 404.0254 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -71,25 +71,25 @@
 1 24 0.000E+00 'SLOAZI' /

 1 25 0.000E+00 'TFROST' /

 1 33 1.1500 'RLAY' /

-1 34 8.600E-02 'FLAY' /

+1 34 8.714E-02 'FLAY' /

 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41 1.000E-01 'DJUL' /

 1 42 12.0358 'DELJUL' /

 1 47 1.3150 'GRAV' /

-1 49 3.170E-02 'ConUp0' /

-1 50 7.778E-03 'ConUp1' /

-1 51 6.482E-03 'ConUp2' /

-1 52 1.801E-03 'ConUp3' /
```

**Tolerance Tiers Applied:**
- derived_properties: 5 parameters

### Exponential with N1=50

**Failure Reason:** Exponential with N1=50: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_thick_neg_n1/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 24 15:42:10           0 =IQ   errorfile = eLog20251024T154210.722                 
    .inp and .prt will be added to your input names
    Defaults:  input = krc , output = input
   ?* Input file name or / for default =krc
   ?* Print file name or / for default =krc
   TCARD notice: N4,MAXN4=           1          37
    RBUF= 8 25 0 'zonefile.tab' /
   TCARD@280          25
   LZONE,I= T          12
  TCARD280 25  2  0  0 zonefile.tab
    RBUF= 3 9 0 'LVFT' /
    RBUF= 3 10 1 'LKofT' /
    RBUF= 3 12 1 'LKEY' /
    RBUF= 3 14 1 'LZONE' /
    RBUF= 2 1 50 'N1' /
    RBUF= 2 2 864 'N2' /
    RBUF= 2 3 1 'N3' /
    RBUF= 2 5 1080 'N5' /
    RBUF= 2 6 96 'N24' /
    RBUF= 2 7 -1 'IIB' /
    RBUF= 2 8 999 'IC2' /
    RBUF= 2 9 999 'NRSET' /
    RBUF= 2 12 721 'JDISK' /
    RBUF= 2 15 0 'TUN_Flx15' /
    RBUF= 2 16 1 'KPREF' /
    RBUF= 2 17 52 'K4OUT' /
    RBUF= 2 18 0 'JBARE' /
    RBUF= 1 1 2.364E-01 'ALBEDO' /
    RBUF= 1 2 1.0000 'EMISS' /
    RBUF= 1 3 100.0000 'INERTIA' /
    RBUF= 1 4 1.178E-01 'COND2' /
    RBUF= 1 5 1252.7273 'DENS2' /
    RBUF= 1 6 1.0260 'PERIOD' /
    RBUF= 1 7 609.9060 'SPEC_HEAT' /
    RBUF= 1 8 1110.9091 'DENSITY' /
    RBUF= 1 12 546.0000 'PTOTAL' /
    RBUF= 1 15 180.0000 'TDEEP' /
    RBUF= 1 16 609.9060 'SpHeat2' /
    RBUF= 1 17 3.000E-01 'TAUD' /
    RBUF= 1 18 9.000E-01 'DUSTA' /
    RBUF= 1 19 2.200E-01 'TAURAT' /
    RBUF= 1 21 5.000E-01 'ARC2_G0' /
    RBUF= 1 23 0.000E+00 'SLOPE' /
    RBUF= 1 24 0.000E+00 'SLOAZI' /
    RBUF= 1 25 146.0000 'TFROST' /
    RBUF= 1 33 1.1500 'RLAY' /
    RBUF= 1 34 1.000E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 99.0000 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 1.9083 'DELJUL' /
    RBUF= 1 47 3.7110 'GRAV' /
    RBUF= 1 49 1.484E-02 'ConUp0' /
    RBUF= 1 50 3.406E-03 'ConUp1' /
    RBUF= 1 51 -5.288E-04 'ConUp2' /
    RBUF= 1 52 9.821E-05 'ConUp3' /
    RBUF= 1 53 1.184E-01 'ConLo0' /
    RBUF= 1 54 2.718E-02 'ConLo1' /
    RBUF= 1 55 -4.220E-03 'ConLo2' /
    RBUF= 1 56 7.838E-04 'ConLo3' /
    RBUF= 1 57 609.9060 'SphUp0' /
    RBUF= 1 58 214.2310 'SphUp1' /
    RBUF= 1 59 -40.9437 'SphUp2' /
    RBUF= 1 60 11.2575 'SphUp3' /
    RBUF= 1 61 609.9060 'SphLo0' /
    RBUF= 1 62 214.2310 'SphLo1' /
    RBUF= 1 63 -40.9437 'SphLo2' /
    RBUF= 1 64 11.2575 'SphLo3' /
    RBUF= 8 5 0 './krc.t52' /
   TCARD@280           5
  TCARD280  5  2  0  0 ./krc.t52
    RBUF= 0/
   IBD1:6           0           0           0           0           0           0
   FZONE,NZ,JPRT=zonefile.tab        1000           7
   return NZ=           49
   K,IH,LALCON=           1           3 F
   K,IH,LALCON=           2           4 F
   K,IH,LALCON=           3           0 F
   K,IH,LALCON=           4           0 F
   K,IH,LALCON=           5           0 F
   K,IH,LALCON=           6           0 F
   K,IH,LALCON=           7           0 F
   K,IH,LALCON=           8           0 F
   K,IH,LALCON=           9           0 F
   K,IH,LALCON=          10           0 F
   K,IH,LALCON=          11           0 F
   K,IH,LALCON=          12           0 F
   K,IH,LALCON=          13           0 F
   K,IH,LALCON=          14           0 F
   K,IH,LALCON=          15           0 F
   K,IH,LALCON=          16           0 F
   K,IH,LALCON=          17           0 F
   K,IH,LALCON=          18           0 F
   K,IH,LALCON=          19           0 F
   K,IH,LALCON=          20           0 F
   K,IH,LALCON=          21           0 F
   K,IH,LALCON=          22           0 F
   K,IH,LALCON=          23           0 F
   K,IH,LALCON=          24           0 F
   K,IH,LALCON=          25           0 F
   K,IH,LALCON=          26           0 F
   K,IH,LALCON=          27           0 F
   K,IH,LALCON=          28           0 F
   K,IH,LALCON=          29           0 F
   K,IH,LALCON=          30           0 F
   K,IH,LALCON=          31           0 F
   K,IH,LALCON=          32           0 F
   K,IH,LALCON=          33           0 F
   K,IH,LALCON=          34           0 F
   K,IH,LALCON=          35           0 F
   K,IH,LALCON=          36           0 F
   K,IH,LALCON=          37           0 F
   K,IH,LALCON=          38           0 F
   K,IH,LALCON=          39           0 F
   K,IH,LALCON=          40           0 F
   K,IH,LALCON=          41           0 F
   K,IH,LALCON=          42           0 F
   K,IH,LALCON=          43           0 F
   K,IH,LALCON=          44           0 F
   K,IH,LALCON=          45           0 F
   K,IH,LALCON=          46           0 F
   K,IH,LALCON=          47           0 F
   K,IH,LALCON=          48           0 F
   K,IH,LALCON=          49           0 F
   Case  1  DTIME: total, user, system=    0.0008    0.0005    0.0003
   Case  1  DTIME: total, user, system=    0.0008    0.0005    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251024T154210.722                 
  
  STDERR: 
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_thick_neg_n1/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 24 15:42:10           0 =IQ   errorfile = eLog20251024T154210.722                 
  .inp and .prt will be added to your input names
  Defaults:  input = krc , output = input
 ?* Input file name or / for default =krc
 ?* Print file name or / for default =krc
 TCARD notice: N4,MAXN4=           1          37
  RBUF= 8 25 0 'zonefile.tab' /
 TCARD@280          25
 LZONE,I= T          12
TCARD280 25  2  0  0 zonefile.tab
  RBUF= 3 9 0 'LVFT' /
  RBUF= 3 10 1 'LKofT' /
  RBUF= 3 12 1 'LKEY' /
  RBUF= 3 14 1 'LZONE' /
  RBUF= 2 1 50 'N1' /
  RBUF= 2 2 864 'N2' /
  RBUF= 2 3 1 'N3' /
  RBUF= 2 5 1080 'N5' /
  RBUF= 2 6 96 'N24' /
  RBUF= 2 7 -1 'IIB' /
  RBUF= 2 8 999 'IC2' /
  RBUF= 2 9 999 'NRSET' /
  RBUF= 2 12 721 'JDISK' /
  RBUF= 2 15 0 'TUN_Flx15' /
  RBUF= 2 16 1 'KPREF' /
  RBUF= 2 17 52 'K4OUT' /
  RBUF= 2 18 0 'JBARE' /
  RBUF= 1 1 2.364E-01 'ALBEDO' /
  RBUF= 1 2 1.0000 'EMISS' /
  RBUF= 1 3 100.0000 'INERTIA' /
  RBUF= 1 4 1.178E-01 'COND2' /
  RBUF= 1 5 1252.7273 'DENS2' /
  RBUF= 1 6 1.0260 'PERIOD' /
  RBUF= 1 7 609.9060 'SPEC_HEAT' /
  RBUF= 1 8 1110.9091 'DENSITY' /
  RBUF= 1 12 546.0000 'PTOTAL' /
  RBUF= 1 15 180.0000 'TDEEP' /
  RBUF= 1 16 609.9060 'SpHeat2' /
  RBUF= 1 17 3.000E-01 'TAUD' /
  RBUF= 1 18 9.000E-01 'DUSTA' /
  RBUF= 1 19 2.200E-01 'TAURAT' /
  RBUF= 1 21 5.000E-01 'ARC2_G0' /
  RBUF= 1 23 0.000E+00 'SLOPE' /
  RBUF= 1 24 0.000E+00 'SLOAZI' /
  RBUF= 1 25 146.0000 'TFROST' /
  RBUF= 1 33 1.1500 'RLAY' /
  RBUF= 1 34 1.000E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 99.0000 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 1.9083 'DELJUL' /
  RBUF= 1 47 3.7110 'GRAV' /
  RBUF= 1 49 1.484E-02 'ConUp0' /
  RBUF= 1 50 3.406E-03 'ConUp1' /
  RBUF= 1 51 -5.288E-04 'ConUp2' /
  RBUF= 1 52 9.821E-05 'ConUp3' /
  RBUF= 1 53 1.184E-01 'ConLo0' /
  RBUF= 1 54 2.718E-02 'ConLo1' /
  RBUF= 1 55 -4.220E-03 'ConLo2' /
  RBUF= 1 56 7.838E-04 'ConLo3' /
  RBUF= 1 57 609.9060 'SphUp0' /
  RBUF= 1 58 214.2310 'SphUp1' /
  RBUF= 1 59 -40.9437 'SphUp2' /
  RBUF= 1 60 11.2575 'SphUp3' /
  RBUF= 1 61 609.9060 'SphLo0' /
  RBUF= 1 62 214.2310 'SphLo1' /
  RBUF= 1 63 -40.9437 'SphLo2' /
  RBUF= 1 64 11.2575 'SphLo3' /
  RBUF= 8 5 0 './krc.t52' /
 TCARD@280           5
TCARD280  5  2  0  0 ./krc.t52
  RBUF= 0/
 IBD1:6           0           0           0           0           0           0
 FZONE,NZ,JPRT=zonefile.tab        1000           7
 return NZ=           49
 K,IH,LALCON=           1           3 F
 K,IH,LALCON=           2           4 F
 K,IH,LALCON=           3           0 F
 K,IH,LALCON=           4           0 F
 K,IH,LALCON=           5           0 F
 K,IH,LALCON=           6           0 F
 K,IH,LALCON=           7           0 F
 K,IH,LALCON=           8           0 F
 K,IH,LALCON=           9           0 F
 K,IH,LALCON=          10           0 F
 K,IH,LALCON=          11           0 F
 K,IH,LALCON=          12           0 F
 K,IH,LALCON=          13           0 F
 K,IH,LALCON=          14           0 F
 K,IH,LALCON=          15           0 F
 K,IH,LALCON=          16           0 F
 K,IH,LALCON=          17           0 F
 K,IH,LALCON=          18           0 F
 K,IH,LALCON=          19           0 F
 K,IH,LALCON=          20           0 F
 K,IH,LALCON=          21           0 F
 K,IH,LALCON=          22           0 F
 K,IH,LALCON=          23           0 F
 K,IH,LALCON=          24           0 F
 K,IH,LALCON=          25           0 F
 K,IH,LALCON=          26           0 F
 K,IH,LALCON=          27           0 F
 K,IH,LALCON=          28           0 F
 K,IH,LALCON=          29           0 F
 K,IH,LALCON=          30           0 F
 K,IH,LALCON=          31           0 F
 K,IH,LALCON=          32           0 F
 K,IH,LALCON=          33           0 F
 K,IH,LALCON=          34           0 F
 K,IH,LALCON=          35           0 F
 K,IH,LALCON=          36           0 F
 K,IH,LALCON=          37           0 F
 K,IH,LALCON=          38           0 F
 K,IH,LALCON=          39           0 F
 K,IH,LALCON=          40           0 F
 K,IH,LALCON=          41           0 F
 K,IH,LALCON=          42           0 F
 K,IH,LALCON=          43           0 F
 K,IH,LALCON=          44           0 F
 K,IH,LALCON=          45           0 F
 K,IH,LALCON=          46           0 F
 K,IH,LALCON=          47           0 F
 K,IH,LALCON=          48           0 F
 K,IH,LALCON=          49           0 F
 Case  1  DTIME: total, user, system=    0.0008    0.0005    0.0003
 Case  1  DTIME: total, user, system=    0.0008    0.0005    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251024T154210.722                 

STDERR: 
```

**Temperature Array Status:** Not compared (reason unknown)

### lzone with external file

**Failure Reason:** lzone with external file: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_lzone_ext_file/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 24 15:42:21           0 =IQ   errorfile = eLog20251024T154221.528                 
    .inp and .prt will be added to your input names
    Defaults:  input = krc , output = input
   ?* Input file name or / for default =krc
   ?* Print file name or / for default =krc
   TCARD notice: N4,MAXN4=           1          37
    RBUF= 8 25 0 'zonefile.tab' /
   TCARD@280          25
   LZONE,I= T          12
  TCARD280 25  2  0  0 zonefile.tab
    RBUF= 3 9 0 'LVFT' /
    RBUF= 3 10 1 'LKofT' /
    RBUF= 3 12 1 'LKEY' /
    RBUF= 3 14 1 'LZONE' /
    RBUF= 2 1 37 'N1' /
    RBUF= 2 2 864 'N2' /
    RBUF= 2 3 1 'N3' /
    RBUF= 2 5 1080 'N5' /
    RBUF= 2 6 96 'N24' /
    RBUF= 2 7 -1 'IIB' /
    RBUF= 2 8 999 'IC2' /
    RBUF= 2 9 999 'NRSET' /
    RBUF= 2 12 721 'JDISK' /
    RBUF= 2 15 0 'TUN_Flx15' /
    RBUF= 2 16 1 'KPREF' /
    RBUF= 2 17 52 'K4OUT' /
    RBUF= 2 18 0 'JBARE' /
    RBUF= 1 1 1.864E-01 'ALBEDO' /
    RBUF= 1 2 1.0000 'EMISS' /
    RBUF= 1 3 40.0000 'INERTIA' /
    RBUF= 1 4 2.455E-03 'COND2' /
    RBUF= 1 5 1068.3636 'DENS2' /
    RBUF= 1 6 1.0260 'PERIOD' /
    RBUF= 1 7 609.9060 'SPEC_HEAT' /
    RBUF= 1 8 1068.3636 'DENSITY' /
    RBUF= 1 12 546.0000 'PTOTAL' /
    RBUF= 1 15 180.0000 'TDEEP' /
    RBUF= 1 16 609.9060 'SpHeat2' /
    RBUF= 1 17 3.000E-01 'TAUD' /
    RBUF= 1 18 9.000E-01 'DUSTA' /
    RBUF= 1 19 2.200E-01 'TAURAT' /
    RBUF= 1 21 5.000E-01 'ARC2_G0' /
    RBUF= 1 23 0.000E+00 'SLOPE' /
    RBUF= 1 24 0.000E+00 'SLOAZI' /
    RBUF= 1 25 146.0000 'TFROST' /
    RBUF= 1 33 1.1500 'RLAY' /
    RBUF= 1 34 1.000E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 99.0000 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 1.9083 'DELJUL' /
    RBUF= 1 47 3.7110 'GRAV' /
    RBUF= 1 49 2.469E-03 'ConUp0' /
    RBUF= 1 50 5.666E-04 'ConUp1' /
    RBUF= 1 51 -8.797E-05 'ConUp2' /
    RBUF= 1 52 1.634E-05 'ConUp3' /
    RBUF= 1 53 2.469E-03 'ConLo0' /
    RBUF= 1 54 5.666E-04 'ConLo1' /
    RBUF= 1 55 -8.797E-05 'ConLo2' /
    RBUF= 1 56 1.634E-05 'ConLo3' /
    RBUF= 1 57 609.9060 'SphUp0' /
    RBUF= 1 58 214.2310 'SphUp1' /
    RBUF= 1 59 -40.9437 'SphUp2' /
    RBUF= 1 60 11.2575 'SphUp3' /
    RBUF= 1 61 609.9060 'SphLo0' /
    RBUF= 1 62 214.2310 'SphLo1' /
    RBUF= 1 63 -40.9437 'SphLo2' /
    RBUF= 1 64 11.2575 'SphLo3' /
    RBUF= 8 5 0 './krc.t52' /
   TCARD@280           5
  TCARD280  5  2  0  0 ./krc.t52
    RBUF= 0/
   IBD1:6           0           0           0           0           0           0
   FZONE,NZ,JPRT=zonefile.tab        1000           7
   return NZ=           37
   K,IH,LALCON=           1           0 T
   K,IH,LALCON=           2           0 T
   K,IH,LALCON=           3           0 T
   K,IH,LALCON=           4           0 T
   K,IH,LALCON=           5           0 T
   K,IH,LALCON=           6           0 T
   K,IH,LALCON=           7           0 T
   K,IH,LALCON=           8           0 T
   K,IH,LALCON=           9           0 T
   K,IH,LALCON=          10           0 T
   K,IH,LALCON=          11           0 T
   K,IH,LALCON=          12           0 T
   K,IH,LALCON=          13           0 T
   K,IH,LALCON=          14           0 T
   K,IH,LALCON=          15           0 T
   K,IH,LALCON=          16           0 T
   K,IH,LALCON=          17           0 T
   K,IH,LALCON=          18           0 T
   K,IH,LALCON=          19           0 T
   Case  1  DTIME: total, user, system=    0.0006    0.0003    0.0003
   Case  1  DTIME: total, user, system=    0.0006    0.0003    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251024T154221.528                 
  
  STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG
  
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_lzone_ext_file/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 24 15:42:21           0 =IQ   errorfile = eLog20251024T154221.528                 
  .inp and .prt will be added to your input names
  Defaults:  input = krc , output = input
 ?* Input file name or / for default =krc
 ?* Print file name or / for default =krc
 TCARD notice: N4,MAXN4=           1          37
  RBUF= 8 25 0 'zonefile.tab' /
 TCARD@280          25
 LZONE,I= T          12
TCARD280 25  2  0  0 zonefile.tab
  RBUF= 3 9 0 'LVFT' /
  RBUF= 3 10 1 'LKofT' /
  RBUF= 3 12 1 'LKEY' /
  RBUF= 3 14 1 'LZONE' /
  RBUF= 2 1 37 'N1' /
  RBUF= 2 2 864 'N2' /
  RBUF= 2 3 1 'N3' /
  RBUF= 2 5 1080 'N5' /
  RBUF= 2 6 96 'N24' /
  RBUF= 2 7 -1 'IIB' /
  RBUF= 2 8 999 'IC2' /
  RBUF= 2 9 999 'NRSET' /
  RBUF= 2 12 721 'JDISK' /
  RBUF= 2 15 0 'TUN_Flx15' /
  RBUF= 2 16 1 'KPREF' /
  RBUF= 2 17 52 'K4OUT' /
  RBUF= 2 18 0 'JBARE' /
  RBUF= 1 1 1.864E-01 'ALBEDO' /
  RBUF= 1 2 1.0000 'EMISS' /
  RBUF= 1 3 40.0000 'INERTIA' /
  RBUF= 1 4 2.455E-03 'COND2' /
  RBUF= 1 5 1068.3636 'DENS2' /
  RBUF= 1 6 1.0260 'PERIOD' /
  RBUF= 1 7 609.9060 'SPEC_HEAT' /
  RBUF= 1 8 1068.3636 'DENSITY' /
  RBUF= 1 12 546.0000 'PTOTAL' /
  RBUF= 1 15 180.0000 'TDEEP' /
  RBUF= 1 16 609.9060 'SpHeat2' /
  RBUF= 1 17 3.000E-01 'TAUD' /
  RBUF= 1 18 9.000E-01 'DUSTA' /
  RBUF= 1 19 2.200E-01 'TAURAT' /
  RBUF= 1 21 5.000E-01 'ARC2_G0' /
  RBUF= 1 23 0.000E+00 'SLOPE' /
  RBUF= 1 24 0.000E+00 'SLOAZI' /
  RBUF= 1 25 146.0000 'TFROST' /
  RBUF= 1 33 1.1500 'RLAY' /
  RBUF= 1 34 1.000E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 99.0000 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 1.9083 'DELJUL' /
  RBUF= 1 47 3.7110 'GRAV' /
  RBUF= 1 49 2.469E-03 'ConUp0' /
  RBUF= 1 50 5.666E-04 'ConUp1' /
  RBUF= 1 51 -8.797E-05 'ConUp2' /
  RBUF= 1 52 1.634E-05 'ConUp3' /
  RBUF= 1 53 2.469E-03 'ConLo0' /
  RBUF= 1 54 5.666E-04 'ConLo1' /
  RBUF= 1 55 -8.797E-05 'ConLo2' /
  RBUF= 1 56 1.634E-05 'ConLo3' /
  RBUF= 1 57 609.9060 'SphUp0' /
  RBUF= 1 58 214.2310 'SphUp1' /
  RBUF= 1 59 -40.9437 'SphUp2' /
  RBUF= 1 60 11.2575 'SphUp3' /
  RBUF= 1 61 609.9060 'SphLo0' /
  RBUF= 1 62 214.2310 'SphLo1' /
  RBUF= 1 63 -40.9437 'SphLo2' /
  RBUF= 1 64 11.2575 'SphLo3' /
  RBUF= 8 5 0 './krc.t52' /
 TCARD@280           5
TCARD280  5  2  0  0 ./krc.t52
  RBUF= 0/
 IBD1:6           0           0           0           0           0           0
 FZONE,NZ,JPRT=zonefile.tab        1000           7
 return NZ=           37
 K,IH,LALCON=           1           0 T
 K,IH,LALCON=           2           0 T
 K,IH,LALCON=           3           0 T
 K,IH,LALCON=           4           0 T
 K,IH,LALCON=           5           0 T
 K,IH,LALCON=           6           0 T
 K,IH,LALCON=           7           0 T
 K,IH,LALCON=           8           0 T
 K,IH,LALCON=           9           0 T
 K,IH,LALCON=          10           0 T
 K,IH,LALCON=          11           0 T
 K,IH,LALCON=          12           0 T
 K,IH,LALCON=          13           0 T
 K,IH,LALCON=          14           0 T
 K,IH,LALCON=          15           0 T
 K,IH,LALCON=          16           0 T
 K,IH,LALCON=          17           0 T
 K,IH,LALCON=          18           0 T
 K,IH,LALCON=          19           0 T
 Case  1  DTIME: total, user, system=    0.0006    0.0003    0.0003
 Case  1  DTIME: total, user, system=    0.0006    0.0003    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251024T154221.528                 

STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG

```

**Temperature Array Status:** Not compared (reason unknown)


## Success Statistics by Test Class

| Test Class | Passed | Total | Success Rate |
|------------|--------|-------|---------------|
| 1P-Halley | 0 | 1 | 0.0% |
| Basalt | 1 | 1 | 100.0% |
| Bennu | 3 | 4 | 75.0% |
| Ceres | 1 | 1 | 100.0% |
| DELLS | 1 | 1 | 100.0% |
| Delta | 1 | 1 | 100.0% |
| Eclipse | 0 | 1 | 0.0% |
| Europa | 9 | 12 | 75.0% |
| Exponential | 0 | 1 | 0.0% |
| GD | 1 | 1 | 100.0% |
| Gregorian | 1 | 1 | 100.0% |
| High | 1 | 1 | 100.0% |
| JD | 1 | 1 | 100.0% |
| Julian | 1 | 1 | 100.0% |
| Jupiter | 0 | 1 | 0.0% |
| Ls | 1 | 1 | 100.0% |
| Mars | 9 | 9 | 100.0% |
| Mode | 5 | 7 | 71.4% |
| Moon | 2 | 2 | 100.0% |
| Other | 2 | 2 | 100.0% |
| PFlux | 0 | 1 | 0.0% |
| PORB | 6 | 6 | 100.0% |
| PTOTAL<1 | 1 | 1 | 100.0% |
| Phobos | 3 | 5 | 60.0% |
| TPREDICT=False | 1 | 1 | 100.0% |
| Thick | 0 | 2 | 0.0% |
| Two-layer | 1 | 1 | 100.0% |
| User | 2 | 2 | 100.0% |
| Very | 2 | 2 | 100.0% |
| lzone | 0 | 1 | 0.0% |

---

**Testing Philosophy:**
1. **PRIMARY METRIC:** Input files must be identical (ensures Fortran receives same instructions)
2. **SECONDARY METRIC:** Temperature arrays must be nearly identical (validates output)
