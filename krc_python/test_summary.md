# PyKRC Integration Test Summary

**Generated:** 2025-10-23 20:40:51

**Total Tests:** 72
**Passed:** 40 ✓
**Failed:** 32 ✗
**Success Rate:** 55.6%

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
| 9 | TPREDICT=False stability | ✗ FAIL | ✓ Identical | ⚠ Error | N/A |
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
| 28 | Mars T→TI (LKofT=F, TI~13) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 29 | Mars T→TI (LKofT=F, TI~100) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 30 | Mars T→TI (LKofT=T, TI~100) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 31 | Europa T→TI (LKofT=F) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 32 | Europa T→TI (LKofT=T) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 33 | Phobos T→TI (LKofT=F, TI~100) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 34 | Bennu T→TI (LKofT=F, TI~100) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 35 | Mars T→TI cross-LKofT (T→F) | ✗ FAIL | ✗ Different | ⚠ Error | N/A |
| 36 | Moon default | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 37 | Moon low INERTIA=55 | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 38 | Bennu default | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 39 | Ceres default | ✗ FAIL | ✗ Different | ✗ Different | 460.6274 |
| 40 | 2688_Halley comet | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 41 | Phobos with PFlux | ✗ FAIL | ✗ Different | ✗ Different | 18.5310 |
| 42 | Phobos without PFlux | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 43 | Phobos with PFlux Lon_Hr=12 | ✗ FAIL | ✗ Different | ✗ Different | 25.8579 |
| 44 | Jupiter with N1=40 | ✗ FAIL | ✗ Different | ✗ Different | 626.3835 |
| 45 | PORB Mars | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 46 | PORB Phobos | ✗ FAIL | ✗ Different | ✗ Different | 0.0668 |
| 47 | PORB Europa | ✗ FAIL | ✗ Different | ✗ Different | 585.0636 |
| 48 | PORB Bennu | ✗ FAIL | ✗ Different | ✗ Different | 1111.7565 |
| 49 | PORB Ceres | ✗ FAIL | ✗ Different | ✗ Different | 460.6274 |
| 50 | Europa N1=20 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 51 | Europa N1=30 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 52 | Bennu N1=25 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 53 | Bennu N1=33 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 54 | Europa N1=39 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 55 | Mars N1=50 custom FLAY/RLAY | ✗ FAIL | ✓ Identical | ⚠ Error | N/A |
| 56 | Europa N1=250 (high) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0003 |
| 57 | Mars N1=999 (extreme) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 58 | Mars N1=20 | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 59 | Mode 1: Standard constant properties | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 60 | Mode 2: Two-layer (thick=0.5) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 61 | Mode 3: Exponential profile (thick=-0.20) | ✓ PASS | ✓ Identical | ✓ Identical | 0.0000 |
| 62 | Mode 3: Steep exponential (thick=-0.05) | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
| 63 | Mode 3: Gradual exponential (thick=-1.0) | ✗ FAIL | ⚠ PyKRC Failed | ⚠ PyKRC Failed | N/A |
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

### TPREDICT=False stability

**Failure Reason:** TPREDICT=False stability: No float array comparison available (output file paths not provided)

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Eclipse Style 1.0 (daily)

**Failure Reason:** Eclipse Style 1.0 (daily): PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_type14_eclipse_daily/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 23 20:36:27           0 =IQ   errorfile = eLog20251023T203627.100                 
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
   Elog= eLog20251023T203627.100                 
  
  STDERR: 
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_type14_eclipse_daily/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:36:27           0 =IQ   errorfile = eLog20251023T203627.100                 
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
 Elog= eLog20251023T203627.100                 

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

### Mars T→TI (LKofT=F, TI~13)

**Failure Reason:** Mars T→TI (LKofT=F, TI~13): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 579
First few differences:
--- /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti13/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti13/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF'; Mars T→TI (LKofT=F, TI~13): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 579

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti13/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti13/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 2.364E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 52.0000 'INERTIA' /

 1 4 4.117E-03 'COND2' /

-1 5 1076.8727 'DENS2' /

+1 5 1076.8726 'DENS2' /

 1 6 1.0260 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1076.8727 'DENSITY' /

 1 12 546.0000 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -72,17 +67,11 @@
 1 23 0.000E+00 'SLOPE' /

 1 24 0.000E+00 'SLOAZI' /

 1 25 146.0000 'TFROST' /

-1 33 1.1500 'RLAY' /

-1 34 1.000E-01 
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Mars T→TI (LKofT=F, TI~100)

**Failure Reason:** Mars T→TI (LKofT=F, TI~100): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 579
First few differences:
--- /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti100/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPRE; Mars T→TI (LKofT=F, TI~100): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 579

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_mars_lkoft_false_ti100/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 2.364E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 52.0000 'INERTIA' /

 1 4 4.117E-03 'COND2' /

-1 5 1076.8727 'DENS2' /

+1 5 1076.8726 'DENS2' /

 1 6 1.0260 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1076.8727 'DENSITY' /

 1 12 546.0000 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -72,17 +67,11 @@
 1 23 0.000E+00 'SLOPE' /

 1 24 0.000E+00 'SLOAZI' /

 1 25 146.0000 'TFROST' /

-1 33 1.1500 'RLAY' /

-1 34 1.000E-0
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Mars T→TI (LKofT=T, TI~100)

**Failure Reason:** Mars T→TI (LKofT=T, TI~100): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 579
First few differences:
--- /tmp/krc_integration_test_point_mode_mars_lkoft_true_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_mars_lkoft_true_ti100/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 1 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF'; Mars T→TI (LKofT=T, TI~100): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 579

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_mars_lkoft_true_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_mars_lkoft_true_ti100/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 1 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 2.364E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 52.0000 'INERTIA' /

 1 4 4.117E-03 'COND2' /

-1 5 1076.8727 'DENS2' /

+1 5 1076.8726 'DENS2' /

 1 6 1.0260 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1076.8727 'DENSITY' /

 1 12 546.0000 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -72,17 +67,11 @@
 1 23 0.000E+00 'SLOPE' /

 1 24 0.000E+00 'SLOAZI' /

 1 25 146.0000 'TFROST' /

-1 33 1.1500 'RLAY' /

-1 34 1.000E-01 
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Europa T→TI (LKofT=F)

**Failure Reason:** Europa T→TI (LKofT=F): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 578
First few differences:
--- /tmp/krc_integration_test_point_mode_europa_lkoft_false/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_europa_lkoft_false/davinci/krc.inp
@@ -41,29 +41,24 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 39 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 288 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 6.700E-01 '; Europa T→TI (LKofT=F): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 578

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_europa_lkoft_false/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_europa_lkoft_false/davinci/krc.inp
@@ -41,29 +41,24 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 39 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 288 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 100.0000 'INERTIA' /

-1 4 2.859E-02 'COND2' /

-1 5 398.7780 'DENS2' /

+1 4 2.822E-02 'COND2' /

+1 5 404.0254 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 398.7780 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -71,28 +66,513 @@
 1 23 0.000E+00 
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Europa T→TI (LKofT=T)

**Failure Reason:** Europa T→TI (LKofT=T): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 578
First few differences:
--- /tmp/krc_integration_test_point_mode_europa_lkoft_true/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_europa_lkoft_true/davinci/krc.inp
@@ -41,29 +41,24 @@
 3 10 1 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 39 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 288 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 6.700E-01 'AL; Europa T→TI (LKofT=T): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 578

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_europa_lkoft_true/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_europa_lkoft_true/davinci/krc.inp
@@ -41,29 +41,24 @@
 3 10 1 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 39 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 288 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 100.0000 'INERTIA' /

-1 4 2.859E-02 'COND2' /

-1 5 398.7780 'DENS2' /

+1 4 2.822E-02 'COND2' /

+1 5 404.0254 'DENS2' /

 1 6 3.5500 'PERIOD' /

-1 7 877.0655 'SPEC_HEAT' /

-1 8 398.7780 'DENSITY' /

+1 7 877.0656 'SPEC_HEAT' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

-1 16 877.0655 'SpHeat2' /

+1 16 877.0656 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

 1 18 -999.0000 'DUSTA' /

 1 19 -999.0000 'TAURAT' /

@@ -71,28 +66,513 @@
 1 23 0.000E+00 'S
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Phobos T→TI (LKofT=F, TI~100)

**Failure Reason:** Phobos T→TI (LKofT=F, TI~100): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 578
First few differences:
--- /tmp/krc_integration_test_point_mode_phobos_lkoft_false_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_phobos_lkoft_false_ti100/davinci/krc.inp
@@ -41,26 +41,21 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 41 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 ; Phobos T→TI (LKofT=F, TI~100): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 578

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_phobos_lkoft_false_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_phobos_lkoft_false_ti100/davinci/krc.inp
@@ -41,26 +41,21 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 41 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 6.700E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 100.0000 'INERTIA' /

 1 4 1.476E-02 'COND2' /

 1 5 1110.9091 'DENS2' /

 1 6 3.189E-01 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1110.9091 'DENSITY' /

 1 12 0.000E+00 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -71,20 +66,14 @@
 1 23 0.000E+00 'SLOPE' /

 1 24 0.000E+00 'SLOAZI' /

 1 25 0.000E+00 'TFROST' /

-1 33 1.1500 'RLAY' /

-1 34 1.000E-01 'FLAY' /

 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Bennu T→TI (LKofT=F, TI~100)

**Failure Reason:** Bennu T→TI (LKofT=F, TI~100): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 97 vs 577
First few differences:
--- /tmp/krc_integration_test_point_mode_bennu_lkoft_false_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_bennu_lkoft_false_ti100/davinci/krc.inp
@@ -41,26 +41,21 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 33 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 0 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 5.0; Bennu T→TI (LKofT=F, TI~100): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 97
- Davinci lines: 577

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_bennu_lkoft_false_ti100/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_bennu_lkoft_false_ti100/davinci/krc.inp
@@ -41,26 +41,21 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 33 'N1' /

-2 2 864 'N2' /

 2 3 15 'N3' /

-2 5 1080 'N5' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 0 'IIB' /

-2 8 999 'IC2' /

 2 9 3 'NRSET' /

-2 12 721 'JDISK' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 5.000E-02 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 100.0000 'INERTIA' /

 1 4 1.476E-02 'COND2' /

 1 5 1110.9091 'DENS2' /

 1 6 1.790E-01 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1110.9091 'DENSITY' /

 1 12 1.000E-01 'PTOTAL' /

 1 16 609.9060 'SpHeat2' /

 1 17 0.000E+00 'TAUD' /

@@ -70,20 +65,14 @@
 1 23 0.000E+00 'SLOPE' /

 1 24 0.000E+00 'SLOAZI' /

 1 25 0.000E+00 'TFROST' /

-1 33 1.1500 'RLAY' /

-1 34 1.000E-01 'FLAY' /

 1 38 0.000E+00 'PhotoFunc' /

 1 39 1.000E-01 'GGT' /

 1 41
```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Mars T→TI cross-LKofT (T→F)

**Failure Reason:** Mars T→TI cross-LKofT (T→F): INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 99 vs 579
First few differences:
--- /tmp/krc_integration_test_point_mode_cross_lkoft_mars/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_cross_lkoft_mars/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF' /

 2 17 ; Mars T→TI cross-LKofT (T→F): No float array comparison available (output file paths not provided)

**Input File Differences:**
- PyKRC lines: 99
- Davinci lines: 579

**First Differences:**
```diff
--- /tmp/krc_integration_test_point_mode_cross_lkoft_mars/pykrc/krc.inp
+++ /tmp/krc_integration_test_point_mode_cross_lkoft_mars/davinci/krc.inp
@@ -41,27 +41,22 @@
 3 10 0 'LKofT' /

 3 12 1 'LKEY' /

 3 14 0 'LZONE' /

-2 1 37 'N1' /

-2 2 864 'N2' /

-2 3 1 'N3' /

-2 5 1080 'N5' /

+2 3 15 'N3' /

+2 5 135 'N5' /

 2 6 96 'N24' /

 2 7 -1 'IIB' /

-2 8 999 'IC2' /

-2 9 999 'NRSET' /

-2 12 721 'JDISK' /

+2 9 3 'NRSET' /

+2 12 91 'JDISK' /

 2 15 0 'TUN_Flx15' /

 2 16 1 'KPREF' /

 2 17 52 'K4OUT' /

 2 18 0 'JBARE' /

 1 1 2.364E-01 'ALBEDO' /

 1 2 1.0000 'EMISS' /

-1 3 52.0000 'INERTIA' /

 1 4 4.117E-03 'COND2' /

-1 5 1076.8727 'DENS2' /

+1 5 1076.8726 'DENS2' /

 1 6 1.0260 'PERIOD' /

 1 7 609.9060 'SPEC_HEAT' /

-1 8 1076.8727 'DENSITY' /

 1 12 546.0000 'PTOTAL' /

 1 15 180.0000 'TDEEP' /

 1 16 609.9060 'SpHeat2' /

@@ -72,17 +67,11 @@
 1 23 0.000E+00 'SLOPE' /

 1 24 0.000E+00 'SLOAZI' /

 1 25 146.0000 'TFROST' /

-1 33 1.1500 'RLAY' /

-1 34 1.000E-01 'FLAY' /


```

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Moon default

**Failure Reason:** Moon default: PyKRC failed - unsupported operand type(s) for *: 'NoneType' and 'float'
assert False

**PyKRC Error:**
```
unsupported operand type(s) for *: 'NoneType' and 'float'
```

**Temperature Array Status:** Not compared (reason unknown)

### Moon low INERTIA=55

**Failure Reason:** Moon low INERTIA=55: PyKRC failed - unsupported operand type(s) for *: 'NoneType' and 'float'
assert False

**PyKRC Error:**
```
unsupported operand type(s) for *: 'NoneType' and 'float'
```

**Temperature Array Status:** Not compared (reason unknown)

### Ceres default

**Failure Reason:** Ceres default: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_ceres_default/pykrc/krc.inp
+++ /tmp/krc_integration_test_ceres_default/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:18 IPLAN,TC= 301.0 0.00000 Ceres

-   301.0000       0.000000       1.400627      0.1848304       1.280374    

-  0.7900000E-01   2.767000      0.4090926       0.000000       1.029744    

-   5.078908       0.000000       0.000000   ; Ceres default: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 460.62741370667266 K
Max rel diff: 1.0
Mean abs diff: 31.72041281084386 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_ceres_default/pykrc/krc.inp
+++ /tmp/krc_integration_test_ceres_default/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:18 IPLAN,TC= 301.0 0.00000 Ceres

-   301.0000       0.000000       1.400627      0.1848304       1.280374    

-  0.7900000E-01   2.767000      0.4090926       0.000000       1.029744    

-   5.078908       0.000000       0.000000       1681.170      -2391626.    

-   9.074170       0.000000      0.5204842      0.6936836E-01   0.000000    

-   0.000000      0.8675785     -0.4961042     -0.3446924E-01  0.4973003    

-  0.8654920      0.6013424E-01  -0.000000     -0.6931274E-01  0.9975950    

+ 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars

+   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

+  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    

+   5
```

### 2688_Halley comet

**Failure Reason:** 2688_Halley comet: PyKRC failed - Body '2688_Halley' not found in small_bodies.hdf or comets.hdf
assert False

**PyKRC Error:**
```
Body '2688_Halley' not found in small_bodies.hdf or comets.hdf
```

**Temperature Array Status:** Not compared (reason unknown)

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

**Failure Reason:** Jupiter with N1=40: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_jupiter_high_n1/pykrc/krc.inp
+++ /tmp/krc_integration_test_jupiter_high_n1/davinci/krc.inp
@@ -30,13 +30,13 @@
    0.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:12 IPLAN,TC= 101.0 0.10000 Jupiter:Jupiter

-   101.0000      0.1000000       1.753958      0.2276282E-01  -1.496526    

-  0.4837299E-01   5.202875      0.4090926       0.000000       1.125660    

-   4.678459       0.000000    ; Jupiter with N1=40: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 626.3834858354187 K
Max rel diff: 0.9311426193309883
Mean abs diff: 47.514527797464936 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_jupiter_high_n1/pykrc/krc.inp
+++ /tmp/krc_integration_test_jupiter_high_n1/davinci/krc.inp
@@ -30,13 +30,13 @@
    0.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:12 IPLAN,TC= 101.0 0.10000 Jupiter:Jupiter

-   101.0000      0.1000000       1.753958      0.2276282E-01  -1.496526    

-  0.4837299E-01   5.202875      0.4090926       0.000000       1.125660    

-   4.678459       0.000000       0.000000       4334.739      -238.1847    

-   9.924920       0.000000       2.137560      0.5445530E-01   0.000000    

-   0.000000     -0.5369042     -0.8423927     -0.4591814E-01  0.8436432    

- -0.5361083     -0.2922283E-01   0.000000     -0.5442839E-01  0.9985177    

+ 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars

+   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

+  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229
```

### PORB Phobos

**Failure Reason:** PORB Phobos: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_porb_phobos/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_phobos/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:14 IPLAN,TC= 101.0 0.10000 Mars:Phobos

-   101.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

-  0.9340198E-01   1.523712      0.4090926       0.000000      0.9231727    

-   5.544373       0.000000       0.000000 ; PORB Phobos: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 0.066813504515153 K
Max rel diff: 0.01719050363872253
Mean abs diff: 0.0028408616088298 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_porb_phobos/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_phobos/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:14 IPLAN,TC= 101.0 0.10000 Mars:Phobos

-   101.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

-  0.9340198E-01   1.523712      0.4090926       0.000000      0.9231727    

-   5.544373       0.000000       0.000000       686.9928       3397.977    

-   7.653844       0.000000      -1.239967      0.4395194       0.000000    

-   0.000000      0.3248275      0.8558835      0.4024309     -0.9457733    

-  0.2939546      0.1382156       0.000000     -0.4255046      0.9049563    

+ 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars

+   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

+  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    

+  
```

### PORB Europa

**Failure Reason:** PORB Europa: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_porb_europa/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_europa/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:15 IPLAN,TC= 101.0 0.10000 Jupiter:Europa

-   101.0000      0.1000000       1.753958      0.2276282E-01  -1.496526    

-  0.4837299E-01   5.202875      0.4090926       0.000000       1.125917    

-   4.678863       0.000000       0.0000; PORB Europa: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 585.0635977797825 K
Max rel diff: 1.0
Mean abs diff: 45.59422838051967 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_porb_europa/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_europa/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:15 IPLAN,TC= 101.0 0.10000 Jupiter:Europa

-   101.0000      0.1000000       1.753958      0.2276282E-01  -1.496526    

-  0.4837299E-01   5.202875      0.4090926       0.000000       1.125917    

-   4.678863       0.000000       0.000000       4334.739      -238.1847    

-   85.22835       0.000000       2.136860      0.5414684E-01   0.000000    

-   0.000000     -0.5363136     -0.8427818     -0.4567862E-01  0.8440188    

- -0.5355276     -0.2902550E-01   0.000000     -0.5412039E-01  0.9985344    

+ 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars

+   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

+  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    


```

### PORB Bennu

**Failure Reason:** PORB Bennu: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_porb_bennu/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_bennu/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:17 IPLAN,TC= 301.0 0.00000 Bennu

-   301.0000       0.000000      0.3595378E-01  0.1052434       1.155757    

-  0.2037000       1.126000      0.4090926       0.000000      -1.504474    

-  -1.185079       0.000000       0.000000       43; PORB Bennu: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 1111.7564570379118 K
Max rel diff: 1.0
Mean abs diff: 35.22466465727277 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_porb_bennu/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_bennu/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:17 IPLAN,TC= 301.0 0.00000 Bennu

-   301.0000       0.000000      0.3595378E-01  0.1052434       1.155757    

-  0.2037000       1.126000      0.4090926       0.000000      -1.504474    

-  -1.185079       0.000000       0.000000       436.4208      -2396106.    

-   4.296061       0.000000       2.001663       2.565199       0.000000    

-   0.000000     -0.4176583      0.7618043     -0.4951926      0.9086042    

-  0.3501788     -0.2276253       0.000000     -0.5450037     -0.8384337    

+ 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars

+   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

+  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    

+   5.54440
```

### PORB Ceres

**Failure Reason:** PORB Ceres: INPUT FILES DO NOT MATCH (PRIMARY FAILURE)
Line count: 98 vs 98
First few differences:
--- /tmp/krc_integration_test_porb_ceres/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_ceres/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:18 IPLAN,TC= 301.0 0.00000 Ceres

-   301.0000       0.000000       1.400627      0.1848304       1.280374    

-  0.7900000E-01   2.767000      0.4090926       0.000000       1.029744    

-   5.078908       0.000000       0.000000       16; PORB Ceres: TEMPERATURE ARRAYS DIFFER (SECONDARY FAILURE)
Max abs diff: 460.62741370667266 K
Max rel diff: 1.0
Mean abs diff: 31.72041281084386 K

**Input File Differences:**
- PyKRC lines: 98
- Davinci lines: 98

**First Differences:**
```diff
--- /tmp/krc_integration_test_porb_ceres/pykrc/krc.inp
+++ /tmp/krc_integration_test_porb_ceres/davinci/krc.inp
@@ -30,13 +30,13 @@
   12.00

  _____7 _____7 _____7 Elevations: in 10F7.2 ____7 _____7 _____7 _____7

    0.00

-PORB:2014jun10 2024 Jun 27 13:04:18 IPLAN,TC= 301.0 0.00000 Ceres

-   301.0000       0.000000       1.400627      0.1848304       1.280374    

-  0.7900000E-01   2.767000      0.4090926       0.000000       1.029744    

-   5.078908       0.000000       0.000000       1681.170      -2391626.    

-   9.074170       0.000000      0.5204842      0.6936836E-01   0.000000    

-   0.000000      0.8675785     -0.4961042     -0.3446924E-01  0.4973003    

-  0.8654920      0.6013424E-01  -0.000000     -0.6931274E-01  0.9975950    

+ 2013 Jul 24 11:28:09=RUNTIME.  IPLAN AND TC= 104.0 0.10000 Mars:Mars

+   104.0000      0.1000000      0.8644665      0.3226901E-01  -1.281586    

+  0.9340198E-01   1.523712      0.4090926       0.000000      0.9229373    

+   5.54440
```

### Mars N1=50 custom FLAY/RLAY

**Failure Reason:** Mars N1=50 custom FLAY/RLAY: No float array comparison available (output file paths not provided)

**Temperature Array Comparison Error:**
No float array comparison available (output file paths not provided)

### Mode 3: Steep exponential (thick=-0.05)

**Failure Reason:** Mode 3: Steep exponential (thick=-0.05): PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode3_steep/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 23 20:40:19           0 =IQ   errorfile = eLog20251023T204019.122                 
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
    RBUF= 2 1 48 'N1' /
    RBUF= 2 2 384 'N2' /
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
    RBUF= 1 3 50.0000 'INERTIA' /
    RBUF= 1 4 1.982E-01 'COND2' /
    RBUF= 1 5 1323.6364 'DENS2' /
    RBUF= 1 6 1.0260 'PERIOD' /
    RBUF= 1 7 609.9060 'SPEC_HEAT' /
    RBUF= 1 8 1075.4545 'DENSITY' /
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
    RBUF= 1 34 1.500E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 99.0000 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 1.9083 'DELJUL' /
    RBUF= 1 47 3.7110 'GRAV' /
    RBUF= 1 49 3.832E-03 'ConUp0' /
    RBUF= 1 50 8.795E-04 'ConUp1' /
    RBUF= 1 51 -1.366E-04 'ConUp2' /
    RBUF= 1 52 2.536E-05 'ConUp3' /
    RBUF= 1 53 1.992E-01 'ConLo0' /
    RBUF= 1 54 4.573E-02 'ConLo1' /
    RBUF= 1 55 -7.101E-03 'ConLo2' /
    RBUF= 1 56 1.319E-03 'ConLo3' /
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
   return NZ=           47
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
   Case  1  DTIME: total, user, system=    0.0007    0.0004    0.0003
   Case  1  DTIME: total, user, system=    0.0007    0.0004    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251023T204019.122                 
  
  STDERR: 
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode3_steep/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:40:19           0 =IQ   errorfile = eLog20251023T204019.122                 
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
  RBUF= 2 1 48 'N1' /
  RBUF= 2 2 384 'N2' /
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
  RBUF= 1 3 50.0000 'INERTIA' /
  RBUF= 1 4 1.982E-01 'COND2' /
  RBUF= 1 5 1323.6364 'DENS2' /
  RBUF= 1 6 1.0260 'PERIOD' /
  RBUF= 1 7 609.9060 'SPEC_HEAT' /
  RBUF= 1 8 1075.4545 'DENSITY' /
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
  RBUF= 1 34 1.500E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 99.0000 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 1.9083 'DELJUL' /
  RBUF= 1 47 3.7110 'GRAV' /
  RBUF= 1 49 3.832E-03 'ConUp0' /
  RBUF= 1 50 8.795E-04 'ConUp1' /
  RBUF= 1 51 -1.366E-04 'ConUp2' /
  RBUF= 1 52 2.536E-05 'ConUp3' /
  RBUF= 1 53 1.992E-01 'ConLo0' /
  RBUF= 1 54 4.573E-02 'ConLo1' /
  RBUF= 1 55 -7.101E-03 'ConLo2' /
  RBUF= 1 56 1.319E-03 'ConLo3' /
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
 return NZ=           47
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
 Case  1  DTIME: total, user, system=    0.0007    0.0004    0.0003
 Case  1  DTIME: total, user, system=    0.0007    0.0004    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251023T204019.122                 

STDERR: 
```

**Temperature Array Status:** Not compared (reason unknown)

### Mode 3: Gradual exponential (thick=-1.0)

**Failure Reason:** Mode 3: Gradual exponential (thick=-1.0): PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode3_grad/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 23 20:40:21           0 =IQ   errorfile = eLog20251023T204021.482                 
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
    RBUF= 2 1 40 'N1' /
    RBUF= 2 2 384 'N2' /
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
    RBUF= 1 4 8.418E-02 'COND2' /
    RBUF= 1 5 1217.2727 'DENS2' /
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
    RBUF= 1 34 1.500E-01 'FLAY' /
    RBUF= 1 38 0.000E+00 'PhotoFunc' /
    RBUF= 1 39 99.0000 'GGT' /
    RBUF= 1 41 1.000E-01 'DJUL' /
    RBUF= 1 42 1.9083 'DELJUL' /
    RBUF= 1 47 3.7110 'GRAV' /
    RBUF= 1 49 1.484E-02 'ConUp0' /
    RBUF= 1 50 3.406E-03 'ConUp1' /
    RBUF= 1 51 -5.288E-04 'ConUp2' /
    RBUF= 1 52 9.821E-05 'ConUp3' /
    RBUF= 1 53 8.463E-02 'ConLo0' /
    RBUF= 1 54 1.942E-02 'ConLo1' /
    RBUF= 1 55 -3.016E-03 'ConLo2' /
    RBUF= 1 56 5.602E-04 'ConLo3' /
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
   return NZ=           39
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
   Case  1  DTIME: total, user, system=    0.0006    0.0004    0.0003
   Case  1  DTIME: total, user, system=    0.0006    0.0004    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251023T204021.482                 
  
  STDERR: 
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode3_grad/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:40:21           0 =IQ   errorfile = eLog20251023T204021.482                 
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
  RBUF= 2 1 40 'N1' /
  RBUF= 2 2 384 'N2' /
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
  RBUF= 1 4 8.418E-02 'COND2' /
  RBUF= 1 5 1217.2727 'DENS2' /
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
  RBUF= 1 34 1.500E-01 'FLAY' /
  RBUF= 1 38 0.000E+00 'PhotoFunc' /
  RBUF= 1 39 99.0000 'GGT' /
  RBUF= 1 41 1.000E-01 'DJUL' /
  RBUF= 1 42 1.9083 'DELJUL' /
  RBUF= 1 47 3.7110 'GRAV' /
  RBUF= 1 49 1.484E-02 'ConUp0' /
  RBUF= 1 50 3.406E-03 'ConUp1' /
  RBUF= 1 51 -5.288E-04 'ConUp2' /
  RBUF= 1 52 9.821E-05 'ConUp3' /
  RBUF= 1 53 8.463E-02 'ConLo0' /
  RBUF= 1 54 1.942E-02 'ConLo1' /
  RBUF= 1 55 -3.016E-03 'ConLo2' /
  RBUF= 1 56 5.602E-04 'ConLo3' /
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
 return NZ=           39
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
 Case  1  DTIME: total, user, system=    0.0006    0.0004    0.0003
 Case  1  DTIME: total, user, system=    0.0006    0.0004    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251023T204021.482                 

STDERR: 
```

**Temperature Array Status:** Not compared (reason unknown)

### Mode 4: Basic zone table

**Failure Reason:** Mode 4: Basic zone table: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_basic/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 23 20:40:23           0 =IQ   errorfile = eLog20251023T204023.980                 
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
   return NZ=           36
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
   Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
   Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251023T204023.980                 
  
  STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG
  
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_basic/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:40:23           0 =IQ   errorfile = eLog20251023T204023.980                 
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
 return NZ=           36
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
 Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
 Case  1  DTIME: total, user, system=    0.0005    0.0003    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251023T204023.980                 

STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG

```

**Temperature Array Status:** Not compared (reason unknown)

### Mode 4: Complex zone table

**Failure Reason:** Mode 4: Complex zone table: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_complex/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 23 20:40:25           0 =IQ   errorfile = eLog20251023T204025.440                 
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
    RBUF= 2 1 6 'N1' /
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
   return NZ=            5
   K,IH,LALCON=           1           0 T
   K,IH,LALCON=           2           0 T
   K,IH,LALCON=           3           0 T
   K,IH,LALCON=           4           0 T
   K,IH,LALCON=           5           0 T
   Case  1  DTIME: total, user, system=    0.0004    0.0001    0.0003
   Case  1  DTIME: total, user, system=    0.0004    0.0001    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.000
   Elog= eLog20251023T204025.440                 
  
  STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG
  
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_mode4_complex/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:40:25           0 =IQ   errorfile = eLog20251023T204025.440                 
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
  RBUF= 2 1 6 'N1' /
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
 return NZ=            5
 K,IH,LALCON=           1           0 T
 K,IH,LALCON=           2           0 T
 K,IH,LALCON=           3           0 T
 K,IH,LALCON=           4           0 T
 K,IH,LALCON=           5           0 T
 Case  1  DTIME: total, user, system=    0.0004    0.0001    0.0003
 Case  1  DTIME: total, user, system=    0.0004    0.0001    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.000
 Elog= eLog20251023T204025.440                 

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
   DAYTIM = 2025 Oct 23 20:40:39           0 =IQ   errorfile = eLog20251023T204039.262                 
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
   Case  1  DTIME: total, user, system=    0.0008    0.0004    0.0003
   Case  1  DTIME: total, user, system=    0.0008    0.0004    0.0003
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.001
   Elog= eLog20251023T204039.262                 
  
  STDERR: 
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_thick_neg_n1/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:40:39           0 =IQ   errorfile = eLog20251023T204039.262                 
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
 Case  1  DTIME: total, user, system=    0.0008    0.0004    0.0003
 Case  1  DTIME: total, user, system=    0.0008    0.0004    0.0003
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.001
 Elog= eLog20251023T204039.262                 

STDERR: 
```

**Temperature Array Status:** Not compared (reason unknown)

### lzone with external file

**Failure Reason:** lzone with external file: PyKRC failed - KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_lzone_ext_file/pykrc/krc.t52
  Return code: 0
  STDOUT:  -FILLD         428         214     6504915     3340616    22390021
   FILLD+         428         214     6504915     3340616    22390021
   This is KRC:  KRCv3.6.5            365
   DAYTIM = 2025 Oct 23 20:40:50           0 =IQ   errorfile = eLog20251023T204050.026                 
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
    RBUF= 2 1 6 'N1' /
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
   return NZ=            5
   K,IH,LALCON=           1           0 T
   K,IH,LALCON=           2           0 T
   K,IH,LALCON=           3           0 T
   K,IH,LALCON=           4           0 T
   K,IH,LALCON=           5           0 T
   Case  1  DTIME: total, user, system=    0.0003    0.0001    0.0002
   Case  1  DTIME: total, user, system=    0.0003    0.0001    0.0002
    RBUF= 0/
   DUMB,C=   772.00000000000000        773.00000000000000     
        END KRC   Total time [s]=      0.000
   Elog= eLog20251023T204050.026                 
  
  STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG
  
assert False

**PyKRC Error:**
```
KRC execution failed: KRC failed to produce output file: /tmp/krc_integration_test_lzone_ext_file/pykrc/krc.t52
Return code: 0
STDOUT:  -FILLD         428         214     6504915     3340616    22390021
 FILLD+         428         214     6504915     3340616    22390021
 This is KRC:  KRCv3.6.5            365
 DAYTIM = 2025 Oct 23 20:40:50           0 =IQ   errorfile = eLog20251023T204050.026                 
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
  RBUF= 2 1 6 'N1' /
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
 return NZ=            5
 K,IH,LALCON=           1           0 T
 K,IH,LALCON=           2           0 T
 K,IH,LALCON=           3           0 T
 K,IH,LALCON=           4           0 T
 K,IH,LALCON=           5           0 T
 Case  1  DTIME: total, user, system=    0.0003    0.0001    0.0002
 Case  1  DTIME: total, user, system=    0.0003    0.0001    0.0002
  RBUF= 0/
 DUMB,C=   772.00000000000000        773.00000000000000     
      END KRC   Total time [s]=      0.000
 Elog= eLog20251023T204050.026                 

STDERR: Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG

```

**Temperature Array Status:** Not compared (reason unknown)


## Success Statistics by Test Class

| Test Class | Passed | Total | Success Rate |
|------------|--------|-------|---------------|
| 2688_Halley | 0 | 1 | 0.0% |
| Basalt | 1 | 1 | 100.0% |
| Bennu | 3 | 4 | 75.0% |
| Ceres | 0 | 1 | 0.0% |
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
| Mars | 4 | 9 | 44.4% |
| Mode | 3 | 7 | 42.9% |
| Moon | 0 | 2 | 0.0% |
| Other | 2 | 2 | 100.0% |
| PFlux | 0 | 1 | 0.0% |
| PORB | 2 | 6 | 33.3% |
| PTOTAL<1 | 1 | 1 | 100.0% |
| Phobos | 2 | 5 | 40.0% |
| TPREDICT=False | 0 | 1 | 0.0% |
| Thick | 0 | 2 | 0.0% |
| Two-layer | 1 | 1 | 100.0% |
| User | 2 | 2 | 100.0% |
| Very | 2 | 2 | 100.0% |
| lzone | 0 | 1 | 0.0% |

---

**Testing Philosophy:**
1. **PRIMARY METRIC:** Input files must be identical (ensures Fortran receives same instructions)
2. **SECONDARY METRIC:** Temperature arrays must be nearly identical (validates output)
