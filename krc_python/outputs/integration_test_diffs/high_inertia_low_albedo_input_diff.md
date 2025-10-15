# Input File Diff: Comprehensive - High TI, Low albedo
**Test**: `high_inertia_low_albedo`

**Changes**: 4 additions, 4 deletions

```diff
--- PyKRC/krc.inp+++ Davinci/krc.inp@@ -36,7 +36,7 @@    5.544402       0.000000       0.000000       686.9928       3397.977    
    24.62296       0.000000      -1.240317      0.4397026       0.000000    
    0.000000      0.3244966      0.8559125      0.4026360     -0.9458869    
-  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783
+  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783    
 3 9 0 'LVFT' /
 3 10 1 'LKofT' /
 3 12 1 'LKEY' /
@@ -58,10 +58,10 @@ 1 2 1.0000 'EMISS' /
 1 3 1200.0000 'INERTIA' /
 1 4 1.2486 'COND2' /
-1 5 1890.9091 'DENS2' /
+1 5 1890.9092 'DENS2' /
 1 6 1.0260 'PERIOD' /
 1 7 609.9060 'SPEC_HEAT' /
-1 8 1890.9091 'DENSITY' /
+1 8 1890.9092 'DENSITY' /
 1 12 546.0000 'PTOTAL' /
 1 15 180.0000 'TDEEP' /
 1 16 609.9060 'SpHeat2' /
@@ -95,6 +95,6 @@ 1 62 214.2310 'SphLo1' /
 1 63 -40.9437 'SphLo2' /
 1 64 11.2575 'SphLo3' /
-8 5 0 './krc.t52' /
+8 5 0 '/tmp/krc_integration_test_high_inertia_low_albedo/davinci/outdata.bin.52' /
 0/
 0/

```
