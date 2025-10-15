# Input File Diff: Parameter Resolution - DELLS blocks DELJUL
**Test**: `dells_blocks_deljul`

**Changes**: 5 additions, 5 deletions

```diff
--- PyKRC/krc.inp+++ Davinci/krc.inp@@ -36,19 +36,19 @@    5.544402       0.000000       0.000000       686.9928       3397.977    
    24.62296       0.000000      -1.240317      0.4397026       0.000000    
    0.000000      0.3244966      0.8559125      0.4026360     -0.9458869    
-  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783
+  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783    
 3 9 0 'LVFT' /
 3 10 1 'LKofT' /
 3 12 1 'LKEY' /
 3 14 0 'LZONE' /
 2 1 37 'N1' /
 2 2 864 'N2' /
-2 3 1 'N3' /
+2 3 15 'N3' /
 2 5 540 'N5' /
 2 6 96 'N24' /
 2 7 -1 'IIB' /
 2 8 999 'IC2' /
-2 9 999 'NRSET' /
+2 9 3 'NRSET' /
 2 12 361 'JDISK' /
 2 15 0 'TUN_Flx15' /
 2 16 1 'KPREF' /
@@ -75,7 +75,7 @@ 1 33 1.1500 'RLAY' /
 1 34 1.000E-01 'FLAY' /
 1 38 0.000E+00 'PhotoFunc' /
-1 39 99.0000 'GGT' /
+1 39 1.000E-01 'GGT' /
 1 41 1.000E-01 'DJUL' /
 1 42 3.8166 'DELJUL' /
 1 47 3.7110 'GRAV' /
@@ -95,6 +95,6 @@ 1 62 214.2310 'SphLo1' /
 1 63 -40.9437 'SphLo2' /
 1 64 11.2575 'SphLo3' /
-8 5 0 './krc.t52' /
+8 5 0 '/tmp/krc_integration_test_dells_blocks_deljul/davinci/outdata.bin.52' /
 0/
 0/

```
