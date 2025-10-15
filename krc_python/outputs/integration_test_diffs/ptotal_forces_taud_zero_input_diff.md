# Input File Diff: Edge Cases - PTOTAL<1 forces TAUD=0
**Test**: `ptotal_forces_taud_zero`

**Changes**: 2 additions, 2 deletions

```diff
--- PyKRC/krc.inp+++ Davinci/krc.inp@@ -36,7 +36,7 @@    5.544402       0.000000       0.000000       686.9928       3397.977    
    24.62296       0.000000      -1.240317      0.4397026       0.000000    
    0.000000      0.3244966      0.8559125      0.4026360     -0.9458869    
-  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783
+  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783    
 3 9 0 'LVFT' /
 3 10 1 'LKofT' /
 3 12 1 'LKEY' /
@@ -95,6 +95,6 @@ 1 62 214.2310 'SphLo1' /
 1 63 -40.9437 'SphLo2' /
 1 64 11.2575 'SphLo3' /
-8 5 0 './krc.t52' /
+8 5 0 '/tmp/krc_integration_test_ptotal_forces_taud_zero/davinci/outdata.bin.52' /
 0/
 0/

```
