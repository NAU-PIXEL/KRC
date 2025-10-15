# Input File Diff: Material Properties - k_style=Moon
**Test**: `k_style_moon`

**Changes**: 6 additions, 6 deletions

```diff
--- PyKRC/krc.inp+++ Davinci/krc.inp@@ -36,7 +36,7 @@    5.544402       0.000000       0.000000       686.9928       3397.977    
    24.62296       0.000000      -1.240317      0.4397026       0.000000    
    0.000000      0.3244966      0.8559125      0.4026360     -0.9458869    
-  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783
+  0.2936299      0.1381286       0.000000     -0.4256704      0.9048783    
 3 9 0 'LVFT' /
 3 10 1 'LKofT' /
 3 12 1 'LKEY' /
@@ -80,12 +80,12 @@ 1 42 1.9083 'DELJUL' /
 1 47 3.7110 'GRAV' /
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
@@ -95,6 +95,6 @@ 1 62 214.2310 'SphLo1' /
 1 63 -40.9437 'SphLo2' /
 1 64 11.2575 'SphLo3' /
-8 5 0 './krc.t52' /
+8 5 0 '/tmp/krc_integration_test_k_style_moon/davinci/outdata.bin.52' /
 0/
 0/

```
