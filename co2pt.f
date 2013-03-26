      REAL FUNCTION CO2PT (KODE, ARG2)
C_Titl  CO2PT:  CO2 pressure/temperature relation
C_Args
      INTEGER KODE               !in. 1 =  P-to-T  2=  T-to-P
      REAL*4 ARG2               !in. pressure (pascal) or temperature (kelvin)
CC REAL*4  CO2PT !func. temperature or pressure
C_Desc
C  Clausius-Clapeyron equation based on  MARS p 959
C  ln  P = a -  b/T   or  T = b/(a - ln  P)
C this relation is good to ~ 1% for 120-160 kelvin or .4 to 314 pascal
C_Hist  Hugh_Kieffer  97feb11
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      DATA A   /27.9546/  ! first  C-C coefficient for  CO2 (mb) mars p 959
      DATA B   /3182.48/  ! second  C-C coefficient for  CO2  (1/K)

      IF (KODE.EQ.1) THEN
        P=ARG2
        IF (P.LE.0.) THEN P=1.  ! dumb insurance to avoid log failure
        OUT = B/(A-LOG(P))
      ELSE
        T=ARG2
        IF (T.LT.10.) T=10.
        OUT = EXP( A-B/T)
      ENDIF
      CO2PT = OUT
      RETURN
      END
