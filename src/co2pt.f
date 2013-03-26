      REAL FUNCTION CO2PT (KODE, ARG2)
C_Titl  CO2PT:  CO2 pressure/temperature relation
      IMPLICIT NONE
C_Args
      INTEGER KODE            !in. 1=P-to-T  2=T-to-P
      REAL ARG2               !in. pressure (pascal) or temperature (kelvin)
CC REAL  CO2PT !func. temperature or pressure
C_Desc
C  Clausius-Clapeyron equation based on  MARS p 959
C  ln  P = a -  b/T   or  T = b/(a - ln  P)
C this relation is good to ~ 1% for 120-160 kelvin or .4 to 314 pascal
C_Hist  Hugh_Kieffer  97feb11
C 2010jan11 HK Go to implicit none
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      REAL A   /27.9546/  ! first  C-C coefficient for  CO2 (mb) mars p 959
      REAL B   /3182.48/  ! second  C-C coefficient for  CO2  (1/K)
      REAL OUT,P, T

      IF (KODE.EQ.1) THEN
        P=ARG2
        IF (P.LE.0.) P=1.  ! dumb insurance to avoid log failure
        OUT = B/(A-LOG(P))
      ELSE
        T=ARG2
        IF (T.LT.10.) T=10.
        OUT = EXP( A-B/T)
      ENDIF
      CO2PT = OUT
      RETURN
      END
