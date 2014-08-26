C_Titl  KSUBS8  A collection of R*8 routines with same name as R*4 versions
C Includes  AVEDAY  AVEYEAR  CO2PT  SIGMA 

      REAL*8 FUNCTION AVEDAY (SDEC,XLAT)
C_Titl  AVEDAY average daily exposure of surface to sunlight.
c   this is the average daily insolation, normalized to the solar flux at 
C    current distance from the sun.
      IMPLICIT NONE
C_Args
        REAL*8 SDEC ! in. solar declination in degrees.
C                       (the sub-solar latitude on the planet)
        REAL*8 XLAT ! in. latitude on surface; in degrees. 
C
C_Desc formula from  WARD(1974),  J.G.R. vol. 79, page 3375;
C   equations 49 & 50 on page 3382.
C Input angles should be no greater than 90. degrees, this routine does
C   not check them.
C_Hist  86jul03  HHKieffer; revision of earlier  CFSOLAR.
C 1987sep13  HK incorporate the 1/pi term in this routine.
C 2010jan11  HK convert to implicit none
C 2012feb26  HK Remove unused variables
C 2014mar10 HK Make  REAL*8  version  
C_End
        REAL*8 PI/3.141592653589793D0/
        REAL D2R /1.74532925199433D-2/ ! radians pre degree
        REAL*8 sdecr            ! solar declination in radians
        REAL*8 xlatr            ! surface latitude, radians
        REAL*8 eta              ! length of half-day, in radians
        REAL*8 result

        SDECR=D2R*SDEC
        XLATR=D2R*XLAT
        IF (ABS(XLAT).LT.(90.-ABS(SDEC))) THEN
          ETA=ACOS(-TAN(SDECr)*TAN(XLATr))      ! day and night
          RESULT = ( ETA*SIN(SDECr)*SIN(XLATr) +
     &                SIN(ETA)*COS(SDECr)*COS(XLATr) )/PI
        ELSEIF (XLAT.GE.(90.-SDEC).OR.XLAT.LE.(-90.-SDEC)) THEN
CC***     ETA=ACOS(-1.)                         ! polar day
          RESULT = SIN(SDECr)*SIN(XLATr)
        ELSE
CC***   ELSEIF (XLAT.LE.(-90.+SDEC).OR.(XLAT.GE.90.+SDEC)) THEN
CC***     ETA=ACOS(1.)                  ! polar night
          RESULT = 0.
        ENDIF
        AVEDAY = RESULT ! transfer to caller
        RETURN
        END

      REAL*8 FUNCTION AVEYEAR (BLIQ,XLAT)
C_Titl  AVEYEAR  Average annual exposure of surface to sunlight. 
C  This is the average annual insolation, normalized to mean annual 
C solar flux over the orbit
      IMPLICIT NONE
C_Args
        REAL*8 BLIQ ! in. Obliquity in degrees.
C                       (the maximum sub-solar latitude on the planet)
        REAL*8 XLAT ! in. latitude on surface; in degrees. 
C
C_Desc formula from  WARD in  MARS p309 Eq 12
C Input angles should be no greater magnitude than 90. degrees, 
C  this routine does  not check them. But does force to positive.
C Approximate by numerical integration
C [Formula for mean annual insolation is Eq 11 p 308. 
C Note, near poles, result is sin(obliquity)/pi
C_Hist 2012mar09   Hugh_Kieffer
C 2014mar10 HK Make  REAL*8  version  
C_End
        REAL*8 PI /3.141592653589793D0/

        INTEGER NP /360/    ! number of integration points
        INTEGER I ! looping
        REAL*8 D2R,DELA,THETA,CON1,CON2,DELP,SINP,SUM

        D2R=PI/180.D0             ! math constant: Degrees to radians

        DELA=D2R*DABS(XLAT)       ! del = latitude in radian
        THETA=D2R*DABS(BLIQ)     ! theta= oblquity in radian

        CON1=DSIN(DELA)*DCOS(THETA) ! constant in integral
        CON2=DCOS(DELA)*DSIN(THETA) ! constant in integral
        DELP=2.*PI/NP           ! increment in psi
        
        SUM=0.                  ! summation
        DO I=0,NP               ! crude numerical integration
           SINP=DSIN(I*DELP)     ! sin psi
           SUM=SUM+DSQRT(1.-(CON1-CON2*SINP)**2)
        ENDDO

C 1/(2 pi^2) * sum*delp=  1/(2 pi^2) * sum*(2 pi/N)= sum/(N pi)
       
        AVEYEAR= SUM/(NP*PI)    ! transfer to caller
        RETURN
        END



      REAL*8 FUNCTION CO2PT (KODE, ARG2)
C_Titl  CO2PT:  CO2 pressure/temperature relation
      IMPLICIT NONE
C_Args
      INTEGER KODE            !in. 1=P-to-T  2=T-to-P
      REAL*8 ARG2               !in. pressure (pascal) or temperature (kelvin)
CC REAL  CO2PT !func. temperature or pressure
C_Desc
C  Clausius-Clapeyron equation based on  MARS p 959
C  ln  P = a -  b/T   or  T = b/(a - ln  P)
C this relation is good to ~ 1% for 120-160 kelvin or .4 to 314 pascal
C_Hist  Hugh_Kieffer  97feb11
C 2010jan11 HK Go to implicit none
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      REAL*8 A   /27.9546D0/  ! first  C-C coefficient for  CO2 (mb) mars p 959
      REAL*8 B   /3182.48D0/  ! second  C-C coefficient for  CO2  (1/K)
      REAL*8 OUT,P, T

      IF (KODE.EQ.1) THEN
        P=ARG2
        IF (P.LE.0.) P=1.D0  ! dumb insurance to avoid log failure
        OUT = B/(A-DLOG(P))
      ELSE
        T=ARG2
        IF (T.LT.10.D0) T=10.D0
        OUT = DEXP( A-B/T)
      ENDIF
      CO2PT = OUT
      RETURN
      END



      SUBROUTINE SIGMA (BUF,N, AVE,SIG)
C_TITLE SIGMA Computes mean and standard deviation of real array 
      IMPLICIT NONE
C_ARGS
      REAL*8 BUF(*)             ! [In] array
      INTEGER N                 ! [In]  number of items in BUF
      REAL*8 AVE                ! [Out] average value of input array
      REAL*8 SIG                ! [Out] standard deviation of input array
C               will be 0. if  N less than 2.
C_KEYS  MATH STATISTICS 
C_DESC Sums the values and value_squared for entire array, then computes 
C   mean and standard deviation from these sums.
C_CALLS 0
C_LIMS  Could overflow REAL sum_squares 
C_HIST  85Jan09  Hugh_H_Kieffer  U.S.G.S. Flagstaff  ORIGINAL  VERSION
C       86Dec06  HHK Revised to handle  N=1 properly (adapted from
C                       byte version). 
C_END
      REAL*8 P,S,SS,X
      INTEGER I
      SIG=0.                    ! set here in case N<2
      IF (N.LT.1) THEN          ! not enough points 
        AVE=0.
        RETURN
      ENDIF
      P=N                       ! need real number_of_points for formulas
      S=0.                      ! initialise the sums
      SS=0.
      DO I=1,N
        X=BUF(I)
        S=S+X                   ! sum the values
        SS=SS+X*X               ! sum the values-squared
      ENDDO
      AVE=S/P                   ! average
      IF (N.GT.1) SIG=DSQRT((SS-2.*AVE*S+P*AVE*AVE)/(P-1.)) ! standard dev.
      RETURN
      END
