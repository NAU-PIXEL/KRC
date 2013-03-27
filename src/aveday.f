      REAL FUNCTION AVEDAY (SDEC,XLAT)
C_Titl  AVEDAY average daily exposure of surface to sunlight.
c   this is the average daily insolation, normalized to the solar flux at 
C    current distance from the sun.
      IMPLICIT NONE
C_Args
	REAL SDEC ! in. solar declination in degrees.
C			(the sub-solar latitude on the planet)
	REAL XLAT ! in. latitude on surface; in degrees. 
C
C_Desc formula from  WARD(1974),  J.G.R. vol. 79, page 3375;
C   equations 49 & 50 on page 3382.
C Input angles should be no greater than 90. degrees, this routine does
C   not check them.
C_Hist	86jul03  HHKieffer; revision of earlier  CFSOLAR.
C 1987sep13  HK incorporate the 1/pi term in this routine.
C 2010jan11  HK convert to implicit none
C 2012feb26  HK Remove unused variables
C_End
	REAL PI/3.1415926/
C        REAL R2D /57.29578/     ! degrees in one radian
        REAL D2R /1.7453293E-2/ ! radians pre degree
        REAL sdecr            ! solar declination in radians
        REAL xlatr            ! surface latitude, radians
        REAL eta              ! length of half-day, in radians
        REAL result

        SDECR=D2R*SDEC
        XLATR=D2R*XLAT
	IF (ABS(XLAT).LT.(90.-ABS(SDEC))) THEN
	  ETA=ACOS(-TAN(SDECr)*TAN(XLATr))	! day and night
	  RESULT = ( ETA*SIN(SDECr)*SIN(XLATr) +
     &		      SIN(ETA)*COS(SDECr)*COS(XLATr) )/PI
	ELSEIF (XLAT.GE.(90.-SDEC).OR.XLAT.LE.(-90.-SDEC)) THEN
CC***	  ETA=ACOS(-1.)				! polar day
	  RESULT = SIN(SDECr)*SIN(XLATr)
	ELSE
CC***	ELSEIF (XLAT.LE.(-90.+SDEC).OR.(XLAT.GE.90.+SDEC)) THEN
CC***	  ETA=ACOS(1.)			! polar night
	  RESULT = 0.
	ENDIF
	AVEDAY = RESULT	! transfer to caller
	RETURN
	END
