	SUBROUTINE ORBIT (SMA,PERIOD,ECC,TPER, RADF,XX)
C_TITL  ORBIT  Compute radius and coordinates for elliptical orbit. DefPrec.
	IMPLICIT NONE
C_ARG
	REAL SMA        !I. semimajor axis, in units desired for  XX.
	REAL PERIOD 	!I. orbital period in same units as  TPER
	REAL ECC 	!I. eccentricity of orbit
	REAL TPER	!I. request time from periapsis, same units as  PERIOD
	REAL RADF	!O. current distance from focus, in units of  SMA.
C			   Will be set negative if eccentric anomoly 
C			   computation not fully accurate. 
	REAL XX(3)	!O. cartesian position of body in system with "z" toward
C			  positive pole of orbit, "x" toward periapsis
C_Calls  ECCANOM  [SIN,COS,SQRT,AMOD]
C_Hist 84may29  Hugh_H_Kieffer  From earlier versions ~69,~78
C 1989dec11  HK  Move determination of eccentric anomoly into  ECCANOM
C 2005dec28  HK  Change to use of IMPLICIT NONE
C 2012mar02  HK  Clean up names and case. no change to algorithm
C_END
	REAL TWOPI/6.283185307179586/
	REAL ANOM,EE,CE
	REAL ECCANOM		! function

	RADF=SMA		! Temporary so it could be negated as flag
	ANOM=AMOD(TWOPI*TPER/PERIOD,TWOPI)	! calc. mean anomoly
	EE=ECCANOM(ECC,ANOM)			! get eccentric anomoly
	IF (EE-ANOM.GT.4) RADF=-SMA  !  eccanom did not reach desired accuracy
	CE=COS(EE)
	XX(1)= SMA * (CE-ECC)
	XX(2)= SMA * SQRT(1.-ECC**2) *SIN(EE)
	XX(3)= 0.
	RADF=RADF*(1.-ECC*CE)	! Heliocentric radius
	RETURN
	END
