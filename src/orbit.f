	SUBROUTINE ORBIT (A,PERIOD,ECC,TPER, R,XX)
C_TIT  ORBIT compute radius and coordinates FOR ELLIPTICAL ORBIT
C_ARG
	REAL*4 A	!I. semimajor axis, in units desired for  XX.
	REAL*4 PERIOD 	!I. orbital period in same units as  TPER
	REAL*4 ECC 	!I. eccentricity of orbit
	REAL*4 TPER	!I. request time from periapsis, same units as PERIOD
	REAL*4 R	!O. current distance from focus, in units of  A.
C			   will be set negative if ECCENTRIC ANOMOLY 
C			   COMPUTATION NOT FULLY ACCURATE. 
	REAL*4 XX	!O. cartesian position of body in system with "z" toward
C			  positive pole of orbit, "x" toward periapsis
C_Calls  ECCANOM  [SIN,COS,SQRT,AMOD]
C_HIST  84MAY29  HUGH_H_KIEFFER FROM EARLIER VERSIONS ~69,~78
C	89DEC11  HHK MOVE DETERMINATION OF ECCENTRIC ANOMOLY INTO   ECCANOM
C_END
	DIMENSION XX(3)
	DATA TWOPI/6.28318531/

	R=A
	ANOM=AMOD(TWOPI*TPER/PERIOD,TWOPI)	! CALC. MEAN ANOMOLY
	EE=ECCANOM(ECC,ANOM)			! GET ECCENTRIC ANOMOLY
	IF (EE-ANOM.GT.4) R=-A	!  ECCANOM DID NOT REACH DESIRED ACCURACY
	CE=COS(EE)
	XX(1)= A * (CE-ECC)
	XX(2)= A * SQRT(1.-ECC**2) *SIN(EE)
	XX(3)= 0.
	R=R*(1.-ECC*CE)
	RETURN
	END
