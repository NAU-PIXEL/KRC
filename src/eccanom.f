	REAL*4 FUNCTION ECCANOM (ECC,ANOM)
C_Title  ECCANOM iterative solution of  Kepler's equations for eccentric orbit.
	IMPLICIT NONE
C_Args
	REAL*4 ECC	!i. eccentricity of orbit
	REAL*4 ANOM	!i. mean anomoly <radians>
C 	REAL ECCANOM	!f. ECCENTRIC anomoly <radians>
C_Keys  ORBITAL  GEOMETRY
C_Desc
C anom=mean anomaly = 2*pi*(time_from_periapsis / period)
C e=eccentric anomaly
C  Keplers equation: e = anom + ecc * sin (e)
C  Initial guess is equal to 2 iterations plus an approximate curvature term
C   If has not converged in  NTRY trys, returns the last estimate + 2 pi.
C_Lims
	INTEGER*4 NTRY /20/	! maximum number of iterations
	REAL*4 ACCUR /0.5E-6/	! accuracy goal <radians>
C_Calls:  [SIN,COS,SQRT,AMOD]
C_Hist
C	89dec08  Hugh_H_Kieffer  U.S.G.S._Flagstaff,  Derived from  ORBIT
C 2005dec28 HK Change to use of IMPLICIT NONE.
C 2012mar30 HK Explicit *4 for word size 
C_End&___1_________2_________3_________4_________5_________6_________.72
	REAL*4 	TWOPI /6.28318531/
        INTEGER*4 I
        REAL*4 E,DANOM,EE,ACC
C initial guess
	E=ANOM+ ECC*SIN(ANOM) + ECC**2 *SIN(2.0*ANOM)/2.0
C iterate keplers equation
	DO  I=1,NTRY
	   DANOM= ANOM-E+ECC*SIN(E)
	   EE= E+DANOM/(1.0-ECC*COS(E))
	   ACC= ABS(E-EE)
	   IF (ACC.LE.ACCUR) GO TO 110
	   E= EE
	ENDDO
C	TYPE * 'WARNING, ECCANOM accuracy not reached for:',ANOM,ecc
	EE = EE+TWOPI		! add 2 pi as indication of non-conergence
110 	ECCANOM = EE
	RETURN
	END
