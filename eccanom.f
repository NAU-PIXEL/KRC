
	REAL FUNCTION ECCANOM (ECC,ANOM)
C_Title  ECCANOM iterative solution of keplers equations for eccentric orbit
C_Args
	REAL*4 ECC	!i. eccentricity of orbit
	REAL*4 ANOM	!i. mean anomoly <radians>
CC	REAL*4 ECCANOM	!f. ECCENTRIC anomoly <radians>
C_Keys  ORBITAL  GEOMETRY
C_Desc
C anom=mean anomaly = 2*pi*(time_from_periapsis / period)
C e=eccentric anomaly
C keplers equation: e = anom + ecc * sin (e)
C initial guess is equal to 2 iterations plus an approximate curvature term
C   IF HAS NOT CONVERGED IN  NTRY TRYS, RETURNS THE LAST ESTIMATE + 2 PI.
C_Lims
	DATA NTRY /20/		! maximum number of iterations
	DATA ACCUR /0.5E-6/	! accuracy goal <radians>
C_Calls:  [SIN,COS,SQRT,AMOD]
C_Hist
C	89dec08  Hugh_H_Kieffer  U.S.G.S._Flagstaff,  DERIVED from  ORBIT
C_End&___1_________2_________3_________4_________5_________6_________.72
	DATA TWOPI /6.28318531/
C initial guess
	E=ANOM+ ECC*SIN(ANOM) + ECC**2*SIN(2.0*ANOM)/2.0
C iterate keplers equation
	DO  I=1,NTRY
	   DANOM= ANOM-E+ECC*SIN(E)
	   EE= E+DANOM/(1.0-ECC*COS(E))
	   ACC= ABS(E-EE)
	   IF (ACC.LE.ACCUR) GO TO 110
	   E= EE
	   ENDDO
C	TYPE * 'WARNING, ECCANOM accuracy not reached for:',ANOM,ecc
	EE = EE+TWOPI
110 	ECCANOM = EE
	RETURN
	END
