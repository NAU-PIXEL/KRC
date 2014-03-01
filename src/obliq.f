	SUBROUTINE OBLIQ (QA,QB,CLIN,ODE,OBL, EB,EC,ARGU,ARGV)
C_Titl  OBLIV  Convert body pole from J2000 to ecliptic, and two angles
C_Args           All are in radians.
C  QA    In. Latitude (Declination) of planets pole in J2000. Radian
C  QB    In. Longtitude (RightAscension) " " "
C  CLIN  In. Inclination of planets orbit. Radian
C  CODE  In. Node of planets orbit. Radian
C  OBL   In. Obliquity of the Earth (angle between J2000 and Ecliptic). Radian
C  EB   Out. Longtitude of planets pole in ecliptic coordinates. Radian
C  EC   Out. Co-latitude of planets pole in ecliptic coordinates. Radian
C  ARGU Out. Angle from intersection of planets equator & ecliptic to planets
C            vernal equinox (intersection of planets equator & planets orbit)
C  ARGV Out.  Angle from node (intersection of planets orbit and ecliptic) to
C            planets vernal equinox.
C_Desc
C Compute long, colat of planets pole and asc equinox of Sun in 
C  ecliptic coords from long and colat in earth equatorial coords (J2000),
C  inclination and node of orbit and obliquity of earth
C Vernal equinox is along: spin-pole  cross  orbit-pole
C "A:B"   means the line between points A and B on the geometry sphere
C "A:B:C" means the spherical angle at B between lines to A and to C
Calls: [sin,cos,tan,asin,acos,atan]
C_Hist: 1984may29  Hugh Kieffer  (note; older versions had input co-lat=qc)
C 2013jun15 HK Revise comments and some variable names
	PI= 3.141592654
	PI2= PI/2.
	QC=PI2-QA ! colat of pole in J2000
C Find pole position in ecliptic coordinate from equitorial coordinates.
C 1 Use lawCosines with corners at ZA = J2000 Z axis
C                                  ZE = ecliptic system Z axis
C                                  ZB = body spinAxis
	A= QB+PI2		! angle ZE:ZA:ZB
	CEC= COS(QC)* COS(OBL)+ SIN(QC)* SIN(OBL)* COS(A) ! cos ZE:ZB
	EC= ACOS(CEC)		! colat of spin axis in Ecliptic
C Law of sines:  sin ZE:ZB / sin ZE:ZA:ZB = sin ZA:ZB / sin ZA:ZE:ZB 
	SB= SIN(QC)* SIN(A)/ SIN(EC) ! sine of ZA:ZE:ZB
	B= ASIN(SB) ! ZA:ZE:ZB = Node in ecliptic + pi/2


	CB= ( COS(QC)- COS(OBL)*CEC)/( SIN(OBL)* SIN(EC))
	EB= PI2-B		! spinAxis RA in Ecliptic
	IF(CB.LT.0.) EB=1.5*PI+B
C find angle along equator from ecliptic to orbit
	A= EB+PI2-ODE
	B= PI-EC
	TC= SIN(A)/( COS(A)* COS(B)+ SIN(B)/ TAN(CLIN))
	ARGU= ATAN(TC)
C find angle along orbit from ecliptic to equator
	TV= SIN(A)/ (COS(A)*COS(CLIN) + SIN(CLIN)/TAN(B) )
	ARGV= ATAN(TV)
	RETURN
	END
