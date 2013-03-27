	SUBROUTINE OBLIQ (QA,QB,CLIN,ODE,OBL,EB,EC,C,V)
C compute long, colat of planets pole and asc equinox of sun in 
C  ecliptic coords from long and colat in earth equatorial coords,
C  inclination and node of orbit and obliquity of earth
C all angular arguments in radians.
C	input:
C qa,qb = latitude & longtitude & of planets pole in earth equitorial
C clin,ode = inclination and node of planets orbit
C obl = obliquity of the earth
C	output:
C eb,ec = longtitude & co-latitude of planets pole in ecliptic coordinates
C c = angle from intersection of planets equator & ecliptic to planets
C   vernal equinox (intersection of planets equator & planets orbit)
C v = angle from node (intersection of planets orbit and ecliptic) to
C   planets vernal equinox.
CAlls: [sin,cos,tan,asin,acos,atan]
COded: h.kieffer 84may29  (note; older versions had input co-lat=qc)
	PI= 3.141592654
	PI2= PI/2.
	QC=PI2-QA
C find pole position in ecliptic coordinate from equitorial coordinates.
	A= QB+PI2
	CEC= COS(QC)* COS(OBL)+ SIN(QC)* SIN(OBL)* COS(A)
	EC= ACOS(CEC)
	SB= SIN(QC)* SIN(A)/ SIN(EC)
	CB= ( COS(QC)- COS(OBL)*CEC)/( SIN(OBL)* SIN(EC))
	B= ASIN(SB)
	EB= PI2-B
	IF(CB.LT.0.) EB=1.5*PI+B
C find angle along equator from ecliptic to orbit
	A= EB+PI2-ODE
	B= PI-EC
	TC= SIN(A)/( COS(A)* COS(B)+ SIN(B)/ TAN(CLIN))
	C= ATAN(TC)
C find angle along orbit from ecliptic to equator
	TV= SIN(A)/ (COS(A)*COS(CLIN) + SIN(CLIN)/TAN(B) )
	V= ATAN(TV)
	RETURN
	END
