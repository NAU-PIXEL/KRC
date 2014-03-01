	SUBROUTINE DEDING1 (OMEGA,G0,AS,AINC,TAU, BOND,ABS,RI)
C_Titl  DEDING1 delta-eddington solution for single homogeneous layer
C_Arg
	REAL OMEGA	![i] dust single scattering albedo
	REAL G0 	![i] dust asymmetry parameter
	REAL AS 	![i] surface albedo
	REAL AINC	![i] solar zenith angle in degrees
	REAL TAU	![i] dust vertical opacity
	REAL BOND	![o] planetary (atm plus surface system) albedo
	REAL ABS	![o] fraction of irradiance absorbed by surface 
	REAL RI(2,2)	![o] radiances: 
C		(1,=  I0 = isotropic   (2, =  I1 = assymetric
C		,1)= at top of atmosphere  ,2) = at bottom of atm
C_Desc
C  Assumes unit solar irradiance at top of atmosphere. e.g.,  pi*F = 1.
C  {NOTE: this is different than older  DEDING, which used   pi*f = 1/cos_i .}
C  Eddington approximation  I(t,u) =  I0(t) +  u*I1(t).  SW eq.2,
C   where  t = tau = extinction opacity,  and u = cosine(angle from zenith).
C  The net flux is  F =  pi*[I0+-(2/3)*I1]   + is down, - is up.  SW eq. 8.
C  For large tau, which would exceed machine single-precision range,
C   this routine returns the semi-infinite result.
C  Based upon:
C.  JWW =  The  Delta-Eddington  Approximation for radiative flux transfer.
C.  J.H.Joseph,W.J.Wiscombe,J.A.Weinman.  J.Atm.Sci vol.33,2453-2459 (1976)
C.  SW = the transfer of solar irradiance through inhomgeneous turbid
C    atmospheres evaluated by  Eddingtons approximation.
C.  E.P.Shettle,J.A.Weinman.  J.Atm.Sci vol.27,1048-1055 (1970)
C  All equation numbers are from  SW unless otherwise noted.
C_Limitations
C Limits  OMEGA to .999999 and  |G0| to .99 
C_Calls  None
C_Hist	88may05  HKieffer derive from  DEDING.
C 99apr15  HHK bring  EXPMAX &  VLARGE well within single precision range
C       and test for extreme  G0. 
C  Add conservative scattering case, but it does not give similiar answers.
C  Seems to works consistantly for  OMEGA up to .999999, so use this as limit
C  After messing around with limiting cases for  G0 near +- 1, decide
C to simply limit |G0| to .99 
C_End
	DATA PI/3.1415926/,TT/.6666666666/	! pi, 2/3
	DATA EXPMAX,VLARGE/46.05,1.0E20/	! large exponent & real number

C delta-eddington transformation
	F = G0**2		!  JWW eq. 5
	OMF=1.-F
	IF (OMF.GT. 1.0E-4) THEN ! normal condition
	  G = (G0-F)/OMF	!  JWW eq. 2b
	ELSE			!   G blows up; limit assymetry to +/- 0.99
	  F=.99**2
	  OMF=1.0-F
	  IF (G0 .GT. 0.) THEN	!  near +1.
	    G=(0.99-F)/OMF
C       	  G=0.5			! limit for pure forward scattering
	  ELSE			! near -1.
	    G=(-0.99-F)/OMF
C       	  G=-2.E5		! limit is -1/epsilon  epsilon=1+G0
	  ENDIF
	ENDIF
	
	COSI = COS (AINC*1.745329E-2)	! mu_0
	A0=MIN(OMEGA,.999999)	! avoid 0 determinant at omega=1.

	OMFA0=1.-F*A0
	T1 = OMFA0*TAU		!  JWW eq. 13
	EMTC = 0.		! exp -tau_prime/mu     set=0
	X = T1/COSI		!   if outside machine range
	IF (X.LT.EXPMAX) EMTC = EXP(-X) ! exp (- tau/mu_0)

	IF (OMFA0.GT. 1.0E-7) THEN !  ALWAYS_TRUE  Non-conservative case -----

	A = OMF*A0/OMFA0        !  JWW eq. 14
	OMA = 1.-A		! 1 - scaled albedo (used in 4 places)

C eddington 2-stream single homogeneous layer
C note. integral from -1 to +1 of   u*(x*u) du   is 2/3 x.  u = cos theta
	CAP = SQRT (3.*OMA*(1.-A*G))		! eq. 12b+1   ->0 as  A0->0
	P = SQRT (3.*OMA/(1.-A*G))		! eq. 12b+2   ->0 as  A0->0
C if  P=0, then  V1=V2 &  V4=V5 &  VDET=0
C	if (p .lt. 1.e-4) P=1.e-4 ! avoid pole

	F0 = 1./PI				! unit irradiance
	FACTOR =3.*A*F0*COSI / (4.*(1.-(CAP*COSI)**2)) ! used for next 2 lines
	ALPHA = FACTOR* COSI*(1.+G*OMA)		! eq. 12b+3
	BETA =  FACTOR* (1.+3.*G*OMA*COSI**2)	! eq. 12b+4

c	write(*,*) 'G,EMTC,OMFA0=',g,emtc,omfa0
c	write(*,*) 'OMA,P=',oma,p
C set up simultaneous equations
C set up simultaneous equations
CC [ V1 V2 ] [C1] = [V3]
CC [ V4 V5 ] [C2]   [V6]
	V1 = 1.+TT*P		! coefficients of eq. 13
	V2 = 1.-TT*P
	V3 = ALPHA+TT*BETA
	CAPT = CAP*T1
	IF (CAPT.LT.EXPMAX) THEN	! within machine floating-point range
		EPKT = EXP(+CAPT)
		EMKT = EXP(-CAPT)	!  v4,v5,v6 are coeff. of eq. 14
		V4 = (1.-AS-TT*(1.+AS)*P)*EMKT	! can get small
		V5 = (1.-AS+TT*(1.+AS)*P)*EPKT	! can get large
		V6 = ((1.-AS)*ALPHA-TT*(1.+AS)*BETA+AS*COSI*F0)*EMTC
		VDET = V1*V5 - V4*V2		! determinant of eq. 13 and 14
C determinant goes to  V1*V5 as tau gets large
		C1 = (V3*V5-V6*V2)/VDET
		C2 = (V1*V6-V4*V3)/VDET
c		write(*,*) v1,v2,v3
c		write(*,*) v4,v5,v6
c		write(*,*) vdet,c1,c2

	    ELSE			! output semi-infinite case
		EMKT = 0.
		C1 = V3/V1
		C2 = 0.
	    ENDIF
C
	RI(1,1) = C1+C2-ALPHA			  !  I0_top	eq. 12a@tau=0
	RI(2,1) = P*(C1-C2)-BETA		  !  I1_top	eq. 12b@tau=0
	RI(1,2) = C1*EMKT + C2*EPKT - ALPHA*EMTC  !  I0_bot	eq. 12a
	RI(2,2) = P*(C1*EMKT-C2*EPKT) - BETA*EMTC !  I1_bot	eq. 12b

	else !  Conservative scattering, A0=1.  ---  NEVER EXECUTED------------
c omit the  F_0 factor throughout, should not effect  BOND
	cosi3=3.*cosi
	cosisq=cosi*cosi
	omas=1.-as
	b2top= cosi3*omas*(2.+cosi3)/16.             ! eq 17a
	b1top= (0.75*cosisq +0.5*cosi)-tt*b2top      ! eq 17b

	ttaustar=(1.-g)*t1	! scaled tau at bottom of layer
	b2=(cosi3*omas*(2.+cosi3-(2.-cosi3)*emtc))
     &     /(16.+12.*omas*ttaustar)           ! eq 17a
	b1= (0.75*cosisq +0.5*cosi)-tt*b2     ! eq 17b

	RI(1,1) = b1top-0.75*cosisq               !  I0_top	eq. 16a@tau=0
	RI(2,1) = b2top-0.75*cosi		  !  I1_top	eq. 16b@tau=0
	RI(1,2) = b1-0.75*cosisq*emtc-b2*ttaustar !  I0_bot	eq. 16a
	RI(2,2) = b2-0.75*cosi*emtc 		  !  I1_bot	eq. 16b
	endif			! ------------------------------------------

	BOND = PI*(RI(1,1)-TT*RI(2,1))/COSI		! eq. 8 & 11
	ABS  = PI*(RI(1,2)+TT*RI(2,2)+COSI*F0*EMTC)*(1.-AS)/COSI ! ~eq. 10
CC	ABA = 1.-ABS-BOND		! fraction absorbed in atmosphere
	RETURN
	END
