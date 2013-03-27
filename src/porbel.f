	SUBROUTINE PORBEL (IOP,IOD,IFILE,IPLAN,TCEN, ODE,CLIN,ARGP
     &,ECC,PERIOD,SJA,SIDAY,PRA,PDEC,TJO,TITLE)
C_Title  PORBEL read planetary orbital element file, compute basic constants
	IMPLICIT REAL*8 (A-F,O-Z)
C_Args
	INTEGER IOP	!I LOGICAL UNIT FOR REPORT, IF NOT=0.
	INTEGER IOD	!i logical unit number to use for input data file
	INTEGER IFILE 	!i which type of input file
	INTEGER IPLAN	!i body in file; for sturms, first 9 are planet 
C				number counting out from sun
	REAL*4 TCEN	!i request time in tropical centuries (=36524.22 days)
C				from 1950.0.
	REAL*4 ODE	!o longitude of ascending node <radians>
	REAL*4 CLIN	!o inclination
	REAL*4 ARGP	!o argument of periapsis <radians>
	REAL*4 ECC	!o eccentricity
	REAL*4 PERIOD	!o orbital period in days
	REAL*4 SJA	!o semi-major axis in astronomical units
	REAL*4 SIDAY	!o length of siderial day in hours
	REAL*4 PRA	!o right ascension of pole <radians>
	REAL*4 PDEC	!o declination of pole <radians>
	REAL*4 TJO	!o julian day (offset from 2,440,000) of perihelion
	CHARACTER*(*) TITLE  !o planet name
C_Keys  ORBITS
C_Desc  
C planetary orbital elements referred to mean ecliptic and equinox of 1950
C INTERNALLY USES DOUBLE PRECISION AND DEGREES, RETURNS SINGLE PRECISION RADIANS
C read a file based on:
C  1) "mean elements of the principal planets" by
C	seidelman, doggett and deluccia; astron jour v. 29, 1974, p 57
C  2)  JPL  TR 32-1508 by Fancis M. Sturms, Jr. 1972jan15
C  3)  Comet elements from Ted Bowell 1985sep07
C_File  IOD = orbital elements file, opened and closed
C_Call  YMD2JD
C_Hist	85feb00  Hugh_H_Kieffer  U.S.G.S._Flagstaff   original_version
C	89sep11  HK fix bug in Sturms  TJO, put more into double precession
C 2009dec15  HK Add fourth file for single object. Initally Vesta
C 2012feb29  HK Minor Plan Handle either full JD or relative to 2440000 as inputs
C 2012nov21  HK Change element file names to all lower case and .tab extension
C               and change NAME to FILE in OPEN statement
C_End&___1_________2_________3_________4_________5_________6_________.72

	PARAMETER (NFILES=4)
	CHARACTER*12 FNAME(NFILES)
	DATA FNAME/'seidel.tab','sturms.tab','bowell.tab','minor.tab'/
	INTEGER*4 NLINES(NFILES)/10,13,10,13/  ! number of input lines per body
	REAL*8 EPOCH		! Julian Day of elements
	real*8 DJBASE /2.44D6/	! 2440000  All days offset from this.
	REAL*8 VVV(10)		! elements computed in loop 
	REAL*8 c0,c1,c2,tc

	PARAMETER (ZERO=0.D0, D60=60.0D0, D360=360.0D0)
C function: convert file format to VALUE at request date. minimize roundoff.
c When time dependence is in arcSec/century and this squared
	DEGREE (I,J,S,S1,S2,T) = ( ((S2*T+S1)*T+S)/D60 + J)/D60 + I
C 
	DEGREE2 (D,D1,D2,T) = (D2*T+D1)*T + D ! when rates are Deg/century
C function: OR TO RADIANS
CC	RADIAN(I,J,S,S1,S2,T) = RSEC*(S+T*(S1+S2*T)) + J*RMIN + I*RDEG
CC	RADI2 (D,D1,D2,T) = RDEG * ( ( T*(D1+D2*T)) + D )

C initialize constants
	SECDEG = D60*D60		! secondS of arc / DEGREE
	TWOPI = 6.2831853071795864769	! radians/revolution
	PI = TWOPI/2.
	RDEG = TWOPI/D360		! radians/degree of arc
	RMIN = RDEG/D60			! radians/minute of arc
	RSEC = RMIN/D60			! radians/second of arc
	TRPCEN = 36524.22D0 		! days/tropical century
	DMOTE2 = 2.959130739D-4  ! mean daily motion of earth, squared RADIANS
	DJ50=-6718.D0	! julian day of 1950.0 (=2433282), offset from 2440000
	T = TCEN
	DAYAT = T*TRPCEN
C open data file, skip to requested planet
	OPEN (UNIT=IOD, FILE=FNAME(IFILE), STATUS='OLD')
	ISKIP = 2 + NLINES(IFILE)*(IPLAN-1)
	DO I=1,ISKIP
		READ (IOD,*)
		ENDDO

	READ (IOD,*) TITLE		! get planets name
	GOTO (100,200,300,400),IFILE


C seidel===========================================================
100	READ(IOD,*)IDEG,MIN,SEC,IREV,SEC1,SEC2	! get mean longitude
	A=  DEGREE (IDEG,MIN,SEC,SEC1,SEC2,T)
C..	a=  DEGREE (ideg,min,sec,ZERO,sec2,t)	!# use for alternate tjo

	READ(IOD,*)IDEG,MIN,SEC,SEC1,SEC2	! get longitude of perihelion
	B=  DEGREE (IDEG,MIN,SEC,SEC1,SEC2,T)

	READ(IOD,*)IDEG,MIN,SEC,SEC1,SEC2	! get long. of ascending node
	DODE=DEGREE (IDEG,MIN,SEC,SEC1,SEC2,T)

	READ (IOD,*)IDEG,MIN,SEC,SEC1,SEC2	! get inclination
	DCLIN = DEGREE (IDEG,MIN,SEC,SEC1,SEC2,T)

	READ(IOD,*)ANUM,BNUM,CNUM		! get eccentricity
	DECC = ANUM + (BNUM +CNUM*T)*T

	IF(IPLAN .LE. 4)THEN
	 READ(IOD,*)SEC,SEC1		! get either mean century motion 
	 DMOT=(SEC+SEC1*T)*RSEC/TRPCEN	!  DMOT = mean daily motion, RADIAN
	 DSJA=(DMOTE2/DMOT**2)**(1./3.)
	ELSE
	 READ(IOD,*)DSJA			!   or semimajor axis
	 DMOT=SQRT(DMOTE2/DSJA**3)
	ENDIF
	PERIOD = TWOPI/DMOT		! and calculate period (in days)

	READ(IOD,*)ANUM,SEC,BNUM,SEC1	! get ra and dec of north pole. 
	DPRA  = ANUM + SEC *T/SECDEG	! IN DEGREES
	DPDEC = BNUM + SEC1*T/SECDEG

	READ(IOD,*)SIDAY		! get length of siderial day

C argument of perihelion is longitude of perihelion minus the
C longitude of the node
	DARGP=B-DODE
C mean anomaly is mean longitude minus the longitude of perihelion
	DD = IREV*D360*T
	DANOM = MOD (DD+A-B,D360)
C get julian date of nearby perihelion
C	tjo = ifix (dayat/period) * period +dj50 -(a-b)/dmot !# alternate
	TJO = DAYAT + DJ50 - DANOM/(DMOT/RDEG) ! current time - since last perih.
	GOTO 800

C sturms ===========================================================	
C first 3 items are independant of coordinate system
200	READ (IOD,*) DSJA			! get semimajor axis
	DMOT=SQRT(DMOTE2/DSJA**3)	!  and calculate mean daily motion
	PERIOD = TWOPI/DMOT		!  and period (in days)

	READ (IOD,*) DEG,DEG1,DEG2	! get eccentricity, inputs are really
	DECC = ((DEG2*T)+DEG1)*T +DEG	!   dimensionless, not in degrees.

	READ (IOD,*) DEG,DD,DEG2		! get mean anomoly, linear term 
	DANOM = DEGREE2 (DEG,ZERO,DEG2,T)	!  was specified in degree/day. 
	DANOM = MOD ((DD*DAYAT + DANOM),D360)

	DO I=3,9		! each numeric entry
	   DEG1=ZERO		! set linear term to zero
	   DEG2=ZERO		! and quadratic
	   READ (IOD,*) DEG0,DEG1,DEG2 ! list-directed read
	   VVV(I)=DEGREE2 (DEG0,DEG1,DEG2,TC) ! at request time
	ENDDO 
C next 3 items in mean equinox and ecliptic of 1950.0
	DCLIN=VVV(3)		! inclination
	DODE =VVV(4)		! longitude of node
	DARGP=VVV(5)		! argument of perihelion
C next 3 items in mean equinox and earth equator of 1950.
	DPRA =VVV(6)		! pole ra
	DPDEC=VVV(7)		! pole dec
C	DEL = VVV(8)             ! angle for node on earth equator along 
C                               !   planet equator to autumal equinox
C	OBLIQ =VVV(9)		!obliquity of the planet 

	READ (IOD,*) DEG,DD		! get prime meridian constants
	SIDAY = (D360*24.D0)/DD		! get length of siderial day
CC	PRIMEM = MOD ((DD*TRPCEN + DEG),D360) ! ROTATION THUS FAR

C get julian date of nearby perihelion
	TJO = DAYAT + DJ50 - DANOM/(DMOT/RDEG) ! current time - since last perih.
	GOTO 800

C bowell ===========================================================
300	READ (IOD,*) IYEAR,IMON,DAY	! get time of perihelion
	IDAY = DAY	! get integer days 
	CALL YMD2JD (IYEAR,IMON,IDAY, A)  ! get julian date base 2,440,000
	TJO = A + (DAY-FLOAT(IDAY)) 	! add back on the fraction of a day

	READ (IOD,*) DARGP		! get argument of perihelion
	READ (IOD,*) DODE		! get longitude of node
	READ (IOD,*) DCLIN		! get inclination
	READ (IOD,*) Q			! get perihelion in au
	READ (IOD,*) DECC		! get eccentricity
	DSJA = Q/(1.D0-DECC)		! convert to semi-major axis
	DMOT = SQRT ( DMOTE2/DSJA**3)	!  and to mean daily motion
	PERIOD = TWOPI / DMOT		!  and to orbital period.

	READ (IOD,*) DPRA,DPDEC	! get polar ra and dec
	READ (IOD,*) SIDAY	! get siderial day length
	GOTO 800

C Minor planet ===========================================================
C for KRC historic reasons, retain 1950.0 and 2440000 as reference dates
C But file may (usually will) contain full JD
400	READ (IOD,*) EPOCH	! get epoch as Julian Date
	IF (EPOCH .GT. DJBASE/2.) EPOCH=EPOCH-DJBASE ! make relative to 2440000
	TC=TCEN-(EPOCH-DJ50)/TRPCEN ! centuries from epoch to request data
	DO I=1,10		! each numeric entry
	   DEG1=ZERO		! set linear term to zero
	   DEG2=ZERO		! and quadratic
	   READ (IOD,*) DEG0,DEG1,DEG2 ! list-directed read
	   VVV(I)=DEGREE2 (DEG0,DEG1,DEG2,TC) ! at request time
	ENDDO 
	DSJA =VVV(1)		! semi-major axis
	DECC =VVV(2)		! eccentricity
	DCLIN=VVV(3)		! inclination
	DODE =VVV(4)		! longitude of node
	DARGP=VVV(5)		! argument of perihelion
	DANOM=VVV(6)		! Mean anomoly at epoch
	DPRA =VVV(7)		! pole ra
	DPDEC=VVV(8)		! pole dec
C	Q    =VVV(9)		! Prime meridian at epoch
	SIDAY=VVV(10)		! siderial day length
	DMOT = SQRT ( DMOTE2/DSJA**3)	!  mean daily motion, radians
	PERIOD = TWOPI/DMOT		!  and period (in days)
C JD of periapsis is Epoch -(M/360)Period = Epoch -M/MeanDailyMotion
C This date in the KRC system is JD-2440000 
	TJO = EPOCH - DANOM/(DMOT/RDEG) ! day at periapsis, offset from 2440000
	GOTO 800

C =================================================================
800	CLOSE (UNIT=IOD)
	ODE = DODE*RDEG	 ! convert output arguments to single precision radians
	CLIN = DCLIN*RDEG
	ARGP = DARGP*RDEG
	ECC = DECC
C  PERIOD  SIDAY  TJO WERE NOT IN DOUBLE_PREC.
	SJA = DSJA
	PRA = DPRA*RDEG
	PDEC = DPDEC*RDEG
	IF (IOP.NE.0) THEN
	  WRITE(IOP,801) IFILE,IPLAN,FNAME(IFILE),TITLE, TCEN
     &,DODE, DCLIN,DARGP,  ODE,CLIN,ARGP
     &,DSJA,DECC,TJO,PERIOD,  SIDAY, DPDEC,DPRA, PDEC,PRA
801	FORMAT ('0IFILE IPLAN FILE TITLE TCEN ='/2I4,2X,A,2X,A10,F8.6
     &/'[double] ODE CLIN ARGP =' /3F15.8 /3F15.8 
     &/' SJA ECC TJO PERIOD ='/4F15.8
     &/' SIDAY   [double] PDEC PRA       ='/F15.8/2F15.8/2F15.8)
	ENDIF
	RETURN
	END
