C_Titl  PORBCM.INC   include for PORB system COMMOM
      REAL PLANUM ! 1 PLANET NUMBER, OUT FROM THE SUN (>9 ARE OTHER OBJECTS)
     &,TC	 ! 2 TIME IN CENTURIES FROM REFERENCE DATE (1950.0)
     &,ODE	 ! 3 LONGITUDE OF THE NODE
     &,CLIN	 ! 4 INCLINATION
     &,ARGP	 ! 5 ARGUMENT OF PERIAPSIS
     &,ECC	 ! 6 ECCENTRICITY
     &,SJA	 ! 7 SEMI-MAJOR AXIS (ASTRONOMICAL UNITS)
     &,TJO	 ! 8 JULIAN DATE OF PERIHELION
     &,OPERIOD	 ! 9 PERIOD OF THE ORBIT (JULIAN DAYS)
     &,ZFQA  ! 10 PLANET POLE ORIENTATION IN EARTH EQUITORIAL SYSTEM: DECLINATION
     &,ZFQB  ! 11  "     "      "        "    "       "        "     RIGHT ACEN.
     &,SIDAY    ! 12 SIDERIAL ROTATION PERIOD OF BIDY (HOURS)
     &,TLO   ! 13 JULIAN DATE OF ZERO LONGITUDE TOWARDS PLANETS VERNAL EQUINOX ??
     &,OBL	! 14 OBLIQUITY OF THE EARTH'S AXIS
     &,CQA	! 15 REFERENCE STAR, IN EARTH EQUATORIAL: DECLINATION
     &,CQB	! 16  "        "   "    "       "     : RIGHT ACENSION
     &,P17(2)   ! 17 SPARE       Then items computed from epoch constants
     &,ZFEB	! 19 Planets pole in ecliptic system; longitude
     &,ZFEC	! 20    "	"	"	"	 co-latitude
     &,XFEXB	! 21 Angle along planets equator from ecliptic to planet orbit
     &,ARGV	! 22 ANGLE FROM PLANETS ORBITAL NODE TO PLANETS VERNAL EQUINOX
     &,SLP	! 23 L-SUB-S AT PERIAPSIS
     &,BLIP	! 24 Planet obliquity
     &,PHOXX(3)	! 25 VECTOR FROM FOCUS (SUN) TO PLANET: ORBITAL PLANE COORDINATES
     &,PHEXX(3)	! 28 "     "     "    "    "     "    EQUTORIAL COORDINATES
     &,HFXX(3)	! 31 VECTOR FROM PLANET TO SUN IN PLANET-FIXED COORDINATES
     &,FQ(9)	! 34 ROTATION MATRIX: TO PLANET-FIXED FROM EARTH-EQUITORIAL
     &,EO(9)	! 43 ROTATION MATRIX: TO ECLIPTI     &,FROM ORBITAL
     &,HO(9)	! 52 ROTATION MATRIX: TO HELIOCENTRIC PLANET-FIXED, FROM ORBITAL
C --- end of the 60 real variables transfered to disk files
      Real PICON		! 3.14159...
     &,RUNTIM(5)	! run_time in  20 ASCII bytes
      INTEGER IOK 	! LogicIO UNIT FOR INPUT (KEYBOARD)
     &,IOS		! LogicIO UNIT FOR PROMPT (SCREEN)
     &,IOP		! LogicIO UNIT FOR OUTPUT (PRINTER)
     &,IOD		! LogicIO UNIT FOR DATA FILE

C_Vars
      COMMON /PORBCM/ PLANUM,TC, ODE,CLIN,ARGP,ECC,SJA,TJO,OPERIOD
     1 ,ZFQA,ZFQB,  SIDAY,TLO,  OBL, CQA,CQB,  P17
     2 ,ZFEB,ZFEC,XFEXB,ARGV,SLP,BLIP
     3 ,PHOXX,PHEXX,HFXX, FQ,EO,HO
     5 ,PICON, RUNTIM, IOK,IOS,IOP,IOD
C_Desc
C    >>> All angles in radians <<<
C Conditions at request epoch
C_Hist 1997jan30 and earlier  Hugh Kieffer
C 2009mar17  HK Number the positions in comments
C 2009dec24  HK Change PERIOD, PI, DAYTIM to OPERIOD, PICON, RUNTIM; to avoid 
C    conflict with  krccom.inc.  Revise PLANUM
C_Hist 2010jan12 Hugh Kieffer  Change to IMPLICIT NONE.
C 2012mar02  HK  Replace spare P24 with BLIP
C_End 
