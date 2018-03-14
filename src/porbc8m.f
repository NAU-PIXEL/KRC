C_Titl  PORBCM   R*8 include for PORB version 3 system COMMOM
C Angles in radians unless specified
C Default orientation for input: Orbit is J2000 ecliptic. 
C      IMPLICIT NONE  
C                                Pole is J2000 equatorial  
      REAL*8 PLANUM ! 1 Body number. 100*fileNumber+itemNumber
     &,TC        ! 2 Time in centuries from reference date (2000.0)
     &,RODE      ! 3 Longitude of the rising node, radin
     &,CLIN      ! 4 Inclination, radian
     &,ARGP      ! 5 Argument of periapsis, radian
     &,XECC      ! 6 Eccentricity
     &,SJA       ! 7 Semi-major axis (Astronomical Units)
     &,EOBL      ! 8 Obliquity of the Earth's axis, radians
     &,SFLAG     ! 9 Input system flag 0:default, +1: pole=ec +10:orb=eq
     &,ZBAA      ! 10 Body pole: declination, radian in J2000 eq.
     &,ZBAB      ! 11  "     "       Right Ascension, radian in J2000 eq.
     &,WDOT      ! 12 Siderial rotation rate; degrees/day (used only in reconc.f) 
     &,W0        ! 13 Position of prime meridian at 2000.0, degree
C Above are from orbital element tables  Below are derived    
      REAL*8 OPERIOD ! 14 Period of the orbit (days)
     &,TJP       ! 15 J2000 Date of perihelion
     &,SIDAY     ! 16 Siderial rotation period of body (hours)
     &,spar17    ! 17 unused
     &,TAV       ! 18 True anomaly at spring equinox, radians
     &,BLIP      ! 19 Body obliquity, radians
     &,PBUG      ! 20 Debug Flag   
     &,spar21    ! 21 Unused    
     &,BFRM(9)   ! 22 Rotation matrix from orbital to seasonal
C --- end of the 30 real variables generated by PORB system

      REAL*8 PICON      !31  3.14159...  Set by main program 
     &,R2D        !32 multiplier; radian to degrees: "
     &,PHFXX(3)   !33 Vector from focus (Sun) to planet: orbit-plane coordinates
     &,HPBXX(3)   !36 Vector from planet to Sun in body-inertial coordinates
     &,spar39(3)  !39 spare vector      Total 41 R**

      INTEGER*4 IOK     ! Logical UNIT FOR INPUT (KEYBOARD) Set by main 
     &,IOS              ! Logical UNIT FOR PROMPT (SCREEN)  Set by main 
     &,IOP              ! Logical UNIT FOR OUTPUT (PRINTER) Set by main 
     &,IOD              ! Logical UNIT FOR DATA FILE        Set by main 
C above Num. bytes equiv to 2 R*8   Below equiv to 2.5 R*8
      CHARACTER*20 RUNTIM     ! run date_time  Set by main
      CHARACTER*14 PVERS      ! version        Set by main

C_Vars
      COMMON /PORBCM/ PLANUM,TC,RODE,CLIN,ARGP,XECC,SJA,EOBL,SFLAG
     2,ZBAA,ZBAB,WDOT,W0,OPERIOD,TJP,SIDAY,spar17,TAV,BLIP,PBUG
     3,spar21,BFRM,PICON,R2D,PHFXX,HPBXX,spar39
     4,IOK,IOS,IOP,IOD,  RUNTIM,PVERS

      INTEGER IDEM1
      PARAMETER (IDEM1=30)      ! number of words in input geometry matrix
      REAL*8 PCOM(IDEM1)          ! geometry matrix
      EQUIVALENCE (PCOM,PLANUM) 

C_Hist 1997jan30 and earlier  Hugh Kieffer
C deletia
C 2013jun19 HK Major revision for KRC version 2
C 2014mar14 HK Make  REAL*8  version 
C 2014jun06:10 HK Put spar39 into common, Rename variables to lessen confusion: 
C              ECC to XECC, OBL to EOBL, ODE to RODE  
C              Make RUNTIME character, add PVERS=version to common
C 2015dec12 HK Correct comments for several units from degree to radian
C 2016aug22 HK Comment implicit none
C_End 

