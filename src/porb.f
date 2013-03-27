      SUBROUTINE PORB (TJ, PER,PEA,PEB,HFA,HFB)
C_TITLE	PORB computes planetary angles and location for specific time.
      IMPLICIT NONE
C_ARGS	e.g.  WEIGHT (10) R*4 [I]  Weighting factors
      REAL TJ !in. request time in same units as used in common (julian day).
C outputs:   all returned angles in radians
      REAL per ! radius       position of a planet 
      REAL pea ! latitude         in heliocentric ecliptic
      REAL peb ! longitude         coordinates.
      REAL hfa ! latitude       position of sun in
      REAL hfb ! longitude        planetocentric fixed coordinates.
C_VARS
	INCLUDE 'porbcm.inc'
C_DESCR	conic orbit; uses solution to Keplers equation
C_CALLS ORBIT ROTVEC /PORBCM/=PROBCM.INC
C_HIST	70xxxxx HHKieffer UCLA ORIGINAL VERSION
C 75sep29 HHK recode ?
C 84may29 HHK VAX version
C 85jan13 HHK revised PORBCM
C 85sep14 HHK change called routine argument order to agree with *SP system
C 2010jan12 HK Use IMPLICIT NONE
C 2012nov08 HK Change ROTATE to ROTVEC to ensure getting proper routine
C 2013jan24 HK Fix order of comments for output.
C_END
C
        REAL T
      T = TJ-TJO
      CALL ORBIT (SJA,OPERIOD,ECC,T, PER,PHOXX)
      CALL ROTVEC (EO,PHOXX, PHEXX)
      PEA = ASIN (PHEXX(3)/PER)
      PEB = ATAN2 (PHEXX(2),PHEXX(1))
C PHOXX is vector from focus (sun) to planet in orbital plane
C PHEXX is vector from focus to planet in equitorial system
C HFXX  is vector from planet to sun in planetocentric-fixed system
      CALL ROTVEC (HO,PHOXX, HFXX)
      HFA = ASIN (HFXX(3)/PER)
      HFB = ATAN2 (HFXX(2),HFXX(1) )
      RETURN
      END
