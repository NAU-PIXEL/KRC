      SUBROUTINE PORB (TJ, PER,PEA,PEB,HFA,HFB)
C_TITLE	PORB computes planetary angles and location for specific time.
C_ARGS	e.g.  WEIGHT (10) R*4 [I]  Weighting factors
C  TJ R* [I] request time in same time units as used in common (julian day).
C outputs:   all returned angles in radians
C	pea = latitude	  position of a planet 
C	peb = longitude	    in heliocentric ecliptic
C	per = radius	    coordinates.
C	hfa = latitude	  position of sun in
C	hfb = longitude	    planetocentric fixed coordinates.
C_KEYS	ORBIT
C_DESCR	conic orbit; uses soltuion to Keplers equation
C_CALLS ORBIT ROTVEC /PORBCM/=PROBCM.INC
C_HIST	70xxxxx HHKieffer UCLA ORIGINAL VERSION
C	75sep29 HHK recode ?
C	84may29 HHK VAX version
C	85jan13 HHK revised PORBCM
C	85sep14 HHK change called routine argument order to agree with *SP system
C 2004sep28 HK Change name  ROTATE to ROTVEC to avoid library conflicts
C_END
C
C	INCLUDE 'porbcm.inc/NOLIST'
	INCLUDE 'porbcm.inc'

      T = TJ-TJO
      CALL ORBIT (SJA,PERIOD,ECC,T, PER,PHOXX)
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
