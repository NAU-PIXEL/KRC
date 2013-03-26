	SUBROUTINE  PORB0
C_Titl PORB0 Planetary orbit. Read pre-computed matrices and do rotation; minimal for KRC
C_HIST	97jan30  HHK revised from  PORB1
C 2005dec28 HK Change to use of IMPLICIT NONE
C_END
      IMPLICIT NONE
	INCLUDE 'units.inc'
	INCLUDE 'porbcm.inc'

	REAL P(60)		! blcvk of 60 floats to read
	EQUIVALENCE (P,PLANUM)	! correspond to start of common

	READ(IOIN,33)	! skip the line of PORB runtime
	READ(IOIN,33) P
33	FORMAT (5G15.7) 
	PICON=3.141592654 ! ensure value in porbcm set.
	RETURN
	END
