	SUBROUTINE  PORB0
C_Titl PORB0 Planetary orbit. Read pre-computed matrices; minimal for KRC
C_HIST	97jan30  HHK revised from  PORB1
C 2005dec28 HK Change to use of IMPLICIT NONE
C 2013jul24 HK Change to V2 PORB system
C_END
	INCLUDE 'porbcm.inc' ! contains IMPLICIT NONE
	INCLUDE 'units.inc'

	REAL*4 P(30)		! block of 30 floats to read
	EQUIVALENCE (P,PLANUM)	! correspond to start of common
	logical LOP		! debug print
	LOP=IDB1.GT.3
	IF (LOP) WRITE(IOSP,*)'PORB0 1'
	READ(IOIN,33)	! skip the line of PORB runtime
	IF (LOP) WRITE(IOSP,*)'PORB0 2'
	READ(IOIN,33) P
	IF (LOP) WRITE(IOSP,33) P
	IF (LOP) WRITE(IOSP,*)'PORB0 3'
33	FORMAT (5G15.7) 
	PICON=3.141592654 ! ensure value in porbcm set.
	R2D=180./PICON

	IF (LOP) WRITE(IOSP,*)'PORB0 4'
	RETURN
	END
