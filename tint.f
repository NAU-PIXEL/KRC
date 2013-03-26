	SUBROUTINE TINT (AIN, AREA, SUM)
C_Titl  TINT   spherical integrals over globe
C_Vars
	INCLUDE 'krccom.inc'	! contains  IMPLICIT statement. uses  RAD  N4
	INCLUDE 'latcom.inc' ! uses  ALAT = band latitudes <degrees>
C_Args
	REAL*4 AIN(*)	! in. values representative of each latitude band
	REAL*4 AREA(*)	! in/out. normalized area of each band
	REAL*4 SUM	! out. global average 
C_Lims
C  Latitudes in  ALAT must increase (from south to north), and should cover 
C that part of the planet where  AIN will be non-zero.
C_Desc 
C  Starts at south pole, and places boundaries between the bands at the
C point halfway between successive entries in  ALAT.  The weighting functions
C are computed on an intializing call with  AREA(1) = 0. on input.
C_Hist	84jun12  Hugh_H_Kieffer  U.S.G.S._Flagstaff original version
C 87sep23  HHK revise documentation
C 93mar03  ECisneros Ported to UNIX; made include filenames lowercase
C 97jan30  HHK minor edits
C_End

	IF (AREA(1).LE.0.) THEN	! first time, compute area array
	  SL = -1.0		! sine of prior band edge (south pole at start)
	  DO  I = 1,N4
	    IF (I.LT.N4) THEN
	      EDGE = (ALAT(I)+ALAT(I+1))/2. ! halfway between
	    ELSE
	      EDGE = 90.	! north pole for last edge
	    ENDIF
	    S = SIN (EDGE/RAD)
	    AREA(I) = (S-SL)/2.	! normalized area for this band
	    SL = S
	  ENDDO
	ENDIF

	SUM = 0.		! do summing weighted by area
	DO I = 1, N4
	  SUM = SUM + AREA(I) * AIN(I)
	ENDDO

	RETURN
	END
