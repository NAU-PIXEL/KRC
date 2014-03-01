C_Title	COCOSP General coordinate conversion package, many routines.
CC_Args
CC  Argument order is:   INPUTS _space_ OUTPUTS
CC  Argument names are consistent thru all routines.
CC  DLAT = Latitude; in degrees
CC  DLON = West longitude; in degrees
CC  E = Ellipticity of oblate spheroid = SQRT (1. - (R_Pole/R_Equat)**2)
CC  R = equatorial radius
CC  P = Phi = Polar angle from Z axis (co-latitude); in radians
CC  Q = Lambda = azimuthal angle, positive from X axis; in radians
CC  V(3) = Cartesian vector, in same units as R
CC_Keys	 MATH VECTOR COORDINATE CONVERSION
C_Description  The routine COCOSP is a general coordinate conversion package
C with many routines.  This routine is only documentation for a family
C of programs; it has no code.
C_Bugs	This routine is only documentation for family, no code.
C_History  Hugh_H_Kieffer 82Oct26
C  85Apr changed entries to separate routines, other trivial changes
C  85Oct17 Document to NIMS standards
C_Pause
C
C Routine names: COC456 where:
C  4: O=Sphere  E=Ellipsoid of rotation (mapping only)
C  5: Input system symbol
C  6: Output system symbol
C 	 System      Symbol   Arguments
C 	Cartesian	C	V(3)
C 	Mapping		M	DLAT,DLON,R
C	Ellip. map     EM	DLAT,DLON,R,E
C	Spherical	S	P,Q,R
C  ** This is single precision version; to generate a double precision
C  **   version, make the following changes throughout
C  **  REAL*4 > REAL*8
C  **  SIN,COS,SQRT,ATAN2 > DSIN, DCOS, DSQRT, DATAN2
C  **  E0 > D0
C  ** Make similar changes to documentation
C  Uses 90.E0 rather than PI/2 where no additional conversion is involved,
C   as this eliminates one roundoff error.

	SUBROUTINE COCOCM (V,   DLAT,DLON,R)	! CARTESIAN TO MAPPING
	IMPLICIT NONE
C_Title	COCOCM Coordinate conversion: cartesian to mapping
C_Args
	REAL*4  V(3)	![I] Cartesian vector.
	REAL*4  DLAT	![O] Latitude; in degrees.
	REAL*4  DLON	![O] West longitude; in degrees.
	REAL*4  R	![O] Equatorial radius.
C_Keys	MATH VECTOR COORDINATE CONVERSION
C_Desc	The subroutine COCOCM performs coordinate conversion: Cartesian
C	to mapping.  
C_Call	COCOCS COCOSM
C_Hist	85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_Pause
	REAL*4 P,Q            ! Internal variables, used as arguments in calls
C
	CALL COCOCS (V,   P,Q,R)	! To spherical
	CALL COCOSM (P,Q,   DLAT,DLON)	! Then to mapping angles
C
	RETURN
	END


	SUBROUTINE COCOMC (DLAT,DLON,R,    V)	! MAPPING TO CARTESIAN
	IMPLICIT NONE
C_Title  COCOMC Coordinate conversion: mapping to cartesian
C_Args
	REAL*4 DLAT	! [I] Latitude; in degrees
	REAL*4 DLON	! [I] West longitude; in degrees
	REAL*4 R	! [I] Spherical radius
	REAL*4 V(3)	! [O] Cartesian vector
C_Keys  MATH  VECTOR  COORDINATE  CONVERSION
C_Desc	Part of COCOSP family, which see
C_Hist	85Oct17  H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C	89feb03  HHK revise to ISIS standards.
C_Paus
	REAL*4 Q,H,RLAT,RPD		! Internal variables
	PARAMETER (RPD=.01745329251994329577E0)  ! Radians per degree
C
	RLAT = DLAT*RPD
	Q = -DLON*RPD           ! Lambda
	H = R*COS(RLAT)		! Radius of rotation
	V(1) = H*COS(Q)
	V(2) = H*SIN(Q)
	V(3) = R*SIN(RLAT)
C
	RETURN
	END



	SUBROUTINE COCOSC ( P,Q,R,   V)		! SPHERICAL TO CARTESIAN
	IMPLICIT NONE
C_Title	COCOSC Coordinate conversion: spherical to cartesian
C_Args
	REAL*4  P   ![I] Phi = polar angle from Z axis (co-latitude); in radians.
	REAL*4  Q   ![I] Lambda = azimuthal angle, positive from X axis; in radians.
        REAL*4  R   ![I] Equatorial radius.
        REAL*4  V(3)	![O] Cartesian vector.
C_Keys	MATH VECTOR COORDINATE CONVERSION
C_Desc	The subroutine COCOSC performs coordinate conversion: spherical
C	to Cartesian.
C_Call	0
C_Hist	85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_Paus
	REAL*4 H			! Internal variables
C
	H = R*SIN(P)	! Radius of rotation around Z
	V(1)=H*COS(Q)
	V(2)=H*SIN(Q)
	V(3)=R*COS(P)
C
	RETURN
	END


	SUBROUTINE COCOCS (V,   P,Q,R)		! CARTESIAN TO SPHERICAL
	IMPLICIT NONE
C_Title	COCOCS Coordinate conversion: cartesian to spherical.
C_Args
	REAL*4  V(3)	![I] Cartesian vector.
	REAL*4  P    ![O] Phi = polar angle from Z axis (co-latitude); in radians.
	REAL*4  Q    ![O] Lambda = azimuthal angle, positive from X axis; in radians.
	REAL*4  R    ![O] Equatorial radius.
C_Keys	MATH VECTOR COORDINATE CONVERSION
C_Desc	The subroutine COCOCS performs coordinate conversion: Cartesian to
C	spherical.  It is part of the COCOSP family of programs.
C_Call	0
C_Hist	85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_Paus
	REAL*4 H,PID2			! Internal variables
C
	PARAMETER (PID2=1.57079632679489662E0)  ! PI/2
	H = V(1)**2 + V(2)**2	! X-Y Radius squared
	IF (H .EQ. 0.E0) THEN
		Q = 0.E0		! On polar axis, azimuth indeterminate
	    ELSE
		Q = ATAN2 (V(2),V(1))
	    ENDIF
	R = SQRT (H + V(3)**2)	! Radius
	IF(R .EQ. 0.E0) THEN
		P = 0.E0	! Radius is zero, polar angle indeterminate
	    ELSE
		P = PID2 - ATAN2 (V(3), SQRT(H) )
	    ENDIF
C
	RETURN
	END


	SUBROUTINE COCOMS (DLAT,DLON,  P,Q)	! MAPPING TO SPHERICAL ANGLES
	IMPLICIT NONE
C_Title	COCOMS Coordinate conversion: mapping to spherical angles
C_Args
	REAL*4  DLAT	![I] Latitude; in degrees.
	REAL*4  DLON	![I] West longitude; in degrees.
	REAL*4  P	![O] Phi = polar angle from Z axis (co-latitude); in radians.
        REAL*4  Q	![O] Lambda = azimuthal angle, positive from X axis; in radians.
C_Keys	MATH COORDINATE CONVERSION
C_Desc	The subroutine COCOMS performs coordinate conversion: mapping
C	to spherical angles.  
C_Call	0
C_Hist	85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_Paus
	REAL*4 RPD			! Internal variables
	PARAMETER (RPD=.01745329251994329577E0)  ! Radians per degree
C
	P = (90.E0-DLAT)*RPD
	Q = -DLON*RPD
C
	RETURN
	END


	SUBROUTINE COCOSM (P,Q,   DLAT,DLON)	! SPHERICAL TO MAPPING ANGLES
	IMPLICIT NONE
C_Title	COCOSM Coordinate conversion: spherical to mapping angles
C_Args
	REAL*4 P   ![I] Phi = polar angle from Z axis (co-latitude); in radians.
	REAL*4 Q   ![I] Lambda = azimuthal angle, positive from X axis; in radians.
	REAL*4 DLAT ![O] Latitude; in degrees.
	REAL*4 DLON ![O] West longitude; in degrees.
C_Keys	MATH COORDINATE CONVERSION
C_Desc	The subroutine COCOSM performs coordinate conversion: spherical to
C	mapping angles.  
C_Call	0
C_Hist	85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_Paus
	REAL*4 DPR
	PARAMETER (DPR=57.295779513082320876798E0)
C
	DLAT = 90.E0-P*DPR
	DLON = -Q*DPR
C
	RETURN
	END


	SUBROUTINE COCEMC (DLAT,DLON,R,E,   V)	! ELLIP. MAPPING TO CARTESIAN
	IMPLICIT NONE
C_Title	COCEMC Coordinate conversion: ellip. mapping to cartesian
C_Args
	REAL*4  DLAT	![I] Latitude; in degrees.
	REAL*4  DLON	![I] West longitude; in degrees.
	REAL*4  R	![I] Equatorial radius.
	REAL*4  E	![I] Ellipticity of oblate
                        ! spheroid = sqrt(1.-(R_pole /R_equat)**2).
	REAL*4  V(3)	![O] Cartesian vector.
C_Keys	MATH VECTOR COORDINATE CONVERSION
C_Desc	The subroutine COCEMC performs coordinate conversion:  Elliptical 
C	mapping to Cartesian.  
C_Call	0
C_Hist	85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_End
	REAL*4 F,G,H,RLAT,RPD,Q		! Internal variables
	PARAMETER (RPD=.01745329251994329577E0)  ! Radians per degree
C
	RLAT = DLAT*RPD		! Latitude in radians
	F = SIN(RLAT)		! Sine of latitude
	G = R/SQRT(1.E0-(E*F)**2) ! Radius of meridian curvature
	H = G*COS(RLAT)		! Radius of rotation
	V(3)=G*F*(1.E0-E**2)
	Q = -DLON*RPD		! Lambda = azimuth in radians
	V(1) = H*COS(Q)
	V(2) = H*SIN(Q)
C
	RETURN
	END


	SUBROUTINE COCECM (V,E,DLAT,DLON,R)     !CARTESIAN TO ELLIP. MAPPING
	IMPLICIT NONE
C_Title	COCECM Coordinate conversion: cartesian to ellip. mapping
C_Args
	REAL*4 V(3)       ! [I] Cartesian vector
	REAL*4 E          ! [I] Ellipticity of oblate spheroid
                          !     sqrt(1.-(R_pole/R_equat)**2)
	REAL*4 DLAT       ! [O] Latitude; in degrees
	REAL*4 DLON       ! [O] West longitude; in degrees
	REAL*4 R          ! [O] Equatorial radius.
C_Keys	MATH VECTOR COORDINATE CONVERSION
C_Desc	COCECM shall be part of the COCOSP family of routines.  COCECM shall
C	convert cartesian coordinates to elliptical mapping coordinates.
C_Hist	86May22 D. Cook, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_End
        REAL*4 TINY,TINY2,EVAL,F,G,H    ! Internal variables
	REAL*4 RLAT,RPD,Q,RG,XU,YU,ZU   ! More internal variables
	PARAMETER (RPD=.01745329251994329577D0)  ! Radians per degree
        PARAMETER (TINY=2.E-7)          ! Precision limit
        PARAMETER (TINY2=1.E-37)        ! Dynamic range limit
C
        RG = SQRT(V(1)**2+V(2)**2+V(3)**2)      ! Geocentric radius
          IF (RG.LT.TINY2) THEN                 ! Too small for this precision
              DLAT = 0.                         ! Arbitrarily set output to zero
              DLON = 0.
          ELSE
              XU = V(1)/RG                      ! Normalized cartesian vector
              YU = V(2)/RG
              ZU = V(3)/RG
              H  =  SQRT(XU**2 + YU**2)         ! Calculate radius of rotation
              EVAL  =  1.E0 - E*E
                IF (H.GT.TINY) THEN             ! Not near a pole
                      IF (ABS(XU).LT.TINY) THEN ! On Y-axis
                          Q = SIGN(90.*RPD,YU)
                      ELSE
                          Q = ATAN2(YU,XU)
                      END IF
                    DLON = -Q/RPD
                    RLAT = ATAN(ZU/(H*EVAL))    ! Latitude in radians
                    G = H/COS(RLAT)             ! Radius of meridian curvature
                    F = SIN(RLAT)
                    R  =  RG*G * SQRT(1.E0-(E*F)**2) ! Equitorial radius
                    DLAT = RLAT/RPD
                ELSE                            ! Point near a pole
                    R  =  ABS(V(3)) / SQRT(EVAL)! V(3) must be near identical
                    DLAT = SIGN(90.,ZU)         ! to polar radius.
                    DLON = 0.
                END IF
          END IF
C
	RETURN
	END
