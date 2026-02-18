C_Title	COCODP8 General coordinate conversion package, many routines. DP
Contains: COCOCS 
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
C_Description  This file is a general coordinate conversion package
C with many routines.  
C_History  Hugh_H_Kieffer 82Oct26
C  85Apr changed entries to separate routines, other trivial changes
C  85Oct17 Document to NIMS standards
C 2014mar11  HK  Convert cocosp.f to this  REAL*8 version
C 2026Feb18  NMS Remove unused subroutines
C_Pause
C
C Routine names: COC456 where:
C  4: O=Sphere  E=Ellipsoid of rotation (mapping only)
C  5: Input system symbol
C  6: Output system symbol 
C        System      Symbol   Arguments
C       Cartesian       C       V(3)
C       Mapping         M       DLAT,DLON,R
C       Ellip. map     EM       DLAT,DLON,R,E
C       Spherical       S       P,Q,R
C  ** Thto get from single precision to double precision
C  **   version, make the following changes throughout
C  **  REAL*4 > REAL*8
C  **  SIN,COS,SQRT,ATAN > DSIN, DCOS, DSQRT, DATAN
C  **   ABS > DABS  SIGN > DSIGN
C  **  E0 > D0
C  ** Make similar changes to documentation
C  Uses 90.E0 rather than PI/2 where no additional conversion is involved,
C   as this eliminates one roundoff error.


        SUBROUTINE COCOCS (V,   P,Q,R)          ! CARTESIAN TO SPHERICAL
        IMPLICIT NONE
C_Title COCOCS Coordinate conversion: cartesian to spherical.
C_Args
        REAL*8  V(3)    ![I] Cartesian vector.
        REAL*8  P    ![O] Phi = polar angle from Z axis (co-latitude); in radians.
        REAL*8  Q    ![O] Lambda = azimuthal angle, positive from X axis; in radians.
        REAL*8  R    ![O] Equatorial radius.
C_Keys  MATH VECTOR COORDINATE CONVERSION
C_Desc  The subroutine COCOCS performs coordinate conversion: Cartesian to
C       spherical.  It is part of the COCOSP family of programs.
C_Call  0
C_Hist  85Oct17 H.Kieffer, U.S.G.S.,Flagstaff. See COCOSP.FOR
C_Paus
        REAL*8 H,PID2                   ! Internal variables
C
        PARAMETER (PID2=1.57079632679489662D0)  ! PI/2
        H = V(1)**2 + V(2)**2   ! X-Y Radius squared
        IF (H .EQ. 0.D0) THEN
                Q = 0.D0                ! On polar axis, azimuth indeterminate
            ELSE
                Q = DATAN2 (V(2),V(1))
            ENDIF
        R = DSQRT (H + V(3)**2) ! Radius
        IF(R .EQ. 0.D0) THEN
                P = 0.D0        ! Radius is zero, polar angle indeterminate
            ELSE
                P = PID2 - DATAN2 (V(3), DSQRT(H) )
            ENDIF
C
        RETURN
        END


