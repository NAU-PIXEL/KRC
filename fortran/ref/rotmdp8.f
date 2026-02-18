C_TITLE   ROTMDP   General 3-dimension rotation matrix geometry package. Double
C_DESCRIPTION  
Contains:   ROTV  ROTVEC  VROTV 
C Argument order is: input _SPACE_ output,
C Angular arguments are in radians; except ROTMAT  ROTORB  ROTRIP use degrees
C Matrices are assumed stored as:  AA(I,J) = AA(row,column) = A(I+3(J-1)).
C   Commonly treat matrices as a 9-element vector
C Output argument may overwrite input argument for routine TRANS3.
C
C  AA,BB,CC and  A,B,C are 3X3 matrices;  U,V are 3-vectors;
C  R, are argument scalars;  E,F,G are internal scalars.
CC ** This is a double precision version made from rotmsp by
CC ** makeing the following changes throughout:
CC **  REAL*4  >  REAL*8
CC **  ASIN, ATAN2  >  DASIN, DTAN2
CC **  E0  >  D0  
C_Calls  VADDDP8 package
C_HIST  85apr17  Hugh_H._Kieffer  U.S.Geological_Survey
C 91apr17  HK reorder, use  ENDDO
C 91mar04  HK include  ROTEX_ routines
C 2013jun20 HK Include  ROTV.  Rename ROTATE to ROTVEC to avoid NumRec conflict
C 2013jun30 HK Use  IMPLICIT NONE  in  each routine
C 2014mar10 HK Make  REAL*8  version. All routines name the same as Single Prec
CC ** This is a double precision version made from rotmsp by
CC ** makeing the following changes throughout:
CC **  REAL*4  >  REAL*8
CC **  ASIN, ATAN2  >  DASIN, DTAN2
CC **  E0  >  D0  
C 2014jun06 HK Put routines in alphabetical order and add list of them
C 2016sep26 HK Untabify, adjust spacing and edit some comments. No algor. changes
C 2026Feb18 NMS Remove unused subroutines
C_Pause


      SUBROUTINE ROTV   (A,N,R, B)    ! B = [R_N]*A
C_Title  ROTV  Rotate a vector about a Cartesian axis
      IMPLICIT NONE
      REAL*8 A(3) ! in.  original vector
      INTEGER N   ! in.  Integer axis about which to rotate. 1=X, 2=Y, or 3=Z
      REAL*8 R    ! in.  Size of rotation, in radians.
      REAL*8 B(3) ! out. New vector
C_Desc  
C   Component along axis of rotation is not changed.
C   Other two components subject to simple rotation
C_Calls  none
C_Hist 2002dec11 Hugh Kieffer  IDL version
C 2012may31  HK  Fortran version
C_Pause
      INTEGER I,J,K
      REAL*8 C,S,XIN,YIN

      C = COS(R)
      S = SIN(R)
!     Get indices as if rotation was about Z! which reduces this to rotation 
!     effectively in XY plane.  Must keep all MOD positive
      I=  MOD(N+3,3)+1        ! 0-based location of effective X
      J=  MOD(N+1,3)+1        ! " " Y
      K=  MOD(N+2,3)+1        ! rotation axis 
      XIN=A(I)              ! effective X input
      YIN=A(J)              !  " Y "
      B(I)= C*XIN - S*YIN       ! do the rotation, put X in proper location
      B(J)= S*XIN + C*YIN       ! " Y "
      B(K)=A(K)             ! component along rotation axis is unchanged
      RETURN
      END


      SUBROUTINE ROTVEC (B,V, U)      !  U =  B * V
C_Title  ROTVEC  Rotate a vector      !  U =  B *  V
      IMPLICIT NONE
C_Arguments
      REAL*8 B(9)     ![I] 3x3 Rotation matrix.
      REAL*8 V(3)     ![I] Original vector.
      REAL*8 U(3)     ![O] Rotated vector, must not be same array as V.
C_Keys  
C_Desc  straight-forward product of rotation matrix and original vector.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I
      DO I=1,3
        U(I)=B(I)*V(1)+B(I+3)*V(2)+B(I+6)*V(3)
        ENDDO
      RETURN
      END


      SUBROUTINE VROTV (VIN,AXIS,THETA, ROUT) ! rotate VIN about AXIS by THETA
C_Titl  VROTV  Vector rotation about another vector
      IMPLICIT NONE
C_Args
      REAL*8   VIN  (3)   ! I   Vector to be rotated.
      REAL*8   AXIS (3  ) ! I   Axis of the rotation
      REAL*8   THETA      ! I Angle of rotation (radians)
      REAL*8   ROUT (3)   ! O Result of rotating V about AXIS by THETA
C_Calls    VADD  VCROS VCROSS  VFDOT  VEQUAL  VFMAG  VNORM  VSUB   
C_History
C 2003feb04 Hugh Kieffer  Derived IDL version from NAIF vrotv.f
C 2012may31 HK  Derive from IDL version using local .f library
C 2012jun03 HK  Extensive testing. 
C 2013jul24 HK  Move into rotmsp and use vaddsp routines
C_End
      INTEGER I
      REAL*8 AXMAG,Q,CT,ST ! ,  Q2,q3
      REAL*8 RPLANE (3)
      REAL*8 PP     (3) ! ,  p3(3)
      REAL*8 V1     (3)
      REAL*8 V2     (3)
      REAL*8 XX     (3)
 
      CALL VMAG(AXIS, AXMAG) ! length of AXIS, used to normalize it
C
      IF (AXMAG .EQ. 0.) THEN ! if axis has zero length
       CALL VEQUAL (VIN, ROUT)     ! return the input vector
       RETURN
      END IF
C 
      DO I=1,3              ! equiv to VHAT(AXIS,XX)
       XX(I)=AXIS(I)/AXMAG     !  unit vector in the direction of axis
      ENDDO
C
C     Compute the projection of V onto AXIS.  Call it PP.
C      CALL VPROJ ( V, XX, PP )   sub VPROJ ( A, B, PP )
C  t=a/maxa  r=b/maxb || scale= vdot(T,R)*maxa / cdot(R,R)==r^2
C  VSCL (scale,R,PP)  == out=scale*R
      CALL VDOT (VIN,XX,Q)          ! dot product with length of VIN 

      CALL VSCALE(Q,XX, PP)  !PP= q* XX
C
      CALL VSUB (VIN,PP,  V1 ) ! component of V orthogonal to AXIS
C
      CALL VCROSS (XX,V1, V2) ! Rotate V1 by 90 degrees about the AXIS 
C
C     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about
C     the AXIS in the plane normal to the axis, call the result RPLANE
C     CALL VLCOM ( C, V1, S, V2, RPLANE )
      CT = DCOS (THETA)
      ST = DSIN (THETA) 
      DO I=1,3
       RPLANE(I)=CT*V1(I) + ST*V2(I)
      ENDDO
C
C     Add the rotated component in the normal plane to AXIS to the
C     projection of V onto AXIS (PP) to obtain R.
C
      CALL VADD (RPLANE,PP, ROUT )

CD      i=6
CD 33   format(a6,'=[',f9.5,',',f9.5,',',f9.5,']')
CD       write(i,33)' V',vin
CD       write(i,33)'axis ',axis
CD       print *,'theta+',theta,c,s
CD       print *,'axmag',axmag
CD       write(i,33)' XX',xx
CD       print *,' Q=',q,q2,q3
CD       write(i,33)' p',pp
CD       write(i,33)'P3',p3
CD       write(i,33)'V1',v1
CD       write(i,33)'V2',v2
CD       write(i,33)'RP',rplane
CD       write(i,33)' out',rout
C
      RETURN
      END
