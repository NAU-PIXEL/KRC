      SUBROUTINE VROTV ( VIN, AXIS, THETA,   ROUT )
C_Titl  VROTV  Vector rotation about another vector
      IMPLICIT NONE
C_Args
      REAL*4   VIN  (3)   ! I   Vector to be rotated.
      REAL*4   AXIS (3  ) ! I   Axis of the rotation
      REAL*4   THETA      ! I Angle of rotation (radians)
      REAL*4   ROUT (3)   ! O Result of rotating V about AXIS by THETA
C_Calls    VADD  VCROS  VFDOT  VEQUAL VFMAG  VSUB   
C_History
C 2003feb04 Hugh Kieffer  Derived IDL version from NAIF vrotv.f
C 2012may31 HK  Derive from IDL version using local .f library
C 2012jun03 HK  Extensive testing. 
C
      REAL*4 VFMAG,VFDOT              ! called functions
      INTEGER I
      REAL*4 AXMAG,Q,CT,ST ! ,  Q2,q3
      REAL*4 RPLANE (3)
      REAL*4 P      (3) ! ,  p3(3)
      REAL*4 V1     (3)
      REAL*4 V2     (3)
      REAL*4 X      (3)
 
      AXMAG=VFMAG( AXIS ) ! length of AXIS, used to normalize it
C
      IF (AXMAG .EQ. 0.) THEN ! if axis has zero length
         CALL VEQUAL (VIN, ROUT)     ! return the input vector
         RETURN
      END IF
C 
      DO I=1,3                  ! equiv to VHAT(AXIS,X)
         X(I)=AXIS(I)/AXMAG     !  unit vector in the direction of axis
      ENDDO
C
C     Compute the projection of V onto AXIS.  Call it P.
C      CALL VPROJ ( V, X, P )   sub VPROJ ( A, B, P )
C  t=a/maxa  r=b/maxb || scale= vdot(T,R)*maxa / cdot(R,R)==r^2
C  VSCL (scale,R,P)  == out=scale*R
      Q= VFDOT (VIN,X)            ! dot product with length of VIN 
C-      q2=(VFDOT(VIN,X)/VFDOT(X,X))*VFMAG(X) ! full math
C-      Q3= VFDOT(VIN,AXIS)/(AXMAG*AXMAG) ! simplist implimentation
      DO I=1,3                  !
         P(I)=Q*X(I)
C         P(I)=Q3*AXIS(I)
      ENDDO     
C
      CALL VSUB (VIN,P,  V1 ) ! component of V orthogonal to AXIS
C
      CALL VCROS (X,V1, V2) ! Rotate V1 by 90 degrees about the AXIS 
C
C     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about
C     the AXIS in the plane normal to the axis, call the result RPLANE
C     CALL VLCOM ( C, V1, S, V2, RPLANE )
      CT = COS (THETA)
      ST = SIN (THETA) 
      DO I=1,3
         RPLANE(I)=CT*V1(I) + ST*V2(I)
      ENDDO
C
C     Add the rotated component in the normal plane to AXIS to the
C     projection of V onto AXIS (P) to obtain R.
C
      CALL VADD (RPLANE,P, ROUT )

CD      i=6
CD 33   format(a6,'=[',f9.5,',',f9.5,',',f9.5,']')
CD         write(i,33)' V',vin
CD         write(i,33)'axis ',axis
CD         print *,'theta+',theta,c,s
CD         print *,'axmag',axmag
CD         write(i,33)' X',x
CD         print *,' Q=',q,q2,q3
CD         write(i,33)' p',p
CD         write(i,33)'P3',p3
CD         write(i,33)'V1',v1
CD         write(i,33)'V2',v2
CD         write(i,33)'RP',rplane
CD         write(i,33)' out',rout
C
      RETURN
      END
