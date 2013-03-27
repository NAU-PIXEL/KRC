      SUBROUTINE ROTV (A,N,R, B) ! b = [r_n]*a
C_Title  ROTV  Rotate a vector about a Cartesian axis
      REAL*4 a(3) ! in.  original vector
      INTEGER n   ! in.  Integer axis about which to rotate. 1=X, 2=Y, or 3=Z
      REAL*4 r    ! in.  Size of rotation, in radians.
      REAL*4 b(3) ! out. New vector
C_Desc  
C   Component along axis of rotation is not changed.
C   Other two components subject to simple rotation
C_Calls  none
C_Hist 2002dec11 Hugh Kieffer  IDL version
C 2012may31  HK  Fortran version
C_End

      C = COS(R)
      S = SIN(R)
!     Get indices as if rotation was about Z! which reduces this to rotation 
!     effectively in XY plane.  Must keep all MOD positive
      i=  mod(n+3,3)+1          ! 0-based location of effective X
      j=  mod(n+1,3)+1          ! " " Y
      k=  mod(n+2,3)+1          ! rotation axis 
      xin=a(i)                  ! effective X input
      yin=a(j)                  !  " Y "
      b(i)= c*xin - s*yin       ! do the rotation, put X in proper location
      b(j)= s*xin + c*yin       ! " Y "
      b(k)=a(k)                 ! component along rotation axis is unchanged
      RETURN
      END
