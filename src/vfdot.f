      REAL*4 FUNCTION VFDOT (U,V) !  R =  U .  V
C_Title  VFDOT calculates the dot product of two vectors of dimension 3.
      IMPLICIT NONE
C_Args
      REAL*4  U(*)              ! in. first vector.
      REAL*4  V(*)              ! in. second vector
C_Keys  MATH  VECTOR
C_Desc  Produces the dot product of two vectors of dimension 3,  R=U.V
C_Hist  2012may31 HK Derived from vaddsp.f subroutines
C_Pause
      INTEGER I
      REAL*4 SUM
      SUM = 0.E0
      DO I=1,3
         SUM  = SUM + U(I)*V(I)
      ENDDO
      VFDOT = SUM
      RETURN
      END
