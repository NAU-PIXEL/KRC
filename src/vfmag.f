      REAL*4 FUNCTION VFMAG (V) !  R =  |V|
C_Title	VFMAG  get magnitude (length) of a vector of dimension 3.
C_Args
      REAL*4  V(3)              ! [I] Vector to be normalized
      REAL*4  R                 ! [O] Vector length
C_Keys	MATH VECTOR
C_Desc  Finds length of a vector of dimension 3.  R = |V|.
C_Call	0
C_Hist  2012may31 HK Derived from vaddsp.f subroutines
C_End
      R = 0.E0
      DO I=1,3
         R = R + V(I)**2
      ENDDO
      VFMAG = SQRT(R)
      RETURN
      END
