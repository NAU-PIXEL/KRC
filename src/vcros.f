      SUBROUTINE VCROS (U,V, W) !  W =  U X  V
C_Title  VCROS  cross product of two vectors of dimension 3.
C_Args
      REAL*4  U(3)              ! [i] first vector.
      REAL*4  V(3)              ! [i] second vector.
      REAL*4  W(3)              ! [o] cross product
C_Keys  MATH  VECTOR
C_Desc  Calculates the cross product of two vectors.
C   use temporary vector in case caller  W in same location  U or  V
C_Hist  2012jun02  Hugh Kieffer  Celestial reasonings
C_END
      REAL*4  Y(3)              ! temporary
C cross product into temporary    
      Y(1) = U(2)*V(3) - U(3)*V(2)
      Y(2) = U(3)*V(1) - U(1)*V(3)
      Y(3) = U(1)*V(2) - U(2)*V(1)
C move temporary to output      
      W(1) = Y(1)
      W(2) = Y(2)
      W(3) = Y(3)
      
      RETURN
      END
