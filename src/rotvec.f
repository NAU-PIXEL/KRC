
        SUBROUTINE ROTVEC (B,V, U)
C_Title  ROTVEC Apply rotation matrix to rotate a vector   !  U =  B *  V
C_Arguments
        REAL*4 B(9)     ![I] 3x3 Rotation matrix.
        REAL*4 V(3)     ![I] Original vector.
        REAL*4 U(3)     ![O] Rotated vector, must not be same array as V.
C_Keys
C_Desc  straight-forward product of rotation matrix and original vector.
C_Calls 0
C_History       1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C  2004sep28 HK Rename  rotate.f  to avoid conflict with NumRec routine
C_Pause
        DO I=1,3
          U(I)=B(I)*V(1)+B(I+3)*V(2)+B(I+6)*V(3)
          ENDDO
        RETURN
        END
