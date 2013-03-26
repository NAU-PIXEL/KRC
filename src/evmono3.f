      SUBROUTINE EVMONO3 (CC,M,XX,XOFF,XMUL,YY)
C_Titl  EVMONO3  Evaluate monomial of 3rd degree for a vector input, with scaling
      IMPLICIT NONE
      REAL*4 CC(4)              ! in. coefficents
      INTEGER*4 M               ! number of independent values
      REAL*4 XX(M)              ! in. independent values
      REAL*4 XOFF               ! value to subtract from all  XX
      REAL*4 XMUL               ! multiply factor for offset  XX
      REAL*4 YY(M)              ! out. dependant values
C_Hist 2008nov04  Hugh  Kieffer: for thermal conductivity
C_End
      INTEGER*4 I,J
      REAL*4 S,X

      DO J=1,M                  ! each input value
         X=(XX(J)-XOFF)*XMUL
         S=CC(4)
         DO I=3,1,-1            ! standard reverse method
            S=S*X+CC(I)
         ENDDO
         YY(J)=S
      ENDDO

      RETURN
      END
