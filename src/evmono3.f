      SUBROUTINE EVMONO3 (CC,M,XX,XOFF,XMUL, YY)
C_Titl  EVMONO3  Evaluate monomial of 3rd degree for a vector input, with scaling
      IMPLICIT NONE
      REAL CC(4)              ! in.  Coefficents
      INTEGER M               ! in.  Number of independent values
      REAL XX(M)              ! in.  Independent values
      REAL XOFF               ! in.  Value to subtract from all  XX
      REAL XMUL               ! in.  Multiply factor for offset  XX
      REAL YY(M)              ! out. Dependant values
C_Hist 2008nov04  Hugh  Kieffer: for thermal conductivity
C 2010jan09 HK  Replace do loop for powers with explicit code 
C 2012feb26 HK  Remove unused variables
C_End
      INTEGER J
      REAL X

      DO J=1,M                  ! each input value
         X=(XX(J)-XOFF)*XMUL
         YY(J)=(((CC(4)*X)+CC(3))*X+CC(2))*X+CC(1)
      ENDDO

      RETURN
      END
