	SUBROUTINE MPROD3 (BB,CC, AA)		! [AA] = [BB] * [CC]
C_Title  MPROD3 Matrix product (hard-coded for size=3).
C_Arguments
	REAL*4 BB(3,3) 	![I] First matrix of product.
	REAL*4 CC(3,3)  ![I] Second matrix of product.
	REAL*4 AA(3,3)  ![O] Resulting matrix
C_Keys	
C_Desc  Straight-forward multiply and sum of elements.
C coded for size=3).  
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	DO I=1,3
	  DO J=1,3
	    AA(I,J)=BB(I,1)*CC(1,J)+BB(I,2)*CC(2,J)+BB(I,3)*CC(3,J)
	    ENDDO
	  ENDDO
	RETURN
	END
