	SUBROUTINE ROTDIA (R, B)		! [B] = R * [IDENTITY]
C_Title  ROTDIA Form diagonal matrix of magnitude  R.
C_Arguments
	REAL*4 R	![I] Magnitude of matrix.
	REAL*4 B(9)	![O] Diagonal 3x3 matrix (treated internally as 
			!9-vector)
C_Keys	
C_Desc Zero the matrix, replace diagonl elements with  R.
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	DO I=1,9
		B(I)=0.E0
		ENDDO
 	DO I=1,9,4
		B(I)=R
		ENDDO
	RETURN
	END
