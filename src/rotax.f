	SUBROUTINE ROTAX  (N,R, B)		! [B] = [R*|N|]*[B]
C_Title  ROTAX Change rotation matrix to include additional rotation 'R'
C_Arguments
	INTEGER N	![I] Axis about which to rotate.
	REAL*4 R	![I] Size of rotation, in radians.
	REAL*4 B(9) 	![B] Rotation matrix (treated internally as 9-vector).
C_Keys	
C_Desc  
C multiply appropriate elements of input rotation matrix by the
c sine and cosine of additional angle.   e.g. If start with matrix  AB 
C (from  B to  A), output will be  A'B, where  A' is reached from  A by 
C rotation of angle  R around axis  N of the  A system.
C_Calls	0	
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	REAL*4 E,F,G
	E = COS(R)
	F = SIN(R)
	I2=MOD(N,3)
	I3=MOD(N+1,3)
	DO I=1,7,3
		G=B(I+I2)
		B(I+I2)=E*G       +F*B(I+I3)
		B(I+I3)=E*B(I+I3) -F*G
		ENDDO
	RETURN
	END
