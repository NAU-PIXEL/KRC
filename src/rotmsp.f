C_TITLE   ROTMSP   General 3-dimension rotation matrix geometry package.
C_DESCRIPTION  ROTMSP is a general 3-dimension rotation matrix geometry
C package with many routines. 
C Argument order is: input _SPACE_ output,
C Angular arguments are in radians; except the pointing triple is in degrees.
C Matrices are assumed stored as:  AA(I,J) = AA(row,column) = A(I+3(J-1)).
C
C Output argument may overwrite input argument for routine TRANS3.
C
C  AA,BB,CC and  A,B,C are 3X3 matrices;  U,V are 3-vectors;
C  R, are argument scalars;  E,F,G are internal scalars.
CC ** This is a single precision version; to generate double precision
CC **   version, make the following changes throughout:
CC **  REAL*4  >  REAL*8
CC **  ASIN, ATAN2  >  DASIN, DTAN2
CC **  E0  >  D0       E15.6  > D22.13
C_HIST	85APR17  Hugh_H._Kieffer  U.S.Geological_Survey
C	91apr17  HHK reorder, use  ENDDO
C	91mar04  HHK include  ROTEX_ routines
C_PAUSE


	SUBROUTINE ROTORB (ODE,CLIN,ARGP, A)
C_Titl  ROTORB construct rotation matrix from classic orbital elements
C_Args
	REAL*4 ODE	! in. node of orbit <degrees>
	REAL*4 CLIN	! in. inclination of orbit <degrees>
	REAL*4 ARGP	! in. argument of periapsis <degrees>
	REAL*4 A(9)	! out. rotation matrix, from orbital to reference.
C_Desc
C does 3  Euler rotations.   orbital elements input must be specified in the
C   cordinate system to be the "to" orientation of the rotation matrix.
C_Hist	87oct11  Hugh_H_Kieffer  U.S.G.S._Flagstaff
C_Call  ROTDIA  ROTAX
C_End
	PARAMETER (RPD=.01745329251994329577E0)	! radians per degree

	CALL ROTDIA (1., A)	! initialize as identity matrix
C the 3  Euler rotations required are:
CC A = (-node)Z * (-inclination)X * (-argument of periapsis)Z
	CALL ROTAX (3, -ARGP*RPD, A)
	CALL ROTAX (1, -CLIN*RPD, A)
	CALL ROTAX (3, -ODE*RPD, A)

	RETURN
	END


	SUBROUTINE ROTMAT (V, A)
C_Title  ROTMAT Derive rotation matrix from pointing triple.
C_Arguments
	REAL*4 V(3)	![I] Pointing triple  (RA, DEC, TWIST) in degrees.
	REAL*4 A(9)	![O] Corresponding 3x3 rotation matrix.
C_Keys  VECTOR  ROTATION  MATRIX
C_Desc  Begins with a diagonal matrix, then does two  Euler rotations to the
C pointing vector
C_Calls	ROTDIA ROTAX
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	PARAMETER (RPD=.01745329251994329577E0)	! RADIANS PER DEGREE

	CALL ROTDIA (1.E0, A)
	CALL ROTAX  (3,RPD*(V(1)+90.), A)
	CALL ROTAX  (1,RPD*(90.-V(2)), A)
	IF (V(3) .NE. 0.E0) CALL ROTAX (3,RPD*(V(3)), A)
	RETURN
	END


	SUBROUTINE ROTATE (B,V, U)
C_Title  ROTATE Rotate a vector 	!  U =  B *  V
C_Arguments
	REAL*4 B(9)	![I] 3x3 Rotation matrix.
	REAL*4 V(3)	![I] Original vector.
	REAL*4 U(3) 	![O] Rotated vector, must not be same array as V.
C_Keys	
C_Desc  straight-forward product of rotation matrix and original vector.
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	DO I=1,3
	  U(I)=B(I)*V(1)+B(I+3)*V(2)+B(I+6)*V(3)
	  ENDDO
	RETURN
	END


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


	SUBROUTINE ROTRIP (A, V)
C_Title  ROTRIP Converts rotation matrix to pointing triple.
C_Arguments
	REAL*4 A(9)	![I] 3x3 Rotation matrix.
	REAL*4 V(3)	![O] Pointing triple in astronomic convention, 
			!    in degrees.
C              (1) is Right Ascension  (2) is Declination  (3) is Twist.
C_Keys	
C_Desc  Takes trig functions of combinations of elements of the matrix, 
C and scales them to degrees.
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause

	PARAMETER (DPR=57.2957795130823208768E0)  ! DEGREES PER RADIAN

	V(1) = DPR * ATAN2 (A(6),A(3))	! right ascension
	V(2) = DPR * ASIN  (A(9))	! declination
	V(3) = DPR * ATAN2 (A(7),A(8))	! twist
	RETURN
	END


	SUBROUTINE ROTROW (A,N, V)	! EXTRACT N'TH ROW FROM [A]
C_Title  ROTROW Extract  N'th row from a 3x3 matrix.
C_Arguments
	REAL*4 A(9)	![I] Rotation (3x3) matrix.
	INTEGER N       ![I] Row to extract.
	REAL*4	V(3)	![O] Row extracted.
C_Keys	
C_Description  Just move the 3 elemnts of the row into a vector.
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	V(1) = A(N)
	V(2) = A(N+3)
	V(3) = A(N+6)
	RETURN
	END


	SUBROUTINE ROTZXM (Z,X, A)
C_Title  ROTZXM Make rotation matrix from vectors along Z-axis, and in X-Z plane
C_Arguments
	REAL*4 Z(3)	![I] Vector along Z axis (need not be normalized).
	REAL*4 X(3) 	![I] Vector in the X-Z plane (need not be normalized).
	REAL*4 A(9)	![O] 3x3 Rotation matrix to reference system, from
		 	!orientation specified by Z and X.
C_Keys
C_Desc Construct a rotation matrix from system in which  Z and  X are specified
C to a sytem in which the  Z axis is along the input  Z vector, and the  X axis
C is in the plane definded by the input  Z and  X vectors.
C Finds the new  Y axis as the normalized cross product of the input  Z  and X.
C_Lim
C Degenerate if the  X vector is parallel to  Z.
C_Calls	VNORM VCROSS
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause

	CALL VNORM  (Z, A(7))		! unit vector along Z-axis
	CALL VCROSS (Z,X, A(4))		! vector in direction of Y-axis
	CALL VNORM  (A(4), A(4))	! unit vector along Y-axis
	CALL VCROSS (A(4),A(7), A(1))	! unit vector along X-axis
	RETURN
	END


	SUBROUTINE TRANS3 (B, A)		! A = B_TRANSPOSE
C_Title  TRANS3 Transpose a 3x3 matrix,  A and  B may be same array.
C_Arguments
	REAL*4 B(9)	![I] 3x3 matrix.
	REAL*4 A(9)	![O] transposed matrix.
C_Keys	
C_Description  The subroutine TRANS3 will transpose a 3x3 matrix; the
C original and the transposed matrix may be the same array.
C this routine coded to allow expansion to any size matrix.
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	REAL*4 E
	DATA M,MP,MM/3,4,9/	! M = matrix size, MP = M+1, MM=M*M 
	DO I=1,MM,MP		! transfer diagonal
		A(I)=B(I)
		ENDDO
	DO II=2,M		! do increasing off-diagonal bands
	  IOFF=(II-1)*(M-1)
	  III=M*(MP-II)
	  DO I=II,III,MP
		IT=I+IOFF
		E=B(IT)		! save one element so transpose
		A(IT)=B(I)	!  may overwrite input array
		A(I)=E
		ENDDO
	  ENDDO
	RETURN
	END


	SUBROUTINE ROTEXM (IX,IZ,U, V)
C_Title  ROTEXM modify rotation matrix to new system with axes interchanged
C_Arguments
	INTEGER IX	![i] axis in old system along which new x-axis falls;
C			  1=X  2=Y  3=Z, minus for negative axis.
	INTEGER IZ	![i] axis in old system along which new z-axis falls.
	REAL*4 U(9)	![i] original rotation matrix.
	REAL*4 V(9) 	![o] rotated matrix, must not be same array as  U.
C_Keys
C_Description  Determines direction and sign of new  Y axis, then 
C exchanges rows (with possible change of sign) to generate new matrix.
C_Calls	0
C_Lims does not check for valid values of  IX &  IZ, 
C_History	91jul29  Hugh_H_Kieffer  USGS_flagstaff original version
C_Pause
	INTEGER NEW(3)	! will contain  IX,IY,IZ.
	INTEGER NEA(3)	! will contain absolute value of  NEW.

	NEW(1) = IX
	NEW(3) = IZ
	DO I=1,3,2
	  NEA(I) = IABS(NEW(I))
CC	  IF (NEA(I).LT.1 .OR. NEA(I).GT.3) GOTO 81  ! check for valid values
	  ENDDO
CC	IF (NEA(1).EQ.NEA(2) GOTO 82		! no axes may be same.

C find right-handed  Y axis.   there are 6*4=24 possibile right-handed systems
C sum of 3 axes must be 6; this allows easy determination of which axis is  Y
C then, 3 independant binary tests for sign of  Y axis
	NEA(2) = 6-NEA(1)-NEA(3)
	IY = NEA(2)
	NZ = MOD(NEA(3),3)	    ! get  Z below  X
	IF (NEA(1).NE.NZ+1) IY=-IY  ! if  X follows  Z, then  Y must follow  X
	IF (IX.LT.0) IY=-IY	    ! but reverse if  X or  Z is along neg axis.
	IF (IZ.LT.0) IY=-IY
	NEW(2) = IY

	DO I=1,3
	  K = NEA(I)
	  IF (NEW(I).GT.0) THEN
	    DO J=0,2
		V(I+3*J) =  U(K+3*J)
		ENDDO
	  ELSE
	    DO J=0,2
		V(I+3*J) = -U(K+3*J)
		ENDDO
	  ENDIF
	  ENDDO
	RETURN
	END


	SUBROUTINE ROTEXV (IX,IZ,V1, V2)
C_Title  ROTEXV rotate a vector to system with axes interchanged
C_Arguments
	INTEGER IX	![i] axis in old system along which new x-axis falls;
C			  1=X  2=Y  3=Z, minus for negative axis.
	INTEGER IZ	![i] axis in old system along which new z-axis falls.
	REAL*4 V1(3)	![i] original vector.
	REAL*4 V2(3) 	![o] rotated vector, must not be same array as  V1.
C_Keys
C_Description  checks for valid values of  IX &  IZ, determines proper 
C exchanges of coordinates, and moves to output vector.
C_Calls	0
C_lims  Does not check for valid input values.
C_History	91jul29  Hugh_H_Kieffer usgs_flagstaff original version
C_Pause
	KX = IABS(IX)
	KZ = IABS(IZ)

C check for validity.
CC	IF (KX.LT.1 .OR. KZ.LT.1 .OR. KX.GT.3 .OR. KZ.GT.3) GOTO 81
CC	IF (KX.EQ.KZ) GOTO 82

C find right-handed  Y axis. there are 6*4=24 possibile right-handed systems
C sum of 3 axes must be 6; this allows easy determination of which axis is  Y
C then, 3 independant binary tests for sign of  Y axis
	KY = 6-KX-KZ
	IY = KY
	NZ = MOD(KZ,3)		! get  Z below  X
	IF (KX.NE.NZ+1) IY=-IY	! if  X follows  Z, then  Y must follow  X
	IF (IX.LT.0) IY=-IY	! but reverse if  X or  Z  IS along neg axis.
	IF (IZ.LT.0) IY=-IY

	IF (IX.GT.0) THEN
	  V2(1) =  V1(KX)
	ELSE
	  V2(1) = -V1(KX)
	ENDIF
	IF (IY.GT.0) THEN
	  V2(2) =  V1(KY)
	ELSE
	  V2(2) = -V1(KY)
	ENDIF
	IF (IZ.GT.0) THEN
	  V2(3) =  V1(KZ)
	ELSE
	  V2(3) = -V1(KZ)
	ENDIF

	RETURN
	END




	SUBROUTINE MEQUAL (B, A)		! [A] = [B]
C_Title  MEQUAL Equate one 3x3 matrix to another.
C_Arguments
	REAL*4 B(9) 	![I] Input matrix.
	REAL*4 A(9)	![O] Output matrix.
C_Keys	
C_Description  Simple copy of inpout array.
C_Calls	0
C_History	1985April H.Kieffer argument order revised
C               1985Oct15 H.Kieffer standard documentation
C_Pause
	DO I=1,9
		A(I)=B(I)
		ENDDO
	RETURN
	END


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
