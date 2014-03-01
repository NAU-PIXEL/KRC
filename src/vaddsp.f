	SUBROUTINE VADD (U,V, W)		!  W =  U +  V
C_Title  VADD  Add two vectors of dimension 3.   single precision
C_Args
	REAL*4 U(*)	! [i] vector to be added.
	REAL*4 V(*)	! [i] vector to be added.
	REAL*4 W(*)	! [o] sum of two vectors; may overwrite input.
C_Keys  MATH  VECTOR
C_Desc  Add two vectors of dimension 3 resulting in a third vector.
C -----------------------------------------------------------------------
C  This is the top of a general  3-dimension vector geometry package.
C  There are four such files, single and double precision, 3 and  N dimensions
C  All routines similiar in all four files.
C    Argument order in all routines is:  INPUT _space_  OUTPUT
C  Output vector may validly be the same location as input vector for:
C routines:  VNORM_  VSUB_  VADD_  VNEG_  VSCALE_
C  VADDNDP is considered the primary version
C -----------------------------------------------------------------------
C_Hist	75-----  Hugh_H_Kieffer  VECTSP.FOR.  Revised 1982Jan12 84May24 84Oct26
C   85Apr12  HK  Change argument order from algebraic 
C   85Oct17  HK  Change variable symbols; document to  NIMS standards
C   91dec27  HK  Add  VMAGN; treat  VADDNDP as primary file
C 2013jun16  HK  Modify only comment typos and format
C_Paus

	DO I=1,3
		W(I) = U(I) + V(I)
		ENDDO
	RETURN
	END


	SUBROUTINE VCROSS (U,V, W)		!  W =  U X  V
C_Title  VCROSS  Cross product of two vectors of dimension 3.
C_Args
	REAL*4  U(*)	! [i] first vector.
	REAL*4  V(*)	! [i] second vector.
	REAL*4  W(3)	! [o] cross product; must not overwrite input.
C_Keys  MATH  VECTOR
C_Desc  Calculates the cross product of two vectors.
C_Hist  90dec27  Hugh Kieffer    see comments in first routine
C 2013jul11 HK complete recode for 3 dimesions
C_Paus
	W(1) = U(2)*V(3) - U(3)*V(2)
	W(2) = U(3)*V(1) - U(1)*V(3)
	W(3) = U(1)*V(2) - U(2)*V(1)
	RETURN
	END


	SUBROUTINE VDOT (U,V, R)		!  R =  U .  V
C_Title  VDOT  Calculates the dot product of two vectors of dimension 3.
C_Args
	REAL*4  U(*)	! [i] first vector.
	REAL*4  V(*)	! [i] second vector.
	REAL*4  R	! [o] dot product of two vectors.
C_Keys  MATH  VECTOR
C_Desc  Produces the dot product of two vectors of dimension 3,  R=U.V
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
	REAL*4 SUM
	SUM = 0.E0
	DO I=1,3
	SUM  = SUM + U(I)*V(I)
		ENDDO
	R = SUM
	RETURN
	END


	SUBROUTINE VEQUAL (V, W)		!  W =  V
C_Title  VEQUAL  Equate one vector of dimension 3 to another.
C_Args
	REAL*4  V(*)	! [i] initial vector 
	REAL*4  W(*)	! [o] vector equal to input
C_Keys  MATH  VECTOR
C_Desc  Equates a vector of dimension 3 to another vector.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
	DO I=1,3
		W(I)=V(I)
		ENDDO
	RETURN
	END


	SUBROUTINE VMAG (V, R)		!  R =  |V|
C_Title	VMAG   Get magnitude (length) of a vector of dimension 3.
C_Args
	REAL*4  V(3) ! [I] Vector to be normalized
	REAL*4  R    ! [O] Vector length
C_Keys	MATH VECTOR
C_Desc  Finds length of a vector of dimension 3.  R = |V|.
C_Call	0
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
	R = 0.E0
	DO I=1,3
		R = R + V(I)**2
		ENDDO
	R = SQRT(R)
	RETURN
	END


	SUBROUTINE VNEG (V, W)		!  W =  -V
C_Title  VNEG  Negate each element of a vector of dimension 3.
C_Args
	REAL*4  V(*)	! [i] vector to be negated
	REAL*4  W(*)	! [o] vector of negated elements; 
C			may overwrite input
C_Keys  MATH  VECTOR
C_Desc  Each element of the input vector is copied with a change of sign.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus

	DO I=1,3
		W(I)=-V(I)
		ENDDO
	RETURN
	END


	SUBROUTINE VNORM (V, W)		!  W =  V/|V|
C_Title  VNORM  Normalize a vector of dimension 3.
C_Args
	REAL*4  V(*)	! [i] vector to be normalized
	REAL*4  W(*)	! [o] vector of unit length; may overwrite input
C_Keys  MATH  VECTOR
C_Desc  Normalizes a vector of dimension 3 to unit length.  W=V/|V|.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
	REAL*4 SUM
	SUM = 0.E0
	DO I=1,3
		SUM = SUM +V(I)**2
		ENDDO
	SUM = 1.E0/SQRT(SUM)
	DO I=1,3
		W(I) = SUM * V(I)
		ENDDO
	RETURN
	END

	SUBROUTINE VPRF (ID,V,IOP,FMT)
C_Title  VPRF  Print a vector of dimension 3 in user format.
C_Args
	CHARACTER*(*) ID ! [i] character variable identification
	REAL*4 V(*)	! [i] vector to print
	INTEGER  IOP	! [i] logical unit for printer
	CHARACTER*(*) FMT ![i] format to use for vector. this should be only
C			the core of the format, e.g.   '8F8.3' 
C_Keys  MATH  VECTOR
C_Desc  Prints a vector of dimension 3 to the logical unit specified
C	and in the format specified (up to 30 characters).
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
	CHARACTER*60 FMT2
	FMT2 = '(1X,A,''='',(/1X,' // FMT // '))'
	WRITE (IOP,FMT2) ID,(V(I),I=1,3)
	RETURN
	END


	SUBROUTINE VSCALE (R,V, W)		!  W =  R *  V
C_Title  VSCALE  Multiply a vector of dimension 3 by a constant.
C_Args
	REAL*4  R	! [i] constant multiplier.
	REAL*4  V(*)    ! [i] vector to be multiplied.
	REAL*4  W(*)	! [o] product of a vector and a constant; 
C			may overwrite input vector
C_Keys  MATH  VECTOR
C_Desc  Multiplies a one dimensional array of dimension 3 by a constant  R 
C producing a third one dimensional array w.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus

	DO I=1,3
		W(I)=R*V(I)
		ENDDO
	RETURN
	END


	SUBROUTINE VSHOW (ID,V,IOP)
C_Title  VSHOW  Print a vector of dimension 3 as cartesion and spherical angles
C_Args
	CHARACTER*(*) ID	! [i] character variable identification
	REAL*4 V(3)		! [i] vector to print
	INTEGER IOP		! in. Logical unit. Negative to print col headings
C_Keys  MATH  VECTOR
C_Desc  Prints a vector of dimension 3 to default unit
C_Hist  2013jun16  Hugh Kieffer.  See comments in first routine
C_Paus
 	REAL*4 RAD /57.295780/  ! radian to degree
	CHARACTER*60 FMT
	FMT='(3g13.5,2x,2f8.2,g12.5,1x,A)'
	CALL COCOCS(V, P,Q,R)	!  colat and  East longitude in radians, length
	QD=Q*RAD		! convert longitude to degrees
	PD=90.-P*RAD		! convert co-lat to latitude degrees
	J=ABS(IOP)		! logical unit to use
	IF (IOP.LT.0) WRITE(J,*)'C_END  X          Y           Z'
     &,'          Lat    E.Lon    Radius     What'
	WRITE (J,FMT) (V(I),I=1,3),QD,PD,R,ID
	RETURN
	END


	SUBROUTINE VSUB (U,V, W)			!  W =  U -  V
C_Title  VSUB   Find difference of two vectors of dimension 3
C_Args
	REAL*4  U(*)	! [i] vector, initial
	REAL*4  V(*)	! [i] vector to be subtracted
	REAL*4  W(*)	! [o] difference of two input vectors; 
C			may overwrite input
C_Keys  MATH  VECTOR
C_Desc  Difference of two vectors of dimension 3.  W=U-V.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus

	DO I=1,3
		W(I)=U(I)-V(I)
		ENDDO
	RETURN
	END


	SUBROUTINE VSUBR (U,V, W)			!  W =  U -  V
C_Title  VSUBR  Find reduced-precision difference of two vectors of dimension 3
C_Args
	REAL*8  U(*)	! [i] vector, initial
	REAL*8  V(*)	! [i] vector to be subtracted
	REAL*4  W(*)	! [o] difference of two input vectors; 
C_Keys  MATH  VECTOR
C_Desc  Difference of two vectors of dimension 3.  W=U-V.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus

	DO I=1,3
		W(I)=U(I)-V(I)
		ENDDO
	RETURN
	END


	SUBROUTINE VUNIT (IAX, U)
C_Title  VUNIT  Construct unit vector of dimension 3 along one axis 
C_Args
	INTEGER IAX	! [i] axis along which vector will point.
	REAL*4  U(*)	! [o] unit vector along axis.
C_Keys  MATH  VECTOR
C_Desc  Creates a unit vector of dimension 3 along a  Cartesian axis.
C     if  IAX is not within  1:N, then a zero vector is returned.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_End

	DO I=1,3
		U(I)=0.E0
		ENDDO
	IF (IAX.GE.1 .AND. IAX.LE.3) U(IAX)=1.E0
	RETURN
	END
