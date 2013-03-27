	SUBROUTINE MPRINT (ID,A,IOP)
C_Title	MPRINT Print a 3x3 matrix with ID;  E format
C_Args
	CHARACTER*(*) ID 	![I] Character variable identification.
	REAL*4 A(9)		![I] 3x3 array to print.
	INTEGER	IOP      	![I] Logical unit for printer
C_Keys	
C_Desc  Uses E15.6 format, so can handle any size elements.
C_Calls	0
C_Hist	1985April  H.Kieffer argument order revised
C	1985Oct15  H.Kieffer standard documentation
C_End
	WRITE(IOP,33) ID,(A(J),A(J+3),A(J+6),J=1,3)
33	FORMAT('0MPRINT dump of [',A,']',3(/5X,3E15.6))
	RETURN
	END
