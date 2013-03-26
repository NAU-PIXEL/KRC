	SUBROUTINE SIGMA (BUF,N, AVE,SIG)
C_TITLE	SIGMA Computes mean and standard deviation of real array 
C_ARGS
	REAL*4 BUF(*)   ! [In] array
	INTEGER N 	! [In]  number of items in BUF
	REAL*4 AVE 	! [Out] average value of input array
	REAL*4 SIG	! [Out] standard deviation of input array
C		will be 0. if  N less than 2.
C_KEYS	MATH STATISTICS 
C_DESC Sums the values and value_squared for entire array, then computes 
C   mean and standard deviation from these sums.
C_CALLS 0
C_LIMS  Could overflow REAL sum_squares 
C_HIST	85Jan09  Hugh_H_Kieffer  U.S.G.S. Flagstaff  ORIGINAL  VERSION
C	86Dec06  HHK Revised to handle  N=1 properly (adapted from
C			byte version). 
C_END

	SIG=0.			! set here in case N<2
	IF (N.LT.1) THEN 	! not enough points 
		AVE=0.
		RETURN
	   ENDIF
	P=N		! need real number_of_points for formulas
	S=0.		! initialise the sums
	SS=0.
	DO I=1,N
		X=BUF(I)
		S=S+X		! sum the values
		SS=SS+X*X	! sum the values-squared
		ENDDO
	AVE=S/P			! average
	IF (N.GT.1) SIG=SQRT((SS-2.*AVE*S+P*AVE*AVE)/(P-1.))	! standard dev.
	RETURN
	END
