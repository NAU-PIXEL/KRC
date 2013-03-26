	REAL FUNCTION RNDEX (XIN,A,N)
C_Titl  RNDEX  Finds floating-point index of input argument within a R*4 array
C function set to zero if the range is zero;
C function set to negative of nearest index if  XIN  not within  A(1)  to  A(N)
C_Args
	REAL*4  XIN	! [i] value to be located within the array
	REAL*4  A(1)	! [i] array to be searched; should be monotonic;
C				but can be of either slope.
	INTEGER  N	! [i] size of array
C_Desc  Uses binary search, and then linear interpolation within last interval.
C_Calls  0
C_Hist  83Dec05  Hugh_H_Kieffer
C	87dec15  HHK convert from exhaustive search to binary search
C_End

C determine slope. set  L &  M to index of low & high ends
	IF (A(N)-A(1)) 5,90,8
C negative slope
5	L=N
	M=1
	GOTO 10
C positive slope
8	L=1
	M=N
10	X=XIN				! transefer to local variable
	IF (X-A(L)) 91,11,12		! test at low end
11	I = L				! equaled lowest point
	GOTO 80
12	IF (X-A(M)) 20,13,92		! test at high end
13	I = M				! equaled highest point
	GOTO 80

C===================================================================
20	I = (L+M)/2			! bisect the remaining table
	IF (X-A(I)) 31,80,32		! test against bisection point
31	M = I				! replace upper pointer
	GOTO 40
32	L = I				! replace lower pointer
40	IF (IABS(M-L).GT.1) GOTO 20	! remaining table still too big
C===================================================================
C  X falls within l:m interval. 
	F = (X-A(L))/(A(M)-A(L))	! fraction of way from  A_L to  A_M
	IF (M.GT.L) THEN
		RNDEX = FLOAT(L) + F	! increasing table
	    ELSE
		RNDEX = FLOAT(L) - F	! decreasing table
	    ENDIF
9	RETURN

C formal errors.
90	I = 0				! input table endpoints equal
	GOTO 80
91	I = -L				! request below end of table
	GOTO 80
92	I = -M				! request above end of table

80	RNDEX = FLOAT(I)		! request may = a point in the table
	GOTO 9
	END
