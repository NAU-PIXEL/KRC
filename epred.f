        REAL FUNCTION EPRED (Y,X,IDIM,YMIN,YMAX)
C_Title:  EPRED  Exponetial Prediction of numerical iteration
C_Args:
        REAL*4 Y(IDIM,*) !in. array beginning with 2 before last value;
C                        !     assuming uniform spacing in x
	INTEGER IDIM	 ! spacing of points in y-array
        REAL*4 X         !in. prediction point,  FORWARD beyond 3rd point
        REAL*4 YMIN,YMAX !in. limits of allowed prediction range
C_Desc
C 3-point prediction; assumes asymptotic exponential form.
C   y = c0 + c1 * exp (c2 * x);  WHERE c2 is <0. or y=c0+c1*r^x where r<1.
C  IF not asymptotic (c2 >= 0.) does linear prediction using last 2 points
C  If  X <  XLIN (below)  then uses linear relation between first two points.
C  Thus, one can call this routine with only 2 y-values defined,
C   but must remember that the x-origin is at the [virtual] 3rd point.
C  However, MUST NOT call this routine with only 2 points and  X>=XLIN.
C_Lim
	REAL*4 XLIN / -0.9 / ! minimum value for exponential form
Calls: none
C_Hist h.kieffer 1984jun03
c  98sep03  HHK make linear for  X<XLIM, which allows use with 2 points
C_End&789012345678901234567890123456789012345678901234567890123456789012_456789

C d=y3-y2   dl=y2-y1   r=(y3-y2)/(y2-y1)
C c1 = d/(1 - 1/r)    c0 = y3 - c1    y=y3+[d/(1/r -1)](1-r^x)

        DL = Y(1,2) - Y(1,1)    ! slope between first 2 points
        IF (X .LT. XLIN) THEN   ! y3 may not be defined
           YP=Y(1,2)+(X+1.)*DL  ! linear fit to first 2 points
           GOTO 9
        ENDIF

        YP = Y(1,3)              ! last good point
        IF (DL.EQ.0.) GOTO 9    ! no change between last two points

        D = YP - Y(1,2)
        R = D/DL                ! ratio of successive changes
        IF (R.GT.0.0 .AND. R.LT.1.0) THEN ! asymptotic form
           YP = YP + (D/(1./R-1.) ) * (1. -R**X)
        ELSE                    ! linear form
           YP = YP + X*D
        ENDIF
 9      EPRED = MIN( YMAX, MAX (YP,YMIN)) ! check bounds
        RETURN
        END
