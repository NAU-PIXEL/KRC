      SUBROUTINE CALDATE(DJIN, IYEAR,IMON,IDAY,FDAY,MONTH,DAY,IDATE)
C_Titl  CALDATE convert julian date (base 2440000) to year,month,day,day-of-week
      IMPLICIT NONE
C_Args
	REAL*4 DJIN	!i. julian date, base 2440000 or full (less accurate)
	INTEGER*4 IYEAR	!o. year
	INTEGER*4 IMON	!o. month of year, 1 through 12
	INTEGER*4 IDAY	!o. day of month, 1 through 31
	REAL*4 FDAY	!o. fraction of  GMT (civil) day.
	INTEGER*4 MONTH	!o. 4  ASCII bytes that contain ' mon'
	INTEGER*4 DAY	!o. 4  ASCII bytes that contain ' day' of week
	INTEGER*4 IDATE	!o. day-of-year
C_Desc
C  Work on any day after about 2000 BC
C civil days are offset from  JD by 12 hours.
C_Calls: Numerical Recipes:  CALDAT  JULDAY
C_Hist
C 2012feb29  HK Simplification of JDATE by use of  NumRec  caldat and julday
C_End

      REAL*8 DJBASE /2.44D6/	! 2440000  PORB system base day
C        INTEGER*4 JDBASE /2440000/  All days offset from this.

      REAL*8 FULLJD             ! full Julian date
      REAL*8 FDDAY              ! double precision fractional  GMT
      INTEGER*4 JULDAY          ! function
      INTEGER*4 JD0,IW,JULIAN         
      
      INTEGER*4 WDAY(7),MON(12)
      DATA WDAY /4H SUN,4H MON,4H TUE,4H WED,4H THU,4H FRI,4H SAT/
      DATA MON /4H JAN,4H FEB,4H MAR,4H APR,4H MAY,4H JUN,
     &	    4H JUL,4H AUG,4H SEP,4H OCT,4H NOV,4H DEC/

      FULLJD=DJIN
      IF (DJIN.LT.1.22D6) FULLJD=DJBASE+DJIN
C now have full Julian date, virtually certain to be positive

      JULIAN=IDINT(FULLJD)       ! get floor integer
      FDDAY=DMOD(FULLJD,1.D0)+0.5d0 ! double precision needed
      IF (FDDAY.GE. 1.D0) THEN  ! ensure fractional day <1.
         FDDAY=FDDAY-1.D0
         JULIAN=JULIAN+1
      ENDIF
      FDAY=SNGL(FDDAY)          ! from double to single precision
      
      CALL CALDAT(JULIAN, IMON,IDAY,IYEAR)
      MONTH=MON(IMON) ;         ! month as 3-characters
      JD0=JULDAY(1,1,IYEAR)     ! first day of the year
      IDATE=JULIAN-JD0+1        ! day of year
      
      IW=MOD(JULIAN-1,7)          ! day of week,0=Sunday
      DAY=WDAY(IW+1)            ! day of week as 3 characters

      RETURN
      END
