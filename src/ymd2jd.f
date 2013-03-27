	SUBROUTINE YMD2JD (IYEAR,IMON,IDAY, DJ)
C_TITLE	YMD2JD convert year, month, day to Julian date offset from 2,440,000
C_KEYS	TIME UTILITY PORB 
C_DESCR accounts for leap years.
C_BUGS Correct for 1900 to 2099
C_Arguments
C  IYEAR I* [I] year, e.g. 1985
C  IMON I* [I] month count, 1 thru 12
C  IDAY I* [I] day of month
C  JD R* [O] Julian day, offset from 2,440,000, at 0 hours GMT (start of day)
C_HISTORY	85sept13 HHK original version
C_Calls 0
C_END
	DIMENSION ID0(12)
	DATA ID0 /0,31,59,90,120,151,181,212,243,273,304,334/
	DATA IFEB29,I1YEAR,I4YEAR,ILEAP, DJ50
     1    /    60,   365,  1461,  790, -6718.5/
C leap day = ILEAP = 1952 Feb 29

	J = IYEAR - 1950	! number of years from base year
	N4YEAR = J/4		! number of 4-year blocks
	J4 = N4YEAR*I4YEAR	! days in 4-year blocks
	J = J - 4*N4YEAR	! remaining number of years
	J = J*I1YEAR + ID0(IMON) + IDAY	! remaining number of days
	IF (J.GT.ILEAP) J=J+1	! test if a leap day in remaining days
	DJ = DJ50 + (J4 + J)	! Julian date offset from 2,440,000.0
	RETURN
	END
