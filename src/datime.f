	SUBROUTINE DATIME (DATETIME)
C_TITL  DATIME  Returns current date and time as ccyy-mon-dd_hh:mm:ss in 5 4-byte words
C 
C_ARGS
	REAL*4 DATETIME(5)	!out, date/time value
                                ! in the format yyyy-mon-dd_hh:mm:ss
C
C_DESC 
C
C_CALLS  Intrinsic function TIME  CTIME,   B2B
C
C_HIST  97jan30  Hugh_H_Kieffer complete rewrite of older version
C	98jan22  HHK revise; fdate had changed
C     2012may09  HK Add century and drop leading and trailing blanks
C_END
	CHARACTER*24 STRING     ! to hold  return
	CHARACTER*24 CTIME      ! Intrinsic function
	INTEGER*4 J
	INTEGER*4 TIME ! system function
        BYTE IBUF(20)

	J=TIME()                ! get system time
	STRING=CTIME(J)         ! convert to character

C     123456789012345678901234
C     www mon dd hh:mm:ss yyyy   from ctime
C     yyyy mon dd hh:mm:ss        desired output

        CALL B2B (STRING(21:24),IBUF,4) ! move the year
        CALL B2B (STRING(4:19),IBUF(5),16) ! move the rest
        CALL B2B (IBUF,DATETIME,20) ! transfer to R*4 array

	RETURN
	END
C potential time intrinsic functions: DATE_AND_TIME IDATE ITIME SECNDS
