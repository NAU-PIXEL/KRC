	SUBROUTINE CATIME (DATETIME)
C_TITL  CATIME  Returns current date & time as " yyyy mon dd hh:mm:ss "
C_ARGS
	character*22 DATETIME	!out, date/time value
C_DESC  Uses SunPro specific calls get date/time, rearranges bytes
C
C_CALLS  TIME  CTIME
C
C_HIST  98jan22  Hugh_Kieffer character version derived from  DATIME
C_END
	integer j,time
	character*24 string,ctime

	j=time()		! seconds since 1970.0
	string=ctime(j)		! convert to string

C     123456789012345678901234
C     www mon dd hh:mm:ss yyyy   from ctime
C     _yyyy mon dd hh:mm:ss_     desired output
	datetime(1:1)=' '
	datetime(2:5)=string(21:24)
	datetime(6:22)=string(4:20)

	RETURN
	END
