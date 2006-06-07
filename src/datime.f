	SUBROUTINE DATIME (DATETIME)
C_TITL  UTIME  Returns current date and time as  _yy-mon-dd_hh:mm:ss_
C 
C_ARGS
	REAL*4 DATETIME(5)	!out, date/time value
                                ! in the format _yy-mon-dd_hh:mm:ss_
C
C_DESC  Uses SunPro specific calls to create date/time of the
C       form " yy-mon-dd hh:mm:ss ". 
C
C_CALLS  FDATE
C
C_HIST  97jan30  Hugh_H_Kieffer complete rewrite of older version
C	98jan22  HHK revise; fdate had changed
C_END
	character*24 string,ctime     ! to hold fdate return
	integer j,time
        byte IBUF(20)

	j=time()
	string=ctime(j)

C     123456789012345678901234
C     www mon dd hh:mm:ss yyyy   from ctime
C      yy mon dd hh:mm:ss       desired output

        call b2b (string(1:1),ibuf,20) ! move into byte buffer
        call b2b (string(23:23),ibuf(2),2)
	ibuf(1)=ichar(' ')
        call b2b (ibuf,datetime,20)

	RETURN
	END
