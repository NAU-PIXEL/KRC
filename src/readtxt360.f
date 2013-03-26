      INTEGER FUNCTION READTXT360 (fname,iod, xxx,yyy)
C_Titl  READTXT360  Read 2-column text file of REAL values
      IMPLICIT  None
      integer*4 mrow,mskip
      parameter (mrow=362)      ! max number of table entries
      parameter (mskip=10)      ! max number of lines to skip through C_END
      integer*4 ioerr             ! logical unit for reporting errors
      parameter(ioerr=6)
C_Vars
C_Args
      CHARACTER*80 fname         ! in. File name
      integer*4 iod               ! in. Available logical unit
      REAL*4 xxx(*)             ! both.  Values from first column, [date]
      REAL*4 yyy(*)             ! both.  Values from 2nd  column
      integer*4 kk                ! Function out Number of defined entries

C_Desc
C Reads an Text table of two columns, white-space separated
C Columnar table must be immediately preceeded by a line beginning:  C_END
C Before that may be up to 9 lines of free-form comments
C Assumes first column is Modulo 360, and prepends and appends values
C  to avoid later wrap-around
C Returns -1 if error occurs
C_Hist  Hugh_Kieffer  2006sep09
C 2008mar31 HK integer >> integer*4
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C Local variables
      character*80 rbuf
      real*4 xin,yin
      integer*4 iost              ! error status
      integer*4 i,j

      kk=-1                   ! possible error flag
      
D      write(*,*)'FNAME=',fname
      OPEN (UNIT=IOD,FILE=Fname,STATUS='OLD',iostat=iost,err=81)
C     Skip past the C_END line
      do i=1, mskip
        READ (IOD,'(A80)',ERR=82,END=30) RBUF ! read into character buffer
D        WRITE (*,*) I,RBUF
        J=INDEX(RBUF,'C_END')
        IF (J.GT.0 .AND. J.LT.5) GOTO 30
      ENDDO

C read single lines until a negative season [ indicating all done]
 30   do i=2, mrow-1
        READ (IOD,*,ERR=83,END=40), xin,yin 
D       write (*,*) i,xin,yin
        xxx(i)=xin
        yyy(i)=yin
        kk=i
      enddo

C Fabricate first and last point to avoid wrap-araound later
 40   continue
      xxx(1)=xxx(kk)-360.       ! wrap last point to negative Ls
      yyy(1)=yyy(kk)
      kk=kk+1
      xxx(kk)=xxx(2)+360.       ! wrap first point to > one year
      yyy(kk)=yyy(2)

 9    READTXT360=kk
D      write (*,*)' READT kk=',kk
      CLOSE (IOD)
      RETURN

C error section
 81   WRITE(IOERR,*)'READTXT360 error opening input file =',Fname
      WRITE(IOERR,*),'IOSTAT=',iost
      GOTO 9

 82   WRITE(IOERR,*)'READTXT360 unexpected end-of file ',Fname,i
      GOTO 9
 
 83   WRITE(IOERR,*)'READTXT360 error reading a data line',Fname,i
      goto 9

      end
