	PROGRAM test
C_Titl  KRC planet/comet surface thermal model
        IMPLICIT REAL*4 (A-H,O-Z), INTEGER*4 (I,J,K,M,N), LOGICAL*4 (L)  ! std.,+L
C_Vars
        CHARACTER*40    FINPUT,FOUT,FDISK
	IOIN = 3
	IOKEY = 5
	IOPM = 6
	IOSP = 6
	IOERR = IOPM
	LONE =.FALSE.		! true when in one-point mode

        fdisk='startingthing'
        finput='one.inp'
	  CLOSE(IOIN)		! close the card input file
	  write(*,*)'FINPUT=',finput
	  OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',iostat=iost,err=81)
	  write(*,*)' IOSTAT=',iost
	  READ (IOIN,'(A40)',ERR=83,END=84) FDISK ! read Users title
	  write(*,*)' k2'
	  WRITE(IOSP,*)'---- Start of one-point mode ----'
	  WRITE(IOSP,*)FDISK	! write users title
	  READ (I0IN,'(A80)',ERR=83,END=84) FDISK ! skip the col header line
	  write(*,*)' k3'
 9        STOP
 81	WRITE(IOERR,*)'KRC error opening input file =',FINPUT
	write(ioerr,*) 'IOSTAT=',iost
	GOTO 9
 83	WRITE(IOERR,*)'KRC error reading one-point header lines',FDISK 
	GOTO 9
 84	WRITE(IOERR,*)'KRC unexpected EOF reading one-point header lines' 
	GOTO 9

	END
