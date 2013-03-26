	SUBROUTINE TDISK (KODE,KREC)
C_Titl  TDISK  save/read results at the end of a season
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'
C_Args
	INTEGER KODE		!in. control
C  1 = open file.  then see  KREC
C        will set  LOPN2=.true. if file opened
C  2 = write a season. appended after  JREC.  KREC ignored
C  3 = read a season.  input  KREC as record number,  IERR returned as iostat
C	also, sets record position to append on next write
C  4 = close the file
C  5 = write a record of  KRCCOM
	INTEGER KREC	!in (when  KODE=1): file status: 0=new  1=old
C_Hist	93mar03	ecisneros ported code to unix platform
C		          converted include filenames to lowercase.
C 97feb11  HHK get file name only from common
C 97aug03  HHK add short form output  97sep08 accomodate  TPF,ALAT,ELEV
C 98may26  HHK  NRECL in bytes for  Linux
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C does direct write fillout record?
C?	parameter (nplus=macn24*maxn4-nwkrc) ! needed to fill out record
	CHARACTER CSTAT*3
	Real*4 COMKRC(NWKRC),COMLAT(NWLAT),COMDAY(NWDAY)
	EQUIVALENCE (COMKRC,ALB),(COMLAT,ALAT),(COMDAY,X)
C
	GOTO (100,200,300,400,500),KODE
C request file name and open file			1  1  1  1  1  1  1  1
100	IF (LOPN2) CLOSE (IOD2)
	LOPN2=.FALSE.
	IF (KREC.EQ.0) THEN
	  CSTAT='NEW'
	  WRITE (IOPM,*) ' File for saving results?'
	ELSE
	  CSTAT='OLD'
	  WRITE (IOPM,*) ' File to start (and continue) from ?'
	ENDIF
C  RECL may be bytes or longwords, depending upon  OS and compiler options.
C  Solaris: it is longwords, since file is unformatted (default for direct access)
	IF (K4OUT.EQ.0) THEN
	  NWTOT=NWKRC+NWLAT	! krccom & latcom
	ELSEIF (K4OUT.GT.0) THEN
	  NWTOT=NWKRC+NWDAY	! krccom & daycom
	ELSE
	  NWTOT=2*MACN24*MAXN4	!  TSF  &  TPF only
	ENDIF

	nrecl=4*nwtot  !  OR nrecl=nwtot  ! depends upon compioler <<<<<<<<<

	OPEN (UNIT=IOD2,FILE=FDISK,ACCESS='DIRECT',STATUS=CSTAT
     &,RECL=NRECL,ERR=191,IOSTAT=IOS)
	WRITE(IOSP,110)CSTAT,FDISK,NWTOT,DAYTIM
110	FORMAT (/'0TDISK:  Opened ',A,' direct access file = ',A
     &/8X,'Record length in R*4 & NRECL = ',2I6,3X,'NOW = ',5A4)
	LOPN2=.TRUE.
	JREC=0
	GOTO 9
C
191	WRITE (IOERR,*) ' TDISK: ERROR OPENING FILE. IOSTAT=',IOS
	LOPN2=.FALSE.
	GOTO 9
C
C write next record (internal record count)		2  2  2  2  2  2  2  2
200	IF (LOPN2) THEN
	  JREC=JREC+1
	  I=JREC
	  IF (K4OUT.EQ.0) THEN
	    WRITE(IOD2,REC=I)COMKRC,COMLAT
	  ELSEIF (K4OUT.GT.0) THEN
	    WRITE(IOD2,REC=I)COMKRC,COMDAY
	  ELSE
	    WRITE(IOD2,REC=I)TSF,TPF
	  ENDIF
	  WRITE(IOSP,210)FDISK,JREC
 210	  FORMAT('0TDISK:  WROTE FILE = ',A,' RECORD =',I3)
	ELSE
	  WRITE (IOERR,*) ' TDISK:WRITE, BUT NO FILE OPEN'
	ENDIF
	GOTO 9
C
C read requested record (external record control)	3  3  3  3  3  3  3  3
300	IF (LOPN2) THEN
		I=KREC
		READ(IOD2,REC=I,IOSTAT=IOS)COMKRC,COMLAT
		WRITE(IOSP,310)FDISK,KREC
310		FORMAT('0TDISK:  READ FILE= ',A,' RECORD=',I3)
		IERR=IOS
		JREC=KREC
	    ELSE
		WRITE (IOERR,*)' TDISK:READ, BUT NO FILE OPEN'
	    ENDIF
	GOTO 9
C
C close the file					4  4  4  4  4  4  4  4
400	IF (LOPN2) CLOSE (IOD2)
	LOPN2=.FALSE.
	goto 9

 500	IF (K4OUT.LT.0 .and. jrec.eq.0) THEN	! output parameters once
	  JREC=1
	  I=JREC		! need  NWKRC+2*MAXN4=203 items
	  WRITE (IOD2,REC=I) COMKRC,ALAT,ELEV
	  else 
	    Write (IOERR,*) 'TDISK:5 under wrong conditions'
	  ENDIF

9	RETURN
	END
