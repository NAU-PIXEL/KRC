	SUBROUTINE TDISK (KODE,KREC)
C      implicit none
C_Titl  TDISK  save/read results at the end of a season  VERSION  FOR  BINF5
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'
C_Lims
        INTEGER MD11,MD12,MD13,MD14,MD15 ! maximum dimensions
        PARAMETER (MD11=30)      ! layers
        PARAMETER (MD12=2)       ! min/max
        PARAMETER (MD13=5)       ! latitudes
        PARAMETER (MD14=20)      ! seasons
        PARAMETER (MD15=5)       ! case = inertias
        REAL*4 BMD1 (MD11*MD12*MD13*MD14*MD15)

        INTEGER MD21,MD22,MD23,MD24,MD25 ! maximum dimensions
        PARAMETER (MD21=MAXN24)  ! hours
        PARAMETER (MD22=2)       ! surface & planetary
        PARAMETER (MD23=10)       ! latitudes
        PARAMETER (MD24=40)      ! seasons
        PARAMETER (MD25=10)       ! case = inertias
        REAL*4 BMD2 (MD21,MD22,MD23,MD24,MD25)
	EQUIVALENCE(BMD1,BMD2)
C_Args
	INTEGER KODE		! in. control
C  1 = open file.  then see  KREC
C        will set  LOPN2=.true. if file opened
C  2 = write a season. appended after  JREC.  KREC ignored
C  3 = read a season.  input  KREC as record number,  IERR returned as iostat
C	also, sets record position to append on next write
C  4 = close the file
C  5 = write a record of  KRCCOM
	INTEGER KREC	!in (when  KODE=1): file status: 0=new  1=old
C_Hist ~1975 Hugh Kieffer Original version; evolved over 20 years
C 93mar03 ecisneros ported code to unix platform
C		          converted include filenames to lowercase.
C 97feb11  HHK get file name only from common
C 97aug03  HHK add short form output  97sep08 accomodate  TPF,ALAT,ELEV
C 98may26  HHK  NRECL in bytes for  Linux
C 98aug31  HHK include custom  bin5file capablity 
C                   51=(30 layers, 2 min/max,  5 lat, 20 seasons,  5 cases)
C     The last 2 layers for min are: TpMin, L_sub_s ; for max are: TpMax, DJU5
C 98nov16  HHK add  52=(24 hours,   Tsurf/Tp, 10 lat, 40 seasons, 10 cases)
C 99nov24  HHK move setting  LOPN2 false at 400
C_Bugs
C all but last dimension of  BMDn idealy would be dynamically allocated
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C does direct write fillout record?
C?	parameter (nplus=macn24*maxn4-nwkrc) ! needed to fill out record
	CHARACTER CSTAT*3
	REAL*4 COMKRC(NWKRC),COMLAT(NWLAT),COMDAY(NWDAY)
	EQUIVALENCE (COMKRC,ALB),(COMLAT,NDJ4),(COMDAY,X)

        INTEGER*4 JB5(10) ! sizes to go to  BINF5
        CHARACTER*30 HEADER /'KRC-tes custom save'/
        INTEGER HEADLEN /30/
        INTEGER JREC,NWTOT,NRECL,I,IOS,J,ND1,ND3,IRET
	INTEGER MM2,MM3,MM4	! multiple array dimensions
C
	GOTO (100,200,300,400,500),KODE
C
C request file name and open file			1  1  1  1  1  1  1  1
100	IF (LOPN2) CLOSE (IOD2)
	LOPN2=.FALSE.
	IF (KREC.EQ.0) THEN
	  CSTAT='NEW'
CC	  WRITE (IOPM,*) ' File for saving results?'
	ELSE
	  CSTAT='OLD'
	  WRITE (IOPM,*) ' File to start (and continue) from ?'
	ENDIF
C     recl may be bytes or longwords, depending upon  OS and compiler options.
C Solaris: it is longwords, since file is unformatted (default for direct access)
	IF (K4OUT.EQ.0) THEN
	  NWTOT=NWKRC+NWLAT	! krccom & latcom
	ELSEIF (K4OUT.GE.50) THEN ! don't open file yet
	   JB5(5) = 0		!
	   JB5(6) = 0		!
           JB5(7) = 0		!
           JB5(8) = 4		! set type as  REAL*4
           JB5(9) = HEADLEN	! header length
	   IF (K4OUT.EQ.51) THEN !
	      JB5(1) = 5	! # of dimensions
	      JB5(2) = MD11	!
	      JB5(3) = MD12	!
	      JB5(4) = MD13	!
	      JB5(5) = MD14	!
	      JB5(6) = MD15	!
	      MM2=MD11*MD12
	      MM3=MM2*MD13
	      MM4=MM3*MD14
	   ELSE 
	      JB5(1) = 5	! # of dimensions
	      JB5(2) = MD21	!
	      JB5(3) = MD22	!
	      JB5(4) = MD23	!
	      JB5(5) = MD24	!
	      JB5(6) = MD25	!
	   ENDIF
	   LOPN2=.TRUE.
	   JREC=0
	   WRITE(IOSP,13)K4OUT,JB5
 13	   FORMAT ('Initiated custom output: K4OUT=',I3,/'JB5=',10I6)
           GOTO 9
	ELSEIF (K4OUT.GT.0) THEN ! k4out is 1:49
	  NWTOT=NWKRC+NWDAY	! krccom & daycom
	ELSE			! k4out is negative
	  NWTOT=2*MACN24*MAXN4	!  TSF  &  TPF only
	ENDIF

	NRECL=4*NWTOT  !  OR nrecl=nwtot  ! depends upon compiler <<<<<<<<<

	OPEN (UNIT=IOD2,FILE=FDISK,ACCESS='DIRECT',STATUS=CSTAT
     &,RECL=NRECL,ERR=191,IOSTAT=IOS)
	WRITE(IOSP,110)CSTAT,FDISK,NWTOT, NRECL,DAYTIM
	WRITE(IOERR,110)CSTAT,FDISK,NWTOT, NRECL,DAYTIM
110	FORMAT (/'0TDISK:  Opened ',A,' direct access file = ',A
     &/8X,'Record length in R*4 & NRECL = ',2I6,3X,'NOW = ',5A4)
	LOPN2=.TRUE.
	JREC=0
	GOTO 9
C
191	WRITE (IOERR,*) ' TDISK:1, ERROR OPENING FILE. IOSTAT=',IOS
	WRITE (IOERR,*) '   IOD2=',IOD2,'  status=',cstat,'  recl=',nrecl
	WRITE (IOERR,*) '   file=  ',FDISK
	LOPN2=.FALSE.
	GOTO 9
C
C write next record (internal record count)		2  2  2  2  2  2  2  2
 200    IF (LOPN2) THEN
	  JREC=JREC+1
	  I=JREC
	  IF (K4OUT.EQ.0) THEN
	     WRITE(IOD2,REC=I)COMKRC,COMLAT
          ELSEIF (K4OUT.GE.50) THEN ! save current values
	    IF (K4OUT.EQ.51) THEN !
	      ND1=MIN(N1,MD11-2) ! save room for 2 items
	      ND3=MIN(N4,MD13)
	      IF (J5.LE.MD14 .AND. NCASE.LE.MD15) THEN

		DO J4=1,ND3	! do each latitude
        CALL XTREME (TPF(1,j4),1,N24,TPMIN,TPMAX)  ! get planetary limits
	          I=1+(J4-1)*MM2+(J5-1)*MM3+(NCASE-1)*MM4 ! first of this set
		  CALL R2R (TIN(1,j4),BMD1(I),ND1) ! transfer layer minima
		  BMD1(I+MD11-2) =TPMIN
		  BMD1(I+MD11-1) =SUBS
		  I=I+MD11	! offset to maxima
		  CALL R2R (TAX(1,j4),BMD1(I),ND1) ! transfer layer minima
		  BMD1(I+MD11-2) =TPMAX
		  BMD1(I+MD11-1) =DJU5
	        ENDDO
	     ENDIF
	   ELSE			! K4OUT=50
	     IF (J5.LE.MD24 .AND. NCASE.LE.MD25) THEN
		ND1=MIN(N24,MD21)
		ND3=MIN(N4,MD23)
		DO J4=1,ND3	! do each latitude
		   CALL R2R (TSF(1,J4),BMD2(1,1,J4,J5,NCASE),ND1)
		   CALL R2R (TPF(1,J4),BMD2(1,2,J4,J5,NCASE),ND1)
		ENDDO
	     ENDIF
	   ENDIF
	 ELSEIF (K4OUT.GT.0) THEN ! K4OUT=1:49
	     WRITE(IOD2,REC=I)COMKRC,COMDAY
	   ELSE			! K4OUT negative
	     WRITE(IOD2,REC=I)TSF,TPF
	   ENDIF
	  I=LNBLNK(FDISK) ! last non-blank character in file name
	  IF (K4OUT.LT.50) WRITE(IOSP,210)J5,JREC,SUBS,FDISK(1:I)
 210	  FORMAT(' TDISK wrote: J5 rec Ls File ',2I4,F7.2,1X,A)
       ELSE
             WRITE (IOERR,*) ' TDISK:2, WRITE, BUT NO FILE OPEN'
       ENDIF
       GOTO 9
C
C read requested record (external record control)	3  3  3  3  3  3  3  3
300	IF (LOPN2) THEN
		I=KREC
		READ(IOD2,REC=I,IOSTAT=IOS)COMKRC,COMLAT
		WRITE(IOSP,310)FDISK,KREC
310		FORMAT(' TDISK:  READ FILE= ',A,' RECORD=',I3)
		IERR=IOS
		JREC=KREC
	    ELSE
		WRITE (IOERR,*)' TDISK:3, READ, BUT NO FILE OPEN'
	    ENDIF
	GOTO 9
C
C close the file					4  4  4  4  4  4  4  4
400	IF (LOPN2) then
	   IF (K4OUT.GE.50) THEN ! save custom array
	      WRITE(IOSP,*)'jb5=',JB5
	      CALL BINF5 ('W',FDISK,HEADER,JB5,BMD1,IRET) ! others equivalenced
	      WRITE (IOSP,*)'Wrote custom array, iret=',IRET
	   ELSE
	      CLOSE (IOD2)
	   ENDIF
	   LOPN2=.FALSE.
	ELSE
	   WRITE (IOERR,*)' TDISK:4, no file was open'
	ENDIF
	GOTO 9
C       
C       output parameters once                          5  5  5  5  5  5  5  5
 500	IF (K4OUT.LT.0 .AND. JREC.EQ.0) THEN
	   JREC=1
	   I=JREC		! need  NWKRC+2*MAXN4=203 items
	   WRITE (IOD2,REC=I) COMKRC,ALAT,ELEV
	ELSE 
      WRITE (IOERR,*) 'TDISK:5,wrong conditions: k4out,jrec=',k4out,jrec
	ENDIF
	
 9	RETURN
	END
