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
        PARAMETER (MD14=40)      ! seasons
        PARAMETER (MD15=5)       ! cases = inertias
        REAL*4 BMD1 (MD11*MD12*MD13*MD14*MD15) ! 60000 words

        INTEGER MD21,MD22,MD23,MD24,MD25 ! maximum dimensions
        PARAMETER (MD21=MAXNH)	! hours
        PARAMETER (MD22=2)	! surface & planetary
        PARAMETER (MD23=10)	! latitudes
        PARAMETER (MD24=80)	! seasons
        PARAMETER (MD25=10)	! cases = inertias
        REAL*4 BMD2 (MD21,MD22,MD23,MD24,MD25) ! 384000 words

        INTEGER MD31,MD34,MD35! maximum dimensions
        PARAMETER (MD31=1+2*MAXNH+2*MAXN1+MAXNH) ! combined
        PARAMETER (MD34=82)	! seasons (first 2 hold KRCCOM)
        PARAMETER (MD35=10)	! cases
        REAL*4 BMD3 (MD31*MD34*MD35) ! 89380 words

	EQUIVALENCE(BMD1,BMD2,BMD3)
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
C                   51=(30 layers, 2 min/max,  5 lat, 40 seasons,  5 cases)
C     The last 2 layers for min are: TpMin, L_sub_s ; for max are: TpMax, DJU5
C 98nov16  HHK add  52=(24 hours,   Tsurf/Tp, 10 lat, 80 seasons, 10 cases)
C 99nov24  HHK move setting  LOPN2 false at 400
C 2002aug04 HHK Add 53=(combo  at 1 lat, 2+80 seasons, 10 cases). Recode logic
C 2002aug15 HK Increase season size
C_Bugs
C all but last dimension of  BMDn idealy would be dynamically allocated
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C Direct write fillout will zero-fill unused part of record
	CHARACTER CSTAT*3
	REAL*4 COMKRC(NWKRC),COMLAT(NWLAT),COMDAY(NWDAY)
	EQUIVALENCE (COMKRC,ALB),(COMLAT,NDJ4),(COMDAY,X)

        INTEGER*4 JB5(10) ! sizes to go to  BINF5
        CHARACTER*30 HEADER /'KRC-tes custom save'/
        INTEGER HEADLEN /30/
        INTEGER JREC,NWTOT,NRECL,I,IOS,J,k,ND1,ND3,IRET
	INTEGER MM2,MM3,MM4	! multiple array dimensions
	REAL*4 ASOLH(MAXNH),AH
C
C	write(iosp,*)'TDISK called with kode,case,j5=',kode,case,j5
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
	IF (K4OUT.LE.50) THEN	! Open output file
	  IF (K4OUT.LT.0) THEN	! k4out is negative
	    NWTOT=2*MACNH*MAXN4 !  TSF  &  TPF only
	  ELSEIF (K4OUT.EQ.0) THEN
	    NWTOT=NWKRC+NWLAT	! krccom & latcom
	  ELSE			! k4out is 1:50
	    NWTOT=NWKRC+NWDAY	! krccom & daycom
	  ENDIF
	  NRECL=4*NWTOT		!  OR nrecl=nwtot  ! depends upon compiler <<<<
	  OPEN (UNIT=IOD2,FILE=FDISK,ACCESS='DIRECT',STATUS=CSTAT
	1      ,RECL=NRECL,ERR=191,IOSTAT=IOS)
	  WRITE(IOSP,110)CSTAT,FDISK,NWTOT, NRECL,DAYTIM
	  WRITE(IOERR,110)CSTAT,FDISK,NWTOT, NRECL,DAYTIM
 110	  FORMAT (/'0TDISK:  Opened ',A,' direct access file = ',A
	1      /8X,'Record length in R*4 & NRECL = ',2I6,3X,'NOW = ',5A4)
	  JREC=0
	ELSE			! setup for BIN5 file, which does not use  JREC
	  JB5(5) = 0		!
	  JB5(6) = 0		!
	  JB5(7) = 0		!
	  JB5(8) = 4		! set type as  REAL*4
	  JB5(9) = HEADLEN	! header length
	  IF (K4OUT.EQ.51) THEN !
	    JB5(1) = 5		! # of dimensions
	    JB5(2) = MD11	!
	    JB5(3) = MD12	!
	    JB5(4) = MD13	!
	    JB5(5) = MD14	!
	    JB5(6) = MD15	!
	    MM2=MD11*MD12
	    MM3=MM2*MD13
	    MM4=MM3*MD14
	  ELSEIF (K4OUT.EQ.52) THEN 
	    JB5(1) = 5		! # of dimensions
	    JB5(2) = MD21	!
	    JB5(3) = MD22	!
	    JB5(4) = MD23	!
	    JB5(5) = MD24	!
	    JB5(6) = MD25	!
	  ELSE			! K4OUT=53
	    if (N4.ne.1) WRITE(iosp,*)'ERROR: Expect J4=1 for K4OUT=53'
	    JB5(1) = 3		! # of dimensions
	    JB5(2) = MD31	!
	    JB5(3) = MD34	!
	    JB5(4) = MD35	!
	    MM4=MD31*MD34
	  ENDIF
	   WRITE(IOSP,13)K4OUT,JB5
 13	   FORMAT ('Initiated custom output: K4OUT=',I3,/'JB5=',10I6)
	ENDIF
	LOPN2=.TRUE.
	GOTO 9
C
191	WRITE (IOERR,*) ' TDISK:1, ERROR OPENING FILE. IOSTAT=',IOS
	WRITE (IOERR,*) '   IOD2=',IOD2,'  status=',cstat,'  recl=',nrecl
	WRITE (IOERR,*) '   file=  ',FDISK
	LOPN2=.FALSE.
	GOTO 9
C
C write next record (internal record count)		2  2  2  2  2  2  2  2
 200	IF (K4OUT.LE.50) THEN !...............................................
	  IF (LOPN2) THEN	! save current values
	    JREC=JREC+1
	    I=JREC		! may be incremented by the  WRITE command
	    IF (K4OUT.LT.0) THEN ! K4OUT negative
	      WRITE(IOD2,REC=I)TSF,TPF
	    ELSEIF (K4OUT.EQ.0) THEN
	      WRITE(IOD2,REC=I)COMKRC,COMLAT
	    ELSE		! K4OUT=1:50
	      WRITE(IOD2,REC=I)COMKRC,COMDAY
	    ENDIF
	    I=LNBLNK(FDISK)	! last non-blank character in file name
	    WRITE(IOSP,210)J5,JREC,SUBS,FDISK(1:I)
 210	    FORMAT(' TDISK wrote: J5 rec Ls File ',2I4,F7.2,1X,A)
	  ELSE
	    WRITE (IOERR,*) ' TDISK:2, WRITE, BUT NO FILE OPEN'
	  ENDIF
	ELSEIF (K4OUT.EQ.51) THEN ! .........................................
	  ND1=MIN(N1,MD11-2)	! save room for 2 items
	  ND3=MIN(N4,MD13)
	    JREC=J5-JDISK
	  IF (JREC.LE.MD14 .AND. NCASE.LE.MD15) THEN
	    DO J4=1,ND3		! do each latitude
	      CALL XTREME (TPF(1,j4),1,N24,TPMIN,TPMAX) ! get planetary limits
	      I=1+(J4-1)*MM2+(JREC-1)*MM3+(NCASE-1)*MM4 ! first of this set
	      CALL R2R (TIN(1,j4),BMD1(I),ND1) ! transfer layer minima
	      BMD1(I+MD11-2) =TPMIN
	      BMD1(I+MD11-1) =SUBS
	      I=I+MD11		! offset to maxima
	      CALL R2R (TAX(1,j4),BMD1(I),ND1) ! transfer layer minima
	      BMD1(I+MD11-2) =TPMAX
	      BMD1(I+MD11-1) =DJU5
	    ENDDO
	  ENDIF
	ELSEIF (K4OUT.EQ.53) THEN !......................................
	  IF  (NCASE.LE.MD35) THEN
	    JREC=J5-JDISK
	    IF (JREC.EQ.1) THEN
C KRCCOM is 203 4-byte words, and MD31 is 133, so requires 2 seasons
C  and has 63  extra words
	      I=1+(NCASE-1)*MM4 ! first two "seasons" are  KRCCOM
C	      write(iosp,*)'case,j5,I=',case,j5,I
	      CALL R2R (ALB,BMD3(I),NWKRC) ! transfer KRCCOM
	    ENDIF
	    IF (JREC.LE.MD34-2) THEN
	      I=1+(JREC+1)*MD31+(NCASE-1)*MM4 ! append seasons after KRCCOM
C	      write(iosp,*)'case,j5,I=',case,j5,I
	      BMD3(I) =FROST4(1) ! predicted final frost
	      I=I+1
	      CALL R2R (TSFH,BMD3(I),MAXNH)
	      I=I+MAXNH
	      CALL R2R (TPFH,BMD3(I),MAXNH)
	      I=I+MAXNH
	      CALL R2R (TMIN,BMD3(I),MAXN1)
	      I=I+MAXN1
	      CALL R2R (TMAX,BMD3(I),MAXN1) ! total 109 words
	      I=I+MAXN1
	      AH = FLOAT(N2)/FLOAT(N24) ! time steps between saving results
	      DO J=1,N24
		K=FLOAT(J)*AH +.5 ! time step nearest each "hour"
		ASOLH(J)=ASOL(K) ! transfer from full time array
	      ENDDO
	      CALL R2R (ASOLH,BMD3(I),MAXNH) ! total 133 words
C including the new two arrays puts MD31 at 877? words, file is 1403712? bytes
C	      I=I+MAXN1
C	      CALL R2R (ASOL,BMD3(I),MAXN2)
C	      I=I+MAXN2
C	      CALL R2R (TOUT,BMD3(I),MAXN2)
C	      write(iosp,*)'case,j5,I=',case,j5,I
	    ENDIF
	  ENDIF
	ELSE			! K4OUT=52  ...................................
	  IF (J5.LE.MD24 .AND. NCASE.LE.MD25) THEN
	    ND1=MIN(N24,MD21)
	    ND3=MIN(N4,MD23)
	    DO J4=1,ND3		! do each latitude
	      CALL R2R (TSF(1,J4),BMD2(1,1,J4,J5,NCASE),ND1)
	      CALL R2R (TPF(1,J4),BMD2(1,2,J4,J5,NCASE),ND1)
	    ENDDO
	  ENDIF
	ENDIF			!...............................................
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
	  IF (K4OUT.GT.50) THEN ! save custom array
	    WRITE (IOSP,*)'jb5=',JB5
	    CALL BINF5 ('W',FDISK,HEADER,JB5,BMD1,IRET) ! others equivalenced
	    WRITE (IOSP,*)'Wrote custom array, iret=',IRET
	  ELSE			! close direct access file, or message
	    CLOSE (IOD2)
	    LOPN2=.FALSE.
	  ENDIF
	ELSE ! not open
	  IF (K4OUT.LE.50) WRITE (IOERR,*)' TDISK:4, no file was open'
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
