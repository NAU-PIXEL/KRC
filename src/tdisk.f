	SUBROUTINE TDISK (KODE,KREC)
C	implicit none
C_Titl  TDISK  save/read results at the end of a season  VERSION  FOR  BINF5
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'hatcom.inc'
	INCLUDE 'units.inc'
	INCLUDE 'filcom.inc'
C_Lims
        PARAMETER (KOMMON=583200) ! words for Type 52, which is
        REAL*4 FFF (KOMMON)	!   the largest of the 5n types
        INTEGER*4 JBB(10)	! sizes to go to  BINF5
	INTEGER*4 KASE,KODED	! words/case, file type
	COMMON /BINCOM/ JBB,KODED,KASE,FFF
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
C_Desc
C Can write several styles of binary files; controled by K4OUT. See helplist.txt
C The two basic families are Direct access files containing combinations of
C the COMMON arrays, and bin5 files of specific variables and arrays.
C The bin5 files have K4OUT=50+; the parameter statements above set the 
C maximum allowed sizes, but they are written dense to the requested array
C sizes, and BINF5 writes only the utilized part of the storage allocated.
C
C_Hist ~1975 Hugh Kieffer Original version; evolved over 20 years
C 93mar03 ecisneros ported code to unix platform
C		          converted include filenames to lowercase.
C 97feb11  HHK get file name only from common
C 97aug03  HHK add short form output  97sep08 accomodate  TPF,ALAT,ELEV
C 98may26  HHK  NRECL in bytes for  Linux
C 98aug31  HHK include custom  bin5 file capablity 
C                   51=(30 layers, 2 min/max,  5 lat, 40 seasons,  5 cases)
C     The last 2 layers for min are: TpMin, L_sub_s ; for max are: TpMax, DJU5
C 98nov16  HHK add  52=(24 hours,   Tsurf/Tp, 10 lat, 80 seasons, 10 cases)
C 99nov24  HHK move setting  LOPN2 false at 400
C 2002aug04 HHK Add 53=(combo  at 1 lat, 2+80 seasons, 10 cases). Recode logic
C 2002aug15 HK Increase season size
C 2003may25 HK Increase MD23 from 10 to 19. Use NDx consistently for K4OUT=52
C 2003jun10 HK Fix indexing for seasons for type 53
C 2003aug27 HK Fix indexing error
C 2004jul06 HK Add file style 54. [GO to  IMPLICIT NONE. NOPE]
C 2004Oct05 HK Revise style 52; add Down fluxes and spare
C 2005aug04 HK Add first version of type 55
C 2005nov16 HK Set ncase=0 after a file close so next would start at 1
C 2006apr12 HK Change file style 54 to have both 1am and 1pm surface Temp..
C 2006apr22 HK Allow flexible number of cases for  output file type 52 and 54
C 2006may04 HK Remove the parameter statements for each type. Add BINCOM
C_Bugs
C all but last dimension of fff ideally would be dynamically allocated
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C	REAL*4 ALB
C Direct write will zero-fill unused part of record
	CHARACTER CSTAT*3
	REAL*4 COMKRC(NWKRC),COMLAT(NWLAT),COMDAY(NWDAY)
	EQUIVALENCE (COMKRC,ALB),(COMLAT,NDJ4),(COMDAY,X)

        CHARACTER*30 HEADER /'KRC-tes custom save'/
        INTEGER HEADLEN /30/
        INTEGER JREC,KOFF,NWTOT,NRECL,I,I2,IOS,J,K,ND1,ND3,ND4,IRET
C JREC is the 1-based output record number
	INTEGER MM2,MM3,MM4,MM5,MTOT ! multiple array dimensions. Reused
	INTEGER NSOUT		! number of seasons expected to be output

	REAL*4 ASOLH(MAXNH),AH,TPMIN,TPMAX
	SAVE KOFF,MASE,ND1,ND3,ND4,JREC,MM2,MM3,MM4,MTOT ! insure these remain defined
C
C	write(iosp,*)'TDISK called with kode,case,j5=',kode,case,j5
	GOTO (100,200,300,400,500),KODE
C
C request file name and open file			1  1  1  1  1  1  1  1
C Note. Case and KRCCOM are completely defined at this point
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
	IF (K4OUT.LE.50) THEN	! Open direct-access output file
	  IF (K4OUT.LT.0) THEN	!  K4OUT is negative
	    NWTOT=2*MAXNH*MAXN4 !  TSF  &  TPF only
	  ELSEIF (K4OUT.EQ.0) THEN
	    NWTOT=NWKRC+NWLAT	!  KRCCOM & LATCOM
	  ELSE			!  K4OUT is 1:50
	    NWTOT=NWKRC+NWDAY	!  KRCCOM & DAYCOM
	  ENDIF
	  NRECL=4*NWTOT		!  or  NRECL=NWTOT  ! depends upon compiler <<<<
	  OPEN (UNIT=IOD2,FILE=FDISK,ACCESS='DIRECT',STATUS=CSTAT
	1      ,RECL=NRECL,ERR=191,IOSTAT=IOS)
	  WRITE(IOSP,110)CSTAT,FDISK,NWTOT, NRECL,DAYTIM
	  WRITE(IOERR,110)CSTAT,FDISK,NWTOT, NRECL,DAYTIM
 110	  FORMAT (/'0TDISK:  Opened ',A,' direct access file = ',A
	1      /8X,'Record length in R*4 & NRECL = ',2I6,3X,'NOW = ',5A4)
	ELSE			! setup for BIN5 file
	  JBB(5) = 0		!
	  JBB(6) = 0		!
	  JBB(7) = 0		!
	  JBB(8) = 4		! set type as  REAL*4
	  JBB(9) = HEADLEN	! header length
	  IF (K4OUT.EQ.51) THEN !
	    JBB(1) = 5		! # of dimensions
	    JBB(2) = 30		! layers. Some may be empty
	    JBB(3) = 2		! min/max
	    JBB(4) = 5		! latitudes  Some may be empty
	    JBB(5) = 40		! seasons  Some may be empty
	    JBB(6) = 5		! cases. Some may be empty
	    MM1=JBB(2)
	    MM2=MM1*JBB(3)	! # words / latitude
	    MM3=MM2*JBB(4)	! # words / season
	    KASE=MM3*JBB(5)	! # words in a case
	    MASE=JBB(6)		! max number of cases allowed
	  ELSEIF (K4OUT.EQ.52) THEN 
C For hourly conditions:  revised 2004jul22  and 2004Oct06
C a Lat is 24*6 words = 144  First "season" contains: 
C  Lats 1:2 contain KRCCOM (203 words in space for 288)
C  Lats 3 contain DJU5 (up to # seasons in space of 144)
C  Lats 4 contain SUBS (L_s) for each season. " "
C  Lats 5 contain PZREF. " "
	    MD24=81		! 1+ # seasons . Minimum of 2. None empty
	    ND4=MIN(N4,MD24)	! remember how many latitudes to store
	    JBB(1) = 5		! # of dimensions
	    JBB(2) = MAXNH	! hours; 24 Fixed
	    JBB(3) = 6		! surface & planetary & Atmosphere Temperature
	    JBB(4) = MAX(ND4,5)	! # latitudes. At least first season packing
	    JBB(5) = N5-JDISK+2 ! 1+ number of seasons to come.
c	    JBB(6) = 5		! cases . Minimum of 1. None empty. Temporary
	    ND1=JBB(2)		! offset between items
	    MM3=JBB(2)*JBB(3)	! # words in a latitude
	    MM4=MM3*JBB(4)	! # words in a season
	    KASE=MM4*JBB(5)	! # words in a case
	    MASE=0		! TBD
	    CALL R2R (0.,FFF,-KASE) ! insure first case initialized to zero
	  ELSEIF (K4OUT.EQ.53) THEN 
	    IF (N4.NE.1) WRITE(IOSP,*)'ERROR: Expect J4=1 for K4OUT=53'
	    JBB(1) = 3		! # of dimensions
	    JBB(2) = 1+2*MAXNH+2*MAXN1+MAXNH ! combined
	    JBB(3) = 82		! seasons (first 2 hold KRCCOM) Some may be empty
	    MASE = 10		! cases. Some may be empty
 	    JBB(4) = MASE	!
	    KASE=JBB(2)*JBB(3)
	  ELSEIF (K4OUT.EQ.54) THEN
C for Heat flow. Save surface temperture and heat flow at midnight
C as function of season and latitude.  Store KRCCOM in the first latitude
C Output file will be only as large as used.
	    JBB(1) = 4		! # of dimensions
	    JBB(2) = MAXN5	! max # seasons [740]  Else might fail for 'continue' mode
	    JBB(3) = 5		! Tsur 1am, Tsur 1pm, heat flow, frost, Tbot
	    JBB(4) = N4+1	! # latitudes +1
	    ND1=JBB(2)
	    MM2=JBB(2)*JBB(3)	! # words in a latitude
	    KASE=MM2*JBB(4)	! # words in a case
	    MASE=0		! TBD
	    CALL R2R(0.,FFF,-KASE) ! insure first case initialized to zero
	  ELSEIF (K4OUT.EQ.55) THEN
C for seasonal studies at one latitude, [seasons,koff+items,cases]
C KRCCOM stored as prepended items.  Output file will be only as large as used.
C THIS MODE DOES NOT SUPPORT CONTINUATION RUNS
	    JBB(1) = 3		! # of dimensions
	    JBB(2) = N5-JDISK+1	! # seasons to store
	    KOFF=(NWKRC-1)/JBB(2)+1 ! # items req to hold KRCCOM
C In next line, the constant is the number of items stored in section 200 and is 
	    JBB(3) = KOFF+10	! VARIABLE betweens codings:  DJU5, Tsur ...
	    MM1=JBB(2)		! size of first dimension
	    KASE=MM1*JBB(3)	! # words in a case
	    MASE=0		! TBD
	    CALL R2R(0.,FFF,-KASE) ! insure first case initialized to zero
	  ENDIF
	   WRITE(IOSP,13)K4OUT,JBB
 13	   FORMAT ('Initiated custom output: K4OUT=',I3,/'JBB=',10I6)
	ENDIF
	KODED=K4OUT ! transfer into Common
	IF (MASE.EQ.0) MASE=KOMMON/KASE	! # cases that could be accomodated
	MTOT=KASE*MASE
	WRITE(IOSP,*),'KOMMON,KASE,MASE,MTOT=',KOMMON,KASE,MASE,MTOT
	JREC=0			! records written thus far = none
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
	  GOTO 9
	ENDIF
	IF (NCASE.GT.MASE) GOTO 9 ! no room left for storage
	IF (K4OUT.EQ.51) THEN ! .........................................
	  ND1=MIN(N1,MM1-2)	! save room for 2 items
	  ND3=MIN(N4,JBB(4))	! # lats to save
	  JREC=J5-JDISK+1	! 1-based count of this record
	  IF (JREC.LE.JBB(5)) THEN ! proceed only if room for this season
	    DO J4=1,ND3		! do each latitude
	      CALL XTREME (TPF(1,J4),1,N24,TPMIN,TPMAX) ! get planetary limits
	      I=1+(J4-1)*MM2+(JREC-1)*MM3+(NCASE-1)*KASE ! first of this set
	      CALL R2R (TIN(1,J4),FFF(I),ND1) ! transfer layer minima
	      FFF(I+MM1-2) =TPMIN
	      FFF(I+MM1-1) =SUBS
	      I=I+MM1		! offset to maxima
	      CALL R2R (TAX(1,J4),FFF(I),ND1) ! transfer layer minima
	      FFF(I+MM1-2) =TPMAX
	      FFF(I+MM1-1) =DJU5
	    ENDDO
	  ENDIF
	ELSEIF (K4OUT.EQ.52) THEN !...................
	  JOUT=J5-JDISK		! 0-based season count after start to disk
	  I=(NCASE-1)*KASE+1	! first word for this case
	  IF (JOUT.EQ.0) THEN	! jrec used to insure done only once
	    CALL R2R (ALB,FFF(I),NWKRC) ! into first lat.
	    IF (N24.NE.ND1 .OR. N4.GT.ND4 
	1	 .OR. N5-JDISK+2.NE.JBB(5)) THEN
	      WRITE (IOERR,*)
	1	   'Change of NHOUR, NLAT, or #to Disk not allowed for K4OUT=52'
	      WRITE (IOERR,*)N24,ND1, N4,ND4 ,N5-JDISK+1,JBB(5)
	      GOTO 9
	    ENDIF
	  ENDIF
	  IF (JOUT.LE.JBB(5)-2 ) THEN
	    J=I+JOUT		! first word for this case/season in header
	    FFF(J+2*MM3) =DJU5	! date for this case and season
	    FFF(J+3*MM3) =SUBS
	    FFF(J+4*MM3) =PZREF
	    J=I+MM4*(JOUT+1)	! season offset, +1 for header stuff
	    DO J4=1,ND4		! do each latitude (for which there is room)
	      I=J+(J4-1)*MM3
	      CALL R2R (TSF(1,J4),FFF(I)          ,ND1) ! copy one day
	      CALL R2R (TPF(1,J4),FFF(I+ND1)      ,ND1)
	      CALL R2R (TAF(1,J4),FFF(I+ND1+ND1)  ,ND1)
	      CALL R2R (DOWNVIS(1,J4),FFF(I+3*ND1),ND1)
	      CALL R2R (DOWNIR (1,J4),FFF(I+4*ND1),ND1)
	    ENDDO
	  ENDIF
	ELSEIF (K4OUT.EQ.53) THEN ! ...................................
	    JREC=J5-JDISK+1
	    IF (JREC.EQ.1) THEN ! first output season of a case. Save KRCCOM
C KRCCOM is 203 4-byte words, and MD31 is 133, so requires 2 seasons
C  and has 63  extra words
	      I=1+(NCASE-1)*KASE ! first two "seasons" are  KRCCOM
C	      write(iosp,*)'case,j5,I=',case,j5,I
	      CALL R2R (ALB,FFF(I),NWKRC) ! transfer KRCCOM
	    ENDIF
	    IF (JREC.LE.JBB(3)-2) THEN
	      I=1+(JREC+1)*JBB(2)+(NCASE-1)*KASE ! append seasons after KRCCOM
C	      write(iosp,*)'case,j5,I=',case,j5,I
	      FFF(I) =FROST4(1) ! predicted final frost
	      I=I+1
	      CALL R2R (TSFH,FFF(I),MAXNH)
	      I=I+MAXNH
	      CALL R2R (TPFH,FFF(I),MAXNH)
	      I=I+MAXNH
	      CALL R2R (TMIN,FFF(I),MAXN1)
	      I=I+MAXN1
	      CALL R2R (TMAX,FFF(I),MAXN1) ! total 109 words
	      I=I+MAXN1
	      AH = FLOAT(N2)/FLOAT(N24) ! time steps between saving results
	      DO J=1,N24
		K=FLOAT(J)*AH +.5 ! time step nearest each "hour"
		ASOLH(J)=ASOL(K) ! transfer from full time array
	      ENDDO
	      CALL R2R (ASOLH,FFF(I),MAXNH) ! total 133 words
	    ENDIF
	ELSEIF (K4OUT.EQ.54) THEN	! ......
C need to save surface temperature and heatflow for all lats at each season
	    I=1+(NCASE-1)*KASE	! first word for this case
C       At first output season of each case, save KRCCOM as the first "latitude
C   KRCCOM is 203 4-byte words long
C  [ season, items,latitude,case]
	    JOUT=J5-JDISK	! output season offset
	    IF (JOUT.EQ.0)  CALL R2R (ALB,FFF(I),NWKRC) ! into first lat.
	    IF (JOUT.LT.JBB(2)) THEN ! within storage room
	      J=I+ND1+ND1+JOUT 	! "Frost" area for fake latitude, first case
	      FFF(J)=DJU5	! save the season
	      K=I+JOUT		! base index of first real lat. at this season
	      I2=1+N24/2	! index of noon
	      DO J=1,N4		! each latitude
		I=K+J*MM2	! location in first vector for this latitude
		FFF(I)      =TSF(1,J)  ! surface temperature 1am
		FFF(I+ND1)  =TSF(I2,J) ! surface temperature 1pm
		FFF(I+ND1+ND1)    =HEATMM(J) ! surface heat flow
		FFF(I+ND1+ND1+ND1)=FROST4(J) ! frost budget
		FFF(I+4*ND1)      =TTB4(J)   ! T bottom 
	      ENDDO
	    ENDIF
	ELSEIF (K4OUT.EQ.55) THEN	! ......
	    I=1+(NCASE-1)*KASE	! first word for this case
C At first output season of each case, save KRCCOM as the first several seasons.
C  [ koff+seasons, items,case]
	    JOUT=J5-JDISK	! output season offset
	    IF (JOUT.EQ.0)  CALL R2R (ALB,FFF(I),NWKRC) ! into early "seasons"
	    IF (JOUT.LT.MM1) THEN ! within storage room
	      J=I+KOFF*MM1+JOUT		! Skip over KRCCOM and earlier seasons
	      I2=1+N24/2	! index of noon
	      FFF(J      )=DJU5	     ! save the season
	      FFF(J+  MM1)=TSF(1,1)  ! surface temperature 1am
	      FFF(J+2*MM1)=TSF(3,1)  ! surface temperature 3am
	      FFF(J+3*MM1)=TSF(I2,1) ! surface temperature 1pm
	      FFF(J+4*MM1)=7.	     ! spare
	      FFF(J+5*MM1)=TPF(1,1)  ! TOA bolo temperature 1am
	      FFF(J+6*MM1)=TPF(I2,1) ! TOA bolo temperature 1pm
	      FFF(J+7*MM1)=HEATMM(1) ! surface heat flow
	      FFF(J+8*MM1)=FROST4(1) ! frost budget
	      FFF(J+9*MM1)=TTB4(1)   ! T bottom 
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
	    IF (K4OUT.EQ.52) JBB(6) = NCASE 
	    IF (K4OUT.EQ.54) JBB(5) = NCASE  
	    IF (K4OUT.EQ.55) JBB(4) = NCASE 
	    WRITE (IOSP,*)'JBB=',JBB
	    CALL BINF5 ('W',FDISK,HEADER,JBB,FFF,IRET) ! others equivalenced
	    WRITE (IOSP,*)'Wrote bin5 file: type and iret= ',K4OUT,IRET
	    WRITE (IOSP,*)'  File name=',FDISK
	  ELSE			! close direct access file, or message
	    CLOSE (IOD2)
	    WRITE (IOSP,*)'Closed direct access file'
	  ENDIF
	  LOPN2=.FALSE.
	ELSE ! not open
	  IF (K4OUT.LE.50) WRITE (IOERR,*)' TDISK:4, no file was open'
	ENDIF
	NCASE=0			! ensure next file indexing start at 1 
	CALL R2R(0.,FFF,-MTOT) ! ensure possible next set initialized to zero
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
