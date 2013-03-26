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
	INTEGER*4 KOMMON,MAXDIM
        PARAMETER (KOMMON=1056720) ! Type 52, 48 hours, 37 lats,84 seasons
	PARAMETER (MAXDIM=6)	! max number of dimensions needed
        REAL*4 FFF (KOMMON)	!   the largest of the 5n types
	INTEGER*4 MMM(MAXDIM)	! to hold cumulative size of each dimension
        INTEGER*4 JJJ(10)	! sizes to go to  BINF5
        Real*4 FRONT(4)		! Leading size integers converted to real
	INTEGER*4 KASE,KODED	! words/case, file type
	COMMON /BINCOM/ JJJ,KODED,KASE,FFF ! ensure these are remembered
C_Args
	INTEGER*4 KODE		! in. control
C  1 = open file.  then see  KREC
C        will set  LOPN2=.true. if file opened
C  2 = write a season. appended after  JREC.  KREC ignored
C  3 = read a season.  input  KREC as record number,  IERR returned as iostat
C	also, sets record position to append on next write
C  4 = close the file.  KREC ignored
C  5 = write a record of  KRCCOM.  KREC ignored
	INTEGER*4 KREC	!in (when  KODE=1): file status: 0=new  1=old
C_Desc
C Can write several styles of binary files; controled by K4OUT. See helplist.txt
C The two basic families are Direct access files containing combinations of
C the COMMON arrays, and bin5 files of specific variables and arrays.
C The bin5 files have K4OUT=50+; the parameter statements above set the 
C maximum allowed sizes, but they are written dense to the requested array
C sizes, and BINF5 writes only the utilized part of the storage allocated.
C 2008oct22 All bin5 files have 4 indices and KRCCOM [and other vectors) in the 
C first few latitudes or seasons. The indices are always: NWKRC, 
C     1-based index of the dimension that has the extra leading values, 
C     how many extra are used, and spare
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
C 2006Oct16 HK Add two convergence items into output file type 52
C 2008mar31 HK Ensure all INTEGER are INTEGER*4 before successful 63-bit compile
C 2008apr13 HK Add spaced TMIN and TMAX to Type 52
C 2008sep30-oct15 HK Add type 56, Revise type 51 and 52, remove type 53
C 2009feb23 HK recode all bin5 outputs
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C JJJ are the IDL SIZE sent to BIN5 files;  JJJ[1]= dimensionality
C MMj are the number of words in the j'th dimension
C ie MM1 =JJJ[2], MM2=MM1*JJJ[3] etc   OR MM4,MM5 used as saved other values

C Direct write will zero-fill unused part of record
	CHARACTER CSTAT*3
	REAL*4 COMKRC(NWKRC),COMLAT(NWLAT),COMDAY(NWDAY)
	EQUIVALENCE (COMKRC,ALB),(COMLAT,NDJ4),(COMDAY,XCEN)

        CHARACTER*30 HEADER /'KRC-tes custom save'/
        INTEGER*4 HEADLEN /30/
        INTEGER*4 JREC,KOFF,NWTOT,NRECL,I,I2,I4,IOS,J,K,IRET
C JREC is the 1-based output record number
	INTEGER*4 HEAD,IDX,NDX,MM1,MM2,MM3,MM4,MM5 ! multiple array dimensions
	INTEGER*4 NSOUT		! number of seasons expected to be output

	SAVE IDX,NDX,JREC,KOFF,MASE,MMM ! insure these 
	SAVE FRONT,NSOUT	! remain defined
C
D	IF (KODE.NE.2) WRITE(IOSP,*)'TDISKa ',KODE,KREC,NCASE,J5,K4OUT
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
C Solaris: is longwords, since file is unformatted (default for direct access)
	IF (K4OUT.LT.50) THEN	! Open direct-access output file
	  IF (K4OUT.LT.0) THEN	!  K4OUT is negative
	    NWTOT=2*MAXNH*MAXN4 !  TSF  &  TPF only
	  ELSEIF (K4OUT.EQ.0) THEN
	    NWTOT=NWKRC+NWLAT	!  KRCCOM & LATCOM
	  ELSE			!  K4OUT is 1:49
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
	  JJJ(4) = 0		!
	  JJJ(5) = 0		!
	  JJJ(6) = 0		!
	  JJJ(7) = 0		!
	  JJJ(8) = 4		! set type as  REAL*4
	  JJJ(9) = HEADLEN	! header length
	  NSOUT=N5-JDISK+1 ! number of seasons to output
	  IF (K4OUT.EQ.51) THEN 
	    ND4=N4		! remember how many latitudes in first case
	    JJJ(1) = 5		! # of dimensions
	    JJJ(2) = N24	! Size of Dim. 1: # hours output
	    JJJ(3) = 2		! Size of Dim. 2: 2 temperature
	    JJJ(4) = N4		! Size of Dim. 3: latitudes
	    JJJ(5) = NSOUT	!  number of seasons to come.
	    HEAD   =5*NSOUT	! Extra words in case header
	  ELSEIF (K4OUT.EQ.52) THEN 
	    ND4=N4		! remember how many latitudes in first case
	    JJJ(1) = 5		! # of dimensions
	    JJJ(2) = N24	! # hours output
	    JJJ(3) = 7		! 3 temperature, 2 fluxes, 2 layers
	    JJJ(4) = N4		! dimen of latitudes
	    JJJ(5) = NSOUT	!  number of seasons to come.
	    HEAD   =5*NSOUT	!  Extra words in case header
	    WRITE(IOPM,*)'N4=',n4
	  ELSEIF (K4OUT.EQ.54) THEN
	    JJJ(1) = 4		! # of dimensions
	    JJJ(2) = NSOUT	! expected total seasons
	    JJJ(3) = 5		! the items
            JJJ(4) = N4		! # latitudes 
	    HEAD=NSOUT		! extra words in case header
	  ELSEIF (K4OUT.EQ.55) THEN
	    JJJ(1) = 3		! # of dimensions
	    JJJ(2) = NSOUT	! # seasons to store
	    JJJ(3) = 9		!  constant must agree with # items stored 
	    HEAD=NSOUT
	  ELSEIF (K4OUT.EQ.56) THEN
	    JJJ(1) = 4		! # of dimensions
	    JJJ(2) = 2*N24+N1+3	! # words in a latitude
	    JJJ(3) = N4		! # latitudes
	    JJJ(4) = NSOUT	! number of seasons to come.
	    HEAD=5*NSOUT	! words in case header
          ELSE
	     WRITE (IOERR,*)' TDISK Invalid K4OUT fatal: ',K4OUT
	     LOPN2=.FALSE.
	     GOTO 9
	  ENDIF
D	  WRITE(IOSP,13)K4OUT,JJJ
 13	  FORMAT ('Initiated custom output: K4OUT=',I3,/'JJJ=',10I6)
C at this point, jbb(2:n) contain the dimension needed for data.
C need to update next to last for NDX, and compute the number of cases possible
	  WRITE(IOPM,*) 'N4,jjj=',n4,jjj
	  IDX=JJJ(1)-1		! dimension that has extra size
	  WRITE(IOPM,*)'IDX=',idx
CC	  IF (IDX.LT.2) THEN STOP ! ensure enough dimensions for this scheme
	  MMM(1)=JJJ(2)		! compute number of words for each dimension
	  DO J=2,IDX		! up to the largest used
	     MMM(J)=MMM(J-1)*JJJ(J+1) !
	  ENDDO
	  K=4+NWKRC+HEAD	! Increase case header for standard things
	  NDX=(K-1)/MMM(IDX-1)+1 ! extra Dim N-1 needed to hold case header
	  WRITE(IOPM,*)'NDX,K=',NDX,K
	  JJJ(IDX+1)=JJJ(IDX+1)+NDX ! revised this dimension
	  WRITE(IOPM,*)'Idx,JJJ=',IDX,JJJ
	  WRITE(IOPM,*)'MMM=',MMM
	  WRITE(IOSP,*)'Idx,JJJ=',IDX,JJJ
	  WRITE(IOSP,*)'MMM=',MMM
	  KASE=MMM(IDX-1)*JJJ(IDX+1) ! # words in a case
	  MASE=KOMMON/KASE	! # cases that could be accomodated
	  KODED=K4OUT		! transfer into Common
	  FRONT(1)=FLOAT(NWKRC)	! first 4 words are sizes used
	  FRONT(2)=FLOAT(IDX)	! 1-based index of dimension with extra values
	  FRONT(3)=FLOAT(NDX)	! Number of those extra
	  FRONT(4)=FLOAT(NSOUT)	! not used yet
	  WRITE(IOSP,*),'KOMMON,KASE,MASE,MTOT=',KOMMON,KASE,MASE,KASE*MASE
	  JREC=0		! records written thus far = none
	  LOPN2=.TRUE.
	  IF (MASE.LT.1) LOPN2=.FALSE. ! KASE larger than KOMMON
	ENDIF
	GOTO 9
C
191	WRITE (IOERR,*) ' TDISK:1, ERROR OPENING FILE. IOSTAT=',IOS
	WRITE (IOERR,*) '   IOD2=',IOD2,'  status=',cstat,'  recl=',nrecl
	WRITE (IOERR,*) '   file=  ',FDISK
	LOPN2=.FALSE.
	GOTO 9
C
C write next record (internal record count)		2  2  2  2  2  2  2  2
 200	IF (K4OUT.LT.50) THEN !---------------------------------------------..
	  IF (LOPN2) THEN	! save current values
	    JREC=JREC+1
	    I=JREC		! may be incremented by the  WRITE command
	    IF (K4OUT.LT.0) THEN ! K4OUT negative
	      WRITE(IOD2,REC=I)TSF,TPF
	    ELSEIF (K4OUT.EQ.0) THEN
	      WRITE(IOD2,REC=I)COMKRC,COMLAT
	    ELSE		! K4OUT=1:49
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

C get here if type 5x  !---------------------------------------
D	  WRITE(IOSP,*)'TDISKb ',kode,krec,ncase,j5,' jd,jjj',jdisk,JJJ
	IF (NCASE.GT.MASE) GOTO 9 ! no room left for storage
	JOUT=J5-JDISK		! 0-based season count after start to disk
	JREC=JOUT+1		! 1-based count of this record
	IF (JREC.GT.NSOUT) GOTO 9 ! beyond allocated season range
	I=(NCASE-1)*KASE+1	! first word for this case
	IF (JOUT.EQ.0) then	! first season of this case
	   CALL R2R(0.,FFF(I),-KASE) ! zero the case
	   CALL R2R (FRONT,FFF(I),4) ! first 4 words are sizes used
	   CALL R2R (ALB,FFF(I+4),NWKRC) ! followed by KRCCOM
	ENDIF
	I4=I+4+NWKRC      ! first word in prefix after standard case header
	MM1=MMM(1)		! may be needed several times in 5x sections
	MM2=MMM(2)
	MM3=MMM(3)
	MM4=MMM(4)
	IF (K4OUT.EQ.51) THEN !---------------------------------------
C For Global temperatures: 2008oct16
C [N24,2,Nlat,nseas+x,ncase]   First x "seasons" of each case contains: 
C   Float of NWKRC & nlay & nlat & nseas, KRCCOM, DJU5(nseas),SUBS(nseas)
C   ,PZREF(nseas), TAUD(nseas), SUMF(nseas)
C True seasons contain for every hour: TSF,TPF
	  MM4 = MIN(N1-1,MM1-2) ! compute # layers to store
	  IF (JOUT.EQ.0) THEN	! insure done only once per case 
	    J=N5-JDISK+1+NDX	! season dimension required 
	    IF (N24.NE.JJJ(2) .OR. N4.GT.JJJ(4) .OR. J.NE.JJJ(5)) THEN
	       WRITE (IOERR,*)
	1 'Change of NHOUR, NLAT, or #to Disk not allowed for K4OUT=51'
	       WRITE (IOERR,*)N24,MM1, N4,JJJ(4) ,J,JJJ(5)
	       GOTO 9
	    ENDIF
	  ENDIF
	  J=I4+JOUT		! loc of this seasons DJU5
	  FFF(J) =DJU5		! date for this case and season
	  FFF(J+NSOUT) =SUBS
	  FFF(J+NSOUT+NSOUT) =PZREF
	  FFF(J+3*NSOUT) =TAUD
	  FFF(J+4*NSOUT) =SUMF
	  J=I+MM3*(NDX+JOUT)	! first word of this case, season
	  DO J4=1,N4		! do each latitude
	      I=J+(J4-1)*MM2 ! first word of this case, season, latitude
	      CALL R2R (TSF(1,J4),FFF(I)          ,MM1) ! copy one day
	      CALL R2R (TPF(1,J4),FFF(I+MM1)      ,MM1)
	   ENDDO
	ELSEIF (K4OUT.EQ.52) THEN !------------------------------------
C For hourly conditions:  revised 2004jul22  and 2004Oct06 and 2008oct15
C [N24,7,Nlat,x+nseas,ncase]   First x "seasons" of each case contains: 
C   Float of Front+KRCCOM, DJU5(nseas),SUBS(nseas)
C   ,PZREF(nseas), TAUD(nseas), SUMF(nseas)
C True seasons contain for every hour: TSF,TPF,TAF,DOWNVIS,DOWNIR
C  and float(NDJ4)+ DTM4 + TTA4+ Tmin(Nlay-) Omitting virtual first layer
C  and    FROST4+ AFRO4+ HEATMM+ Tmax(Nlay-) " " "
C the Nlay- are as many layers as fit within the number of Hours
	   MM4 = MIN(N1-1,MM1-2) ! compute # layers to store
	   IF (JOUT.EQ.0) THEN	! insure done only once per case 
	     J=N5-JDISK+1+NDX	! season dimension required 
	     IF (N24.NE.JJJ(2) .OR. N4.GT.JJJ(4) .OR. J.NE.JJJ(5)) THEN
	       WRITE (IOERR,*)
	1 'Change of NHOUR, NLAT, or #to Disk not allowed for K4OUT=52'
	       WRITE (IOERR,*)N24,MM1, N4,JJJ(4) ,J,JJJ(5)
	       GOTO 9
	     ENDIF
	   ENDIF
	   J=I4+JOUT		! loc of this seasons DJU5
	   FFF(J) =DJU5		! date for this case and season
	   FFF(J+NSOUT) =SUBS
	   FFF(J+NSOUT+NSOUT) =PZREF
	   FFF(J+3*NSOUT) =TAUD
	   FFF(J+4*NSOUT) =SUMF
	   J=I+MM3*(NDX+JOUT)	! first word of this case, season
	   DO J4=1,N4		! do each latitude
	     I=J+(J4-1)*MM2	! first word of this case, season, latitude
	     CALL R2R (TSF(1,J4),FFF(I)          ,MM1) ! copy one day
	     CALL R2R (TPF(1,J4),FFF(I+MM1)      ,MM1)
	     CALL R2R (TAF(1,J4),FFF(I+MM1+MM1)  ,MM1)
	     CALL R2R (DOWNVIS(1,J4),FFF(I+3*MM1),MM1)
	     CALL R2R (DOWNIR (1,J4),FFF(I+4*MM1),MM1)
	     K=I+5*MM1		! first index of item 6
	     FFF(K)=FLOAT(NDJ4(J4)) ! # days to compute solution 
	     FFF(K+1)=DTM4(J4)	! rms temperature change on last day
	     FFF(K+2)=TTA4(J4) 
	     CALL R2R (TIN(2,J4),FFF(K+3),MM4)
	     K=K+MM1		! first index of item 7
	     FFF(K)=FROST4(J4)	! Frost at midnight
	     FFF(K+1)=AFRO4(J4) ! frost albedo
	     FFF(K+2)=HEATMM(J4)  
	     CALL R2R (TAX(2,J4),FFF(K+3),MM4)
	   ENDDO
D	   WRITE(iosp,*)'JOUT,J,I,K',JOUT,J,I,K
	ELSEIF (K4OUT.EQ.54) THEN	!------------------------------------
C for Heat flow. [seasons, 5 items, x+latitudes, cases]
C Save surface temperature and heat flow at midnight
C   as function of season and latitude.
C Items are: Tsur 1am, Tsur 1pm, heat flow, frost, Tbot
C  First x "latitudes" of each case contains: 
C     Float of Front+KRCCOM, DJU5(nseas)
	   FFF(I4+JOUT)=DJU5	! save the season in header
	   K=I+(NDX-1)*MM2+JOUT ! base index of 0 real lat. at this season
	   I2=1+N24/2		! index of noon
	   DO J=1,N4		! each latitude
	      I=K+J*MM2		! location in first vector for this latitude
	      FFF(I)      =TSF(1,J) ! surface temperature 1am
	      FFF(I+MM1)  =TSF(I2,J) ! surface temperature 1pm
	      FFF(I+MM1+MM1)    =HEATMM(J) ! surface heat flow
	      FFF(I+MM1+MM1+MM1)=FROST4(J) ! frost budget
	      FFF(I+4*MM1)      =TTB4(J) ! T bottom 
	   ENDDO
	ELSEIF (K4OUT.EQ.55) THEN	! ------------------------------
C for seasonal studies at one latitude, [seasons,x+9 items,cases]
C Items: Ts 1am, Ts 3am, Ts 1pm, spare, Tp 1am, Tp 1pm, heat flow, frost, T_bot.
C  First x "items" of each case contains: 
C     Float of Front+KRCCOM, DJU5(nseas)
	   FFF(I4+JOUT)=DJU5	! save the season in the header
	   J=I+(NDX-1)*MM1+JOUT ! base index of 0 real item at this season
	   I2=1+N24/2		  ! index of noon
	   FFF(J+  MM1)=TSF(1,1)  ! surface temperature 1am
	   FFF(J+2*MM1)=TSF(3,1)  ! surface temperature 3am
	   FFF(J+3*MM1)=TSF(I2,1) ! surface temperature 1pm
	   FFF(J+4*MM1)=DJU5	  ! spare   DJU5 is redundant with prefix
	   FFF(J+5*MM1)=TPF(1,1)  ! TOA bolo temperature 1am
	   FFF(J+6*MM1)=TPF(I2,1) ! TOA bolo temperature 1pm
	   FFF(J+7*MM1)=HEATMM(1) ! surface heat flow
	   FFF(J+8*MM1)=FROST4(1) ! frost budget
	   FFF(J+9*MM1)=TTB4(1)	  ! T bottom 
        ELSEIF (K4OUT.EQ.56) THEN ! ------------------------------
C (TSF+TPF+TMN4+3,latitudes,x+seasons, cases)
C For each latitude, surface and planet temperature every hour, 
C   plus for each layer, midnight temp.
C   plus frost, heatflow, midnight atm temp
C Header contains KRCCOM plus, sub-array(seasons,5) containing:
C    djul, L-sub-s, global pressure, tauD, total_frost
	   J=I4+JOUT
	   FFF(J)=DJU5		! load as if an additional season
	   FFF(J+NSOUT)=SUBS
	   FFF(J+2*NSOUT)=PZREF
	   FFF(J+3*NSOUT)=TAUD
	   FFF(J+4*NSOUT)=SUMF
	   K=I+MM2*(NDX+JOUT)	! Skip over KRCCOM and earlier seasons
	   DO J=1,N4		! each latitude
	     I=K+(J-1)*MM1	! location in first vector for this latitude 
	     CALL R2R (TSF(1,J),FFF(I) ,N24) ! every hour
	     I=I+N24 
	     CALL R2R (TPF(1,J),FFF(I) ,N24) ! every hour
	     I=I+N24
	     CALL R2R (TMN4(1,J),FFF(I) ,N1) ! every layer at midnight
	     I=I+N1
	     FFF(I)  =FROST4(J)	! predicted frost amount kg/m^2.
	     FFF(I+1)=HEATMM(J) ! Mean upward heat flow into surface
	     FFF(I+2)=TTA4(J)	! predicted final atmosphere temperature 
	   ENDDO
        ENDIF			!---------------------------------------------
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
	    JJJ(IDX+2) = NCASE
	    WRITE (IOSP,*)'JJJ=',JJJ
	    CALL BINF5 ('W',FDISK,HEADER,JJJ,FFF,IRET) ! others equivalenced
	    WRITE (IOSP,*)'Wrote bin5 file: type and iret= ',K4OUT,IRET
	    WRITE (IOSP,*)'  File name=',FDISK
	  ELSE			! close direct access file, or message
	    CLOSE (IOD2)
	    WRITE (IOSP,*)'Closed direct access file'
	  ENDIF
	  LOPN2=.FALSE.
	ELSE ! not open
	  IF (K4OUT.LT.50) WRITE (IOERR,*)' TDISK:4, no file was open'
	ENDIF
	NCASE=0			! ensure next file indexing start at 1 
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
	
 9	CONTINUE
D       IF (KREC.NE.0) WRITE(IOPM,*)'TDISKc  KREC=',KREC
	RETURN
	END
