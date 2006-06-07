	SUBROUTINE TCARD (IQ,IR)
C_Titl  TCARD data input routine for  KRC system
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'filcom.inc'
	INCLUDE 'units.inc'
C_Args
	INTEGER IQ	!in.	1 = read full set or start from disk
C				2 = read change cards
	INTEGER IR	!out. status.  1 = normal start
C			2 = restarted from disk record
C       		3 = continue from current conditions
C			4 = Switch to "one-point" mode
C			5 = END of data
C_Hist	85sep22  Hugh_H_Kieffer  First version was ~ 1971
C 87nov22  HHK put in report if any input item reset
C 93mar93  ECisneros ported to unix
C 97jan30  HHK changed name of  PORB call
C 98sep04  HHK relax some constraints on  IB  &  N1
C 99dec07  HHK add 'continue for memory' option
C 2006mar22 HK If I/O error on read of full card set, prints all current values.
C   User can then look to see where zeros start as indication of error location.
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
	INTEGER LNBLNK ! intrinsic function, need this because of implicit  L
	REAL*4 LSUBS		! function
	PARAMETER(NFDR = 48)		! # of  REAL input variables
	PARAMETER(NIDR = 20)		! # of  INTEGER input variables
	PARAMETER(NLDR = 20)		! # of  LOGICAL input variables

	CHARACTER TEXT*74
	CHARACTER RBUF*80 ! internal file buffer
	CHARACTER*6 TITF(NFDR) 
	CHARACTER*6 TITI(NIDR) 
	CHARACTER*6 TITL(NLDR) 

	DATA TITF /'ALB','EMIS','SKRC','COND2','DENS2'
     1,'PERIOD','SPHT','DENS','CABR','ABRAMP','ABRPHA','PTOTAL'
     1,'FANON','TATM','TDEEP','SPHT2','TAUD','DUSTA','TAURAT'
     1,'TWILI','ARC2','ARC3','SLOPE','SLOAZI','TFROST',' CFROST'
     1,'AFROST','FEMIS','AF1','AF2','FROEXT','FD32','RLAY','FLAY'
     1,'CONVF','DEPTH','DRSET','DDT','GGT','FDOWN','DJUL','DELJUL'
     1,'SDEC','DAU','SUBS','SOLCON','GRAV','ATMCP' /
	DATA TITI /'N1','N2','N3','N4','N5','N24','IB'
     1,'IC','NRSET','NMHA','NRUN','JDISK','IDOWN','I14','I15'
     2,'KPREF','K4OUT','JBARE','NMOD','IDISK2'/
	DATA TITL /'LP1','LP2','LP3','LP4','LP5','LP6'
     1,'LPGLOB','LVFA','LVFT','L10','LPORB','LKEY','LSC','LNOTIF'
     2,'LOCAL','LD16','LD17','LD18','LD19','LONE'/
C
	IR=1
	KOUNT=0

C	write(*,*)'TCARD entry  IQ=',iq
	GO TO (100,160), IQ
C
C initiate commons from input file or from disk saved record  (IQ = 1)
100	NFD=NFDR ! transefer sizes into comon
	NID=NIDR
	NLD=NLDR
        READ (IOIN,*) KOLD,KEEP	! get: 0=input card set,  >0 = disk record
	IF (KOLD.NE.0) THEN
	  IR=2
	  WRITE (IOPM,*)' OLD DATA FILE?'
	  CALL TDISK(1,1)    ! open old file to read starting conditions
	  CALL TDISK(3,KOLD) ! read starting record; will override  KRCCOM
	  IF (KEEP.EQ.0) THEN
	    CALL TDISK(4,0)  ! close 'starting' file
	  ELSE
	    JDISK=J5 ! prepare to start saving to file after first new season
	  ENDIF
	ELSE		!  READ parameter cards
	  READ (IOIN,'(20A4)'     ,END=330) TITLE
	  READ (IOIN,'(/8F10.2)'  ,ERR=331,END=330) (FD(I),I=1,NFD)
	  READ (IOIN,'(/8I10)'    ,ERR=331,END=330) (ID(I),I=1,NID)
	  READ (IOIN,'(/10L7)'    ,ERR=331,END=330) (LD(I),I=1,NLD)
	  READ (IOIN,'(/(10F7.2))',ERR=331,END=330) (ALAT(I),I=1,N4)
	  READ (IOIN,'(/(10F7.2))',ERR=331,END=330) (ELEV(I),I=1,N4)
	ENDIF
	
C  GET orbital parameters if needed
	IF (LPORB) CALL PORB0

C  READ a set of parameter change cards  (IQ = 2)
C
160	IIIN=IOIN
	IF (LKEY) IIIN=IOKEY
	IF (LKEY) WRITE(IOPM,162)
162	FORMAT (' ENTER* CHANGES:  TYPE, LOCATION, NEW VALUE,[TEXT]'
     &/'   TYPE: 0/=DONE, 1=REAL, 2=INT, 3=LOGI, 7=file, ?=HELP')
	GOTO 164
163	WRITE (IOPM,'(A)') RBUF
	WRITE (IOPM,1635)
1635	FORMAT(' input error, try again'
     &/' location is index within the FD, ID, OR LD array'
     &/' new value should be positive number for a logical "true"'
     &/' text = optional ascii description, within quotes')

164	READ (IIIN,'(A80)',ERR=163,END=330) RBUF ! read into character buffer
	READ (RBUF,*,ERR=163,END=330) IG
C	WRITE(*,*)'IG=',IG, '  RBUF=',RBUF
	IF (IG.LT.1) GOTO 320 ! no more changes
	IF (IG.EQ.11) GOTO 310	! "one-point" mode
	READ (RBUF,*,ERR=163,END=330) IG,IREAD,XREAD,TEXT
	ILEN = LNBLNK(TEXT)
	IF (.NOT. LONE .AND. KOUNT.EQ.0) THEN
	  CALL TPRINT (8)
	  WRITE (IOSP,166)
 166	  FORMAT ('0PARAMETER CHANGES'/' TYPE LOC VALUE')
	ENDIF
	WRITE (IOSP,'(2I4,G12.4,1X,A)') IG,IREAD,XREAD,TEXT(1:ILEN)
	KOUNT=KOUNT+1
	GO TO (210,220,230,240,250,260,270,280,100,300,310,160), IG

210	FD(IREAD)=XREAD			!  IG=1: change  REAL parm
	WRITE (IOSP,*)'Changed Float: ',TITF(IREAD)
	GO TO 160
220	ID(IREAD)=NINT(XREAD)		!  IG=2: change  INTEGER parm
	IF (IREAD.EQ.12) NCASE=0
	WRITE (IOSP,*)'Changed Integer: ',TITI(IREAD)
	GO TO 160
 230	IF (IREAD.GT.20) THEN	! special flag  
	  IF (IR.NE.2) IR=3	! set "continue" unless restarting
	  WRITE (IOSP,'(1X,A)') 'TCARD sets IR to: continue from memory'
	ELSE
	  LD(IREAD)=XREAD.GT.0.	!  IG=3: change  LOGICAL parm
	WRITE (IOSP,*)'Changed Logical: ',TITL(IREAD)
	ENDIF
	GO TO 160
240	READ (IIIN,'(10F7.2)',END=330) (ALAT(I),I=1,N4) !  IG=4: read latitudes
	GO TO 160
250	READ (IIIN,'(10F7.2)',END=330) (ELEV(I),I=1,N4) !  IG=5: read elevations
	GO TO 160
260	CALL PORB0			!  IG=6: read  PORB data lines
	GO TO 160
270	READ (TEXT,'(20A4)',END=160) TITLE !  IG=7: change  TITLE
	GO TO 160
280     CALL TDISK(4,0)			!  IG=8: new disk file name, close current
	  FDISK = TEXT		! new name
	  WRITE (IOSP,*)'New Disk file name= ',FDISK(1:ILEN)
	GOTO 160

 300	FINPUT=TEXT(1:ILEN)	! name of next input file
	IR=4			! flag for switch to new input file
C	write(*,*)'TCARD setting IR=4'
	RETURN

 310	xread=123.456		! one-point model
C	write(*,313)XREAD,ALAT(1),HOURO,ELEV(1) 
C	1    ,ALB,SKRC,TAUD,SLOPE,SLOAZI
	READ (RBUF,313,ERR=317,END=318) XREAD,ALAT(1),HOURO,ELEV(1) 
	1    ,ALB,SKRC,TAUD,SLOPE,SLOAZI
C                  ls   lat hour Elev  Alb Iner Opac Slop Azim
 313	FORMAT(2X,F6.1,F6.1,F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0)
C       10322.34 =  start of mars year circa 1986aug26, base 2440000
C  Set starting date so that last season will be at requested L_s
	DJUL=10322.34+ALSUBS(2,XREAD)-(N5-1)*DELJUL
C	write(*,*)'TCARD got: ', XREAD,ALAT(1),HOURO,ELEV(1) 
C	1    ,ALB,SKRC,TAUD,SLOPE,SLOAZI,djul
C	write(*,313) XREAD,ALAT(1),HOURO,ELEV(1) 
C	1    ,ALB,SKRC,TAUD,SLOPE,SLOAZI
C	write(*,*)'TCARD returning with IR=',IR
	RETURN
 317	 write(IOERR,*)'TCARD: Error reading internal buffer for one-point'
	 IR=5
	return
 318	write(IOERR,*)'TCARD: EOF reading internal buffer for one-point'
	 IR=5
	return
C
C quit if there was no interactive input
C
320	IF (IQ.EQ.2 .AND. KOUNT.EQ.0) GOTO 330
C
C  NORMAL return - check input parameters. bound array dimensions
C
	N1PIB = N1		! set lowest layer used in calculations
	IF (IB.GE.1) N1PIB=N1+1 !   :condition for constant  T_bottom
	IF (N1.LT.2   .OR. N1PIB.GT.MAXN1) THEN
		NEW = MAXN1/2 ! insure within dimen
		WRITE (IOERR,388) 'N1',N1,NEW
388	FORMAT (/1X,A,' invalid. Input and reset = ',2I6)
		N1 = NEW
		N1PIB = N1	! must reset 
		IF (IB.GE.1) N1PIB=N1+1 !
	    ENDIF
	IF (N2.LT.32  .OR. N2.GT.MAXN2) THEN
		NEW = 384	! insure within dimen
		WRITE (IOERR,388) 'N2',N2,NEW
		N2 = NEW
	    ENDIF
	IF (N3.LT.1 .OR. N3.GT.MAXN3-1) THEN  ! 1 for j3+1 in  TDAY
		NEW = MAXN3/2	
		WRITE (IOERR,388) 'N3',N3,NEW
		N3 = NEW
	    ENDIF
	IF (N4.LT.1  .OR. N4.GT.MAXN4) THEN
		NEW = MAXN4/2	! insure within dimen.
		WRITE (IOERR,388) 'N4',N4,NEW
		N4 = NEW
	    ENDIF
	IF (N24.LT.2  .OR. N24.GT.MAXNH) THEN
		NEW = 24	! insure within dimen.
		WRITE (IOERR,388) 'N24',N24,NEW
		N24 = NEW
	    ENDIF
	NMOD = MAX(NMOD,1)	! insure positive modulo
	IF (DRSET.LT.0. .OR. DRSET.GT.0.) THEN ! insure reasonable
 389	  FORMAT (/1X,A,' invalid. Input and reset = ',2g12.5)
	  q=0.
	  WRITE (IOERR,389) 'DRSET',DRSET,q
	  DRSET=q
	ENDIF
	IF (TAUD.LT.0.) THEN	! insure physically valid
	  q=0.
	  WRITE (IOERR,389) 'TAUD',TAUD,q
	  TAUD=q
	ENDIF
	RETURN
C
C no more input data
C
 331	WRITE (IOSP,*),'TCARD: IO error'
	N4=MAX(N4,1)
	CALL TPRINT(2)
	STOP


330	IR=5
	WRITE (IOSP,'(//5X,A)') 'END OF DATA ON INPUT UNIT'
 9	RETURN
	END
