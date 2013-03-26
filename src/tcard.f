	SUBROUTINE TCARD (IQ,IRET)
C_Titl  TCARD data input routine for  KRC system
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'daycom.inc'
	INCLUDE 'filcom.inc'
	INCLUDE 'units.inc'
C       INCLUDE 'titcom.inc'
C_Args
	INTEGER*4 IQ	!in.	1 = read full set and optional changes
C                                   or start from disk
C				2 = read change cards
	INTEGER*4 IRET	!out. status.  1 = normal start
C			2 = restarted from disk record
C       		3 = continue from current conditions
C			4 = Switch to "one-point" mode
C			5 = END of data
C_Hist	85sep22  Hugh_H_Kieffer  First version was ~ 1971
C 87nov22  HHK put in report if any input item reset
C 93mar93  ECisneros ported to unix
C 97jan30  HHK changed name of  PORB call
C 98sep04  HHK relax some constraints on  IB  &  N1
C 99dec07  HHK add 'continue from memory' option
C 2006mar22 HK If I/O error on read of full card set, prints all current values.
C 2008oct02 HK Replace ID22(1) and (2) with KVALB and KVTAU
C 2008nov13 HK Add T-dependent conductivity parameters
C 2009feb24 HK Briefly try using titcom.inc and block data for parameter titles
C 2009may10 HK add TITONE as one-point comment field, Add 1. to start date
C   User can then look to see where zeros start as indication of error location.
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
	INTEGER*4 LNBLNK ! intrinsic function, need this because of implicit  L
	REAL*4 ALSUBS		! function
	CHARACTER TEXT*74
	CHARACTER RBUF*80 ! internal file buffer
	CHARACTER*8 WHAT ! distinguish what was expected to read
        PARAMETER(NFDR = 56)            ! # of  REAL input variables
        PARAMETER(NIDR = 20)            ! # of  INTEGER*4 input variables
        PARAMETER(NLDR = 20)            ! # of  LOGICAL input variables
        CHARACTER*8 TITF(NFDR) 
        CHARACTER*6 TITI(NIDR) 
        CHARACTER*6 TITL(NLDR) 

	DATA TITF /'ALBEDO','EMISS','INERTIA','COND2','DENS2','PERIOD'
     1,'SPEC_HEAT','DENSITY','CABR','AMW','ABRPHA','PTOTAL','FANON'
     1,'TATM','TDEEP','SpHeat2','TAUD','DUSTA','TAURAT','TWILI'
     1,'ARC2','ARC3','SLOPE','SLOAZI','TFROST','CFROST','AFROST'
     1,'FEMIS','AF1','AF2','FROEXT','spare','RLAY','FLAY','CONVF'
     1,'DEPTH','DRSET','DDT','GGT','DTMAX','DJUL','DELJUL','SolarDec'
     1,'DAU','L_S','SOLCON','GRAV','Atm_Cp','ConUp0','ConUp1','ConUp2'
     1,'ConUp3','ConLo0','ConLo1','ConLo2','ConLo3'/

	DATA TITI /'N1','N2','N3','N4','N5','N24','IB','IC'
     1,'NRSET','NMHA','NRUN','JDISK','IDOWN','I1','I15','KPREF'
     1,'K4OUT','JBARE','NMOD','IDISK2'/

	DATA TITL /'LP1','LP2','LP3','LP4','LP5','LP6'
     1,'LPGLOB','LVFA','LVFT','LKofT','LPORB','LKEY','LSC','LNOTIF'
     1,'LOCAL','LD16','LPTAVE','Prt.78','Prt.79','L_ONE'/

C
	IRET=1			! normal return is a new case
	IF (J5.GT.1 .AND. J5.EQ.IDOWN) THEN IRET=3 ! continue after changes
	KOUNT=0
D	WRITE(*,*)'TCARD entry  IQ,J5=',IQ,J5 !<<<dbug
D	WRITE(IOSP,*)'TCARD entry  IQ,J5=',IQ,J5 !<<<dbug
	GO TO (100,160), IQ
C
C initiate commons from input file or from disk saved record  (IQ = 1)
100	NFD=NFDR ! transfer sizes into common
	NID=NIDR
	NLD=NLDR
        READ (IOIN,*) KOLD,KEEP	! get: 0=input card set,  >0 = disk record
	IF (KOLD.NE.0) THEN
	  IRET=2
	  WRITE (IOPM,*)' OLD DATA FILE?'
	  CALL TDISK(1,1)    ! open old file to read starting conditions
	  CALL TDISK(3,KOLD) ! read starting record; will override  KRCCOM
	  IF (KEEP.EQ.0) THEN
	    CALL TDISK(4,0)  ! close 'starting' file
	  ELSE
	    JDISK=J5 ! prepare to start saving to file after first new season
	  ENDIF
	ELSE		!  READ parameter cards
	   j=0			! in case of IO error
	  READ (IOIN,'(20A4)'     ,END=430) TITLE
	  READ (IOIN,'(/8F10.2)'  ,ERR=431,END=430) (FD(I),I=1,NFD)
	  READ (IOIN,'(/8I10)'    ,ERR=432,END=430) (ID(I),I=1,NID)
	  READ (IOIN,'(/10L7)'    ,ERR=433,END=430) (LD(I),I=1,NLD)
	  READ (IOIN,'(/(10F7.2))',ERR=434,END=430) (ALAT(I),I=1,N4)
	  READ (IOIN,'(/(10F7.2))',ERR=435,END=430) (ELEV(I),I=1,N4)
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

164	READ (IIIN,'(A80)',ERR=163,END=430) RBUF ! read into character buffer
	READ (RBUF,*,ERR=163,END=430) IG
	WRITE(*,*)'IG=',IG, '  RBUF=',RBUF
	IF (IG.LT.1) GOTO 370 ! no more changes
	IF (IG.EQ.11) GOTO 310	! "one-point" mode
	KOUNT=KOUNT+1
	IF (IG.EQ.12) GOTO 370	! read 8 conductivity parameters
	READ (RBUF,*,ERR=163,END=430) IG,IREAD,XREAD,TEXT
	ILEN = LNBLNK(TEXT)
	IF (.NOT. LONE .AND. KOUNT.EQ.0) THEN
	  CALL TPRINT (8)
	  WRITE (IOSP,166)
 166	  FORMAT ('0PARAMETER CHANGES'/' TYPE LOC VALUE')
	ENDIF
 167	FORMAT (2I4,G12.4,1X,A,2x,A,'<Changed')
	IF (IG.GT.3) WRITE (IOSP,167) IG,IREAD,XREAD,TEXT(1:ILEN)
	GO TO (210,220,230,240,250,260,270,280,100,300,310,320), IG

210	IF (IREAD.LT.1 .OR. IREAD.GT.NFDR) GOTO 295
	FD(IREAD)=XREAD		!  IG=1: change  REAL parm
	WRITE (IOSP,167)IG,IREAD,XREAD,TEXT(1:ILEN),TITF(IREAD)
	GO TO 160

220	IF (IREAD.LT.1 .OR. IREAD.GT.NIDR) GOTO 295
	ID(IREAD)=NINT(XREAD)		!  IG=2: change  INTEGER*4 parm
C	IF (IREAD.EQ.12) NCASE=0 ! JDISK: 2009mar reason lost
	WRITE (IOSP,167)IG,IREAD,XREAD,TEXT(1:ILEN),TITI(IREAD)
	GO TO 160

230	IF (IREAD.LT.1 .OR. IREAD.GT.NLDR) GOTO 295
	LD(IREAD)=XREAD.GT.0.	!  IG=3: change  LOGICAL parm
	WRITE (IOSP,167)IG,IREAD,XREAD,TEXT(1:ILEN),TITL(IREAD)
	GO TO 160

240	READ (IIIN,'(10F7.2)',END=430) (ALAT(I),I=1,N4) !  IG=4: read latitudes
	GO TO 160

250	READ (IIIN,'(10F7.2)',END=430) (ELEV(I),I=1,N4) !  IG=5: read elevations
	GO TO 160

260	CALL PORB0			!  IG=6: read  PORB data lines
	GO TO 160

270	READ (TEXT,'(20A4)',END=160) TITLE !  IG=7: change  TITLE
	GO TO 160

 280	IF (IREAD.EQ.22) THEN	! IG=8 Read file name
	  FVALB=TEXT		! move file name into common
	  I=SEASALB(-999.)	! READ DATA FILE
	  KVALB=0		! SET FLAG OFF
	  IF (I.GT.1) KVALB=1	! SET VARIABLE ALBEDO FLAG
	  write (*,*) 'TCARD I back=',i
	ELSEIF (IREAD.EQ.23) THEN 
	  FVTAU=TEXT		! move file name into common
	  I=SEASTAU(-999.)	! READ DATA FILE
	  KVTAU=0		! SET FLAG OFF
	  IF (I.GT.1) KVTAU=1	! SET VARIABLE Tau FLAG
	ELSE
	  CALL TDISK(4,0)	! Default: new disk file name, close current
	  FDISK = TEXT		! move new file name into common
	  WRITE (IOSP,*)'New Disk file name= ',FDISK(1:ILEN)
	ENDIF
	write(*,*)'TCARD_280',IREAD, I, KVALB,KVTAU
	GOTO 160

 295	WRITE(IOERR,*)'Invalid change index: ',RBUF
	GOTO 160

 300	FINPUT=TEXT(1:ILEN)	! 10. name of one-point input file
	IRET=4			! flag for switch to new input file
C	write(*,*)'TCARD setting IRET=4'
	GOTO 9

 310	xread=123.456		! one-point model
	WHAT='1-Point'
	READ (RBUF,3103,ERR=417,END=418) XREAD,ALAT(1),HOURO,ELEV(1) 
	1    ,ALB,SKRC,TAUD,SLOPE,SLOAZI,TITONE
C                  ls   lat hour Elev  Alb Iner Opac Slop Azim
 3103	FORMAT(2X,F6.1,F6.1,F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0,A20)
C       10322.34 =  start of mars year circa 1986aug26, base 2440000
C  Set starting date so that last season will be at requested L_s
C 2009may11  Add emperical amount to yield reguested L_s 
	DJUL=10322.34+ALSUBS(2,XREAD)-(N5-1)*DELJUL + 1. 
D	write(*,*)'TCARD got: ', XREAD,ALAT(1),HOURO,ELEV(1) !<<<debug
D	1    ,ALB,SKRC,TAUD,SLOPE,SLOAZI,TITONE,djul         !<<<debug
D	write(*,*)'TCARD returning with IRET=',IRET          !<<<debug
	GOTO 9

 320	WHAT='8-Cond.'
	READ (RBUF,*,ERR=417,END=418) IG,(FD(I),I=49,56) !ConUp0:ConLo3
	GO TO 160
C
C quit if there was no interactive input
C
370	IF (IQ.EQ.2 .AND. KOUNT.EQ.0) GOTO 430
C
C  NORMAL return -  Check input parameters.  Bound array dimensions
C
	N1PIB = N1		! set lowest layer used in calculations
	IF (IB.GE.1) N1PIB=N1+1 !   :condition for constant  T_bottom
	IF (N1.LT.2   .OR. N1PIB.GT.MAXN1) THEN
	   NEW = MAXN1/2	! insure within dimen
	   WRITE (IOERR,388) 'N1',N1,NEW
 388	   FORMAT (/1X,A,' invalid. Input and reset = ',2I6)
	   N1 = NEW
	   N1PIB = N1		! must reset 
	   IF (IB.GE.1) N1PIB=N1+1 !
	ENDIF
	IF (IC.LT.3 .OR. IC.GT.N1-2) THEN ! no change near contacts
	   NEW=999	
	   WRITE (IOERR,388) 'IC',IC,NEW
	   IC = NEW
	ENDIF
	IF (N2.LT.32 .OR. N2.GT.MAXN2) THEN
	   NEW = MAXN2/2	! insure within dimen
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
	   WRITE (IOERR,*) '^ This may cause Latitude read failure.'
	   N4 = NEW
	ENDIF
	IF (N24.LT.2  .OR. N24.GT.MAXNH) THEN
	   NEW = 24		! insure within dimen.
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
	GOTO 9

 417	WRITE(IOERR,*)'TCARD: Error reading internal buffer for ',WHAT
	 IRET=5
	GOTO 9
 418	WRITE(IOERR,*)'TCARD: EOF reading internal buffer for ',WHAT
	 IRET=5
	GOTO 9
C
C no more input data
C
 435	j=j+1
 434	j=j+1
 433	j=j+1
 432	j=j+1
 431	j=j+1
	WRITE (IOSP,*),'TCARD: IO error: at ',430+j
	N4=MAX(N4,1)
	CALL TPRINT(2)
	STOP

430	IRET=5
	WRITE (IOSP,'(//5X,A)') 'END OF DATA ON INPUT UNIT'
 9	CONTINUE	! only exit from this routine
D	WRITE(*,*)'TCARD Exit with IRET=',IRET !<<<dbug
D	WRITE(IOSP,*)'TCARD Exit with IRET=',IRET !<<<dbug
	RETURN		
	END
