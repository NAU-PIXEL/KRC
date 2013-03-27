	SUBROUTINE TSEAS (IQ,IR)
C_Titl TSEAS Advance one "season" along planets orbit for KRC system
C_Vars
	INCLUDE 'krccom.inc'	! has IMPLICIT NONE
	INCLUDE 'latcom.inc'
	INCLUDE 'hatcom.inc'
	INCLUDE 'units.inc'
        REAL  PCOM1(22),SLP,P24,PHOXX(3),PCOM2(33)
	COMMON /PORBCM/ PCOM1,SLP,P24,PHOXX,PCOM2 ! only SLP, PHOXX used

C_Args
	INTEGER IQ ! in. 1 = start from scratch
C		           2 = restart  from disk
C			   3 = continue from current conditions
	INTEGER IR		! return from lower routine
C_Desc
C if IQ =3, then continuing from existing model, and adopt its last date
C_Hist 97feb11  Hugh_Kieffer  USGS_Flagstaff
C 97aug22  HHK calculated julian day by multiplication, not addition
C 2002nov01 HK Have DJU5 increment by current DELJUL for each season
C 2008oct02 HK Replace ID22(1) and (2) with KVALB and KVTAU
C 2008oct22 HK Used IDOWN as season to make changes
C 2009mar05 HK Minor cleanup
C 2009apr22 HK Add call to TYEAR
C 2010jan12 HK Use IMPLICIT NONE, move TINT call from TLATS to TSEAS
C 2011aug07 HK Change test to call PORB from N5.GT.0	
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C 
	INTEGER IRL,ITIM0,ITIM,KREC
	REAL*4 PEA,PEB,HFA,HFB,TANOM ! related to PORB

	INTEGER TIME	! system function, time in seconds since 1970
	REAL SEASALB,SEASTAU	! functions called
	REAL BUF(MAXN4)	! fractional surface area in each latitude zone
	DATA BUF(1) /0./

 991	IF (IDB1.NE.0) WRITE(IOSP,*)'MSEASa',IQ,IR,J5,LSC,N5,LONE

	IF (IQ .EQ. 1) THEN 	! if start of a model
C	   IF (IQ .EQ. 1) then ! ryped
	  J5=0			! initialize season counter
	  ITIM0=TIME()		! save starting time 
	  DJU5=DJUL-DELJUL      ! set date to -1 season
	  SUMF=0.		! global seasons frost
	ENDIF
	BUF(1)=0.	! flag for  TINT to compute areas

C  ----------------------------new season loop loop loop------------
C
100	J5=J5+1
	IF (LSC .OR. J5.EQ.IDOWN) THEN !  LSC  invokes changes at each season
	  CALL TCARD (2,IR)	! read parameter change cards
	  IF (IR.GE.4) RETURN	! either 1-point or end-of-data
D         write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,IR !<< ,KVALB,alb
	  CALL TDAY (1,IR)	! re-initialize day computations
	  IF (IR.NE.1) RETURN
	ENDIF
	DJU5=DJU5+DELJUL	! increment julian date
	IF (MOD(J5,NMOD).EQ.0) THEN	! notify terminal
	  ITIM = TIME ()
	  ITIM=ITIM-ITIM0
	  WRITE(IOPM,115) ITIM,'start',J5,N5
 115	  FORMAT(1X,I5,' seconds at ',A,' of season ',I4,' of ',I4)
	ENDIF
	IF (LPORB) THEN	                ! get orbital parameters 
	  CALL PORB (DJU5,DAU,PEA,PEB,HFA,HFB)  ! FIND CURRENT PLANET POSITION
	  SDEC = HFA*RADC	        ! CONVERT SOLAR DECLINATION TO DEGREES
	  TANOM = ATAN2 (PHOXX(2),PHOXX(1))     ! TRUE ANOMOLY
	  SUBS = MOD(((TANOM+SLP)*RADC)+360.,360.)	! L-SUB-S IN DEGREES
CC      SUBS=ANG360 ((TANOM+SLP)*RADC)
	ENDIF
C
	IF (KVALB .GT. 0) ALB=SEASALB(SUBS) ! variable soil albedo
	IF (KVTAU .GT. 0) TAUD=SEASTAU(SUBS) ! variable atm. opacity
D	write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,SUBS ! ,KVALB,alb
C======

	CALL TLATS (IQ,IRL)		! execute latitude loop

C======

	IF (N4.GT.8) CALL TINT (FROST4, BUF, SUMF) !  MKS units  BUF = normalized area 
	IF (LP5) CALL TPRINT (5)	! print latitude summary
	IF (LP6) CALL TPRINT (6)	!  & min and max layer temperature

C	CALL TYEARP (1,IRY) ! store midnight T for all layers and latitudes
C	IF (IRY.LT.0) WRITE(IOERR,*)'TYEARP error return=',IRY

C save season result by appending to disk file
C write  KRCCOM record only if first season and short file form
C
C	write (*,*)'j5,jdisk,kold,k4out=',j5,jdisk,kold,k4out
C	write(*,*)'s1', subs
	IF (J5.EQ.JDISK .AND. KOLD.EQ.0 ! write  KRCCOM record
     &        .AND. K4OUT.LT.0) CALL TDISK (5,KREC) ! must follow tday(1)
	IF (J5.GE.JDISK .AND. JDISK .GT.0) CALL TDISK(2,KREC) ! write this season
C
C  check if more seasons
	IF (J5.LT.N5 .AND. IRL.NE.2) GO TO 100
C  ---------------------------------------------------------------------
	IF (.NOT.LONE) THEN
	  ITIM = TIME ()
	  ITIM=ITIM-ITIM0
	  WRITE(IOPM,115) ITIM,'end',J5,N5
	  WRITE(IOSP,115) ITIM,'end',J5,N5
	ENDIF
	IR=IRL
C	write(*,*)'s2', subs
	RETURN
	END
