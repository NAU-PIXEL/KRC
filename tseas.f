	SUBROUTINE TSEAS (IQ,IR)
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'units.inc'
	COMMON /PORBCM/ PCOM1(22),SLP,P24,PHOXX(3),PCOM2(33)
C       SA - Sep 3, 03 - replaced above line
C	COMMON /PORBCM/ PCOM1(22),SLP,P24,PHOXX(3),PCOM2(43)
C the above for  PHOXX rather than  INCLUDE '/home/hkieffer/krc/porb/porbcm.inc'
C  because of conflicts with krccom.inc for  PI and  PERIOD
C_Args
	INTEGER IQ ! in. 1 = start from scratch
C			 2 = restart  from disk
C			 3 = continue from current conditions
	INTEGER IR		! return from lower routine
C_Hist 97feb11  Hugh_Kieffer  USGS_Flagstaff
C 97aug22  HHK calculated julian day by multiplication, not addition
C 2002nov01 HK Have DJU5 increment by current DELJUL for each season

C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C
	INTEGER TIME	! system function, time in seconds since 1970

	IF (IQ.EQ.1) THEN 	! if start of a model
	  J5=0			! initialize season counter
	  ITIM0=TIME()		! save starting time
	  DJU5=DJUL-DELJUL	! set date to -1 season
C if IQ =3, then continuing form existing model, and adopt its last date
	ENDIF
	NMOD = MAX(NMOD,1)	! insure positive modulo
C  ----------------------------new season loop loop loop------------
C
100	J5=J5+1
	IF (LSC) THEN		!  LSC .true. invokes changes at each season
	  CALL TCARD (2,IR)	! read parameter change cards
	  IF (IR.NE.1) RETURN
	  CALL TDAY (1,IR)	! re-initialize day computations
	  IF (IR.NE.1) RETURN
	ENDIF
	DJU5=DJU5+DELJUL	! increment julian date
	IF (LNOTIF.AND.(MOD(J5,NMOD).EQ.0)) THEN	! notify terminal
	  ITIM = TIME ()
	  ITIM=ITIM-ITIM0
	  WRITE(IOPM,115) ITIM,'start',J5,N5
 115	  FORMAT(1X,I5,' seconds at ',A,' of season ',I4,' of ',I4)
	ENDIF
	IF (N5.GT.0) THEN	                ! get orbital parameters 
	  CALL PORB (DJU5,DAU,PEA,PEB,HFA,HFB)  ! FIND CURRENT PLANET POSITION
	  SDEC = HFA*RAD	        ! CONVERT SOLAR DECLINATION TO DEGREES
	  TANOM = ATAN2 (PHOXX(2),PHOXX(1))     ! TRUE ANOMOLY
	  SUBS = MOD(((TANOM+SLP)*RAD)+360.,360.)	! L-SUB-S IN DEGREES
CC      SUBS=ANG360 ((TANOM+SLP)*RAD)

	ENDIF
C
C======

	CALL TLATS (IQ,IRL)		! execute latitude loop

C======
	IF (LP5) CALL TPRINT (5)	! print latitude summary
	IF (LP6) CALL TPRINT (6)	!  & min and max layer temperature
C
C save season result by appending to disk file
C write  KRCCOM record only if first season and short file form
C
C	write (*,*)'j5,jdisk,kold,k4out=',j5,jdisk,kold,k4out
	IF (J5.EQ.JDISK .AND. KOLD.EQ.0 ! write  KRCCOM record
     &        .AND. K4OUT.LT.0) CALL TDISK (5,KREC) ! must follow tday(1)
	IF (J5.GE.JDISK .AND. .NOT. LONE) CALL TDISK (2,KREC) ! write this season
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
	RETURN
	END
