	SUBROUTINE TSEAS (IQ,IR)
C_Titl TSEAS Advance one "season" along planets orbit for KRC system
C_Vars
	INCLUDE 'krccom.inc'	! has IMPLICIT NONE
	INCLUDE 'latcom.inc'
	INCLUDE 'hatcom.inc'
	INCLUDE 'units.inc'
C_Args
	INTEGER IQ ! in. 1 = start from scratch
C		         2 = restart from disk
C		         3 = continue from current conditions
	INTEGER IR		! return code from lower routine
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
C 2012mar02 HK Add atmosphere flag, remove unused labels
C 2012mar27  HK	 Test on KVTAU changed from .GE.0 to EQ.1
C 2014jan24  HK  Compute date with multiply rather than repeated adds
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C 
	INTEGER I,J,IRL,ITIM0,ITIM,KREC
	REAL*4 TANOM ! related to PORBIT

	INTEGER TIME	! system function, time in seconds since 1970
	REAL SEASALB,SEASTAU	! functions called
	REAL BUF(MAXN4)	! fractional surface area in each latitude zone
	REAL*4 QLOST
	DATA BUF(1) /0./
	LOGICAL LATM

	LATM=PTOTAL.GT.1.		! atmosphere present flag
 	IF (IDB1.NE.0) WRITE(IOSP,*)'MSEASa',IQ,IR,J5,LSC,N5,LONE

	IF (IQ .EQ. 1) THEN 	! if start of a model
C	   IF (IQ .EQ. 1) then ! ryped
	  J5=0			! initialize season counter
	  ITIM0=TIME()		! save starting time 
	  SUMF=0.		! global seasons frost
	ENDIF
	BUF(1)=0.	! flag for  TINT to compute areas

C  ----------------------------new season loop loop loop------------
C
100	DJU5=DJUl+FLOAT(J5)*DELJUL	! current julian date
	J5=J5+1			! increment season index
	IF (LSC .OR. J5.EQ.IDOWN) THEN !  LSC  invokes changes at each season
	  CALL TCARD (2,IR)	! read parameter change cards
	  IF (IR.GE.4) RETURN	! either 1-point or end-of-data
D         write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,IR !<< ,KVALB,alb
	  CALL TDAY (1,IR)	! re-initialize day computations
	  IF (IR.NE.1) RETURN
	ENDIF
C	DJU5=DJU5+DELJUL	! increment julian date
	IF (MOD(J5,NMOD).EQ.0) THEN	! notify terminal
	  ITIM = TIME ()
	  ITIM=ITIM-ITIM0
	  WRITE(IOPM,115) ITIM,'start',J5,N5
 115	  FORMAT(1X,I5,' seconds at ',A,' of season ',I4,' of ',I4)
	ENDIF
	IF (LPORB) CALL PORBIT (1, DJU5,SUBS,SDEC,DAU)  ! Get current Ls, AU and sub-solar lat
C
	IF (KVALB .GT. 0) ALB=SEASALB(SUBS) ! variable soil albedo
	IF (KVTAU .EQ. 1) TAUD=SEASTAU(SUBS) ! variable atm. opacity
D	write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,SUBS ! ,KVALB,alb
C======

	CALL TLATS (IQ,IRL)		! execute latitude loop

C======
C If there was a blowup, IRL will be 2  and will exit the season loop 

	IF (N4.GT.8 .AND. LATM) CALL TINT (FROST4, BUF, SUMF) !  MKS units  BUF = normalized area 
	IF (LP5) CALL TPRINT (5)	! print latitude summary
	IF (LP6) CALL TPRINT (6)	!  & min and max layer temperature
C
C	write (*,*)'j5,jdisk,kold,k4out=',j5,jdisk,kold,k4out
C	write(*,*)'s1', subs
	IF (J5.EQ.JDISK .AND. KOLD.EQ.0 ! write  KRCCOM record
     &        .AND. K4OUT.LT.0) CALL TDISK (5,KREC) ! must follow tday(1)
	IF (J5.GE.JDISK .AND. JDISK .GT.0) CALL TDISK(2,KREC) ! write this season
C
C Check if more seasons and no blowup
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
