      SUBROUTINE TSEAS8 (IQ,IR)
C_Titl TSEAS Advance one "season" along planets orbit for KRC system
C_Vars
      INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
      INCLUDE 'latc8m.f'
      INCLUDE 'hatc8m.f'
      INCLUDE 'units.inc'
C_Args
      INTEGER*4 IQ              ! in. 1 = start from scratch
C                        2 = restart from disk
C                        3 = continue from current conditions
      INTEGER*4 IR              ! return code from lower routine
C_Desc
C if IQ =3, then continuing from existing model, and adopt its last date
C_Hist 97feb11  Hugh_Kieffer  USGS_Flagstaff
C
C_Calls  PORBIT  SEASALB  SEASTAU  TCARD8  TDAY8  TDISK8  TINT8  TLATS8  TPRINT8 
C
C 97aug22  HHK calculated julian day by multiplication, not addition
C 2002nov01 HK Have DJU5 increment by current DELJUL for each season
C 2008oct02 HK Replace ID22(1) and (2) with KVALB and KVTAU
C 2008oct22 HK Used IDOWN as season to make changes
C 2009mar05 HK Minor cleanup
C 2009apr22 HK Add call to TYEAR
C 2010jan12 HK Use IMPLICIT NONE, move TINT call from TLATS to TSEAS
C 2011aug07 HK Change test to call PORB from N5.GT.0
C 2012mar02 HK Add atmosphere flag, remove unused labels
C 2012mar27 HK Test on KVTAU changed from .GE.0 to EQ.1
C 2014jan24 HK Compute date with multiply rather than repeated adds
C 2014feb05 HK LKEY true will compute date of first season 
C 2014feb25 HK Specify most variables as *4  Untabify and justify
C 2014mar10 HK Make  REAL*8  version  
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C 
      INTEGER*4 IRL,KREC
      REAL*8 DJONE              ! first date
      REAL*8 BUF(MAXN4)         ! fractional surface area in each latitude zone

      REAL SEASALB,SEASTAU      ! functions called
      REAL*4 ALB4,SUBS4,TAUD4  ! for *4*8 conversion 
      REAL*4 TIME1,TIME2,TIME3 
      DATA BUF(1) /0./
      LOGICAL LATM

      LATM=PTOTAL.GT.1.         ! atmosphere present flag
      IF (IDB1.NE.0) WRITE(IOSP,*)'MSEASa',IQ,IR,J5,LSC,N5,LONE

      IF (IQ .EQ. 1) THEN       ! if start of a model
        J5=0                    ! initialize season counter
C System_Clock is millisec or finer
C DTIME is real seconds
C CPU_TIME is real with resolution microseconds
        CALL CPU_TIME(TIME1)
        SUMF=0.                 ! global seasons frost
      ENDIF
      BUF(1)=0.                 ! flag for  TINT to compute areas

      IF (LKEY) THEN
        CALL PORBIT(2,DJU5,DJUL,SDEC,DAU) ! DJU5 will be the MJD for Ls=DJUL
        DJONE=DJU5-(JDISK-1)*DELJUL ! starting date to reach Ls at Jdisk
      ELSE
        DJONE=DJUL              ! starting date as input
      ENDIF

C  ----------------------------new season loop loop loop------------
C
 100  DJU5=DJONE+FLOAT(J5)*DELJUL ! current julian date
      J5=J5+1                   ! increment season index
      IF (LSC .OR. J5.EQ.IDOWN) THEN !  LSC  invokes changes at each season
        CALL TCARD8 (2,IR)       ! read parameter change cards
        IF (IR.GE.4) RETURN     ! either 1-point or end-of-data
D     write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,IR !<< ,KVALB,alb
        CALL TDAY8 (1,IR)        ! re-initialize day computations
        IF (IR.NE.1) RETURN
      ENDIF
      IF (MOD(J5,NMOD).EQ.0) THEN ! notify terminal
        CALL CPU_TIME(TIME2)
        TIME3=TIME2-TIME1
        WRITE(IOPM,115) TIME3,'start',J5,N5
 115    FORMAT(1X,F9.3,' seconds at ',A,' of season ',I4,' of ',I4)
      ENDIF
      IF (LPORB) CALL PORBIT (1, DJU5,SUBS,SDEC,DAU) ! Get current Ls, AU and sub-solar lat
C
      SUBS4=SUBS                ! get R*4 version
      IF (KVALB .GT. 0) THEN
        ALB4=SEASALB(SUBS4)     ! variable soil albedo
        ALB=ALB4                ! move into R*8
      ENDIF
      IF (KVTAU .EQ. 1) THEN 
        TAUD4=SEASTAU(SUBS4)    ! variable atm. opacity
        TAUD=TAUD4
      ENDIF
D       write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,SUBS ! ,KVALB,alb
C======

      CALL TLATS8 (IQ,IRL)       ! execute latitude loop

C======
C If there was a blowup, IRL will be 2  and will exit the season loop 

      IF (N4.GT.8 .AND. LATM) CALL TINT8 (FROST4, BUF, SUMF) !  MKS units  BUF = normalized area 
      IF (LP5) CALL TPRINT8 (5)  ! print latitude summary
      IF (LP6) CALL TPRINT8 (6)  !  & min and max layer temperature
C
C       write (*,*)'j5,jdisk,kold,k4out=',j5,jdisk,kold,k4out
C       write(*,*)'s1', subs
      IF (J5.EQ.JDISK .AND. KOLD.EQ.0 ! write  KRCCOM record
     &     .AND. K4OUT.LT.0) CALL TDISK8 (5,KREC) ! must follow tday(1)
      IF (J5.GE.JDISK .AND. JDISK .GT.0) CALL TDISK8 (2,KREC) ! write this season
C
C Check if more seasons and no blowup
      IF (J5.LT.N5 .AND. IRL.NE.2) GO TO 100
C  ---------------------------------------------------------------------
      IF (.NOT.LONE) THEN
        CALL CPU_TIME(TIME2)
        TIME3=TIME2-TIME1
        WRITE(IOPM,115) TIME3,'end',J5,N5
        WRITE(IOSP,115) TIME3,'end',J5,N5
      ENDIF
      IR=IRL
C     write(*,*)'s2', subs
      RETURN
      END
