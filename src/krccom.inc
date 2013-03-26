C_Titl  KRCCOM.INC  common for input and transfer variables
C_Limitations
	IMPLICIT REAL*4 (A-H,O-Z), INTEGER*4 (I,J,K,M,N), LOGICAL*4 (L) !std.,+L
C Here are all the dimension-defining parameters for items in any common
	PARAMETER (MAXN1 =30)	   ! dimension of layers
	PARAMETER (MAXN2 =384*4)   ! dimension of times of day: 384=24*16
	PARAMETER (MAXN3 =16)	   ! dimension of iteration days
	PARAMETER (MAXN4 =37)	   ! dimension of latitudes
	PARAMETER (MAXN5 =161)	   ! dimension of seasons
	PARAMETER (MAXN6 =6)	   ! dimension of saved years
	PARAMETER (MAXNH =48)	   ! dimension of saved times of day
	PARAMETER (MAXBOT=6)	   ! dimension of time divisions
	PARAMETER (MAXN1P=MAXN1+1) ! dimension layer temperature points
	PARAMETER (NUMFD=56+32, NUMID=40, NUMLD=20)! number of each type
	PARAMETER (NWKRC=NUMFD+NUMID+NUMLD+25+2*MAXN4) 
				! above is size of common in 32-bit words
C_Description.  
C  Cset -> routine which sets value.  seas=TSEAS  lat=TLAT
C *= initially set as input, this routine may reset value.
C lines 1-5 = real: input via  TCARD
C	  6 =   " : transfer
C	7-8 = integer: input via  TCARD
C	9-A =    "   : transfer
C	B-C = logical: input via  TCARD
C	D   = title (input via tcard) and run_time (transfer)

	COMMON /KRCCOM/
     1 ALB,EMIS,SKRC,COND2,DENS2, PERIOD,SPHT,DENS,CABR,AMW          ! 1:10
     2,ABRPHA,PTOTAL,FANON,TATM,TDEEP, SPHT2,TAUD,DUSTA,TAURAT,TWILI ! 11:20
     3,ARC2,ARC3,SLOPE,SLOAZI,TFROST, CFROST,AFROST,FEMIS,AF1,AF2    !   :30
     4,FROEXT,FD32,RLAY,FLAY,CONVF, DEPTH,DRSET,DDT,GGT,DTMAX        !   :40
Cset					  *day1
     5,DJUL,DELJUL,SDEC,DAU,SUBS, SOLCON,GRAV,ATMCP,CONUP0,CONUP1    !   :50
Cset					         v total of 56 input
     5,CONUP2,CONUP3,CONLO0,CONLO1,CONLO2, CONLO3,HUGE,TINY          !   :58
     &,EXPMIN,FD60(2),RGAS,TATMIN,PRES,OPACITY,TAUIR,TAUEFF,TATMJ    !   :68
Cset   *seas       ----seas-------
     6,SKYFAC,TFNOW,AFNOW,PZREF,SUMF, TEQUIL,TBLOW, HOURO,SCALEH,BETA !  :78
Cset   -----------lats--------- tint ---lats-------      ---lats----
     6,DJU5,DAM,EFROST,DLAT,COND,  DIFFU,SCALE,PI,SIGSB,RAD	      !79:88
Cset   seas lat day2   lats -------day1------- --main------
     7,N1,N2,N3,N4,N5,  N24,IB,IC,NRSET,NMHA                      !  1:10
     8,NRUN,JDISK,IDOWN,I14,I15,  KPREF,K4OUT,JBARE,NMOD,IDISK2   ! 11:20
     9,KOLD,KVALB,KVTAU,ID24(4),  NFD,NID,NLD                     ! 21:30
Cset             ----card---
     A,N1M1,NLW,JJO,KKK,N1PIB,  NCASE,J2,J3,J4,     J5		  ! 31:40
Cset   ---day1- lat ----day1-    main -day2- lats  seas
     B,LP1,LP2,LP3,LP4,LP5,    LP6,LPGLOB,LVFA,LVFT,LKOFT         !  1:10
     C,LPORB,LKEY,LSC,LNOTIF,LOCAL,   LD16,LD17,LD18,LD19,LONE	  ! 11:20
     D,TITLE(20),DAYTIM(5)   ! these two make up 100 bytes
Cset    tcard    tprint
     &,ALAT(MAXN4)	 ! latitude in degrees. set in  TCARD
     &,ELEV(MAXN4)	 ! elevation in km. set in  TCARD
C
	REAL*4    FD(NUMFD)
        INTEGER*4 ID(NUMID)
        LOGICAL*4   LD(NUMLD)
	EQUIVALENCE (FD(1),ALB), (ID(1),N1), (LD(1),LP1)

C_Hist	85may12  Hugh_H_Kieffer  USGS_Flagstaff major revision
C	86jul01 HHK add comments of where variable is set
C  97feb11  HHK revise many locations and sizes
C  97sep08  HHK add  SCALEH,BETA   97sep11 replace  DIMENSION
C  98MAY26  HHK remove  L from implicit integer
C  00jan23  HHK redefine  BK->COND2  BRC->DENS2  FD16->SPHT2
C 2002mar01 HHK Make implict L logical*4
C 2002mar07 HHK LD20-->LONE.  move ALAT & ELEV from LATCOM, make id(21)=kold
C 2002jul16 HK Add OPACITY to common
C 2004jul07 HK Move dimension-defining parameters from other commons into KRCCOM
C 2008oct02-15 HK Move MAXBOT from DAYCOM to here. Replace ID22 with KVALB & 
C    KVTAU and redefine ABRAMP to be AMW. New maximum dimensions.
C 2008nov13 HK Add T-dependent conductivity parameters.
C_End _______________________________________________________________________
  
