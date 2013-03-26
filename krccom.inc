C_Titl  KRCCOM.INC  common for input and transfer variables
C_Limitations
	IMPLICIT REAL*4 (A-H,O-Z), INTEGER*4 (I,J,K,M,N), LOGICAL*4 (L) ! std.,+L
	PARAMETER (NUMFD=80, NUMID=40, NUMLD=20)! number of each type
	PARAMETER (MACN4 =19)	! dimension of latitudes
	PARAMETER (NWKRC=NUMFD+NUMID+NUMLD+25+2*MACN4) 
				! above is size of common in 32-bit words
C_Description.  
C  Cset -> routine which sets value.  seas=TSEAS  lat=TLAT
C *= initially set as input, this routine may reset value.
C lines 1-4 = real: input via  TCARD
C	5-6 =   " : transfer
C	7-8 = integer: input via  TCARD
C	9-A =    "   : transfer
C	B-C = logical: input via  TCARD
C	D   = title (input via tcard) and run_time (transfer)

	COMMON /KRCCOM/   ! grouped by 5 and 10
     1 ALB,EMIS,SKRC,COND2,DENS2, PERIOD,SPHT,DENS,CABR,ABRAMP       ! 1:10
     2,ABRPHA,PTOTAL,FANON,TATM,TDEEP, SPHT2,TAUD,DUSTA,TAURAT,TWILI ! 11:20
     3,ARC2,ARC3,SLOPE,SLOAZI,TFROST, CFROST,AFROST,FEMIS,AF1,AF2    !   :30
     4,FROEXT,FD32,RLAY,FLAY,CONVF, DEPTH,DRSET,DDT,GGT,FDOWN        !   :40
Cset					  *day1
     4,DJUL,DELJUL,SDEC,DAU,SUBS, SOLCON,GRAV,ATMCP ! total of 48 input  : 48
     &,FD49(6),TATMIN,PRES,OPACITY,TAUIR,TAUEFF,TATMJ                !  :60
Cset   *seas       ----seas-------
     5,SKYFAC,TFNOW,AFNOW,PZREF,SUMF, TEQUIL,TBLOW, HOURO,SCALEH,BETA !  :70
Cset   -----------lats--------- tint ---lats-------      ---lats----
     6,DJU5,DAM,EFROST,DLAT,COND,  DIFFU,SCALE,PI,SIGMA,RAD	!71:80
Cset   seas lat day2   lats -------day1------- --main------
     7,N1,N2,N3,N4,N5,  N24,IB,IC,NRSET,NMHA                      !  1:10
     8,NRUN,JDISK,IDOWN,I14,I15,  KPREF,K4OUT,JBARE,NMOD,IDISK2   ! 11:20
     9,KOLD,ID22(6),  NFD,NID,NLD                                 ! 21:30
Cset             ----card---
     A,N1M1,NLW,JJO,KKK,N1PIB,  NCASE,J2,J3,J4,     J5		  ! 31:40
Cset   ---day1- lat ----day1-    main -day2- lats  seas
     B,LP1,LP2,LP3,LP4,LP5,    LP6,LPGLOB,LVFA,LVFT,L10           !  1:10
     C,LPORB,LKEY,LSC,LNOTIF,LOCAL,   LD16,LD17,LD18,LD19,LONE	  ! 11:20
     D,TITLE(20),DAYTIM(5)   ! these two mkae up 100 bytes
Cset    tcard    tprint
     &,ALAT(MACN4)	 ! latitude in degrees. set in  TCARD
     &,ELEV(MACN4)	 ! elevation in km. set in  TCARD
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
C_End _______________________________________________________________________
