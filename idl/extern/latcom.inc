C_Titl  LATCOM.INC   common for latitude-dependant items in  KRC
	PARAMETER (NWLAT= (9+ 3*MAXN1 + 2*MAXNH) *MAXN4) ! size of common in 4-byte words
C_arg: all are set in  TLATS
      COMMON /LATCOM/
     & NDJ4(MAXN4)	 ! # days to compute solution for each latitude
     &,DTM4(MAXN4)	 ! rms temperature change on last day
     &,TST4(MAXN4)	 ! predicted equilibrium temperature of ground
     &,TTS4(MAXN4)	 ! predicted mean surface temperature for each latitude
     &,TTB4(MAXN4)	 ! predicted mean bottom temperature
     &,FROST4(MAXN4)	 ! predicted frost amount kg/m^2.
     &,AFRO4(MAXN4)	 ! frost albedo.  
     &,TTA4(MAXN4)	 ! predicted final atmosphere temperature  
     &,TTX4(MAXN4)	 ! spare
     &,TMN4(MAXN1,MAXN4) ! predicted convergence midnight temperature
     &,TIN(MAXN1,MAXN4)	 ! minimum hourly layer temperature
     &,TAX(MAXN1,MAXN4)	 ! maximum hourly layer temperature
     &,TSF(MAXNH,MAXN4)  ! final hourly surface temperature
     &,TPF(MAXNH,MAXN4)  ! final hourly planetary temperature
C_Desc
C_Hist  85may12  Hugh_H_Kieffer    97feb11  HHK add  ELEV
C  97sep08  HHK add  TPF
C 2002mar09 HHK  Move ALAT & ELEV from here to KRCCOM
C 2002jul13 HK Add TTA4 and TTX4
C 2002aug15 HK NWLAT is computed, = 2793
C 2004jul07 HK Move dimension-defining parameters from other commons into KRCCOM
C 2009feb   HK With MAXN1=30, MAXNH=48, MAXN4=37, NWLAT is 7215
C_End _______________________________________________________________________

