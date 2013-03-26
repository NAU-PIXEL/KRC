C_Titl  LATCOM.INC   common for latitude-dependant items in  KRC
	PARAMETER (MAXN4 =19)	! dimension of latitudes. Must match  KRCCOM
	PARAMETER (MACNH=24)	! hour dimension,  must match  DAYCOM
	PARAMETER (MACN1 =30)	! layer dimension, must match  DAYCOM
	PARAMETER (NWLAT= (9+ 3*MACN1 +2*MACNH) *MAXN4) ! size of common in 4-byte words
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
     &,T4(MACN1,MAXN4)	 ! predicted convergence midnight temperature
     &,TIN(MACN1,MAXN4)	 ! minimum hourly layer temperature
     &,TAX(MACN1,MAXN4)	 ! maximum hourly layer temperature
     &,TSF(MACNH,MAXN4)  ! final hourly surface temperature
     &,TPF(MACNH,MAXN4)  ! final hourly planetary temperature
C_Desc
C_Hist  85may12  Hugh_H_Kieffer    97feb11  HHK add  ELEV
C  97sep08  HHK add  TPF
C 2002mar09 HHK  Move ALAT & ELEV from here to KRCCOM
C 2002jul13 HK Add TTA4 and TTX4
C 2002aug15 NWLAT is computed, = 2793
C_End _______________________________________________________________________

