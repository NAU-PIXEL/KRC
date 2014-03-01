C_Titl  DAYCOM.INC   common for layer and time items in  KRC
	PARAMETER (NWDAY = 6*MAXN1+1  + (5+MAXN1)*MAXN3 + 3*MAXN2 
	1    + 2*MAXNH + MAXBOT) ! size of this common in 4-byte words
C_Desc
C  ASOL &  ADGR are set in  TLATS, the rest are set in  TDAY
      COMMON /DAYCOM/ XCEN(MAXN1)	! Depth at layer centers [m]
     &, SCONVG(MAXN1)	! Classical convergence factor for each layer
     &, BLAY(MAXN1)	! Layer thicknesses [m]
     &, TMIN(MAXN1)	! Minimum layer temperatures of day on the hour
     &, TMAX(MAXN1)	! Maximum layer temperatures of day on the hour
     &, TTJ(MAXN1P)	! Layer temperatures (TTJ(1) is surface temperature)    
     &, TT1(MAXN1,MAXN3)! Temperatures at start of day for each layer and day 
     &, TTS(MAXN3)	! Mean daily surface temperatures                     
     &, TTB(MAXN3)	! Mean daily bottom temperatures                      
     &, TTA(MAXN3)	! End-of-Day Atmospheric temperatures
     &, DTMJ(MAXN3)	! RMS daily temperature change
     &, FRO(MAXN3)	! Daily frost amounts. [kg/m^2]                    
     &, ASOL(MAXN2)	! Insolation at each time of day, direct + diffuse
     &, ADGR(MAXN2)	! Atm. solar heating at each time of day 
     &, TOUT(MAXN2)	! Surface temperatures of solution at each time of day
     &, TSFH(MAXNH)	! Hourly surface temperatures at solution
     &, TPFH(MAXNH)	! Hourly planetary temperatures at solution
     &, N1K(MAXBOT)	! Binary time division layers
C_Hist	84jun15  Hugh_H_Kieffer  97feb11  HHK add  ADGR  
C    97mar03 correct  NWDAY       97sep08  HHK add  TPFH
C 2002jul12 HK Add TTA, ADGR was down-going atmospheric  IR radiance
C 2002aug15 HK NWDAY is computed, =1951
c 2002oct30 HK set N2 to 8*384 as trial  NWday=10015
C 2004jul07 HK Move dimension-defining parameters from other commons into KRCCOM
C 2008oct02 HK Move MAXBOT and MAXN1P from here to KRCCOM
C 2008nov13 HK Change names: X->XCEN  T->TTJ  TT->TT1  TLAY->BLAY
C_End __________________________________________________________________________
  
