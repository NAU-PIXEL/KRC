C_Titl  hatcom8.f   common to store post-2003 items in  KRC
C     NWHAT= 1+(3+4*MAXNH+MAXN6*MAXN1)*MAXN4 ! size of this common in real words
      REAL*8 HEAT1M         ! Mean upward heat flow into surface on last day
     &, HEATMM(MAXN4)       !  " " for all latitudes <W m^-2>
     &, TEXTRA(MAXN4,2)     ! Extrapolation in surface/bottom temperature
     &, TAF(MAXNH,MAXN4)    ! final hourly atmosphere temperature, not predicted
     &, TOFALB(MAXNH,MAXN4)     ! hourly top-of-atm albedo, not predicted
     &, DOWNVIS(MAXNH,MAXN4)    ! hourly net downward solar flux
     &, DOWNIR(MAXNH,MAXN4)     ! hourly net downward thermal flux
     &, TMN4Y(MAXN6,MAXN1,MAXN4)! midnight temperatures (year,layer,lat.)

      COMMON /HATCOM/HEAT1M,HEATMM,TEXTRA,TAF,TOFALB,DOWNVIS,DOWNIR
     &, TMN4Y

C_Desc Designed for seasonal heatflow into annual frost
C_Hist 2004jul05 Hugh Kieffer
C 2004Oct05 HK Add DOWNVIS and DOWNIR
C 2009apr22 HK Add TMN4Y
C 2010jan12 HK Change to IMPLICIT NONE assumed in krccom
C 2014feb25 HK Specify all word lengths as *4
C 2014mar10 HK Make  REAL*8  version      Change name from hatcom.inc 
C_End __________________________________________________________________________

