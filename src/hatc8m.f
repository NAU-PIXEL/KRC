C_Titl  hatc8m.f   HATCOM: common to store post-2003 items in  KRC
C     NWHAT= (3+6*MAXNH)*MAXN4+2*MAXN2+2*MAXFF+2  ! size of this common
      INTEGER MAXFF
      PARAMETER (MAXFF=384*4*4) ! dimension of far-field times of day
      REAL*8 HEATMM(MAXN4)   ! daily average surface heat flow,  <W m^-2>
     &, TEXTRA(MAXN4,2)      ! Extrapolation in surface/bottom temperature
     &, TAF(MAXNH,MAXN4)     ! final hourly atmosphere temperature, not predicted
     &, TOFALB(MAXNH,MAXN4)  ! hourly top-of-atm albedo, not predicted
     &, DOWNVIS(MAXNH,MAXN4) ! hourly net downward solar flux
     &, DOWNIR(MAXNH,MAXN4)  ! hourly net downward thermal flux
     &, ALBJ(MAXN2)          ! hemispherical albedo at each time of day
     &, SOLDIF(MAXN2) ! Solar diffuse (with bounce) insolation at each time W/m^2 
     &, HEAT1M               ! Mean upward heat flow into surface on last day
     &, SALB                 ! spherical albedo of the soil
      REAL*8 FARTS(MAXNH,MAXN4,2) ! far-field Tsurf/Tatm for current season
     &, FARAD(MAXFF) ! far-field radiance for every time-step at current latitude
     &, HARTA(MAXFF) ! flat-case Tatm  for every time-step at current latitude
C     &, TMN4Y(MAXN6,MAXN1,MAXN4)! midnight temperatures (year,layer,lat.)

      COMMON /HATCOM/HEATMM,TEXTRA,TAF,TOFALB,DOWNVIS,DOWNIR
     &, ALBJ,SOLDIF,FARTS,FARAD,HARTA, HEAT1M,SALB

C_Notes
C 2016jul07 ALBJ and SOLDIF should move to daycom, SALB should move to krccom.

C_Desc Designed for seasonal heatflow into annual frost
C_Hist 2004jul05 Hugh Kieffer
C 2004Oct05 HK Add DOWNVIS and DOWNIR
C 2009apr22 HK Add TMN4Y
C 2010jan12 HK Change to IMPLICIT NONE assumed in krccom
C 2014feb25 HK Specify all word lengths as *4
C 2014mar10 HK Make  REAL*8  version      Change name from hatcom.inc 
C 2016may08 HK Add items for flat far field,  move HEAT1M  -from first 
C 2016jul07 HK Include  ALBJ and SOLDIF to handle non-Lambertian albedo
C_End __________________________________________________________________________

