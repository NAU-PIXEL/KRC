C_Titl  HATCOM.INC   common to store post 2003 items in  KRC
      COMMON /HATCOM/ HEAT1M	! Mean upward heat flow into surface on last day
     &, HEATMM(MAXN4)		!  " " for all latitudes <W m^-2>
     &, TEXTRA(MAXN4,2) 	! Extrapolation in surface/bottom temperature
     &, TAF(MAXNH,MAXN4)   ! final hourly atmosphere temperature, not predicted
     &, TOFALB(MAXNH,MAXN4)	! hourly top-of-atm albedo, not predicted
C_Desc Designed for seasonal heatflow into annual frost
C_Hist 2004jul05 Hugh Kieffer
C_End __________________________________________________________________________

