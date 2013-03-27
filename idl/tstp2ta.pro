function tstp2ta, ts, tp, emis, cabr, taud, taurat, prat=prat
;_Titl TSTP2TA Convert T_surf and T_plan to T_atm
; ts     in. fltarr(N=any)   Surface kinetic temperature
; tp     in. fltarr(same)  Planetary brightness temperature
; emis   in. Float  Surface emissivity
; cabr   in. Float  IR opacity of dust-free atmosphere
; taud   in. Float  Solar atmospheric opacity
; taurat in. Float  Ratio of thermal to solar opacity
; prat   in_ Float  Ratio of current pressure to nominal total pressure
;                       Default=1.
; func.  out. fltarr(N) Atmosphere kinetic temperature
;_Hist 2008Oct04 Hugh Kieffer
; 2010sep05 HK Better comments
;_End

if not keyword_set(prat) then prat=1.
if n_elements(ts) ne n_elements(tp) then message,'Ts and Tp must be same size' 
; SIGSB = 5.67051e-8        ;  Stephan-Boltzman constant:  SI =  W m^-2 K^-4
; from tlats.f
TAUIR=(CABR+TAUD*TAURAT)*prat   ; thermal opacity, zenith
;from tday.f
EMTIR = EXP(-TAUIR)             ; Zenith Infrared transmission of atm
FAC82=1.-EMTIR                  ;  " absorption "
FAC8=EMTIR*EMIS
;  TP=(FAC8*TSUR4+FAC82*TATM4)**0.25
out= ((tp^4 -fac8*ts^4)/fac82)^0.25 ; Atmosphere kinetic temperature
return, out
end


