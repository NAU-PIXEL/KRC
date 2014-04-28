function definekrc, what,param, labkf,labki,labkl,idmin,idmax $
,mxn2=mxn2,nbyt=nbyt,nword=nword,pid=pid
;_Titl  DEFINEKRC  Define structures in IDL that correspond for Fortran commons
; what	 in.  String to select which common. Valid are: KRC DAY LAT FIL
; params out. lonarr of parameters from krccom.inc
; THE FOLLOWING ARE DEFINED ONLY FOR what='krc'. Use all or none
;	labkf	out.	Strarr of description for krccom: real inputs
;	labki	out.	Strarr " " integer inputs
;	labkl	out.	Strarr " " logical values
;	idmin	out.	Intarr of minimum valid values for krccom.id
;	idmax	out.	Intarr of maximum valid values for krccom.id
; mxn2  in_     Integer  Alternate MAXN2
; nbyt  in_     Integer  Two uses: 8= file is REAL*8.  
; nword out_   Integer  Number of 4-byte words in common
; pid   out_   Strarr   Titles to items on params
; func.	out.   Structure for requested common
;_Calls  BYTEPAD
;_Hist 2014mar12 Hugh Kieffer Derive from definekrc.2013aug31
; 2014mar12 HK Accomodate REAL*8 KRC files, Drop support of pre-KofT versions
;_End     .comp  definekrc
if not keyword_set(nbyt) then nbyt=4 ; Default is REAL*4 version
dodp= nbyt eq 8 ; Expect KRC Version 3 with REAL*8
; vvvvvvv Extract from krccom.f
MAXN1 =30	; dimension of layers
if dodp then MAXN1 =50
MAXN2 =384*4	; dimension of times of day   Was 384=24*16
MAXN3 =16	; dimension of iteration days
MAXN4 =37	; dimension of latitudes.  Was 19
MAXN4E =38     ; " "  Even needed for LATCOM NDJ4
;MAXN5 =161      ; dimension of seasons
MAXN6 =6        ; dimension of saved years
MAXNH =48	; dimension of saved times of day. Was 24
MAXBOT=6	; dimension of time divisions. Was 10 MUST be even
NUMFD =96     ; Size of floats
NUMID =40	; Size of "  "  integers
NUMLD =20	; Size of "  "  Logicals
numtit=20       ;   # 4-byte words in  TITLE
numday=5        ;   # 4-byte words in  DAY
;^^^^^^^^^^^^^^^^^^^^ firm code, must agree with krccom.inc 
MAXN1P=MAXN1+1	; dimension layer temperature points
if dodp then begin 
   numtit=21
   n4krc=NUMFD*2+NUMID+NUMLD+2*MAXN4*2+numtit+numday ; num 4-byt
   nwkrc=n4krc/2
endif else begin
   n4krc=NUMFD+2*MAXN4 +NUMID+NUMLD+NUMTIT+NUMDAY ; length in 4-byte words
   nwkrc=n4krc
endelse

param=[MAXN1,MAXN2,MAXN3,MAXN4,MAXN6,MAXNH,MAXBOT $
,NUMFD,NUMID,NUMLD,NUMTIT,NUMDAY,NWKRC]
pid=['MAXN1','MAXN2','MAXN3','MAXN4','MAXN6','MAXNH','MAXBOT' $
   ,'NUMFD','NUMID','NUMLD','NUMTIT','NUMDAY','NWKRC']

if keyword_set(mxn2) then MAXN2 =mxn2
kode=strupcase(what)

; Now, Define a structure IDENTICAL to each Fortran common
case kode of  ; which COMMON

'KRC': begin
fd0=[ .25, 1.00, 250.0, 2.00, 1.00,1.0275, 630., 1600. $
, 0.10,  .30, 260., 689.7, .055, 200., 200.,  0.0 $
,  0.2,  .90, .5,  2.0,  0.5, 0.10,  0.0,  90. $
, 148.0,  589944.,  .65, 1.00, 0.65, 0.001,  50.,  0.0 $
,1.2000, .1800, 2.0000,  0.0,  0.0, .0020, .1000,  0.1 $
,8948.4,  17.1745, 00.0, 1.465, .0, 1368., 3.727, 800. ]
nfl=n_elements(fd0) ; number of Floats with labels here
if dodp then begin 
   out={ fd:dblarr(numfd) $               ; R*8: params, latitude in degrees.
         ,ALAT:dblarr(MAXN4),ELEV:dblarr(MAXN4) $ ; R*8: latitudes, elevations
         ,id:lonarr(numid),ld:lonarr(numld)  $    ; I*4: Integers and Logicals 
         ,tit:bytarr(4*numtit),daytim:bytarr(4*numday) } ; Character strings
    out.fd=[double(fd0),replicate(0.d0,numfd-nfl)] ; fill to end with  zeros
endif else begin
   out={ fd:fltarr(numfd),id:lonarr(numid),ld:lonarr(numld) $
         ,tit:bytarr(4*numtit),daytim:bytarr(4*numday) $
         ,ALAT:fltarr(MAXN4)  $ ; latitude in degrees.
         ,ELEV:fltarr(MAXN4)  } ; elevation in km.
   out.fd=[fd0,replicate(0.,numfd-nfl)] ; append the place for computed values
endelse
nword=n4krc
id0      =[19,384,10,19,10,24,0,0,  3,24,1,5,0,71,68,1,  -1,0,7,2]
nil=n_elements(id0) ; number of Ints with labels here
out.id=[id0,replicate(0L,numid-nil)]

ld0= [0,1,0,0,0,0,0,0,0,0,  1,0,0,1,0,0,0,0,0]
nll=n_elements(ld0)
out.ld=[ld0] ;replicate(0L,numld-nll)]
if N_PARAMS() ge 5 then begin ; if caller wants definitions 
labkf=[ $
 'ALBEDO  Surface albedo' $
,'EMISS   Surface emissivity' $
,'INERTIA Surface thermal inertia' $; [J m^-2 s^-1/2 K^-1] { cal cm * 4.184e4}' $
,'COND2   Lower material conductivity (IC>0)' $
,'DENS2   Lower material density (IC>0)' $
,'PERIOD  Length of solar day in days (of 86400 seconds)' $
,'SpcHeat Surface specific heat [J/Kg/K]' $ ;  {cal/g/K * 4184.}' $
,'DENSITY Surface density [kg/m^3] {g/cubic cm. *10}' $
,'CABR    Atmospheric infrared back radiation coefficient' $
,'AMW     Atm. molecular weight' $
,'[ABRPHA  unused Atm. Phase of ABRAMP, degrees relative to midnight' $.
,'PTOTAL  Atm. Global annual mean surface pressure at 0 elev.' $ ;Pascal[=.01mb
,'FANON   Non-condensing Mass-fraction of atmosphere' $ 
,'T_ATM   Atm temp for scale-height calculations' $
,'T_DEEP  Fixed bottom temperature. Used if IB<=1.' $
,'SpHeat2 Lower material specific heat (IC>0)' $
,'TauDust Atm. Mean solar opacity of dust' $
,'Dust_A  Atm. Single scattering albedo of dust' $
,'TauRati Atm. Ratio of thermal to visible opacity' $
,'Twilite Atm. Twilight extension angle [deg]' $
,'ARC2    Atm. Henyey-Greenstein asymmetry factor' $
,'[ARC3    unused Atm. coeff. for planetary heating' $
,'SLOPE   Ground or pit slope, degrees dip. MUST NOT slope beyond pole.' $
,'SloAzi  Ground Slope azimuth, degrees east from north' $
,'TFROST  Frost Minimum saturation temperature <<- local sat. T (LVFT)' $
,'CFROST  Frost latent heat [J/Kg]' $ ; {cal/gm*4184.  [ Not used if' $
,'AFROST  Frost albedo, may be overridden (LVFA)' $ ;  [ TFROST never' $
,'F_Emis  Frost emissivity' $ ;                        [ reached' $
,'AF1     Frost constant term in linear relation of albedo to solar flux' $
,'AF2     Frost linear term in relation of albedo to solar flux units=1/flux' $
,'FroExt  Frost required for unity scattering attenuation coeff. [Kg/m^2]' $
,'[fd32    unused' $
,'RLAY    Layer thickness ratio' $
,'FLAY    First layer thickness (in skin depths)' $
,'CONVF   Safety factor for classical numerical convergence' $ ; 0=no time division' $
,'DEPTH   Total model depth (scaled) (overrides FLAY if not 0.)' $
,'DRSET   Convergence Perturbation factor' $ ;. 0=reset all layers to surface ave.' $
,'DDT     Convergence limit of temperature 2nd differences' $
,'GGT     Convergence test: RMS layer T in a day; del_T surface each time' $
,'DTMAX   Convergence test: RMS layer T changes in a day' $
,'D_Jul   Starting Julian date of run (N5>0)' $
,'DelJul  Increment between seasons in Julian days (N5>1)' $
,'SDEC    Solar declination in degrees. (N5=0)' $
,'D_AU    Distance from Sun in astronomical units (N5=0)' $
,'LsubS   Aerocentric longitude of Sun, in degrees. (for printout only)' $
,'SolCon  Solar constant 1367.9 W/m^2' $
,'Gravity Surface gravity.  MKS-units' $
,'AtmCp   Specific heat at constant pressure of the atmosphere' ]
if numfd gt 80 then labkf=[labkf $
,'ConUp0  Constant Coefficient in T-dep conductivity; Upper material'  $
,'ConUp1  Linear  "   in k=c0+c1x+c2x^2+c3x^3 where x=(T-220)*0.015' $
,'ConUp2  Quadratic    " ' $
,'ConUp3  Cubic coeff. "' $
,'ConLo0  Constant coef for lower material ' $
,'ConLo1  Linear       "' $
,'ConLo2  Quadratic    "' $
,'ConLo3  Cubic coeff. "']
if numfd gt 88 then labkf=[labkf $
,'SphUp0  COnstant Coefficient in T-dep specificHeat; Upper material'  $
,'SphUp1  Linear  "   in k=c0+c1x+c2x^2+c3x^3 where x=(T-220)*0.015' $
,'SphUp2  Quadratic    " ' $
,'SphUp3  Cubic coeff. "' $
,'SphLo0  Constant coef for lower material ' $
,'SphLo1  Linear       "' $
,'SphLo2  Quadratic    "' $
,'SphLo3  Cubic coeff. "']

labkf=[labkf $
,'HUGE = 3.3E38    nearly largest  REAL*4 value' $
,'TINY = 2.0E-38   nearly smallest REAL*4 value' $
,'EXPMIN = 86.80   neg exponent that would almost cause underflow' $
,'FSPARE Spare' $
,'FSPARE Spare' $
,'RGAS = 8.3145    ideal gas constant  (MKS=J/mol/K)' $
,'TATMIN  Atmosphere saturation temperature' $
,'PRES    Local surface pressure at current season' $
,'OPACITY Solar opacity for current elevation and season' $
,'TAUIR   current thermal opacity at the zenith' $
,'TAUEFF  effective current thermal opacity ' $
,'TATMJ   One-layer atmosphere temperature' $
,'SKYFAC  fraction of upper hemisphere that is sky' $
,'TFNOW   frost condensation temperature at current latitude' $
,'AFNOW   frost albedo  at current latitude' $
,'PZREF   Current surface pressure at 0 elevation, [Pascal]' $
,'SUMF    Global average columnar mass of frost [MKS]' $
,'TEQUIL  Equilibrium temperature ( no diurnal variation)' $
,'TBLOW   Numerical limit (Blowup) temperature' $
,'HOURO   Output Hour requested for "one-point" model' $
,'SCALEH  Atmospheric scale height' $
,'BETA    Atmospheric IR absorption' $
,'DJU5    Current Julian date (offset 2440000 ala PORB convention)' $
,'DAM     Half length of daylight in degrees' $
,'EFROST  Frost on the ground at current latitude [kg/m^2] {g/cm^2 * 10.} ' $
,'DLAT    Current latitude' $
,'COND    Top material Thermal conductivity (for printout only)' $
,'DIFFU   Top material Thermal diffusivity (for printout only)' $
,'SCALE   Top material Diurnal skin depth (for printout only)' $
,'PIVAL   pi' $
,'SIGSB   Stephan-Boltzman constant (set in KRC)' $
,'RADC    Degrees/radian' ]

labki=[ $
 'N1    # layers (including fake first layer)' $
,'N2    # times per day (lim MAXN2)' $ ;. Even: multiple of N24 and NMHA.' $
,'N3    Maximum # days to iterate for solution' $
,'N4    # latitudes (lim MAXN4=19)' $ ;. Global integrations done for N4>8' $
,'N5    # seasons' $
,'N24   # hours per day stored, should be divisior of  N2' $
,'IB    Bottom: 0=insulating, 1=constant T, 2= 1 & start all layers =TDEEP' $
,'IC    First layer of changed properties. 999=homogeneous, or 3 to N1-2' $
,'NRSET # [ays before reset of lower layers;  >N3=no reset' $
,'NMHA  # hour angles per day for printout (no limit)' $
,'NRUN  Run #' $
,'JDISK Season count disk output is to begin' $
,'IDOWN Season at which to read additional change cards. -1=never' $
,'Flx14 Index in FD of flexible print' $
,'Flx15 Index in FD of flexible print' $
,'KPREF Global pressure control. 0=constant, 1=Viking, 2=global frost' $
,'K4OUT Disk output control: -=KRCCOM(1)&TSF&TPF 0=LATCOM +=DAYCOM_last_lat 50+=bin5' $
,'JBARE J5 season count at which to set frost amount to 0. 0=never' $
,'Notif  Notification modulo' $
,'IDISK2 Number of disk writes to report']

idmin=[1,   5, 48, 7, 1, 0,12,0,  3,2, 4,0,0,0,61,61,0,-1,0,1,-1]
idmax=[999,  maxn1,maxn2,maxn3,maxn4, 222, maxnh,2, 999, 99, maxn2 $
       , 999,161,0,80,80,2,52,221,1000,999]

labkl=[ $
 'LP1   Print program description. TPRINT(1)' $
,'LP2   Print all parameters and change cards (2)' $
,'LP3   Print hourly conditions on last day (3)' $
,'LP4   Print daily convergence summary (4)' $
,'LP5   Print latitude summary (5)' $
,'LP6   Print TMIN and TMAX versus latitude and layer (6)' $
,'LPGLOB Print global parameters each season' $
,'LVFA  Use variable frost albedo' $
,'LVFT  Use variable frost temperatures' $
,'LkofT Use T-dependent conductivity' $
,'LPORB Call PORB1 just after full input set' $
,'LKEY  Read change item from terminal after main input set' $
,'LSC   Read change cards from input file at start of each season' $
,'LNOTIF Notify terminal at start of each season' $
,'LOCAL Use each layer for scaling depth' $
,'Prt76 Output to fort.76 [TLATS]  SUBS,DLAT,ALB,SKRC,TAUD,PRES' $
,'LPTAVE Print <T>-<TSUR> at midnight for each layer [TDAY]' $
,'Prt78 Output to fort.78 [TLATS] insolation and atm.rad.coefficents' $
,'Prt79 Output to fort.79 [TLATS] insolation and atm.rad. arrays' $
,'LONE  (Computed) Set TRUE if KRC is in the "one-point" mode']
; help,krccom.fd,labkf,fd0,krccom.id,labki,id0,krccom.ld,labkl,ld0
endif ;  N_PARAMS() ge 5
end ; case 'KRC'

'LAT': if dodp then begin
out ={DTM4:dblarr(MAXN4)   $ ; rms temperature change on last day
 ,TST4:dblarr(MAXN4)       $ ; predicted equilibrium temperature of ground
 ,TTS4:dblarr(MAXN4)     $ ; predicted mean surface temperature for each latitude
 ,TTB4:dblarr(MAXN4)       $ ; predicted mean bottom temperature
 ,FROST4:dblarr(MAXN4)     $ ; predicted frost amount gram/cm^2.
 ,AFRO4:dblarr(MAXN4)      $ ; frost albedo.
 ,TTA4:dblarr(MAXN4)       $ ; final atmosphere temperature
 ,TTX4:dblarr(MAXN4)       $ ; spare
 ,TMN4:dblarr(MAXN1,MAXN4) $ ; predicted convergence surface temperature
 ,TIN:dblarr(MAXN1,MAXN4)  $ ; minimum hourly layer temperature
 ,TAX:dblarr(MAXN1,MAXN4)  $ ; maximum hourly layer temperature
 ,TSF:dblarr(MAXNH,MAXN4)  $ ; final hourly surface temperature
 ,TPF:dblarr(MAXNH,MAXN4)  $ ; final hourly planetary temperature 
 ,NDJ4:lonarr(MAXN4) }       ; # days to compute solution for each latitude
nwlat=(8L+ 3L*MAXN1 + 2L*MAXNH) *MAXN4 +MAXN4E/2 ; number of 8-byte words
nword=2*nwlat
endif else begin
out ={ NDJ4:lonarr(MAXN4)  $ ; # days to compute solution for each latitude
 ,DTM4:fltarr(MAXN4)       $ ; rms temperature change on last day
 ,TST4:fltarr(MAXN4)       $ ; predicted equilibrium temperature of ground
 ,TTS4:fltarr(MAXN4)     $ ; predicted mean surface temperature for each latitude
 ,TTB4:fltarr(MAXN4)       $ ; predicted mean bottom temperature
 ,FROST4:fltarr(MAXN4)     $ ; predicted frost amount gram/cm^2.
 ,AFRO4:fltarr(MAXN4)      $ ; frost albedo.
 ,TTA4:fltarr(MAXN4)       $ ; final atmosphere temperature
 ,TTX4:fltarr(MAXN4)       $ ; spare
 ,TMN4:fltarr(MAXN1,MAXN4) $ ; predicted convergence surface temperature
 ,TIN:fltarr(MAXN1,MAXN4)  $ ; minimum hourly layer temperature
 ,TAX:fltarr(MAXN1,MAXN4)  $ ; maximum hourly layer temperature
 ,TSF:fltarr(MAXNH,MAXN4)  $ ; final hourly surface temperature
 ,TPF:fltarr(MAXNH,MAXN4) } ; final hourly planetary temperature
nword=(9L+ 3L*MAXN1 + 2L*MAXNH) *MAXN4

endelse ; case LAT

'DAY': if dodp then begin
 out={XCEN:dblarr(MAXN1)  $ ; Depth at layer centers [m]
 ,SCONVG:dblarr(MAXN1)   $ ; Classical convergence factor for each layer
 ,BLAY:dblarr(MAXN1)     $ ; Layer thicknesses [m]
 ,TMIN:dblarr(MAXN1)     $ ; Minimum layer temperatures of day
 ,TMAX:dblarr(MAXN1)     $ ; Maximum layer temperatures of day
 ,TTJ:dblarr(MAXN1P)     $ ; Layer temperatures (T(1) is surface temperature)    
 ,TT1:dblarr(MAXN1,MAXN3) $ ; Temperatures at start of day for each layer and day
 ,TTS:dblarr(MAXN3)      $ ; Mean daily surface temperatures                     
 ,TTB:dblarr(MAXN3)      $ ; Mean daily bottom temperatures                      
 ,TTA:dblarr(MAXN3)      $ ; End-of-Day Atmospheric temperatures
 ,DTMJ:dblarr(MAXN3)     $ ; RMS daily temperature                               
 ,FRO:dblarr(MAXN3)      $ ; Daily frost amounts. [kg/m^2]                    
 ,ASOL:dblarr(MAXN2)     $ ; Insolation at each time of day                      
 ,ADGR:dblarr(MAXN2)     $ ; Atm. solar heating at each time of day 
 ,TOUT:dblarr(MAXN2)     $ ; Surface temperatures of solution at each time of day
 ,TSFH:dblarr(MAXNH)     $ ; Hourly surface temperatures at solution
 ,TPFH:dblarr(MAXNH)     $ ; Hourly planetary temperatures at solution
 ,N1K:lonarr(MAXBOT)     } ; Binary time division layers
nwday= 5L*MAXN1 + MAXN1P + (5L+MAXN1)*MAXN3 + 3*MAXN2 + 2*MAXNH + MAXBOT/2
nword=2*nwday
endif else begin
 out={XCEN:fltarr(MAXN1)  $ ; Depth at layer centers [m]
 ,SCONVG:fltarr(MAXN1)   $ ; Classical convergence factor for each layer
 ,BLAY:fltarr(MAXN1)     $ ; Layer thicknesses [m]
 ,TMIN:fltarr(MAXN1)     $ ; Minimum layer temperatures of day
 ,TMAX:fltarr(MAXN1)     $ ; Maximum layer temperatures of day
 ,TTJ:fltarr(MAXN1P)     $ ; Layer temperatures (T(1) is surface temperature)    
 ,TT1:fltarr(MAXN1,MAXN3) $ ; Temperatures at start of day for each layer and day
 ,TTS:fltarr(MAXN3)      $ ; Mean daily surface temperatures                     
 ,TTB:fltarr(MAXN3)      $ ; Mean daily bottom temperatures                      
 ,TTA:fltarr(MAXN3)      $ ; End-of-Day Atmospheric temperatures
 ,DTMJ:fltarr(MAXN3)     $ ; RMS daily temperature                               
 ,FRO:fltarr(MAXN3)      $ ; Daily frost amounts. [kg/m^2]                    
 ,ASOL:fltarr(MAXN2)     $ ; Insolation at each time of day                      
 ,ADGR:fltarr(MAXN2)     $ ; Atm. solar heating at each time of day 
 ,TOUT:fltarr(MAXN2)     $ ; Surface temperatures of solution at each time of day
 ,TSFH:fltarr(MAXNH)     $ ; Hourly surface temperatures at solution
 ,TPFH:fltarr(MAXNH)     $ ; Hourly planetary temperatures at solution
 ,N1K:lonarr(MAXBOT)     } ; Binary time division layers
nword= 5L*MAXN1 + MAXN1P  + (5L+MAXN1)*MAXN3 + 3*MAXN2 + 2*MAXNH + MAXBOT
endelse ; case DAY

'FIL': begin & numch=80 ; FILCOM
blan80=BYTEPAD('dum',80)
blan20=BYTEPAD('dum',20)
blan12=BYTEPAD('dum',12)
out={FINPUT:blan80,FOUT:blan80,FDISK:blan80,FVALB:blan80,VTAU:blan80 $
     ,TITONE:blan20,VERSIN:blan12 }
nword=(5*numch+20+12)/4 ; number of 4-byte words
end

else: message,'invalid string for what'
endcase

return,out
end
