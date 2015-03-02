function lshk, xx, hi=hi
;_Titl  LSHK  LsubS routine based on MY calendar paper:  low and high precision
; xx in. scalar or vector.  Ephemeris days from epoch J2000.0
; hi in_ Flag.  If set, high-precision (0.002 deg) version, 
;               If =2, higher-precision (0.001 deg) version, else low (0.05 deg)
; func. out. Single-precision L_S for each date input.
;_Calls  PM180  ZERO360 
;_Desc
; Described in "Enumeration of Mars Years and Seasons since the Beginning 
; of Telescopic Exploration"
; Valid for 1607 to 2143; xx=-40380 to +52361
;_Hist 2015jan02 Hugh Kieffer, Celestial Reasonings, Genoa NV 89411-1057
; 2015jan19 HK Include 9 more Planetary Commensurate Periods (PCPs)
;_End               .comp lshk

julcen=36525.D0               ; days in a Julian century
twopi=2.*!dpi                 ; 2 pi
dtor=!dpi/180.D0              ; double precision version of !dtor
radeg=180.D0/!dpi             ; " " " !radeg
;^^^^ constants

nin=n_elements(xx)              ; number of dates requested
if keyword_set(hi) then begin 
   a0=  270.389001822D0
   a1=  0.52403850205D0
   a2=   -0.000565452D0
   m0=   19.380433818D0
   m1=  0.52402076345D0
   e0=    0.093402202D0
   e1=    0.000091406D0
;   period,days     amp*1000     phase         which    years
ppp=[816.3755210D0,   7.0591,    48.48944, $ ;   M-J   2.23511
    1005.8002614D0,   6.0890,   167.55418, $ ;  M-2J   2.75373
     408.1877605D0,   4.4462,   188.35480, $ ; 2M-2J   1.11756
    5765.3098103D0,   3.8947,    19.97295, $ ;  2M-E  15.78456
     779.9286472D0,   2.4328,    12.03224, $ ;   E-M   2.13533
     901.9431281D0,   2.0400,    95.98253, $ ; 2E-3M   2.46939
   11980.9332471D0,   1.7746,    49.00256]   ;  V-3M  32.80201

   alpha=a0 +a1*xx +a2*(xx/julcen)^2 ; alpha
   mang=PM180(m0+m1*xx)         ; mean anomaly, degrees
   c33=13.D0/12.                ; fixed part of equation-of-center terms
   c42=-(11.D0/24.) & c44=(103.D0/96.)
   c51=(5.D0/96.) & c53=-(43.D0/64.)   & c55=(1097.D0/960.)
   c62=17.D0/192. & c64=-(451.D0/480.) & c66=1223.D0/960.
   delnu=dblarr(nin)            ; to hold M-nu = mean - true anomaly
   for i=0,nin-1 do begin
      ecc=e0+(e1/julcen)*xx[i]  ; current eccentricity
      e2=ecc*ecc & e3=e2*ecc & e4=e3*ecc & e5=e4*ecc & e6=e5*ecc
;  coeff of sin nM
      d1= 2.*ecc-.25*e3+c51*e5
      d2= 1.25*e2+c42*e4+c62*e6
      d3=c33*e3+c53*e5
      d4=c44*e4+c64*e6
      d5=c55*e5
      d6=c66*e6
      M=dtor*mang[i]                 ; current mean anomaly in radians
      s1=sin(M) & s2=sin(2.*M) &  s3=sin(3.*M) & s4=sin(4.*M) & s5=sin(5.*M) 
      s6=sin(6.*M)
      dsur=d6*s6 +d5*s5 +d4*s4 +d3*s3 +d2*s2 +d1*s1 ; add smallest terms first
      delnu[i]=dsur                                 ; true-mean in radians
   endfor
   pbs=dblarr(nin)              ; to hold planetary perturbations
   for i=0,6 do begin           ; loop on planet
      j=3*i                     ; index of period
      pbs=pbs+(0.001*ppp[j+1])*cos((twopi/ppp[j])*xx +dtor*ppp[j+2])
   endfor
   if hi eq 2 then begin ; more PCP               what   Standish years
      pcp=[2882.1147D0,   1.34607,  288.7737, $ ;   4M-2Ef   7.89228  
           4332.2204D0,   1.03438,   37.9378, $ ; (2M-E)*3/4 9.06113  
           373.07883D0,   0.88180,   65.3160, $ ;    2M-Jf   1.02138  
           1069.3231D0,   0.72350,  175.4911, $ ;   5M-3Ef   2.92735  
           343.49194D0,   0.65555,   98.8644, $ ;     M-Vf   0.91423  
           1309.9410D0,   0.81460,  186.2253, $ ;    M-3Jf   3.58573  
           450.69255D0,   0.74578,  202.9323, $ ;   2M-3Jf   1.23373  
           256.06036D0,   0.58359,  212.1853, $ ;   3M-2Jf   0.70103  
           228.99145D0,   0.42864,   32.1227]   ;    3M-Sf   0.64060  
      pcsum=dblarr(nin)    
      for i=0,8 do begin                       ; loop on planet
         j=3*i                                 ; index of period
         pcsum=pcsum+(0.001*pcp[j+1])*cos((twopi/pcp[j])*xx +dtor*pcp[j+2])
      endfor
;      hpb=pbs    ;test
      pbs=pbs+pcsum 
   endif

   out=alpha+radeg*delnu+pbs

endif else begin ; ^^^^^^^^^^^high   vvvvvvvvvvvvvvvv low

;  more digits than needed...
;   B0=270.388590979D0 
;   B1=0.52403854224D0  
;   Q0= 19.380950928D0
;   Q1=0.52402076875D0
;   scc=[10.67848D0, 0.6207657D0, 0.05031290D0]  ; coefficient of sine 
; Below are then number of digits published. 
   a0=270.38859D0 
   a1=0.524038542D0  
   m0= 19.38095D0
   m1=0.524020769D0
   scc=[10.67848D0, 0.62077D0, 0.05031D0]  ; coeff. of sine  
   ytrop= a0+a1*xx                         ; alpha
   mang=dtor*PM180(m0+m1*xx)               ; Mean anom, radians
   sum=dblarr(nin)
   for i=0,2 do sum=sum+scc[i]*sin((i+1.D0)*mang) ; eq. of center
;   sum=scc[0]*sin(mang) +scc[1]*sin(2.*mang) +scc[2]*sin(3.*mang) ; alt. form
   out=ytrop+sum

endelse
;if !dbug ge 6 then stop
return,float(ZERO360(OUT))
 end
