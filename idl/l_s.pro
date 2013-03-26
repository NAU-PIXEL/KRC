function l_s, arg, kode=kode
;_Titl  L_S  Convert Martian season L_s <-> Julian day
; arg	in.	DEPENDS upon kode:  May be scaler or array
;		kode =0 or absent: julian day OR offset from J2000.
;                         Switches at arg[0] 1000000.
;		Kode = 1:   days from start of Martian year
;		Kode = 2:   L_s aerographic longitude of the sun, in degrees. 
;		   should be in 0 to 360.  May be scaler or array
;		Kode = 3:  integer or deciaml year, e.g.,1998.7
; 		
; kode	in_	Controls action
;	kode=0 Convert full Julian day , or day offset from J2000, to L_s
;	kode=1 convert days from start of Martian year (L_s=0) to L_s
;	kode=2 Convert L_s to days into a martian year
;	kode=3 Find nearest full julian day of L_s=0 for requested year
; func.	out.	Kode=0,1  Aerographic longitude of the sun, in degrees.
;		Kode=2	  Days (of 86400 seconds) from prior L_s=0
;		Kode=3    Full julian day of previous L_s=0
;_Desc
;     kode 1 and 2 are inverse of each other
;_Hist 97dec17 Hugh Kieffer original version
;   97dec31 HHK include modulo so the results are always 0 to 360.
; 2000dec22 HHK Derive from L_sub_s, Add kode=1,2,3 options
; 2003jul17 HK Fix so this will handle long-integer full Julian date
; 2008apr08 HK Set so dates <1.E6 are considered offset from J2000
;_End

if not keyword_set (kode) then kode=0
jd2000=2451545.D0               ; Julian day of J2000, 2000jan01 noon GMT
jdy=2450322.34D0                ; start of mars year circa 1986aug26
myear=686.98D0                  ; days in a martian year

case kode of
    0: begin                    ; from absolute date to L_s
      k=size(arg,/type)
      if arg[0] gt 1.e6 then jd=double(arg) else jd=arg+jd2000  ; get full Ju
      mday=jd-jdy               ; days since start of mars year circa 1986aug26
      goto,getls & end

  1: begin & mday=arg       ; from Mars Day-of-year to L_s
getls:  x=mday/686.98D0     ; fractional martian year
       x=x-floor(x)         ; insure in range 0 to 1.
       x=x*2.0D0*!dpi           ; radians from start of martian year     
a= [ -10.328371,    57.296001,   -10.688416,   -3.2931221,  -0.62748339 $
     ,  -5.0168313, -0.050566287,  -0.41753547 ]
f=a(0) +a(1)*x +a(2)*cos(x-a(3)) + a(4)*cos(2.*x-a(5)) + a(6)*cos(3.*x-a(7))
f=ZERO360(f)                    ; avoid ge 360.
end 

2: begin ; from degrees L_s to days since start of a Mars year
    x=!dtor*arg                 ; convert degrees to radians
a= [19.717923, 109.33317, 20.417421, -3.4731399, -0.70853928 $
    , -5.3719812, 0.029281483, -0.92372042]
f= a(0) + a(1)*x + a(2)*cos(x-a(3)) + a(4)*cos(2.*x-a(5)) + a(6)*cos(3.*x-a(7))
end

3: begin ; find full JD of prior start of Mars year
    day=(arg-2000.)*365.25      ; approx days from J2000
    x= day + (jd2000-jdy)       ; days from  start of a mars year
    x=floor(x/myear)            ; smaller integer
    f=jdy+x*myear               ; JD that was on the start of a Mars year
end

else: message,'Called with illegal Kode'
endcase

return,f
end

;coefficents derived in   fit_lsubs  97dec17 to porb.prt
; x=    0.00910607* ( date- 11010.0)
;a= [ -9.86388 , 57.5329 , -10.7144 , -3.31933 , -0.636553 , -5.10982]

; following are fits to MICA run, every 5 days 
;Inital data file has L_s to 2 decimal places., this dominates error!
;dblarr
;chisq=   2.1078586e-05
;sigma=      0.44947921      0.13704680      0.17420014     0.029852505      0.21318231     0.27769774      0.17337688       3.7167788
;Maximum errors=    -0.013322718     0.012496469
;average absolute error=    0.0037515840
;form is a0 +a1 x + a2 cos(x-a3)+a4 cos ( 2x-a5) +... a_even cos(nx-a_odd)
;a=      -10.328371       57.296001      -10.688416      -3.2931221     -0.62748339      -5.0168313    -0.050566287     -0.41753547
;Before applying formula, MUST scale date by:
; radian =    0.00914610* [ Julian date- 2450322.34 ]




; following are fits to MICA run, every 5 days 
;Inital data file has L_s to 2 decimal places., this dominates error!
;chisq=  3.2785410e-05
;sigma=      0.11952286     0.033925342      0.16837019
;    0.0083114977      0.16930702      0.23817455
;      0.16962759       5.7524534
;Maximum errors=   -0.0084436715    0.0077122648
;average absolute error=    0.0044286921
;form is a0 +a1 x + a2 cos(x-a3)+a4 cos ( 2x-a5) +... a_even cos(nx-a_odd)
;a= [       19.717923       109.33317       20.417421
;      -3.4731399     -0.70853928      -5.3719812
;     0.029281483     -0.92372042
