function rndex, xin,xx,noex=noex
;_Titl  RNDEX  Finds floating-point index of within a monotonic array
;_args
; xin	in.	Value to be located within the array
; xx	in. 	Array to be searched; must be monotonic, of either slope.
; noex	in_	Controls how exterior requests are handled.
;     0= Default = Uses limiting valid index, equivalent to linear extrapolation
;			RTERP would yield end-point values
;     1= no extrapolation, RTERP would yield end-point values
;  else= -71 for below  xx range and -72 for above  
; func	out. 	Floating-point 0-based index 
;_Desc
; if xin falls within xx, does linear interpolation
; if formal error (XX has less than 2 points), returns -77
; Thus, general test for failure is return < 70.
;_Calls   locate  to find interval
;_Lien If two adjacent points equal, get infinity 
;_Hist 19-- Hugh Kieffer fortran version
; 2001mar31 HK revamp comments, add the -7x codes
; 2004Oct05 HK Improve documentation  2007aug17 More improvement
;_End

if not keyword_set(noex) then noex=0 ; default
na = n_elements(xx)     	; get length of input array
if na lt 2 then return,-77	; not enough elements for interpolation

loc = LOCATE(xx,xin)		; get lower index of interval containing xin
; if xin low, returns -1. if xin high, returns largest index
if noex eq 0 then loc = (loc >0) < (na-2)  $ ; force to specify an interval
else if noex eq 1 then begin    ; set index so that RNDEX would limit
    if loc lt 0 then return,0.	; set to first element
    if loc ge na-1 then return, na-1. ; set to last interval
endif else begin 
    if loc lt 0 then return,-71	; set low flag
    if loc ge na-1 then return,-72 ; set high flag
endelse
f=(xin-xx(loc))/(xx(loc+1)-xx(loc))	; interpolate or extrapolate
return,loc+f
end
