function rterp1,yy,x
;_title  RTERP1  real interpolation in a vector
;_args, all input.
; yy	in.	vector of values, should be >1 elements, need not be monotonic.
; x	in.	floating location in vector for which the value is desired.
; func.	out.	Interpolation of yy at x, or limiting value.
;_desc 
; if x is within the table, returns linear interpolation.
; if x is outside the table, returns value at the nearest end of table.
;_lims Does not check that yy has at least 2 elements
;_Desc
; This routine equal to RTERP with argument order reversed
;_calls  none
;_hist 98dec12 H.Kieffer  derived from rterp.pro
; 2000jul02 make j long.

if x lt 0. then return,yy(0)		; off low_index end
j=long(x)				; index of lower point
last=n_elements(yy)-1
if j ge last then return,yy(last)	; off high_index end
y= yy(j) + (x-j) * (yy(j+1)-yy(j))	; linear interpolated value
return,y			
end
