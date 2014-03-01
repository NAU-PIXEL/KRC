function zero360, aa,rad=rad
;_Titl  ZERO360  Brings angles into 0:360 degree range
;_args
; aa	in. scalar or vector of angles
; rad	in_opt.	if set, will treat angles as being in radians
; func	out. output array; OK to override input. double prec. if needed
;_Desc Coding chosen to avoid any operations (and hence possible loss 
;  of precision) for values already within the desired output range.
;_Hist 99may13 Hugh Kieffer   derived from pm180

type=size(aa,/type)	; get type of input
if keyword_set(rad) then half=!dpi else  half=180.D ; set angle units
if type ne 5 then half=float(half)
cir=2.*half

ii = fix(aa/cir)		; number of full circles
bb=aa-cir*ii			; subtract them
j=where (bb lt 0, nj)		; find all negative values
if nj gt 0 then bb(j)=bb(j)+cir	;   move to positive
return,bb
end