function pm180, aa,rad=rad,nan=nan
;_Titl  PM180  Brings angles into half-circle range [-180,+180)
;_args
; aa	in.   Scalar or vector of angles
; rad	in_   If set as flag (thus == 1), will treat angles as being in radians
;              if other value, uses that as 1/2 period 
; nan	in_   If set, will look for and avoid NAN's
; func	out. output array; OK to override input. double prec. if needed
;_Desc Coding chosen to avoid any operations (and hence possible loss 
;  of precision) for values already within the desired output range.
;_Hist 98dec28 Hugh Kieffer   Complete recode of earlier procedure.
;  99feb06  HK  Add radians option
; 2001jan07 HK  Recode, eliminating most tests.  add NaN option
; 2010jan05 HK  Allow  rad  to be any half period

;help,rad,nan

ss=size(aa) & type=ss(ss(0)+1)		; get type of input
if keyword_set(rad) then begin
    if abs(rad) eq 1 then half=!dpi else half=rad
endif else  half=180.D ; set angle units
if type ne 5 then half=float(half)
cir=2.*half

if keyword_set(nan) then begin  ; test for NAN's
    kk=where(finite(aa),ngood)  ; all the finite ones
    if ngood eq 0 then return,aa ; no finite values to test
    bb=aa[kk]                   ; only the finite ones
endif else bb=aa

ii = round (bb/cir)		; number of full circles
bb=bb-cir*ii			; subtract them
if !dbug ge 9 then stop
if keyword_set(nan) then begin
    ff=aa                       ; copy full input array
    ff[kk]=bb                   ; replace the finite ones
    return,ff
endif else return,bb

end
