function st0, vin , nojoin=nojoin
;_Titl  ST0  Make minimal string for numbers, or string arrays
; vin	 in.   scalar or vector of numbers, all the same type
; nojoin in_ If set, output has same number of elements as input, 
;		i.e., no blank padding
; 	     If not set, return scalar string with blank padding. (2 for strings)
; func	 out.  String of one value, or vector of them with one blank between
;_Lims Not designed for more than 99 items in a vector
;_Calls  DELAST0
;_Hist 99jul02 Hugh Kieffer  Revised to remove float trailing zeros 
; 99aug30 HHK fix to handle byte values
; 99sep15 HHK revise to return single string for integer arrays
; 99dec30 HHK minor fix, string input not treated as float.
; 2001nov01 HK Add nojoin keyword, recode.
; 2002aug05 HK Extract DELAST0 into separate file
;_End

vv=vin
n= N_ELEMENTS(vv)
type=SIZE(vv,/type)
flt = type ge 4 and type le 6 ; float or double precision
if type eq 1 then vv=fix(vin)	; must convert byte to integer

if keyword_set(nojoin) then begin 
    ss=strarr(n)                ; output array
endif else begin
    if type eq 7 then b='  ' else b=' ' ; to separate items in output
    ss=b; 
endelse
for i=0,n-1 do begin
    vvi=vv[i]
    if flt then q=DELAST0(STRTRIM(STRING(vvi),2)) $
    else if type eq 7 then q=STRTRIM(vvi,2) $
    else  q=STRTRIM(STRING(vvi),2)
    if keyword_set(nojoin) then ss[i]=q else ss=ss+q+b  
endfor
return,ss
end
