FUNCTION cumsum, aa, rev=rev ,sing=sing
;_Titl  CUMSUM  Form cumulative sum of 1-D array, option for reverse
;_Args
; aa	in.  Array to be summed
; rev   in_  Flag. If set, sums from last to first
; sing  in_  Flag. If set, return input precesion (if would not overflow)
; func.	out. Cumulative sum array; will be same size as input array
;_Desc
; If input is byte, integer or longword but could not overflow, sum is longword 
; else, sum is done in double precision
; If input array inappropriate, stops with message
;_Lein
; Does not handle numerical types >6; e.g., double-precision complex
;_Hist  98nov30  Hugh Kieffer
; 2000jan04 HK  Do float sums in double precision
; 2000feb08 HK  Formal error yields message
; 2001jan10 HK  Skip NAN's
; 2004jul31 HK  Use double if long might overflow
; 2009jan30 HK  Add reverse option
; 2010dec12 HK  And input precision option
; 2011may08 HK !dbug  ge 9
;_End

ss=size(aa)			; get characteristics of input array
if ss[0] eq 0 then message,'Requires array input'	; input not an array
intype=ss[ss[0]+1]		; input array type
n    =ss[ss[0]+2]		; input array length

if intype le 0 or intype gt 6 then message,'input type must be numeric'

; set summation word type
itype=intype>3                  ; sum all integers into long
if itype eq 3 then begin ; check for worst-case possible Long overflow
    i=double(max(abs(aa)))
    if n*i ge 2.147483647D9 then itype=5 ; force double precision
endif
if intype eq 4 then itype=5

cc=make_array(n,type=itype)     ; to hold output
sum=cc[0]                       ; will be of same type
i1=0 & i2=n-1 & i3=1
if keyword_set(rev) then begin ; sum top down
    i1=n-1 & i2=0 & i3=-1
endif

for i=i1,i2,i3 do begin
    if finite(aa[i]) then sum=sum+aa[i]
    cc[i]=sum
endfor

if keyword_set(sing) and itype ne intype then begin
    ya=min(cc,max=yb)
    case intype of
        1: if ya ge 0 and yb le 255 then cc=byte(cc)
        2: if ya ge -32768 and yb le 32767 then cc=fix(cc)
;    3: if ya ge 0 and yb le 255 then cc=long(cc)
        4: if ya ge -1.e37 and yb le 1.e37 then cc=float(cc)
        else:                   ; do nothing for long, double,complex
    endcase
endif

if !dbug ge 9 then stop
; help,intype,itype,cc,ya,yb

return,cc
end

