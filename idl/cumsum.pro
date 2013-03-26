FUNCTION cumsum, aa, rev=rev
;_Titl  CUMSUM  Form cumulative sum of 1-D array, option for reverse
;_Args
; aa	in.  Array to be summed
; rev   in_  Flag. If set, sums from last to first
; func.	out. Cumulative sum array; will be same size as input array
;_Desc
; If input is byte, integer or longword but could not overflow, sum is longword 
; else, sum is done in double precision
; If input array inappropriate, stops with message
;_Hist  98nov30  HHKieffer
; 2000jan04 HHK  Do float sums in double precision
; 2000feb08 HHK  Formal error yields message
; 2001jan10 HHK  Skip NAN's
; 2004jul31 HK   Use double if long might overflow
; 2009jan30 HK   Add reverse option
;_End

ss=size(aa)			; get characteristics of input array
if ss[0] eq 0 then message,'Requires array input'	; input not an array
itype=ss[ss[0]+1]		; input array type
n    =ss[ss[0]+2]		; input array length
if itype gt 6 then message,'input type must be numeric'

if itype eq 2 and n gt 65536L then itype=3 ; possible overflow, force next check

if itype eq 3 then begin ; check for worst-case possible overflow
    i=double(max(abs(aa)))
    if n*i ge 2.147483647D9 then itype=4 ; force double precision
endif

if itype le 3 then begin 
	cc=make_array(n,/long)
	sum=0L	
    endif else begin
	cc=make_array(n,/double)
	sum=0.D
    endelse

i1=0 & i2=n-1 & i3=1
if keyword_set(rev) then begin ; sum top down
    i1=n-1 & i2=0 & i3=-1
endif

for i=i1,i2,i3 do begin
    if finite(aa[i]) then sum=sum+aa[i]
    cc[i]=sum
endfor

return,cc
end

