function mean_std,xx,wei=wei,nan=nan,double=double, STD=std
;_Titl  MEAN_STD   Mean and standard deviation of a vector
; xx	in.     Vector
; wei	in_	Weight for each point, Default= uniform
; nan	in_	If set, treats NAN's as missing data
; double in_    Flag: Do totals in double precision
; std	out_	Standard deviation;  set to -1. if too few points
; func. out.    Mean.  It is possible that this is not finite
;_Hist 98jul05 Hugh Kieffer 99feb01 more robust
; 2000feb06 HHK add weight option
; 2001jan05 HHK add NAN option
; 2001dec17 HK Fix bug of undefined std when input is all NANs
; 2005jun22 HK Fix StdDev bug, Did not handle n=2
; 2009sep27 HK Add keyword double
; 2014jan08 HK Add comment that mean (and std) can be not finite in perverse
;    cases. E.g., total exceeds valid numerical range   
;_Desc
; Weight is squared for weighting in the variance sum
;_End

n=n_elements(xx)
db=keyword_set(double)          ; do totals in double precision
if keyword_set( nan ) then begin
    igood = where( finite(xx) ne 0, ngood) ; number that are not NAN's
    if ngood eq 0 then begin
        std=-1
        return,!values.F_NAN    ; no good values
    endif
    if ngood lt n then  begin   ; some good, use recursive call
        mean=MEAN_STD( xx[igood],wei=wei,STD=std, doub=db)
        return,mean
    endif
endif                           ; else, all are good, proceed normally


if n gt 1 then begin ;  must sum to get average
   if not keyword_set(wei) then mean=total(xx,doub=db)/float(n) else begin
      if n_elements(wei) ne n then message,'wei must be same size as xx'
      mean = total(wei*xx,doub=db)/total(wei,doub=db)
   endelse    
endif else begin ; only one point supplied
  if n eq 1 then mean=xx[0]  else message,'xx undefined' ; <error handling
endelse

if arg_present(std) then begin	; do standard deviation
    if n ge 2 then begin
      if not keyword_set(wei) then var=total((xx-mean)^2,doub=db) /(n-1.) $ ; variance
           else var=(total((wei*(xx-mean))^2,doub=db) / total(wei^2,doub=db))*(float(n)/(n-1.))
      std=sqrt(var)             ; standard deviation
    endif else std=-1.; not enough points for std_dev
endif

return,mean
end
