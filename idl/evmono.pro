function evmono, cc,x
;_Titl  EVMONO  Evaluate monomial power series
; cc	in.	vector of coefficients, 0-degree first, 
;			must have 2 or more elements
; x	in.	independant value; scalar or array
; func	out	sum of the monomial series. Will be float unless x was double
;			or cc was double
;
;_Hist  99aug04 Hugh Kieffer
;_End

n=n_elements(cc)	 ; number of coefficients in monomial
ss=size(x)
if ss(0) eq 0 then sum=cc(n-1) else begin; start sum(s) with last coefficent
  if ss(ss(0)+1) ne 5 then ss(ss(0)+1) = 4 ; make float unless it was double
  sum=MAKE_ARRAY(size=ss,value=cc(n-1))	; make output array and set initial 
					;   value to last coefficent
  endelse
  for i=n-2,0,-1 do	sum=x*sum+cc(i) ; sum all the lower terms
return,sum
end
  
