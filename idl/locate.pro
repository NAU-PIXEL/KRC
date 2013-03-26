function locate,xx,x
;_titl  LOCATE  Find lower index of interval in ordered xx containing x
; xx in. array to be searched, must be monotonic of either sign.
; x  in. scalar value to be located
;_Desc
; If x is within the table, returns the lower index (not necessarily the 
;     lower value) of the interval containing x
; Returns -1 if x is beyond the low_index end of table
; Returns largest index if x is beyond the high_index end of table
;_limits  Does not test that xx is at least 2 elements long
;_Hist 98mar24 H.Kieffer derive from  Numerical Recipies routine LOCATE
; 2000apr02 HHK make jl long  2000jul02 insure jm stays long
;_End

n=n_elements(xx)                ; size of input table
jl=-1L                          ; initialize lower limit
ju=n                            ; "          upper limit
inc = xx(n-1) gt xx(0)          ; true if table is increasing

while ju-jl gt 1 do begin	; interval is not down to 1 yet
    jm=(ju+jl)/2                ; compute a midpoint
    if inc eq (x ge xx(jm)) then jl=jm else ju=jm ; reset one limit
endwhile

return,jl                       ; return lower index of final interval
end
