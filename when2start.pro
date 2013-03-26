function when2start, ls,j5,deljul
;_Titl WHEN2START Calc starting date for KRC to readh Ls on specific season step
; ls	in.	Float.  L_sub_S target
; j5	in.	Integer. Step on which to reach target
; deljul in.	Float. KRC season step size
;_Desc
; KRC uses Julian days offset from 2440000
if n_params() ne 3 then begin 
print,'Usage: q=when2start ( Ls target, # Steps to reach this, step size in days)'
print,'Return is starting date, offset from 2440000.'
return,0
endif

jd0=L_S(2002,kode=3)                 ; get JD of Ls=0 in 2003
jdl=L_S(ls,kode=2)                ; Days from Ls=0 to target Ls
jdt=jd0+jdl-2440000.D0          ; KRC target date
out=jdt-(j5-1.)*deljul          ; KRC start date

return,out
end
