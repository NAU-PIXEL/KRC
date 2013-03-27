function when2start, ls,j5,del
;_Titl WHEN2START Calc starting date for KRC to reach Ls on specific season step
; ls	in.	Float.  L_sub_S target
; j5	in.	Integer. 1-based Step on which to reach target
; del   in.	Float. KRC season step size OR negative of either:
;               number of Sols. <36 OR intervals per Mars-years >35 
;_Desc
; KRC uses Julian days offset from 2440000 so J2000.0 is 11545 in KRC
; This routine will print intervals/Mars year, so can call first with any 
; value in second argument, then again using this new information.
; If 2nd arg is negative, 
;_Hist 2002nov 1 or earlier  Hugh Kieffer
; 2008apr08 HK Allow input of negative timestep 
;_End

if n_params() ne 3 then begin 
print,'Usage: q=when2start ( Ls target, Step index to reach this' $
+', step size in days OR -sols)'
print,'Return is starting date, offset from 2440000.'
return,0
endif

j2000= 2451545L ; Julian date of MDJ base , 2000 jan 01,noon
kbase= 2440000L ; julian date of KRC Base date
k2000= j2000-kbase ; KRC date of J2000   = 11545
if del lt -35.5 then deljul=-686.98/del $ ; input was - steps/year
else if del lt 0. then deljul=-del*(88775.2/86400.) $ ; input was sols
else deljul=del                 ; input was days

ipy=686.98/deljul ; Intervals per Mars year 
n5=j5-1+round(ipy) ; index of last of a full year including start
Print,'Intervals per Mars year=', ipy
Print,'Interval in sols & days =' ,deljul/1.02749, deljul
Print,'N5 to have a full Mars year recorded=',n5 

jd0 =L_S(2002,kode=3)           ; get full JD of Ls=0 in 2003
dls=L_S(ls,kode=2)              ; Days from Ls=0 to target Ls

mjt=jd0+dls-j2000               ; MJD target date
mj1=mjt-(j5- 1)*deljul          ; MJD start date
mn5=mjt+(n5-j5)*deljul          ; MJD of last of a Mars year

; help,ls,j5,del,kbase,jd0,dls
fmt='(a,i4,f11.2,f8.2,f11.2)'
print,'      What   Index    KRCdate      Ls        MJD'
print,'start==1      ', 1,mj1+k2000,L_s(mj1),mj1,form=fmt
print,'target index  ',j5,mjt+k2000,L_s(mjt),mjt,form=fmt
print,'Full year end ',n5,mn5+k2000,L_s(mn5),mn5,form=fmt

return,mj1+k2000
end
