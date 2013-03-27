function krchange,hold, list=list, log=log, kcom1=kcom1
;_Titl  KRCHANGE  Find changes in KRC input values in common KRCCOM
; hold   in.    Lonarr generated by READKRCCOM
; list	in_	Intarr of initial items for MAKEKRCVAL.  Default is 
;       [101,103,117,123,124,208] == ALBEDO INERTIA TauDust SLOPE SloAzi IC
; log   in_.  If set, will print a log of changes between cases.
; kcom1	out_	Structure containing KRCCOM for the first case
; func	out.	Strarr(ncase) of detected changes from initial case
;_Calls  READKRCCOM  MAKEKRCVAL
;_Hist 2006mar22 Hugh Kieffer
; 2006apr22 HK  Add kcom21 as output keyword
; 2007feb26 HK  Allow for debug
; 2008oct17 HK  Get nwkrc from DEFINEKRC
; 2008oct23 HK  Modify to call READKRCCOM , stop before return if  !dbug  true
; 2011jul28 HK  Increase last floating point item compared
;_End

ncase=hold[2]
if not keyword_set(list) then list=[101,103,117,123,124,208]

out=strarr(ncase)
for k=0,ncase-1 do begin
    kcom=READKRCCOM(k+1,hold) ; read KRCCOM for case k+1
    if size(kcom,/type) ne 8 then message,'Bad return from READKRCCOM'
    if k eq 0 then begin ; base case
        kcom1=kcom 
        ss=MAKEKRCVAL(kcom1,list)
    endif else begin
        jj=[1] ; initiate an integer array
        ii=where(kcom.fd[0:63]-kcom1.fd[0:63] ne 0,j)
        if j ne 0 then jj=[jj,ii+101]; float index
        ii=where(kcom.id[0:19]-kcom1.id[0:19] ne 0,j)
        if j ne 0 then jj=[jj,ii+201] ; integer index
        ii=where(kcom.ld-kcom1.ld ne 0,j)
        if j ne 0 then jj=[jj,ii+301] ; logical index
        i=n_elements(jj)        ; first is a dummy
        if i gt 1 then ss=MAKEKRCVAL(kcom,jj[1:*]) else ss='No change '
    endelse
    out[k]=ss
endfor

if keyword_set(log) then begin 
    print,'Case=  1 had: '+out[0]
    for j=1,ncase-1 do  print,'Case=',j+1,' changed: '+out[j] 
endif
if !dbug then stop
return,out
end
