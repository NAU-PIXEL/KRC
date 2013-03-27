function molewt, ss, nn ,holds,holdi,holdd
;_Titl MOLEWT  Molecular weights based upon common mix of isotopes
; ss  in. strarr(n) of symbols  Must be proper case
; nn  in_ int or fltarr(n2) Number of atoms in the molecule
;      if n2<n, all values assumed to be 1. and output array aligned with ss
; holds both. strarr(M) names symbols   DO NOT MODIFY
; holdi both  intarr(M) atomic number
; holdd both. fltarr(M) atomic weight   DO NOT MODIFY
; func. out float, molecular weight of fltarr of atomic weights
;_Desc  reads table once, 
;_Lien  Table name firm coded
;_Hist 2010feb10 Hugh Kieffer

nel=n_elements(holdi)
if nel lt 50 then begin 
    file='/home/hkieffer/mars/AtomicWeight.tab'
    sss=READTXTCOL(file,sp='W',nskip=-1,ncol=4,mrow=140,/quiet)
    siz=size(sss)
    if siz[0] ne 2 then message,'Read failed for '+file
    holds=sss[*,0:1]            ; names and symbols
    holdi=fix(sss[*,2])         ; atomic number
    holdd=float(sss[*,3])       ; atomic weight
endif

nis=n_elements(ss)
nin=n_elements(nn)
dom=nin ge nis                  ; do molecular wt
sym=holds[*,1]                  ; atomic symbols
if dom then sum=0. else out=fltarr(nis)
sum=0.
for k=0,nis-1 do begin 
    i=where(sym eq ss[k]) & i=i[0]
    if i ge 0 then begin
        if dom then sum=sum+nn[k]*holdd[i] else out[k]=holdd[i]
    endif else print,'Element not found: ',ss[k]
endfor

if dom then out=sum
 return,out
 end
