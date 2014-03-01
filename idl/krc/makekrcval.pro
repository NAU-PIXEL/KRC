function makekrcval, krccom,ii
;_Titl  MAKEKRCVAL Make string of selected KRC inputs: Key=val
; krccom	in.	KRC KRCCOM structure
; ii	in.	Intarr of 1-based indices to print: 
;			+100 for .fd +200 =if +300=ld
; func.	out.	String. of label=value. Null if ii is invalid
;_Calls DEFINEKRC  DELAST0  
;_Hist 2002aug05 Hugh Kieffer
;_End
qq=DEFINEKRC('KRC',params,labkf,labki,labkl,idmin,idmax)
labkf=strword1(labkf)           ; retain only the first word of description
labki=strword1(labki)           ; retain only the first word of description
labkl=strword1(labkl)           ; retain only the first word of description
n=n_elements(ii)

out=''                          ; null string to become results
for j=0,n-1 do begin            ; each requested item
    i=ii[j]
    if i gt 300 then begin
        k=i-301
        v=krccom.ld[k]
        if v ne 0 then qq='T' else qq='F'
        out=out+labkl[k]+'='+qq+' '
    endif else if i gt 200 then begin
        k=i-201
        v=krccom.id[k]
        out=out+labki[k]+'='+strtrim(v,2)+' '
    endif else if i gt 100 then begin
        k=i-101
        v=krccom.fd[k]
        qq=DELAST0(strtrim(v,2)) ; remove trailing zeros
        out=out+labkf[k]+'='+qq+' '
    endif
endfor
return,out
end
